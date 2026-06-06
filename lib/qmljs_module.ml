module Loader = struct
  type t = Qmljs_types.module_loader
  type request = Qmljs_types.module_load_request =
    { base : string option
    ; specifier : string
    ; normalized : string
    ; attributes : (string * string) list
    }

  type import_attributes = (string * string) list

  let identity_normalize ~base:_ ~specifier ~attributes:_ = Ok specifier

  let table_load table request =
    Ok (Hashtbl.find_opt table request.Qmljs_types.normalized)

  let memory sources =
    let table = Hashtbl.create (List.length sources) in
    List.iter (fun (specifier, source) -> Hashtbl.replace table specifier source) sources;
    { Qmljs_types.requests = Hashtbl.create 16
    ; request_identity = Qmljs_types.Normalized_request
    ; deny = (fun _ -> false)
    ; normalize = identity_normalize
    ; load = table_load table
    }

  let deny predicate loader = { loader with Qmljs_types.deny = predicate }

  let split_specifier specifier =
    specifier
    |> String.split_on_char '/'
    |> List.filter (fun segment -> not (String.equal segment ""))

  let normalize_segments specifier segments =
    let rec loop acc = function
      | [] -> Ok (String.concat "/" (List.rev acc))
      | "." :: rest -> loop acc rest
      | ".." :: rest ->
          (match acc with
           | [] ->
               Error
                 (Qmljs_error.Structured.of_policy specifier)
           | _ :: acc -> loop acc rest)
      | segment :: rest -> loop (segment :: acc) rest
    in
    loop [] segments

  let base_segments = function
    | None -> []
    | Some base ->
        (match List.rev (split_specifier base) with
         | [] | [ _ ] -> []
         | _file :: rest -> List.rev rest)

  let is_relative specifier =
    String.length specifier > 0 && Char.equal specifier.[0] '.'

  let filesystem_normalize ~base ~specifier =
    let segments =
      if is_relative specifier then base_segments base @ split_specifier specifier
      else split_specifier specifier
    in
    normalize_segments specifier segments

  let filesystem_normalize ~base ~specifier ~attributes:_ =
    filesystem_normalize ~base ~specifier

  let read_file path =
    let channel = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr channel)
      (fun () -> really_input_string channel (in_channel_length channel))

  let lstat_kind path =
    match (Unix.lstat path).Unix.st_kind with
    | kind -> Some kind
    | exception Unix.Unix_error (Unix.ENOENT, _, _) -> None
    | exception Unix.Unix_error (Unix.ENOTDIR, _, _) -> None

  let filesystem_path root specifier =
    let rec loop path = function
      | [] -> None
      | [ segment ] ->
          let path = Filename.concat path segment in
          (match lstat_kind path with
           | Some Unix.S_REG -> Some path
           | Some Unix.S_LNK | Some Unix.S_DIR | Some Unix.S_CHR
           | Some Unix.S_BLK | Some Unix.S_FIFO | Some Unix.S_SOCK | None -> None)
      | segment :: rest ->
          let path = Filename.concat path segment in
          (match lstat_kind path with
           | Some Unix.S_DIR -> loop path rest
           | Some Unix.S_LNK | Some Unix.S_REG | Some Unix.S_CHR
           | Some Unix.S_BLK | Some Unix.S_FIFO | Some Unix.S_SOCK | None -> None)
    in
    loop root (split_specifier specifier)

  let filesystem_load root request =
    match filesystem_path root request.Qmljs_types.normalized with
    | None -> Ok None
    | Some path -> Ok (Some (read_file path))

  let filesystem ~root ~allow =
    { Qmljs_types.requests = Hashtbl.create 16
    ; request_identity = Qmljs_types.Normalized_request
    ; deny = (fun specifier -> not (allow ~specifier))
    ; normalize = filesystem_normalize
    ; load = filesystem_load root
    }

  let normalize normalize loader =
    { loader with
      Qmljs_types.normalize =
        (fun ~base ~specifier ~attributes ->
          match normalize ~base ~specifier with
          | Error _ as error -> error
          | Ok specifier ->
              loader.Qmljs_types.normalize ~base ~specifier ~attributes)
    }

  let callback ~normalize ~load =
    { Qmljs_types.requests = Hashtbl.create 16
    ; request_identity = Qmljs_types.Exact_request
    ; deny = (fun _ -> false)
    ; normalize
    ; load
    }

  let fallback loaders =
    match loaders with
    | [] -> invalid_arg "module loader fallback requires at least one loader"
    | loaders ->
        let load (request : request) =
          let rec loop = function
            | [] -> Ok None
            | loader :: rest ->
                (match
                   loader.Qmljs_types.normalize
                     ~base:request.base
                     ~specifier:request.specifier
                     ~attributes:request.attributes
                 with
                 | Error _ as error -> error
                 | Ok normalized ->
                     if loader.Qmljs_types.deny normalized then
                       Error (Qmljs_error.Structured.of_policy normalized)
                     else
                       let request = { request with normalized } in
                       (match loader.Qmljs_types.load request with
                        | Ok None -> loop rest
                        | Ok Some _ as result -> result
                        | Error _ as error -> error))
          in
          loop loaders
        in
        { Qmljs_types.requests = Hashtbl.create 16
        ; request_identity = Qmljs_types.Exact_request
        ; deny = (fun _ -> false)
        ; normalize = filesystem_normalize
        ; load
        }
end

type namespace = Qmljs_types.value

type evaluated =
  { namespace : namespace
  ; import_meta : Qmljs_types.value
  }

type native_exports =
  { context : Qmljs_types.context
  ; raw : Qmljs_raw.native_exports
  }

module Native = struct
  type exports = native_exports

  type declarations =
    { mutable export_names : string list
    }

  type t =
    { export_names : string array
    ; evaluate : exports -> unit
    }

  let check_export_name name =
    if String.contains name '\000' then
      invalid_arg "native module export name cannot contain NUL"

  let declare (declarations : declarations) name =
    check_export_name name;
    if List.mem name declarations.export_names then
      invalid_arg ("duplicate native module export: " ^ name);
    declarations.export_names <- name :: declarations.export_names

  let define declare ~evaluate =
    let declarations = { export_names = [] } in
    declare declarations;
    { export_names = Array.of_list (List.rev declarations.export_names)
    ; evaluate
    }

  let export exports ~name value =
    check_export_name name;
    Qmljs_raw.module_native_export
      (Qmljs_types.context_raw exports.context)
      exports.raw
      name
      (Qmljs_types.value_raw value)
end

let declare_native context ~name (native : Native.t) =
  if String.contains name '\000' then
    invalid_arg "native module name cannot contain NUL";
  let evaluate raw_exports =
    let exports = { context; raw = raw_exports } in
    match Qmljs_error.Structured.capture (fun () -> native.evaluate exports) with
    | Ok () -> Ok ()
    | Error error ->
        Error (Qmljs_error.Structured.to_marker ~ocaml_frame:name error)
  in
  Qmljs_raw.module_declare_native
    (Qmljs_types.context_raw context)
    name
    native.export_names
    evaluate

let dynamic_import ?(base = "<dynamic-import>") context specifier =
  { Qmljs_types.context
  ; raw =
      Qmljs_raw.module_dynamic_import
        (Qmljs_types.context_raw context)
        base
        specifier
  }

let eval_with_meta ?(filename = "<module>") context source =
  let namespace, import_meta =
    Qmljs_raw.context_eval_module_with_meta
      (Qmljs_types.context_raw context)
      source
      filename
  in
  { namespace = { Qmljs_types.context; raw = namespace }
  ; import_meta = { Qmljs_types.context; raw = import_meta }
  }

let eval ?(filename = "<module>") context source =
  { Qmljs_types.context
  ; raw =
      Qmljs_raw.context_eval_module_namespace
        (Qmljs_types.context_raw context)
        source
        filename
  }

let default namespace =
  let context = Qmljs_types.value_context namespace in
  Qmljs_object.get namespace ~key:(Qmljs_value.string context "default")

let get namespace ~name =
  let context = Qmljs_types.value_context namespace in
  let key = Qmljs_value.string context name in
  if Qmljs_object.has namespace ~key then Some (Qmljs_object.get namespace ~key)
  else None

let keys = Qmljs_object.keys

let default_export_to_int = Qmljs_value.to_int
