module Loader = struct
  type t = Qmljs_types.module_loader

  let identity_normalize ~base:_ ~specifier = Ok specifier

  let memory sources =
    let table = Hashtbl.create (List.length sources) in
    List.iter (fun (specifier, source) -> Hashtbl.replace table specifier source) sources;
    { Qmljs_types.sources = table
    ; deny = (fun _ -> false)
    ; normalize = identity_normalize
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
                 (Qmljs_error.Structured.of_legacy_error
                    ("module specifier escapes loader root: " ^ specifier))
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

  let read_file path =
    let channel = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr channel)
      (fun () -> really_input_string channel (in_channel_length channel))

  let filesystem ~root ~allow =
    let table = Hashtbl.create 16 in
    let rec scan prefix directory =
      Sys.readdir directory
      |> Array.iter (fun name ->
        let path = Filename.concat directory name in
        let specifier =
          match prefix with
          | "" -> name
          | prefix -> prefix ^ "/" ^ name
        in
        if Sys.is_directory path then scan specifier path
        else Hashtbl.replace table specifier (read_file path))
    in
    scan "" root;
    { Qmljs_types.sources = table
    ; deny = (fun specifier -> not (allow ~specifier))
    ; normalize = filesystem_normalize
    }

  let normalize normalize loader =
    { loader with
      Qmljs_types.normalize =
        (fun ~base ~specifier ->
          match normalize ~base ~specifier with
          | Error _ as error -> error
          | Ok specifier -> loader.Qmljs_types.normalize ~base ~specifier)
    }
end

type namespace = Qmljs_types.value

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
