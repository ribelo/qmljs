type kind =
  | JavaScript
  | Policy_violation_kind of string
  | Out_of_memory_kind
  | Stack_overflow_kind
  | Resource_limit_kind

and frame_kind =
  | Js_frame
  | Ocaml_frame

and frame =
  { kind : frame_kind
  ; function_name : string
  ; raw : string
  }

and structured =
  { name : string
  ; message : string
  ; stack : string
  ; frames : frame list
  ; cause : structured option
  ; kind : kind
  }

exception Error of string
exception Host_error of string * (string * string) list
exception Structured_error of structured
exception Policy_violation of string
exception Out_of_memory
exception Stack_overflow
exception Resource_limit of string

let () = Callback.Safe.register_exception "qmljs.error" (Error "")

let raise_policy_violation specifier = raise (Policy_violation specifier)
let raise_out_of_memory () = raise Out_of_memory
let raise_stack_overflow () = raise Stack_overflow
let raise_resource_limit message = raise (Resource_limit message)

let find_substring ~needle value =
  let value_length = String.length value in
  let needle_length = String.length needle in
  let rec loop index =
    if index + needle_length > value_length then false
    else String.sub value index needle_length = needle || loop (index + 1)
  in
  needle_length = 0 || loop 0

let lines value = String.split_on_char '\n' value

let js_frames stack =
  stack
  |> lines
  |> List.filter_map (fun raw ->
    let raw = String.trim raw in
    if String.equal raw "" then None
    else
      let function_name =
        match String.index_opt raw '@' with
        | Some 0 | None -> raw
        | Some index -> String.sub raw 0 index
      in
      Some { kind = Js_frame; function_name; raw })

let ocaml_frames = function
  | "" -> []
  | function_name -> [ { kind = Ocaml_frame; function_name; raw = function_name } ]

let make ?(name = "") ?(stack = "") ?(ocaml_frame = "") ?cause ~kind
    message =
  { name
  ; message
  ; stack
  ; frames = js_frames stack @ ocaml_frames ocaml_frame
  ; cause
  ; kind
  }

let raise_js_exception name message stack ocaml_frame cause_name cause_message
    cause_stack cause_ocaml_frame =
  let cause =
    if String.equal cause_name ""
       && String.equal cause_message ""
       && String.equal cause_stack ""
       && String.equal cause_ocaml_frame ""
    then None
    else
      Some
        (make ~name:cause_name ~stack:cause_stack
           ~ocaml_frame:cause_ocaml_frame ~kind:JavaScript cause_message)
  in
  raise
    (Structured_error
       (make ~name ~stack ~ocaml_frame ?cause ~kind:JavaScript message))

let host_exception_message = function
  | Host_error (message, _metadata) -> message
  | Error message -> message
  | exn -> Printexc.to_string exn

let () =
  Callback.Safe.register "qmljs.raise_js_exception" raise_js_exception;
  Callback.Safe.register "qmljs.host_exception_message" host_exception_message

module Structured = struct
  type t = structured

  let of_legacy_error message = make ~kind:JavaScript message
  let of_policy specifier = make ~kind:(Policy_violation_kind specifier) specifier
  let of_out_of_memory () = make ~kind:Out_of_memory_kind "out of memory"
  let of_stack_overflow () = make ~kind:Stack_overflow_kind "stack overflow"
  let of_resource_limit message = make ~kind:Resource_limit_kind message

  let capture fn =
    try Stdlib.Result.Ok (fn ()) with
    | Structured_error error -> Stdlib.Result.Error error
    | Policy_violation specifier -> Stdlib.Result.Error (of_policy specifier)
    | Out_of_memory -> Stdlib.Result.Error (of_out_of_memory ())
    | Stack_overflow -> Stdlib.Result.Error (of_stack_overflow ())
    | Resource_limit message -> Stdlib.Result.Error (of_resource_limit message)
    | Host_error (message, _metadata) ->
        Stdlib.Result.Error (make ~kind:JavaScript message)
    | Error message -> Stdlib.Result.Error (of_legacy_error message)
    | exn -> Stdlib.Result.Error (of_legacy_error (Printexc.to_string exn))

  let host ~message ~metadata = Host_error (message, metadata)
  let message t = t.message
  let cause_exn t = Option.value t.cause ~default:t

  let rec has_frame kind t ~function_name =
    List.exists
      (fun frame ->
        frame.kind = kind
        && (String.equal frame.function_name function_name
            || find_substring ~needle:function_name frame.raw))
      t.frames
    ||
    match t.cause with
    | None -> false
    | Some cause -> has_frame kind cause ~function_name

  let has_js_frame = has_frame Js_frame
  let has_ocaml_frame = has_frame Ocaml_frame

  let is_policy_violation (t : t) =
    match t.kind with
    | Policy_violation_kind _ -> true
    | _ -> false

  let denied_module_specifier_exn (t : t) =
    match t.kind with
    | Policy_violation_kind specifier -> specifier
    | _ -> invalid_arg "not a module policy violation"

  let is_out_of_memory (t : t) =
    match t.kind with
    | Out_of_memory_kind -> true
    | _ -> false

  let is_stack_overflow (t : t) =
    match t.kind with
    | Stack_overflow_kind -> true
    | _ -> false

  let is_resource_limit (t : t) =
    match t.kind with
    | Resource_limit_kind | Out_of_memory_kind | Stack_overflow_kind -> true
    | _ -> false
end
