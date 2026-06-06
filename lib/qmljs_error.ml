type kind =
  | JavaScript
  | Policy_violation_kind of string
  | Out_of_memory_kind
  | Stack_overflow_kind
  | Resource_limit_kind
  | Host_callback_defect_kind

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

let kind_of_code denied_specifier = function
  | 1 -> Out_of_memory_kind
  | 2 -> Stack_overflow_kind
  | 3 -> Resource_limit_kind
  | 4 -> Policy_violation_kind denied_specifier
  | 5 -> Host_callback_defect_kind
  | _ -> JavaScript

(* Construct one level of a structured error. Called by the C boundary while
   the live QuickJS error values are still in hand, innermost cause first. *)
let make_structured name message stack ocaml_frame kind_code denied_specifier
    cause =
  make ~name ~stack ~ocaml_frame ?cause
    ~kind:(kind_of_code denied_specifier kind_code)
    message

let raise_structured (structured : structured) = raise (Structured_error structured)

let host_exception_message = function
  | Host_error (message, _metadata) -> message
  | Error message -> message
  | exn -> Printexc.to_string exn

let () =
  Callback.Safe.register "qmljs.make_structured" make_structured;
  Callback.Safe.register "qmljs.raise_structured" raise_structured;
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
        Stdlib.Result.Error (make ~kind:Host_callback_defect_kind message)
    | Error message -> Stdlib.Result.Error (of_legacy_error message)
    | exn -> Stdlib.Result.Error (of_legacy_error (Printexc.to_string exn))

  let host ~message ~metadata = Host_error (message, metadata)
  let message t = t.message
  let cause_exn t =
    match t.cause with
    | Some cause -> cause
    | None -> invalid_arg "error has no cause"

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

  let is_host_callback_defect (t : t) =
    match t.kind with
    | Host_callback_defect_kind -> true
    | _ -> false
end
