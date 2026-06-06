type kind =
  | JavaScript
  | Policy_violation_kind of string
  | Out_of_memory_kind
  | Stack_overflow_kind
  | Resource_limit_kind
  | Host_callback_defect_kind
  | Timeout_kind
  | Closed_handle_kind
  | Wrong_domain_kind

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
  ; suppressed : structured list
  ; kind : kind
  ; metadata : (string * string) list
  }

type marker =
  { marker_name : string
  ; marker_message : string
  ; marker_kind : string
  ; marker_denied_specifier : string
  ; marker_stack : string
  ; marker_ocaml_frame : string
  ; marker_metadata : (string * string) array
  ; marker_cause : marker option
  }

exception Error of string
exception Host_error of string * (string * string) list
exception Structured_error of structured
exception Policy_violation of string
exception Out_of_memory
exception Stack_overflow
exception Resource_limit of string
exception Timeout of string

let () = Callback.Safe.register_exception "qmljs.error" (Error "")

let raise_policy_violation specifier = raise (Policy_violation specifier)
let raise_out_of_memory () = raise Out_of_memory
let raise_stack_overflow () = raise Stack_overflow
let raise_resource_limit message = raise (Resource_limit message)
let raise_timeout message = raise (Timeout message)

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

let make ?(name = "") ?(stack = "") ?(ocaml_frame = "") ?cause
    ?(suppressed = []) ~kind ?(metadata = []) message =
  { name
  ; message
  ; stack
  ; frames = js_frames stack @ ocaml_frames ocaml_frame
  ; cause
  ; suppressed
  ; kind
  ; metadata
  }

let kind_of_code denied_specifier = function
  | 1 -> Out_of_memory_kind
  | 2 -> Stack_overflow_kind
  | 3 -> Resource_limit_kind
  | 4 -> Policy_violation_kind denied_specifier
  | 5 -> Host_callback_defect_kind
  | 6 -> Timeout_kind
  | 7 -> Closed_handle_kind
  | 8 -> Wrong_domain_kind
  | _ -> JavaScript

(* Construct one level of a structured error. Called by the C boundary while
   the live QuickJS error values are still in hand, innermost cause first. *)
let make_structured name message stack ocaml_frame kind_code denied_specifier
    cause metadata =
  make ~name ~stack ~ocaml_frame ?cause
    ~metadata:(Array.to_list metadata)
    ~kind:(kind_of_code denied_specifier kind_code)
    message

let raise_structured (structured : structured) = raise (Structured_error structured)

let host_exception_message = function
  | Host_error (message, _metadata) -> message
  | Error message -> message
  | exn -> Printexc.to_string exn

let host_exception_metadata = function
  | Host_error (_message, metadata) -> Array.of_list metadata
  | _ -> [||]

let () =
  Callback.Safe.register "qmljs.make_structured" make_structured;
  Callback.Safe.register "qmljs.raise_structured" raise_structured;
  Callback.Safe.register "qmljs.host_exception_message" host_exception_message;
  Callback.Safe.register "qmljs.host_exception_metadata" host_exception_metadata

module Structured = struct
  type t = structured

  type kind =
    | JavaScript
    | Host_callback_defect
    | Policy_violation of { denied_module_specifier : string option }
    | Out_of_memory
    | Stack_overflow
    | Resource_limit
    | Timeout
    | Closed_handle
    | Wrong_domain

  let option_of_nonempty = function
    | "" -> None
    | value -> Some value

  let of_legacy_error message =
    let kind =
      if find_substring ~needle:"closed" message then Closed_handle_kind
      else if find_substring ~needle:"different context" message
              || find_substring ~needle:"different runtime" message
              || find_substring ~needle:"non-owner domain" message
      then Wrong_domain_kind
      else if find_substring ~needle:"timed out" message then Timeout_kind
      else JavaScript
    in
    make ~kind message

  let of_conversion message =
    make ~kind:JavaScript ("conversion error: " ^ message)

  let of_policy specifier = make ~kind:(Policy_violation_kind specifier) specifier
  let of_out_of_memory () = make ~kind:Out_of_memory_kind "out of memory"
  let of_stack_overflow () = make ~kind:Stack_overflow_kind "stack overflow"
  let of_resource_limit message = make ~kind:Resource_limit_kind message
  let of_timeout message = make ~kind:Timeout_kind message
  let timeout_with_cause ~message cause =
    make ~kind:Timeout_kind ~cause message
  let with_suppressed t suppressed =
    match suppressed with
    | [] -> t
    | _ -> { t with suppressed = t.suppressed @ suppressed }

  let marker_kind_of_kind ?denied_specifier_default = function
    | Policy_violation_kind specifier ->
        let denied_specifier =
          match specifier, denied_specifier_default with
          | "", Some default -> default
          | _ -> specifier
        in
        "policy_violation", denied_specifier
    | Host_callback_defect_kind -> "host_callback_defect", ""
    | Out_of_memory_kind -> "out_of_memory", ""
    | Stack_overflow_kind -> "stack_overflow", ""
    | Resource_limit_kind -> "resource_limit", ""
    | Timeout_kind -> "timeout", ""
    | Closed_handle_kind -> "closed_handle", ""
    | Wrong_domain_kind -> "wrong_domain", ""
    | JavaScript -> "javascript", ""

  let first_ocaml_frame (frames : frame list) =
    List.find_map
      (fun frame ->
        match frame.kind with
        | Js_frame -> None
        | Ocaml_frame ->
            Some
              (if String.equal frame.function_name "" then frame.raw
               else frame.function_name))
      frames

  let rec to_marker ?denied_specifier_default ?(ocaml_frame = "") (t : t) =
    let marker_kind, marker_denied_specifier =
      marker_kind_of_kind ?denied_specifier_default t.kind
    in
    let marker_ocaml_frame =
      match first_ocaml_frame t.frames with
      | Some frame when not (String.equal frame "") -> frame
      | _ -> ocaml_frame
    in
    { marker_name = t.name
    ; marker_message = t.message
    ; marker_kind
    ; marker_denied_specifier
    ; marker_stack = t.stack
    ; marker_ocaml_frame
    ; marker_metadata = Array.of_list t.metadata
    ; marker_cause =
        Option.map
          (fun cause -> to_marker ?denied_specifier_default cause)
          t.cause
    }

  let of_exception = function
    | Structured_error error -> error
    | Policy_violation specifier -> of_policy specifier
    | Out_of_memory -> of_out_of_memory ()
    | Stack_overflow -> of_stack_overflow ()
    | Resource_limit message -> of_resource_limit message
    | Timeout message -> of_timeout message
    | Host_error (message, metadata) ->
        make ~kind:Host_callback_defect_kind ~metadata message
    | Error message -> of_legacy_error message
    | exn -> of_legacy_error (Printexc.to_string exn)

  let capture fn =
    try Stdlib.Result.Ok (fn ()) with
    | exn -> Stdlib.Result.Error (of_exception exn)

  let raise_with_suppressed exn backtrace suppressed =
    let error = with_suppressed (of_exception exn) suppressed in
    Printexc.raise_with_backtrace (Structured_error error) backtrace

  let host ~message ~metadata = Host_error (message, metadata)
  let conversion = of_conversion
  let message t = t.message
  let metadata t = t.metadata
  let cause t = t.cause
  let suppressed t = t.suppressed
  let cause_exn t =
    match t.cause with
    | Some cause -> cause
    | None -> invalid_arg "error has no cause"

  let raise t = raise (Structured_error t)

  let kind (t : t) =
    match t.kind with
    | JavaScript -> JavaScript
    | Host_callback_defect_kind -> Host_callback_defect
    | Policy_violation_kind specifier ->
        Policy_violation
          { denied_module_specifier = option_of_nonempty specifier }
    | Out_of_memory_kind -> Out_of_memory
    | Stack_overflow_kind -> Stack_overflow
    | Resource_limit_kind -> Resource_limit
    | Timeout_kind -> Timeout
    | Closed_handle_kind -> Closed_handle
    | Wrong_domain_kind -> Wrong_domain

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
    | Resource_limit_kind | Timeout_kind | Out_of_memory_kind | Stack_overflow_kind -> true
    | _ -> false

  let is_host_callback_defect (t : t) =
    match t.kind with
    | Host_callback_defect_kind -> true
    | _ -> false
end
