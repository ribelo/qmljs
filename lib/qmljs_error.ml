exception Error of string

let () = Callback.Safe.register_exception "qmljs.error" (Error "")

let raise_policy_violation specifier =
  raise (Error ("qmljs policy violation: denied module " ^ specifier))

let raise_out_of_memory () = raise (Error "qmljs out of memory")
let raise_stack_overflow () = raise (Error "qmljs stack overflow")

module Structured = struct
  type kind =
    | JavaScript
    | Policy_violation of string
    | Out_of_memory
    | Stack_overflow
    | Resource_limit

  type t =
    { message : string
    ; raw : string
    ; cause : t option
    ; kind : kind
    }

  let starts_with ~prefix value =
    let prefix_length = String.length prefix in
    String.length value >= prefix_length
    && String.sub value 0 prefix_length = prefix

  let strip_prefix prefix value =
    if starts_with ~prefix value then
      String.sub value (String.length prefix)
        (String.length value - String.length prefix)
    else value

  let first_line value =
    match String.index_opt value '\n' with
    | None -> value
    | Some index -> String.sub value 0 index

  let trim = String.trim

  let find_substring ~needle value =
    let value_length = String.length value in
    let needle_length = String.length needle in
    let rec loop index =
      if index + needle_length > value_length then None
      else if String.sub value index needle_length = needle then Some index
      else loop (index + 1)
    in
    if needle_length = 0 then Some 0 else loop 0

  let extract_quoted_error value =
    match find_substring ~needle:"Error(\"" value with
    | None -> value
    | Some start ->
        let content_start = start + String.length "Error(\"" in
        let rec find_end index =
          if index + 2 > String.length value then None
          else if String.sub value index 2 = "\")" then Some index
          else find_end (index + 1)
        in
        (match find_end content_start with
         | None -> value
         | Some stop -> String.sub value content_start (stop - content_start))

  let normalize_message value =
    value
    |> first_line
    |> trim
    |> strip_prefix "Uncaught "
    |> strip_prefix "Error: "
    |> strip_prefix "InternalError: "
    |> strip_prefix "RangeError: "
    |> strip_prefix "TypeError: "
    |> extract_quoted_error
    |> trim

  let denied_specifier raw =
    let prefix = "qmljs policy violation: denied module " in
    if starts_with ~prefix raw then
      String.sub raw (String.length prefix) (String.length raw - String.length prefix)
    else ""

  let kind_of_raw raw =
    let normalized = String.lowercase_ascii raw in
    if starts_with ~prefix:"qmljs policy violation: denied module " raw then
      Policy_violation (denied_specifier raw)
    else if find_substring ~needle:"out of memory" normalized <> None then
      Out_of_memory
    else if find_substring ~needle:"stack overflow" normalized <> None
            || find_substring ~needle:"maximum call stack" normalized <> None
    then Stack_overflow
    else if find_substring ~needle:"timed out" normalized <> None
            || find_substring ~needle:"resource limit" normalized <> None
    then Resource_limit
    else JavaScript

  let rec of_message raw =
    let cause =
      match find_substring ~needle:"Caused by: " raw with
      | None -> None
      | Some index ->
          let start = index + String.length "Caused by: " in
          let rest = String.sub raw start (String.length raw - start) in
          Some (of_message rest)
    in
    { message = normalize_message raw; raw; cause; kind = kind_of_raw raw }

  let capture fn =
    try Stdlib.Result.Ok (fn ()) with
    | Error message -> Stdlib.Result.Error (of_message message)
    | exn -> Stdlib.Result.Error (of_message (Printexc.to_string exn))

  let host ~message ~metadata:_ = Error message
  let message t = t.message
  let cause_exn t = Option.value t.cause ~default:t

  let has_frame t ~function_name =
    find_substring ~needle:function_name t.raw <> None

  let has_js_frame = has_frame
  let has_ocaml_frame = has_frame

  let is_policy_violation t =
    match t.kind with
    | Policy_violation _ -> true
    | _ -> false

  let denied_module_specifier_exn t =
    match t.kind with
    | Policy_violation specifier -> specifier
    | _ -> invalid_arg "not a module policy violation"

  let is_out_of_memory t =
    match t.kind with
    | Out_of_memory -> true
    | _ -> false

  let is_stack_overflow t =
    match t.kind with
    | Stack_overflow -> true
    | _ -> false

  let is_resource_limit t =
    match t.kind with
    | Resource_limit | Out_of_memory | Stack_overflow -> true
    | _ -> false
end
