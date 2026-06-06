let parse ?(filename = "<json>") context input =
  { Qmljs_types.context
  ; raw =
      Qmljs_raw.json_parse
        (Qmljs_types.context_raw context)
        input
        filename
  }

let stringify ?space value =
  let context = Qmljs_types.value_context value in
  Qmljs_raw.json_stringify
    (Qmljs_types.context_raw context)
    (Qmljs_types.value_raw value)
    space

let of_string ?filename context input =
  Qmljs_error.Structured.capture (fun () -> parse ?filename context input)

let to_string ?space value =
  Qmljs_error.Structured.capture (fun () -> stringify ?space value)
