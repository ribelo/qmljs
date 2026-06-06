type t = Qmljs_types.value

let of_string context value =
  { Qmljs_types.context
  ; raw =
      Qmljs_raw.bigint_of_string
        (Qmljs_types.context_raw context)
        value
  }

let to_string = Qmljs_value.to_string
let to_int64 t = Int64.of_string_opt (to_string t)
let of_int64 context value = of_string context (Int64.to_string value)
