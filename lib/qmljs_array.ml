type t = Qmljs_types.value

let of_list context values =
  { Qmljs_types.context
  ; raw =
      Qmljs_raw.array_of_values
        (Qmljs_types.context_raw context)
        (Stdlib.Array.of_list (List.map Qmljs_types.value_raw values))
  }
