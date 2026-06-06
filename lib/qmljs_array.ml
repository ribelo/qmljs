type t = Qmljs_types.value

let create context =
  { Qmljs_types.context
  ; raw = Qmljs_raw.array_create (Qmljs_types.context_raw context)
  }

let of_array context values =
  { Qmljs_types.context
  ; raw =
      Qmljs_raw.array_of_values
        (Qmljs_types.context_raw context)
        (Stdlib.Array.map Qmljs_types.value_raw values)
  }

let of_list context values =
  of_array context (Stdlib.Array.of_list values)

let length t =
  Qmljs_raw.array_length
    (Qmljs_types.context_raw (Qmljs_types.value_context t))
    (Qmljs_types.value_raw t)

let get t index =
  let context = Qmljs_types.value_context t in
  { Qmljs_types.context
  ; raw =
      Qmljs_raw.array_get
        (Qmljs_types.context_raw context)
        (Qmljs_types.value_raw t)
        index
  }

let set t index value =
  Qmljs_raw.array_set
    (Qmljs_types.context_raw (Qmljs_types.value_context t))
    (Qmljs_types.value_raw t)
    index
    (Qmljs_types.value_raw value)

let push t value =
  Qmljs_raw.array_push
    (Qmljs_types.context_raw (Qmljs_types.value_context t))
    (Qmljs_types.value_raw t)
    [| Qmljs_types.value_raw value |]

let append t values =
  Qmljs_raw.array_push
    (Qmljs_types.context_raw (Qmljs_types.value_context t))
    (Qmljs_types.value_raw t)
    (Stdlib.Array.of_list (List.map Qmljs_types.value_raw values))

let to_array t =
  Stdlib.Array.init (length t) (get t)

let to_list t = Stdlib.Array.to_list (to_array t)
