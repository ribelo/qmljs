module Uint8Array = struct
  type t = Qmljs_types.value

  let of_bytes_copy context bytes =
    { Qmljs_types.context
    ; raw =
        Qmljs_raw.typed_array_uint8_of_bytes_copy
          (Qmljs_types.context_raw context)
          bytes
    }

  let length t =
    Qmljs_raw.typed_array_uint8_length
      (Qmljs_types.context_raw (Qmljs_types.value_context t))
      (Qmljs_types.value_raw t)

  let get t index =
    Qmljs_raw.typed_array_uint8_get
      (Qmljs_types.context_raw (Qmljs_types.value_context t))
      (Qmljs_types.value_raw t)
      index

  let set t index byte =
    Qmljs_raw.typed_array_uint8_set
      (Qmljs_types.context_raw (Qmljs_types.value_context t))
      (Qmljs_types.value_raw t)
      index
      byte

  let to_bytes_copy t =
    Qmljs_raw.typed_array_uint8_to_bytes_copy
      (Qmljs_types.context_raw (Qmljs_types.value_context t))
      (Qmljs_types.value_raw t)
end
