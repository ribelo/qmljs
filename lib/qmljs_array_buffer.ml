type t = Qmljs_types.value

type byte_bigarray =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let of_bytes_copy context bytes =
  { Qmljs_types.context
  ; raw =
      Qmljs_raw.array_buffer_of_bytes_copy
        (Qmljs_types.context_raw context)
        bytes
  }

let of_bigarray_copy context bigarray =
  { Qmljs_types.context
  ; raw =
      Qmljs_raw.array_buffer_of_bigarray_copy
        (Qmljs_types.context_raw context)
        bigarray
  }

let byte_length t =
  Qmljs_raw.array_buffer_byte_length
    (Qmljs_types.context_raw (Qmljs_types.value_context t))
    (Qmljs_types.value_raw t)

let to_bytes_copy t =
  Qmljs_raw.array_buffer_to_bytes_copy
    (Qmljs_types.context_raw (Qmljs_types.value_context t))
    (Qmljs_types.value_raw t)

let detach t =
  Qmljs_raw.array_buffer_detach
    (Qmljs_types.context_raw (Qmljs_types.value_context t))
    (Qmljs_types.value_raw t)
