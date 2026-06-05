module Uint8Array = struct
  type t = Qmljs_types.value

  let of_bytes_copy context bytes =
    { Qmljs_types.context
    ; raw =
        Qmljs_raw.typed_array_uint8_of_bytes_copy
          (Qmljs_types.context_raw context)
          bytes
    }
end
