type t = Qmljs_types.value

let undefined context =
  { Qmljs_types.context
  ; raw = Qmljs_raw.value_undefined (Qmljs_types.context_raw context)
  }

let null context =
  { Qmljs_types.context
  ; raw = Qmljs_raw.value_null (Qmljs_types.context_raw context)
  }

let bool context value =
  { Qmljs_types.context
  ; raw = Qmljs_raw.value_bool (Qmljs_types.context_raw context) value
  }

let int context value =
  { Qmljs_types.context
  ; raw = Qmljs_raw.value_int (Qmljs_types.context_raw context) value
  }

let float context value =
  { Qmljs_types.context
  ; raw = Qmljs_raw.value_float (Qmljs_types.context_raw context) value
  }

let string context value =
  { Qmljs_types.context
  ; raw = Qmljs_raw.value_string (Qmljs_types.context_raw context) value
  }

let to_bool t =
  Qmljs_raw.value_to_bool
    (Qmljs_types.context_raw (Qmljs_types.value_context t))
    (Qmljs_types.value_raw t)

let to_int t =
  Qmljs_raw.value_to_int
    (Qmljs_types.context_raw (Qmljs_types.value_context t))
    (Qmljs_types.value_raw t)

let to_float t =
  Qmljs_raw.value_to_float
    (Qmljs_types.context_raw (Qmljs_types.value_context t))
    (Qmljs_types.value_raw t)

let to_string t =
  Qmljs_raw.value_to_string
    (Qmljs_types.context_raw (Qmljs_types.value_context t))
    (Qmljs_types.value_raw t)

let call t args =
  let context = Qmljs_types.value_context t in
  let raw_args = Array.map Qmljs_types.value_raw args in
  { Qmljs_types.context
  ; raw =
      Qmljs_raw.value_call
        (Qmljs_types.context_raw context)
        (Qmljs_types.value_raw t)
        raw_args
  }
