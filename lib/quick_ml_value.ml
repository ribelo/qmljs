type t = Quick_ml_types.value

let undefined context =
  { Quick_ml_types.context
  ; raw = Quick_ml_raw.value_undefined (Quick_ml_types.context_raw context)
  }

let null context =
  { Quick_ml_types.context
  ; raw = Quick_ml_raw.value_null (Quick_ml_types.context_raw context)
  }

let bool context value =
  { Quick_ml_types.context
  ; raw = Quick_ml_raw.value_bool (Quick_ml_types.context_raw context) value
  }

let int context value =
  { Quick_ml_types.context
  ; raw = Quick_ml_raw.value_int (Quick_ml_types.context_raw context) value
  }

let float context value =
  { Quick_ml_types.context
  ; raw = Quick_ml_raw.value_float (Quick_ml_types.context_raw context) value
  }

let string context value =
  { Quick_ml_types.context
  ; raw = Quick_ml_raw.value_string (Quick_ml_types.context_raw context) value
  }

let to_bool t =
  Quick_ml_raw.value_to_bool
    (Quick_ml_types.context_raw (Quick_ml_types.value_context t))
    (Quick_ml_types.value_raw t)

let to_int t =
  Quick_ml_raw.value_to_int
    (Quick_ml_types.context_raw (Quick_ml_types.value_context t))
    (Quick_ml_types.value_raw t)

let to_float t =
  Quick_ml_raw.value_to_float
    (Quick_ml_types.context_raw (Quick_ml_types.value_context t))
    (Quick_ml_types.value_raw t)

let to_string t =
  Quick_ml_raw.value_to_string
    (Quick_ml_types.context_raw (Quick_ml_types.value_context t))
    (Quick_ml_types.value_raw t)

let call t args =
  let context = Quick_ml_types.value_context t in
  let raw_args = Array.map Quick_ml_types.value_raw args in
  { Quick_ml_types.context
  ; raw =
      Quick_ml_raw.value_call
        (Quick_ml_types.context_raw context)
        (Quick_ml_types.value_raw t)
        raw_args
  }
