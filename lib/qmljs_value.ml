type t = Qmljs_types.value

type kind =
  | Undefined
  | Null
  | Bool
  | Int
  | Float
  | String
  | Symbol
  | Bigint
  | Object
  | Array
  | Function
  | Promise
  | Typed_array

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

let object_ t = t
let array t = t
let bigint t = t
let typed_array t = t

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

let kind t =
  let context = Qmljs_types.value_context t in
  match
    Qmljs_raw.value_kind
      (Qmljs_types.context_raw context)
      (Qmljs_types.value_raw t)
  with
  | 0 -> Undefined
  | 1 -> Null
  | 2 -> Bool
  | 3 -> Int
  | 4 -> Float
  | 5 -> String
  | 6 -> Symbol
  | 7 -> Bigint
  | 8 -> Object
  | 9 -> Array
  | 10 -> Function
  | 11 -> Promise
  | 12 -> Typed_array
  | tag -> invalid_arg ("unknown QuickJS value kind " ^ string_of_int tag)

let is_null t =
  match kind t with
  | Null -> true
  | _ -> false

let is_undefined t =
  match kind t with
  | Undefined -> true
  | _ -> false

let is_function t =
  match kind t with
  | Function -> true
  | _ -> false

let is_array t =
  match kind t with
  | Array -> true
  | _ -> false

let same_context2 left right =
  let context = Qmljs_types.value_context left in
  Qmljs_types.context_raw context, Qmljs_types.value_raw left, Qmljs_types.value_raw right

let strict_equal left right =
  let context, left, right = same_context2 left right in
  Qmljs_raw.value_strict_equal context left right

let same_value left right =
  let context, left, right = same_context2 left right in
  Qmljs_raw.value_same_value context left right

let same_value_zero left right =
  let context, left, right = same_context2 left right in
  Qmljs_raw.value_same_value_zero context left right

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
