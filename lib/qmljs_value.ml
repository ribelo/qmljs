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

type 'a encoder = Qmljs_types.context -> 'a -> t
type 'a decoder = t -> ('a, Qmljs_error.Structured.t) result

let context t = Qmljs_types.value_context t
let raw t = Qmljs_types.value_raw t
let wrap context raw = { Qmljs_types.context; raw }

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

let conversion_error message =
  Error (Qmljs_error.Structured.of_conversion message)

let expected name = conversion_error ("expected " ^ name)

let ( let* ) result fn =
  match result with
  | Ok value -> fn value
  | Error error -> Error error

let capture_conversion fn =
  match Qmljs_error.Structured.capture fn with
  | Ok result -> result
  | Error _ as error -> error

let as_bool t =
  match kind t with
  | Bool -> Ok (to_bool t)
  | _ -> expected "bool"

let as_int t =
  match kind t with
  | Int | Bigint -> Ok (to_int t)
  | _ -> expected "int"

let as_float t =
  match kind t with
  | Int | Float -> Ok (to_float t)
  | _ -> expected "number"

let as_string t =
  match kind t with
  | String -> Ok (to_string t)
  | _ -> expected "string"

let as_bigint t =
  match kind t with
  | Bigint -> Ok t
  | _ -> expected "bigint"

let as_object t =
  match kind t with
  | Object -> Ok t
  | _ -> expected "object"

let as_array t =
  match kind t with
  | Array -> Ok t
  | _ -> expected "array"

let as_typed_array t =
  match kind t with
  | Typed_array -> Ok t
  | _ -> expected "typed array"

let of_array encode context values =
  let values =
    Stdlib.Array.map
      (fun value -> Qmljs_types.value_raw (encode context value))
      values
  in
  wrap context
    (Qmljs_raw.array_of_values (Qmljs_types.context_raw context) values)

let of_list encode context values =
  of_array encode context (Stdlib.Array.of_list values)

let to_array decode t =
  capture_conversion @@ fun () ->
  match kind t with
  | Array ->
      let context = context t in
      let length =
        Qmljs_raw.array_length
          (Qmljs_types.context_raw context)
          (raw t)
      in
      let rec loop index acc =
        if index = length then Ok (Stdlib.Array.of_list (List.rev acc))
        else
          let element =
            wrap context
              (Qmljs_raw.array_get
                 (Qmljs_types.context_raw context)
                 (raw t)
                 index)
          in
          match decode element with
          | Ok value -> loop (index + 1) (value :: acc)
          | Error _ as error -> error
      in
      loop 0 []
  | _ -> expected "array"

let to_list decode t =
  match to_array decode t with
  | Ok values -> Ok (Stdlib.Array.to_list values)
  | Error _ as error -> error

let of_option encode context = function
  | None -> null context
  | Some value -> encode context value

let to_option decode t =
  capture_conversion @@ fun () ->
  match kind t with
  | Null | Undefined -> Ok None
  | _ ->
      (match decode t with
       | Ok value -> Ok (Some value)
       | Error _ as error -> error)

let set_string_raw object_ name value =
  Qmljs_raw.object_set
    (Qmljs_types.context_raw (context object_))
    (raw object_)
    name
    (raw value)

let get_string_raw object_ name =
  let context = context object_ in
  let key = string context name in
  if
    Qmljs_raw.object_has
      (Qmljs_types.context_raw context)
      (raw object_)
      (raw key)
  then
    Some
      (wrap context
         (Qmljs_raw.object_get
            (Qmljs_types.context_raw context)
            (raw object_)
            (raw key)))
  else None

let new_object context =
  wrap context (Qmljs_raw.object_create (Qmljs_types.context_raw context))

let of_result ~ok ~error context = function
  | Ok value ->
      let object_ = new_object context in
      set_string_raw object_ "ok" (bool context true);
      set_string_raw object_ "value" (ok context value);
      object_
  | Error value ->
      let object_ = new_object context in
      set_string_raw object_ "ok" (bool context false);
      set_string_raw object_ "error" (error context value);
      object_

let require_object t =
  match kind t with
  | Object -> Ok ()
  | _ -> expected "object"

let require_property object_ name =
  match get_string_raw object_ name with
  | Some value -> Ok value
  | None -> conversion_error ("expected property " ^ name)

let to_result ~ok ~error t =
  capture_conversion @@ fun () ->
  let* () = require_object t in
  let* ok_value = require_property t "ok" in
  let* is_ok = as_bool ok_value in
  if is_ok then (
    let* value = require_property t "value" in
    let* value = ok value in
    Ok (Ok value))
  else (
    let* value = require_property t "error" in
    let* value = error value in
    Ok (Error value))

let of_assoc encode context fields =
  let object_ = new_object context in
  List.iter
    (fun (name, value) -> set_string_raw object_ name (encode context value))
    fields;
  object_

let to_assoc decode t =
  capture_conversion @@ fun () ->
  let* () = require_object t in
  let context = context t in
  let keys =
    Qmljs_raw.object_own_keys
      (Qmljs_types.context_raw context)
      (raw t)
      5
  in
  let rec loop index acc =
    if index = Stdlib.Array.length keys then Ok (List.rev acc)
    else
      let key = wrap context keys.(index) in
      let* name = as_string key in
      let value =
        wrap context
          (Qmljs_raw.object_get
             (Qmljs_types.context_raw context)
             (raw t)
             (raw key))
      in
      let* value = decode value in
      loop (index + 1) ((name, value) :: acc)
  in
  loop 0 []

let of_record = of_assoc
let to_record = to_assoc

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
