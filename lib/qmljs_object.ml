type t = Qmljs_types.value

type descriptor =
  { value : Qmljs_types.value option
  ; writable : bool option
  ; enumerable : bool
  ; configurable : bool
  ; get : Qmljs_types.value option
  ; set : Qmljs_types.value option
  }

let create context =
  { Qmljs_types.context
  ; raw = Qmljs_raw.object_create (Qmljs_types.context_raw context)
  }

let set t ~name value =
  let context = Qmljs_types.value_context t in
  Qmljs_raw.object_set
    (Qmljs_types.context_raw context)
    (Qmljs_types.value_raw t)
    name
    (Qmljs_types.value_raw value)

let define_data_property t ~key ~value ~writable ~enumerable ~configurable =
  let flags =
    (if writable then 1 else 0)
    lor (if enumerable then 2 else 0)
    lor (if configurable then 4 else 0)
  in
  let context = Qmljs_types.value_context t in
  Qmljs_raw.object_define_data_property
    (Qmljs_types.context_raw context)
    (Qmljs_types.value_raw t)
    (Qmljs_types.value_raw key)
    (Qmljs_types.value_raw value)
    flags

let wrap context raw = { Qmljs_types.context; raw }

let get t ~key =
  let context = Qmljs_types.value_context t in
  wrap context
    (Qmljs_raw.object_get
       (Qmljs_types.context_raw context)
       (Qmljs_types.value_raw t)
       (Qmljs_types.value_raw key))

let has t ~key =
  let context = Qmljs_types.value_context t in
  Qmljs_raw.object_has
    (Qmljs_types.context_raw context)
    (Qmljs_types.value_raw t)
    (Qmljs_types.value_raw key)

let delete t ~key =
  let context = Qmljs_types.value_context t in
  Qmljs_raw.object_delete
    (Qmljs_types.context_raw context)
    (Qmljs_types.value_raw t)
    (Qmljs_types.value_raw key)

let get_own_property_descriptor t ~key =
  let context = Qmljs_types.value_context t in
  let wrap_option = Option.map (wrap context) in
  match
    Qmljs_raw.object_get_own_property_descriptor
      (Qmljs_types.context_raw context)
      (Qmljs_types.value_raw t)
      (Qmljs_types.value_raw key)
  with
  | None -> None
  | Some (value, writable, enumerable, configurable, get, set) ->
      Some
        { value = wrap_option value
        ; writable
        ; enumerable
        ; configurable
        ; get = wrap_option get
        ; set = wrap_option set
        }

let keys t =
  let context = Qmljs_types.value_context t in
  Qmljs_raw.object_keys
    (Qmljs_types.context_raw context)
    (Qmljs_types.value_raw t)
  |> Array.to_list
