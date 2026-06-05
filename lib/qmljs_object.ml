type t = Qmljs_types.value

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
