type t = Qmljs_types.value

let wrap context raw = { Qmljs_types.context; raw }

let create ~target ~handler =
  let context = Qmljs_types.value_context target in
  wrap context
    (Qmljs_raw.proxy_create
       (Qmljs_types.context_raw context)
       (Qmljs_types.value_raw target)
       (Qmljs_types.value_raw handler))

let is_proxy value =
  Qmljs_raw.proxy_is
    (Qmljs_types.context_raw (Qmljs_types.value_context value))
    (Qmljs_types.value_raw value)

let target value =
  let context = Qmljs_types.value_context value in
  Option.map (wrap context)
    (Qmljs_raw.proxy_target
       (Qmljs_types.context_raw context)
       (Qmljs_types.value_raw value))

let handler value =
  let context = Qmljs_types.value_context value in
  Option.map (wrap context)
    (Qmljs_raw.proxy_handler
       (Qmljs_types.context_raw context)
       (Qmljs_types.value_raw value))
