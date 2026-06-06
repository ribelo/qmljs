type call =
  { this_ : Qmljs_types.value
  ; args : Qmljs_types.value array
  ; new_target : Qmljs_types.value option
  }

let make context ~name ~arity callback =
  if arity < 0 then invalid_arg "arity must be non-negative";
  let raw_callback raw_args =
    let args =
      Array.map
        (fun raw -> { Qmljs_types.context; raw })
        raw_args
    in
    Qmljs_types.value_raw (callback args)
  in
  { Qmljs_types.context
  ; raw =
      Qmljs_raw.function_make
        (Qmljs_types.context_raw context)
        name
        arity
        raw_callback
  }

let make_with_this context ~name ~arity callback =
  if arity < 0 then invalid_arg "arity must be non-negative";
  let wrap raw = { Qmljs_types.context; raw } in
  let raw_callback raw_this raw_args raw_new_target =
    let call =
      { this_ = wrap raw_this
      ; args = Array.map wrap raw_args
      ; new_target = Option.map wrap raw_new_target
      }
    in
    Qmljs_types.value_raw (callback call)
  in
  { Qmljs_types.context
  ; raw =
      Qmljs_raw.function_make_with_this
        (Qmljs_types.context_raw context)
        name
        arity
        raw_callback
  }

let set_global context ~name ~arity callback =
  Qmljs_context.set_global context ~name (make context ~name ~arity callback)

let set_global_with_this context ~name ~arity callback =
  Qmljs_context.set_global context ~name
    (make_with_this context ~name ~arity callback)

let reject_with_exn context reject name exn =
  let reason =
    { Qmljs_types.context
    ; raw =
        Qmljs_raw.function_host_error
          (Qmljs_types.context_raw context)
          name
          (Qmljs_error.host_exception_message exn)
          (Qmljs_error.host_exception_metadata exn)
    }
  in
  ignore (Qmljs_value.call reject [| reason |] : Qmljs_value.t)

let set_global_async context ~name ~arity callback =
  set_global context ~name ~arity @@ fun args ->
  let promise, resolve, reject = Qmljs_promise.create context in
  let on_cancel () =
    reject_with_exn context reject name
      (Qmljs_error.Structured.host
         ~message:"async host callback cancelled"
         ~metadata:[ "function", name; "cancelled", "true" ])
  in
  Qmljs_runtime.enqueue_host_job
    ~on_cancel
    (Qmljs_types.context_runtime context)
    (fun () ->
      match callback args with
      | result -> ignore (Qmljs_value.call resolve [| result |] : Qmljs_value.t)
      | exception exn -> reject_with_exn context reject name exn);
  promise

let call ?this_ t args =
  let context = Qmljs_types.value_context t in
  let this_ =
    match this_ with
    | Some this_ -> this_
    | None -> Qmljs_value.undefined context
  in
  { Qmljs_types.context
  ; raw =
      Qmljs_raw.function_call
        (Qmljs_types.context_raw context)
        (Qmljs_types.value_raw t)
        (Qmljs_types.value_raw this_)
        (Array.map Qmljs_types.value_raw args)
  }

let construct t args =
  let context = Qmljs_types.value_context t in
  { Qmljs_types.context
  ; raw =
      Qmljs_raw.function_construct
        (Qmljs_types.context_raw context)
        (Qmljs_types.value_raw t)
        (Array.map Qmljs_types.value_raw args)
  }

let defer ?this_ t args =
  let context = Qmljs_types.value_context t in
  let this_ =
    match this_ with
    | Some this_ -> this_
    | None -> Qmljs_value.undefined context
  in
  Qmljs_raw.function_defer
    (Qmljs_types.context_raw context)
    (Qmljs_types.value_raw t)
    (Qmljs_types.value_raw this_)
    (Array.map Qmljs_types.value_raw args)

let name t =
  Qmljs_raw.function_name
    (Qmljs_types.context_raw (Qmljs_types.value_context t))
    (Qmljs_types.value_raw t)

let set_name t name =
  Qmljs_raw.function_set_name
    (Qmljs_types.context_raw (Qmljs_types.value_context t))
    (Qmljs_types.value_raw t)
    name

let length t =
  Qmljs_raw.function_length
    (Qmljs_types.context_raw (Qmljs_types.value_context t))
    (Qmljs_types.value_raw t)

let set_length t length =
  if length < 0 then invalid_arg "function length must be non-negative";
  Qmljs_raw.function_set_length
    (Qmljs_types.context_raw (Qmljs_types.value_context t))
    (Qmljs_types.value_raw t)
    length

let prototype context =
  { Qmljs_types.context
  ; raw = Qmljs_raw.function_prototype (Qmljs_types.context_raw context)
  }

let is_constructor t =
  Qmljs_raw.function_is_constructor
    (Qmljs_types.context_raw (Qmljs_types.value_context t))
    (Qmljs_types.value_raw t)

let set_constructor t is_constructor =
  Qmljs_raw.function_set_constructor
    (Qmljs_types.context_raw (Qmljs_types.value_context t))
    (Qmljs_types.value_raw t)
    is_constructor
