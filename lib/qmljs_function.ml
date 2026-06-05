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

let set_global context ~name ~arity callback =
  Qmljs_context.set_global context ~name (make context ~name ~arity callback)

let set_global_async context ~name ~arity callback =
  set_global context ~name ~arity @@ fun args ->
  let promise, resolve, reject = Qmljs_promise.create context in
  Qmljs_runtime.enqueue_host_job
    (Qmljs_types.context_runtime context)
    (fun () ->
      match callback args with
      | result -> ignore (Qmljs_value.call resolve [| result |] : Qmljs_value.t)
      | exception exn ->
          let reason = Qmljs_value.string context (Printexc.to_string exn) in
          ignore (Qmljs_value.call reject [| reason |] : Qmljs_value.t));
  promise
