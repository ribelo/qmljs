let make context ~name ~arity callback =
  if arity < 0 then invalid_arg "arity must be non-negative";
  let raw_callback raw_args =
    let args =
      Array.map
        (fun raw -> { Quick_ml_types.context; raw })
        raw_args
    in
    Quick_ml_types.value_raw (callback args)
  in
  { Quick_ml_types.context
  ; raw =
      Quick_ml_raw.function_make
        (Quick_ml_types.context_raw context)
        name
        arity
        raw_callback
  }

let set_global context ~name ~arity callback =
  Quick_ml_context.set_global context ~name (make context ~name ~arity callback)

let set_global_async context ~name ~arity callback =
  set_global context ~name ~arity @@ fun args ->
  let promise, resolve, reject = Quick_ml_promise.create context in
  Quick_ml_runtime.enqueue_host_job
    (Quick_ml_types.context_runtime context)
    (fun () ->
      match callback args with
      | result -> ignore (Quick_ml_value.call resolve [| result |] : Quick_ml_value.t)
      | exception exn ->
          let reason = Quick_ml_value.string context (Printexc.to_string exn) in
          ignore (Quick_ml_value.call reject [| reason |] : Quick_ml_value.t));
  promise
