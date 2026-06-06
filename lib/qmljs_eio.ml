type driver = Qmljs_types.driver

let cancel driver =
  driver.Qmljs_types.cancelled <- true;
  driver.Qmljs_types.cancel ()

let drain_until_idle driver = driver.Qmljs_types.drain_until_idle ()

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

let enqueue_settlement ?on_cancel driver runtime job =
  if not driver.Qmljs_types.closed then
    Qmljs_runtime.enqueue_host_job ?on_cancel runtime (fun () ->
        if not driver.Qmljs_types.closed then job ())

let promise ?(name = "Qmljs.Eio.promise") driver context fn =
  if driver.Qmljs_types.closed then
    invalid_arg "QuickJS Eio driver is closed";
  let promise, resolve, reject = Qmljs_promise.create context in
  let runtime = Qmljs_types.context_runtime context in
  Eio.Fiber.fork ~sw:driver.Qmljs_types.sw (fun () ->
      match fn () with
      | result ->
          let on_cancel () =
            reject_with_exn context reject name
              (Eio.Cancel.Cancelled (Failure "Eio driver cancelled"))
          in
          enqueue_settlement ~on_cancel driver runtime (fun () ->
              ignore (Qmljs_value.call resolve [| result |] : Qmljs_value.t))
      | exception (Eio.Cancel.Cancelled _ as exn) ->
          Eio.Cancel.protect (fun () ->
              enqueue_settlement driver runtime (fun () ->
                  reject_with_exn context reject name exn))
      | exception exn ->
          enqueue_settlement driver runtime (fun () ->
              reject_with_exn context reject name exn));
  promise

let with_runtime ~sw ~clock:_ ?memory_limit ?max_stack_size
    ?promise_job_limit f =
  Qmljs_runtime.with_ ?memory_limit ?max_stack_size ?promise_job_limit
    @@ fun runtime ->
  Qmljs_context.with_ runtime @@ fun context ->
  let condition = Eio.Condition.create () in
  let driver =
    { Qmljs_types.sw
    ; condition
    ; closed = false
    ; cancelled = false
    ; drain_until_idle = (fun () -> Qmljs_async.drain_jobs runtime)
    ; cancel =
        (fun () ->
          Qmljs_runtime.request_interrupt runtime;
          Eio.Condition.broadcast condition)
    }
  in
  Qmljs_runtime.attach_driver runtime driver;
  Fun.protect
    ~finally:(fun () ->
      driver.Qmljs_types.closed <- true;
      Qmljs_runtime.detach_driver runtime driver;
      Eio.Condition.broadcast condition)
    (fun () -> f driver runtime context)
