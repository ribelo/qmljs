type driver = Qmljs_types.driver

let cancel driver = driver.Qmljs_types.cancel ()

let drain_until_idle driver = driver.Qmljs_types.drain_until_idle ()

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
