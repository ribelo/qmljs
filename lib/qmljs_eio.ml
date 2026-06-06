type driver = Qmljs_types.driver

let cancel driver =
  Qmljs_runtime.cancel_driver driver

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

let reject_with_exn_noerr context reject name exn =
  try reject_with_exn context reject name exn with
  | _ -> ()

let resolve_or_reject context ~name ~resolve ~reject result =
  match Qmljs_value.call resolve [| result |] with
  | _ -> ()
  | exception exn -> reject_with_exn_noerr context reject name exn

let enqueue_settlement ?on_cancel driver runtime job =
  if not (Qmljs_runtime.driver_closed driver) then
    Qmljs_runtime.enqueue_host_job ?on_cancel runtime (fun () ->
        if not (Qmljs_runtime.driver_closed driver) then job ())

let promise ?(name = "Qmljs.Eio.promise") driver context fn =
  if Qmljs_runtime.driver_closed driver then
    invalid_arg "QuickJS Eio driver is closed";
  let promise, resolve, reject = Qmljs_promise.create context in
  let runtime = Qmljs_types.context_runtime context in
  Eio.Fiber.fork ~sw:driver.Qmljs_types.sw (fun () ->
      match fn () with
      | result ->
          let on_cancel () =
            reject_with_exn_noerr context reject name
              (Eio.Cancel.Cancelled (Failure "Eio driver cancelled"))
          in
          enqueue_settlement ~on_cancel driver runtime (fun () ->
              resolve_or_reject context ~name ~resolve ~reject result)
      | exception (Eio.Cancel.Cancelled _ as exn) ->
          Eio.Cancel.protect (fun () ->
              enqueue_settlement driver runtime (fun () ->
                  reject_with_exn_noerr context reject name exn))
      | exception exn ->
          enqueue_settlement driver runtime (fun () ->
              reject_with_exn_noerr context reject name exn));
    promise

let with_runtime ~sw ~clock:_ ?memory_limit ?max_stack_size
    ?promise_job_limit f =
  Qmljs_runtime.with_ ?memory_limit ?max_stack_size ?promise_job_limit
    @@ fun runtime ->
  Qmljs_context.with_ runtime @@ fun context ->
  let condition = Eio.Condition.create () in
  let mutex = Eio.Mutex.create () in
  let driver =
    { Qmljs_types.sw
    ; condition
    ; mutex
    ; wakeup_generation = 0
    ; closed = false
    ; cancelled = false
    ; drain_until_idle = (fun () -> Qmljs_async.drain_jobs runtime)
    ; cancel = (fun () -> Qmljs_runtime.request_interrupt runtime)
    }
  in
  Qmljs_runtime.attach_driver runtime driver;
  Fun.protect
    ~finally:(fun () ->
      Qmljs_runtime.close_driver driver;
      Qmljs_runtime.detach_driver runtime driver;
      ())
    (fun () -> f driver runtime context)
