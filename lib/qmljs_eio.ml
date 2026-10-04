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
          (Qmljs_error.host_exception_marker name exn)
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
  let runtime = Qmljs_types.context_runtime context in
  (match runtime.Qmljs_types.driver with
   | Some attached when attached == driver -> ()
   | _ -> invalid_arg "QuickJS context does not belong to this Eio driver");
  let promise, resolve, reject = Qmljs_promise.create context in
  let reject_cancelled () =
    reject_with_exn_noerr context reject name
      (Eio.Cancel.Cancelled (Failure "Eio driver cancelled"))
  in
  let start_worker () =
    Eio.Fiber.fork ~sw:driver.Qmljs_types.worker_switch (fun () ->
        match fn () with
        | result ->
            enqueue_settlement ~on_cancel:reject_cancelled driver runtime (fun () ->
                resolve_or_reject context ~name ~resolve ~reject result)
        | exception (Eio.Cancel.Cancelled _ as exn) ->
            (* The worker scope was cancelled (driver cancel/close). Reject the
               promise with structured cancellation; the rejection runs even on
               the cancelled path (on_cancel), but is dropped if the driver has
               already closed and the context is gone. *)
            Eio.Cancel.protect (fun () ->
                let do_reject () = reject_with_exn_noerr context reject name exn in
                enqueue_settlement ~on_cancel:do_reject driver runtime do_reject)
        | exception exn ->
            enqueue_settlement driver runtime (fun () ->
                reject_with_exn_noerr context reject name exn))
  in
  enqueue_settlement ~on_cancel:reject_cancelled driver runtime start_worker;
  promise

let with_runtime ~sw ~clock:_ ?memory_limit ?max_stack_size
    ?promise_job_limit f =
  Qmljs_runtime.with_ ?memory_limit ?max_stack_size ?promise_job_limit
    @@ fun runtime ->
  Qmljs_context.with_ runtime @@ fun context ->
  (* A driver-owned scope hosts every Qmljs.Eio.promise worker. The host fiber
     keeps a worker switch and its cancel context alive until the driver is
     cancelled or closed, at which point all workers are cancelled together
     rather than outliving the driver. *)
  let setup_promise, setup_resolver = Eio.Promise.create () in
  let workers_done, finish_workers = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      Fun.protect
        ~finally:(fun () -> Eio.Promise.resolve finish_workers ())
        (fun () ->
          try
            Eio.Cancel.sub (fun worker_cancel ->
                Eio.Switch.run (fun worker_switch ->
                    Eio.Promise.resolve setup_resolver
                      (worker_switch, worker_cancel);
                    Eio.Fiber.await_cancel ()))
          with Eio.Cancel.Cancelled _ -> ()));
  let worker_switch, worker_cancel = Eio.Promise.await setup_promise in
  let condition = Eio.Condition.create () in
  let mutex = Eio.Mutex.create () in
  let driver =
    { Qmljs_types.sw
    ; worker_switch
    ; worker_cancel
    ; worker_scope_cancelled = false
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
    ~finally:(fun () -> Eio.Cancel.protect (fun () ->
      Qmljs_runtime.close_driver driver;
      (* Cancellation requests shutdown; joining keeps worker cleanup inside the
         context bracket, including when the calling fiber is cancelled. *)
      Eio.Promise.await workers_done;
      Qmljs_runtime.detach_driver runtime driver))
    (fun () -> f driver runtime context)
