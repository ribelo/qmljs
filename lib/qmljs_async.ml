type driver = Qmljs_types.driver

let drain_jobs = Qmljs_driver.drain_jobs
let cancel = Qmljs_runtime.cancel_driver
let cancelled = Qmljs_runtime.driver_cancelled

let drain_until_idle (driver : driver) =
  Qmljs_driver.drain_jobs driver.driver_runtime

let promise ?(name = "Qmljs.Async.promise") driver context work =
  if Qmljs_runtime.driver_closed driver then
    invalid_arg "QuickJS Async driver is closed";
  (match (Qmljs_types.context_runtime context).Qmljs_types.driver with
   | Some attached when attached == driver -> ()
   | Some _ | None ->
       invalid_arg "QuickJS context does not belong to this Async driver");
  Qmljs_driver.host_promise context ~name work

(* Async.Monitor.protect is not used for teardown: when both the body and the
   finalizer fail it raises an opaque pair, while qmljs keeps the body failure
   and attaches the cleanup failure as suppressed structure. *)
let with_runtime ?memory_limit ?max_stack_size ?promise_job_limit f =
  let runtime =
    Qmljs_runtime.create_with_limits ?memory_limit ?max_stack_size
      ?promise_job_limit ()
  in
  let close_runtime outcome =
    Qmljs_runtime.close_scope outcome ~close_error:(fun () ->
      Qmljs_runtime.close_error runtime)
  in
  match Qmljs_context.create runtime with
  | exception exn -> close_runtime (Error (exn, Printexc.get_raw_backtrace ()))
  | context ->
      let driver = Qmljs_runtime.attach_driver runtime in
      Async_kernel.Deferred.bind
        (Async_kernel.Monitor.try_with ~extract_exn:true (fun () ->
           f driver runtime context))
        ~f:(fun outcome ->
          (* Closing stops in-flight JS, wakes waiters and tells cooperative
             workers to finish; joining them keeps every worker inside the
             context's lifetime. *)
          Qmljs_runtime.close_driver driver;
          Async_kernel.Deferred.map
            (Qmljs_runtime.workers_done runtime)
            ~f:(fun () ->
              Qmljs_runtime.detach_driver runtime driver;
              let outcome =
                Result.map_error
                  (fun exn -> exn, Printexc.get_callstack 0)
                  outcome
              in
              close_runtime
                (Qmljs_runtime.capture_scope (fun () ->
                   Qmljs_runtime.close_scope outcome ~close_error:(fun () ->
                     Qmljs_context.close_error context)))))
