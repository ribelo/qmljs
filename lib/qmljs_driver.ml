(* The one place that advances QuickJS promise jobs and qmljs host jobs, and
   that runs host work as Async workers. Promise.await, module evaluation,
   Qmljs.Async and async host callbacks all delegate here. *)

let drain_jobs ?(yield_every = 64) runtime =
  if yield_every <= 0 then invalid_arg "yield_every must be positive";
  let rec loop jobs quickjs_jobs =
    if Qmljs_runtime.execute_host_job runtime then next (jobs + 1) quickjs_jobs
    else if Qmljs_runtime.is_job_pending runtime then begin
      (match runtime.Qmljs_types.promise_job_limit with
       | Some limit when quickjs_jobs >= limit ->
           Qmljs_error.raise_resource_limit
             "QuickJS promise job limit exceeded"
       | Some _ | None -> ());
      ignore (Qmljs_runtime.execute_pending_job runtime : bool);
      next (jobs + 1) (quickjs_jobs + 1)
    end
    else begin
      Stdlib.Gc.full_major ();
      Qmljs_runtime.run_gc runtime;
      Async_kernel.Deferred.return jobs
    end
  and next jobs quickjs_jobs =
    if jobs mod yield_every = 0 then
      Async_kernel.Deferred.bind
        (Async_kernel.Async_kernel_scheduler.yield ())
        ~f:(fun () -> loop jobs quickjs_jobs)
    else loop jobs quickjs_jobs
  in
  loop 0 0

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

(* Settlement runs as a host job with no JS caller to report to: a failure to
   reject (the context is closing, or memory is exhausted) leaves nothing more
   useful to do with the promise. *)
let reject_with_exn_noerr context reject name exn =
  try reject_with_exn context reject name exn with
  | _ -> ()

let resolve_or_reject context ~name ~resolve ~reject result =
  match Qmljs_value.call resolve [| result |] with
  | _ -> ()
  | exception exn -> reject_with_exn_noerr context reject name exn

let reject_cancelled context ~name ~reject =
  reject_with_exn_noerr context reject name
    (Qmljs_error.Structured.host
       ~message:"async host callback cancelled"
       ~metadata:[ "function", name; "cancelled", "true" ])

(* Runs on the owner as a host job. The runtime tracks the worker, so driver
   teardown joins it before the context closes and driverless module evaluation
   knows work is still in flight. Its outcome is settled through another host
   job: a cancelled driver rejects with structured cancellation instead, and a
   closed driver drops it without touching QuickJS. *)
let start_worker context ~name ~resolve ~reject work =
  let runtime = Qmljs_types.context_runtime context in
  let driver = runtime.Qmljs_types.driver in
  Qmljs_runtime.worker_started runtime;
  Async_kernel.Deferred.upon
    (Async_kernel.Monitor.try_with ~extract_exn:true work)
    (fun outcome ->
      let settle () =
        match outcome with
        | Ok result -> resolve_or_reject context ~name ~resolve ~reject result
        | Error exn -> reject_with_exn_noerr context reject name exn
      in
      (match driver with
       | Some driver when Qmljs_runtime.driver_closed driver -> ()
       | Some _ | None ->
           Qmljs_runtime.enqueue_host_job
             ~on_cancel:(fun () -> reject_cancelled context ~name ~reject)
             runtime settle);
      Qmljs_runtime.worker_finished runtime)

(* Creates the QuickJS promise now and starts [work] from the host queue, so a
   driver cancelled before the queue is drained rejects the promise without ever
   running [work]. *)
let host_promise context ~name work =
  let promise, resolve, reject =
    Qmljs_raw.promise_create (Qmljs_types.context_raw context)
  in
  let wrap raw = { Qmljs_types.context; raw } in
  let resolve = wrap resolve and reject = wrap reject in
  Qmljs_runtime.enqueue_host_job
    ~on_cancel:(fun () -> reject_cancelled context ~name ~reject)
    (Qmljs_types.context_runtime context)
    (fun () -> start_worker context ~name ~resolve ~reject work);
  wrap promise
