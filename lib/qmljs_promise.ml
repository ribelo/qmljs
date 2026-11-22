type state = Qmljs_types.promise_state =
  | Not_a_promise
  | Pending
  | Fulfilled
  | Rejected

let is value =
  let context = Qmljs_types.value_context value in
  Qmljs_raw.value_is_promise
    (Qmljs_types.context_raw context)
    (Qmljs_types.value_raw value)

let state value =
  let context = Qmljs_types.value_context value in
  match
    Qmljs_raw.value_promise_state
      (Qmljs_types.context_raw context)
      (Qmljs_types.value_raw value)
  with
  | -1 -> Not_a_promise
  | 0 -> Pending
  | 1 -> Fulfilled
  | 2 -> Rejected
  | state -> invalid_arg ("unknown QuickJS promise state " ^ string_of_int state)

let result value =
  match state value with
  | Pending -> invalid_arg "pending promise has no result"
  | Not_a_promise -> invalid_arg "value is not a promise"
  | Fulfilled | Rejected ->
      let context = Qmljs_types.value_context value in
      { Qmljs_types.context
      ; raw =
          Qmljs_raw.value_promise_result
            (Qmljs_types.context_raw context)
            (Qmljs_types.value_raw value)
      }

let create context =
  let promise, resolve, reject =
    Qmljs_raw.promise_create (Qmljs_types.context_raw context)
  in
  ( { Qmljs_types.context; raw = promise }
  , { Qmljs_types.context; raw = resolve }
  , { Qmljs_types.context; raw = reject } )

let structured_rejection reason =
  let context = Qmljs_types.value_context reason in
  match
    Qmljs_error.Structured.capture (fun () ->
        Qmljs_raw.value_raise_as_exception
          (Qmljs_types.context_raw context)
          (Qmljs_types.value_raw reason))
  with
  | Error error -> error
  | Ok () -> Qmljs_error.Structured.of_legacy_error "promise rejected"

let drive_module_evaluation ?(timed_out = fun () -> false) eval_result =
  if not (is eval_result) then Async_kernel.Deferred.unit
  else begin
    let runtime =
      Qmljs_types.context_runtime (Qmljs_types.value_context eval_result)
    in
    let drain () =
      Async_kernel.Deferred.map
        (Async_kernel.Monitor.try_with ~extract_exn:true (fun () ->
           Qmljs_driver.drain_jobs runtime))
        ~f:(function
          | Ok jobs -> jobs
          | Error exn ->
              Qmljs_runtime.reclaim_pending_jobs runtime;
              raise exn)
    in
    let settle () =
      match state eval_result with
      | Not_a_promise | Fulfilled -> `Done
      | Rejected ->
          Qmljs_error.Structured.raise (structured_rejection (result eval_result))
      | Pending -> `Pending
    in
    (* Module evaluation must reach a terminal state before the namespace is
       handed back: returning while top-level await is still pending would
       expose a half-evaluated module and hide queued async work. Drive with the
       same semantics as Promise.await — keep draining, wait on driver wakeups
       when a driver exists — but fail loudly when the module is permanently
       pending with no queued job, no running worker and no driver to make
       further progress, or when the caller's deadline passed while it was
       suspended. *)
    let rec loop () =
      let wakeup = Qmljs_runtime.driver_wakeup runtime in
      Async_kernel.Deferred.bind (drain ()) ~f:(fun jobs ->
        match settle () with
        | `Done -> Async_kernel.Deferred.unit
        | `Pending when timed_out () ->
            Qmljs_error.raise_timeout
              "JavaScript execution timed out while top-level await was pending"
        | `Pending ->
            (match wakeup with
             | Some wakeup ->
                 Async_kernel.Deferred.bind
                   (Qmljs_runtime.await_driver_wakeup wakeup)
                   ~f:loop
             | None ->
                 if jobs > 0 then loop ()
                 else if Qmljs_runtime.workers_running runtime then
                   (* Without a driver nothing signals the worker's settlement,
                      so poll once per cycle while it is in flight. *)
                   Async_kernel.Deferred.bind
                     (Async_kernel.Async_kernel_scheduler.yield ())
                     ~f:loop
                 else
                   Qmljs_error.raise_resource_limit
                     "module evaluation did not complete: top-level await is \
                      still pending and no driver can make progress"))
    in
    match settle () with
    | `Done -> Async_kernel.Deferred.unit
    | `Pending -> loop ()
  end

let await_result ?yield_every promise =
  let runtime =
    Qmljs_types.context_runtime (Qmljs_types.value_context promise)
  in
  let rec loop () =
    let wakeup = Qmljs_runtime.driver_wakeup runtime in
    Async_kernel.Deferred.bind
      (Qmljs_driver.drain_jobs ?yield_every runtime)
      ~f:(fun (_ : int) ->
        match state promise with
        | Not_a_promise -> invalid_arg "value is not a promise"
        | Fulfilled -> Async_kernel.Deferred.return (Ok (result promise))
        | Rejected ->
            Async_kernel.Deferred.return
              (Error (structured_rejection (result promise)))
        | Pending ->
            (* Without a driver nothing signals progress (a resolve function may
               be called from any Async job), so poll once per cycle. *)
            let next =
              match wakeup with
              | Some wakeup -> Qmljs_runtime.await_driver_wakeup wakeup
              | None -> Async_kernel.Async_kernel_scheduler.yield ()
            in
            Async_kernel.Deferred.bind next ~f:loop)
  in
  match state promise with
  | Not_a_promise ->
      Async_kernel.Deferred.return
        (Error (Qmljs_error.Structured.of_legacy_error "value is not a promise"))
  | Pending | Fulfilled | Rejected -> loop ()

let await_exn ?yield_every promise =
  Async_kernel.Deferred.map (await_result ?yield_every promise) ~f:(function
    | Ok value -> value
    | Error error -> Qmljs_error.Structured.raise error)

let await = await_exn
