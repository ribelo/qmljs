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

let driver_wakeup_of runtime =
  match runtime.Qmljs_types.driver with
  | Some driver ->
      Option.map
        (fun generation -> driver, generation)
        (Qmljs_runtime.driver_wakeup_generation driver)
  | None -> None

let drive_module_evaluation eval_result =
  if is eval_result then begin
    let runtime =
      Qmljs_types.context_runtime (Qmljs_types.value_context eval_result)
    in
    let drain () =
      try Qmljs_async.drain_jobs runtime with
      | exn ->
          let backtrace = Printexc.get_raw_backtrace () in
          Qmljs_runtime.reclaim_pending_jobs runtime;
          Printexc.raise_with_backtrace exn backtrace
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
       pending and no driver can make further progress. *)
    let rec loop () =
      let wakeup = driver_wakeup_of runtime in
      let jobs = drain () in
      match settle () with
      | `Done -> ()
      | `Pending ->
          (match wakeup with
           | Some (driver, generation) ->
               Qmljs_runtime.await_driver_wakeup driver generation;
               loop ()
           | None ->
               if jobs > 0 then loop ()
               else
                 Qmljs_error.raise_resource_limit
                   "module evaluation did not complete: top-level await is \
                    still pending and no driver can make progress")
    in
    match settle () with
    | `Done -> ()
    | `Pending -> loop ()
  end

let await_result ?yield_every promise =
  let runtime =
    Qmljs_types.context_runtime (Qmljs_types.value_context promise)
  in
  let driver_wakeup () = driver_wakeup_of runtime in
  let rec loop () =
    let wakeup = driver_wakeup () in
    ignore (Qmljs_async.drain_jobs ?yield_every runtime : int);
    match state promise with
    | Not_a_promise -> invalid_arg "value is not a promise"
    | Fulfilled -> Ok (result promise)
    | Rejected -> Error (structured_rejection (result promise))
    | Pending ->
        (match wakeup with
         | Some (driver, generation) ->
             Qmljs_runtime.await_driver_wakeup driver generation
         | None -> Eio.Fiber.yield ());
        loop ()
  in
  match state promise with
  | Not_a_promise ->
      Error (Qmljs_error.Structured.of_legacy_error "value is not a promise")
  | Pending | Fulfilled | Rejected -> loop ()

let await_exn ?yield_every promise =
  match await_result ?yield_every promise with
  | Ok value -> value
  | Error error -> Qmljs_error.Structured.raise error

let await = await_exn
