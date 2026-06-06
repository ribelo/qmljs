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

let await_result ?yield_every promise =
  let runtime =
    Qmljs_types.context_runtime (Qmljs_types.value_context promise)
  in
  let rec loop () =
    ignore (Qmljs_async.drain_jobs ?yield_every runtime : int);
    match state promise with
    | Not_a_promise -> invalid_arg "value is not a promise"
    | Fulfilled -> Ok (result promise)
    | Rejected -> Error (structured_rejection (result promise))
    | Pending ->
        (match runtime.Qmljs_types.driver with
         | Some driver when not driver.Qmljs_types.closed ->
             Eio.Condition.await_no_mutex driver.Qmljs_types.condition
         | _ -> Eio.Fiber.yield ());
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
