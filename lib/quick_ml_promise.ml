type state = Quick_ml_types.promise_state =
  | Not_a_promise
  | Pending
  | Fulfilled
  | Rejected

let is value = Quick_ml_raw.value_is_promise (Quick_ml_types.value_raw value)

let state value =
  let context = Quick_ml_types.value_context value in
  match
    Quick_ml_raw.value_promise_state
      (Quick_ml_types.context_raw context)
      (Quick_ml_types.value_raw value)
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
      let context = Quick_ml_types.value_context value in
      { Quick_ml_types.context
      ; raw =
          Quick_ml_raw.value_promise_result
            (Quick_ml_types.context_raw context)
            (Quick_ml_types.value_raw value)
      }

let await ?yield_every promise =
  let rec loop () =
    ignore
      ( Quick_ml_async.drain_jobs ?yield_every
          (Quick_ml_types.context_runtime (Quick_ml_types.value_context promise))
        : int );
    match state promise with
    | Not_a_promise -> invalid_arg "value is not a promise"
    | Fulfilled -> result promise
    | Rejected ->
        let reason = result promise in
        raise (Quick_ml_error.Error (Quick_ml_value.to_string reason))
    | Pending ->
        Eio.Fiber.yield ();
        loop ()
  in
  match state promise with
  | Not_a_promise -> promise
  | Pending | Fulfilled | Rejected -> loop ()
