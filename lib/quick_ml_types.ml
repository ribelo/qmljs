type runtime =
  { raw : Quick_ml_raw.runtime
  ; host_jobs : (unit -> unit) Queue.t
  }

type context =
  { runtime : runtime
  ; raw : Quick_ml_raw.context
  }

type value =
  { context : context
  ; raw : Quick_ml_raw.value
  }

type promise_state =
  | Not_a_promise
  | Pending
  | Fulfilled
  | Rejected

let runtime_raw (t : runtime) = t.raw
let context_runtime (t : context) = t.runtime
let context_raw (t : context) = t.raw
let value_context (t : value) = t.context
let value_raw (t : value) = t.raw
