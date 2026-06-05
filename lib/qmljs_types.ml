type module_loader =
  { sources : (string, string) Hashtbl.t
  ; deny : string -> bool
  }

type runtime =
  { raw : Qmljs_raw.runtime
  ; host_jobs : (unit -> unit) Queue.t
  ; mutable module_loader : module_loader option
  ; memory_limit : int option
  ; max_stack_size : int option
  ; promise_job_limit : int option
  }

type context =
  { runtime : runtime
  ; raw : Qmljs_raw.context
  }

type value =
  { context : context
  ; raw : Qmljs_raw.value
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
