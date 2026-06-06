type module_loader =
  { requests : (string, module_load_request) Hashtbl.t
  ; deny : string -> bool
  ; normalize :
      base:string option ->
      specifier:string ->
      attributes:(string * string) list ->
      (string, Qmljs_error.Structured.t) result
  ; load :
      module_load_request ->
      (string option, Qmljs_error.Structured.t) result
  }

and module_load_request =
  { base : string option
  ; specifier : string
  ; normalized : string
  ; attributes : (string * string) list
  }

type driver =
  { sw : Eio.Switch.t
  ; condition : Eio.Condition.t
  ; mutex : Eio.Mutex.t
  ; mutable wakeup_generation : int
  ; mutable closed : bool
  ; mutable cancelled : bool
  ; drain_until_idle : unit -> int
  ; cancel : unit -> unit
  }

type host_job =
  { driver : driver option
  ; run : unit -> unit
  ; on_cancel : (unit -> unit) option
  }

type runtime =
  { raw : Qmljs_raw.runtime
  ; host_jobs : host_job Queue.t
  ; mutable driver : driver option
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

type atom =
  { atom_runtime : runtime
  ; atom_raw : Qmljs_raw.atom
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
let atom_runtime (t : atom) = t.atom_runtime
let atom_raw (t : atom) = t.atom_raw
