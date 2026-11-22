type module_request_identity =
  | Normalized_request
  | Exact_request

type module_loader =
  { requests : (string, module_load_request) Hashtbl.t
  ; request_identity : module_request_identity
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

(* A driver only moves forward: Open -> Cancelled -> Closed, or Open -> Closed.
   Cancelled still lets queued cancellation settlements run; Closed drops all
   queued work because the context may already be gone. *)
type driver_state =
  | Open
  | Cancelled
  | Closed

(* Workers are the Async computations behind host promises. Async cannot
   interrupt a running Deferred, so driver teardown joins them instead, and
   module evaluation without a driver knows work is still in flight: [all_done]
   is filled when the last running worker finishes. *)
type host_workers =
  | Idle
  | Running of
      { count : int
      ; all_done : unit Async_kernel.Ivar.t
      }

(* All fields are touched only by the owner thread running the Async scheduler,
   so plain mutation is race-free; [Runtime.request_interrupt] stays the only
   cross-domain entry point. *)
type driver =
  { driver_runtime : runtime
  ; mutable state : driver_state
  ; cancelled : unit Async_kernel.Ivar.t
  ; mutable wakeup_generation : int
  ; mutable wakeup : unit Async_kernel.Ivar.t
  }

and host_job =
  { driver : driver option
  ; run : unit -> unit
  ; on_cancel : (unit -> unit) option
  }

and runtime =
  { raw : Qmljs_raw.runtime
  ; host_jobs : host_job Queue.t
  ; mutable workers : host_workers
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
