type t = Qmljs_types.runtime

let create () =
  { Qmljs_types.raw = Qmljs_raw.runtime_create ()
  ; host_jobs = Queue.create ()
  }

let is_job_pending t =
  Qmljs_raw.runtime_is_job_pending (Qmljs_types.runtime_raw t)

let execute_pending_job t =
  Qmljs_raw.runtime_execute_pending_job (Qmljs_types.runtime_raw t)

let request_interrupt t =
  Qmljs_raw.runtime_request_interrupt (Qmljs_types.runtime_raw t)

let clear_interrupt t =
  Qmljs_raw.runtime_clear_interrupt (Qmljs_types.runtime_raw t)

let interrupt_requested t =
  Qmljs_raw.runtime_interrupt_requested (Qmljs_types.runtime_raw t)

let drain_jobs t =
  let rec loop n =
    if execute_pending_job t then loop (n + 1) else n
  in
  loop 0

let enqueue_host_job t job = Queue.add job t.Qmljs_types.host_jobs

let execute_host_job t =
  match Queue.take_opt t.Qmljs_types.host_jobs with
  | None -> false
  | Some job ->
      job ();
      true
