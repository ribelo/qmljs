type t = Quick_ml_types.runtime

let create () =
  { Quick_ml_types.raw = Quick_ml_raw.runtime_create ()
  ; host_jobs = Queue.create ()
  }

let is_job_pending t =
  Quick_ml_raw.runtime_is_job_pending (Quick_ml_types.runtime_raw t)

let execute_pending_job t =
  Quick_ml_raw.runtime_execute_pending_job (Quick_ml_types.runtime_raw t)

let drain_jobs t =
  let rec loop n =
    if execute_pending_job t then loop (n + 1) else n
  in
  loop 0

let enqueue_host_job t job = Queue.add job t.Quick_ml_types.host_jobs

let execute_host_job t =
  match Queue.take_opt t.Quick_ml_types.host_jobs with
  | None -> false
  | Some job ->
      job ();
      true
