type t = Qmljs_types.runtime

let create_with_limits ?memory_limit ?max_stack_size ?promise_job_limit () =
  let raw = Qmljs_raw.runtime_create () in
  Option.iter (Qmljs_raw.runtime_set_memory_limit raw) memory_limit;
  Option.iter (Qmljs_raw.runtime_set_max_stack_size raw) max_stack_size;
  { Qmljs_types.raw
  ; host_jobs = Queue.create ()
  ; module_loader = None
  ; memory_limit
  ; max_stack_size
  ; promise_job_limit
  }

let create () = create_with_limits ()

let with_ ?memory_limit ?max_stack_size ?promise_job_limit f =
  let runtime =
    create_with_limits ?memory_limit ?max_stack_size ?promise_job_limit ()
  in
  Fun.protect
    ~finally:(fun () ->
      Gc.full_major ();
      Qmljs_raw.runtime_drain_deferred (Qmljs_types.runtime_raw runtime);
      Qmljs_raw.runtime_close (Qmljs_types.runtime_raw runtime))
    (fun () -> f runtime)

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

let set_module_loader t loader = t.Qmljs_types.module_loader <- Some loader
