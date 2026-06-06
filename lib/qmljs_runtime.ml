type t = Qmljs_types.runtime

let create_with_limits ?memory_limit ?max_stack_size ?promise_job_limit () =
  let raw = Qmljs_raw.runtime_create () in
  Option.iter (Qmljs_raw.runtime_set_memory_limit raw) memory_limit;
  Option.iter (Qmljs_raw.runtime_set_max_stack_size raw) max_stack_size;
  { Qmljs_types.raw
  ; host_jobs = Queue.create ()
  ; driver = None
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
  let close () =
    let runtime_raw = Qmljs_types.runtime_raw runtime in
    ignore (Qmljs_raw.runtime_close runtime_raw : bool)
  in
  match f runtime with
  | result ->
      close ();
      result
  | exception exn ->
      let backtrace = Printexc.get_raw_backtrace () in
      (try close () with _ -> ());
      Printexc.raise_with_backtrace exn backtrace

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
    match t.Qmljs_types.promise_job_limit with
    | Some limit when n >= limit ->
        Qmljs_error.raise_resource_limit "QuickJS promise job limit exceeded"
    | _ -> if execute_pending_job t then loop (n + 1) else n
  in
  loop 0

(* Best-effort reclamation of queued QuickJS promise jobs after a hostile or
   interrupted evaluation. Each job is dequeued before it runs, so the queue
   drains even when individual jobs fail (e.g. at the memory ceiling), letting
   the runtime free memory and recover. *)
let reclaim_pending_jobs ?(limit = 1_000_000) t =
  let rec loop n =
    if n < limit && is_job_pending t then begin
      (try ignore (execute_pending_job t : bool) with _ -> ());
      loop (n + 1)
    end
  in
  loop 0

let attach_driver t driver =
  match t.Qmljs_types.driver with
  | Some _ -> invalid_arg "QuickJS runtime already has an Eio driver"
  | None -> t.Qmljs_types.driver <- Some driver

let detach_driver t driver =
  if t.Qmljs_types.driver == Some driver then t.Qmljs_types.driver <- None

let signal_driver t =
  match t.Qmljs_types.driver with
  | None -> ()
  | Some driver -> Eio.Condition.broadcast driver.Qmljs_types.condition

let enqueue_host_job t job =
  Queue.add job t.Qmljs_types.host_jobs;
  signal_driver t

let execute_host_job t =
  match Queue.take_opt t.Qmljs_types.host_jobs with
  | None -> false
  | Some job ->
      job ();
      signal_driver t;
      true

let set_module_loader t loader =
  t.Qmljs_types.module_loader <- Some loader;
  let entries =
    loader.Qmljs_types.sources
    |> Hashtbl.to_seq
    |> Array.of_seq
  in
  Qmljs_raw.runtime_install_module_loader
    (Qmljs_types.runtime_raw t)
    entries
    loader.Qmljs_types.deny
