type t = Qmljs_types.runtime

type memory_usage =
  { malloc_size : int64
  ; malloc_limit : int64
  ; memory_used_size : int64
  ; malloc_count : int64
  ; memory_used_count : int64
  ; atom_count : int64
  ; atom_size : int64
  ; str_count : int64
  ; str_size : int64
  ; obj_count : int64
  ; obj_size : int64
  ; prop_count : int64
  ; prop_size : int64
  ; shape_count : int64
  ; shape_size : int64
  ; js_func_count : int64
  ; js_func_size : int64
  ; js_func_code_size : int64
  ; js_func_pc2line_count : int64
  ; js_func_pc2line_size : int64
  ; c_func_count : int64
  ; array_count : int64
  ; fast_array_count : int64
  ; fast_array_elements : int64
  ; binary_object_count : int64
  ; binary_object_size : int64
  }

type promise_rejection =
  { reason : Qmljs_error.Structured.t
  ; is_handled : bool
  }

type promise_hook_kind =
  | Promise_init
  | Promise_before
  | Promise_after
  | Promise_resolve

type promise_hook =
  { hook : promise_hook_kind
  ; state : Qmljs_types.promise_state
  ; has_parent : bool
  }

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

let memory_usage t =
  match Qmljs_raw.runtime_memory_usage (Qmljs_types.runtime_raw t) with
  | [| malloc_size
     ; malloc_limit
     ; memory_used_size
     ; malloc_count
     ; memory_used_count
     ; atom_count
     ; atom_size
     ; str_count
     ; str_size
     ; obj_count
     ; obj_size
     ; prop_count
     ; prop_size
     ; shape_count
     ; shape_size
     ; js_func_count
     ; js_func_size
     ; js_func_code_size
     ; js_func_pc2line_count
     ; js_func_pc2line_size
     ; c_func_count
     ; array_count
     ; fast_array_count
     ; fast_array_elements
     ; binary_object_count
     ; binary_object_size
    |] ->
      { malloc_size
      ; malloc_limit
      ; memory_used_size
      ; malloc_count
      ; memory_used_count
      ; atom_count
      ; atom_size
      ; str_count
      ; str_size
      ; obj_count
      ; obj_size
      ; prop_count
      ; prop_size
      ; shape_count
      ; shape_size
      ; js_func_count
      ; js_func_size
      ; js_func_code_size
      ; js_func_pc2line_count
      ; js_func_pc2line_size
      ; c_func_count
      ; array_count
      ; fast_array_count
      ; fast_array_elements
      ; binary_object_count
      ; binary_object_size
      }
  | _ -> invalid_arg "unexpected QuickJS memory usage snapshot shape"

let run_gc t = Qmljs_raw.runtime_run_gc (Qmljs_types.runtime_raw t)
let gc_threshold t = Qmljs_raw.runtime_get_gc_threshold (Qmljs_types.runtime_raw t)

let set_info t info =
  if String.contains info '\000' then
    invalid_arg "runtime info cannot contain NUL bytes";
  Qmljs_raw.runtime_set_info (Qmljs_types.runtime_raw t) info

let set_promise_rejection_tracking t enabled =
  Qmljs_raw.runtime_set_promise_rejection_tracking
    (Qmljs_types.runtime_raw t)
    enabled

let take_promise_rejections t =
  Qmljs_raw.runtime_take_promise_rejections (Qmljs_types.runtime_raw t)
  |> Array.map (fun (reason, is_handled) -> { reason; is_handled })
  |> Array.to_list

let promise_hook_kind_of_int = function
  | 0 -> Promise_init
  | 1 -> Promise_before
  | 2 -> Promise_after
  | 3 -> Promise_resolve
  | hook -> invalid_arg ("unknown QuickJS promise hook " ^ string_of_int hook)

let promise_state_of_int = function
  | -1 -> Qmljs_types.Not_a_promise
  | 0 -> Qmljs_types.Pending
  | 1 -> Qmljs_types.Fulfilled
  | 2 -> Qmljs_types.Rejected
  | state -> invalid_arg ("unknown QuickJS promise state " ^ string_of_int state)

let set_promise_hook_tracking t enabled =
  Qmljs_raw.runtime_set_promise_hook_tracking
    (Qmljs_types.runtime_raw t)
    enabled

let take_promise_hooks t =
  Qmljs_raw.runtime_take_promise_hooks (Qmljs_types.runtime_raw t)
  |> Array.map (fun (hook, state, has_parent) ->
    { hook = promise_hook_kind_of_int hook
    ; state = promise_state_of_int state
    ; has_parent
    })
  |> Array.to_list

let set_gc_threshold t threshold =
  if threshold < 0 then invalid_arg "GC threshold must be non-negative";
  Qmljs_raw.runtime_set_gc_threshold (Qmljs_types.runtime_raw t) threshold

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

let execute_host_job t =
  ignore (Qmljs_raw.runtime_is_job_pending (Qmljs_types.runtime_raw t) : bool);
  match Queue.take_opt t.Qmljs_types.host_jobs with
  | None -> false
  | Some job ->
      let drop, cancel =
        match job.Qmljs_types.driver with
        | None -> false, false
        | Some driver ->
            driver.Qmljs_types.closed, driver.Qmljs_types.cancelled
      in
      if drop then ()
      else if cancel then Option.iter (fun cancel -> cancel ()) job.on_cancel
      else job.run ();
      signal_driver t;
      true

let enqueue_host_job ?on_cancel t run =
  Queue.add
    { Qmljs_types.driver = t.Qmljs_types.driver; run; on_cancel }
    t.Qmljs_types.host_jobs;
  signal_driver t

let set_module_loader t loader =
  t.Qmljs_types.module_loader <- Some loader;
  Hashtbl.clear loader.Qmljs_types.requests;
  let attributes_to_list (attributes : (string * string) array) =
    Array.to_list attributes
  in
  let marker error specifier =
    let kind, denied_specifier =
      match Qmljs_error.Structured.kind error with
      | Policy_violation { denied_module_specifier } ->
          ( "policy_violation"
          , Option.value denied_module_specifier ~default:specifier )
      | Host_callback_defect -> "host_callback_defect", ""
      | Out_of_memory -> "out_of_memory", ""
      | Stack_overflow -> "stack_overflow", ""
      | Resource_limit -> "resource_limit", ""
      | Timeout -> "timeout", ""
      | Closed_handle -> "closed_handle", ""
      | Wrong_domain -> "wrong_domain", ""
      | JavaScript -> "javascript", ""
    in
    Qmljs_error.Structured.message error, kind, denied_specifier
  in
  let normalize base specifier attributes =
    let attributes = attributes_to_list attributes in
    match loader.Qmljs_types.normalize ~base ~specifier ~attributes with
    | Ok normalized ->
        let request =
          { Qmljs_types.base
          ; specifier
          ; normalized
          ; attributes
          }
        in
        Hashtbl.replace loader.Qmljs_types.requests normalized request;
        Ok normalized
    | Error error -> Error (marker error specifier)
  in
  let load normalized attributes =
    let attributes = attributes_to_list attributes in
    let request =
      match Hashtbl.find_opt loader.Qmljs_types.requests normalized with
      | Some request -> request
      | None ->
          { Qmljs_types.base = None
          ; specifier = normalized
          ; normalized
          ; attributes
          }
    in
    match loader.Qmljs_types.load request with
    | Ok _ as result -> result
    | Error error -> Error (marker error normalized)
  in
  Qmljs_raw.runtime_install_module_loader
    (Qmljs_types.runtime_raw t)
    loader.Qmljs_types.deny
    normalize
    load
