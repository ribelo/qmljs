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

type domain_runtimes =
  { mutable runtimes : t Weak.t }

let close_runtime_noerr runtime =
  try
    ignore (Qmljs_raw.runtime_close (Qmljs_types.runtime_raw runtime) : bool)
  with
  | _ -> ()

let close_domain_runtimes domain =
  for i = 0 to Weak.length domain.runtimes - 1 do
    match Weak.get domain.runtimes i with
    | None -> ()
    | Some runtime -> close_runtime_noerr runtime
  done

let compact_domain_runtimes domain =
  let live =
    List.init (Weak.length domain.runtimes) Fun.id
    |> List.filter_map (fun i -> Weak.get domain.runtimes i)
  in
  let next_length = max 16 (List.length live * 2) in
  let runtimes = Weak.create next_length in
  List.iteri (fun i runtime -> Weak.set runtimes i (Some runtime)) live;
  domain.runtimes <- runtimes

let add_domain_runtime domain runtime =
  let rec first_empty i =
    if i = Weak.length domain.runtimes then None
    else
      match Weak.get domain.runtimes i with
      | None -> Some i
      | Some _ -> first_empty (i + 1)
  in
  match first_empty 0 with
  | Some i -> Weak.set domain.runtimes i (Some runtime)
  | None ->
      compact_domain_runtimes domain;
      (match first_empty 0 with
       | Some i -> Weak.set domain.runtimes i (Some runtime)
       | None -> invalid_arg "could not register qmljs runtime cleanup")

let register_domain_cleanup =
  let cleanup_key =
    Domain.DLS.new_key (fun () ->
      let domain = { runtimes = Weak.create 16 } in
      Domain.at_exit (fun () ->
        close_domain_runtimes domain;
        for _ = 1 to 3 do
          Gc.full_major ();
          Qmljs_raw.runtime_drain_deferred_runtimes ()
        done);
      domain)
  in
  fun () -> Domain.DLS.get cleanup_key

let create_with_limits ?memory_limit ?max_stack_size ?promise_job_limit () =
  let domain = register_domain_cleanup () in
  let raw = Qmljs_raw.runtime_create () in
  Option.iter (Qmljs_raw.runtime_set_memory_limit raw) memory_limit;
  Option.iter (Qmljs_raw.runtime_set_max_stack_size raw) max_stack_size;
  let runtime =
    { Qmljs_types.raw
    ; host_jobs = Queue.create ()
    ; driver = None
    ; module_loader = None
    ; memory_limit
    ; max_stack_size
    ; promise_job_limit
    }
  in
  add_domain_runtime domain runtime;
  runtime

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
  let close_error () =
    let runtime_raw = Qmljs_types.runtime_raw runtime in
    try
      if Qmljs_raw.runtime_close runtime_raw then None
      else
        Some
          (Qmljs_error.Structured.of_resource_limit
             "QuickJS runtime close was deferred")
    with
    | exn -> Some (Qmljs_error.Structured.of_exception exn)
  in
  match f runtime with
  | result ->
      (match close_error () with
       | None -> result
       | Some error -> Qmljs_error.Structured.raise error)
  | exception exn ->
      let backtrace = Printexc.get_raw_backtrace () in
      (match close_error () with
       | None -> Printexc.raise_with_backtrace exn backtrace
       | Some cleanup ->
           Qmljs_error.Structured.raise_with_suppressed exn backtrace [ cleanup ])

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
  try
    let rec loop n =
      if n < limit && is_job_pending t then begin
        (try ignore (execute_pending_job t : bool) with _ -> ());
        loop (n + 1)
      end
    in
    loop 0
  with
  | _ -> ()

let attach_driver t driver =
  match t.Qmljs_types.driver with
  | Some _ -> invalid_arg "QuickJS runtime already has an Eio driver"
  | None -> t.Qmljs_types.driver <- Some driver

let detach_driver t driver =
  if t.Qmljs_types.driver == Some driver then t.Qmljs_types.driver <- None

(* [wakeup_generation] is the condition predicate. Waiters snapshot it before
   checking promise state; signalers advance it under [mutex] before broadcast. *)
let driver_closed driver =
  Eio.Mutex.use_ro driver.Qmljs_types.mutex (fun () ->
      driver.Qmljs_types.closed)

let driver_wakeup_generation driver =
  Eio.Mutex.use_ro driver.Qmljs_types.mutex (fun () ->
      if driver.Qmljs_types.closed then None
      else Some driver.Qmljs_types.wakeup_generation)

let signal_driver_event driver =
  Eio.Mutex.use_rw ~protect:false driver.Qmljs_types.mutex (fun () ->
      driver.Qmljs_types.wakeup_generation <-
        driver.Qmljs_types.wakeup_generation + 1);
  Eio.Condition.broadcast driver.Qmljs_types.condition

let signal_driver t =
  match t.Qmljs_types.driver with
  | None -> ()
  | Some driver -> signal_driver_event driver

let await_driver_wakeup driver generation =
  Eio.Mutex.use_ro driver.Qmljs_types.mutex (fun () ->
      while
        (not driver.Qmljs_types.closed)
        && driver.Qmljs_types.wakeup_generation = generation
      do
        Eio.Condition.await driver.Qmljs_types.condition
          driver.Qmljs_types.mutex
      done)

let close_driver driver =
  let changed =
    Eio.Mutex.use_rw ~protect:false driver.Qmljs_types.mutex (fun () ->
        if driver.Qmljs_types.closed then false
        else begin
          driver.Qmljs_types.closed <- true;
          driver.Qmljs_types.wakeup_generation <-
            driver.Qmljs_types.wakeup_generation + 1;
          true
        end)
  in
  if changed then Eio.Condition.broadcast driver.Qmljs_types.condition

let cancel_driver driver =
  let changed =
    Eio.Mutex.use_rw ~protect:false driver.Qmljs_types.mutex (fun () ->
        if driver.Qmljs_types.closed then false
        else begin
          driver.Qmljs_types.cancelled <- true;
          driver.Qmljs_types.wakeup_generation <-
            driver.Qmljs_types.wakeup_generation + 1;
          true
        end)
  in
  if changed then begin
    driver.Qmljs_types.cancel ();
    Eio.Condition.broadcast driver.Qmljs_types.condition
  end

let execute_host_job t =
  ignore (Qmljs_raw.runtime_is_job_pending (Qmljs_types.runtime_raw t) : bool);
  match Queue.take_opt t.Qmljs_types.host_jobs with
  | None -> false
  | Some job ->
      let drop, cancel =
        match job.Qmljs_types.driver with
        | None -> false, false
        | Some driver ->
            Eio.Mutex.use_ro driver.Qmljs_types.mutex (fun () ->
                driver.Qmljs_types.closed, driver.Qmljs_types.cancelled)
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
  Hashtbl.clear loader.Qmljs_types.requests;
  let attributes_to_list (attributes : (string * string) array) =
    Array.to_list attributes
  in
  let same_attributes left right =
    List.equal
      (fun (left_name, left_value) (right_name, right_value) ->
        String.equal left_name right_name && String.equal left_value right_value)
      (List.sort compare left)
      (List.sort compare right)
  in
  let same_request_identity left right =
    Option.equal String.equal left.Qmljs_types.base right.Qmljs_types.base
    && String.equal left.specifier right.specifier
    && same_attributes left.attributes right.attributes
  in
  let same_cached_request left right =
    match loader.Qmljs_types.request_identity with
    | Qmljs_types.Normalized_request ->
        same_attributes left.Qmljs_types.attributes right.Qmljs_types.attributes
    | Qmljs_types.Exact_request -> same_request_identity left right
  in
  let marker ~ocaml_frame error specifier =
    Qmljs_error.Structured.to_marker
      ~denied_specifier_default:specifier
      ~ocaml_frame
      error
  in
  let host_defect_of_exception exn =
    let message = Qmljs_error.host_exception_message exn in
    let metadata = Array.to_list (Qmljs_error.host_exception_metadata exn) in
    Qmljs_error.Structured.of_exception
      (Qmljs_error.Structured.host ~message ~metadata)
  in
  let capture_loader_callback fn =
    match fn () with
    | result -> Ok result
    | exception (Qmljs_error.Structured_error error) -> Error error
    | exception (Qmljs_error.Host_error _ as exn) ->
        Error (Qmljs_error.Structured.of_exception exn)
    | exception (Qmljs_error.Policy_violation _ as exn) ->
        Error (Qmljs_error.Structured.of_exception exn)
    | exception Qmljs_error.Out_of_memory ->
        Error (Qmljs_error.Structured.of_exception Qmljs_error.Out_of_memory)
    | exception Qmljs_error.Stack_overflow ->
        Error (Qmljs_error.Structured.of_exception Qmljs_error.Stack_overflow)
    | exception (Qmljs_error.Resource_limit _ as exn) ->
        Error (Qmljs_error.Structured.of_exception exn)
    | exception (Qmljs_error.Timeout _ as exn) ->
        Error (Qmljs_error.Structured.of_exception exn)
    | exception exn -> Error (host_defect_of_exception exn)
  in
  let normalize base specifier attributes =
    let attributes = attributes_to_list attributes in
    match
      capture_loader_callback (fun () ->
        loader.Qmljs_types.normalize ~base ~specifier ~attributes)
    with
    | Error error ->
        Error (marker ~ocaml_frame:"module_loader.normalize" error specifier)
    | Ok (Ok normalized) ->
        let request =
          { Qmljs_types.base
          ; specifier
          ; normalized
          ; attributes
          }
        in
        (match Hashtbl.find_opt loader.Qmljs_types.requests normalized with
         | Some previous when not (same_cached_request previous request) ->
             Error
               (marker ~ocaml_frame:"module_loader.normalize"
                  (Qmljs_error.Structured.of_policy specifier)
                  specifier)
         | _ when String.contains normalized '\000' ->
             Error
               (marker ~ocaml_frame:"module_loader.normalize"
                  (Qmljs_error.Structured.of_policy specifier)
                  specifier)
         | _ ->
             Hashtbl.replace loader.Qmljs_types.requests normalized request;
             Ok normalized)
    | Ok (Error error) ->
        Error (marker ~ocaml_frame:"module_loader.normalize" error specifier)
  in
  let load normalized attributes =
    let attributes = attributes_to_list attributes in
    let request =
      match Hashtbl.find_opt loader.Qmljs_types.requests normalized with
      | Some request when same_attributes request.attributes attributes -> request
      | Some _ | None ->
          { Qmljs_types.base = None
          ; specifier = normalized
          ; normalized
          ; attributes
          }
    in
    match
      capture_loader_callback (fun () -> loader.Qmljs_types.load request)
    with
    | Error error ->
        Error (marker ~ocaml_frame:"module_loader.load" error normalized)
    | Ok (Ok _ as result) -> result
    | Ok (Error error) ->
        Error (marker ~ocaml_frame:"module_loader.load" error normalized)
  in
  let deny specifier =
    match
      capture_loader_callback (fun () -> loader.Qmljs_types.deny specifier)
    with
    | Ok denied -> Ok denied
    | Error error ->
        Error (marker ~ocaml_frame:"module_loader.deny" error specifier)
  in
  Qmljs_raw.runtime_install_module_loader
    (Qmljs_types.runtime_raw t)
    deny
    normalize
    load;
  t.Qmljs_types.module_loader <- Some loader
