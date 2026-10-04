type t = Qmljs_types.context

type intrinsic =
  | Date
  | Eval
  | Reg_exp_compiler
  | Reg_exp
  | Json
  | Proxy
  | Map_set
  | Typed_arrays
  | Promise
  | Big_int
  | Weak_ref
  | Performance
  | Dom_exception

let intrinsic_tag = function
  | Date -> 0
  | Eval -> 1
  | Reg_exp_compiler -> 2
  | Reg_exp -> 3
  | Json -> 4
  | Proxy -> 5
  | Map_set -> 6
  | Typed_arrays -> 7
  | Promise -> 8
  | Big_int -> 9
  | Weak_ref -> 10
  | Performance -> 11
  | Dom_exception -> 12

let create runtime =
  { Qmljs_types.runtime
  ; raw = Qmljs_raw.context_create (Qmljs_types.runtime_raw runtime)
  }

let create_raw ?(intrinsics = []) runtime =
  let intrinsic_tags =
    intrinsics |> List.map intrinsic_tag |> Array.of_list
  in
  { Qmljs_types.runtime
  ; raw =
      Qmljs_raw.context_create_raw
        (Qmljs_types.runtime_raw runtime)
        intrinsic_tags
  }

let with_context create runtime f =
  let context = create runtime in
  let close_error () =
    let context_raw = Qmljs_types.context_raw context in
    try
      if Qmljs_raw.context_close context_raw then None
      else
        Some
          (Qmljs_error.Structured.of_resource_limit
             "QuickJS context close was deferred")
    with
    | exn -> Some (Qmljs_error.Structured.of_exception exn)
  in
  match f context with
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

let with_ runtime f = with_context create runtime f

let with_raw ?intrinsics runtime f =
  with_context (fun runtime -> create_raw ?intrinsics runtime) runtime f

let eval ?(filename = "<eval>") ?(strict = false) ?(async = false)
    ?(module_ = false) ?(backtrace_barrier = false) t source =
  let flags =
    (if module_ then 1 else 0)
    lor (if strict then 1 lsl 3 else 0)
    lor (if async then 1 lsl 7 else 0)
    lor (if backtrace_barrier then 1 lsl 6 else 0)
  in
  { Qmljs_types.context = t
  ; raw = Qmljs_raw.context_eval (Qmljs_types.context_raw t) source filename flags
  }

(* Native deadline tokens are int64 nanosecond values, so a timeout whose
   nanosecond delta cannot be represented (NaN, infinity, or >= 2^63 ns) is
   rejected instead of being converted out of range. *)
let max_timeout_ns = 9_223_372_036_854_775_808.0

let validate_timeout timeout =
  if Float.is_nan timeout then invalid_arg "timeout must not be NaN";
  if not (Float.is_finite timeout) then invalid_arg "timeout must be finite";
  if timeout < 0. then invalid_arg "timeout must be non-negative";
  if not (timeout *. 1e9 < max_timeout_ns) then
    invalid_arg "timeout must be representable in nanoseconds"

(* Arm the runtime deadline for the duration of [f], then restore the deadline
   and the interrupt request that were in place before the bracket. A nested
   bracket keeps the earlier of the two deadlines, so an inner evaluation can
   neither extend nor drop an outer budget. Only the bracket that owns the
   effective deadline clears the interrupt request it caused: an inner
   evaluation that merely inherited an earlier outer deadline leaves the
   request pending, so the outer bracket still reports its own timeout. An
   interrupt requested before the bracket belongs to its caller and is restored
   rather than cleared. *)
let with_interrupt_timeout ~timeout runtime f =
  validate_timeout timeout;
  let raw = Qmljs_types.runtime_raw runtime in
  let previous_interrupt = Qmljs_runtime.interrupt_requested runtime in
  let previous_deadline, owns_deadline =
    Qmljs_raw.runtime_arm_interrupt_timeout raw timeout
  in
  let restore () =
    Qmljs_raw.runtime_restore_interrupt_deadline raw previous_deadline;
    if previous_interrupt then Qmljs_runtime.request_interrupt runtime
    else if owns_deadline then Qmljs_raw.runtime_clear_interrupt_request raw
  in
  let cleanup_noerr cleanup =
    try cleanup () with
    | _ -> ()
  in
  let timeout_error = "JavaScript execution timed out" in
  match f () with
  | result ->
      (* A request pending at entry belongs to the caller: untimed eval would
         also run the script and leave the request pending, so the bracket does
         not report a timeout it did not cause. *)
      let interrupted =
        (not previous_interrupt)
        && Qmljs_runtime.interrupt_requested runtime
      in
      restore ();
      if interrupted then begin
        Qmljs_runtime.reclaim_pending_jobs runtime;
        Qmljs_error.raise_timeout timeout_error
      end;
      result
  | exception exn ->
      let backtrace = Printexc.get_raw_backtrace () in
      let interrupted =
        (not previous_interrupt)
        &&
        (match Qmljs_runtime.interrupt_requested runtime with
         | interrupted -> interrupted
         | exception _ -> false)
      in
      cleanup_noerr restore;
      (* Reclaim any promise jobs a hostile script queued so the runtime can
         recover memory after an interrupt or out-of-memory failure. *)
      cleanup_noerr (fun () -> Qmljs_runtime.reclaim_pending_jobs runtime);
      if interrupted then begin
        let cause = Qmljs_error.Structured.of_exception exn in
        Qmljs_error.Structured.raise
          (Qmljs_error.Structured.timeout_with_cause
             ~message:timeout_error
             cause)
      end;
      Printexc.raise_with_backtrace exn backtrace

let eval_with_timeout ~timeout ?filename ?strict ?async ?module_
    ?backtrace_barrier t source =
  with_interrupt_timeout ~timeout (Qmljs_types.context_runtime t)
    (fun () -> eval ?filename ?strict ?async ?module_ ?backtrace_barrier t source)

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let eval_file ?strict ?async ?module_ ?backtrace_barrier t path =
  eval ~filename:path ?strict ?async ?module_ ?backtrace_barrier t
    (read_file path)

let global t =
  { Qmljs_types.context = t
  ; raw = Qmljs_raw.context_global (Qmljs_types.context_raw t)
  }

let set_global t ~name value =
  Qmljs_raw.context_set_global
    (Qmljs_types.context_raw t)
    name
    (Qmljs_types.value_raw value)

let eval_module ?(filename = "<module>") t source =
  let namespace_raw, eval_result_raw =
    Qmljs_raw.context_eval_module_namespace
      (Qmljs_types.context_raw t)
      source
      filename
  in
  let namespace = { Qmljs_types.context = t; raw = namespace_raw } in
  let eval_result = { Qmljs_types.context = t; raw = eval_result_raw } in
  Qmljs_promise.drive_module_evaluation eval_result;
  Qmljs_object.get namespace ~key:(Qmljs_value.string t "default")

let eval_module_with_timeout ~timeout ?filename t source =
  with_interrupt_timeout ~timeout (Qmljs_types.context_runtime t) (fun () ->
    eval_module ?filename t source)

let eval_module_file t path =
  eval_module ~filename:path t (read_file path)
