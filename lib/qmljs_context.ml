type t = Qmljs_types.context

let create runtime =
  { Qmljs_types.runtime
  ; raw = Qmljs_raw.context_create (Qmljs_types.runtime_raw runtime)
  }

let with_ runtime f =
  let context = create runtime in
  let close () =
    let context_raw = Qmljs_types.context_raw context in
    if not (Qmljs_raw.context_close context_raw) then
      raise (Qmljs_error.Error "QuickJS context close was deferred")
  in
  match f context with
  | result ->
      close ();
      result
  | exception exn ->
      let backtrace = Printexc.get_raw_backtrace () in
      (try close () with _ -> ());
      Printexc.raise_with_backtrace exn backtrace

let eval ?(filename = "<eval>") ?(strict = false) ?(async = false)
    ?(module_ = false) t source =
  let flags =
    (if module_ then 1 else 0)
    lor (if strict then 1 lsl 3 else 0)
    lor (if async then 1 lsl 7 else 0)
  in
  { Qmljs_types.context = t
  ; raw = Qmljs_raw.context_eval (Qmljs_types.context_raw t) source filename flags
  }

let with_interrupt_timeout ~timeout runtime f =
  if timeout < 0. then invalid_arg "timeout must be non-negative";
  let timeout_error = "JavaScript execution timed out" in
  Qmljs_runtime.clear_interrupt runtime;
  Qmljs_raw.runtime_set_interrupt_timeout
    (Qmljs_types.runtime_raw runtime)
    timeout;
  Fun.protect
    ~finally:(fun () ->
      Qmljs_raw.runtime_clear_interrupt_timeout
        (Qmljs_types.runtime_raw runtime);
      Qmljs_runtime.clear_interrupt runtime;
      (* Reclaim any promise jobs a hostile script queued so the runtime can
         recover memory after an interrupt or out-of-memory failure. *)
      Qmljs_runtime.reclaim_pending_jobs runtime)
    (fun () ->
      match f () with
      | result ->
          if Qmljs_runtime.interrupt_requested runtime then
            Qmljs_error.raise_resource_limit timeout_error;
          result
      | exception exn ->
          let interrupted = Qmljs_runtime.interrupt_requested runtime in
          if interrupted then
            Qmljs_error.raise_resource_limit timeout_error;
          raise exn)

let eval_with_timeout ~timeout ?filename ?strict ?async ?module_ t source =
  with_interrupt_timeout ~timeout (Qmljs_types.context_runtime t)
    (fun () -> eval ?filename ?strict ?async ?module_ t source)

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
  { Qmljs_types.context = t
  ; raw = Qmljs_raw.context_eval_module (Qmljs_types.context_raw t) source filename
  }

let eval_module_with_timeout ~timeout ?filename t source =
  with_interrupt_timeout ~timeout (Qmljs_types.context_runtime t) (fun () ->
    eval_module ?filename t source)
