type t = Qmljs_types.context

let create runtime =
  { Qmljs_types.runtime
  ; raw = Qmljs_raw.context_create (Qmljs_types.runtime_raw runtime)
  }

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

let eval_in_domain ~domain_mgr ?filename ?strict ?async ?module_ t source =
  Eio.Domain_manager.run domain_mgr (fun () ->
      eval ?filename ?strict ?async ?module_ t source)

let eval_with_timeout ~domain_mgr ~clock ~timeout ?filename ?strict ?async ?module_ t
    source =
  if timeout < 0. then invalid_arg "timeout must be non-negative";
  let runtime = Qmljs_types.context_runtime t in
  let timed_out = ref false in
  let timeout_error = "JavaScript execution timed out" in
  Qmljs_runtime.clear_interrupt runtime;
  Fun.protect
    ~finally:(fun () -> Qmljs_runtime.clear_interrupt runtime)
    (fun () ->
      match
        Eio.Fiber.first
          (fun () ->
            eval_in_domain ~domain_mgr ?filename ?strict ?async ?module_ t source)
          (fun () ->
            Eio.Time.sleep clock timeout;
            timed_out := true;
            Qmljs_runtime.request_interrupt runtime;
            let rec wait () =
              Eio.Fiber.yield ();
              wait ()
            in
            wait ())
      with
      | result ->
          if !timed_out then raise (Qmljs_error.Error timeout_error);
          result
      | exception Qmljs_error.Error _ when !timed_out ->
          raise (Qmljs_error.Error timeout_error))

let global t =
  { Qmljs_types.context = t
  ; raw = Qmljs_raw.context_global (Qmljs_types.context_raw t)
  }

let set_global t ~name value =
  Qmljs_raw.context_set_global
    (Qmljs_types.context_raw t)
    name
    (Qmljs_types.value_raw value)
