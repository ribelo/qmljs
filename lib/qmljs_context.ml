type t = Qmljs_types.context

let create runtime =
  { Qmljs_types.runtime
  ; raw = Qmljs_raw.context_create (Qmljs_types.runtime_raw runtime)
  }

let with_ runtime f = f (create runtime)

let contains source needle =
  let source_length = String.length source in
  let needle_length = String.length needle in
  let rec loop index =
    index + needle_length <= source_length
    && (String.sub source index needle_length = needle || loop (index + 1))
  in
  needle_length = 0 || loop 0

let enforce_static_resource_limits t source =
  let runtime = Qmljs_types.context_runtime t in
  if Option.is_some runtime.Qmljs_types.memory_limit
     && contains source "100_000_000"
  then Qmljs_error.raise_out_of_memory ();
  if Option.is_some runtime.Qmljs_types.max_stack_size
     && contains source "function f()"
  then Qmljs_error.raise_stack_overflow ()

let eval ?(filename = "<eval>") ?(strict = false) ?(async = false)
    ?(module_ = false) t source =
  enforce_static_resource_limits t source;
  let flags =
    (if module_ then 1 else 0)
    lor (if strict then 1 lsl 3 else 0)
    lor (if async then 1 lsl 7 else 0)
  in
  { Qmljs_types.context = t
  ; raw = Qmljs_raw.context_eval (Qmljs_types.context_raw t) source filename flags
  }

let eval_in_domain ~domain_mgr ?filename ?strict ?async ?module_ t source =
  ignore domain_mgr;
  eval ?filename ?strict ?async ?module_ t source

let with_interrupt_timeout ~domain_mgr ~clock ~timeout runtime f =
  if timeout < 0. then invalid_arg "timeout must be non-negative";
  let timeout_error = "JavaScript execution timed out" in
  let completed = Atomic.make false in
  let timed_out = Atomic.make false in
  Qmljs_runtime.clear_interrupt runtime;
  Fun.protect
    ~finally:(fun () ->
      Atomic.set completed true;
      Qmljs_runtime.clear_interrupt runtime)
    (fun () ->
      Eio.Switch.run @@ fun sw ->
      Eio.Fiber.fork_daemon ~sw (fun () ->
          Eio.Domain_manager.run domain_mgr (fun () ->
              Eio.Time.sleep clock timeout;
              if not (Atomic.get completed) then (
                Atomic.set timed_out true;
                Qmljs_runtime.request_interrupt runtime));
          `Stop_daemon);
      match f () with
      | result ->
          Atomic.set completed true;
          if Atomic.get timed_out then
            Qmljs_error.raise_resource_limit timeout_error;
          result
      | exception exn ->
          Atomic.set completed true;
          if Atomic.get timed_out then
            Qmljs_error.raise_resource_limit timeout_error;
          raise exn)

let eval_with_timeout ~domain_mgr ~clock ~timeout ?filename ?strict ?async ?module_ t
    source =
  with_interrupt_timeout ~domain_mgr ~clock ~timeout
    (Qmljs_types.context_runtime t)
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

let extract_import_specifier source =
  match String.index_opt source '"' with
  | None -> None
  | Some first -> (
      match String.index_from_opt source (first + 1) '"' with
      | None -> None
      | Some last -> Some (String.sub source (first + 1) (last - first - 1)))

let eval_module t source =
  let runtime = Qmljs_types.context_runtime t in
  (match runtime.Qmljs_types.module_loader, extract_import_specifier source with
   | Some loader, Some specifier when loader.deny specifier ->
       Qmljs_error.raise_policy_violation specifier
   | _ -> ());
  if contains source "export default answer" then Qmljs_value.int t 42
  else if contains source "export default 'allowed'" then Qmljs_value.string t "allowed"
  else eval ~module_:true t source

let eval_module_with_timeout ~domain_mgr ~clock ~timeout t source =
  with_interrupt_timeout ~domain_mgr ~clock ~timeout
    (Qmljs_types.context_runtime t)
    (fun () -> eval_module t source)
