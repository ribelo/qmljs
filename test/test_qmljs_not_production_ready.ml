let fail message = raise (Failure message)

let check_int label expected actual =
  if actual <> expected then
    fail (Printf.sprintf "%s: expected %d, got %d" label expected actual)

let check_bool label expected actual =
  if expected <> actual then
    fail (Printf.sprintf "%s: expected %b, got %b" label expected actual)

let check_string label expected actual =
  if not (String.equal expected actual) then
    fail (Printf.sprintf "%s: expected %S, got %S" label expected actual)

let check_kind label expected actual =
  if expected <> actual then fail (label ^ ": unexpected error kind")

let expect_structured_error label fn assert_error =
  match Qmljs.Error.capture fn with
  | Ok _ -> fail (label ^ ": expected structured error")
  | Error error -> assert_error error

external test_reject_arg_count : nativeint -> unit
  = "qmljs_test_reject_arg_count"

external test_context_close : Qmljs.Context.t -> bool = "qmljs_test_context_close"

external test_runtime_close : Qmljs.Runtime.t -> bool = "qmljs_test_runtime_close"

external test_internal_runtime_close : Qmljs__Qmljs_types.runtime -> bool
  = "qmljs_test_runtime_close"

external test_set_array_failpoint : int -> unit
  = "qmljs_test_set_array_failpoint"

external test_set_host_callback_failpoint : int -> unit
  = "qmljs_test_set_host_callback_failpoint"

external test_host_callback_depth : Qmljs.Runtime.t -> int
  = "qmljs_test_host_callback_depth"

let host_error ~message ~metadata =
  match Qmljs.Error.capture (fun () ->
    raise (Qmljs.Error.host ~message ~metadata))
  with
  | Ok _ -> fail "host error helper did not raise"
  | Error error -> error

let test_call_argument_count_over_int_max_is_rejected () =
  expect_structured_error "oversized argument count"
    (fun () -> test_reject_arg_count (Int64.to_nativeint 2_147_483_648L))
    (fun error ->
      check_kind "oversized argument count kind"
        Qmljs.Error.JavaScript (Qmljs.Error.kind error);
      check_string "oversized argument count message"
        "function argument count exceeds QuickJS argc range"
        (Qmljs.Error.message error))

let expect_array_failpoint label site fn =
  Fun.protect
    ~finally:(fun () -> test_set_array_failpoint 0)
    (fun () ->
      test_set_array_failpoint site;
      expect_structured_error label fn (fun error ->
        check_string (label ^ " message")
          "qmljs test array allocation failpoint"
          (Qmljs.Error.message error)))

let test_c_stubs_initialize_ocaml_arrays_before_allocating_elements () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  Qmljs.Runtime.set_module_loader runtime
    (Qmljs.Module.Loader.memory [ "data.json", {|{"answer": 42}|} ]);
  expect_array_failpoint "import attributes array failpoint" 1
    (fun () ->
      ignore
        (Qmljs.Module.eval context
           {|import data from "data.json" with { type: "json" };
             export default data.answer;|}
          : Qmljs.Module.namespace));
  expect_array_failpoint "memory usage array failpoint" 2
    (fun () -> ignore (Qmljs.Runtime.memory_usage runtime));
  Qmljs.Runtime.set_promise_rejection_tracking runtime true;
  ignore
    (Qmljs.Context.eval context
       "globalThis.arrayFailpointRejection = Promise.reject(new Error('boom'))"
      : Qmljs.Value.t);
  ignore (Qmljs.Runtime.drain_jobs runtime : int);
  expect_array_failpoint "promise rejections array failpoint" 3
    (fun () -> ignore (Qmljs.Runtime.take_promise_rejections runtime));
  Qmljs.Runtime.set_promise_hook_tracking runtime true;
  let promise =
    Qmljs.Context.eval context
      "Promise.resolve(40).then((value) => value + 2)"
  in
  ignore (Qmljs.Promise.await ~yield_every:1 promise : Qmljs.Value.t);
  expect_array_failpoint "promise hooks array failpoint" 4
    (fun () -> ignore (Qmljs.Runtime.take_promise_hooks runtime))

let expect_host_callback_failpoint label ~runtime ~site ~frame fn =
  Fun.protect
    ~finally:(fun () -> test_set_host_callback_failpoint 0)
    (fun () ->
      test_set_host_callback_failpoint site;
      expect_structured_error label fn (fun error ->
        check_kind (label ^ " kind") Qmljs.Error.Host_callback_defect
          (Qmljs.Error.kind error);
        check_string (label ^ " message")
          "Failure(\"qmljs test host callback wrapper allocation failpoint\")"
          (Qmljs.Error.message error);
        check_bool (label ^ " frame") true
          (Qmljs.Error.has_ocaml_frame error ~function_name:frame)));
  check_int (label ^ " callback depth") 0
    (test_host_callback_depth runtime)

let test_host_callback_argument_wrapping_allocation_failure_is_js_exception () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  Qmljs.Function.set_global context ~name:"wrap_args" ~arity:3 (fun _args ->
      fail "wrap_args callback should not run");
  expect_host_callback_failpoint "argument wrapping failpoint" ~runtime ~site:1
    ~frame:"wrap_args"
    (fun () ->
      ignore (Qmljs.Context.eval context "wrap_args(1, 2, 3)" : Qmljs.Value.t));
  check_int "runtime reusable after argument wrapping failure" 2
    (Qmljs.Value.to_int (Qmljs.Context.eval context "1 + 1"));
  let constructor =
    Qmljs.Function.make_with_this context ~name:"WrapCtor" ~arity:1
      (fun _call -> fail "WrapCtor callback should not run")
  in
  Qmljs.Function.set_constructor constructor true;
  Qmljs.Context.set_global context ~name:"WrapCtor" constructor;
  expect_host_callback_failpoint "this wrapping failpoint" ~runtime ~site:2
    ~frame:"WrapCtor"
    (fun () ->
      ignore (Qmljs.Context.eval context "new WrapCtor(1)" : Qmljs.Value.t));
  check_int "runtime reusable after this wrapping failure" 3
    (Qmljs.Value.to_int (Qmljs.Context.eval context "1 + 2"));
  expect_host_callback_failpoint "new.target wrapping failpoint" ~runtime ~site:3
    ~frame:"WrapCtor"
    (fun () ->
      ignore (Qmljs.Context.eval context "new WrapCtor(1)" : Qmljs.Value.t));
  check_int "runtime reusable after new.target wrapping failure" 4
    (Qmljs.Value.to_int (Qmljs.Context.eval context "2 + 2"))

let test_with_preserves_body_and_cleanup_failures () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let success_cleanup = ref None in
  let body_and_cleanup = ref None in
  Qmljs.Function.set_global context ~name:"probe_brackets" ~arity:0 (fun _args ->
      success_cleanup :=
        Some
          (Qmljs.Error.capture (fun () ->
             Qmljs.Context.with_ runtime @@ fun _inner -> ()));
      body_and_cleanup :=
        Some
          (Qmljs.Error.capture (fun () ->
             Qmljs.Context.with_ runtime @@ fun _inner ->
             raise
               (Qmljs.Error.host ~message:"body boom"
                  ~metadata:[ "phase", "body" ])));
      Qmljs.Value.int context 0);
  ignore (Qmljs.Context.eval context "probe_brackets()" : Qmljs.value);
  (match !success_cleanup with
   | Some (Error error) ->
       check_kind "cleanup failure on success kind" Qmljs.Error.Resource_limit
         (Qmljs.Error.kind error);
       check_string "cleanup failure on success message"
         "QuickJS context close was deferred" (Qmljs.Error.message error)
   | Some (Ok ()) -> fail "context close failure on success was ignored"
   | None -> fail "success cleanup result was not captured");
  (match !body_and_cleanup with
   | Some (Error error) ->
       check_kind "body failure kind survives"
         Qmljs.Error.Host_callback_defect (Qmljs.Error.kind error);
       check_string "body failure message survives" "body boom"
         (Qmljs.Error.message error);
       check_bool "body failure metadata survives" true
         (List.mem ("phase", "body") (Qmljs.Error.metadata error));
       let suppressed = Qmljs.Error.suppressed error in
       check_int "cleanup failure is suppressed" 1 (List.length suppressed);
       let cleanup = List.hd suppressed in
       check_kind "suppressed cleanup failure kind"
         Qmljs.Error.Resource_limit (Qmljs.Error.kind cleanup);
       check_string "suppressed cleanup failure message"
         "QuickJS context close was deferred" (Qmljs.Error.message cleanup)
   | Some (Ok ()) -> fail "body failure was ignored"
   | None -> fail "body cleanup result was not captured")

let test_sync_host_callback_timeout_is_preemptive () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  Qmljs.Function.set_global context ~name:"block" ~arity:0 (fun _args ->
      Unix.sleepf 0.05;
      Qmljs.Value.int context 1);
  let started = Unix.gettimeofday () in
  expect_structured_error "host callback timeout"
    (fun () ->
      Qmljs.Context.eval_with_timeout ~timeout:0.005 context "block()")
    (fun error ->
      let elapsed = Unix.gettimeofday () -. started in
      check_bool "timeout classified as resource limit" true
        (Qmljs.Error.is_resource_limit error);
      if elapsed > 0.03 then
        fail (Printf.sprintf "timeout was not preemptive: %.3fs" elapsed))

let test_eval_with_timeout_does_not_drain_microtasks_on_success () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  ignore (Qmljs.Context.eval context "globalThis.hit = 0" : Qmljs.value);
  check_int "eval_with_timeout result" 0
    (Qmljs.Value.to_int
       (Qmljs.Context.eval_with_timeout ~timeout:1.0 context
          "Promise.resolve().then(() => globalThis.hit = 1); 0"));
  check_int "microtask not drained after successful timeout eval" 0
    (Qmljs.Value.to_int (Qmljs.Context.eval context "globalThis.hit"));
  ignore (Qmljs.Async.drain_jobs runtime : int);
  check_int "microtask runs after explicit drain" 1
    (Qmljs.Value.to_int (Qmljs.Context.eval context "globalThis.hit"))

let test_timeout_preserves_original_structured_stack () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  Qmljs.Function.set_global context ~name:"host_probe" ~arity:0 (fun _args ->
      Qmljs.Value.int context 1);
  expect_structured_error "timeout preserves structured stack"
    (fun () ->
      ignore
        (Qmljs.Context.eval_with_timeout ~timeout:1.0 context
           {|
             function entry() {
               return host_probe();
             }
             entry();
           |}
         : Qmljs.value))
    (fun error ->
      check_kind "timeout preserves kind" Qmljs.Error.Timeout
        (Qmljs.Error.kind error);
      let cause = Qmljs.Error.cause_exn error in
      check_bool "timeout preserves JS frame" true
        (Qmljs.Error.has_js_frame cause ~function_name:"entry"));
  check_bool "timeout clears interrupt flag" false
    (Qmljs.Runtime.interrupt_requested runtime);
  check_int "runtime reusable after preserved timeout" 2
    (Qmljs.Value.to_int (Qmljs.Context.eval context "1 + 1"))

let test_eval_with_timeout_closed_runtime_masks_error () =
  let runtime = Qmljs__Qmljs_runtime.create () in
  expect_structured_error "closed runtime timeout cleanup preserves body error"
    (fun () ->
      Qmljs__Qmljs_context.with_interrupt_timeout ~timeout:1.0 runtime
        (fun () ->
          ignore (test_internal_runtime_close runtime : bool);
          raise
            (Qmljs.Error.host ~message:"closed runtime body error" ~metadata:[])))
    (fun error ->
      check_kind "closed runtime timeout cleanup kind"
        Qmljs.Error.Host_callback_defect (Qmljs.Error.kind error);
      check_string "closed runtime timeout cleanup message"
        "closed runtime body error" (Qmljs.Error.message error))

let test_bigint_of_string_does_not_execute_code () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  ignore (Qmljs.Context.eval context "globalThis.pwned = 0" : Qmljs.value);
  let result =
    Qmljs.Error.capture (fun () ->
        ignore
          (Qmljs.Bigint.of_string context "1; globalThis.pwned = 42; 2"
           : Qmljs.value))
  in
  check_int "Bigint.of_string did not run injected code" 0
    (Qmljs.Value.to_int (Qmljs.Context.eval context "globalThis.pwned"));
  (match result with
   | Ok _ -> fail "Bigint.of_string accepted executable input"
   | Error _ -> ());
  check_string "valid Bigint.of_string still works" "9007199254740993"
    (Qmljs.Bigint.to_string
       (Qmljs.Bigint.of_string context "9007199254740993"))

let test_json_stringify_undefined_is_error_not_string () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let undefined = Qmljs.Value.undefined context in
  (match Qmljs.Json.to_string undefined with
   | Ok "undefined" -> fail "Json.to_string returned non-JSON text"
   | Ok text -> fail ("Json.to_string unexpectedly returned " ^ text)
   | Error error ->
       check_string "Json.to_string undefined message"
         "JSON.stringify returned undefined"
         (Qmljs.Error.message error));
  expect_structured_error "Json.stringify undefined"
    (fun () -> ignore (Qmljs.Json.stringify undefined : string))
    (fun error ->
      check_string "Json.stringify undefined message"
        "JSON.stringify returned undefined"
        (Qmljs.Error.message error))

let test_nul_in_public_string_keys_is_not_silently_truncated () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let key = "a\000b" in
  let full_key = Qmljs.Value.string context key in
  let truncated_key = Qmljs.Value.string context "a" in
  let object_ = Qmljs.Object.create context in
  Qmljs.Object.set_string object_ key (Qmljs.Value.int context 42);
  check_int "object full NUL key" 42
    (Qmljs.Value.to_int (Qmljs.Object.get object_ ~key:full_key));
  check_bool "object truncated NUL key is not set" true
    (Qmljs.Value.is_undefined (Qmljs.Object.get object_ ~key:truncated_key));
  Qmljs.Context.set_global context ~name:key (Qmljs.Value.int context 43);
  let global = Qmljs.Context.global context in
  check_int "global full NUL key" 43
    (Qmljs.Value.to_int (Qmljs.Object.get global ~key:full_key));
  check_bool "global truncated NUL key is not set" true
    (Qmljs.Value.is_undefined (Qmljs.Object.get global ~key:truncated_key));
  Qmljs.Context.set_global context ~name:"nulSymbol"
    (Qmljs.Symbol.global context key);
  check_int "global symbol NUL description length" 3
    (Qmljs.Value.to_int
       (Qmljs.Context.eval context "Symbol.keyFor(globalThis.nulSymbol).length"));
  check_int "global symbol NUL description middle byte" 0
    (Qmljs.Value.to_int
       (Qmljs.Context.eval context "Symbol.keyFor(globalThis.nulSymbol).charCodeAt(1)"))

let test_user_error_message_interrupted_is_javascript_error () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  List.iter
    (fun message ->
      expect_structured_error ("user error " ^ message)
        (fun () ->
          Qmljs.Context.eval context
            ("throw new Error(" ^ Qmljs.Json.stringify
               (Qmljs.Value.string context message)
             ^ ")"))
        (fun error ->
          check_kind ("user error kind " ^ message) Qmljs.Error.JavaScript
            (Qmljs.Error.kind error);
          check_string ("user error message " ^ message) message
            (Qmljs.Error.message error)))
    [ "interrupted"
    ; "out of memory"
    ; "stack overflow"
    ; "Maximum call stack size exceeded"
    ]

let test_throwing_error_getters_do_not_poison_context () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  expect_structured_error "throwing error getters"
    (fun () ->
      ignore
        (Qmljs.Context.eval context
           {|
             throw {
               get name() { throw new Error("name boom"); },
               get message() { throw new Error("getter boom"); },
               get stack() { throw new Error("stack boom"); },
               get _qmljs_kind() { throw new Error("kind boom"); },
               get _qmljs_metadata() { throw new Error("metadata boom"); },
               get cause() { throw new Error("cause boom"); },
               toString() { return "outer"; }
             };
           |}
         : Qmljs.value))
    (fun error ->
      check_kind "throwing getters keep original kind"
        Qmljs.Error.JavaScript (Qmljs.Error.kind error);
      check_string "throwing getters keep fallback message" "outer"
        (Qmljs.Error.message error));
  check_int "context reusable after throwing getters" 2
    (Qmljs.Value.to_int (Qmljs.Context.eval context "1 + 1"))

let test_native_module_host_error_metadata_survives () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let failing_module =
    Qmljs.Module.Native.define
      (fun declarations -> Qmljs.Module.Native.declare declarations "default")
      ~evaluate:(fun _exports ->
        raise
          (Qmljs.Error.host ~message:"boom"
             ~metadata:[ "request_id", "m-1" ]))
  in
  Qmljs.Module.declare_native context ~name:"native:metadata-fail"
    failing_module;
  expect_structured_error "native module host metadata"
    (fun () ->
      ignore
        (Qmljs.Module.eval ~filename:"native-metadata-fail.mjs" context
           {|import value from "native:metadata-fail"; export default value;|}
         : Qmljs.Module.namespace))
    (fun error ->
      check_kind "native module host metadata kind"
        Qmljs.Error.Host_callback_defect (Qmljs.Error.kind error);
      check_string "native module host metadata message" "boom"
        (Qmljs.Error.message error);
      check_bool "native module host metadata survives" true
        (List.mem ("request_id", "m-1") (Qmljs.Error.metadata error));
      check_bool "native module host frame survives" true
        (Qmljs.Error.has_ocaml_frame error ~function_name:"native:metadata-fail"))

let test_module_loader_host_error_metadata_survives () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let loader_error =
    host_error ~message:"loader boom" ~metadata:[ "request_id", "l-1" ]
  in
  let loader =
    Qmljs.Module.Loader.callback
      ~normalize:(fun ~base:_ ~specifier ~attributes:_ -> Ok specifier)
      ~load:(fun _request -> Error loader_error)
  in
  Qmljs.Runtime.set_module_loader runtime loader;
  expect_structured_error "module loader host metadata"
    (fun () ->
      ignore
        (Qmljs.Module.eval ~filename:"loader-metadata-fail.mjs" context
           {|import value from "loader:metadata-fail"; export default value;|}
         : Qmljs.Module.namespace))
    (fun error ->
      check_kind "module loader host metadata kind"
        Qmljs.Error.Host_callback_defect (Qmljs.Error.kind error);
      check_string "module loader host metadata message" "loader boom"
        (Qmljs.Error.message error);
      check_bool "module loader host metadata survives" true
        (List.mem ("request_id", "l-1") (Qmljs.Error.metadata error));
      check_bool "module loader host frame survives" true
        (Qmljs.Error.has_ocaml_frame error ~function_name:"module_loader.load"))

let test_module_loader_deny_host_error_metadata_survives () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let loader =
    Qmljs.Module.Loader.memory [ "denied.js", "export default 1;" ]
    |> Qmljs.Module.Loader.deny (fun _specifier ->
      raise
        (Qmljs.Error.host ~message:"deny boom"
           ~metadata:[ "request_id", "d-1" ]))
  in
  Qmljs.Runtime.set_module_loader runtime loader;
  expect_structured_error "module loader deny host metadata"
    (fun () ->
      ignore
        (Qmljs.Module.eval ~filename:"loader-deny-metadata-fail.mjs" context
           {|import value from "denied.js"; export default value;|}
         : Qmljs.Module.namespace))
    (fun error ->
      check_kind "module loader deny host metadata kind"
        Qmljs.Error.Host_callback_defect (Qmljs.Error.kind error);
      check_string "module loader deny host metadata message" "deny boom"
        (Qmljs.Error.message error);
      check_bool "module loader deny host metadata survives" true
        (List.mem ("request_id", "d-1") (Qmljs.Error.metadata error));
      check_bool "module loader deny host frame survives" true
        (Qmljs.Error.has_ocaml_frame error ~function_name:"module_loader.deny"))

let test_promise_rejection_preserves_js_stack () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let promise =
    Qmljs.Context.eval context
      {|
        function rejector() {
          return Promise.reject(new Error("async boom"));
        }
        rejector();
      |}
  in
  expect_structured_error "promise rejection"
    (fun () -> Qmljs.Promise.await promise)
    (fun error ->
      check_bool "rejection has JS frame" true
        (Qmljs.Error.has_js_frame error ~function_name:"rejector"))

let test_deep_js_cause_chain_is_preserved () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  expect_structured_error "deep cause chain"
    (fun () ->
      Qmljs.Context.eval context
        {|
          let error = new Error("root");
          for (let i = 1; i <= 20; i++) {
            error = new Error("cause-" + i, { cause: error });
          }
          throw error;
        |})
    (fun error ->
      let rec descend n current =
        if n = 0 then current else descend (n - 1) (Qmljs.Error.cause_exn current)
      in
      check_string "deepest cause" "root" (Qmljs.Error.message (descend 20 error)))

let test_same_runtime_cross_context_object_set_is_rejected () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  let left = Qmljs.Context.create runtime in
  let right = Qmljs.Context.create runtime in
  let foreign = Qmljs.Value.int left 42 in
  let object_ = Qmljs.Object.create right in
  expect_structured_error "cross-context object set"
    (fun () -> Qmljs.Object.set object_ ~name:"foreign" foreign)
    (fun error ->
      check_kind "cross-context object set kind" Qmljs.Error.Wrong_domain
        (Qmljs.Error.kind error))

let test_same_runtime_cross_context_function_call_is_rejected () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  let left = Qmljs.Context.create runtime in
  let right = Qmljs.Context.create runtime in
  let foreign = Qmljs.Value.int left 41 in
  let add1 = Qmljs.Context.eval right "(x) => x + 1" in
  expect_structured_error "cross-context function call arg"
    (fun () -> Qmljs.Value.call add1 [| foreign |])
    (fun error ->
      check_kind "cross-context function call kind" Qmljs.Error.Wrong_domain
        (Qmljs.Error.kind error))

let test_function_call_cross_context_arg_leak () =
  let before = Qmljs.Diagnostics.snapshot () in
  (Qmljs.Runtime.with_ @@ fun runtime ->
   let left = Qmljs.Context.create runtime in
   let right = Qmljs.Context.create runtime in
   let foreign = Qmljs.Value.int right 41 in
   let function_ = Qmljs.Context.eval left "(x) => x + 1" in
   for _ = 1 to 100 do
     expect_structured_error "function call cross-context arg leak"
       (fun () ->
         ignore
           (Qmljs.Function.call function_ [| foreign |] : Qmljs.Value.t))
       (fun error ->
         check_kind "function call cross-context arg leak kind"
           Qmljs.Error.Wrong_domain (Qmljs.Error.kind error));
     expect_structured_error "value call cross-context arg leak"
       (fun () ->
         ignore (Qmljs.Value.call function_ [| foreign |] : Qmljs.Value.t))
       (fun error ->
         check_kind "value call cross-context arg leak kind"
           Qmljs.Error.Wrong_domain (Qmljs.Error.kind error))
   done);
  let after = Qmljs.Diagnostics.snapshot () in
  Qmljs.Diagnostics.assert_no_leaks ~before ~after

let test_sync_host_callback_foreign_return_is_js_host_defect () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  let left = Qmljs.Context.create runtime in
  let right = Qmljs.Context.create runtime in
  let foreign = Qmljs.Value.int right 41 in
  Qmljs.Function.set_global left ~name:"bad_return" ~arity:0 (fun _args ->
      foreign);
  expect_structured_error "foreign host callback return"
    (fun () -> ignore (Qmljs.Context.eval left "bad_return()" : Qmljs.value))
    (fun error ->
      check_kind "foreign host callback return kind"
        Qmljs.Error.Host_callback_defect (Qmljs.Error.kind error);
      check_string "foreign host callback return message"
        "QuickJS value belongs to a different context"
        (Qmljs.Error.message error));
  check_int "runtime reusable after foreign return" 2
    (Qmljs.Value.to_int (Qmljs.Context.eval left "1 + 1"))

let test_json_import_attribute_loads_json_module () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let loader =
    Qmljs.Module.Loader.memory [ "config.json", {|{"answer":42}|} ]
  in
  Qmljs.Runtime.set_module_loader runtime loader;
  let value =
    Qmljs.Context.eval_module context
      {|
        import config from "config.json" with { type: "json" };
        export default config.answer;
      |}
  in
  check_int "json module default" 42 (Qmljs.Module.default_export_to_int value)

let test_error_kind_and_metadata_api () =
  expect_structured_error "host structured metadata"
    (fun () ->
      raise
        (Qmljs.Error.host ~message:"host boom"
           ~metadata:[ "phase", "callback"; "id", "42" ]))
    (fun error ->
      check_string "host error message" "host boom" (Qmljs.Error.message error);
      check_kind "host error kind" Qmljs.Error.Host_callback_defect
        (Qmljs.Error.kind error);
      check_bool "metadata contains id" true
        (List.mem ("id", "42") (Qmljs.Error.metadata error));
      check_bool "host error has no cause" true
        (Qmljs.Error.cause error = None))

let test_structured_promise_await_api () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let fulfilled =
    Qmljs.Context.eval context "Promise.resolve(41).then(x => x + 1)"
  in
  (match Qmljs.Promise.await_result fulfilled with
   | Ok value -> check_int "fulfilled promise" 42 (Qmljs.Value.to_int value)
   | Error error ->
       fail ("fulfilled promise returned error: " ^ Qmljs.Error.message error));
  let rejected =
    Qmljs.Context.eval context
      {|
        function rejector() {
          return Promise.reject(new Error("structured boom"));
        }
        rejector();
      |}
  in
  (match Qmljs.Promise.await_result rejected with
   | Ok _ -> fail "rejected promise returned Ok"
   | Error error ->
       check_string "rejection message" "structured boom"
         (Qmljs.Error.message error);
       check_bool "rejection stack" true
         (Qmljs.Error.has_js_frame error ~function_name:"rejector"));
  expect_structured_error "await_exn rejection"
    (fun () -> Qmljs.Promise.await_exn rejected)
    (fun error ->
      check_string "await_exn rejection message" "structured boom"
        (Qmljs.Error.message error))

let test_object_introspection_api () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let object_ = Qmljs.Object.create context in
  let key = Qmljs.Value.string context "answer" in
  Qmljs.Object.define_data_property object_ ~key
    ~value:(Qmljs.Value.int context 42)
    ~writable:false ~enumerable:true ~configurable:false;
  check_bool "object has answer" true (Qmljs.Object.has object_ ~key);
  check_int "object get answer" 42
    (Qmljs.Value.to_int (Qmljs.Object.get object_ ~key));
  (match Qmljs.Object.get_own_property_descriptor object_ ~key with
   | None -> fail "missing answer descriptor"
   | Some descriptor ->
       check_int "descriptor value" 42
         (Qmljs.Value.to_int (Option.get descriptor.value));
       check_bool "descriptor writable" false (Option.get descriptor.writable);
       check_bool "descriptor enumerable" true descriptor.enumerable;
       check_bool "descriptor configurable" false descriptor.configurable;
       check_bool "descriptor getter absent" true (descriptor.get = None);
       check_bool "descriptor setter absent" true (descriptor.set = None));
  let tmp = Qmljs.Value.string context "tmp" in
  Qmljs.Object.set object_ ~name:"tmp" (Qmljs.Value.int context 7);
  check_bool "object has tmp" true (Qmljs.Object.has object_ ~key:tmp);
  check_bool "delete tmp" true (Qmljs.Object.delete object_ ~key:tmp);
  check_bool "tmp deleted" false (Qmljs.Object.has object_ ~key:tmp)

let test_module_namespace_api () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let loader =
    Qmljs.Module.Loader.memory
      [ "dep", "export const named = 40; export default 2;" ]
  in
  Qmljs.Runtime.set_module_loader runtime loader;
  let namespace =
    Qmljs.Module.eval context
      {|
        import base, { named } from "dep";
        export const answer = named + base;
        export default answer;
      |}
  in
  check_int "module default" 42
    (Qmljs.Value.to_int (Qmljs.Module.default namespace));
  (match Qmljs.Module.get namespace ~name:"answer" with
   | Some value -> check_int "module named export" 42 (Qmljs.Value.to_int value)
   | None -> fail "missing module named export");
  let keys = Qmljs.Module.keys namespace in
  check_bool "module keys include default" true (List.mem "default" keys);
  check_bool "module keys include answer" true (List.mem "answer" keys)

let test_eio_scoped_driver_api env =
  Eio.Switch.run @@ fun sw ->
  Qmljs.Eio.with_runtime ~sw ~clock:(Eio.Stdenv.clock env)
    (fun driver runtime context ->
      check_int "idle driver drain" 0 (Qmljs.Eio.drain_until_idle driver);
      let promise =
        Qmljs.Context.eval context "Promise.resolve(21).then(x => x * 2)"
      in
      let drained = Qmljs.Eio.drain_until_idle driver in
      if drained < 1 then fail "driver did not drain a pending promise job";
      check_int "driven promise result" 42
        (Qmljs.Value.to_int (Qmljs.Promise.result promise));
      Qmljs.Eio.cancel driver;
      check_bool "driver cancel requests interrupt" true
        (Qmljs.Runtime.interrupt_requested runtime);
      Qmljs.Runtime.clear_interrupt runtime;
      check_int "runtime reusable after driver cancel" 42
        (Qmljs.Value.to_int (Qmljs.Context.eval context "6 * 7")))

let test_eio_promise_helper_api env =
  Eio.Switch.run @@ fun sw ->
  Qmljs.Eio.with_runtime ~sw ~clock:(Eio.Stdenv.clock env)
    (fun driver runtime context ->
      let fulfilled =
        Qmljs.Eio.promise driver context (fun () ->
            Eio.Fiber.yield ();
            Qmljs.Value.int context 42)
      in
      (match Qmljs.Promise.await_result fulfilled with
       | Ok value -> check_int "Eio promise fulfilled" 42 (Qmljs.Value.to_int value)
       | Error error ->
           fail ("Eio promise rejected: " ^ Qmljs.Error.message error));
      let foreign_context = Qmljs.Context.create runtime in
      let foreign = Qmljs.Value.int foreign_context 41 in
      let foreign_return =
        Qmljs.Eio.promise ~name:"eio_foreign_return" driver context
          (fun () ->
            Eio.Fiber.yield ();
            foreign)
      in
      (match Qmljs.Promise.await_result foreign_return with
       | Ok _ -> fail "Eio foreign return unexpectedly fulfilled"
       | Error error ->
           check_kind "Eio foreign return kind"
             Qmljs.Error.Host_callback_defect (Qmljs.Error.kind error);
           check_string "Eio foreign return message"
             "QuickJS value belongs to a different context"
             (Qmljs.Error.message error));
      let rejected =
        Qmljs.Eio.promise ~name:"eio_promise_fail" driver context (fun () ->
            Eio.Fiber.yield ();
            raise
              (Qmljs.Error.host ~message:"eio promise boom"
                 ~metadata:[ "phase", "promise" ]))
      in
      match Qmljs.Promise.await_result rejected with
      | Ok _ -> fail "Eio promise unexpectedly fulfilled"
      | Error error ->
          check_kind "Eio promise rejection kind" Qmljs.Error.Host_callback_defect
            (Qmljs.Error.kind error);
          check_string "Eio promise rejection message" "eio promise boom"
            (Qmljs.Error.message error);
          check_bool "Eio promise metadata" true
            (List.mem ("phase", "promise") (Qmljs.Error.metadata error));
      let cancelled =
        Qmljs.Eio.promise ~name:"eio_promise_cancelled" driver context
          (fun () -> raise (Eio.Cancel.Cancelled (Failure "cancelled job")))
      in
      match Qmljs.Promise.await_result cancelled with
      | Ok _ -> fail "cancelled Eio promise unexpectedly fulfilled"
      | Error error ->
          check_kind "Eio promise cancellation kind"
            Qmljs.Error.Host_callback_defect (Qmljs.Error.kind error);
          check_bool "Eio promise cancellation has message" true
            (String.length (Qmljs.Error.message error) > 0))

let test_eio_queued_host_jobs_are_scoped env =
  let callback_ran = ref false in
  Eio.Switch.run @@ fun sw ->
  Qmljs.Eio.with_runtime ~sw ~clock:(Eio.Stdenv.clock env)
    (fun driver _runtime context ->
      Qmljs.Function.set_global_async context ~name:"queued_work" ~arity:0
        (fun _args ->
          callback_ran := true;
          Qmljs.Value.int context 1);
      let promise = Qmljs.Context.eval context "queued_work()" in
      Qmljs.Eio.cancel driver;
      (match Qmljs.Promise.await_result promise with
       | Ok _ -> fail "cancelled queued host job unexpectedly fulfilled"
       | Error error ->
           check_kind "cancelled queued host job kind"
             Qmljs.Error.Host_callback_defect (Qmljs.Error.kind error);
           check_string "cancelled queued host job message"
             "async host callback cancelled" (Qmljs.Error.message error);
           check_bool "cancelled queued host job did not run" false
             !callback_ran));
  let stale_runtime = ref None in
  let stale_ran = ref false in
  Eio.Switch.run @@ fun sw ->
  Qmljs.Eio.with_runtime ~sw ~clock:(Eio.Stdenv.clock env)
    (fun _driver runtime context ->
      stale_runtime := Some runtime;
      Qmljs.Function.set_global_async context ~name:"stale_work" ~arity:0
        (fun _args ->
          stale_ran := true;
          Qmljs.Value.int context 1);
      ignore (Qmljs.Context.eval context "stale_work()" : Qmljs.Value.t));
  (match !stale_runtime with
   | None -> fail "test did not capture runtime"
   | Some runtime ->
       expect_structured_error "stale host job after scope"
         (fun () -> ignore (Qmljs.Async.drain_jobs runtime : int))
         (fun error ->
           check_kind "stale host job closed runtime" Qmljs.Error.Closed_handle
             (Qmljs.Error.kind error)));
  check_bool "stale queued host job did not run" false !stale_ran

let test_async_host_callback_closed_context_crash env =
  Eio.Switch.run @@ fun sw ->
  Qmljs.Eio.with_runtime ~sw ~clock:(Eio.Stdenv.clock env)
    (fun driver runtime context ->
      let result = Qmljs.Value.int context 1 in
      Qmljs.Function.set_global_async context ~name:"closed_context_async"
        ~arity:0
        (fun _args ->
          Eio.Fiber.yield ();
          result);
      ignore (Qmljs.Context.eval context "closed_context_async()" : Qmljs.Value.t);
      ignore (test_context_close context : bool);
      ignore (Qmljs.Eio.drain_until_idle driver : int);
      let fresh = Qmljs.Context.create runtime in
      check_int "runtime reusable after closed async context" 2
        (Qmljs.Value.to_int (Qmljs.Context.eval fresh "1 + 1")))

let tests env =
  [ "call argument count over int max is rejected"
  , test_call_argument_count_over_int_max_is_rejected
  ; "C stubs initialize OCaml arrays before allocating elements"
  , test_c_stubs_initialize_ocaml_arrays_before_allocating_elements
  ; "host callback argument wrapping allocation failure is JS exception"
  , test_host_callback_argument_wrapping_allocation_failure_is_js_exception
  ; "with preserves body and cleanup failures"
  , test_with_preserves_body_and_cleanup_failures
  ; "sync host callback timeout is preemptive"
  , test_sync_host_callback_timeout_is_preemptive
  ; "eval_with_timeout does not drain microtasks on success"
  , test_eval_with_timeout_does_not_drain_microtasks_on_success
  ; "timeout preserves original structured stack"
  , test_timeout_preserves_original_structured_stack
  ; "eval_with_timeout closed runtime masks error"
  , test_eval_with_timeout_closed_runtime_masks_error
  ; "Bigint.of_string does not execute code"
  , test_bigint_of_string_does_not_execute_code
  ; "Json.stringify undefined is error not string"
  , test_json_stringify_undefined_is_error_not_string
  ; "NUL in public string keys is not silently truncated"
  , test_nul_in_public_string_keys_is_not_silently_truncated
  ; "user error message interrupted is JavaScript error"
  , test_user_error_message_interrupted_is_javascript_error
  ; "throwing error getters do not poison context"
  , test_throwing_error_getters_do_not_poison_context
  ; "native module host error metadata survives"
  , test_native_module_host_error_metadata_survives
  ; "module loader host error metadata survives"
  , test_module_loader_host_error_metadata_survives
  ; "module loader deny host error metadata survives"
  , test_module_loader_deny_host_error_metadata_survives
  ; "promise rejection preserves JS stack"
  , test_promise_rejection_preserves_js_stack
  ; "deep JS cause chain is preserved", test_deep_js_cause_chain_is_preserved
  ; "same-runtime cross-context object set is rejected"
  , test_same_runtime_cross_context_object_set_is_rejected
  ; "same-runtime cross-context function call is rejected"
  , test_same_runtime_cross_context_function_call_is_rejected
  ; "function call cross-context arg leak"
  , test_function_call_cross_context_arg_leak
  ; "sync host callback foreign return is JS host defect"
  , test_sync_host_callback_foreign_return_is_js_host_defect
  ; "json import attribute loads JSON module"
  , test_json_import_attribute_loads_json_module
  ; "structured error kind and metadata API"
  , test_error_kind_and_metadata_api
  ; "structured promise await API", test_structured_promise_await_api
  ; "object introspection API", test_object_introspection_api
  ; "module namespace API", test_module_namespace_api
  ; "Eio scoped driver API", (fun () -> test_eio_scoped_driver_api env)
  ; "Eio promise helper API", (fun () -> test_eio_promise_helper_api env)
  ; "Eio queued host jobs are scoped"
  , (fun () -> test_eio_queued_host_jobs_are_scoped env)
  ; "async host callback closed context crash"
  , (fun () -> test_async_host_callback_closed_context_crash env)
  ]

let () =
  Eio_main.run @@ fun env ->
  List.iter
    (fun (name, test) ->
      match test () with
      | () -> Printf.eprintf "PASS: %s\n%!" name
      | exception exn ->
          Printf.eprintf "FAIL: %s: %s\n%!" name (Printexc.to_string exn);
          raise exn)
    (tests env)
