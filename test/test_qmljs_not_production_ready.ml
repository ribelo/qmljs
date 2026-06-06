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

let tests env =
  [ "sync host callback timeout is preemptive"
  , test_sync_host_callback_timeout_is_preemptive
  ; "eval_with_timeout does not drain microtasks on success"
  , test_eval_with_timeout_does_not_drain_microtasks_on_success
  ; "promise rejection preserves JS stack"
  , test_promise_rejection_preserves_js_stack
  ; "deep JS cause chain is preserved", test_deep_js_cause_chain_is_preserved
  ; "same-runtime cross-context object set is rejected"
  , test_same_runtime_cross_context_object_set_is_rejected
  ; "same-runtime cross-context function call is rejected"
  , test_same_runtime_cross_context_function_call_is_rejected
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
