open Qmljs_test_common

let test_runtime_hooks_api () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  let usage = Qmljs.Runtime.memory_usage runtime in
  check_bool "runtime malloc size is non-negative" true
    (Int64.compare usage.Qmljs.Runtime.malloc_size 0L >= 0);
  check_bool "runtime atom count is non-negative" true
    (Int64.compare usage.Qmljs.Runtime.atom_count 0L >= 0);
  let previous_threshold = Qmljs.Runtime.gc_threshold runtime in
  Qmljs.Runtime.set_gc_threshold runtime 4096;
  check_int "runtime GC threshold round-trip" 4096
    (Qmljs.Runtime.gc_threshold runtime);
  Qmljs.Runtime.set_info runtime "qmljs test runtime";
  Qmljs.Runtime.set_info runtime "qmljs renamed test runtime";
  (match Qmljs.Runtime.set_info runtime "bad\000info" with
   | () -> fail "runtime info accepted NUL byte"
   | exception Invalid_argument _ -> ());
  Qmljs.Runtime.run_gc runtime;
  Qmljs.Runtime.set_gc_threshold runtime previous_threshold

let test_closed_runtime_info_is_rejected () =
  let runtime =
    Qmljs.Runtime.with_ @@ fun runtime ->
    Qmljs.Runtime.set_info runtime "closing runtime";
    runtime
  in
  expect_structured_error "closed runtime info"
    (fun () -> Qmljs.Runtime.set_info runtime "closed")
    (fun error ->
      check_kind "closed runtime info kind" Qmljs.Error.Closed_handle
        (Qmljs.Error.kind error))

let with_temp_file source fn =
  let path = Filename.temp_file "qmljs-eval-" ".js" in
  Fun.protect
    ~finally:(fun () ->
      try Sys.remove path with
      | Sys_error _ -> ())
    (fun () ->
      let channel = open_out_bin path in
      Fun.protect
        ~finally:(fun () -> close_out_noerr channel)
        (fun () -> output_string channel source);
      fn path)

let test_context_eval_options_api () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let top_level_await =
    Qmljs.Context.eval ~async:true context
      "globalThis.tlaResult = await Promise.resolve(42)"
  in
  (match Qmljs.Promise.await_result top_level_await with
   | Ok _value ->
       check_int "top-level await side effect" 42
         (Qmljs.Value.to_int (Qmljs.Context.eval context "globalThis.tlaResult"))
   | Error error ->
       fail ("top-level await rejected: " ^ Qmljs.Error.message error));
  check_int "backtrace barrier eval" 42
    (Qmljs.Value.to_int
       (Qmljs.Context.eval ~backtrace_barrier:true context "40 + 2"));
  with_temp_file "40 + 2" @@ fun path ->
  check_int "eval_file result" 42
    (Qmljs.Value.to_int (Qmljs.Context.eval_file context path));
  with_temp_file "export default 42;" @@ fun path ->
  check_int "eval_module_file result" 42
    (Qmljs.Value.to_int (Qmljs.Context.eval_module_file context path))

let test_context_raw_intrinsics_api () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  let eval_or_fail label context source =
    match Qmljs.Error.capture (fun () -> Qmljs.Context.eval context source) with
    | Ok value -> value
    | Error error -> fail (label ^ ": " ^ Qmljs.Error.message error)
  in
  let eval_module_or_fail label context source =
    match
      Qmljs.Error.capture (fun () -> Qmljs.Context.eval_module context source)
    with
    | Ok value -> value
    | Error error -> fail (label ^ ": " ^ Qmljs.Error.message error)
  in
  Qmljs.Context.with_raw runtime @@ fun _context -> ();
  Qmljs.Context.with_raw ~intrinsics:[ Qmljs.Context.Eval ] runtime
  @@ fun context ->
  check_string "raw context has base objects" "function"
    (Qmljs.Value.to_string
       (eval_or_fail "raw base Object" context "typeof Object"));
  check_string "raw context omits JSON" "undefined"
    (Qmljs.Value.to_string
       (eval_or_fail "raw missing JSON" context "typeof JSON"));
  check_string "raw context omits Promise" "undefined"
    (Qmljs.Value.to_string
       (eval_or_fail "raw missing Promise" context "typeof Promise"));
  Qmljs.Context.with_raw
    ~intrinsics:
      [ Qmljs.Context.Eval
      ; Qmljs.Context.Json
      ; Qmljs.Context.Promise
      ; Qmljs.Context.Typed_arrays
      ; Qmljs.Context.Big_int
      ]
    runtime
  @@ fun context ->
  check_int "raw eval intrinsic" 42
    (Qmljs.Value.to_int
       (eval_or_fail "raw eval intrinsic" context "eval('40 + 2')"));
  check_int "raw JSON intrinsic" 42
    (Qmljs.Value.to_int
       (eval_or_fail "raw JSON intrinsic" context
          {|JSON.parse('{"answer":42}').answer|}));
  let promise =
    eval_or_fail "raw Promise expression" context
      "Promise.resolve(40).then((value) => value + 2)"
  in
  (match Qmljs.Promise.await_result promise with
   | Ok value -> check_int "raw Promise intrinsic" 42 (Qmljs.Value.to_int value)
   | Error error ->
       fail ("raw Promise intrinsic rejected: " ^ Qmljs.Error.message error));
  check_int "raw typed array intrinsic" 3
    (Qmljs.Value.to_int
       (eval_or_fail "raw typed array intrinsic" context
          "new Uint8Array([1, 2, 3]).byteLength"));
  check_string "raw BigInt intrinsic" "42"
    (Qmljs.Value.to_string
       (eval_or_fail "raw BigInt intrinsic" context "(40n + 2n).toString()"));
  check_int "raw context module eval" 42
    (Qmljs.Value.to_int
       (eval_module_or_fail "raw module eval" context "export default 40 + 2;"))

let test_timeout_has_distinct_error_kind () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  expect_structured_error "timeout kind"
    (fun () ->
      Qmljs.Context.eval_with_timeout ~timeout:0.005 context "while(1){}")
    (fun error ->
      check_kind "timeout kind" Qmljs.Error.Timeout (Qmljs.Error.kind error))

let test_host_metadata_survives_js_cause_chain () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  Qmljs.Function.set_global context ~name:"host_fail" ~arity:0 (fun _args ->
      raise
        (Qmljs.Error.host ~message:"host boom"
           ~metadata:[ "request_id", "req-42"; "phase", "callback" ]));
  expect_structured_error "host metadata cause"
    (fun () ->
      Qmljs.Context.eval context
        {|
          function entry() {
            try { host_fail(); }
            catch (e) { throw new Error("js wrapper", { cause: e }); }
          }
          entry();
        |})
    (fun error ->
      let cause = Qmljs.Error.cause_exn error in
      check_bool "host cause metadata survives JS wrapping" true
        (List.mem ("request_id", "req-42") (Qmljs.Error.metadata cause)))

let test_await_result_rejects_non_promise () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let value = Qmljs.Context.eval context "42" in
  match Qmljs.Promise.await_result value with
  | Error error ->
      check_kind "non-promise await kind" Qmljs.Error.JavaScript
        (Qmljs.Error.kind error)
  | Ok _ -> fail "await_result accepted a non-promise value"

let test_promise_tracking_api () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  Qmljs.Runtime.set_promise_rejection_tracking runtime true;
  ignore
    (Qmljs.Context.eval context
       "globalThis.trackedRejection = Promise.reject(new Error('tracker boom'))"
      : Qmljs.value);
  ignore (Qmljs.Runtime.drain_jobs runtime : int);
  let rejections = Qmljs.Runtime.take_promise_rejections runtime in
  check_bool "unhandled rejection tracked" true
    (List.exists
       (fun event ->
         (not event.Qmljs.Runtime.is_handled)
         && String.equal "tracker boom"
              (Qmljs.Error.message event.Qmljs.Runtime.reason))
       rejections);
  ignore
    (Qmljs.Context.eval context "globalThis.trackedRejection.catch(() => 0)"
      : Qmljs.value);
  ignore (Qmljs.Runtime.drain_jobs runtime : int);
  let handled = Qmljs.Runtime.take_promise_rejections runtime in
  check_bool "handled rejection tracked" true
    (List.exists
       (fun event ->
         event.Qmljs.Runtime.is_handled
         && String.equal "tracker boom"
              (Qmljs.Error.message event.Qmljs.Runtime.reason))
       handled);
  Qmljs.Runtime.set_promise_hook_tracking runtime true;
  let promise =
    Qmljs.Context.eval context "Promise.resolve(40).then((value) => value + 2)"
  in
  ignore (Qmljs.Promise.await promise : Qmljs.value);
  let hooks = Qmljs.Runtime.take_promise_hooks runtime in
  check_bool "promise init hook tracked" true
    (List.exists
       (fun event -> event.Qmljs.Runtime.hook = Qmljs.Runtime.Promise_init)
       hooks);
  check_bool "promise resolve hook tracked" true
    (List.exists
       (fun event -> event.Qmljs.Runtime.hook = Qmljs.Runtime.Promise_resolve)
       hooks);
  check_bool "promise parent hook tracked" true
    (List.exists
       (fun event ->
         event.Qmljs.Runtime.hook = Qmljs.Runtime.Promise_init
         && event.Qmljs.Runtime.has_parent)
       hooks);
  Qmljs.Runtime.set_promise_rejection_tracking runtime false;
  Qmljs.Runtime.set_promise_hook_tracking runtime false

let tests =
  [ "runtime hooks API", test_runtime_hooks_api
  ; "closed runtime info is rejected", test_closed_runtime_info_is_rejected
  ; "context eval options API", test_context_eval_options_api
  ; "context raw intrinsics API", test_context_raw_intrinsics_api
  ; "timeout has distinct error kind", test_timeout_has_distinct_error_kind
  ; "host metadata survives JS cause chain"
  , test_host_metadata_survives_js_cause_chain
  ; "await_result rejects non-promise", test_await_result_rejects_non_promise
  ; "promise tracking API", test_promise_tracking_api
  ]
