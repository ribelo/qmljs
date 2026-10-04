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

let test_invalid_timeout_inputs_are_rejected () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  List.iter (fun (name, timeout) ->
    expect_invalid_arg ("invalid timeout: " ^ name) (fun () ->
      ignore
        (Qmljs.Context.eval_with_timeout ~timeout context
           "globalThis.invalidTimeoutEvaluated = true; 42"
          : Qmljs.value));
    check_bool ("invalid timeout does not evaluate: " ^ name) true
      (Qmljs.Value.to_bool
         (Qmljs.Context.eval context
            "typeof invalidTimeoutEvaluated === 'undefined'")))
    [ "NaN", nan; "positive infinity", infinity
    ; "negative infinity", neg_infinity; "huge finite", Float.max_float
    ; "unrepresentable nanoseconds", 9.223372036854776e9 ];
  check_int "valid timeout after rejected inputs" 42
    (Qmljs.Value.to_int
       (Qmljs.Context.eval_with_timeout ~timeout:1. context "40 + 2"))

let check_inner_kind label = function
  | Some kind -> check_kind label Qmljs.Error.Timeout kind
  | None -> fail (label ^ ": inner eval did not raise")

let test_nested_timeout_keeps_earliest_deadline () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let entered = ref false in
  let inner_kind = ref None in
  let loader = Qmljs.Module.Loader.callback
    ~normalize:(fun ~base:_ ~specifier ~attributes:_ ->
      entered := true;
      (match
         Qmljs.Error.capture (fun () ->
           ignore
             (Qmljs.Context.eval_with_timeout ~timeout:1. context
                "(function () { const end = Date.now() + 150; while (Date.now() < end) {}; return 42; })()"
               : Qmljs.value))
       with
       | Ok _ -> ()
       | Error error -> inner_kind := Some (Qmljs.Error.kind error));
      Ok specifier)
    ~load:(fun _ -> Ok (Some "export default 42;"))
  in
  Qmljs.Runtime.set_module_loader runtime loader;
  expect_structured_error "outer timeout during nested loader eval"
    (fun () -> Qmljs.Context.eval_module_with_timeout ~timeout:0.05 context
      "import value from 'nested.js'; export default value;")
    (fun error -> check_kind "outer timeout kind" Qmljs.Error.Timeout
       (Qmljs.Error.kind error));
  check_bool "nested loader callback actually entered" true !entered;
  check_inner_kind "inner eval inherits earlier outer deadline" !inner_kind;
  check_int "runtime recovers after nested timeout" 42
    (Qmljs.Value.to_int (Qmljs.Context.eval context "40 + 2"))

let test_nested_timeout_restores_outer_deadline () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let entered = ref false in
  let inner_result = ref None in
  let loader = Qmljs.Module.Loader.callback
    ~normalize:(fun ~base:_ ~specifier ~attributes:_ -> Ok specifier)
    ~load:(fun _ ->
      entered := true;
      inner_result :=
        Some
          (Qmljs.Error.capture (fun () ->
             Qmljs.Value.to_int
               (Qmljs.Context.eval_with_timeout ~timeout:1. context "40 + 2")));
      Ok (Some "const end = Date.now() + 150; while (Date.now() < end) {}; export default 42;"))
  in
  Qmljs.Runtime.set_module_loader runtime loader;
  expect_structured_error "outer timeout after successful nested loader eval"
    (fun () -> Qmljs.Context.eval_module_with_timeout ~timeout:0.05 context
      "import value from 'nested.js'; export default value;")
    (fun error -> check_kind "restored outer timeout kind" Qmljs.Error.Timeout
       (Qmljs.Error.kind error));
  check_bool "successful nested loader callback actually entered" true !entered;
  (match !inner_result with
   | Some (Ok value) ->
       check_int "nested eval completes before outer deadline" 42 value
   | Some (Error error) ->
       fail
         ("nested eval failed before the outer deadline: "
         ^ Qmljs.Error.message error)
   | None -> fail "nested loader callback was not entered");
  check_int "runtime recovers after restored outer timeout" 42
    (Qmljs.Value.to_int (Qmljs.Context.eval context "40 + 2"))

(* An inner bracket that exhausts its own budget must not leave its expired
   deadline armed: the outer bracket gets its own, later deadline back. *)
let test_nested_inner_timeout_keeps_outer_budget () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let entered = ref false in
  let inner_kind = ref None in
  let loader = Qmljs.Module.Loader.callback
    ~normalize:(fun ~base:_ ~specifier ~attributes:_ -> Ok specifier)
    ~load:(fun _ ->
      entered := true;
      (match
         Qmljs.Error.capture (fun () ->
           ignore
             (Qmljs.Context.eval_with_timeout ~timeout:0.005 context "while(1){}"
               : Qmljs.value))
       with
       | Ok _ -> ()
       | Error error -> inner_kind := Some (Qmljs.Error.kind error));
      Ok (Some "const end = Date.now() + 20; while (Date.now() < end) {}; export default 42;"))
  in
  Qmljs.Runtime.set_module_loader runtime loader;
  check_int "outer eval survives a caught inner timeout" 42
    (Qmljs.Module.default_export_to_int
       (Qmljs.Context.eval_module_with_timeout ~timeout:1. context
          "import value from 'nested.js'; export default value;"));
  check_bool "inner loader callback actually entered" true !entered;
  check_inner_kind "inner eval times out on its own budget" !inner_kind;
  check_bool "inner timeout leaves no interrupt request" false
    (Qmljs.Runtime.interrupt_requested runtime);
  check_int "runtime recovers after nested inner timeout" 42
    (Qmljs.Value.to_int (Qmljs.Context.eval context "40 + 2"))

(* A request set before the evaluation belongs to the caller. Timed eval must
   abort the way untimed eval does, and must not consume the request. *)
let test_pending_interrupt_matches_untimed_eval () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let abort ~label eval =
    Qmljs.Runtime.request_interrupt runtime;
    let error =
      match Qmljs.Error.capture eval with
      | Ok _ -> fail (label ^ ": eval completed despite a pending request")
      | Error error -> error
    in
    check_bool (label ^ ": request stays pending") true
      (Qmljs.Runtime.interrupt_requested runtime);
    Qmljs.Runtime.clear_interrupt runtime;
    error
  in
  let untimed =
    abort ~label:"untimed eval" (fun () ->
      Qmljs.Context.eval context "while(1){}")
  in
  let timed =
    abort ~label:"timed eval" (fun () ->
      Qmljs.Context.eval_with_timeout ~timeout:1. context "while(1){}")
  in
  check_kind "pending interrupt kind" Qmljs.Error.Timeout
    (Qmljs.Error.kind timed);
  check_kind "timed and untimed pending interrupt kind agree"
    (Qmljs.Error.kind untimed) (Qmljs.Error.kind timed);
  check_string "timed and untimed pending interrupt abort the same way"
    (Qmljs.Error.message untimed) (Qmljs.Error.message timed);
  check_int "runtime recovers after pending requests" 42
    (Qmljs.Value.to_int
       (Qmljs.Context.eval_with_timeout ~timeout:1. context "40 + 2"))

(* The large end of the representable range must arm, not overflow, and the
   deadline must be gone once the bracket exits. *)
let test_large_representable_timeout_is_cleared () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  Qmljs.Function.set_global context ~name:"deadline_probe" ~arity:0
    (fun _args -> Qmljs.Value.int context 7);
  check_int "large representable timeout eval" 42
    (Qmljs.Value.to_int
       (Qmljs.Context.eval_with_timeout ~timeout:9.0e9 context "40 + 2"));
  check_int "deadline is cleared after the timed eval" 7
    (Qmljs.Value.to_int (Qmljs.Context.eval context "deadline_probe()"))

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
  ; "invalid timeout inputs rejected", test_invalid_timeout_inputs_are_rejected
  ; "nested timeout keeps earliest deadline", test_nested_timeout_keeps_earliest_deadline
  ; "nested timeout restores outer deadline", test_nested_timeout_restores_outer_deadline
  ; "nested inner timeout keeps outer budget"
  , test_nested_inner_timeout_keeps_outer_budget
  ; "pending interrupt matches untimed eval"
  , test_pending_interrupt_matches_untimed_eval
  ; "large representable timeout is cleared"
  , test_large_representable_timeout_is_cleared
  ; "host metadata survives JS cause chain"
  , test_host_metadata_survives_js_cause_chain
  ; "await_result rejects non-promise", test_await_result_rejects_non_promise
  ; "promise tracking API", test_promise_tracking_api
  ]
