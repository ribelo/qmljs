type race =
  | Resolved
  | Cancelled

let fail message = raise (Failure message)

let check_int label expected actual =
  if actual <> expected then
    fail (Printf.sprintf "%s: expected %d, got %d" label expected actual)

let check_string label expected actual =
  if not (String.equal actual expected) then
    fail (Printf.sprintf "%s: expected %S, got %S" label expected actual)

let check_bool label expected actual =
  if actual <> expected then
    fail (Printf.sprintf "%s: expected %b, got %b" label expected actual)

let expect_qmljs_error label fn =
  match fn () with
  | _ -> fail (label ^ ": expected Qmljs.Error")
  | exception Qmljs.Error _ -> ()

let expect_structured_error label fn assert_error =
  match Qmljs.Error.capture fn with
  | Ok _ -> fail (label ^ ": expected structured error")
  | Error error -> assert_error error

(* Runs [f] on the Async scheduler and returns its value, re-raising the
   original exception so structured qmljs errors keep their classification. *)
let run_async f =
  match
    Async.Thread_safe.block_on_async (fun () ->
      Async.Monitor.try_with ~extract_exn:true f)
  with
  | Ok (Ok value) -> value
  | Ok (Error exn) | Error exn -> raise exn

let with_context fn =
  let runtime = Qmljs.Runtime.create () in
  let context = Qmljs.Context.create runtime in
  fn runtime context

let test_eval () =
  with_context @@ fun _runtime context ->
  let value = Qmljs.Context.eval context "40 + 2" in
  check_int "eval int" 42 (Qmljs.Value.to_int value)

let test_runtime_interrupt_flag () =
  with_context @@ fun runtime _context ->
  check_bool "interrupt starts clear" false
    (Qmljs.Runtime.interrupt_requested runtime);
  Qmljs.Runtime.request_interrupt runtime;
  check_bool "interrupt requested" true
    (Qmljs.Runtime.interrupt_requested runtime);
  Qmljs.Runtime.clear_interrupt runtime;
  check_bool "interrupt cleared" false
    (Qmljs.Runtime.interrupt_requested runtime)

let test_eval_timeout () =
  with_context @@ fun runtime context ->
  let value = Qmljs.Context.eval context "20 + 22" in
  check_int "eval" 42 (Qmljs.Value.to_int value);
  expect_structured_error "expected JavaScript timeout"
    (fun () ->
      Qmljs.Context.eval_with_timeout
        ~timeout:0.01 context "while (true) {}")
    (fun error ->
      if not (String.contains (Qmljs.Error.message error) 't') then
        fail "missing timeout message";
      check_bool "timeout clears interrupt" false
        (Qmljs.Runtime.interrupt_requested runtime);
      let value = Qmljs.Context.eval context "1 + 1" in
      check_int "eval after timeout" 2 (Qmljs.Value.to_int value));
  expect_structured_error "compact infinite loop timeout"
    (fun () ->
      Qmljs.Context.eval_with_timeout
        ~timeout:0.01 context "while(1){}")
    (fun error ->
      check_bool "compact timeout kind" true
        (Qmljs.Error.is_resource_limit error))

let test_conversions () =
  with_context @@ fun _runtime context ->
  check_bool "bool roundtrip" true
    (Qmljs.Value.to_bool (Qmljs.Value.bool context true));
  check_int "int roundtrip" 7
    (Qmljs.Value.to_int (Qmljs.Value.int context 7));
  check_int "large int roundtrip" 5_000_000_000
    (Qmljs.Value.to_int (Qmljs.Value.int context 5_000_000_000));
  check_string "string roundtrip" "quickjs-ng"
    (Qmljs.Value.to_string (Qmljs.Value.string context "quickjs-ng"))

let test_closed_handles_raise () =
  let runtime_ref = ref None in
  let promise_ref = ref None in
  (Qmljs.Runtime.with_ @@ fun runtime ->
   Qmljs.Context.with_ runtime @@ fun context ->
   runtime_ref := Some runtime;
   promise_ref := Some (Qmljs.Context.eval context "Promise.resolve(1)"));
  let runtime = Option.get !runtime_ref in
  let promise = Option.get !promise_ref in
  expect_qmljs_error "closed runtime job check" (fun () ->
      Qmljs.Runtime.is_job_pending runtime);
  expect_qmljs_error "closed runtime interrupt check" (fun () ->
      Qmljs.Runtime.interrupt_requested runtime);
  expect_qmljs_error "closed promise is check" (fun () -> Qmljs.Promise.is promise);
  expect_qmljs_error "closed promise state check" (fun () ->
      Qmljs.Promise.state promise)

let test_context_scope_closes_escaped_values () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  let escaped = ref None in
  (Qmljs.Context.with_ runtime @@ fun context ->
   escaped := Some (Qmljs.Context.eval context "21 * 2"));
  let value = Option.get !escaped in
  expect_qmljs_error "escaped value is closed" (fun () ->
      Qmljs.Value.to_int value);
  Qmljs.Context.with_ runtime @@ fun context ->
  let value = Qmljs.Context.eval context "40 + 2" in
  check_int "runtime reusable after context close" 42 (Qmljs.Value.to_int value)

let test_error () =
  with_context @@ fun _runtime context ->
  expect_structured_error "expected JavaScript error"
    (fun () -> Qmljs.Context.eval context "throw new Error('boom')")
    (fun error ->
      if not (String.contains (Qmljs.Error.message error) 'b') then
        fail "missing error message")

let test_promise_jobs () =
  with_context @@ fun runtime context ->
  let promise =
    Qmljs.Context.eval context "Promise.resolve(41).then(x => x + 1)"
  in
  check_bool "is promise" true (Qmljs.Promise.is promise);
  let jobs = run_async (fun () -> Qmljs.Async.drain_jobs ~yield_every:1 runtime) in
  if jobs <= 0 then fail "expected at least one QuickJS job";
  check_int "promise result" 42
    (Qmljs.Value.to_int (Qmljs.Promise.result promise))

let test_await_promise () =
  with_context @@ fun _runtime context ->
  let promise =
    Qmljs.Context.eval context "Promise.resolve('ok').then(x => x + '!')"
  in
  let result = run_async (fun () -> Qmljs.Promise.await ~yield_every:1 promise) in
  check_string "await result" "ok!" (Qmljs.Value.to_string result)

let test_rejected_promise () =
  with_context @@ fun _runtime context ->
  let promise = Qmljs.Context.eval context "Promise.reject(new Error('bad'))" in
  expect_structured_error "expected rejected promise"
    (fun () -> run_async (fun () -> Qmljs.Promise.await ~yield_every:1 promise))
    (fun error ->
      if not (String.contains (Qmljs.Error.message error) 'b') then
        fail "missing rejection reason")

let test_host_function () =
  with_context @@ fun _runtime context ->
  Qmljs.Function.set_global context ~name:"add1" ~arity:1 (fun args ->
      Qmljs.Value.int context (Qmljs.Value.to_int args.(0) + 1));
  let result = Qmljs.Context.eval context "add1(41)" in
  check_int "host function result" 42 (Qmljs.Value.to_int result)

let test_host_function_error () =
  with_context @@ fun _runtime context ->
  Qmljs.Function.set_global context ~name:"explode" ~arity:0 (fun _args ->
      raise (Qmljs.Error "host boom"));
  expect_structured_error "expected host callback error"
    (fun () -> Qmljs.Context.eval context "explode()")
    (fun error ->
      if not (String.contains (Qmljs.Error.message error) 'b') then
        fail "missing host error")

let test_async_host_function () =
  with_context @@ fun _runtime context ->
  Qmljs.Function.set_global_async context ~name:"later_add1" ~arity:1
    (fun args ->
      Async.Deferred.map (Async.Scheduler.yield ()) ~f:(fun () ->
        Qmljs.Value.int context (Qmljs.Value.to_int args.(0) + 1)));
  let promise = Qmljs.Context.eval context "later_add1(40).then(x => x + 1)" in
  let result = run_async (fun () -> Qmljs.Promise.await ~yield_every:1 promise) in
  check_int "async host function result" 42 (Qmljs.Value.to_int result)

let test_async_host_function_error () =
  with_context @@ fun _runtime context ->
  Qmljs.Function.set_global_async context ~name:"later_fail" ~arity:0
    (fun _args ->
      Async.Deferred.map (Async.Scheduler.yield ()) ~f:(fun () ->
        raise (Failure "async host boom")));
  let promise = Qmljs.Context.eval context "later_fail()" in
  expect_structured_error "expected async host rejection"
    (fun () -> run_async (fun () -> Qmljs.Promise.await ~yield_every:1 promise))
    (fun error ->
      if not (String.contains (Qmljs.Error.message error) 'b') then
        fail "missing async host error")

(* Async cannot cancel a Deferred, so a caller abandons a pending await by
   racing it. The abandoned await must stay harmless: it sleeps on the driver
   instead of spinning, and once the runtime closes it ends with a closed-handle
   failure inside the caller's monitor instead of escaping to the top-level
   monitor. Everything runs inside one scheduler run so the abandoned await
   never touches QuickJS from another thread. *)
let test_cancel_pending_await () =
  let abandoned = ref None in
  let race =
    run_async @@ fun () ->
    let race =
      Qmljs.Async.with_runtime @@ fun _driver _runtime context ->
      let promise = Qmljs.Context.eval context "new Promise(() => {})" in
      let await =
        Async.Monitor.try_with ~extract_exn:true (fun () ->
          Qmljs.Promise.await ~yield_every:1 promise)
      in
      Async.upon await (fun outcome -> abandoned := Some outcome);
      Async.Deferred.any
        [ Async.Deferred.map await ~f:(fun (_ : (Qmljs.value, exn) result) ->
            Resolved)
        ; Async.Deferred.map (Async.Scheduler.yield ()) ~f:(fun () ->
            Cancelled)
        ]
    in
    let rec wait_abandoned () =
      match !abandoned with
      | Some _ -> Async.Deferred.unit
      | None -> Async.Deferred.bind (Async.Scheduler.yield ()) ~f:wait_abandoned
    in
    Async.Deferred.bind race ~f:(fun race ->
      Async.Deferred.map (wait_abandoned ()) ~f:(fun () -> race))
  in
  (match race with
   | Cancelled -> ()
   | Resolved -> fail "pending promise resolved unexpectedly");
  match !abandoned with
  | Some (Error (Qmljs.Error _)) -> ()
  | Some (Error exn) ->
      fail ("abandoned await ended with " ^ Printexc.to_string exn)
  | Some (Ok _) -> fail "abandoned await resolved"
  | None -> fail "abandoned await never ended"

let () =
  test_eval ();
  test_runtime_interrupt_flag ();
  test_eval_timeout ();
  test_conversions ();
  test_closed_handles_raise ();
  test_context_scope_closes_escaped_values ();
  test_error ();
  test_promise_jobs ();
  test_await_promise ();
  test_rejected_promise ();
  test_host_function ();
  test_host_function_error ();
  test_async_host_function ();
  test_async_host_function_error ();
  test_cancel_pending_await ()
