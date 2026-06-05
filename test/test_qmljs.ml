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

let test_eval_timeout env =
  with_context @@ fun runtime context ->
  let value =
    Qmljs.Context.eval_in_domain
      ~domain_mgr:(Eio.Stdenv.domain_mgr env)
      context "20 + 22"
  in
  check_int "eval in domain" 42 (Qmljs.Value.to_int value);
  match
    Qmljs.Context.eval_with_timeout
      ~domain_mgr:(Eio.Stdenv.domain_mgr env)
      ~clock:(Eio.Stdenv.clock env)
      ~timeout:0.01 context "while (true) {}"
  with
  | _ -> fail "expected JavaScript timeout"
  | exception Qmljs.Error message ->
      if not (String.contains message 't') then fail "missing timeout message";
      check_bool "timeout clears interrupt" false
        (Qmljs.Runtime.interrupt_requested runtime);
      let value = Qmljs.Context.eval context "1 + 1" in
      check_int "eval after timeout" 2 (Qmljs.Value.to_int value)

let test_conversions () =
  with_context @@ fun _runtime context ->
  check_bool "bool roundtrip" true
    (Qmljs.Value.to_bool (Qmljs.Value.bool context true));
  check_int "int roundtrip" 7
    (Qmljs.Value.to_int (Qmljs.Value.int context 7));
  check_string "string roundtrip" "quickjs-ng"
    (Qmljs.Value.to_string (Qmljs.Value.string context "quickjs-ng"))

let test_error () =
  with_context @@ fun _runtime context ->
  match Qmljs.Context.eval context "throw new Error('boom')" with
  | _ -> fail "expected JavaScript error"
  | exception Qmljs.Error message ->
      if not (String.contains message 'b') then fail "missing error message"

let test_promise_jobs () =
  with_context @@ fun runtime context ->
  let promise =
    Qmljs.Context.eval context "Promise.resolve(41).then(x => x + 1)"
  in
  check_bool "is promise" true (Qmljs.Promise.is promise);
  let jobs = Qmljs.Async.drain_jobs ~yield_every:1 runtime in
  if jobs <= 0 then fail "expected at least one QuickJS job";
  check_int "promise result" 42
    (Qmljs.Value.to_int (Qmljs.Promise.result promise))

let test_await_promise () =
  with_context @@ fun _runtime context ->
  let promise =
    Qmljs.Context.eval context "Promise.resolve('ok').then(x => x + '!')"
  in
  let result = Qmljs.Promise.await ~yield_every:1 promise in
  check_string "await result" "ok!" (Qmljs.Value.to_string result)

let test_rejected_promise () =
  with_context @@ fun _runtime context ->
  let promise = Qmljs.Context.eval context "Promise.reject(new Error('bad'))" in
  match Qmljs.Promise.await ~yield_every:1 promise with
  | _ -> fail "expected rejected promise"
  | exception Qmljs.Error message ->
      if not (String.contains message 'b') then fail "missing rejection reason"

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
  match Qmljs.Context.eval context "explode()" with
  | _ -> fail "expected host callback error"
  | exception Qmljs.Error message ->
      if not (String.contains message 'b') then fail "missing host error"

let test_async_host_function () =
  with_context @@ fun _runtime context ->
  Qmljs.Function.set_global_async context ~name:"later_add1" ~arity:1
    (fun args ->
      Eio.Fiber.yield ();
      Qmljs.Value.int context (Qmljs.Value.to_int args.(0) + 1));
  let promise = Qmljs.Context.eval context "later_add1(40).then(x => x + 1)" in
  let result = Qmljs.Promise.await ~yield_every:1 promise in
  check_int "async host function result" 42 (Qmljs.Value.to_int result)

let test_async_host_function_error () =
  with_context @@ fun _runtime context ->
  Qmljs.Function.set_global_async context ~name:"later_fail" ~arity:0
    (fun _args ->
      Eio.Fiber.yield ();
      raise (Failure "async host boom"));
  let promise = Qmljs.Context.eval context "later_fail()" in
  match Qmljs.Promise.await ~yield_every:1 promise with
  | _ -> fail "expected async host rejection"
  | exception Qmljs.Error message ->
      if not (String.contains message 'b') then fail "missing async host error"

let test_cancel_pending_await () =
  with_context @@ fun _runtime context ->
  let promise = Qmljs.Context.eval context "new Promise(() => {})" in
  let result =
    Eio.Fiber.first
      (fun () ->
        ignore (Qmljs.Promise.await ~yield_every:1 promise : Qmljs.value);
        Resolved)
      (fun () ->
        Eio.Fiber.yield ();
        Cancelled)
  in
  match result with
  | Cancelled -> ()
  | Resolved -> fail "pending promise resolved unexpectedly"

let () =
  Eio_main.run @@ fun env ->
  test_eval ();
  test_runtime_interrupt_flag ();
  test_eval_timeout env;
  test_conversions ();
  test_error ();
  test_promise_jobs ();
  test_await_promise ();
  test_rejected_promise ();
  test_host_function ();
  test_host_function_error ();
  test_async_host_function ();
  test_async_host_function_error ();
  test_cancel_pending_await ()
