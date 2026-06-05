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
  let runtime = Quick_ml.Runtime.create () in
  let context = Quick_ml.Context.create runtime in
  fn runtime context

let test_eval () =
  with_context @@ fun _runtime context ->
  let value = Quick_ml.Context.eval context "40 + 2" in
  check_int "eval int" 42 (Quick_ml.Value.to_int value)

let test_conversions () =
  with_context @@ fun _runtime context ->
  check_bool "bool roundtrip" true
    (Quick_ml.Value.to_bool (Quick_ml.Value.bool context true));
  check_int "int roundtrip" 7
    (Quick_ml.Value.to_int (Quick_ml.Value.int context 7));
  check_string "string roundtrip" "quickjs-ng"
    (Quick_ml.Value.to_string (Quick_ml.Value.string context "quickjs-ng"))

let test_error () =
  with_context @@ fun _runtime context ->
  match Quick_ml.Context.eval context "throw new Error('boom')" with
  | _ -> fail "expected JavaScript error"
  | exception Quick_ml.Error message ->
      if not (String.contains message 'b') then fail "missing error message"

let test_promise_jobs () =
  with_context @@ fun runtime context ->
  let promise =
    Quick_ml.Context.eval context "Promise.resolve(41).then(x => x + 1)"
  in
  check_bool "is promise" true (Quick_ml.Promise.is promise);
  let jobs = Quick_ml.Async.drain_jobs ~yield_every:1 runtime in
  if jobs <= 0 then fail "expected at least one QuickJS job";
  check_int "promise result" 42
    (Quick_ml.Value.to_int (Quick_ml.Promise.result promise))

let test_await_promise () =
  with_context @@ fun _runtime context ->
  let promise =
    Quick_ml.Context.eval context "Promise.resolve('ok').then(x => x + '!')"
  in
  let result = Quick_ml.Promise.await ~yield_every:1 promise in
  check_string "await result" "ok!" (Quick_ml.Value.to_string result)

let test_rejected_promise () =
  with_context @@ fun _runtime context ->
  let promise = Quick_ml.Context.eval context "Promise.reject(new Error('bad'))" in
  match Quick_ml.Promise.await ~yield_every:1 promise with
  | _ -> fail "expected rejected promise"
  | exception Quick_ml.Error message ->
      if not (String.contains message 'b') then fail "missing rejection reason"

let test_cancel_pending_await () =
  with_context @@ fun _runtime context ->
  let promise = Quick_ml.Context.eval context "new Promise(() => {})" in
  let result =
    Eio.Fiber.first
      (fun () ->
        ignore (Quick_ml.Promise.await ~yield_every:1 promise : Quick_ml.value);
        Resolved)
      (fun () ->
        Eio.Fiber.yield ();
        Cancelled)
  in
  match result with
  | Cancelled -> ()
  | Resolved -> fail "pending promise resolved unexpectedly"

let () =
  Eio_main.run @@ fun _env ->
  test_eval ();
  test_conversions ();
  test_error ();
  test_promise_jobs ();
  test_await_promise ();
  test_rejected_promise ();
  test_cancel_pending_await ()
