(* Bug-hunt suite.

   Each test asserts the behaviour qmljs *should* have. Every test in here is
   currently RED: it demonstrates a real defect in either the OCaml layer or the
   C stubs. The comment above each test explains why the asserted behaviour is
   the correct contract, so these are genuine failing tests, not failures on
   principle.

   Run with: nix develop .#oxcaml -c dune exec test/test_qmljs_bug_hunt.exe *)

let fail message = raise (Failure message)

let check_int label expected actual =
  if actual <> expected then
    fail (Printf.sprintf "%s: expected %d, got %d" label expected actual)

let check_bool label expected actual =
  if actual <> expected then
    fail (Printf.sprintf "%s: expected %b, got %b" label expected actual)

let with_context fn =
  let runtime = Qmljs.Runtime.create () in
  let context = Qmljs.Context.create runtime in
  fn runtime context

(* BUG 1 — silent integer precision loss.

   [Value.int] takes an OCaml [int] and [Value.to_int] reads one back. The two
   are an encoder/decoder pair, so for any value that fits in an OCaml [int]
   the round trip must be the identity. 9007199254740993 = 2^53 + 1 is an
   entirely ordinary OCaml int (max_int is ~4.6e18), so [to_int (int n)] must
   equal [n].

   It does not. The C stub builds the JS value with [JS_NewInt64], which demotes
   any value that does not fit in int32 to a float64. 2^53 + 1 is not exactly
   representable as a double, so it is silently rounded down to 2^53. The
   library corrupts a value the user handed it, with no error. *)
let test_int_roundtrip_precision () =
  with_context @@ fun _runtime context ->
  let n = 9007199254740993 in
  let roundtrip = Qmljs.Value.to_int (Qmljs.Value.int context n) in
  check_int "int round trip preserves value" n roundtrip

(* BUG 1b — the same defect made fatal.

   [Value.int context max_int] then [Value.to_int] should round-trip to
   [max_int]. Because the value is stored as a double it rounds up to 2^62,
   which is then > Max_long, so [to_int] *raises* "JavaScript integer is
   outside the OCaml int range". A value produced by the library's own
   constructor cannot survive its own destructor: the API is internally
   inconsistent for large ints. *)
let test_int_max_int_roundtrip () =
  with_context @@ fun _runtime context ->
  let v = Qmljs.Value.int context max_int in
  (match Qmljs.Value.to_int v with
   | got -> check_int "max_int round trip" max_int got
   | exception exn ->
       fail
         (Printf.sprintf
            "to_int (int max_int) raised instead of round-tripping: %s"
            (Printexc.to_string exn)))

(* BUG 2 — define_data_property fails silently.

   Redefining a non-configurable property with a different value is rejected by
   the spec: [Object.defineProperty] throws a TypeError. [define_data_property]
   returns [unit], so the only contract-honouring outcomes are:
     (a) raise a structured error, or
     (b) actually change the value.
   qmljs does neither. The C stub calls [JS_DefineProperty] without
   [JS_PROP_THROW]; on rejection that returns 0 (not -1), so the stub treats it
   as success and returns unit. The value is left unchanged and the caller gets
   no signal — a "No Silent Fallback" violation. This test asserts the call
   must be observable as a failure. *)
let test_define_data_property_rejection_is_loud () =
  with_context @@ fun _runtime context ->
  let object_ = Qmljs.Object.create context in
  let key = Qmljs.Value.string context "x" in
  Qmljs.Object.define_data_property object_ ~key
    ~value:(Qmljs.Value.int context 1) ~writable:false ~enumerable:true
    ~configurable:false;
  let outcome =
    Qmljs.Error.capture (fun () ->
        Qmljs.Object.define_data_property object_ ~key
          ~value:(Qmljs.Value.int context 2) ~writable:false ~enumerable:true
          ~configurable:false)
  in
  match outcome with
  | Error _ -> () (* good: rejection surfaced as a structured error *)
  | Ok () ->
      (* If it claims success it must really have changed the value. *)
      let current =
        Qmljs.Value.to_int (Qmljs.Object.get object_ ~key)
      in
      check_int
        "define_data_property silently no-opped a rejected redefinition" 2
        current

(* BUG 3 — subclassing a native class breaks instanceof.

   [Class.constructor] returns a value marked as a constructor
   ([set_constructor true]) and installs [.prototype], so it is legal to write
   [class Sub extends Base {}] in JS. Per JS construction semantics the base
   constructor builds the instance from [new.target.prototype]; when invoked via
   [new Sub()] that is [Sub.prototype]. Therefore [new Sub() instanceof Sub]
   must be true.

   It is false: the native constructor ignores [new.target] and always builds
   the instance with the fixed [Base.prototype], so the [Sub] prototype is never
   on the chain. *)
let test_native_subclass_instanceof () =
  with_context @@ fun _runtime context ->
  let cls = Qmljs.Class.create context ~name:"Base" in
  let ctor =
    Qmljs.Class.constructor cls ~name:"Base" ~arity:0 (fun _args -> ref 0)
  in
  Qmljs.Context.set_global context ~name:"Base" ctor;
  let is_instance_of_sub =
    Qmljs.Value.to_bool
      (Qmljs.Context.eval context
         "class Sub extends Base {}; (new Sub()) instanceof Sub")
  in
  check_bool "new Sub() instanceof Sub" true is_instance_of_sub

(* BUG 4 — a real engine stack overflow is misclassified as a plain JS error.

   The boundary error model is required to distinguish a QuickJS stack overflow
   from an ordinary JS exception. A default runtime still has an engine stack
   limit, and deep non-tail recursion throws RangeError "Maximum call stack size
   exceeded" — that is, by construction, a stack overflow. [is_stack_overflow]
   must therefore return true.

   It returns false: classification is gated on the user having explicitly set
   [max_stack_size], so a genuine overflow on a default runtime is reported as a
   generic [JavaScript] error and the structured-error contract is not honoured. *)
let test_stack_overflow_classified () =
  with_context @@ fun _runtime context ->
  match
    Qmljs.Error.capture (fun () ->
        Qmljs.Context.eval context "function f(){ return 1 + f(); } f();")
  with
  | Ok _ -> fail "expected a stack overflow error"
  | Error error ->
      check_bool "deep recursion classified as stack overflow" true
        (Qmljs.Error.is_stack_overflow error)

(* BUG 5 — native class finalizers are not flushed by the deterministic API.

   [Class.create ~finalize] documents that [finalize] "runs when QuickJS
   releases an instance on the owner domain". [run_gc] makes QuickJS collect the
   unreachable instances on the owner domain — the C class finalizer fires for
   each and enqueues them on an internal deferred queue. The deterministic,
   owner-domain operations the public API offers for advancing work ([run_gc]
   and [Async.drain_jobs]) should then run the documented callbacks.

   They do not: neither [run_gc] nor [drain_jobs] drains the deferred class
   finalizer queue. The queue is only flushed as a side effect of an unrelated
   OCaml value-wrapper finalizer running (i.e. incidental OCaml GC pressure) or
   at full runtime teardown. So after collecting the instances and draining
   jobs, the user's [finalize] (which might release a file handle or other host
   resource) still has not run, and there is no deterministic API call that
   forces it. This test creates a batch of unreachable instances, collects them,
   drains jobs, and asserts at least one finalizer ran. *)
let test_class_finalizers_run_after_gc () =
  with_context @@ fun runtime context ->
  let finalized = ref 0 in
  let cls =
    Qmljs.Class.create ~finalize:(fun _ -> incr finalized) context ~name:"Res"
  in
  let ctor =
    Qmljs.Class.constructor cls ~name:"Res" ~arity:0 (fun _args -> ref 0)
  in
  Qmljs.Context.set_global context ~name:"Res" ctor;
  (* 200 instances created and immediately discarded: all unreachable. *)
  ignore (Qmljs.Context.eval context "for (let i = 0; i < 200; i++) { new Res(); } 0");
  Qmljs.Runtime.run_gc runtime;
  ignore (Qmljs.Async.drain_jobs runtime : int);
  Qmljs.Runtime.run_gc runtime;
  ignore (Qmljs.Async.drain_jobs runtime : int);
  if !finalized = 0 then
    fail
      "no class finalizer ran after run_gc + drain_jobs (only incidental OCaml \
       GC flushes the deferred queue)"


(* BUG 6 — top-level-await eval leaks QuickJS's async result wrapper.

   Synchronous [eval "40 + 2"] returns the completion value 42. The async form
   [eval ~async:true "40 + 2"] returns a promise; awaiting it should likewise
   yield the completion value 42 — the [~async] flag changes *how* the script
   runs, not *what* it evaluates to.

   Instead the promise resolves to QuickJS's internal JS_EVAL_FLAG_ASYNC wrapper
   object [{ value: 42 }], which the binding never unwraps. The caller gets an
   Object instead of the number, and [to_int] of that object is 0. (The existing
   suite hides this by only checking a global side effect.) *)
let test_async_eval_unwraps_result () =
  with_context @@ fun _runtime context ->
  let promise = Qmljs.Context.eval ~async:true context "40 + 2" in
  let result = Qmljs.Promise.await promise in
  check_int "async eval completion value" 42 (Qmljs.Value.to_int result)

let tests =
  [ "int_roundtrip_precision", test_int_roundtrip_precision
  ; "int_max_int_roundtrip", test_int_max_int_roundtrip
  ; "define_data_property_rejection_is_loud",
    test_define_data_property_rejection_is_loud
  ; "native_subclass_instanceof", test_native_subclass_instanceof
  ; "stack_overflow_classified", test_stack_overflow_classified
  ; "class_finalizers_run_after_gc", test_class_finalizers_run_after_gc
  ; "async_eval_unwraps_result", test_async_eval_unwraps_result
  ]

let () =
  Eio_main.run @@ fun _env ->
  let failures = ref [] in
  List.iter
    (fun (name, test) ->
      match test () with
      | () -> Printf.eprintf "PASS: %s\n%!" name
      | exception exn ->
          failures := (name, Printexc.to_string exn) :: !failures;
          Printf.eprintf "RED: %s: %s\n%!" name (Printexc.to_string exn))
    tests;
  match List.rev !failures with
  | [] -> ()
  | failures ->
      Printf.eprintf "%d/%d bug-hunt specs are red\n%!" (List.length failures)
        (List.length tests);
      exit 1
