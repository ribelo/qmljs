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

(* BUG 7 — the int-precision fix made Value.int produce a BigInt, so to_float
   is no longer total.

   [Value.to_float : t -> float] is presented as a total numeric conversion, and
   a value produced by the library's own [Value.int] constructor must be
   convertible by it. The "preserve unsafe ocaml ints" fix now builds a JS
   BigInt whenever |n| > 2^53, and [to_float] (JS_ToFloat64) cannot implicitly
   coerce a BigInt, so this raises "cannot convert BigInt to number" instead of
   returning the nearest double. Number(9007199254740993n) === 9007199254740992,
   so [to_float] must return 9007199254740992.0. *)
let test_int_to_float_is_total () =
  with_context @@ fun _runtime context ->
  let v = Qmljs.Value.int context 9007199254740993 in
  match Qmljs.Value.to_float v with
  | f ->
      if f <> 9007199254740992.0 then
        fail
          (Printf.sprintf "to_float (int 2^53+1): expected 9007199254740992.0, got %.1f"
             f)
  | exception exn ->
      fail
        (Printf.sprintf "to_float (int 2^53+1) raised instead of converting: %s"
           (Printexc.to_string exn))

(* BUG 8 — the number decoder rejects values the number encoder produced.

   [Value.as_float] is the decoder for JS numbers, and [Value.int] is a number
   encoder. [as_float (int n)] must round-trip for any [n]. Because the fix
   makes [int] build a BigInt for |n| > 2^53, and [as_float] only accepts the
   [Int]/[Float] kinds, [as_float (int 2^53+1)] returns Error "expected number".
   Encoder and decoder for the same logical type must agree. *)
let test_as_float_accepts_int_encoded_value () =
  with_context @@ fun _runtime context ->
  let v = Qmljs.Value.int context 9007199254740993 in
  match Qmljs.Value.as_float v with
  | Ok f ->
      if f <> 9007199254740992.0 then
        fail (Printf.sprintf "as_float (int 2^53+1): unexpected %.1f" f)
  | Error e ->
      fail
        ("as_float rejected a value produced by Value.int: "
        ^ Qmljs.Error.message e)

(* BUG 9 — a value from Value.int cannot be used in ordinary JS arithmetic.

   [Value.int] is the JS-number constructor; a number handed to JS must take
   part in normal arithmetic. After the fix, [int] builds a BigInt for large
   magnitudes, and JS forbids mixing BigInt with Number, so [x + 1] throws
   "cannot convert BigInt to number". Adding 1 to an integer must not throw, and
   the result must still be a number (not a BigInt). *)
let test_int_value_arithmetic_in_js () =
  with_context @@ fun _runtime context ->
  Qmljs.Context.set_global context ~name:"x"
    (Qmljs.Value.int context 9007199254740993);
  match Qmljs.Error.capture (fun () -> Qmljs.Context.eval context "x + 1") with
  | Error e ->
      fail ("x + 1 raised for a Value.int operand: " ^ Qmljs.Error.message e)
  | Ok result ->
      (match Qmljs.Value.kind result with
       | Qmljs.Value.Bigint ->
           fail "x + 1 produced a BigInt; Value.int should yield a JS number"
       | _ -> ())

(* BUG 10 — the stack-overflow fix misclassifies overflows by stack shape.

   The "classify real stack overflows" fix detects an overflow by scanning the
   stack trace for a frame repeated >= 4 times *consecutively*. A genuine engine
   stack overflow is identified by QuickJS's RangeError "Maximum call stack size
   exceeded" — the recursion shape is irrelevant. Mutual recursion produces an
   alternating trace (a, b, a, b, ...), whose longest consecutive run is 1, so it
   is left classified as a plain [JavaScript] error even though it is the same
   overflow that direct recursion (which IS classified) produces. Both forms
   must classify as [Stack_overflow]. *)
let test_mutual_recursion_stack_overflow () =
  with_context @@ fun _runtime context ->
  match
    Qmljs.Error.capture (fun () ->
        Qmljs.Context.eval context
          "function a(){ return b() + 1; } function b(){ return a() + 1; } a();")
  with
  | Ok _ -> fail "expected a stack overflow error"
  | Error error ->
      check_bool "mutual recursion classified as stack overflow" true
        (Qmljs.Error.is_stack_overflow error)

(* BUG 11 — as_int silently wraps out-of-range BigInts to a bogus value.

   The fix that let [as_int] accept the [Bigint] kind routes it through [to_int],
   whose C stub uses JS_ToInt64Ext — that reduces a BigInt modulo 2^64 *before*
   the OCaml-int range check. So 2n**64n reduces to exactly 0 and passes the
   check, and [as_int (2n**64n)] returns [Ok 0] instead of failing. (2n**63n,
   which reduces to int64 min, *is* rejected, so the behaviour is also
   inconsistent.) A BigInt that does not fit in an OCaml int must fail loudly
   per the No-Silent-Fallback contract, not silently decode to a wrong value. *)
let test_as_int_rejects_out_of_range_bigint () =
  with_context @@ fun _runtime context ->
  let v = Qmljs.Context.eval context "2n ** 64n" in
  match Qmljs.Value.as_int v with
  | Error _ -> () (* good: out-of-range BigInt rejected loudly *)
  | Ok i ->
      fail
        (Printf.sprintf
           "as_int(2n**64n) silently returned %d for a value far outside the \
            OCaml int range" i)

(* BUG 12 — the int->BigInt change breaks JSON serialization of integer data.

   [of_assoc]/[of_record] with the [Value.int] encoder is the documented way to
   build a JS object from OCaml data, and [Json.stringify] is the documented
   serializer; "record of ints -> JS object -> JSON text" must work for ordinary
   integers. Because [Value.int] now produces a JS BigInt for |n| > 2^53, and
   BigInt is not JSON-serializable, [Json.stringify] raises "BigInt are forbidden
   in JSON.stringify". This pins a distinct surface (encoders + JSON) and also
   constrains the fix: merely teaching to_float about BigInt would leave this
   red. *)
let test_json_stringify_int_record () =
  with_context @@ fun _runtime context ->
  let obj =
    Qmljs.Value.of_assoc Qmljs.Value.int context [ "count", 9007199254740993 ]
  in
  match Qmljs.Error.capture (fun () -> Qmljs.Json.stringify obj) with
  | Error e ->
      fail ("Json.stringify of an int-valued field raised: " ^ Qmljs.Error.message e)
  | Ok json ->
      (* The exact textual form is engine-dependent; just require the integer
         value to be present and serialized as a JSON number (no BigInt "n"). *)
      if not (String.length json > 0) then fail "empty JSON";
      check_bool "json contains the integer" true
        (let contains s sub =
           let n = String.length s and m = String.length sub in
           let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
           m = 0 || go 0
         in
         contains json "9007199254740992" || contains json "9007199254740993")

let tests =
  [ "int_roundtrip_precision", test_int_roundtrip_precision
  ; "int_max_int_roundtrip", test_int_max_int_roundtrip
  ; "define_data_property_rejection_is_loud",
    test_define_data_property_rejection_is_loud
  ; "native_subclass_instanceof", test_native_subclass_instanceof
  ; "stack_overflow_classified", test_stack_overflow_classified
  ; "class_finalizers_run_after_gc", test_class_finalizers_run_after_gc
  ; "async_eval_unwraps_result", test_async_eval_unwraps_result
  ; "int_to_float_is_total", test_int_to_float_is_total
  ; "as_float_accepts_int_encoded_value", test_as_float_accepts_int_encoded_value
  ; "int_value_arithmetic_in_js", test_int_value_arithmetic_in_js
  ; "mutual_recursion_stack_overflow", test_mutual_recursion_stack_overflow
  ; "as_int_rejects_out_of_range_bigint", test_as_int_rejects_out_of_range_bigint
  ; "json_stringify_int_record", test_json_stringify_int_record
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
