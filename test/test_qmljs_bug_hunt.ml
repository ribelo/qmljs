(* Bug-hunt suite / living regression spec.

   Each test asserts a behaviour qmljs must have, with a comment explaining why
   it is the correct contract. Tests that once pinned a defect now also serve as
   regression guards for the fix that landed.

   The integer specs encode the agreed "Option B" contract for [Value.int]:
   OCaml's native int is 63-bit, a JS Number is an IEEE-754 double (exact only up
   to 2^53 - 1), and JS BigInt is a separate type that does not interoperate with
   Number. There is no representation that is simultaneously exact, a Number, and
   referentially transparent for OCaml ints in (2^53, 2^62). So [Value.int]:
     - accepts integers with |n| <= MAX_SAFE_INTEGER and delivers a true JS
       Number (exact round trip, normal arithmetic / JSON / to_float), and
     - fails loudly for anything larger, pointing the caller at [Bigint].

   Run with: nix develop .#oxcaml -c dune exec test/test_qmljs_bug_hunt.exe *)

let fail message = raise (Failure message)

let check_int label expected actual =
  if actual <> expected then
    fail (Printf.sprintf "%s: expected %d, got %d" label expected actual)

let check_bool label expected actual =
  if actual <> expected then
    fail (Printf.sprintf "%s: expected %b, got %b" label expected actual)

let check_string_eq label expected actual =
  if not (String.equal expected actual) then
    fail (Printf.sprintf "%s: expected %S, got %S" label expected actual)

let expect_raises label thunk =
  match thunk () with
  | _value ->
      fail (label ^ ": expected an exception, but a value was returned")
  | exception _ -> ()

let with_context fn =
  let runtime = Qmljs.Runtime.create () in
  let context = Qmljs.Context.create runtime in
  fn runtime context

let max_safe_integer = 9007199254740991 (* 2^53 - 1 = Number.MAX_SAFE_INTEGER *)

(* INT 1 — exact round trip across the whole safe-integer range.

   [Value.int] / [Value.to_int] are an encoder/decoder pair, so within the range
   [Value.int] accepts they must be the identity, including at the boundaries. *)
let test_int_safe_range_roundtrips () =
  with_context @@ fun _runtime context ->
  List.iter
    (fun n ->
      check_int
        (Printf.sprintf "round trip %d" n)
        n
        (Qmljs.Value.to_int (Qmljs.Value.int context n)))
    [ 0; 1; -1; 42; -42; 2147483648; max_safe_integer; -max_safe_integer ]

(* INT 2 — a Value.int is a genuine JS Number.

   The whole point of contract B is that an accepted [Value.int] behaves like a
   Number in JS: [typeof] is "number", it takes part in ordinary arithmetic
   (never a BigInt-mixing TypeError), [to_float] is total, and it serializes
   through JSON. This guards against regressing to the BigInt representation. *)
let test_int_is_a_js_number () =
  with_context @@ fun _runtime context ->
  let v = Qmljs.Value.int context max_safe_integer in
  Qmljs.Context.set_global context ~name:"x" v;
  check_string_eq "typeof is number" "number"
    (Qmljs.Value.to_string (Qmljs.Context.eval context "typeof x"));
  (* arithmetic must not throw and must stay a number *)
  (match Qmljs.Error.capture (fun () -> Qmljs.Context.eval context "x - 1") with
   | Error e -> fail ("arithmetic on a Value.int raised: " ^ Qmljs.Error.message e)
   | Ok result ->
       (match Qmljs.Value.kind result with
        | Qmljs.Value.Bigint -> fail "x - 1 produced a BigInt, not a Number"
        | _ -> check_int "x - 1" (max_safe_integer - 1) (Qmljs.Value.to_int result)));
  (* to_float is total for an accepted int *)
  (match Qmljs.Value.to_float v with
   | f -> check_bool "to_float finite" true (Float.is_finite f)
   | exception exn ->
       fail ("to_float of a Value.int raised: " ^ Printexc.to_string exn));
  (* JSON serialization of an int-valued field works *)
  let obj = Qmljs.Value.of_assoc Qmljs.Value.int context [ "count", 123456789 ] in
  (match Qmljs.Error.capture (fun () -> Qmljs.Json.stringify obj) with
   | Error e -> fail ("Json.stringify of int field raised: " ^ Qmljs.Error.message e)
   | Ok json ->
       check_bool "json has the integer" true
         (let contains s sub =
            let n = String.length s and m = String.length sub in
            let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
            m = 0 || go 0
          in
          contains json "123456789"))

(* INT 3 — out-of-safe-range magnitudes are rejected loudly.

   Per the No-Silent-Fallback rule, [Value.int] must not silently round, smuggle
   a BigInt, or fake exactness for values a JS Number cannot hold. It must raise.
   These cover just-past-the-boundary (2^53), max_int, and the negative side. *)
let test_int_rejects_out_of_safe_range () =
  with_context @@ fun _runtime context ->
  expect_raises "int (2^53)" (fun () ->
      Qmljs.Value.int context (max_safe_integer + 1));
  expect_raises "int max_int" (fun () -> Qmljs.Value.int context max_int);
  expect_raises "int (-2^53)" (fun () ->
      Qmljs.Value.int context (-(max_safe_integer + 1)));
  expect_raises "int min_int" (fun () -> Qmljs.Value.int context min_int)

(* BUG 2 — define_data_property fails silently.

   Redefining a non-configurable property with a different value is rejected by
   the spec: [Object.defineProperty] throws a TypeError. [define_data_property]
   returns [unit], so the only contract-honouring outcomes are (a) raise a
   structured error, or (b) actually change the value. The original stub called
   [JS_DefineProperty] without [JS_PROP_THROW]; on rejection that returns 0 (not
   -1), so it was treated as success and the value was left unchanged. *)
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
  | Error _ -> ()
  | Ok () ->
      let current = Qmljs.Value.to_int (Qmljs.Object.get object_ ~key) in
      check_int
        "define_data_property silently no-opped a rejected redefinition" 2
        current

(* BUG 3 — subclassing a native class must preserve instanceof.

   [Class.constructor] is marked as a constructor and installs [.prototype], so
   [class Sub extends Base {}] is legal. Per JS construction semantics the base
   constructor builds the instance from [new.target.prototype]; under [new Sub()]
   that is [Sub.prototype], so [new Sub() instanceof Sub] must be true. The
   original constructor ignored [new.target] and always used [Base.prototype]. *)
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

(* BUG 4 — a real engine stack overflow must be distinguished from a plain JS
   exception even on a default runtime. Deep direct recursion throws RangeError
   "Maximum call stack size exceeded"; [is_stack_overflow] must be true. *)
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

(* BUG 10 — stack-overflow classification must not depend on the textual shape
   of the trace. Mutual recursion (a, b, a, b, ...) is the same engine overflow
   as direct recursion and must also classify as [Stack_overflow], even though
   its trace has no frame repeated consecutively. *)
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

(* BUG 5 — native class finalizers must be flushed by the deterministic API.

   [Class.create ~finalize] documents that [finalize] runs when QuickJS releases
   an instance on the owner domain. [run_gc] collects unreachable instances on
   the owner domain; the deterministic operations the API offers ([run_gc] and
   [Async.drain_jobs]) must then run the documented callbacks rather than relying
   on incidental OCaml GC pressure to flush the deferred queue. *)
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
  ignore (Qmljs.Context.eval context "for (let i = 0; i < 200; i++) { new Res(); } 0");
  Qmljs.Runtime.run_gc runtime;
  ignore (Qmljs.Async.drain_jobs runtime : int);
  Qmljs.Runtime.run_gc runtime;
  ignore (Qmljs.Async.drain_jobs runtime : int);
  if !finalized = 0 then
    fail "no class finalizer ran after run_gc + drain_jobs"

(* BUG 6 — top-level-await eval must yield the completion value, not QuickJS's
   internal JS_EVAL_FLAG_ASYNC wrapper [{ value: ... }]. Awaiting the result of
   [eval ~async:true "40 + 2"] must give 42. *)
let test_async_eval_unwraps_result () =
  with_context @@ fun _runtime context ->
  let promise = Qmljs.Context.eval ~async:true context "40 + 2" in
  let result = Qmljs.Promise.await promise in
  check_int "async eval completion value" 42 (Qmljs.Value.to_int result)

(* Async-eval unwrapping must not depend on user-mutable Promise.prototype.then.
   The host maps QuickJS's [{ value: ... }] async-eval wrapper to the completion
   value via a pristine then captured at context creation, so tampering with
   Promise.prototype.then cannot change what [eval ~async:true] returns. *)
let test_async_eval_unwrap_ignores_tampered_then () =
  with_context @@ fun _runtime context ->
  ignore
    (Qmljs.Context.eval context
       "Promise.prototype.then = function () { return Promise.resolve({ value: \
        999 }); };"
      : Qmljs.Value.t);
  let promise = Qmljs.Context.eval ~async:true context "40 + 2" in
  let result = Qmljs.Promise.await promise in
  check_int "async eval ignores tampered then" 42 (Qmljs.Value.to_int result)

(* Module evaluation must not return a namespace while top-level await is still
   pending. A module that awaits a promise that never settles cannot complete;
   with no driver to make progress, [Module.eval] must fail loudly with a
   structured resource-limit error rather than silently exposing a
   half-evaluated module. *)
let test_module_eval_pending_tla_is_not_silent_success () =
  with_context @@ fun _runtime context ->
  match
    Qmljs.Error.capture (fun () ->
        Qmljs.Module.eval context
          "await new Promise(() => {}); export default 1;")
  with
  | Ok _ ->
      fail "Module.eval returned a namespace while top-level await was pending"
  | Error error ->
      check_bool "pending module evaluation is a resource-limit failure" true
        (Qmljs.Error.is_resource_limit error)

(* BUG 11 — as_int must reject (not silently wrap) BigInts outside the OCaml int
   range. [JS_ToInt64Ext] reduces modulo 2^64, so 2n**64n would wrap to 0; the
   decoder must fail loudly instead of returning [Ok 0]. *)
let test_as_int_rejects_out_of_range_bigint () =
  with_context @@ fun _runtime context ->
  let v = Qmljs.Context.eval context "2n ** 64n" in
  match Qmljs.Value.as_int v with
  | Error _ -> ()
  | Ok i ->
      fail
        (Printf.sprintf
           "as_int(2n**64n) silently returned %d for an out-of-range BigInt" i)

(* own_keys returns all own property keys by default.

   [own_keys] mirrors JS [Reflect.ownKeys]: with no filter it returns every own
   key regardless of enumerability. The optional flags [strings], [symbols], and
   [enumerable_only] narrow the result; [enumerable_only] defaults to [false] so
   non-enumerable own properties are included unless the caller opts out. *)
let test_own_keys_default_includes_all_keys () =
  with_context @@ fun _runtime context ->
  let obj = Qmljs.Object.create context in
  Qmljs.Object.define_data_property obj
    ~key:(Qmljs.Value.string context "visible")
    ~value:(Qmljs.Value.int context 1)
    ~writable:true ~enumerable:true ~configurable:true;
  Qmljs.Object.define_data_property obj
    ~key:(Qmljs.Value.string context "hidden")
    ~value:(Qmljs.Value.int context 2)
    ~writable:true ~enumerable:false ~configurable:true;
  let all = Qmljs.Object.own_keys obj in
  check_int "own_keys default includes non-enumerable key" 2 (List.length all)

(* entries and values share the [own_keys] default via [own_key_flags]:
   both include non-enumerable entries/values unless [enumerable_only] is
   set explicitly. *)
let test_entries_default_includes_all () =
  with_context @@ fun _runtime context ->
  let obj = Qmljs.Object.create context in
  Qmljs.Object.define_data_property obj
    ~key:(Qmljs.Value.string context "a")
    ~value:(Qmljs.Value.int context 10)
    ~writable:true ~enumerable:true ~configurable:true;
  Qmljs.Object.define_data_property obj
    ~key:(Qmljs.Value.string context "_internal")
    ~value:(Qmljs.Value.int context 20)
    ~writable:true ~enumerable:false ~configurable:true;
  let entries = Qmljs.Object.entries obj in
  check_int "entries default includes non-enumerable entry" 2 (List.length entries)

(* Value.string requires valid UTF-8 and fails loudly otherwise.

   OCaml strings are arbitrary byte sequences, but a JS string is Unicode text.
   QuickJS's [JS_NewStringLen] silently rewrites invalid UTF-8 to U+FFFD (the
   single byte 0x9c becomes the 3-byte sequence 0xef 0xbf 0xbd), which would
   corrupt the data. Per No-Silent-Fallback, [Value.string] rejects invalid
   UTF-8 instead; raw bytes belong in an ArrayBuffer or typed array. *)
let test_string_rejects_non_utf8_bytes () =
  with_context @@ fun _runtime context ->
  expect_raises "string \\x9c" (fun () ->
      Qmljs.Value.string context "\x9c")

(* Valid UTF-8 still round-trips losslessly, including multi-byte text. *)
let test_string_roundtrips_valid_utf8 () =
  with_context @@ fun _runtime context ->
  List.iter
    (fun s ->
      check_string_eq (Printf.sprintf "round trip %S" s) s
        (Qmljs.Value.to_string (Qmljs.Value.string context s)))
    [ ""; "ascii"; "\xc3\xa9" (* é *); "\xe2\x9c\x93" (* ✓ *); "\xf0\x9f\x98\x80" (* 😀 *) ]

(* BUG 16 — Object.has must check own properties only.

   In every language with an object-shaped container, [has key] means "this
   object itself owns the key" — Python [key in dict], Rust [HashMap::contains_key],
   Java [Map.containsKey] — none walk the prototype chain. JS distinguishes
   [Object.hasOwnProperty] (own) from the [in] operator (chain). The qmljs
   name [Object.has] matches the universal convention, so it must implement
   own-property semantics; calling [JS_HasProperty] (the [in] operator) makes
   it silently include inherited properties, which is a contract violation. *)
let test_has_checks_own_property_not_prototype () =
  with_context @@ fun _runtime context ->
  let proto = Qmljs.Object.create context in
  Qmljs.Object.set proto ~name:"inherited" (Qmljs.Value.int context 1);
  let obj = Qmljs.Object.create ~prototype:(Qmljs.Value.object_ proto) context in
  Qmljs.Object.set obj ~name:"own" (Qmljs.Value.int context 2);
  check_bool "has own property" true
    (Qmljs.Object.has obj ~key:(Qmljs.Value.string context "own"));
  check_bool "has must not see prototype property" false
    (Qmljs.Object.has obj ~key:(Qmljs.Value.string context "inherited"))

(* BUG 17 — Object.has_atom must also check own properties only.

   Same contract as BUG 16, exercised through the atom-key path. *)
let test_has_atom_checks_own_property_not_prototype () =
  with_context @@ fun _runtime context ->
  let proto = Qmljs.Object.create context in
  let inherited_atom = Qmljs.Atom.of_string context "inherited" in
  Qmljs.Object.set_atom proto ~key:inherited_atom (Qmljs.Value.int context 1);
  let obj = Qmljs.Object.create ~prototype:(Qmljs.Value.object_ proto) context in
  let own_atom = Qmljs.Atom.of_string context "own" in
  Qmljs.Object.set_atom obj ~key:own_atom (Qmljs.Value.int context 2);
  check_bool "has_atom own property" true
    (Qmljs.Object.has_atom obj ~key:own_atom);
  check_bool "has_atom must not see prototype property" false
    (Qmljs.Object.has_atom obj ~key:inherited_atom)

let tests =
  [ "int_safe_range_roundtrips", test_int_safe_range_roundtrips
  ; "int_is_a_js_number", test_int_is_a_js_number
  ; "int_rejects_out_of_safe_range", test_int_rejects_out_of_safe_range
  ; "define_data_property_rejection_is_loud",
    test_define_data_property_rejection_is_loud
  ; "native_subclass_instanceof", test_native_subclass_instanceof
  ; "stack_overflow_classified", test_stack_overflow_classified
  ; "mutual_recursion_stack_overflow", test_mutual_recursion_stack_overflow
  ; "class_finalizers_run_after_gc", test_class_finalizers_run_after_gc
  ; "async_eval_unwraps_result", test_async_eval_unwraps_result
  ; "async_eval_unwrap_ignores_tampered_then",
    test_async_eval_unwrap_ignores_tampered_then
  ; "module_eval_pending_tla_is_not_silent_success",
    test_module_eval_pending_tla_is_not_silent_success
  ; "as_int_rejects_out_of_range_bigint", test_as_int_rejects_out_of_range_bigint
  ; "own_keys_default_includes_all_keys", test_own_keys_default_includes_all_keys
  ; "entries_default_includes_all", test_entries_default_includes_all
  ; "string_rejects_non_utf8_bytes", test_string_rejects_non_utf8_bytes
  ; "string_roundtrips_valid_utf8", test_string_roundtrips_valid_utf8
  ; "has_checks_own_property_not_prototype", test_has_checks_own_property_not_prototype
  ; "has_atom_checks_own_property_not_prototype", test_has_atom_checks_own_property_not_prototype
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
