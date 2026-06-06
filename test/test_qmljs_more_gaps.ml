let fail message = raise (Failure message)

let check_bool label expected actual =
  if expected <> actual then
    fail (Printf.sprintf "%s: expected %b, got %b" label expected actual)

let check_int label expected actual =
  if expected <> actual then
    fail (Printf.sprintf "%s: expected %d, got %d" label expected actual)

let check_string label expected actual =
  if not (String.equal expected actual) then
    fail (Printf.sprintf "%s: expected %S, got %S" label expected actual)

let check_bytes label expected actual =
  if not (Bytes.equal expected actual) then
    fail
      (Printf.sprintf "%s: expected %S, got %S" label
         (Bytes.to_string expected) (Bytes.to_string actual))

let check_kind label expected actual =
  if expected <> actual then fail (label ^ ": unexpected error kind")

let expect_structured_error label fn assert_error =
  match Qmljs.Error.capture fn with
  | Ok _ -> fail (label ^ ": expected structured error")
  | Error error -> assert_error error

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

let test_value_type_inspection_api () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let cases =
    [ "undefined", Qmljs.Value.undefined context, Qmljs.Value.Undefined
    ; "null", Qmljs.Value.null context, Qmljs.Value.Null
    ; "bool", Qmljs.Value.bool context true, Qmljs.Value.Bool
    ; "int", Qmljs.Value.int context 7, Qmljs.Value.Int
    ; "float", Qmljs.Value.float context 1.5, Qmljs.Value.Float
    ; "string", Qmljs.Value.string context "qmljs", Qmljs.Value.String
    ; "symbol", Qmljs.Symbol.global context "tag", Qmljs.Value.Symbol
    ; "bigint", Qmljs.Bigint.of_string context "42", Qmljs.Value.Bigint
    ; "object", Qmljs.Object.create context, Qmljs.Value.Object
    ; "array", Qmljs.Array.of_list context [], Qmljs.Value.Array
    ; ( "function"
      , Qmljs.Function.make context ~name:"fn" ~arity:0 (fun _args ->
            Qmljs.Value.undefined context)
      , Qmljs.Value.Function )
    ; ( "promise"
      , (let promise, _resolve, _reject = Qmljs.Promise.create context in
         promise)
      , Qmljs.Value.Promise )
    ; ( "typed array"
      , Qmljs.Typed_array.Uint8Array.of_bytes_copy context (Bytes.of_string "")
      , Qmljs.Value.Typed_array )
    ]
  in
  List.iter
    (fun (label, value, expected) ->
      if Qmljs.Value.kind value <> expected then
        fail (label ^ ": unexpected value kind"))
    cases;
  check_bool "null predicate" true
    (Qmljs.Value.is_null (Qmljs.Value.null context));
  check_bool "undefined predicate" true
    (Qmljs.Value.is_undefined (Qmljs.Value.undefined context));
  check_bool "function predicate" true
    (Qmljs.Value.is_function
       (Qmljs.Function.make context ~name:"fn2" ~arity:0 (fun _args ->
            Qmljs.Value.undefined context)));
  check_bool "array predicate" true
    (Qmljs.Value.is_array (Qmljs.Array.of_list context []))

let test_value_equality_api () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let nan_left = Qmljs.Context.eval context "NaN" in
  let nan_right = Qmljs.Context.eval context "NaN" in
  let minus_zero = Qmljs.Context.eval context "-0" in
  let plus_zero = Qmljs.Context.eval context "0" in
  let object_ = Qmljs.Object.create context in
  check_bool "NaN strict equality" false
    (Qmljs.Value.strict_equal nan_left nan_right);
  check_bool "NaN same-value equality" true
    (Qmljs.Value.same_value nan_left nan_right);
  check_bool "-0/+0 same-value equality" false
    (Qmljs.Value.same_value minus_zero plus_zero);
  check_bool "-0/+0 same-value-zero equality" true
    (Qmljs.Value.same_value_zero minus_zero plus_zero);
  check_bool "object identity strict equality" true
    (Qmljs.Value.strict_equal object_ object_)

let test_value_conversion_helpers_api () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let check_decode label expected = function
    | Ok actual when actual = expected -> ()
    | Ok _ -> fail (label ^ ": decoded unexpected value")
    | Error error -> fail (label ^ ": " ^ Qmljs.Error.message error)
  in
  Qmljs.Value.of_list Qmljs.Value.int context [ 1; 2; 3 ]
  |> Qmljs.Value.to_list Qmljs.Value.as_int
  |> check_decode "list round trip" [ 1; 2; 3 ];
  Qmljs.Value.of_array Qmljs.Value.string context [| "a"; "b" |]
  |> Qmljs.Value.to_array Qmljs.Value.as_string
  |> check_decode "array round trip" [| "a"; "b" |];
  Qmljs.Value.of_option Qmljs.Value.int context (Some 42)
  |> Qmljs.Value.to_option Qmljs.Value.as_int
  |> check_decode "some option round trip" (Some 42);
  Qmljs.Value.of_option Qmljs.Value.int context None
  |> Qmljs.Value.to_option Qmljs.Value.as_int
  |> check_decode "none option round trip" None;
  Qmljs.Value.undefined context
  |> Qmljs.Value.to_option Qmljs.Value.as_int
  |> check_decode "undefined option round trip" None;
  Qmljs.Value.of_result ~ok:Qmljs.Value.int ~error:Qmljs.Value.string context
    (Ok 7)
  |> Qmljs.Value.to_result ~ok:Qmljs.Value.as_int
       ~error:Qmljs.Value.as_string
  |> check_decode "ok result round trip" (Ok 7);
  Qmljs.Value.of_result ~ok:Qmljs.Value.int ~error:Qmljs.Value.string context
    (Error "nope")
  |> Qmljs.Value.to_result ~ok:Qmljs.Value.as_int
       ~error:Qmljs.Value.as_string
  |> check_decode "error result round trip" (Error "nope");
  let record =
    Qmljs.Value.of_record Qmljs.Value.string context
      [ "name", "qmljs"; "kind", "binding" ]
  in
  Qmljs.Context.set_global context ~name:"record" record;
  check_string "record is a JS object" "qmljs:binding"
    (Qmljs.Value.to_string
       (Qmljs.Context.eval context "record.name + ':' + record.kind"));
  record
  |> Qmljs.Value.to_record Qmljs.Value.as_string
  |> check_decode "record round trip" [ "name", "qmljs"; "kind", "binding" ];
  (match Qmljs.Value.as_object record with
   | Ok object_ ->
       check_bool "object strict conversion preserves identity" true
         (Qmljs.Value.strict_equal object_ record)
   | Error error -> fail ("object conversion failed: " ^ Qmljs.Error.message error));
  let array_value = Qmljs.Value.of_list Qmljs.Value.int context [ 4; 5 ] in
  (match Qmljs.Value.as_array array_value with
   | Ok array ->
       check_bool "array strict conversion preserves identity" true
         (Qmljs.Value.strict_equal array array_value)
   | Error error -> fail ("array conversion failed: " ^ Qmljs.Error.message error));
  let bigint = Qmljs.Bigint.of_string context "9007199254740993" in
  (match Qmljs.Value.as_bigint (Qmljs.Value.bigint bigint) with
   | Ok decoded ->
       check_string "bigint strict conversion"
         (Qmljs.Bigint.to_string bigint)
         (Qmljs.Bigint.to_string decoded)
   | Error error -> fail ("bigint conversion failed: " ^ Qmljs.Error.message error));
  let typed_array =
    Qmljs.Typed_array.Uint8Array.of_bytes_copy context (Bytes.of_string "abc")
  in
  (match Qmljs.Value.as_typed_array (Qmljs.Value.typed_array typed_array) with
   | Ok decoded ->
       check_bool "typed array strict conversion preserves identity" true
         (Qmljs.Value.strict_equal decoded typed_array)
   | Error error ->
       fail ("typed array conversion failed: " ^ Qmljs.Error.message error));
  (match
     Qmljs.Value.to_list Qmljs.Value.as_int
       (Qmljs.Value.string context "not an array")
   with
   | Ok _ -> fail "non-array decoded as list"
   | Error error ->
       check_kind "conversion error kind" Qmljs.Error.JavaScript
         (Qmljs.Error.kind error);
       check_string "conversion error message" "conversion error: expected array"
         (Qmljs.Error.message error));
  (match
     Qmljs.Value.to_list Qmljs.Value.as_int
       (Qmljs.Value.of_list Qmljs.Value.string context [ "1" ])
   with
   | Ok _ -> fail "string element decoded as int"
   | Error error ->
       check_string "strict conversion message"
         "conversion error: expected int"
         (Qmljs.Error.message error));
  let throwing_record =
    Qmljs.Context.eval context
      {|({ get bad() { throw new Error("bad getter"); } })|}
  in
  (match Qmljs.Value.to_record Qmljs.Value.as_int throwing_record with
   | Ok _ -> fail "throwing getter decoded as record"
   | Error error ->
       check_kind "throwing getter conversion kind" Qmljs.Error.JavaScript
         (Qmljs.Error.kind error);
       check_string "throwing getter conversion message" "bad getter"
         (Qmljs.Error.message error))

let test_json_codec_api () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let parsed = Qmljs.Json.parse context {|{"answer":42}|} in
  let answer =
    match Qmljs.Object.get_string parsed "answer" with
    | Some value -> Qmljs.Value.to_int value
    | None -> fail "json parse did not expose answer"
  in
  check_int "json answer" 42 answer;
  let encoded = Qmljs.Json.stringify parsed in
  let roundtrip = Qmljs.Json.parse context encoded in
  let roundtrip_answer =
    match Qmljs.Object.get_string roundtrip "answer" with
    | Some value -> Qmljs.Value.to_int value
    | None -> fail "json stringify did not round-trip answer"
  in
  check_int "json roundtrip answer" 42 roundtrip_answer;
  (match Qmljs.Json.of_string context "{not json}" with
   | Error error ->
       check_kind "invalid json kind" Qmljs.Error.JavaScript
         (Qmljs.Error.kind error)
   | Ok _ -> fail "invalid json parsed successfully")

let test_object_string_key_convenience_api () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let object_ = Qmljs.Object.create context in
  Qmljs.Object.set_string object_ "answer" (Qmljs.Value.int context 42);
  (match Qmljs.Object.get_string object_ "answer" with
   | Some value -> check_int "object string get" 42 (Qmljs.Value.to_int value)
   | None -> fail "object string get returned None");
  check_bool "object keys include string property" true
    (List.mem "answer" (Qmljs.Object.keys object_));
  check_bool "object string delete" true
    (Qmljs.Object.delete_string object_ "answer");
  check_bool "object string get after delete" true
    (Option.is_none (Qmljs.Object.get_string object_ "answer"))

let test_atom_property_key_api () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  let answer =
    Qmljs.Context.with_ runtime @@ fun context ->
    Qmljs.Atom.of_string context "answer"
  in
  Qmljs.Context.with_ runtime @@ fun context ->
  let object_ = Qmljs.Object.create context in
  check_string "atom survives source context close" "answer"
    (Qmljs.Atom.to_string context answer);
  check_bool "equal atoms" true
    (Qmljs.Atom.equal answer (Qmljs.Atom.of_string context "answer"));
  check_bool "different atoms" false
    (Qmljs.Atom.equal answer (Qmljs.Atom.length context));
  check_string "atom to value" "answer"
    (Qmljs.Value.to_string (Qmljs.Atom.to_value context answer));
  Qmljs.Object.set_atom object_ ~key:answer (Qmljs.Value.int context 42);
  check_bool "object has atom key" true
    (Qmljs.Object.has_atom object_ ~key:answer);
  check_int "object get atom key" 42
    (Qmljs.Value.to_int (Qmljs.Object.get_atom object_ ~key:answer));
  let hidden = Qmljs.Atom.of_string context "hidden" in
  Qmljs.Object.define_data_property_atom object_ ~key:hidden
    ~value:(Qmljs.Value.int context 11)
    ~writable:false ~enumerable:false ~configurable:false;
  (match Qmljs.Object.get_own_property_descriptor_atom object_ ~key:hidden with
   | Some descriptor ->
       (match descriptor.value with
        | Some value -> check_int "atom descriptor value" 11 (Qmljs.Value.to_int value)
        | None -> fail "atom data descriptor missing value");
       check_bool "atom descriptor writable" true
         (descriptor.writable = Some false);
       check_bool "atom descriptor enumerable" false descriptor.enumerable;
       check_bool "atom descriptor configurable" false descriptor.configurable
   | None -> fail "missing atom data descriptor");
  let backing = ref 7 in
  let getter =
    Qmljs.Function.make context ~name:"get_then" ~arity:0 (fun _args ->
        Qmljs.Value.int context !backing)
  in
  let then_key = Qmljs.Atom.then_ context in
  Qmljs.Object.define_accessor_property_atom object_ ~key:then_key
    ~get:getter ~enumerable:true ~configurable:true ();
  check_int "atom accessor get" 7
    (Qmljs.Value.to_int (Qmljs.Object.get_atom object_ ~key:then_key));
  check_string "length atom" "length"
    (Qmljs.Atom.to_string context (Qmljs.Atom.length context));
  check_string "prototype atom" "prototype"
    (Qmljs.Atom.to_string context (Qmljs.Atom.prototype context));
  check_string "constructor atom" "constructor"
    (Qmljs.Atom.to_string context (Qmljs.Atom.constructor context));
  check_string "catch atom" "catch"
    (Qmljs.Atom.to_string context (Qmljs.Atom.catch context));
  check_string "default atom" "default"
    (Qmljs.Atom.to_string context (Qmljs.Atom.default context));
  check_string "integer atom" "7"
    (Qmljs.Atom.to_string context (Qmljs.Atom.of_int context 7));
  check_bool "object delete atom key" true
    (Qmljs.Object.delete_atom object_ ~key:answer);
  check_bool "object atom key deleted" false
    (Qmljs.Object.has_atom object_ ~key:answer);
  Qmljs.Runtime.with_ @@ fun foreign_runtime ->
  Qmljs.Context.with_ foreign_runtime @@ fun foreign_context ->
  let foreign_object = Qmljs.Object.create foreign_context in
  expect_structured_error "cross-runtime atom key"
    (fun () ->
      Qmljs.Object.set_atom foreign_object ~key:answer
        (Qmljs.Value.int foreign_context 1))
    (fun error ->
      check_kind "cross-runtime atom key kind" Qmljs.Error.Wrong_domain
        (Qmljs.Error.kind error))

let test_closed_atom_is_rejected () =
  let atom =
    Qmljs.Runtime.with_ @@ fun runtime ->
    Qmljs.Context.with_ runtime @@ fun context ->
    Qmljs.Atom.of_string context "closed"
  in
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  expect_structured_error "closed atom"
    (fun () -> ignore (Qmljs.Atom.to_string context atom : string))
    (fun error ->
      check_kind "closed atom kind" Qmljs.Error.Closed_handle
        (Qmljs.Error.kind error))

let test_object_prototype_api () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let parent = Qmljs.Object.create context in
  Qmljs.Object.set_string parent "inherited" (Qmljs.Value.int context 7);
  let child =
    Qmljs.Object.create ~prototype:(Qmljs.Value.object_ parent) context
  in
  (match Qmljs.Object.get_string child "inherited" with
   | Some value -> check_int "inherited property" 7 (Qmljs.Value.to_int value)
   | None -> fail "prototype property was not visible");
  check_bool "explicit prototype round-trips" true
    (Qmljs.Value.strict_equal
       (Qmljs.Object.get_prototype child)
       (Qmljs.Value.object_ parent));
  let null_proto =
    Qmljs.Object.create ~prototype:(Qmljs.Value.null context) context
  in
  check_bool "null prototype round-trips" true
    (Qmljs.Value.is_null (Qmljs.Object.get_prototype null_proto));
  let replacement = Qmljs.Object.create context in
  Qmljs.Object.set_string replacement "inherited" (Qmljs.Value.int context 9);
  Qmljs.Object.set_prototype child ~prototype:(Qmljs.Value.object_ replacement);
  (match Qmljs.Object.get_string child "inherited" with
   | Some value -> check_int "set prototype property" 9 (Qmljs.Value.to_int value)
   | None -> fail "new prototype property was not visible");
  expect_structured_error "prototype cycle"
    (fun () ->
      Qmljs.Object.set_prototype replacement ~prototype:(Qmljs.Value.object_ child))
    (fun error ->
      check_kind "prototype cycle kind" Qmljs.Error.JavaScript
        (Qmljs.Error.kind error))

let test_object_property_descriptor_api () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let object_ = Qmljs.Object.create context in
  let hidden_key = Qmljs.Value.string context "hidden" in
  Qmljs.Object.define_data_property object_ ~key:hidden_key
    ~value:(Qmljs.Value.int context 11)
    ~writable:false ~enumerable:false ~configurable:false;
  (match Qmljs.Object.get_own_property_descriptor object_ ~key:hidden_key with
   | Some descriptor ->
       (match descriptor.value with
        | Some value -> check_int "string descriptor value" 11 (Qmljs.Value.to_int value)
        | None -> fail "string data descriptor missing value");
       check_bool "string descriptor writable" true
         (descriptor.writable = Some false);
       check_bool "string descriptor enumerable" false descriptor.enumerable;
       check_bool "string descriptor configurable" false descriptor.configurable;
       check_bool "string descriptor has no getter" true
         (Option.is_none descriptor.get);
       check_bool "string descriptor has no setter" true
         (Option.is_none descriptor.set)
   | None -> fail "missing string data descriptor");
  let backing = ref 40 in
  let getter =
    Qmljs.Function.make context ~name:"get_answer" ~arity:0 (fun _args ->
        Qmljs.Value.int context !backing)
  in
  let setter =
    Qmljs.Function.make context ~name:"set_answer" ~arity:1 (fun args ->
        backing := Qmljs.Value.to_int args.(0);
        Qmljs.Value.undefined context)
  in
  let answer_key = Qmljs.Value.string context "answer" in
  Qmljs.Object.define_accessor_property object_ ~key:answer_key
    ~get:getter ~set:setter ~enumerable:true ~configurable:false ();
  (match Qmljs.Object.get_own_property_descriptor object_ ~key:answer_key with
   | Some descriptor ->
       check_bool "accessor descriptor has no value" true
         (Option.is_none descriptor.value);
       check_bool "accessor descriptor writable absent" true
         (Option.is_none descriptor.writable);
       check_bool "accessor descriptor enumerable" true descriptor.enumerable;
       check_bool "accessor descriptor configurable" false descriptor.configurable;
       check_bool "accessor descriptor getter" true
         (Option.is_some descriptor.get);
       check_bool "accessor descriptor setter" true
         (Option.is_some descriptor.set)
   | None -> fail "missing accessor descriptor");
  Qmljs.Object.set_string object_ "answer" (Qmljs.Value.int context 42);
  check_int "setter changed backing" 42 !backing;
  (match Qmljs.Object.get_string object_ "answer" with
   | Some value -> check_int "getter returned backing" 42 (Qmljs.Value.to_int value)
   | None -> fail "accessor get returned None");
  let symbol = Qmljs.Symbol.global context "descriptor-symbol" in
  Qmljs.Object.define_data_property object_ ~key:(Qmljs.Key.symbol symbol)
    ~value:(Qmljs.Value.int context 99)
    ~writable:true ~enumerable:true ~configurable:true;
  (match
     Qmljs.Object.get_own_property_descriptor object_
       ~key:(Qmljs.Key.symbol symbol)
   with
   | Some descriptor ->
       (match descriptor.value with
        | Some value -> check_int "symbol descriptor value" 99 (Qmljs.Value.to_int value)
        | None -> fail "symbol data descriptor missing value");
       check_bool "symbol descriptor writable" true (descriptor.writable = Some true);
       check_bool "symbol descriptor enumerable" true descriptor.enumerable;
       check_bool "symbol descriptor configurable" true descriptor.configurable
   | None -> fail "missing symbol data descriptor");
  Qmljs.Context.set_global context ~name:"fixture" (Qmljs.Value.object_ object_);
  let js_view =
    Qmljs.Context.eval context
      {|[
          fixture.answer,
          Object.keys(fixture).includes("answer"),
          Object.keys(fixture).includes("hidden"),
          fixture[Symbol.for("descriptor-symbol")]
        ].join(":")|}
  in
  check_string "descriptor behavior visible to JS" "42:true:false:99"
    (Qmljs.Value.to_string js_view)

let test_object_own_key_iteration_api () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let object_ = Qmljs.Object.create context in
  let visible = Qmljs.Value.string context "visible" in
  let hidden = Qmljs.Value.string context "hidden" in
  let symbol = Qmljs.Symbol.global context "visible-symbol" in
  let hidden_symbol = Qmljs.Symbol.global context "hidden-symbol" in
  Qmljs.Object.define_data_property object_ ~key:visible
    ~value:(Qmljs.Value.int context 1)
    ~writable:true ~enumerable:true ~configurable:true;
  Qmljs.Object.define_data_property object_ ~key:hidden
    ~value:(Qmljs.Value.int context 2)
    ~writable:true ~enumerable:false ~configurable:true;
  Qmljs.Object.define_data_property object_ ~key:(Qmljs.Key.symbol symbol)
    ~value:(Qmljs.Value.int context 3)
    ~writable:true ~enumerable:true ~configurable:true;
  Qmljs.Object.define_data_property object_ ~key:(Qmljs.Key.symbol hidden_symbol)
    ~value:(Qmljs.Value.int context 4)
    ~writable:true ~enumerable:false ~configurable:true;
  let sorted_strings values =
    values |> List.map Qmljs.Value.to_string |> List.sort String.compare
  in
  check_string "default own keys"
    "visible"
    (String.concat "," (sorted_strings (Qmljs.Object.own_keys object_)));
  check_string "all string own keys"
    "hidden,visible"
    (String.concat ","
       (sorted_strings
          (Qmljs.Object.own_keys ~enumerable_only:false object_)));
  let enum_symbols =
    Qmljs.Object.own_keys ~strings:false ~symbols:true object_
  in
  check_int "enumerable symbol count" 1 (List.length enum_symbols);
  check_bool "enumerable symbol key matches" true
    (Qmljs.Value.strict_equal (List.hd enum_symbols) (Qmljs.Key.symbol symbol));
  check_int "all string and symbol own keys" 4
    (List.length
       (Qmljs.Object.own_keys ~symbols:true ~enumerable_only:false object_));
  let entry_values =
    Qmljs.Object.entries object_
    |> List.map (fun (_key, value) -> Qmljs.Value.to_int value)
    |> List.sort compare
  in
  if entry_values <> [ 1 ] then fail "default entries returned wrong values";
  let values =
    Qmljs.Object.values ~symbols:true object_
    |> List.map Qmljs.Value.to_int
    |> List.sort compare
  in
  if values <> [ 1; 3 ] then fail "values returned wrong enumerable values"

let test_proxy_api () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let target = Qmljs.Object.create context in
  Qmljs.Object.set_string target "answer" (Qmljs.Value.int context 1);
  let handler = Qmljs.Object.create context in
  let get_trap =
    Qmljs.Function.make context ~name:"get" ~arity:3 @@ fun args ->
    match Array.get args 1 with
    | property when String.equal (Qmljs.Value.to_string property) "answer" ->
        Qmljs.Value.int context 42
    | _ -> Qmljs.Value.undefined context
    | exception Invalid_argument _ -> Qmljs.Value.undefined context
  in
  Qmljs.Object.set_string handler "get" get_trap;
  let proxy = Qmljs.Proxy.create ~target ~handler in
  check_bool "proxy predicate" true (Qmljs.Proxy.is_proxy proxy);
  check_bool "plain object is not proxy" false (Qmljs.Proxy.is_proxy target);
  (match Qmljs.Proxy.target proxy with
   | Some proxy_target ->
       check_bool "proxy target identity" true
         (Qmljs.Value.strict_equal proxy_target target)
   | None -> fail "proxy target was not exposed");
  (match Qmljs.Proxy.handler proxy with
   | Some proxy_handler ->
       check_bool "proxy handler identity" true
         (Qmljs.Value.strict_equal proxy_handler handler)
   | None -> fail "proxy handler was not exposed");
  (match Qmljs.Proxy.target target, Qmljs.Proxy.handler target with
   | None, None -> ()
   | _ -> fail "plain object exposed proxy internals");
  Qmljs.Context.set_global context ~name:"proxy" proxy;
  check_int "proxy get trap" 42
    (Qmljs.Value.to_int (Qmljs.Context.eval context "proxy.answer"));
  ignore (runtime : Qmljs.Runtime.t)

let test_function_call_constructor_and_metadata_api () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let receiver = Qmljs.Object.create context in
  Qmljs.Object.set_string receiver "base" (Qmljs.Value.int context 37);
  let js_add = Qmljs.Context.eval context "(function add(x) { return this.base + x; })" in
  check_int "explicit this call" 42
    (Qmljs.Value.to_int
       (Qmljs.Function.call js_add
          ~this_:(Qmljs.Value.object_ receiver)
          [| Qmljs.Value.int context 5 |]));
  let host_add =
    Qmljs.Function.make_with_this context ~name:"host_add" ~arity:1
      (fun call ->
        check_bool "host callback is ordinary call" true
          (Option.is_none call.new_target);
        check_int "host args length" 1 (Stdlib.Array.length call.args);
        match Qmljs.Object.get_string call.this_ "base" with
        | Some base ->
            Qmljs.Value.int context
              (Qmljs.Value.to_int base + Qmljs.Value.to_int call.args.(0))
        | None -> fail "host callback this did not have base")
  in
  Qmljs.Object.set_string receiver "hostAdd" host_add;
  Qmljs.Context.set_global context ~name:"receiver" (Qmljs.Value.object_ receiver);
  check_int "host callback receives this" 45
    (Qmljs.Value.to_int (Qmljs.Context.eval context "receiver.hostAdd(8)"));
  let host_constructor =
    Qmljs.Function.make_with_this context ~name:"HostBox" ~arity:1
      (fun call ->
        match call.new_target with
        | None -> fail "host constructor did not receive constructor state"
        | Some new_target ->
            check_string "host constructor new.target" "HostBox"
              (Qmljs.Function.name new_target);
            check_bool "host constructor this is undefined" true
              (Qmljs.Value.is_undefined call.this_);
            check_int "host constructor args length" 1
              (Stdlib.Array.length call.args);
            let result = Qmljs.Object.create context in
            Qmljs.Object.set_string result "value" call.args.(0);
            Qmljs.Value.object_ result)
  in
  Qmljs.Function.set_constructor host_constructor true;
  Qmljs.Context.set_global context ~name:"HostBox" host_constructor;
  check_int "host constructor receives new.target" 19
    (Qmljs.Value.to_int (Qmljs.Context.eval context "new HostBox(19).value"));
  let constructor =
    Qmljs.Context.eval context
      {|
        class Box {
          constructor(value) { this.value = value; }
          double() { return this.value * 2; }
        }
        Box
      |}
  in
  check_bool "class is constructor" true (Qmljs.Function.is_constructor constructor);
  let instance =
    Qmljs.Function.construct constructor [| Qmljs.Value.int context 21 |]
  in
  Qmljs.Context.set_global context ~name:"box" instance;
  check_int "construct creates instance" 42
    (Qmljs.Value.to_int (Qmljs.Context.eval context "box.double()"));
  let host =
    Qmljs.Function.make context ~name:"original" ~arity:2 (fun _args ->
        Qmljs.Value.undefined context)
  in
  check_string "function name" "original" (Qmljs.Function.name host);
  check_int "function length" 2 (Qmljs.Function.length host);
  Qmljs.Function.set_name host "renamed";
  Qmljs.Function.set_length host 4;
  check_string "renamed function" "renamed" (Qmljs.Function.name host);
  check_int "resized function length" 4 (Qmljs.Function.length host);
  check_bool "host function initially not constructor" false
    (Qmljs.Function.is_constructor host);
  Qmljs.Function.set_constructor host true;
  check_bool "host constructor bit" true (Qmljs.Function.is_constructor host);
  Qmljs.Context.set_global context ~name:"functionProto"
    (Qmljs.Function.prototype context);
  check_bool "Function.prototype helper" true
    (Qmljs.Value.to_bool
       (Qmljs.Context.eval context "functionProto === Function.prototype"))

let test_async_host_callback_rejection_is_structured () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  Qmljs.Function.set_global_async context ~name:"later_host_fail" ~arity:0
    (fun _args ->
      Eio.Fiber.yield ();
      raise
        (Qmljs.Error.host ~message:"async host boom"
           ~metadata:[ "request_id", "async-42" ]));
  let promise = Qmljs.Context.eval context "later_host_fail()" in
  match Qmljs.Promise.await_result ~yield_every:1 promise with
  | Ok _ -> fail "async host callback unexpectedly resolved"
  | Error error ->
      check_kind "async host defect kind" Qmljs.Error.Host_callback_defect
        (Qmljs.Error.kind error);
      check_string "async host message" "async host boom"
        (Qmljs.Error.message error);
      check_bool "async host metadata" true
        (List.mem ("request_id", "async-42") (Qmljs.Error.metadata error));
      check_bool "async host OCaml frame" true
        (Qmljs.Error.has_ocaml_frame error ~function_name:"later_host_fail")

let test_async_host_callback_foreign_return_rejects_structured () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  let left = Qmljs.Context.create runtime in
  let right = Qmljs.Context.create runtime in
  let foreign = Qmljs.Value.int right 41 in
  Qmljs.Function.set_global_async left ~name:"bad_async" ~arity:0
    (fun _args ->
      Eio.Fiber.yield ();
      foreign);
  let promise = Qmljs.Context.eval left "bad_async()" in
  (match Qmljs.Promise.await_result ~yield_every:1 promise with
   | Ok _ -> fail "async foreign host return unexpectedly resolved"
   | Error error ->
       check_kind "async foreign host return kind"
         Qmljs.Error.Host_callback_defect (Qmljs.Error.kind error);
       check_string "async foreign host return message"
         "QuickJS value belongs to a different context"
         (Qmljs.Error.message error);
       check_bool "async foreign host return frame" true
         (Qmljs.Error.has_ocaml_frame error ~function_name:"bad_async"));
  check_int "runtime reusable after async foreign return" 2
    (Qmljs.Value.to_int (Qmljs.Context.eval left "1 + 1"))

let test_function_defer_api () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let receiver = Qmljs.Object.create context in
  Qmljs.Object.set_string receiver "base" (Qmljs.Value.int context 40);
  let deferred =
    Qmljs.Context.eval context
      {|
        globalThis.deferredResult = 0;
        function deferredAdd(x) { globalThis.deferredResult = this.base + x; }
        deferredAdd
      |}
  in
  Qmljs.Function.defer deferred
    ~this_:(Qmljs.Value.object_ receiver)
    [| Qmljs.Value.int context 2 |];
  check_int "deferred function has not run yet" 0
    (Qmljs.Value.to_int (Qmljs.Context.eval context "globalThis.deferredResult"));
  check_bool "defer enqueued job" true (Qmljs.Runtime.is_job_pending runtime);
  ignore (Qmljs.Async.drain_jobs runtime : int);
  check_int "deferred function ran" 42
    (Qmljs.Value.to_int (Qmljs.Context.eval context "globalThis.deferredResult"))

type counter =
  { mutable count : int
  }

let test_host_class_api () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let counter_class = Qmljs.Class.create context ~name:"Counter" in
  Qmljs.Class.define_method counter_class ~name:"inc" ~arity:1
    (fun call ->
      let amount =
        match Array.get call.Qmljs.Class.args 0 with
        | value -> Qmljs.Value.to_int value
        | exception Invalid_argument _ -> 1
      in
      call.state.count <- call.state.count + amount;
      Qmljs.Value.int context call.state.count);
  Qmljs.Class.define_accessor counter_class ~name:"count"
    ~get:(fun call -> Qmljs.Value.int context call.state.count)
    ~set:(fun call value -> call.state.count <- Qmljs.Value.to_int value)
    ();
  let constructor =
    Qmljs.Class.constructor counter_class ~name:"Counter" ~arity:1
      (fun args ->
        let count =
          match Array.get args 0 with
          | value -> Qmljs.Value.to_int value
          | exception Invalid_argument _ -> 0
        in
        { count })
  in
  Qmljs.Context.set_global context ~name:"Counter" constructor;
  Qmljs.Object.set_string constructor "kind" (Qmljs.Value.string context "counter");
  let direct_state = { count = 3 } in
  let direct = Qmljs.Class.instance counter_class direct_state in
  Qmljs.Context.set_global context ~name:"directCounter" direct;
  check_bool "direct class instance" true
    (Qmljs.Class.is_instance counter_class direct);
  (match Qmljs.Class.get counter_class direct with
   | Some state when state == direct_state -> ()
   | Some _ -> fail "class get returned a different payload"
   | None -> fail "class get did not return the payload");
  check_bool "plain object is not class instance" false
    (Qmljs.Class.is_instance counter_class (Qmljs.Object.create context));
  let result =
    Qmljs.Context.eval context
      {|const c = new Counter(10);
        if (!(c instanceof Counter)) throw new Error("instanceof failed");
        if (Counter.kind !== "counter") throw new Error("static property failed");
        c.inc(5);
        directCounter.count = 20;
        c.count + directCounter.inc(2);|}
  in
  check_int "class methods and accessors" 37 (Qmljs.Value.to_int result);
  check_int "class payload mutates from JS" 22 direct_state.count;
  expect_structured_error "class constructor requires new"
    (fun () -> ignore (Qmljs.Context.eval context "Counter(1)" : Qmljs.Value.t))
    (fun error ->
      check_kind "constructor defect kind" Qmljs.Error.Host_callback_defect
        (Qmljs.Error.kind error));
  let finalized = ref 0 in
  let finalizing_class =
    Qmljs.Class.create context ~name:"Finalizing"
      ~finalize:(fun state -> finalized := !finalized + state.count)
  in
  let make_finalized_instance () =
    ignore (Qmljs.Class.instance finalizing_class { count = 5 } : Qmljs.Value.t)
  in
  make_finalized_instance ();
  ignore (Qmljs.Diagnostics.snapshot () : Qmljs.Diagnostics.snapshot);
  ignore (Qmljs.Context.eval context "undefined" : Qmljs.Value.t);
  check_int "class finalizer runs on release" 5 !finalized

let test_iterable_api () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let remaining = ref [ 1; 2; 3 ] in
  let iterable =
    Qmljs.Iterable.of_step context @@ fun () ->
    match !remaining with
    | [] -> None
    | value :: rest ->
        remaining := rest;
        Some (Qmljs.Value.int context value)
  in
  Qmljs.Context.set_global context ~name:"ocamlIterable" iterable;
  check_string "OCaml iterable spreads in JS" "1,2,3"
    (Qmljs.Value.to_string
       (Qmljs.Context.eval context "[...ocamlIterable].join(',')"));
  let set = Qmljs.Context.eval context "new Set([4, 5, 6])" in
  (match Qmljs.Iterable.to_list set with
   | Ok values ->
       let ints = List.map Qmljs.Value.to_int values in
       if ints <> [ 4; 5; 6 ] then fail "JS Set consumed in wrong order"
   | Error error -> fail ("Set iteration failed: " ^ Qmljs.Error.message error));
  let iterator = Qmljs.Context.eval context "['a', 'b'][Symbol.iterator]()" in
  (match Qmljs.Iterable.to_list iterator with
   | Ok values ->
       let strings = List.map Qmljs.Value.to_string values in
       if strings <> [ "a"; "b" ] then fail "JS iterator consumed in wrong order"
   | Error error ->
       fail ("direct iterator consumption failed: " ^ Qmljs.Error.message error));
  let symbol_visible =
    Qmljs.Object.create context
  in
  Qmljs.Object.define_data_property symbol_visible
    ~key:(Qmljs.Key.symbol (Qmljs.Symbol.iterator context))
    ~value:(Qmljs.Value.string context "iterator-key")
    ~writable:false
    ~enumerable:false
    ~configurable:true;
  Qmljs.Context.set_global context ~name:"symbolVisible" symbol_visible;
  check_string "Symbol.iterator public key" "iterator-key"
    (Qmljs.Value.to_string
       (Qmljs.Context.eval context "symbolVisible[Symbol.iterator]"));
  (match Qmljs.Iterable.to_list (Qmljs.Value.int context 42) with
   | Ok _ -> fail "non-iterable value was accepted"
   | Error error ->
       check_kind "non-iterable error kind" Qmljs.Error.JavaScript
         (Qmljs.Error.kind error));
  (match Qmljs.Iterable.to_list ~max_steps:1 set with
   | Ok _ -> fail "iterator max step limit was not enforced"
   | Error error ->
       check_kind "iterator limit kind" Qmljs.Error.Resource_limit
         (Qmljs.Error.kind error));
  ignore (runtime : Qmljs.Runtime.t)

let test_typed_array_readback_api () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let array =
    Qmljs.Typed_array.Uint8Array.of_bytes_copy context (Bytes.of_string "\001\002\003")
  in
  check_int "uint8 length" 3 (Qmljs.Typed_array.Uint8Array.length array);
  check_int "uint8 get" 2 (Qmljs.Typed_array.Uint8Array.get array 1);
  Qmljs.Typed_array.Uint8Array.set array 1 9;
  check_int "uint8 get after set" 9 (Qmljs.Typed_array.Uint8Array.get array 1);
  check_bytes "uint8 bytes after set" (Bytes.of_string "\001\009\003")
    (Qmljs.Typed_array.Uint8Array.to_bytes_copy array)

let test_array_buffer_api () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let source = Bytes.of_string "\001\002\003" in
  let buffer = Qmljs.Array_buffer.of_bytes_copy context source in
  Bytes.set source 1 '\009';
  check_int "array buffer length" 3 (Qmljs.Array_buffer.byte_length buffer);
  check_bytes "array buffer copied bytes" (Bytes.of_string "\001\002\003")
    (Qmljs.Array_buffer.to_bytes_copy buffer);
  let bigarray =
    Bigarray.Array1.of_array Bigarray.int8_unsigned Bigarray.c_layout
      [| 4; 5; 6 |]
  in
  let from_bigarray = Qmljs.Array_buffer.of_bigarray_copy context bigarray in
  Bigarray.Array1.set bigarray 1 9;
  check_bytes "array buffer copied bigarray" (Bytes.of_string "\004\005\006")
    (Qmljs.Array_buffer.to_bytes_copy from_bigarray);
  Qmljs.Context.set_global context ~name:"buffer" (Qmljs.Value.object_ buffer);
  check_int "array buffer visible to JS" 3
    (Qmljs.Value.to_int (Qmljs.Context.eval context "buffer.byteLength"));
  Qmljs.Array_buffer.detach buffer;
  expect_structured_error "detached array buffer byte_length"
    (fun () -> ignore (Qmljs.Array_buffer.byte_length buffer : int))
    (fun error ->
      check_kind "detached byte_length kind" Qmljs.Error.JavaScript
        (Qmljs.Error.kind error));
  expect_structured_error "detached array buffer to_bytes"
    (fun () -> ignore (Qmljs.Array_buffer.to_bytes_copy buffer : bytes))
    (fun error ->
      check_kind "detached to_bytes kind" Qmljs.Error.JavaScript
        (Qmljs.Error.kind error))

let test_uint8_array_buffer_interop_api () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let buffer =
    Qmljs.Array_buffer.of_bytes_copy context (Bytes.of_string "\010\020\030\040")
  in
  let view =
    Qmljs.Typed_array.Uint8Array.of_array_buffer
      ~byte_offset:1 ~length:2 buffer
  in
  check_int "uint8 view element length" 2
    (Qmljs.Typed_array.Uint8Array.length view);
  check_int "uint8 view byte offset" 1
    (Qmljs.Typed_array.Uint8Array.byte_offset view);
  check_int "uint8 view byte length" 2
    (Qmljs.Typed_array.Uint8Array.byte_length view);
  check_bool "uint8 view backing buffer" true
    (Qmljs.Value.strict_equal
       (Qmljs.Typed_array.Uint8Array.buffer view)
       (Qmljs.Value.object_ buffer));
  check_int "uint8 view get" 20 (Qmljs.Typed_array.Uint8Array.get view 0);
  Qmljs.Typed_array.Uint8Array.set view 1 99;
  check_bytes "uint8 view writes backing buffer"
    (Bytes.of_string "\010\020\099\040")
    (Qmljs.Array_buffer.to_bytes_copy buffer);
  expect_structured_error "uint8 view invalid offset"
    (fun () ->
      ignore
        (Qmljs.Typed_array.Uint8Array.of_array_buffer
           ~byte_offset:99 ~length:1 buffer
         : Qmljs.Typed_array.Uint8Array.t))
    (fun error ->
      check_kind "uint8 invalid offset kind" Qmljs.Error.JavaScript
        (Qmljs.Error.kind error))

let test_typed_array_family_api () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let backing () =
    Qmljs.Array_buffer.of_bytes_copy context (Bytes.make 128 '\000')
  in
  let check_constructor label expected view =
    Qmljs.Context.set_global context ~name:"typedFixture"
      (Qmljs.Value.typed_array view);
    check_string (label ^ " constructor") expected
      (Qmljs.Value.to_string
         (Qmljs.Context.eval context "typedFixture.constructor.name"))
  in
  let check_int_view label expected_constructor bytes_per_element make length
      byte_offset byte_length buffer_ get set first second expected_first
      expected_second =
    let buffer = backing () in
    let view = make buffer in
    check_constructor label expected_constructor view;
    check_int (label ^ " length") 2 (length view);
    check_int (label ^ " byte offset") bytes_per_element (byte_offset view);
    check_bool (label ^ " buffer") true
      (Qmljs.Value.strict_equal
         (buffer_ view)
         (Qmljs.Value.object_ buffer));
    set view 0 first;
    set view 1 second;
    check_int (label ^ " byte length") (2 * bytes_per_element)
      (byte_length view);
    check_int (label ^ " first") expected_first (get view 0);
    check_int (label ^ " second") expected_second (get view 1)
  in
  check_int_view "Int8Array" "Int8Array" 1
    (Qmljs.Typed_array.Int8Array.of_array_buffer ~byte_offset:1 ~length:2)
    Qmljs.Typed_array.Int8Array.length
    Qmljs.Typed_array.Int8Array.byte_offset
    Qmljs.Typed_array.Int8Array.byte_length
    Qmljs.Typed_array.Int8Array.buffer
    Qmljs.Typed_array.Int8Array.get
    Qmljs.Typed_array.Int8Array.set
    (-5) 12 (-5) 12;
  check_int_view "Uint8Array" "Uint8Array" 1
    (Qmljs.Typed_array.Uint8Array.of_array_buffer ~byte_offset:1 ~length:2)
    Qmljs.Typed_array.Uint8Array.length
    Qmljs.Typed_array.Uint8Array.byte_offset
    Qmljs.Typed_array.Uint8Array.byte_length
    Qmljs.Typed_array.Uint8Array.buffer
    Qmljs.Typed_array.Uint8Array.get
    Qmljs.Typed_array.Uint8Array.set
    250 12 250 12;
  check_int_view "Uint8ClampedArray" "Uint8ClampedArray" 1
    (Qmljs.Typed_array.Uint8ClampedArray.of_array_buffer
       ~byte_offset:1 ~length:2)
    Qmljs.Typed_array.Uint8ClampedArray.length
    Qmljs.Typed_array.Uint8ClampedArray.byte_offset
    Qmljs.Typed_array.Uint8ClampedArray.byte_length
    Qmljs.Typed_array.Uint8ClampedArray.buffer
    Qmljs.Typed_array.Uint8ClampedArray.get
    Qmljs.Typed_array.Uint8ClampedArray.set
    (-5) 300 0 255;
  check_int_view "Int16Array" "Int16Array" 2
    (Qmljs.Typed_array.Int16Array.of_array_buffer ~byte_offset:2 ~length:2)
    Qmljs.Typed_array.Int16Array.length
    Qmljs.Typed_array.Int16Array.byte_offset
    Qmljs.Typed_array.Int16Array.byte_length
    Qmljs.Typed_array.Int16Array.buffer
    Qmljs.Typed_array.Int16Array.get
    Qmljs.Typed_array.Int16Array.set
    (-1234) 2345 (-1234) 2345;
  check_int_view "Uint16Array" "Uint16Array" 2
    (Qmljs.Typed_array.Uint16Array.of_array_buffer ~byte_offset:2 ~length:2)
    Qmljs.Typed_array.Uint16Array.length
    Qmljs.Typed_array.Uint16Array.byte_offset
    Qmljs.Typed_array.Uint16Array.byte_length
    Qmljs.Typed_array.Uint16Array.buffer
    Qmljs.Typed_array.Uint16Array.get
    Qmljs.Typed_array.Uint16Array.set
    65535 42 65535 42;
  check_int_view "Int32Array" "Int32Array" 4
    (Qmljs.Typed_array.Int32Array.of_array_buffer ~byte_offset:4 ~length:2)
    Qmljs.Typed_array.Int32Array.length
    Qmljs.Typed_array.Int32Array.byte_offset
    Qmljs.Typed_array.Int32Array.byte_length
    Qmljs.Typed_array.Int32Array.buffer
    Qmljs.Typed_array.Int32Array.get
    Qmljs.Typed_array.Int32Array.set
    (-123456) 234567 (-123456) 234567;
  check_int_view "Uint32Array" "Uint32Array" 4
    (Qmljs.Typed_array.Uint32Array.of_array_buffer ~byte_offset:4 ~length:2)
    Qmljs.Typed_array.Uint32Array.length
    Qmljs.Typed_array.Uint32Array.byte_offset
    Qmljs.Typed_array.Uint32Array.byte_length
    Qmljs.Typed_array.Uint32Array.buffer
    Qmljs.Typed_array.Uint32Array.get
    Qmljs.Typed_array.Uint32Array.set
    4_000_000_000 42 4_000_000_000 42;
  let check_float_view label expected_constructor bytes_per_element make length
      byte_offset byte_length buffer_ get set =
    let buffer = backing () in
    let view = make buffer in
    check_constructor label expected_constructor view;
    check_int (label ^ " length") 2 (length view);
    check_int (label ^ " byte offset") bytes_per_element (byte_offset view);
    check_bool (label ^ " buffer") true
      (Qmljs.Value.strict_equal
         (buffer_ view)
         (Qmljs.Value.object_ buffer));
    check_int (label ^ " byte length") (2 * bytes_per_element)
      (byte_length view);
    set view 0 1.5;
    set view 1 (-2.25);
    check_bool (label ^ " first") true (Float.abs (get view 0 -. 1.5) < 0.0001);
    check_bool (label ^ " second") true (Float.abs (get view 1 +. 2.25) < 0.0001)
  in
  check_float_view "Float32Array" "Float32Array" 4
    (Qmljs.Typed_array.Float32Array.of_array_buffer ~byte_offset:4 ~length:2)
    Qmljs.Typed_array.Float32Array.length
    Qmljs.Typed_array.Float32Array.byte_offset
    Qmljs.Typed_array.Float32Array.byte_length
    Qmljs.Typed_array.Float32Array.buffer
    Qmljs.Typed_array.Float32Array.get
    Qmljs.Typed_array.Float32Array.set;
  check_float_view "Float64Array" "Float64Array" 8
    (Qmljs.Typed_array.Float64Array.of_array_buffer ~byte_offset:8 ~length:2)
    Qmljs.Typed_array.Float64Array.length
    Qmljs.Typed_array.Float64Array.byte_offset
    Qmljs.Typed_array.Float64Array.byte_length
    Qmljs.Typed_array.Float64Array.buffer
    Qmljs.Typed_array.Float64Array.get
    Qmljs.Typed_array.Float64Array.set;
  let check_bigint_view label expected_constructor bytes_per_element make length
      byte_offset byte_length buffer_ get set first second =
    let buffer = backing () in
    let view = make buffer in
    check_constructor label expected_constructor view;
    check_int (label ^ " length") 2 (length view);
    check_int (label ^ " byte offset") bytes_per_element (byte_offset view);
    check_bool (label ^ " buffer") true
      (Qmljs.Value.strict_equal
         (buffer_ view)
         (Qmljs.Value.object_ buffer));
    check_int (label ^ " byte length") (2 * bytes_per_element)
      (byte_length view);
    set view 0 first;
    set view 1 second;
    check_string (label ^ " first") (Qmljs.Bigint.to_string first)
      (Qmljs.Bigint.to_string (get view 0));
    check_string (label ^ " second") (Qmljs.Bigint.to_string second)
      (Qmljs.Bigint.to_string (get view 1))
  in
  check_bigint_view "BigInt64Array" "BigInt64Array" 8
    (Qmljs.Typed_array.BigInt64Array.of_array_buffer ~byte_offset:8 ~length:2)
    Qmljs.Typed_array.BigInt64Array.length
    Qmljs.Typed_array.BigInt64Array.byte_offset
    Qmljs.Typed_array.BigInt64Array.byte_length
    Qmljs.Typed_array.BigInt64Array.buffer
    Qmljs.Typed_array.BigInt64Array.get
    Qmljs.Typed_array.BigInt64Array.set
    (Qmljs.Bigint.of_int64 context (-42L))
    (Qmljs.Bigint.of_int64 context 123L);
  check_bigint_view "BigUint64Array" "BigUint64Array" 8
    (Qmljs.Typed_array.BigUint64Array.of_array_buffer ~byte_offset:8 ~length:2)
    Qmljs.Typed_array.BigUint64Array.length
    Qmljs.Typed_array.BigUint64Array.byte_offset
    Qmljs.Typed_array.BigUint64Array.byte_length
    Qmljs.Typed_array.BigUint64Array.buffer
    Qmljs.Typed_array.BigUint64Array.get
    Qmljs.Typed_array.BigUint64Array.set
    (Qmljs.Bigint.of_string context "18446744073709551615")
    (Qmljs.Bigint.of_int64 context 123L);
  expect_structured_error "typed array unaligned byte offset"
    (fun () ->
      ignore
        (Qmljs.Typed_array.Int16Array.of_array_buffer
           ~byte_offset:1 ~length:1 (backing ())
         : Qmljs.Typed_array.Int16Array.t))
    (fun error ->
      check_kind "typed array unaligned offset kind" Qmljs.Error.JavaScript
        (Qmljs.Error.kind error));
  expect_structured_error "typed array length exceeds buffer"
    (fun () ->
      ignore
        (Qmljs.Typed_array.Float64Array.of_array_buffer
           ~byte_offset:120 ~length:2 (backing ())
         : Qmljs.Typed_array.Float64Array.t))
    (fun error ->
      check_kind "typed array invalid length kind" Qmljs.Error.JavaScript
        (Qmljs.Error.kind error))

let test_array_api () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let array = Qmljs.Array.create context in
  check_int "empty array length" 0 (Qmljs.Array.length array);
  check_int "push returns length" 1
    (Qmljs.Array.push array (Qmljs.Value.int context 10));
  check_int "append returns length" 3
    (Qmljs.Array.append array
       [ Qmljs.Value.int context 20; Qmljs.Value.int context 30 ]);
  check_int "array length after append" 3 (Qmljs.Array.length array);
  check_int "array get before set" 20
    (Qmljs.Value.to_int (Qmljs.Array.get array 1));
  Qmljs.Array.set array 1 (Qmljs.Value.int context 22);
  check_int "array get after set" 22
    (Qmljs.Value.to_int (Qmljs.Array.get array 1));
  check_int "to_list length" 3 (List.length (Qmljs.Array.to_list array));
  check_int "to_array length" 3
    (Stdlib.Array.length (Qmljs.Array.to_array array));
  let values = List.map Qmljs.Value.to_int (Qmljs.Array.to_list array) in
  if values <> [ 10; 22; 30 ] then fail "array to_list returned wrong values";
  Qmljs.Array.set array 5 (Qmljs.Value.int context 60);
  check_int "sparse set extends length" 6 (Qmljs.Array.length array);
  check_bool "hole reads as undefined" true
    (Qmljs.Value.is_undefined (Qmljs.Array.get array 3));
  Qmljs.Context.set_global context ~name:"fixture" (Qmljs.Value.array array);
  let js_view =
    Qmljs.Context.eval context
      "Array.isArray(fixture) + ':' + fixture.length + ':' + fixture[1] + ':' + fixture[5]"
  in
  check_string "array is visible to JS" "true:6:22:60"
    (Qmljs.Value.to_string js_view)

let test_bigint_readback_api () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let small = Qmljs.Bigint.of_int64 context 42L in
  check_string "bigint string" "42" (Qmljs.Bigint.to_string small);
  (match Qmljs.Bigint.to_int64 small with
   | Some value when Int64.equal value 42L -> ()
   | _ -> fail "small bigint did not round-trip as int64");
  let too_large = Qmljs.Bigint.of_string context "9223372036854775808" in
  check_bool "large bigint does not silently truncate" true
    (Option.is_none (Qmljs.Bigint.to_int64 too_large))

let test_native_module_api () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let evaluated =
    Qmljs.Module.eval_with_meta ~filename:"meta-main.js" context
      {|import.meta.qmljs = "visible";
        export const answer = 11;
        export default import.meta.qmljs;|}
  in
  Qmljs.Context.set_global context ~name:"metaNamespace"
    evaluated.Qmljs.Module.namespace;
  check_int "evaluated module namespace is a value" 11
    (Qmljs.Value.to_int (Qmljs.Context.eval context "metaNamespace.answer"));
  Qmljs.Context.set_global context ~name:"metaObject"
    evaluated.Qmljs.Module.import_meta;
  check_string "evaluated module import.meta object" "visible"
    (Qmljs.Value.to_string
       (Qmljs.Context.eval context "metaObject.qmljs"));
  let evaluations = ref 0 in
  let native_math =
    Qmljs.Module.Native.define
      (fun declarations ->
        Qmljs.Module.Native.declare declarations "answer";
        Qmljs.Module.Native.declare declarations "default")
      ~evaluate:(fun exports ->
        incr evaluations;
        Qmljs.Module.Native.export exports ~name:"answer"
          (Qmljs.Value.int context 42);
        Qmljs.Module.Native.export exports ~name:"default"
          (Qmljs.Value.int context 7))
  in
  Qmljs.Module.declare_native context ~name:"native:math" native_math;
  let namespace =
    Qmljs.Module.eval ~filename:"native-main.js" context
      {|import defaultValue, { answer } from "native:math";
        import { answer as answerAgain } from "native:math";
        export const named = answer;
        export default defaultValue + answer + answerAgain;|}
  in
  check_int "native module default export" 91
    (Qmljs.Value.to_int (Qmljs.Module.default namespace));
  (match Qmljs.Module.get namespace ~name:"named" with
   | Some value -> check_int "native module named export" 42 (Qmljs.Value.to_int value)
   | None -> fail "native module namespace did not expose named export");
  check_int "native module evaluated once" 1 !evaluations;
  let bad_module =
    Qmljs.Module.Native.define
      (fun declarations ->
        Qmljs.Module.Native.declare declarations "default")
      ~evaluate:(fun _exports ->
        raise
          (Qmljs.Error.host ~message:"native module boom"
             ~metadata:[ "module", "native:bad" ]))
  in
  Qmljs.Module.declare_native context ~name:"native:bad" bad_module;
  expect_structured_error "native module evaluation defect"
    (fun () ->
      ignore
        (Qmljs.Module.eval ~filename:"native-bad-main.js" context
           {|import value from "native:bad"; export default value;|}
         : Qmljs.Module.namespace))
    (fun error ->
      check_kind "native module defect kind" Qmljs.Error.Host_callback_defect
        (Qmljs.Error.kind error);
      check_string "native module defect message" "native module boom"
        (Qmljs.Error.message error))

let create_temp_dir () =
  let rec loop attempts =
    if attempts = 0 then fail "could not create temporary module loader directory";
    let name =
      Printf.sprintf "qmljs-loader-%d-%d" (Unix.getpid ()) (Random.bits ())
    in
    let root = Filename.concat (Filename.get_temp_dir_name ()) name in
    try
      Unix.mkdir root 0o700;
      root
    with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> loop (attempts - 1)
  in
  loop 16

let write_file path source =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel source)

let mkdir path =
  try Unix.mkdir path 0o700 with
  | Unix.Unix_error (Unix.EEXIST, _, _) -> ()

let rec remove_tree path =
  if Sys.file_exists path then
    if Sys.is_directory path then begin
      Sys.readdir path
      |> Array.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path
    end else Sys.remove path

let with_temp_loader_dir fn =
  let root = create_temp_dir () in
  Fun.protect
    ~finally:(fun () ->
      try remove_tree root with
      | Sys_error _ | Unix.Unix_error _ -> ())
    (fun () -> fn root)

let test_module_loader_filesystem_api () =
  with_temp_loader_dir @@ fun root ->
  mkdir (Filename.concat root "pages");
  write_file (Filename.concat root "dep.js") "export const answer = 42;";
  write_file (Filename.concat root "data.json") {|{"answer":41}|};
  write_file (Filename.concat root "blocked.js") "export default 0;";
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let loader =
    Qmljs.Module.Loader.filesystem ~root ~allow:(fun ~specifier ->
        not (String.equal specifier "blocked.js"))
    |> Qmljs.Module.Loader.normalize (fun ~base:_ ~specifier ->
        if String.equal specifier "alias" then Ok "dep.js" else Ok specifier)
  in
  Qmljs.Runtime.set_module_loader runtime loader;
  let namespace =
    Qmljs.Module.eval ~filename:"main.js" context
      {|import { answer } from "alias"; export default answer;|}
  in
  check_int "filesystem module default" 42
    (Qmljs.Value.to_int (Qmljs.Module.default namespace));
  let relative_namespace =
    Qmljs.Module.eval ~filename:"pages/main.js" context
      {|import { answer } from "../dep.js"; export default answer + 1;|}
  in
  check_int "filesystem loader relative import" 43
    (Qmljs.Value.to_int (Qmljs.Module.default relative_namespace));
  let json_namespace =
    Qmljs.Module.eval ~filename:"json-main.js" context
      {|import data from "data.json" with { type: "json" };
        export default data.answer;|}
  in
  check_int "filesystem loader JSON import attribute" 41
    (Qmljs.Value.to_int (Qmljs.Module.default json_namespace));
  let repeated_namespace =
    Qmljs.Module.eval ~filename:"repeated-main.js" context
      {|import { answer as a } from "dep.js";
        import { answer as b } from "alias";
        export default a + b;|}
  in
  check_int "filesystem loader repeated import" 84
    (Qmljs.Value.to_int (Qmljs.Module.default repeated_namespace));
  let callback_requests = ref [] in
  let callback_loader =
    Qmljs.Module.Loader.callback
      ~normalize:(fun ~base ~specifier ~attributes ->
        check_bool "callback loader sees import attribute" true
          (List.mem ("type", "json") attributes);
        match base, specifier with
        | Some "pages/callback-main.js", "./virtual" -> Ok "callback.json"
        | _ -> fail "callback loader received wrong normalize request")
      ~load:(fun request ->
        callback_requests := request :: !callback_requests;
        if String.equal request.Qmljs.Module.Loader.normalized "callback.json"
           && String.equal request.specifier "./virtual"
           && request.base = Some "pages/callback-main.js"
           && List.mem ("type", "json") request.attributes
        then Ok (Some {|{"answer":44}|})
        else Ok None)
  in
  Qmljs.Runtime.set_module_loader runtime callback_loader;
  let callback_namespace =
    Qmljs.Module.eval ~filename:"pages/callback-main.js" context
      {|import data from "./virtual" with { type: "json" };
        export default data.answer;|}
  in
  check_int "callback loader JSON default" 44
    (Qmljs.Value.to_int (Qmljs.Module.default callback_namespace));
  check_int "callback loader called once" 1 (List.length !callback_requests);
  let composed_virtual_loads = ref 0 in
  let composed_virtual_loader =
    Qmljs.Module.Loader.callback
      ~normalize:(fun ~base:_ ~specifier ~attributes ->
        if String.equal specifier "virtual:data" then
          check_bool "composed loader sees import attribute" true
            (List.mem ("type", "json") attributes);
        Ok specifier)
      ~load:(fun request ->
        if String.equal request.Qmljs.Module.Loader.normalized "virtual:data"
        then begin
          incr composed_virtual_loads;
          Ok (Some {|{"answer":4}|})
        end else Ok None)
  in
  let native_composed =
    Qmljs.Module.Native.define
      (fun declarations -> Qmljs.Module.Native.declare declarations "default")
      ~evaluate:(fun exports ->
        Qmljs.Module.Native.export exports ~name:"default"
          (Qmljs.Value.int context 6))
  in
  Qmljs.Module.declare_native context ~name:"native:composed" native_composed;
  let composed_loader =
    Qmljs.Module.Loader.fallback
      [ Qmljs.Module.Loader.memory
          [ "memory-only.js", "export const memory = 10;" ]
      ; composed_virtual_loader
      ; loader
      ]
  in
  Qmljs.Runtime.set_module_loader runtime composed_loader;
  let composed_namespace =
    Qmljs.Module.eval ~filename:"pages/composed-main.js" context
      {|import { memory } from "memory-only.js";
        import data from "virtual:data" with { type: "json" };
        import { answer } from "../dep.js";
        import nativeValue from "native:composed";
        export default memory + data.answer + answer + nativeValue;|}
  in
  check_int "composed loader default" 62
    (Qmljs.Value.to_int (Qmljs.Module.default composed_namespace));
  check_int "composed callback loader called once" 1 !composed_virtual_loads;
  expect_structured_error "composed loader deny"
    (fun () ->
      ignore
        (Qmljs.Module.eval ~filename:"composed-blocked-main.js" context
           {|import blocked from "blocked.js"; export default blocked;|}
          : Qmljs.Module.namespace))
    (fun error ->
      check_bool "composed denied module is policy violation" true
        (Qmljs.Error.is_policy_violation error);
      check_string "composed denied specifier" "blocked.js"
        (Qmljs.Error.denied_module_specifier_exn error));
  Qmljs.Runtime.set_module_loader runtime loader;
  let dynamic_namespace =
    match Qmljs.Promise.await_result
            (Qmljs.Module.dynamic_import ~base:"dynamic-main.js" context "dep.js")
    with
    | Ok namespace -> namespace
    | Error error ->
        fail ("dynamic import rejected: " ^ Qmljs.Error.message error)
  in
  (match Qmljs.Module.get dynamic_namespace ~name:"answer" with
   | Some value ->
       check_int "dynamic import namespace" 42 (Qmljs.Value.to_int value)
   | None -> fail "dynamic import did not resolve to a namespace");
  (match
     Qmljs.Promise.await_result
       (Qmljs.Module.dynamic_import ~base:"dynamic-blocked-main.js" context
          "blocked.js")
   with
   | Ok _ -> fail "dynamic import of denied module resolved"
   | Error error ->
       check_bool "dynamic import deny is policy violation" true
         (Qmljs.Error.is_policy_violation error);
       check_string "dynamic import denied specifier" "blocked.js"
         (Qmljs.Error.denied_module_specifier_exn error));
  expect_structured_error "filesystem loader deny"
    (fun () ->
      ignore
        (Qmljs.Module.eval ~filename:"blocked-main.js" context
           {|import blocked from "blocked.js"; export default blocked;|}
          : Qmljs.Module.namespace))
    (fun error ->
      check_bool "blocked module is policy violation" true
        (Qmljs.Error.is_policy_violation error);
      check_string "blocked module specifier" "blocked.js"
        (Qmljs.Error.denied_module_specifier_exn error));
  expect_structured_error "filesystem loader not found"
    (fun () ->
      ignore
        (Qmljs.Module.eval ~filename:"missing-main.js" context
           {|import missing from "missing.js"; export default missing;|}
          : Qmljs.Module.namespace))
    (fun error ->
      check_kind "missing module is JavaScript failure" Qmljs.Error.JavaScript
        (Qmljs.Error.kind error));
  let defective_loader =
    Qmljs.Module.Loader.memory [ "defect.js", "export default 1;" ]
    |> Qmljs.Module.Loader.deny (fun _specifier ->
        failwith "loader defect")
  in
  Qmljs.Runtime.set_module_loader runtime defective_loader;
  expect_structured_error "filesystem loader defect"
    (fun () ->
      ignore
        (Qmljs.Module.eval ~filename:"defect-main.js" context
           {|import value from "defect.js"; export default value;|}
          : Qmljs.Module.namespace))
    (fun error ->
      check_kind "loader defect kind" Qmljs.Error.Host_callback_defect
        (Qmljs.Error.kind error));
  Qmljs.Runtime.set_module_loader runtime loader;
  expect_structured_error "filesystem loader root escape"
    (fun () ->
      ignore
        (Qmljs.Module.eval ~filename:"pages/escape-main.js" context
           {|import escaped from "../../dep.js"; export default escaped;|}
          : Qmljs.Module.namespace))
    (fun error ->
      check_bool "root escape is policy violation" true
        (Qmljs.Error.is_policy_violation error);
      check_string "root escape specifier" "../../dep.js"
        (Qmljs.Error.denied_module_specifier_exn error));
  expect_structured_error "filesystem loader unknown import attribute"
    (fun () ->
      ignore
        (Qmljs.Module.eval ~filename:"unknown-attribute-main.js" context
           {|import { answer } from "dep.js" with { integrity: "dev" };
             export default answer;|}
          : Qmljs.Module.namespace))
    (fun error ->
      check_kind "unknown import attribute kind" Qmljs.Error.JavaScript
        (Qmljs.Error.kind error))

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
  ; "value type inspection API", test_value_type_inspection_api
  ; "value equality API", test_value_equality_api
  ; "value conversion helpers API", test_value_conversion_helpers_api
  ; "json codec API", test_json_codec_api
  ; "object string-key convenience API", test_object_string_key_convenience_api
  ; "atom property-key API", test_atom_property_key_api
  ; "closed atom is rejected", test_closed_atom_is_rejected
  ; "object prototype API", test_object_prototype_api
  ; "object property descriptor API", test_object_property_descriptor_api
  ; "object own-key iteration API", test_object_own_key_iteration_api
  ; "proxy API", test_proxy_api
  ; "function call constructor and metadata API"
  , test_function_call_constructor_and_metadata_api
  ; "async host callback rejection is structured"
  , test_async_host_callback_rejection_is_structured
  ; "async host callback foreign return rejects structured"
  , test_async_host_callback_foreign_return_rejects_structured
  ; "function defer API", test_function_defer_api
  ; "host class API", test_host_class_api
  ; "iterable API", test_iterable_api
  ; "typed array readback API", test_typed_array_readback_api
  ; "array buffer API", test_array_buffer_api
  ; "uint8 array buffer interop API", test_uint8_array_buffer_interop_api
  ; "typed array family API", test_typed_array_family_api
  ; "array API", test_array_api
  ; "bigint readback API", test_bigint_readback_api
  ; "native module API", test_native_module_api
  ; "module loader filesystem API", test_module_loader_filesystem_api
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
      Printf.eprintf "%d/%d qmljs gap specs are red\n%!"
        (List.length failures)
        (List.length tests);
      exit 1
