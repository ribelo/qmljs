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

let remove_if_exists path =
  try Sys.remove path with
  | Sys_error _ -> ()

let with_temp_loader_dir fn =
  let root = create_temp_dir () in
  Fun.protect
    ~finally:(fun () ->
      remove_if_exists (Filename.concat root "dep.js");
      remove_if_exists (Filename.concat root "blocked.js");
      try Unix.rmdir root with
      | Unix.Unix_error _ -> ())
    (fun () -> fn root)

let test_module_loader_filesystem_api () =
  with_temp_loader_dir @@ fun root ->
  write_file (Filename.concat root "dep.js") "export const answer = 42;";
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
        (Qmljs.Error.denied_module_specifier_exn error))

let tests =
  [ "timeout has distinct error kind", test_timeout_has_distinct_error_kind
  ; "host metadata survives JS cause chain"
  , test_host_metadata_survives_js_cause_chain
  ; "await_result rejects non-promise", test_await_result_rejects_non_promise
  ; "value type inspection API", test_value_type_inspection_api
  ; "value equality API", test_value_equality_api
  ; "json codec API", test_json_codec_api
  ; "object string-key convenience API", test_object_string_key_convenience_api
  ; "typed array readback API", test_typed_array_readback_api
  ; "bigint readback API", test_bigint_readback_api
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
