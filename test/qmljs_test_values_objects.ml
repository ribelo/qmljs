open Qmljs_test_common

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
  let spaced = Qmljs.Json.stringify ~space:max_int parsed in
  let expected_spaced =
    Qmljs.Value.to_string
      (Qmljs.Context.eval context
         "JSON.stringify({ answer: 42 }, null, Number.MAX_SAFE_INTEGER)")
  in
  check_string "json stringify large space follows JS clamping" expected_spaced
    spaced;
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

let test_descriptor_preserves_undefined_data_value () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let object_ = Qmljs.Object.create context in
  let undefined = Qmljs.Value.undefined context in
  let check_descriptor label (descriptor : Qmljs.Object.descriptor option) =
    match descriptor with
    | Some descriptor ->
        (match descriptor.value with
         | Some value ->
             check_bool (label ^ " value is undefined") true
               (Qmljs.Value.is_undefined value)
         | None -> fail (label ^ " data descriptor lost undefined value"));
        check_bool (label ^ " writable") true
          (descriptor.writable = Some true);
        check_bool (label ^ " has no getter") true
          (Option.is_none descriptor.get);
        check_bool (label ^ " has no setter") true
          (Option.is_none descriptor.set)
    | None -> fail (label ^ " descriptor missing")
  in
  let key = Qmljs.Value.string context "definedUndefined" in
  Qmljs.Object.define_data_property object_ ~key ~value:undefined
    ~writable:true ~enumerable:true ~configurable:true;
  check_descriptor "string-key undefined descriptor"
    (Qmljs.Object.get_own_property_descriptor object_ ~key);
  let atom = Qmljs.Atom.of_string context "atomDefinedUndefined" in
  Qmljs.Object.define_data_property_atom object_ ~key:atom ~value:undefined
    ~writable:true ~enumerable:true ~configurable:true;
  check_descriptor "atom-key undefined descriptor"
    (Qmljs.Object.get_own_property_descriptor_atom object_ ~key:atom)

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
    "hidden,visible"
    (String.concat "," (sorted_strings (Qmljs.Object.own_keys object_)));
  check_string "enumerable-only string own keys"
    "visible"
    (String.concat ","
       (sorted_strings
          (Qmljs.Object.own_keys ~enumerable_only:true object_)));
  let object_keys = Qmljs.Object.keys object_ |> List.sort String.compare in
  if object_keys <> [ "visible" ] then
    fail
      ("Object.keys returned wrong keys: "
       ^ String.concat "," object_keys);
  let enum_symbols =
    Qmljs.Object.own_keys ~strings:false ~symbols:true ~enumerable_only:true
      object_
  in
  check_int "enumerable symbol count" 1 (List.length enum_symbols);
  check_bool "enumerable symbol key matches" true
    (Qmljs.Value.strict_equal (List.hd enum_symbols) (Qmljs.Key.symbol symbol));
  check_int "all string and symbol own keys" 4
    (List.length (Qmljs.Object.own_keys ~symbols:true object_));
  let entry_values =
    Qmljs.Object.entries object_
    |> List.map (fun (_key, value) -> Qmljs.Value.to_int value)
    |> List.sort compare
  in
  if entry_values <> [ 1; 2 ] then fail "default entries returned wrong values";
  let values =
    Qmljs.Object.values ~symbols:true ~enumerable_only:true object_
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

let tests =
  [ "value type inspection API", test_value_type_inspection_api
  ; "value equality API", test_value_equality_api
  ; "value conversion helpers API", test_value_conversion_helpers_api
  ; "json codec API", test_json_codec_api
  ; "object string-key convenience API", test_object_string_key_convenience_api
  ; "atom property-key API", test_atom_property_key_api
  ; "closed atom is rejected", test_closed_atom_is_rejected
  ; "object prototype API", test_object_prototype_api
  ; "object property descriptor API", test_object_property_descriptor_api
  ; "descriptor preserves undefined data value"
  , test_descriptor_preserves_undefined_data_value
  ; "object own-key iteration API", test_object_own_key_iteration_api
  ; "proxy API", test_proxy_api
  ]
