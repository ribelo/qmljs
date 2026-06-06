let fail message = raise (Failure message)

let check_bool label expected actual =
  if expected <> actual then
    fail (Printf.sprintf "%s: expected %b, got %b" label expected actual)

let check_kind label expected actual =
  if expected <> actual then fail (label ^ ": unexpected error kind")

let expect_structured_error label fn assert_error =
  match Qmljs.Error.capture fn with
  | Ok _ -> fail (label ^ ": expected structured error")
  | Error error -> assert_error error

let missing_api name signature =
  fail
    (Printf.sprintf "%s API is missing or not convenient enough:\n%s" name
       signature)

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
  missing_api "value type inspection"
    {|
      module Qmljs.Value : sig
        type kind =
          | Undefined
          | Null
          | Bool
          | Int
          | Float
          | String
          | Symbol
          | Bigint
          | Object
          | Array
          | Function
          | Promise
          | Typed_array

        val kind : value -> kind
        val is_null : value -> bool
        val is_undefined : value -> bool
        val is_function : value -> bool
        val is_array : value -> bool
      end
    |}

let test_value_equality_api () =
  missing_api "value equality"
    {|
      module Qmljs.Value : sig
        val strict_equal : value -> value -> bool
        val same_value : value -> value -> bool
        val same_value_zero : value -> value -> bool
      end
    |}

let test_json_codec_api () =
  missing_api "json codec"
    {|
      module Qmljs.Json : sig
        val parse : Context.t -> string -> value
        val stringify : ?space:int -> value -> string
        val of_string : Context.t -> string -> (value, Qmljs.Error.t) result
        val to_string : ?space:int -> value -> (string, Qmljs.Error.t) result
      end
    |}

let test_object_string_key_convenience_api () =
  missing_api "object string-key convenience"
    {|
      module Qmljs.Object : sig
        val get_string : t -> string -> value option
        val set_string : t -> string -> value -> unit
        val delete_string : t -> string -> bool
        val keys : t -> string list
      end
    |}

let test_typed_array_readback_api () =
  missing_api "typed array readback"
    {|
      module Qmljs.Typed_array.Uint8Array : sig
        val length : t -> int
        val get : t -> int -> int
        val set : t -> int -> int -> unit
        val to_bytes_copy : t -> bytes
      end
    |}

let test_bigint_readback_api () =
  missing_api "bigint readback"
    {|
      module Qmljs.Bigint : sig
        val to_string : t -> string
        val to_int64 : t -> int64 option
        val of_int64 : Context.t -> int64 -> t
      end
    |}

let test_module_loader_filesystem_api () =
  missing_api "module loader filesystem and normalization"
    {|
      module Qmljs.Module.Loader : sig
        val filesystem :
          root:Fpath.t ->
          allow:(specifier:string -> bool) ->
          t

        val normalize :
          (base:string option -> specifier:string -> (string, Qmljs.Error.t) result) ->
          t ->
          t
      end
    |}

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
