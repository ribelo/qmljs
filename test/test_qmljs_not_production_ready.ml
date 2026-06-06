let fail message = raise (Failure message)

let check_int label expected actual =
  if actual <> expected then
    fail (Printf.sprintf "%s: expected %d, got %d" label expected actual)

let check_bool label expected actual =
  if expected <> actual then
    fail (Printf.sprintf "%s: expected %b, got %b" label expected actual)

let check_string label expected actual =
  if not (String.equal expected actual) then
    fail (Printf.sprintf "%s: expected %S, got %S" label expected actual)

let expect_structured_error label fn assert_error =
  match Qmljs.Error.capture fn with
  | Ok _ -> fail (label ^ ": expected structured error")
  | Error error -> assert_error error

let test_sync_host_callback_timeout_is_preemptive () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  Qmljs.Function.set_global context ~name:"block" ~arity:0 (fun _args ->
      Unix.sleepf 0.05;
      Qmljs.Value.int context 1);
  let started = Unix.gettimeofday () in
  expect_structured_error "host callback timeout"
    (fun () ->
      Qmljs.Context.eval_with_timeout ~timeout:0.005 context "block()")
    (fun error ->
      let elapsed = Unix.gettimeofday () -. started in
      check_bool "timeout classified as resource limit" true
        (Qmljs.Error.is_resource_limit error);
      if elapsed > 0.03 then
        fail (Printf.sprintf "timeout was not preemptive: %.3fs" elapsed))

let test_promise_rejection_preserves_js_stack () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let promise =
    Qmljs.Context.eval context
      {|
        function rejector() {
          return Promise.reject(new Error("async boom"));
        }
        rejector();
      |}
  in
  expect_structured_error "promise rejection"
    (fun () -> Qmljs.Promise.await promise)
    (fun error ->
      check_bool "rejection has JS frame" true
        (Qmljs.Error.has_js_frame error ~function_name:"rejector"))

let test_deep_js_cause_chain_is_preserved () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  expect_structured_error "deep cause chain"
    (fun () ->
      Qmljs.Context.eval context
        {|
          let error = new Error("root");
          for (let i = 1; i <= 20; i++) {
            error = new Error("cause-" + i, { cause: error });
          }
          throw error;
        |})
    (fun error ->
      let rec descend n current =
        if n = 0 then current else descend (n - 1) (Qmljs.Error.cause_exn current)
      in
      check_string "deepest cause" "root" (Qmljs.Error.message (descend 20 error)))

let test_same_runtime_cross_context_object_set_is_rejected () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  let left = Qmljs.Context.create runtime in
  let right = Qmljs.Context.create runtime in
  let foreign = Qmljs.Value.int left 42 in
  let object_ = Qmljs.Object.create right in
  expect_structured_error "cross-context object set"
    (fun () -> Qmljs.Object.set object_ ~name:"foreign" foreign)
    (fun _error -> ())

let test_same_runtime_cross_context_function_call_is_rejected () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  let left = Qmljs.Context.create runtime in
  let right = Qmljs.Context.create runtime in
  let foreign = Qmljs.Value.int left 41 in
  let add1 = Qmljs.Context.eval right "(x) => x + 1" in
  expect_structured_error "cross-context function call arg"
    (fun () -> Qmljs.Value.call add1 [| foreign |])
    (fun _error -> ())

let test_json_import_attribute_loads_json_module () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let loader =
    Qmljs.Module.Loader.memory [ "config.json", {|{"answer":42}|} ]
  in
  Qmljs.Runtime.set_module_loader runtime loader;
  let value =
    Qmljs.Context.eval_module context
      {|
        import config from "config.json" with { type: "json" };
        export default config.answer;
      |}
  in
  check_int "json module default" 42 (Qmljs.Module.default_export_to_int value)

let missing_api name signature =
  fail (Printf.sprintf "%s API is not designed/implemented yet:\n%s" name signature)

let test_error_kind_and_metadata_api () =
  missing_api "structured error kind/metadata"
    {|
      module Qmljs.Error : sig
        type kind =
          [ `JavaScript
          | `Host_callback_defect
          | `Policy_violation of { denied_module_specifier : string option }
          | `Out_of_memory
          | `Stack_overflow
          | `Resource_limit
          | `Timeout
          | `Closed_handle
          | `Wrong_domain
          ]

        val kind : t -> kind
        val metadata : t -> (string * string) list
        val cause : t -> t option
      end
    |}

let test_structured_promise_await_api () =
  missing_api "structured promise await"
    {|
      module Qmljs.Promise : sig
        val await_result :
          ?yield_every:int ->
          value ->
          (value, Qmljs.Error.t) result

        val await_exn :
          ?yield_every:int ->
          value ->
          value
      end
    |}

let test_object_introspection_api () =
  missing_api "object introspection"
    {|
      module Qmljs.Object : sig
        type descriptor =
          { value : value option
          ; writable : bool option
          ; enumerable : bool
          ; configurable : bool
          ; get : value option
          ; set : value option
          }

        val get : t -> key:value -> value
        val has : t -> key:value -> bool
        val delete : t -> key:value -> bool
        val get_own_property_descriptor : t -> key:value -> descriptor option
      end
    |}

let test_module_namespace_api () =
  missing_api "module namespace"
    {|
      module Qmljs.Module : sig
        type namespace

        val eval :
          ?filename:string ->
          Qmljs.Context.t ->
          string ->
          namespace

        val default : namespace -> value
        val get : namespace -> name:string -> value option
        val keys : namespace -> string list
      end
    |}

let test_eio_scoped_driver_api () =
  missing_api "Eio scoped driver"
    {|
      module Qmljs.Eio : sig
        type driver

        val with_runtime :
          sw:Eio.Switch.t ->
          clock:_ Eio.Time.clock ->
          ?memory_limit:int ->
          ?max_stack_size:int ->
          ?promise_job_limit:int ->
          (driver -> Qmljs.Runtime.t -> Qmljs.Context.t -> 'a) ->
          'a

        val cancel : driver -> unit
        val drain_until_idle : driver -> int
      end
    |}

let tests =
  [ "sync host callback timeout is preemptive"
  , test_sync_host_callback_timeout_is_preemptive
  ; "promise rejection preserves JS stack"
  , test_promise_rejection_preserves_js_stack
  ; "deep JS cause chain is preserved", test_deep_js_cause_chain_is_preserved
  ; "same-runtime cross-context object set is rejected"
  , test_same_runtime_cross_context_object_set_is_rejected
  ; "same-runtime cross-context function call is rejected"
  , test_same_runtime_cross_context_function_call_is_rejected
  ; "json import attribute loads JSON module"
  , test_json_import_attribute_loads_json_module
  ; "structured error kind and metadata API"
  , test_error_kind_and_metadata_api
  ; "structured promise await API", test_structured_promise_await_api
  ; "object introspection API", test_object_introspection_api
  ; "module namespace API", test_module_namespace_api
  ; "Eio scoped driver API", test_eio_scoped_driver_api
  ]

let () =
  Eio_main.run @@ fun _env ->
  let failures = ref [] in
  List.iter
    (fun (name, test) ->
      match test () with
      | () -> Printf.eprintf "UNEXPECTED PASS: %s\n%!" name
      | exception exn ->
          failures := (name, Printexc.to_string exn) :: !failures;
          Printf.eprintf "RED: %s: %s\n%!" name (Printexc.to_string exn))
    tests;
  match List.rev !failures with
  | [] -> fail "all production-gap tests unexpectedly passed"
  | failures ->
      Printf.eprintf "%d production-readiness gaps still fail as expected\n%!"
        (List.length failures);
      exit 1
