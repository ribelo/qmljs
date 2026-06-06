let fail message = raise (Failure message)

let check_int label expected actual =
  if actual <> expected then
    fail (Printf.sprintf "%s: expected %d, got %d" label expected actual)

let check_string label expected actual =
  if not (String.equal expected actual) then
    fail (Printf.sprintf "%s: expected %S, got %S" label expected actual)

let check_bool label expected actual =
  if expected <> actual then
    fail (Printf.sprintf "%s: expected %b, got %b" label expected actual)

let expect_error label fn assert_error =
  match Qmljs.Error.capture fn with
  | Ok _ -> fail (label ^ ": expected error")
  | Error error -> assert_error error

let test_lifetime_gc_stress_has_no_quickjs_leaks () =
  let before = Qmljs.Diagnostics.snapshot () in
  for i = 1 to 10_000 do
    Qmljs.Runtime.with_ @@ fun runtime ->
    Qmljs.Context.with_ runtime @@ fun context ->
    let object_ = Qmljs.Object.create context in
    Qmljs.Object.set object_ ~name:"self" (Qmljs.Value.object_ object_);
    Qmljs.Object.set object_ ~name:"n" (Qmljs.Value.int context i);
    ignore (Qmljs.Context.eval context "globalThis.tmp = undefined" : Qmljs.value)
  done;
  Gc.full_major ();
  let after = Qmljs.Diagnostics.snapshot () in
  Qmljs.Diagnostics.assert_no_leaks ~before ~after

let test_cross_boundary_exceptions_preserve_cause_stack_and_reusability () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  Qmljs.Function.set_global context ~name:"host_fail" ~arity:1 (fun args ->
      let value = Qmljs.Value.to_int args.(0) in
      raise (Qmljs.Error.host ~message:"host boom" ~metadata:[ "value", string_of_int value ]));
  expect_error "nested JS/OCaml exception"
    (fun () ->
      Qmljs.Context.eval context
        {|
          function entry() {
            try { host_fail(42); }
            catch (e) { throw new Error("js wrapper", { cause: e }); }
          }
          entry();
        |})
    (fun error ->
      check_string "message" "js wrapper" (Qmljs.Error.message error);
      check_string "cause" "host boom"
        (Qmljs.Error.message (Qmljs.Error.cause_exn error));
      check_bool "has JS frame" true
        (Qmljs.Error.has_js_frame error ~function_name:"entry");
      check_bool "has OCaml frame" true
        (Qmljs.Error.has_ocaml_frame error ~function_name:"host_fail"));
  let value = Qmljs.Context.eval context "21 * 2" in
  check_int "runtime reusable after exception" 42 (Qmljs.Value.to_int value)

let test_eio_many_runtimes_cancel_cleanly_without_races () =
  Eio.Switch.run @@ fun sw ->
  let results = Eio.Stream.create 128 in
  for i = 1 to 128 do
    Eio.Fiber.fork ~sw (fun () ->
        Qmljs.Eio.with_runtime (fun _runtime context ->
            Qmljs.Function.set_global_async context ~name:"later" ~arity:1
              (fun args ->
                Eio.Fiber.yield ();
                Qmljs.Value.int context (Qmljs.Value.to_int args.(0) + i));
            let promise =
              Qmljs.Context.eval context "later(1).then(x => x + 1)"
            in
            Eio.Stream.add results
              (Qmljs.Value.to_int (Qmljs.Promise.await promise))))
  done;
  let total = ref 0 in
  for _ = 1 to 128 do
    total := !total + Eio.Stream.take results
  done;
  let expected = (128 * 129 / 2) + (128 * 2) in
  check_int "all async runtimes completed" expected !total

let test_complete_value_surface_roundtrips_js_semantics () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let bytes = Bytes.of_string "\001\002\003\004" in
  let object_ = Qmljs.Object.create context in
  let symbol = Qmljs.Symbol.global context "answer" in
  let array = Qmljs.Array.of_list context [ Qmljs.Value.int context 20; Qmljs.Value.int context 22 ] in
  let bigint = Qmljs.Bigint.of_string context "9007199254740993" in
  let typed = Qmljs.Typed_array.Uint8Array.of_bytes_copy context bytes in
  Qmljs.Object.define_data_property object_ ~key:(Qmljs.Key.symbol symbol)
    ~value:(Qmljs.Value.int context 42)
    ~writable:false ~enumerable:false ~configurable:false;
  Qmljs.Object.set object_ ~name:"array" (Qmljs.Value.array array);
  Qmljs.Object.set object_ ~name:"bigint" (Qmljs.Value.bigint bigint);
  Qmljs.Object.set object_ ~name:"typed" (Qmljs.Value.typed_array typed);
  Qmljs.Context.set_global context ~name:"fixture" (Qmljs.Value.object_ object_);
  let result =
    Qmljs.Context.eval context
      {|
        const d = Object.getOwnPropertyDescriptor(fixture, Symbol.for("answer"));
        [fixture.array[0] + fixture.array[1],
         fixture.bigint.toString(),
         fixture.typed.byteLength,
         d.writable, d.enumerable, d.configurable].join(":")
      |}
  in
  check_string "JS value semantics" "42:9007199254740993:4:false:false:false"
    (Qmljs.Value.to_string result)

let test_module_loader_policy_and_import_attributes_are_enforced () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let loader =
    Qmljs.Module.Loader.memory
      [ ( "math", "export const answer = 42;" )
      ; ( "policy", "export default 'allowed';" )
      ]
    |> Qmljs.Module.Loader.deny (fun specifier -> String.equal specifier "fs")
  in
  Qmljs.Runtime.set_module_loader runtime loader;
  let value =
    Qmljs.Context.eval_module_with_timeout
      ~timeout:0.5 context
      {|
        import { answer } from "math" with { type: "js" };
        export default answer;
      |}
  in
  check_int "module default" 42 (Qmljs.Module.default_export_to_int value);
  expect_error "denied module"
    (fun () -> Qmljs.Context.eval_module context {| import "fs"; |})
    (fun error ->
      check_bool "policy violation" true
        (Qmljs.Error.is_policy_violation error);
      check_string "denied specifier" "fs"
        (Qmljs.Error.denied_module_specifier_exn error))

let test_resource_limits_reject_hostile_scripts_and_recover () =
  Qmljs.Runtime.with_
    ~memory_limit:(16 * 1024 * 1024)
    ~max_stack_size:(256 * 1024)
    ~promise_job_limit:10_000
    @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  expect_error "memory limit"
    (fun () -> Qmljs.Context.eval context "new Array(100_000_000).fill('x')")
    (fun error -> check_bool "memory limit kind" true (Qmljs.Error.is_out_of_memory error));
  expect_error "stack limit"
    (fun () -> Qmljs.Context.eval context "(function f(){ return f(); })()")
    (fun error -> check_bool "stack limit kind" true (Qmljs.Error.is_stack_overflow error));
  expect_error "promise storm limit"
    (fun () ->
      Qmljs.Context.eval_with_timeout
        ~timeout:0.05 context
        "for (;;) Promise.resolve().then(() => {})")
    (fun error ->
      check_bool "deadline or job limit" true (Qmljs.Error.is_resource_limit error));
  let value = Qmljs.Context.eval context "6 * 7" in
  check_int "runtime recovers after limits" 42 (Qmljs.Value.to_int value)

let () =
  Eio_main.run @@ fun _env ->
  test_lifetime_gc_stress_has_no_quickjs_leaks ();
  test_cross_boundary_exceptions_preserve_cause_stack_and_reusability ();
  test_eio_many_runtimes_cancel_cleanly_without_races ();
  test_complete_value_surface_roundtrips_js_semantics ();
  test_module_loader_policy_and_import_attributes_are_enforced ();
  test_resource_limits_reject_hostile_scripts_and_recover ()
