open Qmljs_test_common

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

let tests =
  [ "function call constructor and metadata API"
  , test_function_call_constructor_and_metadata_api
  ; "async host callback rejection is structured"
  , test_async_host_callback_rejection_is_structured
  ; "async host callback foreign return rejects structured"
  , test_async_host_callback_foreign_return_rejects_structured
  ; "function defer API", test_function_defer_api
  ; "host class API", test_host_class_api
  ; "iterable API", test_iterable_api
  ]
