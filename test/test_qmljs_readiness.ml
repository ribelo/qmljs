[@@@alert "-do_not_spawn_domains"]
[@@@alert "-unsafe_multidomain"]

let fail message = raise (Failure message)

(* Runs [f] on the Async scheduler and returns its value, re-raising the
   original exception so structured qmljs errors keep their classification. *)
let run_async f =
  match
    Async.Thread_safe.block_on_async (fun () ->
      Async.Monitor.try_with ~extract_exn:true f)
  with
  | Ok (Ok value) -> value
  | Ok (Error exn) | Error exn -> raise exn

let check_int label expected actual =
  if actual <> expected then
    fail (Printf.sprintf "%s: expected %d, got %d" label expected actual)

let check_int64 label expected actual =
  if Int64.compare expected actual <> 0 then
    fail (Printf.sprintf "%s: expected %Ld, got %Ld" label expected actual)

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

external test_runtime_close : Qmljs.Runtime.t -> bool = "qmljs_test_runtime_close"

external test_context_close : Qmljs.Context.t -> bool = "qmljs_test_context_close"

external test_drain_deferred_runtimes : unit -> unit
  = "qmljs_runtime_drain_deferred_runtimes"

let force_native_finalizers () =
  for _ = 1 to 4 do
    Gc.full_major ();
    test_drain_deferred_runtimes ()
  done

external test_set_wrapper_failpoint : int -> unit
  = "qmljs_test_set_wrapper_failpoint"

let test_acquired_resources_survive_allocator_failure () =
  force_native_finalizers ();
  let before_wrappers = Qmljs.Diagnostics.snapshot () in
  (* These sites are immediately before custom-block/handle allocation, before
     native ownership is registered. Native heap counts detect leaks that the
     wrapper counters alone cannot see. A selector isolates each red in its
     own process: a leaked raw context cannot safely close its runtime. *)
  let test_site site =
    force_native_finalizers ();
    if site = 5 then (
      let before = Qmljs.Diagnostics.snapshot () in
      Fun.protect ~finally:(fun () -> test_set_wrapper_failpoint 0) (fun () ->
        test_set_wrapper_failpoint site;
        (try
           ignore (Qmljs.Runtime.create () : Qmljs.Runtime.t);
           fail "runtime custom allocator: expected Out_of_memory"
         with Stdlib.Out_of_memory -> ()));
      force_native_finalizers ();
      let after = Qmljs.Diagnostics.snapshot () in
      check_int "runtime custom allocator native runtime leak"
        (Qmljs.Diagnostics.native_runtimes before)
        (Qmljs.Diagnostics.native_runtimes after))
    else (
      let runtime = Qmljs.Runtime.create () in
      let context = Qmljs.Context.create runtime in
      Qmljs.Runtime.run_gc runtime;
      let before = Qmljs.Runtime.memory_usage runtime in
      Fun.protect ~finally:(fun () -> test_set_wrapper_failpoint 0) (fun () ->
        test_set_wrapper_failpoint site;
        (try
           (match site with
            | 6 | 7 -> ignore (Qmljs.Context.create runtime : Qmljs.Context.t)
            | 8 | 9 ->
                ignore (Qmljs.Value.string context "allocator-resource" : Qmljs.Value.t)
            | 10 | 11 ->
                ignore (Qmljs.Atom.of_string context "allocator-resource" : Qmljs.Atom.t)
            | _ -> fail "unknown allocator test site");
           fail "native acquisition: expected Out_of_memory"
         with Stdlib.Out_of_memory -> ()));
      force_native_finalizers ();
      Qmljs.Runtime.run_gc runtime;
      let after = Qmljs.Runtime.memory_usage runtime in
      let label kind = Printf.sprintf "allocator site %d %s leak" site kind in
      check_int64 (label "native allocations") before.malloc_count after.malloc_count;
      check_int64 (label "objects") before.obj_count after.obj_count;
      check_int64 (label "strings") before.str_count after.str_count;
      check_int64 (label "atoms") before.atom_count after.atom_count;
      let result = Qmljs.Context.eval context "21 * 2" in
      check_int (label "recovery") 42 (Qmljs.Value.to_int result);
      check_bool "context closes after allocator failure" true
        (test_context_close context);
      check_bool "runtime closes after allocator failure" true
        (test_runtime_close runtime))
  in
  (match Sys.getenv_opt "QMLJS_OWNERSHIP_FAILPOINT_SITE" with
  | Some site -> test_site (int_of_string site)
  | None -> List.iter test_site [ 5; 6; 7; 8; 9; 10; 11 ]);
  force_native_finalizers ();
  Qmljs.Diagnostics.assert_no_leaks ~before:before_wrappers
    ~after:(Qmljs.Diagnostics.snapshot ())

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

let test_close_races_gc_finalizers_without_uaf () =
  let before = Qmljs.Diagnostics.snapshot () in
  let clear_on_other_domain values =
    Domain.spawn (fun () ->
      Array.iteri
        (fun i _value ->
          values.(i) <- None;
          if i mod 32 = 0 then Gc.full_major ())
        values;
      force_native_finalizers ())
  in
  let context_close_round round =
    let runtime = Qmljs.Runtime.create () in
    let context = Qmljs.Context.create runtime in
    let values =
      Array.init 512 (fun i ->
        Some
          (if i mod 2 = 0 then Qmljs.Value.int context i
           else
             Qmljs.Value.string context
               ("context-race-" ^ string_of_int round ^ "-" ^ string_of_int i)))
    in
    let finalizer = clear_on_other_domain values in
    for _ = 1 to 2_000 do
      Domain.cpu_relax ()
    done;
    ignore (test_context_close context : bool);
    Domain.join finalizer;
    ignore (test_runtime_close runtime : bool)
  in
  let runtime_close_round round =
    let runtime = Qmljs.Runtime.create () in
    let contexts =
      Array.init 16 (fun _i -> Some (Qmljs.Context.create runtime))
    in
    let root =
      match contexts.(0) with
      | Some context -> context
      | None -> fail "runtime close race lost root context"
    in
    let values =
      Array.init 512 (fun i ->
        Some
          (Qmljs.Value.string root
             ("runtime-race-" ^ string_of_int round ^ "-" ^ string_of_int i)))
    in
    let atoms =
      Array.init 512 (fun i ->
        Some
          (Qmljs.Atom.of_string root
             ("runtime-race-atom-" ^ string_of_int round ^ "-"
              ^ string_of_int i)))
    in
    let value_finalizer = clear_on_other_domain values in
    let atom_finalizer = clear_on_other_domain atoms in
    let context_finalizer = clear_on_other_domain contexts in
    for _ = 1 to 2_000 do
      Domain.cpu_relax ()
    done;
    ignore (test_runtime_close runtime : bool);
    Domain.join value_finalizer;
    Domain.join atom_finalizer;
    Domain.join context_finalizer
  in
  for round = 1 to 24 do
    context_close_round round;
    runtime_close_round round;
    force_native_finalizers ()
  done;
  force_native_finalizers ();
  let after = Qmljs.Diagnostics.snapshot () in
  Qmljs.Diagnostics.assert_no_leaks ~before ~after

let test_gc_finalizers_do_not_call_quickjs_reentrantly () =
  let before = Qmljs.Diagnostics.snapshot () in
  let finalizers_during_host_callback = ref 0 in
  let in_host_callback = ref false in
  (Qmljs.Runtime.with_ @@ fun runtime ->
   Qmljs.Context.with_ runtime @@ fun context ->
   let instance_class =
     Qmljs.Class.create context ~name:"GcFinalizerProbe"
       ~finalize:(fun _payload ->
         if !in_host_callback then incr finalizers_during_host_callback)
   in
   let return_value = Qmljs.Value.int context 7 in
   let allocate_short_lived_values () =
     for i = 1 to 2_000 do
       ignore (Qmljs.Class.instance instance_class i : Qmljs.value);
       ignore (Qmljs.Value.string context (string_of_int i) : Qmljs.value);
       ignore
         (Qmljs.Atom.of_string context ("atom-" ^ string_of_int i)
          : Qmljs.atom)
     done
   in
   Qmljs.Function.set_global context ~name:"stress_gc" ~arity:0 (fun _args ->
     Fun.protect
       ~finally:(fun () -> in_host_callback := false)
       (fun () ->
         in_host_callback := true;
         allocate_short_lived_values ();
         Gc.full_major ();
         Gc.full_major ();
         ignore (Qmljs.Value.int context 11 : Qmljs.value);
         return_value));
   let result =
     Qmljs.Context.eval context
       "let total = 0; for (let i = 0; i < 100; i++) total += stress_gc(); total"
   in
   check_int "stress loop result" 700 (Qmljs.Value.to_int result);
   check_int "QuickJS finalizer ran while QuickJS was reentrant" 0
     !finalizers_during_host_callback;
   ignore (Qmljs.Context.eval context "undefined" : Qmljs.value));
  let after = Qmljs.Diagnostics.snapshot () in
  Qmljs.Diagnostics.assert_no_leaks ~before ~after

let test_class_finalizer_cannot_reenter_quickjs () =
  let before = Qmljs.Diagnostics.snapshot () in
  (Qmljs.Runtime.with_ @@ fun runtime ->
   Qmljs.Context.with_ runtime @@ fun context ->
   let finalized = ref 0 in
   let instance_class =
     Qmljs.Class.create context ~name:"ReentrantFinalizer"
       ~finalize:(fun () ->
         let value = Qmljs.Context.eval context "1 + 1" in
         finalized := !finalized + Qmljs.Value.to_int value)
   in
   let make_instance () =
     ignore (Qmljs.Class.instance instance_class () : Qmljs.value)
   in
   for _ = 1 to 100 do
     make_instance ()
   done;
   Gc.full_major ();
   Gc.full_major ();
   Qmljs.Runtime.run_gc runtime;
   let value = Qmljs.Context.eval context "2 + 2" in
   check_int "runtime reusable after class finalizer" 4
     (Qmljs.Value.to_int value);
   check_int "class finalizer reentrant evals" 200 !finalized);
  let after = Qmljs.Diagnostics.snapshot () in
  Qmljs.Diagnostics.assert_no_leaks ~before ~after

let test_class_finalizer_runs_during_context_close () =
  let context_finalized = ref 0 in
  let context_instance = ref None in
  (Qmljs.Runtime.with_ @@ fun runtime ->
   Qmljs.Context.with_ runtime (fun context ->
     let instance_class =
       Qmljs.Class.create context ~name:"ContextCloseFinalizer"
         ~finalize:(fun n -> context_finalized := !context_finalized + n)
     in
     context_instance := Some (Qmljs.Class.instance instance_class 7);
     check_int "context-close finalizer does not run early" 0
       !context_finalized);
   check_int "context close drains class finalizer" 7 !context_finalized);
  context_instance := None;
  let runtime_finalized = ref 0 in
  let runtime_instance = ref None in
  let runtime = Qmljs.Runtime.create () in
  let context = Qmljs.Context.create runtime in
  let instance_class =
    Qmljs.Class.create context ~name:"RuntimeCloseFinalizer"
      ~finalize:(fun n -> runtime_finalized := !runtime_finalized + n)
  in
  runtime_instance := Some (Qmljs.Class.instance instance_class 11);
  check_int "runtime-close finalizer does not run early" 0 !runtime_finalized;
  ignore (test_runtime_close runtime : bool);
  check_int "runtime close drains class finalizer" 11 !runtime_finalized;
  runtime_instance := None

let test_rejected_module_eval_leaks () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  let source =
    "export default 0; throw new Error('module boom');"
  in
  let eval_rejected_module () =
    Qmljs.Context.with_ runtime @@ fun context ->
    expect_error "rejected module evaluation"
      (fun () ->
        ignore
          (run_async (fun () ->
             Qmljs.Context.eval_module ~filename:"rejected-module-leak.mjs"
               context source)
           : Qmljs.value))
      (fun error ->
        check_string "rejected module message" "module boom"
          (Qmljs.Error.message error))
  in
  eval_rejected_module ();
  Qmljs.Runtime.run_gc runtime;
  let before = Qmljs.Runtime.memory_usage runtime in
  for _ = 1 to 100 do
    eval_rejected_module ()
  done;
  Qmljs.Runtime.run_gc runtime;
  let after = Qmljs.Runtime.memory_usage runtime in
  check_int64 "rejected module eval object count" before.obj_count
    after.obj_count;
  check_int64 "rejected module eval memory-used count"
    before.memory_used_count after.memory_used_count

let test_rejected_module_eval_frees_eval_result () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  let source =
    {|await Promise.reject(new Error("boom"));
      export default 1;|}
  in
  let expect_rejection label eval =
    Qmljs.Context.with_ runtime @@ fun context ->
    expect_error label
      (fun () -> eval context)
      (fun error ->
        check_string (label ^ " message") "boom" (Qmljs.Error.message error))
  in
  let eval_all () =
    expect_rejection "Context.eval_module rejected TLA" (fun context ->
      ignore
        (run_async (fun () ->
           Qmljs.Context.eval_module ~filename:"rejected-tla-context.mjs"
             context source)
         : Qmljs.value));
    expect_rejection "Module.eval rejected TLA" (fun context ->
      let namespace =
        run_async (fun () ->
          Qmljs.Module.eval ~filename:"rejected-tla-module.mjs" context source)
      in
      ignore (Qmljs.Module.default namespace : Qmljs.value));
    expect_rejection "Module.eval_with_meta rejected TLA" (fun context ->
      let evaluated =
        run_async (fun () ->
          Qmljs.Module.eval_with_meta ~filename:"rejected-tla-meta.mjs"
            context source)
      in
      ignore (Qmljs.Module.default evaluated.namespace : Qmljs.value))
  in
  eval_all ();
  Qmljs.Runtime.run_gc runtime;
  let before = Qmljs.Runtime.memory_usage runtime in
  for _ = 1 to 100 do
    eval_all ()
  done;
  Qmljs.Runtime.run_gc runtime;
  let after = Qmljs.Runtime.memory_usage runtime in
  check_int64 "rejected module eval_result object count" before.obj_count
    after.obj_count;
  check_int64 "rejected module eval_result memory-used count"
    before.memory_used_count after.memory_used_count

let test_runtime_finalizer_from_dead_owner_domain_does_not_leak_native_runtime () =
  let before = Qmljs.Diagnostics.snapshot () in
  for i = 1 to 16 do
    let domain =
      Domain.spawn (fun () ->
        let runtime = Qmljs.Runtime.create () in
        let context = Qmljs.Context.create runtime in
        ignore
          (Qmljs.Value.string context ("dead-owner-" ^ string_of_int i)
           : Qmljs.value))
    in
    Domain.join domain;
    Gc.full_major ();
    Gc.full_major ()
  done;
  let after = Qmljs.Diagnostics.snapshot () in
  check_int "native runtimes after dead owner domains"
    (Qmljs.Diagnostics.native_runtimes before)
    (Qmljs.Diagnostics.native_runtimes after);
  Qmljs.Diagnostics.assert_no_leaks ~before ~after

let test_deferred_runtime_after_owner_domain_exit_is_not_leaked () =
  let before = Qmljs.Diagnostics.snapshot () in
  let survivor = ref None in
  let domain =
    Domain.spawn (fun () ->
        let runtime = Qmljs.Runtime.create () in
        let context = Qmljs.Context.create runtime in
        Qmljs.Value.string context "owner-domain-survivor")
  in
  survivor := Some (Domain.join domain);
  survivor := None;
  for _ = 1 to 8 do
    Gc.full_major ()
  done;
  let after = Qmljs.Diagnostics.snapshot () in
  check_int "native runtimes after owner-domain survivor"
    (Qmljs.Diagnostics.native_runtimes before)
    (Qmljs.Diagnostics.native_runtimes after);
  Qmljs.Diagnostics.assert_no_leaks ~before ~after

let test_cross_domain_runtime_close_rejected () =
  let runtime = Qmljs.Runtime.create () in
  let result =
    Domain.spawn (fun () ->
      Qmljs.Error.capture (fun () -> ignore (test_runtime_close runtime : bool)))
    |> Domain.join
  in
  (match result with
   | Ok () -> fail "cross-domain runtime close: expected Wrong_domain"
   | Error error ->
       check_bool "cross-domain runtime close kind" true
         (Qmljs.Error.kind error = Qmljs.Error.Wrong_domain));
  ignore (test_runtime_close runtime : bool)

let test_wrong_context_call_arguments_do_not_leak_native_argv () =
  let before = Qmljs.Diagnostics.snapshot () in
  (Qmljs.Runtime.with_ @@ fun runtime ->
   let left = Qmljs.Context.create runtime in
   let right = Qmljs.Context.create runtime in
   let foreign = Qmljs.Value.int right 41 in
   let fn = Qmljs.Context.eval left "(function Box(x) { this.x = x; return x + 1; })" in
   let expect_wrong_context label fn =
     expect_error label fn (fun error ->
       check_bool (label ^ " kind") true
         (Qmljs.Error.kind error = Qmljs.Error.Wrong_domain);
       check_string (label ^ " message")
         "QuickJS value belongs to a different context"
         (Qmljs.Error.message error))
   in
   for _ = 1 to 128 do
     expect_wrong_context "Function.call foreign arg" (fun () ->
       ignore (Qmljs.Function.call fn [| foreign |] : Qmljs.value));
     expect_wrong_context "Function.construct foreign arg" (fun () ->
       ignore (Qmljs.Function.construct fn [| foreign |] : Qmljs.value));
     expect_wrong_context "Function.defer foreign arg" (fun () ->
       Qmljs.Function.defer fn [| foreign |]);
     expect_wrong_context "Value.call foreign arg" (fun () ->
       ignore (Qmljs.Value.call fn [| foreign |] : Qmljs.value))
   done);
  let after = Qmljs.Diagnostics.snapshot () in
  Qmljs.Diagnostics.assert_no_leaks ~before ~after

let test_array_of_values_wrong_context_does_not_leak_partial_array () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  let left = Qmljs.Context.create runtime in
  let right = Qmljs.Context.create runtime in
  let local = Qmljs.Value.int left 1 in
  let foreign = Qmljs.Value.int right 2 in
  let expect_wrong_context () =
    expect_error "Array.of_array foreign element"
      (fun () ->
        ignore (Qmljs.Array.of_array left [| local; foreign |] : Qmljs.value))
      (fun error ->
        check_bool "Array.of_array foreign element kind" true
          (Qmljs.Error.kind error = Qmljs.Error.Wrong_domain);
        check_string "Array.of_array foreign element message"
          "QuickJS value belongs to a different context"
          (Qmljs.Error.message error))
  in
  Qmljs.Runtime.run_gc runtime;
  let before = Qmljs.Runtime.memory_usage runtime in
  for _ = 1 to 256 do
    expect_wrong_context ()
  done;
  Qmljs.Runtime.run_gc runtime;
  let after = Qmljs.Runtime.memory_usage runtime in
  check_int64 "Array.of_array object count" before.obj_count after.obj_count;
  check_int64 "Array.of_array array count" before.array_count after.array_count;
  check_int64 "Array.of_array memory-used count" before.memory_used_count
    after.memory_used_count

let test_context_intrinsic_oom_preserves_native_exception () =
  (* Find a real heap budget that admits a base-only context. At the first
     4 KiB step that succeeds, TypedArrays needs more room than remains. This
     exercises JS_AddIntrinsic failure, rather than JS_NewContextRaw failure. *)
  let create ~intrinsics memory_limit =
    Qmljs.Runtime.with_ ~memory_limit (fun runtime ->
      ignore (Qmljs.Context.create_raw ~intrinsics runtime : Qmljs.Context.t))
  in
  let rec find_base_budget bytes =
    if bytes > 128 * 1024 then fail "could not find base context memory budget";
    match Qmljs.Error.capture (fun () -> create ~intrinsics:[] bytes) with
    | Ok () -> bytes
    | Error error when Qmljs.Error.is_out_of_memory error ->
        find_base_budget (bytes + 4 * 1024)
    | Error error -> Qmljs.Error.raise error
  in
  let memory_limit = find_base_budget (16 * 1024) in
  expect_error "Context.create_raw intrinsic OOM"
    (fun () -> create ~intrinsics:[ Qmljs.Context.Typed_arrays ] memory_limit)
    (fun error ->
      check_bool "intrinsic OOM kind" true
        (Qmljs.Error.kind error = Qmljs.Error.Out_of_memory))

let test_context_creation_oom_is_structured () =
  let check_creation label create =
    expect_error label
      (fun () ->
        Qmljs.Runtime.with_ ~memory_limit:1 (fun runtime ->
          ignore (create runtime : Qmljs.Context.t)))
      (fun error ->
        check_bool (label ^ " kind") true
          (Qmljs.Error.kind error = Qmljs.Error.Out_of_memory))
  in
  check_creation "Context.create_raw memory limit" (fun runtime ->
    Qmljs.Context.create_raw runtime);
  check_creation "Context.create memory limit" Qmljs.Context.create

let test_ocaml_exceptions_are_not_javascript_errors () =
  let message = "host failure: closed, timed out, out of memory" in
  let check_host label fn =
    expect_error label fn (fun error ->
      check_bool (label ^ " kind") true
        (Qmljs.Error.kind error = Qmljs.Error.Host_callback_defect);
      check_string (label ^ " message") (Printexc.to_string (Failure message))
        (Qmljs.Error.message error))
  in
  check_host "Error.capture OCaml failure" (fun () -> fail message);
  check_host "Runtime.with_ OCaml body failure" (fun () ->
    Qmljs.Runtime.with_ (fun _runtime -> fail message));
  check_host "Context.with_ OCaml body failure" (fun () ->
    Qmljs.Runtime.with_ (fun runtime ->
      Qmljs.Context.with_ runtime (fun _context -> fail message)));
  (* Explicit legacy binding errors retain their fallback classification. Their
     text is not used to classify arbitrary exceptions from OCaml code. *)
  expect_error "explicit legacy Qmljs.Error"
    (fun () -> raise (Qmljs.Error "legacy JS error"))
    (fun error ->
      check_bool "legacy kind" true
        (Qmljs.Error.kind error = Qmljs.Error.JavaScript);
      check_string "legacy message" "legacy JS error" (Qmljs.Error.message error));
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  expect_error "real JavaScript exception"
    (fun () -> Qmljs.Context.eval context "throw new Error('closed, timed out, out of memory')")
    (fun error ->
      check_bool "JavaScript kind" true
        (Qmljs.Error.kind error = Qmljs.Error.JavaScript));
  Qmljs.Function.set_global context ~name:"host_failure" ~arity:0
    (fun _args -> fail message);
  check_host "real OCaml callback exception" (fun () ->
    Qmljs.Context.eval context "host_failure()")

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

(* Many runtimes, each with its own driver, run concurrently on the one Async
   scheduler; every scope completes its async host work and tears down. *)
let test_many_runtimes_cancel_cleanly_without_races () =
  let results =
    run_async (fun () ->
      Async.Deferred.List.init ~how:`Parallel 128 ~f:(fun index ->
        let i = index + 1 in
        Qmljs.Async.with_runtime (fun _driver _runtime context ->
          Qmljs.Function.set_global_async context ~name:"later" ~arity:1
            (fun args ->
              Async.Deferred.map (Async.Scheduler.yield ()) ~f:(fun () ->
                Qmljs.Value.int context (Qmljs.Value.to_int args.(0) + i)));
          let promise = Qmljs.Context.eval context "later(1).then(x => x + 1)" in
          Async.Deferred.map (Qmljs.Promise.await promise) ~f:Qmljs.Value.to_int)))
  in
  let total = List.fold_left ( + ) 0 results in
  let expected = (128 * 129 / 2) + (128 * 2) in
  check_int "all async runtimes completed" expected total

(* Each await snapshots the driver wakeup before it inspects the promise, so a
   settlement that lands in between is never lost; a lost wakeup would hang
   until the timeout. The bound only detects a hang: every drain runs a full
   major GC, so 1000 awaits legitimately take several seconds. *)
let test_await_result_no_lost_wakeup_with_driver () =
  run_async @@ fun () ->
  Qmljs.Async.with_runtime @@ fun driver _runtime context ->
  let awaits =
    let rec loop i =
      if i > 1_000 then Async.Deferred.unit
      else begin
        let promise =
          Qmljs.Async.promise driver context (fun () ->
            Async.Deferred.bind (Async.Scheduler.yield ()) ~f:(fun () ->
              Async.Deferred.map (Async.Scheduler.yield ()) ~f:(fun () ->
                Qmljs.Value.int context i)))
        in
        let before_await =
          if i mod 3 = 0 then Async.Scheduler.yield () else Async.Deferred.unit
        in
        Async.Deferred.bind before_await ~f:(fun () ->
          Async.Deferred.bind
            (Qmljs.Promise.await_result ~yield_every:1 promise)
            ~f:(function
              | Ok value ->
                  check_int "Async promise result after await" i
                    (Qmljs.Value.to_int value);
                  loop (i + 1)
              | Error error ->
                  fail ("Async promise rejected: " ^ Qmljs.Error.message error)))
      end
    in
    loop 1
  in
  Async.Deferred.map
    (Async.Clock.with_timeout (Core.Time_float.Span.of_sec 60.0) awaits)
    ~f:(function
      | `Result () -> ()
      | `Timeout -> fail "await_result timed out after a driver wakeup")

let test_promise_default_export_awaits_with_driver () =
  run_async @@ fun () ->
  Qmljs.Async.with_runtime @@ fun _driver _runtime context ->
  let check =
    Async.Deferred.bind
      (Qmljs.Module.eval context
         "export default (async () => { return 7; })();")
      ~f:(fun namespace ->
        let value = Qmljs.Module.default namespace in
        check_bool "promise default export" true (Qmljs.Promise.is value);
        Async.Deferred.map (Qmljs.Promise.await ~yield_every:1 value)
          ~f:(fun value ->
            check_int "awaited promise default export" 7
              (Qmljs.Value.to_int value)))
  in
  Async.Deferred.map
    (Async.Clock.with_timeout (Core.Time_float.Span.of_sec 5.0) check)
    ~f:(function
      | `Result () -> ()
      | `Timeout -> fail "promise default export await timed out")

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
    run_async (fun () ->
      Qmljs.Context.eval_module_with_timeout
        ~timeout:0.5 context
        {|
          import { answer } from "math" with { type: "js" };
          export default answer;
        |})
  in
  check_int "module default" 42 (Qmljs.Module.default_export_to_int value);
  expect_error "denied module"
    (fun () ->
      run_async (fun () -> Qmljs.Context.eval_module context {| import "fs"; |}))
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
  expect_error "promise job drain limit"
    (fun () ->
      ignore
        (Qmljs.Context.eval context
           {|
             Promise.resolve().then(function loop() {
               return Promise.resolve().then(loop);
             });
           |}
          : Qmljs.value);
      ignore (run_async (fun () -> Qmljs.Async.drain_jobs runtime) : int))
    (fun error ->
      check_bool "job limit kind" true (Qmljs.Error.is_resource_limit error));
  let value = Qmljs.Context.eval context "6 * 7" in
  check_int "runtime recovers after limits" 42 (Qmljs.Value.to_int value)

let () =
  test_acquired_resources_survive_allocator_failure ();
  test_context_intrinsic_oom_preserves_native_exception ();
  test_ocaml_exceptions_are_not_javascript_errors ();
  test_context_creation_oom_is_structured ();
  test_lifetime_gc_stress_has_no_quickjs_leaks ();
  test_close_races_gc_finalizers_without_uaf ();
  test_gc_finalizers_do_not_call_quickjs_reentrantly ();
  test_class_finalizer_cannot_reenter_quickjs ();
  test_class_finalizer_runs_during_context_close ();
  test_rejected_module_eval_leaks ();
  test_rejected_module_eval_frees_eval_result ();
  test_runtime_finalizer_from_dead_owner_domain_does_not_leak_native_runtime ();
  test_deferred_runtime_after_owner_domain_exit_is_not_leaked ();
  test_cross_domain_runtime_close_rejected ();
  test_wrong_context_call_arguments_do_not_leak_native_argv ();
  test_array_of_values_wrong_context_does_not_leak_partial_array ();
  test_cross_boundary_exceptions_preserve_cause_stack_and_reusability ();
  test_many_runtimes_cancel_cleanly_without_races ();
  test_await_result_no_lost_wakeup_with_driver ();
  test_promise_default_export_awaits_with_driver ();
  test_complete_value_surface_roundtrips_js_semantics ();
  test_module_loader_policy_and_import_attributes_are_enforced ();
  test_resource_limits_reject_hostile_scripts_and_recover ()
