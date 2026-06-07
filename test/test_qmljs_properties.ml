(* Property-based tests using QCheck2 to find failing invariants. *)
[@@@alert "-do_not_spawn_domains"]
[@@@alert "-unsafe_multidomain"]

let max_safe = 9007199254740991

let fail_count = ref 0
let test_count = ref 0

let check_property ~name gen (prop : _ -> bool) =
  incr test_count;
  let cell = QCheck2.Test.make_cell ~name ~count:1000 gen prop in
  let result = QCheck2.Test.check_cell ~rand:(Random.State.make_self_init ()) cell in
  match QCheck2.TestResult.get_state result with
  | QCheck2.TestResult.Success ->
    Printf.eprintf "PASS: %s\n%!" name
  | QCheck2.TestResult.Failed { instances } ->
    incr fail_count;
    Printf.eprintf "RED: %s (%d counterexamples)\n%!" name (List.length instances)
  | QCheck2.TestResult.Failed_other { msg } ->
    incr fail_count;
    Printf.eprintf "RED: %s FAILED_OTHER: %s\n%!" name msg
  | QCheck2.TestResult.Error { instance; exn; backtrace=_ } ->
    incr fail_count;
    Printf.eprintf "RED: %s ERROR: %s\n%!" name (Printexc.to_string exn)

(* PROPERTY 1: Int round-trip within safe range *)
let () =
  let gen = QCheck2.Gen.(int_range (-max_safe) max_safe) in
  let prop n =
    Qmljs.Runtime.with_ @@ fun rt ->
    Qmljs.Context.with_ rt @@ fun ctx ->
    Qmljs.Value.int ctx n |> Qmljs.Value.to_int = n
  in
  check_property ~name:"int_roundtrip_safe_range" gen prop

(* PROPERTY 2: valid UTF-8 strings round-trip exactly.

   A JS string is Unicode text, so [Value.string] accepts valid UTF-8 (and
   rejects anything else, covered in the bug-hunt suite). Over its accepted
   domain the round trip must be the identity, including multi-byte code
   points. *)
let () =
  let uchar =
    QCheck2.Gen.(
      map Uchar.of_int
        (oneof [ int_range 0 0xD7FF; int_range 0xE000 0x10FFFF ]))
  in
  let gen =
    QCheck2.Gen.(
      map
        (fun chars ->
          let buf = Buffer.create 16 in
          List.iter (Buffer.add_utf_8_uchar buf) chars;
          Buffer.contents buf)
        (list_small uchar))
  in
  let prop s =
    Qmljs.Runtime.with_ @@ fun rt ->
    Qmljs.Context.with_ rt @@ fun ctx ->
    Qmljs.Value.string ctx s |> Qmljs.Value.to_string = s
  in
  check_property ~name:"string_roundtrip" gen prop

(* PROPERTY 3: Bool round-trip *)
let () =
  let prop b =
    Qmljs.Runtime.with_ @@ fun rt ->
    Qmljs.Context.with_ rt @@ fun ctx ->
    Qmljs.Value.bool ctx b |> Qmljs.Value.to_bool = b
  in
  check_property ~name:"bool_roundtrip" QCheck2.Gen.bool prop

(* PROPERTY 4: Object set/get round-trip *)
let () =
  let small_int = QCheck2.Gen.(int_range 0 1000) in
  let prop (k, v) =
    Qmljs.Runtime.with_ @@ fun rt ->
    Qmljs.Context.with_ rt @@ fun ctx ->
    let name = Printf.sprintf "key%d" k in
    let obj = Qmljs.Object.create ctx in
    Qmljs.Object.set obj ~name (Qmljs.Value.int ctx v);
    Qmljs.Value.to_int (Qmljs.Object.get obj ~key:(Qmljs.Value.string ctx name)) = v
  in
  check_property ~name:"object_set_get_roundtrip" (QCheck2.Gen.pair small_int small_int) prop

(* PROPERTY 5: to_int after JS identity function preserves value *)
let () =
  let gen = QCheck2.Gen.(int_range (-max_safe) max_safe) in
  let prop n =
    Qmljs.Runtime.with_ @@ fun rt ->
    Qmljs.Context.with_ rt @@ fun ctx ->
    let v = Qmljs.Value.int ctx n in
    Qmljs.Context.set_global ctx ~name:"__id_arg" v;
    let result = Qmljs.Context.eval ctx "(x => x)(__id_arg)" in
    Qmljs.Value.to_int result = n
  in
  check_property ~name:"to_int_after_identity_fn" gen prop

(* PROPERTY 6: strict_equal reflexive for Value.int values *)
let () =
  let gen = QCheck2.Gen.(int_range (-max_safe) max_safe) in
  let prop n =
    Qmljs.Runtime.with_ @@ fun rt ->
    Qmljs.Context.with_ rt @@ fun ctx ->
    let a = Qmljs.Value.int ctx n in
    let b = Qmljs.Value.int ctx n in
    Qmljs.Value.strict_equal a b
  in
  check_property ~name:"strict_equal_reflexive" gen prop

(* PROPERTY 7: to_float exact for safe-range ints *)
let () =
  let gen = QCheck2.Gen.(int_range (-max_safe) max_safe) in
  let prop n =
    Qmljs.Runtime.with_ @@ fun rt ->
    Qmljs.Context.with_ rt @@ fun ctx ->
    match Qmljs.Value.to_float (Qmljs.Value.int ctx n) with
    | f -> Float.is_finite f && Float.equal (float_of_int n) f
    | exception _ -> false
  in
  check_property ~name:"to_float_exact" gen prop

(* PROPERTY 8: Value.kind correct for int *)
let () =
  let gen = QCheck2.Gen.(int_range 0 10000) in
  let prop n =
    Qmljs.Runtime.with_ @@ fun rt ->
    Qmljs.Context.with_ rt @@ fun ctx ->
    Qmljs.Value.kind (Qmljs.Value.int ctx n) = Qmljs.Value.Int
  in
  check_property ~name:"kind_of_int" gen prop

(* PROPERTY 9: BigInt of_string . to_string round-trip *)
let () =
  let gen = QCheck2.Gen.(int_range 0 99999 >|= string_of_int) in
  let prop s =
    Qmljs.Runtime.with_ @@ fun rt ->
    Qmljs.Context.with_ rt @@ fun ctx ->
    match Qmljs.Error.capture (fun () -> Qmljs.Bigint.of_string ctx s) with
    | Ok bi -> Qmljs.Value.to_string bi = s
    | Error _ -> false
  in
  check_property ~name:"bigint_string_roundtrip" gen prop

(* PROPERTY 10: Runtime.with_ + Context.with_ doesn't leak native runtimes *)
let () =
  let prop _n =
    let before = Qmljs.Diagnostics.snapshot () in
    for _i = 1 to 10 do
      Qmljs.Runtime.with_ @@ fun rt ->
      Qmljs.Context.with_ rt @@ fun ctx ->
      ignore (Qmljs.Value.int ctx 42)
    done;
    let after = Qmljs.Diagnostics.snapshot () in
    Qmljs.Diagnostics.native_runtimes before = Qmljs.Diagnostics.native_runtimes after
  in
  check_property ~name:"no_runtime_leak" (QCheck2.Gen.int_range 0 9) prop

(* PROPERTY 11: Array.set creates correct length *)
let () =
  let gen = QCheck2.Gen.(int_range 0 500) in
  let prop i =
    Qmljs.Runtime.with_ @@ fun rt ->
    Qmljs.Context.with_ rt @@ fun ctx ->
    let arr = Qmljs.Array.of_list ctx [] in
    Qmljs.Array.set arr i (Qmljs.Value.int ctx 1);
    Qmljs.Array.length arr = i + 1
  in
  check_property ~name:"array_set_length" gen prop

(* PROPERTY 12: same_value reflexive for int values *)
let () =
  let gen = QCheck2.Gen.(int_range (-max_safe) max_safe) in
  let prop n =
    Qmljs.Runtime.with_ @@ fun rt ->
    Qmljs.Context.with_ rt @@ fun ctx ->
    Qmljs.Value.same_value (Qmljs.Value.int ctx n) (Qmljs.Value.int ctx n)
  in
  check_property ~name:"same_value_reflexive" gen prop

(* PROPERTY 13: Host function int round-trip *)
let () =
  let gen = QCheck2.Gen.(int_range (-max_safe) max_safe) in
  let prop n =
    Qmljs.Runtime.with_ @@ fun rt ->
    Qmljs.Context.with_ rt @@ fun ctx ->
    Qmljs.Function.set_global ctx ~name:"__return" ~arity:1
      (fun args -> Qmljs.Value.int ctx (Qmljs.Value.to_int args.(0)));
    let result = Qmljs.Context.eval ctx (Printf.sprintf "__return(%d)" n) in
    Qmljs.Value.to_int result = n
  in
  check_property ~name:"host_fn_int_roundtrip" gen prop

(* PROPERTY 14: Object.has is own-property only.

   A prototype object carries [inherited]; the child carries [own].
   [has] must be true for the child's own key and false for the inherited
   key, otherwise it is walking the prototype chain. *)
let () =
  let prop (own_key, inherited_key) =
    Qmljs.Runtime.with_ @@ fun rt ->
    Qmljs.Context.with_ rt @@ fun ctx ->
    let proto = Qmljs.Object.create ctx in
    Qmljs.Object.set proto ~name:inherited_key (Qmljs.Value.int ctx 1);
    let obj = Qmljs.Object.create ~prototype:(Qmljs.Value.object_ proto) ctx in
    Qmljs.Object.set obj ~name:own_key (Qmljs.Value.int ctx 2);
    Qmljs.Object.has obj ~key:(Qmljs.Value.string ctx own_key)
    && not (Qmljs.Object.has obj ~key:(Qmljs.Value.string ctx inherited_key))
  in
  let name_gen prefix =
    QCheck2.Gen.(map (fun k -> Printf.sprintf "%s%d" prefix k) (int_range 0 1000))
  in
  check_property ~name:"has_own_property_only"
    (QCheck2.Gen.pair (name_gen "own") (name_gen "inh")) prop

let () =
  Printf.eprintf "\n=== RESULTS: %d/%d properties passed ===\n%!"
    (!test_count - !fail_count) !test_count;
  if !fail_count > 0 then exit 1
