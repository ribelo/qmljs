(* Property-based tests using Base_quickcheck to find failing invariants. *)
[@@@alert "-do_not_spawn_domains"]
[@@@alert "-unsafe_multidomain"]

module Generator = Base_quickcheck.Generator
module Shrinker = Base_quickcheck.Shrinker

let max_safe = 9007199254740991

let fail_count = ref 0
let test_count = ref 0

(* A fresh random seed per run keeps exploring new inputs; a failure prints the
   counterexample so it can be replayed as an example. *)
let config =
  { Base_quickcheck.Test.default_config with
    seed = Nondeterministic
  ; test_count = 1000
  }

let sexp_of_pair sexp_of_a sexp_of_b (a, b) =
  Base.Sexp.List [ sexp_of_a a; sexp_of_b b ]

let check_property (type a) ~name ~(sexp_of : a -> Base.Sexp.t)
    ?(shrinker : a Shrinker.t = Shrinker.atomic) (generator : a Generator.t)
    (prop : a -> bool) =
  incr test_count;
  let module Input = struct
    type t = a

    let sexp_of_t = sexp_of
    let quickcheck_generator = generator
    let quickcheck_shrinker = shrinker
  end
  in
  let holds input =
    match prop input with
    | true -> Ok ()
    | false -> Error "property returned false"
    | exception exn -> Error ("raised " ^ Printexc.to_string exn)
  in
  match Base_quickcheck.Test.result ~config (module Input) ~f:holds with
  | Ok () -> Printf.eprintf "PASS: %s\n%!" name
  | Error (input, reason) ->
      incr fail_count;
      Printf.eprintf "RED: %s on %s: %s\n%!" name
        (Base.Sexp.to_string (sexp_of input))
        reason

let safe_int = Generator.int_inclusive (-max_safe) max_safe

(* PROPERTY 1: Int round-trip within safe range *)
let () =
  let prop n =
    Qmljs.Runtime.with_ @@ fun rt ->
    Qmljs.Context.with_ rt @@ fun ctx ->
    Qmljs.Value.int ctx n |> Qmljs.Value.to_int = n
  in
  check_property ~name:"int_roundtrip_safe_range" ~sexp_of:Base.Int.sexp_of_t
    ~shrinker:Shrinker.int safe_int prop

(* PROPERTY 2: valid UTF-8 strings round-trip exactly.

   A JS string is Unicode text, so [Value.string] accepts valid UTF-8 (and
   rejects anything else, covered in the bug-hunt suite). Over its accepted
   domain the round trip must be the identity, including multi-byte code
   points. *)
let () =
  let uchar =
    Generator.map ~f:Uchar.of_int
      (Generator.union
         [ Generator.int_inclusive 0 0xD7FF
         ; Generator.int_inclusive 0xE000 0x10FFFF
         ])
  in
  let gen =
    Generator.map
      ~f:(fun chars ->
        let buf = Buffer.create 16 in
        List.iter (Buffer.add_utf_8_uchar buf) chars;
        Buffer.contents buf)
      (Generator.list uchar)
  in
  let prop s =
    Qmljs.Runtime.with_ @@ fun rt ->
    Qmljs.Context.with_ rt @@ fun ctx ->
    Qmljs.Value.string ctx s |> Qmljs.Value.to_string = s
  in
  check_property ~name:"string_roundtrip" ~sexp_of:Base.String.sexp_of_t gen
    prop

(* PROPERTY 3: Bool round-trip *)
let () =
  let prop b =
    Qmljs.Runtime.with_ @@ fun rt ->
    Qmljs.Context.with_ rt @@ fun ctx ->
    Qmljs.Value.bool ctx b |> Qmljs.Value.to_bool = b
  in
  check_property ~name:"bool_roundtrip" ~sexp_of:Base.Bool.sexp_of_t
    Generator.bool prop

(* PROPERTY 4: Object set/get round-trip *)
let () =
  let small_int = Generator.int_inclusive 0 1000 in
  let prop (k, v) =
    Qmljs.Runtime.with_ @@ fun rt ->
    Qmljs.Context.with_ rt @@ fun ctx ->
    let name = Printf.sprintf "key%d" k in
    let obj = Qmljs.Object.create ctx in
    Qmljs.Object.set obj ~name (Qmljs.Value.int ctx v);
    Qmljs.Value.to_int (Qmljs.Object.get obj ~key:(Qmljs.Value.string ctx name)) = v
  in
  check_property ~name:"object_set_get_roundtrip"
    ~sexp_of:(sexp_of_pair Base.Int.sexp_of_t Base.Int.sexp_of_t)
    ~shrinker:(Shrinker.both Shrinker.int Shrinker.int)
    (Generator.both small_int small_int) prop

(* PROPERTY 5: to_int after JS identity function preserves value *)
let () =
  let gen = safe_int in
  let prop n =
    Qmljs.Runtime.with_ @@ fun rt ->
    Qmljs.Context.with_ rt @@ fun ctx ->
    let v = Qmljs.Value.int ctx n in
    Qmljs.Context.set_global ctx ~name:"__id_arg" v;
    let result = Qmljs.Context.eval ctx "(x => x)(__id_arg)" in
    Qmljs.Value.to_int result = n
  in
  check_property ~name:"to_int_after_identity_fn" ~sexp_of:Base.Int.sexp_of_t
    ~shrinker:Shrinker.int gen prop

(* PROPERTY 6: strict_equal reflexive for Value.int values *)
let () =
  let gen = safe_int in
  let prop n =
    Qmljs.Runtime.with_ @@ fun rt ->
    Qmljs.Context.with_ rt @@ fun ctx ->
    let a = Qmljs.Value.int ctx n in
    let b = Qmljs.Value.int ctx n in
    Qmljs.Value.strict_equal a b
  in
  check_property ~name:"strict_equal_reflexive" ~sexp_of:Base.Int.sexp_of_t
    ~shrinker:Shrinker.int gen prop

(* PROPERTY 7: to_float exact for safe-range ints *)
let () =
  let gen = safe_int in
  let prop n =
    Qmljs.Runtime.with_ @@ fun rt ->
    Qmljs.Context.with_ rt @@ fun ctx ->
    match Qmljs.Value.to_float (Qmljs.Value.int ctx n) with
    | f -> Float.is_finite f && Float.equal (float_of_int n) f
    | exception _ -> false
  in
  check_property ~name:"to_float_exact" ~sexp_of:Base.Int.sexp_of_t
    ~shrinker:Shrinker.int gen prop

(* PROPERTY 8: Value.kind correct for int *)
let () =
  let gen = Generator.int_inclusive 0 10000 in
  let prop n =
    Qmljs.Runtime.with_ @@ fun rt ->
    Qmljs.Context.with_ rt @@ fun ctx ->
    Qmljs.Value.kind (Qmljs.Value.int ctx n) = Qmljs.Value.Int
  in
  check_property ~name:"kind_of_int" ~sexp_of:Base.Int.sexp_of_t
    ~shrinker:Shrinker.int gen prop

(* PROPERTY 9: BigInt of_string . to_string round-trip *)
let () =
  let gen =
    Generator.map ~f:string_of_int (Generator.int_inclusive 0 99999)
  in
  let prop s =
    Qmljs.Runtime.with_ @@ fun rt ->
    Qmljs.Context.with_ rt @@ fun ctx ->
    match Qmljs.Error.capture (fun () -> Qmljs.Bigint.of_string ctx s) with
    | Ok bi -> Qmljs.Value.to_string bi = s
    | Error _ -> false
  in
  check_property ~name:"bigint_string_roundtrip" ~sexp_of:Base.String.sexp_of_t
    gen prop

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
  check_property ~name:"no_runtime_leak" ~sexp_of:Base.Int.sexp_of_t
    (Generator.int_inclusive 0 9) prop

(* PROPERTY 11: Array.set creates correct length *)
let () =
  let gen = Generator.int_inclusive 0 500 in
  let prop i =
    Qmljs.Runtime.with_ @@ fun rt ->
    Qmljs.Context.with_ rt @@ fun ctx ->
    let arr = Qmljs.Array.of_list ctx [] in
    Qmljs.Array.set arr i (Qmljs.Value.int ctx 1);
    Qmljs.Array.length arr = i + 1
  in
  check_property ~name:"array_set_length" ~sexp_of:Base.Int.sexp_of_t
    ~shrinker:Shrinker.int gen prop

(* PROPERTY 12: same_value reflexive for int values *)
let () =
  let gen = safe_int in
  let prop n =
    Qmljs.Runtime.with_ @@ fun rt ->
    Qmljs.Context.with_ rt @@ fun ctx ->
    Qmljs.Value.same_value (Qmljs.Value.int ctx n) (Qmljs.Value.int ctx n)
  in
  check_property ~name:"same_value_reflexive" ~sexp_of:Base.Int.sexp_of_t
    ~shrinker:Shrinker.int gen prop

(* PROPERTY 13: Host function int round-trip *)
let () =
  let gen = safe_int in
  let prop n =
    Qmljs.Runtime.with_ @@ fun rt ->
    Qmljs.Context.with_ rt @@ fun ctx ->
    Qmljs.Function.set_global ctx ~name:"__return" ~arity:1
      (fun args -> Qmljs.Value.int ctx (Qmljs.Value.to_int args.(0)));
    let result = Qmljs.Context.eval ctx (Printf.sprintf "__return(%d)" n) in
    Qmljs.Value.to_int result = n
  in
  check_property ~name:"host_fn_int_roundtrip" ~sexp_of:Base.Int.sexp_of_t
    ~shrinker:Shrinker.int gen prop

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
    Generator.map ~f:(fun k -> Printf.sprintf "%s%d" prefix k)
      (Generator.int_inclusive 0 1000)
  in
  check_property ~name:"has_own_property_only"
    ~sexp_of:(sexp_of_pair Base.String.sexp_of_t Base.String.sexp_of_t)
    (Generator.both (name_gen "own") (name_gen "inh")) prop

let () =
  Printf.eprintf "\n=== RESULTS: %d/%d properties passed ===\n%!"
    (!test_count - !fail_count) !test_count;
  if !fail_count > 0 then exit 1
