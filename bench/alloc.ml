let iterations = 1_000_000

let allocated_words fn =
  Gc.compact ();
  let before = Gc.quick_stat () in
  let result = fn () in
  let after = Gc.quick_stat () in
  ( result
  , after.Gc.minor_words +. after.Gc.major_words
    -. before.Gc.minor_words -. before.Gc.major_words )

let () =
  let runtime = Qmljs.Runtime.create () in
  let context = Qmljs.Context.create runtime in
  let value = Qmljs.Value.int context 42 in
  let _, words =
    allocated_words @@ fun () ->
    for _ = 1 to iterations do
      ignore (Qmljs.Value.to_int value : int)
    done
  in
  Printf.printf "Value.to_int: %.6f words/call\n" (words /. float iterations)
