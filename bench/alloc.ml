let iterations = 1_000_000

let allocated_words fn =
  Gc.compact ();
  let before = Gc.quick_stat () in
  let result = fn () in
  let after = Gc.quick_stat () in
  ( result
  , after.Gc.minor_words +. after.Gc.major_words
    -. before.Gc.minor_words -. before.Gc.major_words )

let print_words label words =
  Printf.printf "%s: %.6f words/call\n" label (words /. float iterations)

let () =
  let runtime = Qmljs.Runtime.create () in
  let context = Qmljs.Context.create runtime in
  let value = Qmljs.Value.int context 42 in
  let promise = Qmljs.Context.eval context "Promise.resolve(42)" in

  let _, words =
    allocated_words @@ fun () ->
    for _ = 1 to iterations do
      ignore (Qmljs.Runtime.interrupt_requested runtime : bool)
    done
  in
  print_words "Runtime.interrupt_requested" words;

  let _, words =
    allocated_words @@ fun () ->
    for _ = 1 to iterations do
      ignore (Qmljs.Runtime.is_job_pending runtime : bool)
    done
  in
  print_words "Runtime.is_job_pending" words;

  let _, words =
    allocated_words @@ fun () ->
    for _ = 1 to iterations do
      ignore (Qmljs.Value.to_int value : int)
    done
  in
  print_words "Value.to_int" words;

  let _, words =
    allocated_words @@ fun () ->
    for _ = 1 to iterations do
      ignore (Qmljs.Promise.state promise : Qmljs.Promise.state)
    done
  in
  print_words "Promise.state" words
