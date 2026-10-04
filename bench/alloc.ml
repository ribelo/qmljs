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

let words_and_seconds fn =
  Gc.compact ();
  let before = Gc.quick_stat () in
  let started = Unix.gettimeofday () in
  fn ();
  let elapsed = Unix.gettimeofday () -. started in
  let after = Gc.quick_stat () in
  ( after.Gc.minor_words +. after.Gc.major_words -. before.Gc.minor_words
    -. before.Gc.major_words
  , elapsed )

(* Wrapper creation also owns a native resource: every call allocates the OCaml
   wrapper plus a native handle, and dropping the wrapper returns the handle
   through the finalizer and the deferred release queue, which the next call
   drains. Reporting both counters keeps the native window bounded and shows
   the whole per-call cost. *)
let print_wrapper_probe label create =
  let before = Qmljs.Diagnostics.snapshot () in
  let words, elapsed =
    words_and_seconds (fun () ->
      for _ = 1 to iterations do
        ignore (create ())
      done)
  in
  Printf.printf "%s: %.6f words/call, %.2f ns/call\n" label
    (words /. float iterations)
    (elapsed *. 1e9 /. float iterations);
  (* Dropping every wrapper must return the live wrapper and native counters to
     the pre-probe baseline, otherwise the loop is only measuring accumulation. *)
  Qmljs.Diagnostics.assert_no_leaks ~before ~after:(Qmljs.Diagnostics.snapshot ())

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
  print_words "Promise.state" words;

  print_wrapper_probe "Value.int (wrapper)" (fun () ->
    Qmljs.Value.int context 42);

  print_wrapper_probe "Value.string (wrapper)" (fun () ->
    Qmljs.Value.string context "x")
