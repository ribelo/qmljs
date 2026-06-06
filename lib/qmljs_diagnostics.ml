type snapshot =
  { runtimes : int
  ; contexts : int
  ; values : int
  ; native_runtimes : int
  ; argv_arrays : int
  }

let snapshot () =
  Gc.full_major ();
  Gc.full_major ();
  { runtimes = Qmljs_raw.diagnostics_live_runtimes ()
  ; contexts = Qmljs_raw.diagnostics_live_contexts ()
  ; values = Qmljs_raw.diagnostics_live_values ()
  ; native_runtimes = Qmljs_raw.diagnostics_live_native_runtimes ()
  ; argv_arrays = Qmljs_raw.diagnostics_live_argv_arrays ()
  }

let native_runtimes t = t.native_runtimes

let assert_no_leaks ~before ~after =
  if after.runtimes > before.runtimes
     || after.contexts > before.contexts
     || after.values > before.values
     || after.native_runtimes > before.native_runtimes
     || after.argv_arrays > before.argv_arrays
  then
    failwith
      (Printf.sprintf
         "QuickJS leak: runtimes %d -> %d, contexts %d -> %d, values %d -> %d, native runtimes %d -> %d, argv arrays %d -> %d"
         before.runtimes after.runtimes before.contexts after.contexts
         before.values after.values before.native_runtimes
         after.native_runtimes before.argv_arrays after.argv_arrays)
