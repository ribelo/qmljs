type snapshot =
  { runtimes : int
  ; contexts : int
  ; values : int
  }

let snapshot () =
  { runtimes = Qmljs_raw.diagnostics_live_runtimes ()
  ; contexts = Qmljs_raw.diagnostics_live_contexts ()
  ; values = Qmljs_raw.diagnostics_live_values ()
  }

let assert_no_leaks ~before ~after =
  if after.runtimes > before.runtimes
     || after.contexts > before.contexts
     || after.values > before.values
  then
    failwith
      (Printf.sprintf
         "QuickJS wrapper leak: runtimes %d -> %d, contexts %d -> %d, values %d -> %d"
         before.runtimes after.runtimes before.contexts after.contexts
         before.values after.values)
