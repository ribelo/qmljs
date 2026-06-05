let drain_jobs ?(yield_every = 64) runtime =
  if yield_every <= 0 then invalid_arg "yield_every must be positive";
  let rec loop n =
    if Quick_ml_runtime.execute_host_job runtime then (
      let n = n + 1 in
      if n mod yield_every = 0 then Eio.Fiber.yield ();
      loop n)
    else if Quick_ml_runtime.execute_pending_job runtime then (
      let n = n + 1 in
      if n mod yield_every = 0 then Eio.Fiber.yield ();
      loop n)
    else n
  in
  loop 0
