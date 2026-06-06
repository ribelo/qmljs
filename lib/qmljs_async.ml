let drain_jobs ?(yield_every = 64) runtime =
  if yield_every <= 0 then invalid_arg "yield_every must be positive";
  let rec loop n quickjs_jobs =
    if Qmljs_runtime.execute_host_job runtime then (
      let n = n + 1 in
      if n mod yield_every = 0 then Eio.Fiber.yield ();
      loop n quickjs_jobs)
    else if Qmljs_runtime.execute_pending_job runtime then (
      let n = n + 1 in
      let quickjs_jobs = quickjs_jobs + 1 in
      (match runtime.Qmljs_types.promise_job_limit with
       | Some limit when quickjs_jobs >= limit ->
           Qmljs_error.raise_resource_limit
             "QuickJS promise job limit exceeded"
       | _ -> ());
      if n mod yield_every = 0 then Eio.Fiber.yield ();
      loop n quickjs_jobs)
    else n
  in
  loop 0 0
