let with_runtime ~domain_mgr:_ ~clock:_ ~timeout f =
  if timeout < 0. then invalid_arg "timeout must be non-negative";
  Qmljs_runtime.with_ @@ fun runtime ->
  Qmljs_context.with_ runtime @@ fun context -> f runtime context
