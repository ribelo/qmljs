let with_runtime f =
  Eio.Switch.run @@ fun sw ->
  Qmljs_runtime.with_ @@ fun runtime ->
  Qmljs_context.with_ runtime @@ fun context ->
  let driver =
    { Qmljs_types.sw
    ; condition = Eio.Condition.create ()
    ; closed = false
    }
  in
  Qmljs_runtime.attach_driver runtime driver;
  Fun.protect
    ~finally:(fun () ->
      driver.Qmljs_types.closed <- true;
      Qmljs_runtime.detach_driver runtime driver;
      Eio.Condition.broadcast driver.Qmljs_types.condition)
    (fun () -> f runtime context)
