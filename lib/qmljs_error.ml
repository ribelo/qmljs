exception Error of string

let () = Callback.Safe.register_exception "qmljs.error" (Error "")
