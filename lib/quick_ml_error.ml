exception Error of string

let () = Callback.Safe.register_exception "quick_ml.error" (Error "")
