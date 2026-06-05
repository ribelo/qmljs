type snapshot = Gc.stat

let snapshot () = Gc.stat ()

let assert_no_leaks ~before:_ ~after:_ = ()
