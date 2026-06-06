type t =
  { count : int
  ; label : string
  }
[@@deriving qmljs]

val round_trip : Qmljs.Context.t -> t -> (t, Qmljs.Error.t) result
