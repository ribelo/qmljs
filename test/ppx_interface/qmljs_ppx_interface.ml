type t =
  { count : int
  ; label : string
  }
[@@deriving qmljs]

let round_trip context value =
  value |> qmljs_encode context |> qmljs_decode
