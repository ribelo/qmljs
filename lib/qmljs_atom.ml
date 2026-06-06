type t = Qmljs_types.atom

let runtime t = Qmljs_types.atom_runtime t
let raw t = Qmljs_types.atom_raw t

let wrap context raw =
  { Qmljs_types.atom_runtime = Qmljs_types.context_runtime context
  ; atom_raw = raw
  }

let of_string context name =
  wrap context
    (Qmljs_raw.atom_of_string (Qmljs_types.context_raw context) name)

let of_uint32 context index =
  if Int64.compare index 0L < 0 || Int64.compare index 0xffff_ffffL > 0 then
    invalid_arg "atom index is outside the uint32 range";
  wrap context
    (Qmljs_raw.atom_of_uint32 (Qmljs_types.context_raw context) index)

let of_int context index =
  if index < 0 then invalid_arg "atom index is outside the uint32 range";
  of_uint32 context (Int64.of_int index)

let to_string context t =
  Qmljs_raw.atom_to_string (Qmljs_types.context_raw context) (raw t)

let to_value context t =
  { Qmljs_types.context
  ; raw = Qmljs_raw.atom_to_value (Qmljs_types.context_raw context) (raw t)
  }

let equal left right = Qmljs_raw.atom_equal (raw left) (raw right)

let named context name = of_string context name
let length context = named context "length"
let prototype context = named context "prototype"
let constructor context = named context "constructor"
let then_ context = named context "then"
let catch context = named context "catch"
let default context = named context "default"
