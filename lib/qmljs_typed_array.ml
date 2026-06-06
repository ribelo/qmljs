module Type_id = struct
  let uint8_clamped = 0
  let int8 = 1
  let uint8 = 2
  let int16 = 3
  let uint16 = 4
  let int32 = 5
  let uint32 = 6
  let bigint64 = 7
  let biguint64 = 8
  let float32 = 10
  let float64 = 11
end

type t = Qmljs_types.value

module type Int_view = sig
  type t = Qmljs_types.value

  val of_array_buffer : ?byte_offset:int -> ?length:int -> Qmljs_types.value -> t
  val length : t -> int
  val byte_offset : t -> int
  val byte_length : t -> int
  val buffer : t -> Qmljs_types.value
  val get : t -> int -> int
  val set : t -> int -> int -> unit
end

module type Float_view = sig
  type t = Qmljs_types.value

  val of_array_buffer : ?byte_offset:int -> ?length:int -> Qmljs_types.value -> t
  val length : t -> int
  val byte_offset : t -> int
  val byte_length : t -> int
  val buffer : t -> Qmljs_types.value
  val get : t -> int -> float
  val set : t -> int -> float -> unit
end

module type Bigint_view = sig
  type t = Qmljs_types.value

  val of_array_buffer : ?byte_offset:int -> ?length:int -> Qmljs_types.value -> t
  val length : t -> int
  val byte_offset : t -> int
  val byte_length : t -> int
  val buffer : t -> Qmljs_types.value
  val get : t -> int -> Qmljs_types.value
  val set : t -> int -> Qmljs_types.value -> unit
end

let context t = Qmljs_types.value_context t
let raw t = Qmljs_types.value_raw t

let check_view_args ?(byte_offset = 0) ?length () =
  if byte_offset < 0 then invalid_arg "byte_offset must be non-negative";
  Option.iter
    (fun length ->
      if length < 0 then invalid_arg "length must be non-negative")
    length;
  byte_offset, length

let of_array_buffer type_id ?byte_offset ?length array_buffer =
  let byte_offset, length = check_view_args ?byte_offset ?length () in
  let context = context array_buffer in
  { Qmljs_types.context
  ; raw =
      Qmljs_raw.typed_array_of_array_buffer
        (Qmljs_types.context_raw context)
        type_id
        (raw array_buffer)
        byte_offset
        length
  }

let length type_id t =
  Qmljs_raw.typed_array_length
    (Qmljs_types.context_raw (context t))
    type_id
    (raw t)

let byte_offset type_id t =
  Qmljs_raw.typed_array_byte_offset
    (Qmljs_types.context_raw (context t))
    type_id
    (raw t)

let byte_length type_id t =
  Qmljs_raw.typed_array_byte_length
    (Qmljs_types.context_raw (context t))
    type_id
    (raw t)

let buffer type_id t =
  let context = context t in
  { Qmljs_types.context
  ; raw =
      Qmljs_raw.typed_array_buffer
        (Qmljs_types.context_raw context)
        type_id
        (raw t)
  }

module Make_int (Type : sig
    val id : int
  end) =
struct
  type nonrec t = t

  let of_array_buffer ?byte_offset ?length array_buffer =
    of_array_buffer Type.id ?byte_offset ?length array_buffer

  let length = length Type.id
  let byte_offset = byte_offset Type.id
  let byte_length = byte_length Type.id
  let buffer = buffer Type.id

  let get t index =
    Qmljs_raw.typed_array_get_int
      (Qmljs_types.context_raw (context t))
      Type.id
      (raw t)
      index

  let set t index value =
    Qmljs_raw.typed_array_set_int
      (Qmljs_types.context_raw (context t))
      Type.id
      (raw t)
      index
      value
end

module Make_float (Type : sig
    val id : int
  end) =
struct
  type nonrec t = t

  let of_array_buffer ?byte_offset ?length array_buffer =
    of_array_buffer Type.id ?byte_offset ?length array_buffer

  let length = length Type.id
  let byte_offset = byte_offset Type.id
  let byte_length = byte_length Type.id
  let buffer = buffer Type.id

  let get t index =
    Qmljs_raw.typed_array_get_float
      (Qmljs_types.context_raw (context t))
      Type.id
      (raw t)
      index

  let set t index value =
    Qmljs_raw.typed_array_set_float
      (Qmljs_types.context_raw (context t))
      Type.id
      (raw t)
      index
      value
end

module Make_bigint (Type : sig
    val id : int
  end) =
struct
  type nonrec t = t

  let of_array_buffer ?byte_offset ?length array_buffer =
    of_array_buffer Type.id ?byte_offset ?length array_buffer

  let length = length Type.id
  let byte_offset = byte_offset Type.id
  let byte_length = byte_length Type.id
  let buffer = buffer Type.id

  let get t index =
    let context = context t in
    { Qmljs_types.context
    ; raw =
        Qmljs_raw.typed_array_get_bigint
          (Qmljs_types.context_raw context)
          Type.id
          (raw t)
          index
    }

  let set t index value =
    Qmljs_raw.typed_array_set_bigint
      (Qmljs_types.context_raw (context t))
      Type.id
      (raw t)
      index
      (raw value)
end

module Int8Array = Make_int (struct
    let id = Type_id.int8
  end)

module Uint8ClampedArray = Make_int (struct
    let id = Type_id.uint8_clamped
  end)

module Int16Array = Make_int (struct
    let id = Type_id.int16
  end)

module Uint16Array = Make_int (struct
    let id = Type_id.uint16
  end)

module Int32Array = Make_int (struct
    let id = Type_id.int32
  end)

module Uint32Array = Make_int (struct
    let id = Type_id.uint32
  end)

module Float32Array = Make_float (struct
    let id = Type_id.float32
  end)

module Float64Array = Make_float (struct
    let id = Type_id.float64
  end)

module BigInt64Array = Make_bigint (struct
    let id = Type_id.bigint64
  end)

module BigUint64Array = Make_bigint (struct
    let id = Type_id.biguint64
  end)

module Uint8Array = struct
  include Make_int (struct
      let id = Type_id.uint8
    end)

  let of_bytes_copy context bytes =
    { Qmljs_types.context
    ; raw =
        Qmljs_raw.typed_array_uint8_of_bytes_copy
          (Qmljs_types.context_raw context)
          bytes
    }

  let to_bytes_copy t =
    Qmljs_raw.typed_array_uint8_to_bytes_copy
      (Qmljs_types.context_raw (context t))
      (raw t)
end
