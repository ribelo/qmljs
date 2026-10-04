open Qmljs_test_common

let test_typed_array_readback_api () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let array =
    Qmljs.Typed_array.Uint8Array.of_bytes_copy context (Bytes.of_string "\001\002\003")
  in
  check_int "uint8 length" 3 (Qmljs.Typed_array.Uint8Array.length array);
  check_int "uint8 get" 2 (Qmljs.Typed_array.Uint8Array.get array 1);
  Qmljs.Typed_array.Uint8Array.set array 1 9;
  check_int "uint8 get after set" 9 (Qmljs.Typed_array.Uint8Array.get array 1);
  check_bytes "uint8 bytes after set" (Bytes.of_string "\001\009\003")
    (Qmljs.Typed_array.Uint8Array.to_bytes_copy array)

let test_array_buffer_api () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let source = Bytes.of_string "\001\002\003" in
  let buffer = Qmljs.Array_buffer.of_bytes_copy context source in
  Bytes.set source 1 '\009';
  check_int "array buffer length" 3 (Qmljs.Array_buffer.byte_length buffer);
  check_bytes "array buffer copied bytes" (Bytes.of_string "\001\002\003")
    (Qmljs.Array_buffer.to_bytes_copy buffer);
  let bigarray =
    Bigarray.Array1.of_array Bigarray.int8_unsigned Bigarray.c_layout
      [| 4; 5; 6 |]
  in
  let from_bigarray = Qmljs.Array_buffer.of_bigarray_copy context bigarray in
  Bigarray.Array1.set bigarray 1 9;
  check_bytes "array buffer copied bigarray" (Bytes.of_string "\004\005\006")
    (Qmljs.Array_buffer.to_bytes_copy from_bigarray);
  Qmljs.Context.set_global context ~name:"buffer" (Qmljs.Value.object_ buffer);
  check_int "array buffer visible to JS" 3
    (Qmljs.Value.to_int (Qmljs.Context.eval context "buffer.byteLength"));
  Qmljs.Array_buffer.detach buffer;
  expect_structured_error "detached array buffer byte_length"
    (fun () -> ignore (Qmljs.Array_buffer.byte_length buffer : int))
    (fun error ->
      check_kind "detached byte_length kind" Qmljs.Error.JavaScript
        (Qmljs.Error.kind error));
  expect_structured_error "detached array buffer to_bytes"
    (fun () -> ignore (Qmljs.Array_buffer.to_bytes_copy buffer : bytes))
    (fun error ->
      check_kind "detached to_bytes kind" Qmljs.Error.JavaScript
        (Qmljs.Error.kind error))

let test_empty_array_buffer_is_not_detached () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let buffer = Qmljs.Array_buffer.of_bytes_copy context Bytes.empty in
  check_int "empty ArrayBuffer byte length" 0
    (Qmljs.Array_buffer.byte_length buffer);
  check_bytes "empty ArrayBuffer bytes" Bytes.empty
    (Qmljs.Array_buffer.to_bytes_copy buffer)

let test_uint8_array_buffer_interop_api () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let buffer =
    Qmljs.Array_buffer.of_bytes_copy context (Bytes.of_string "\010\020\030\040")
  in
  let view =
    Qmljs.Typed_array.Uint8Array.of_array_buffer
      ~byte_offset:1 ~length:2 buffer
  in
  check_int "uint8 view element length" 2
    (Qmljs.Typed_array.Uint8Array.length view);
  check_int "uint8 view byte offset" 1
    (Qmljs.Typed_array.Uint8Array.byte_offset view);
  check_int "uint8 view byte length" 2
    (Qmljs.Typed_array.Uint8Array.byte_length view);
  check_bool "uint8 view backing buffer" true
    (Qmljs.Value.strict_equal
       (Qmljs.Typed_array.Uint8Array.buffer view)
       (Qmljs.Value.object_ buffer));
  check_int "uint8 view get" 20 (Qmljs.Typed_array.Uint8Array.get view 0);
  Qmljs.Typed_array.Uint8Array.set view 1 99;
  check_bytes "uint8 view writes backing buffer"
    (Bytes.of_string "\010\020\099\040")
    (Qmljs.Array_buffer.to_bytes_copy buffer);
  expect_structured_error "uint8 view invalid offset"
    (fun () ->
      ignore
        (Qmljs.Typed_array.Uint8Array.of_array_buffer
           ~byte_offset:99 ~length:1 buffer
         : Qmljs.Typed_array.Uint8Array.t))
    (fun error ->
      check_kind "uint8 invalid offset kind" Qmljs.Error.JavaScript
        (Qmljs.Error.kind error))

let test_typed_array_family_api () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let backing () =
    Qmljs.Array_buffer.of_bytes_copy context (Bytes.make 128 '\000')
  in
  let check_constructor label expected view =
    Qmljs.Context.set_global context ~name:"typedFixture"
      (Qmljs.Value.typed_array view);
    check_string (label ^ " constructor") expected
      (Qmljs.Value.to_string
         (Qmljs.Context.eval context "typedFixture.constructor.name"))
  in
  let check_int_view label expected_constructor bytes_per_element make length
      byte_offset byte_length buffer_ get set first second expected_first
      expected_second =
    let buffer = backing () in
    let view = make buffer in
    check_constructor label expected_constructor view;
    check_int (label ^ " length") 2 (length view);
    check_int (label ^ " byte offset") bytes_per_element (byte_offset view);
    check_bool (label ^ " buffer") true
      (Qmljs.Value.strict_equal
         (buffer_ view)
         (Qmljs.Value.object_ buffer));
    set view 0 first;
    set view 1 second;
    check_int (label ^ " byte length") (2 * bytes_per_element)
      (byte_length view);
    check_int (label ^ " first") expected_first (get view 0);
    check_int (label ^ " second") expected_second (get view 1)
  in
  check_int_view "Int8Array" "Int8Array" 1
    (Qmljs.Typed_array.Int8Array.of_array_buffer ~byte_offset:1 ~length:2)
    Qmljs.Typed_array.Int8Array.length
    Qmljs.Typed_array.Int8Array.byte_offset
    Qmljs.Typed_array.Int8Array.byte_length
    Qmljs.Typed_array.Int8Array.buffer
    Qmljs.Typed_array.Int8Array.get
    Qmljs.Typed_array.Int8Array.set
    (-5) 12 (-5) 12;
  check_int_view "Uint8Array" "Uint8Array" 1
    (Qmljs.Typed_array.Uint8Array.of_array_buffer ~byte_offset:1 ~length:2)
    Qmljs.Typed_array.Uint8Array.length
    Qmljs.Typed_array.Uint8Array.byte_offset
    Qmljs.Typed_array.Uint8Array.byte_length
    Qmljs.Typed_array.Uint8Array.buffer
    Qmljs.Typed_array.Uint8Array.get
    Qmljs.Typed_array.Uint8Array.set
    250 12 250 12;
  check_int_view "Uint8ClampedArray" "Uint8ClampedArray" 1
    (Qmljs.Typed_array.Uint8ClampedArray.of_array_buffer
       ~byte_offset:1 ~length:2)
    Qmljs.Typed_array.Uint8ClampedArray.length
    Qmljs.Typed_array.Uint8ClampedArray.byte_offset
    Qmljs.Typed_array.Uint8ClampedArray.byte_length
    Qmljs.Typed_array.Uint8ClampedArray.buffer
    Qmljs.Typed_array.Uint8ClampedArray.get
    Qmljs.Typed_array.Uint8ClampedArray.set
    (-5) 300 0 255;
  check_int_view "Int16Array" "Int16Array" 2
    (Qmljs.Typed_array.Int16Array.of_array_buffer ~byte_offset:2 ~length:2)
    Qmljs.Typed_array.Int16Array.length
    Qmljs.Typed_array.Int16Array.byte_offset
    Qmljs.Typed_array.Int16Array.byte_length
    Qmljs.Typed_array.Int16Array.buffer
    Qmljs.Typed_array.Int16Array.get
    Qmljs.Typed_array.Int16Array.set
    (-1234) 2345 (-1234) 2345;
  check_int_view "Uint16Array" "Uint16Array" 2
    (Qmljs.Typed_array.Uint16Array.of_array_buffer ~byte_offset:2 ~length:2)
    Qmljs.Typed_array.Uint16Array.length
    Qmljs.Typed_array.Uint16Array.byte_offset
    Qmljs.Typed_array.Uint16Array.byte_length
    Qmljs.Typed_array.Uint16Array.buffer
    Qmljs.Typed_array.Uint16Array.get
    Qmljs.Typed_array.Uint16Array.set
    65535 42 65535 42;
  check_int_view "Int32Array" "Int32Array" 4
    (Qmljs.Typed_array.Int32Array.of_array_buffer ~byte_offset:4 ~length:2)
    Qmljs.Typed_array.Int32Array.length
    Qmljs.Typed_array.Int32Array.byte_offset
    Qmljs.Typed_array.Int32Array.byte_length
    Qmljs.Typed_array.Int32Array.buffer
    Qmljs.Typed_array.Int32Array.get
    Qmljs.Typed_array.Int32Array.set
    (-123456) 234567 (-123456) 234567;
  check_int_view "Uint32Array" "Uint32Array" 4
    (Qmljs.Typed_array.Uint32Array.of_array_buffer ~byte_offset:4 ~length:2)
    Qmljs.Typed_array.Uint32Array.length
    Qmljs.Typed_array.Uint32Array.byte_offset
    Qmljs.Typed_array.Uint32Array.byte_length
    Qmljs.Typed_array.Uint32Array.buffer
    Qmljs.Typed_array.Uint32Array.get
    Qmljs.Typed_array.Uint32Array.set
    4_000_000_000 42 4_000_000_000 42;
  let check_float_view label expected_constructor bytes_per_element make length
      byte_offset byte_length buffer_ get set =
    let buffer = backing () in
    let view = make buffer in
    check_constructor label expected_constructor view;
    check_int (label ^ " length") 2 (length view);
    check_int (label ^ " byte offset") bytes_per_element (byte_offset view);
    check_bool (label ^ " buffer") true
      (Qmljs.Value.strict_equal
         (buffer_ view)
         (Qmljs.Value.object_ buffer));
    check_int (label ^ " byte length") (2 * bytes_per_element)
      (byte_length view);
    set view 0 1.5;
    set view 1 (-2.25);
    check_bool (label ^ " first") true (Float.abs (get view 0 -. 1.5) < 0.0001);
    check_bool (label ^ " second") true (Float.abs (get view 1 +. 2.25) < 0.0001)
  in
  check_float_view "Float32Array" "Float32Array" 4
    (Qmljs.Typed_array.Float32Array.of_array_buffer ~byte_offset:4 ~length:2)
    Qmljs.Typed_array.Float32Array.length
    Qmljs.Typed_array.Float32Array.byte_offset
    Qmljs.Typed_array.Float32Array.byte_length
    Qmljs.Typed_array.Float32Array.buffer
    Qmljs.Typed_array.Float32Array.get
    Qmljs.Typed_array.Float32Array.set;
  check_float_view "Float64Array" "Float64Array" 8
    (Qmljs.Typed_array.Float64Array.of_array_buffer ~byte_offset:8 ~length:2)
    Qmljs.Typed_array.Float64Array.length
    Qmljs.Typed_array.Float64Array.byte_offset
    Qmljs.Typed_array.Float64Array.byte_length
    Qmljs.Typed_array.Float64Array.buffer
    Qmljs.Typed_array.Float64Array.get
    Qmljs.Typed_array.Float64Array.set;
  let check_bigint_view label expected_constructor bytes_per_element make length
      byte_offset byte_length buffer_ get set first second =
    let buffer = backing () in
    let view = make buffer in
    check_constructor label expected_constructor view;
    check_int (label ^ " length") 2 (length view);
    check_int (label ^ " byte offset") bytes_per_element (byte_offset view);
    check_bool (label ^ " buffer") true
      (Qmljs.Value.strict_equal
         (buffer_ view)
         (Qmljs.Value.object_ buffer));
    check_int (label ^ " byte length") (2 * bytes_per_element)
      (byte_length view);
    set view 0 first;
    set view 1 second;
    check_string (label ^ " first") (Qmljs.Bigint.to_string first)
      (Qmljs.Bigint.to_string (get view 0));
    check_string (label ^ " second") (Qmljs.Bigint.to_string second)
      (Qmljs.Bigint.to_string (get view 1))
  in
  check_bigint_view "BigInt64Array" "BigInt64Array" 8
    (Qmljs.Typed_array.BigInt64Array.of_array_buffer ~byte_offset:8 ~length:2)
    Qmljs.Typed_array.BigInt64Array.length
    Qmljs.Typed_array.BigInt64Array.byte_offset
    Qmljs.Typed_array.BigInt64Array.byte_length
    Qmljs.Typed_array.BigInt64Array.buffer
    Qmljs.Typed_array.BigInt64Array.get
    Qmljs.Typed_array.BigInt64Array.set
    (Qmljs.Bigint.of_int64 context (-42L))
    (Qmljs.Bigint.of_int64 context 123L);
  check_bigint_view "BigUint64Array" "BigUint64Array" 8
    (Qmljs.Typed_array.BigUint64Array.of_array_buffer ~byte_offset:8 ~length:2)
    Qmljs.Typed_array.BigUint64Array.length
    Qmljs.Typed_array.BigUint64Array.byte_offset
    Qmljs.Typed_array.BigUint64Array.byte_length
    Qmljs.Typed_array.BigUint64Array.buffer
    Qmljs.Typed_array.BigUint64Array.get
    Qmljs.Typed_array.BigUint64Array.set
    (Qmljs.Bigint.of_string context "18446744073709551615")
    (Qmljs.Bigint.of_int64 context 123L);
  expect_structured_error "typed array unaligned byte offset"
    (fun () ->
      ignore
        (Qmljs.Typed_array.Int16Array.of_array_buffer
           ~byte_offset:1 ~length:1 (backing ())
         : Qmljs.Typed_array.Int16Array.t))
    (fun error ->
      check_kind "typed array unaligned offset kind" Qmljs.Error.JavaScript
        (Qmljs.Error.kind error));
  expect_structured_error "typed array length exceeds buffer"
    (fun () ->
      ignore
        (Qmljs.Typed_array.Float64Array.of_array_buffer
           ~byte_offset:120 ~length:2 (backing ())
         : Qmljs.Typed_array.Float64Array.t))
    (fun error ->
      check_kind "typed array invalid length kind" Qmljs.Error.JavaScript
        (Qmljs.Error.kind error))

let test_typed_array_index_over_uint32_rejected () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let buffer = Qmljs.Array_buffer.of_bytes_copy context (Bytes.make 4 '\000') in
  let view =
    Qmljs.Typed_array.Int16Array.of_array_buffer ~byte_offset:0 ~length:2 buffer
  in
  let oversized_index = 0x1_0000_0000 in
  let assert_oversized_index label error =
    check_string (label ^ " message")
      "typed array index exceeds supported property range"
      (Qmljs.Error.message error)
  in
  expect_structured_error "typed array get over UINT32_MAX"
    (fun () ->
      ignore (Qmljs.Typed_array.Int16Array.get view oversized_index : int))
    (assert_oversized_index "typed array get over UINT32_MAX");
  expect_structured_error "typed array set over UINT32_MAX"
    (fun () -> Qmljs.Typed_array.Int16Array.set view oversized_index 7)
    (assert_oversized_index "typed array set over UINT32_MAX")

(* A typed array that tracks a resizable ArrayBuffer must report live geometry
   after ArrayBuffer.prototype.resize(). QuickJS 0.14 freezes the length of
   such a view when it is created, so JS_GetUint8Array/JS_GetTypedArrayBuffer
   keep reporting that frozen length: it under-reports after growth and
   over-reports after shrinkage, and over-reporting reads or writes past the
   live backing allocation. *)

let with_resizable_buffer context ~byte_length ~max_byte_length =
  let buffer =
    Qmljs.Context.eval context
      (Printf.sprintf "new ArrayBuffer(%d, { maxByteLength: %d })" byte_length
         max_byte_length)
  in
  Qmljs.Context.set_global context ~name:"binaryResizableBuffer"
    (Qmljs.Value.object_ buffer);
  buffer

let resize_resizable_buffer context byte_length =
  ignore
    (Qmljs.Context.eval context
       (Printf.sprintf "binaryResizableBuffer.resize(%d)" byte_length)
      : Qmljs.Value.t)

let sentinel_bytes count base =
  Bytes.init count (fun index -> Char.chr (base + index))

let check_typed_array_error label message error =
  check_kind (label ^ " kind") Qmljs.Error.JavaScript (Qmljs.Error.kind error);
  check_string (label ^ " message") message (Qmljs.Error.message error)

let test_uint8_view_over_resizable_array_buffer () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let buffer =
    with_resizable_buffer context ~byte_length:16 ~max_byte_length:1024
  in
  let view = Qmljs.Typed_array.Uint8Array.of_array_buffer buffer in
  for index = 0 to 15 do
    Qmljs.Typed_array.Uint8Array.set view index (0x10 + index)
  done;
  check_int "tracking view length before resize" 16
    (Qmljs.Typed_array.Uint8Array.length view);
  check_bytes "tracking view bytes before resize" (sentinel_bytes 16 0x10)
    (Qmljs.Typed_array.Uint8Array.to_bytes_copy view);
  resize_resizable_buffer context 4;
  check_int "tracking view length after shrink" 4
    (Qmljs.Typed_array.Uint8Array.length view);
  check_int "tracking view byte length after shrink" 4
    (Qmljs.Typed_array.Uint8Array.byte_length view);
  check_int "tracking view byte offset after shrink" 0
    (Qmljs.Typed_array.Uint8Array.byte_offset view);
  (* Bytes past the shrunk allocation must not be reachable. *)
  check_bytes "tracking view bytes after shrink" (sentinel_bytes 4 0x10)
    (Qmljs.Typed_array.Uint8Array.to_bytes_copy view);
  expect_structured_error "tracking view read past shrunk buffer"
    (fun () -> ignore (Qmljs.Typed_array.Uint8Array.get view 10 : int))
    (check_typed_array_error "tracking view read past shrunk buffer"
       "typed array index out of bounds");
  expect_structured_error "tracking view write past shrunk buffer"
    (fun () -> Qmljs.Typed_array.Uint8Array.set view 10 7)
    (check_typed_array_error "tracking view write past shrunk buffer"
       "typed array index out of bounds");
  resize_resizable_buffer context 12;
  check_int "tracking view length after grow" 12
    (Qmljs.Typed_array.Uint8Array.length view);
  check_int "tracking view grown byte is zero" 0
    (Qmljs.Typed_array.Uint8Array.get view 4);
  check_bytes "tracking view bytes after grow"
    (Bytes.cat (sentinel_bytes 4 0x10) (Bytes.make 8 '\000'))
    (Qmljs.Typed_array.Uint8Array.to_bytes_copy view);
  (* Growth past the length the view was created with: a length-tracking view
     follows its buffer, a fixed-length view never does. *)
  resize_resizable_buffer context 24;
  check_int "tracking view length after growth past creation length" 24
    (Qmljs.Typed_array.Uint8Array.length view);
  check_int "tracking view byte length after growth past creation length" 24
    (Qmljs.Typed_array.Uint8Array.byte_length view);
  check_int "tracking view grown byte past creation length" 0
    (Qmljs.Typed_array.Uint8Array.get view 20);
  Qmljs.Typed_array.Uint8Array.set view 20 0x7F;
  check_int "tracking view written byte past creation length" 0x7F
    (Qmljs.Typed_array.Uint8Array.get view 20)

let test_uint8_view_with_byte_offset_over_resizable_array_buffer () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let buffer =
    with_resizable_buffer context ~byte_length:16 ~max_byte_length:64
  in
  let view =
    Qmljs.Typed_array.Uint8Array.of_array_buffer ~byte_offset:4 buffer
  in
  check_int "offset tracking view length before resize" 12
    (Qmljs.Typed_array.Uint8Array.length view);
  Qmljs.Typed_array.Uint8Array.set view 0 0xAB;
  resize_resizable_buffer context 8;
  check_int "offset tracking view length after shrink" 4
    (Qmljs.Typed_array.Uint8Array.length view);
  check_int "offset tracking view byte offset after shrink" 4
    (Qmljs.Typed_array.Uint8Array.byte_offset view);
  check_int "offset tracking view first byte" 0xAB
    (Qmljs.Typed_array.Uint8Array.get view 0);
  check_bytes "offset tracking view bytes after shrink"
    (Bytes.of_string "\171\000\000\000")
    (Qmljs.Typed_array.Uint8Array.to_bytes_copy view);
  expect_structured_error "offset tracking view read past shrunk buffer"
    (fun () -> ignore (Qmljs.Typed_array.Uint8Array.get view 4 : int))
    (check_typed_array_error "offset tracking view read past shrunk buffer"
       "typed array index out of bounds");
  (* The buffer no longer reaches the view's byte offset: JS treats the view as
     out of bounds and throws from its copy methods. *)
  resize_resizable_buffer context 2;
  expect_structured_error "offset tracking view length when offset exceeds buffer"
    (fun () -> ignore (Qmljs.Typed_array.Uint8Array.length view : int))
    (check_typed_array_error "offset tracking view length when offset exceeds buffer"
       "ArrayBuffer is detached or resized");
  expect_structured_error "offset tracking view copy when offset exceeds buffer"
    (fun () -> ignore (Qmljs.Typed_array.Uint8Array.to_bytes_copy view : bytes))
    (check_typed_array_error "offset tracking view copy when offset exceeds buffer"
       "ArrayBuffer is detached or resized")

let test_fixed_uint8_view_does_not_follow_resizable_buffer_growth () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let buffer =
    with_resizable_buffer context ~byte_length:8 ~max_byte_length:64
  in
  let view =
    Qmljs.Typed_array.Uint8Array.of_array_buffer ~byte_offset:2 ~length:4 buffer
  in
  check_int "fixed view length before grow" 4
    (Qmljs.Typed_array.Uint8Array.length view);
  resize_resizable_buffer context 32;
  (* Only a length-tracking view follows the buffer; a fixed-length view keeps
     reporting its own range and must never expose the buffer's new bytes. *)
  check_int "fixed view length after grow" 4
    (Qmljs.Typed_array.Uint8Array.length view);
  check_int "fixed view byte length after grow" 4
    (Qmljs.Typed_array.Uint8Array.byte_length view);
  check_int "fixed view byte offset after grow" 2
    (Qmljs.Typed_array.Uint8Array.byte_offset view);
  check_bytes "fixed view bytes after grow" (Bytes.make 4 '\000')
    (Qmljs.Typed_array.Uint8Array.to_bytes_copy view);
  expect_structured_error "fixed view read past its own length after grow"
    (fun () -> ignore (Qmljs.Typed_array.Uint8Array.get view 4 : int))
    (check_typed_array_error "fixed view read past its own length after grow"
       "typed array index out of bounds")

let test_uint8_view_out_of_bounds_fails_loudly () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let buffer =
    with_resizable_buffer context ~byte_length:16 ~max_byte_length:1024
  in
  let view =
    Qmljs.Typed_array.Uint8Array.of_array_buffer ~byte_offset:4 ~length:8 buffer
  in
  Qmljs.Typed_array.Uint8Array.set view 0 0x5A;
  check_int "fixed view length before shrink" 8
    (Qmljs.Typed_array.Uint8Array.length view);
  resize_resizable_buffer context 4;
  (* JS reports 0 for the geometry of an out-of-bounds view, reads its elements
     as undefined and throws "ArrayBuffer is detached or resized" from its copy
     methods. This binding has no undefined result and must not report an empty
     view, so every operation on it fails loudly with that error. *)
  let out_of_bounds label fn =
    expect_structured_error label fn
      (check_typed_array_error label "ArrayBuffer is detached or resized")
  in
  out_of_bounds "out-of-bounds view length"
    (fun () -> ignore (Qmljs.Typed_array.Uint8Array.length view : int));
  out_of_bounds "out-of-bounds view byte length"
    (fun () -> ignore (Qmljs.Typed_array.Uint8Array.byte_length view : int));
  out_of_bounds "out-of-bounds view byte offset"
    (fun () -> ignore (Qmljs.Typed_array.Uint8Array.byte_offset view : int));
  out_of_bounds "out-of-bounds view read"
    (fun () -> ignore (Qmljs.Typed_array.Uint8Array.get view 0 : int));
  out_of_bounds "out-of-bounds view write"
    (fun () -> Qmljs.Typed_array.Uint8Array.set view 0 1);
  out_of_bounds "out-of-bounds view copy"
    (fun () -> ignore (Qmljs.Typed_array.Uint8Array.to_bytes_copy view : bytes));
  (* A detached backing buffer is the same state. *)
  let detached_buffer = Qmljs.Context.eval context "new ArrayBuffer(8)" in
  let detached_view =
    Qmljs.Typed_array.Uint8Array.of_array_buffer detached_buffer
  in
  Qmljs.Array_buffer.detach detached_buffer;
  out_of_bounds "detached view length"
    (fun () -> ignore (Qmljs.Typed_array.Uint8Array.length detached_view : int));
  out_of_bounds "detached view copy"
    (fun () ->
      ignore (Qmljs.Typed_array.Uint8Array.to_bytes_copy detached_view : bytes));
  (* ArrayBuffer.prototype.transfer() detaches the source buffer as well. *)
  let transferred_buffer = Qmljs.Context.eval context "new ArrayBuffer(8)" in
  Qmljs.Context.set_global context ~name:"binaryTransferredBuffer"
    (Qmljs.Value.object_ transferred_buffer);
  let transferred_view =
    Qmljs.Typed_array.Uint8Array.of_array_buffer transferred_buffer
  in
  ignore
    (Qmljs.Context.eval context "binaryTransferredBuffer.transfer()"
      : Qmljs.Value.t);
  out_of_bounds "transferred view length"
    (fun () ->
      ignore (Qmljs.Typed_array.Uint8Array.length transferred_view : int));
  out_of_bounds "transferred view copy"
    (fun () ->
      ignore
        (Qmljs.Typed_array.Uint8Array.to_bytes_copy transferred_view : bytes));
  (* A live empty view is not an out-of-bounds view, and the failures above
     must not poison later work. *)
  let empty = Qmljs.Typed_array.Uint8Array.of_bytes_copy context Bytes.empty in
  check_int "live empty view length" 0
    (Qmljs.Typed_array.Uint8Array.length empty);
  check_bytes "live empty view bytes" Bytes.empty
    (Qmljs.Typed_array.Uint8Array.to_bytes_copy empty);
  check_int "eval after failed copies" 2
    (Qmljs.Value.to_int (Qmljs.Context.eval context "1 + 1"))

let test_typed_array_view_over_resizable_array_buffer () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let buffer =
    with_resizable_buffer context ~byte_length:8 ~max_byte_length:1024
  in
  let view = Qmljs.Typed_array.Int16Array.of_array_buffer buffer in
  Qmljs.Typed_array.Int16Array.set view 0 1234;
  check_int "int16 tracking view length before resize" 4
    (Qmljs.Typed_array.Int16Array.length view);
  resize_resizable_buffer context 4;
  check_int "int16 tracking view length after shrink" 2
    (Qmljs.Typed_array.Int16Array.length view);
  check_int "int16 tracking view byte length after shrink" 4
    (Qmljs.Typed_array.Int16Array.byte_length view);
  check_int "int16 tracking view first element" 1234
    (Qmljs.Typed_array.Int16Array.get view 0);
  expect_structured_error "int16 tracking view read past shrunk buffer"
    (fun () -> ignore (Qmljs.Typed_array.Int16Array.get view 2 : int))
    (check_typed_array_error "int16 tracking view read past shrunk buffer"
       "typed array index out of bounds");
  expect_structured_error "int16 tracking view write past shrunk buffer"
    (fun () -> Qmljs.Typed_array.Int16Array.set view 2 7)
    (check_typed_array_error "int16 tracking view write past shrunk buffer"
       "typed array index out of bounds");
  resize_resizable_buffer context 12;
  check_int "int16 tracking view length after grow" 6
    (Qmljs.Typed_array.Int16Array.length view);
  check_int "int16 tracking view grown element is zero" 0
    (Qmljs.Typed_array.Int16Array.get view 4);
  Qmljs.Typed_array.Int16Array.set view 5 99;
  check_int "int16 tracking view written element" 99
    (Qmljs.Typed_array.Int16Array.get view 5)

let test_array_api () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let array = Qmljs.Array.create context in
  check_int "empty array length" 0 (Qmljs.Array.length array);
  check_int "push returns length" 1
    (Qmljs.Array.push array (Qmljs.Value.int context 10));
  check_int "append returns length" 3
    (Qmljs.Array.append array
       [ Qmljs.Value.int context 20; Qmljs.Value.int context 30 ]);
  check_int "array length after append" 3 (Qmljs.Array.length array);
  check_int "array get before set" 20
    (Qmljs.Value.to_int (Qmljs.Array.get array 1));
  Qmljs.Array.set array 1 (Qmljs.Value.int context 22);
  check_int "array get after set" 22
    (Qmljs.Value.to_int (Qmljs.Array.get array 1));
  check_int "to_list length" 3 (List.length (Qmljs.Array.to_list array));
  check_int "to_array length" 3
    (Stdlib.Array.length (Qmljs.Array.to_array array));
  let values = List.map Qmljs.Value.to_int (Qmljs.Array.to_list array) in
  if values <> [ 10; 22; 30 ] then fail "array to_list returned wrong values";
  Qmljs.Array.set array 5 (Qmljs.Value.int context 60);
  check_int "sparse set extends length" 6 (Qmljs.Array.length array);
  check_bool "hole reads as undefined" true
    (Qmljs.Value.is_undefined (Qmljs.Array.get array 3));
  Qmljs.Context.set_global context ~name:"fixture" (Qmljs.Value.array array);
  let js_view =
    Qmljs.Context.eval context
      "Array.isArray(fixture) + ':' + fixture.length + ':' + fixture[1] + ':' + fixture[5]"
  in
  check_string "array is visible to JS" "true:6:22:60"
    (Qmljs.Value.to_string js_view)

let test_bigint_readback_api () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let small = Qmljs.Bigint.of_int64 context 42L in
  check_string "bigint string" "42" (Qmljs.Bigint.to_string small);
  (match Qmljs.Bigint.to_int64 small with
   | Some value when Int64.equal value 42L -> ()
   | _ -> fail "small bigint did not round-trip as int64");
  let too_large = Qmljs.Bigint.of_string context "9223372036854775808" in
  check_bool "large bigint does not silently truncate" true
    (Option.is_none (Qmljs.Bigint.to_int64 too_large))

let tests =
  [ "typed array readback API", test_typed_array_readback_api
  ; "array buffer API", test_array_buffer_api
  ; "empty_array_buffer_is_not_detached",
    test_empty_array_buffer_is_not_detached
  ; "uint8 array buffer interop API", test_uint8_array_buffer_interop_api
  ; "typed array family API", test_typed_array_family_api
  ; "typed array index over UINT32_MAX is rejected"
  , test_typed_array_index_over_uint32_rejected
  ; ( "uint8 view over resizable array buffer",
      test_uint8_view_over_resizable_array_buffer )
  ; ( "uint8 view with byte offset over resizable array buffer",
      test_uint8_view_with_byte_offset_over_resizable_array_buffer )
  ; ( "fixed uint8 view does not follow resizable buffer growth",
      test_fixed_uint8_view_does_not_follow_resizable_buffer_growth )
  ; ( "uint8 view out of bounds fails loudly",
      test_uint8_view_out_of_bounds_fails_loudly )
  ; ( "typed array view over resizable array buffer",
      test_typed_array_view_over_resizable_array_buffer )
  ; "array API", test_array_api
  ; "bigint readback API", test_bigint_readback_api
  ]
