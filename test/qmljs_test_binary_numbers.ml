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
  ; "uint8 array buffer interop API", test_uint8_array_buffer_interop_api
  ; "typed array family API", test_typed_array_family_api
  ; "typed array index over UINT32_MAX is rejected"
  , test_typed_array_index_over_uint32_rejected
  ; "array API", test_array_api
  ; "bigint readback API", test_bigint_readback_api
  ]
