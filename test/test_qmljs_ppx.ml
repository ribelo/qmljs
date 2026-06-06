let fail message = raise (Failure message)

let check_int label expected actual =
  if expected <> actual then
    fail (Printf.sprintf "%s: expected %d, got %d" label expected actual)

let check_string label expected actual =
  if not (String.equal expected actual) then
    fail (Printf.sprintf "%s: expected %S, got %S" label expected actual)

let check_bool label expected actual =
  if expected <> actual then
    fail (Printf.sprintf "%s: expected %b, got %b" label expected actual)

let check_option label check expected actual =
  match expected, actual with
  | None, None -> ()
  | Some expected, Some actual -> check label expected actual
  | _ -> fail (label ^ ": unexpected option value")

let check_result label check result =
  match result with
  | Ok value -> check value
  | Error error ->
      fail
        (Printf.sprintf "%s: %s" label (Qmljs.Error.message error))

type profile =
  { id : int
  ; name : string
  ; tags : string list
  ; nickname : string option
  ; display_name : string [@qmljs.name "displayName"]
  }
[@@deriving qmljs]

type shape =
  | Point of int * int
  | Named of string
  | Empty
[@@deriving qmljs]

type nested =
  { profile : profile
  ; favorite : shape option
  ; flags : bool array
  }
[@@deriving qmljs]

type pair = int * string [@@deriving qmljs]

let add : int -> int -> int = fun x y -> x + y [@@qmljs.global "add"]

let multiply (x : int) (y : int) : int = x * y [@@qmljs.global "multiply"]

let greet : string -> string =
  fun name -> "hello " ^ name [@@qmljs.global "greet"]

let later : int -> int = fun x -> x + 1 [@@qmljs.global_async "later"]

let test_codecs () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let input =
    { profile =
        { id = 7
        ; name = "Ada"
        ; tags = [ "math"; "js" ]
        ; nickname = None
        ; display_name = "Ada Lovelace"
        }
    ; favorite = Some (Point (3, 4))
    ; flags = [| true; false; true |]
    }
  in
  let encoded = qmljs_encode_nested context input in
  let js_name =
    match Qmljs.Object.get_string encoded "profile" with
    | None -> fail "missing nested profile"
    | Some profile ->
        (match Qmljs.Object.get_string profile "displayName" with
         | None -> fail "missing renamed field"
         | Some value -> Qmljs.Value.to_string value)
  in
  check_string "renamed field" "Ada Lovelace" js_name;
  check_result "decode nested"
    (fun decoded ->
      check_int "decoded id" input.profile.id decoded.profile.id;
      check_string "decoded name" input.profile.name decoded.profile.name;
      check_option "decoded nickname" check_string None
        decoded.profile.nickname;
      check_string "decoded tag" "math" (List.hd decoded.profile.tags);
      check_bool "decoded flag" true decoded.flags.(0);
      match decoded.favorite with
      | Some (Point (3, 4)) -> ()
      | _ -> fail "decoded variant mismatch")
    (qmljs_decode_nested encoded);
  check_result "decode pair"
    (fun (left, right) ->
      check_int "decoded pair int" 9 left;
      check_string "decoded pair string" "nine" right)
    (qmljs_decode_pair (qmljs_encode_pair context (9, "nine")));
  check_result "interface round trip"
    (fun (decoded : Qmljs_ppx_interface.t) ->
      check_int "interface count" 5 decoded.count;
      check_string "interface label" "five" decoded.label)
    (Qmljs_ppx_interface.round_trip context { count = 5; label = "five" })

let test_function_exports () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Qmljs.Eio.with_runtime ~sw ~clock:(Eio.Stdenv.clock env)
    (fun _driver _runtime context ->
      qmljs_register context;
      let sum = Qmljs.Context.eval context "add(20, 22)" in
      check_int "registered add" 42 (Qmljs.Value.to_int sum);
      let product = Qmljs.Context.eval context "multiply(6, 7)" in
      check_int "registered multiply" 42 (Qmljs.Value.to_int product);
      let greeting = Qmljs.Context.eval context "greet('qmljs')" in
      check_string "registered greet" "hello qmljs"
        (Qmljs.Value.to_string greeting);
      let promise = Qmljs.Context.eval context "later(41)" in
      match Qmljs.Promise.await_result promise with
      | Ok value -> check_int "registered async later" 42 (Qmljs.Value.to_int value)
      | Error error -> fail (Qmljs.Error.message error))

let test_bad_argument_is_structured () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  qmljs_register_add context;
  (match
     Qmljs.Error.capture (fun () ->
       ignore (Qmljs.Context.eval context "add('bad', 1)" : Qmljs.value))
   with
   | Ok () -> fail "bad argument unexpectedly accepted"
   | Error error ->
       check_bool "bad argument kind" true
         (Qmljs.Error.kind error = Qmljs.Error.Host_callback_defect);
       check_bool "bad argument metadata" true
         (List.mem ("function", "add") (Qmljs.Error.metadata error)));
  check_int "runtime reusable after bad argument" 2
    (Qmljs.Value.to_int (Qmljs.Context.eval context "1 + 1"))

let () =
  test_codecs ();
  test_function_exports ();
  test_bad_argument_is_structured ()
