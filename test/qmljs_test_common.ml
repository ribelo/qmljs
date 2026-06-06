let fail message = raise (Failure message)

let check_bool label expected actual =
  if expected <> actual then
    fail (Printf.sprintf "%s: expected %b, got %b" label expected actual)

let check_int label expected actual =
  if expected <> actual then
    fail (Printf.sprintf "%s: expected %d, got %d" label expected actual)

let check_string label expected actual =
  if not (String.equal expected actual) then
    fail (Printf.sprintf "%s: expected %S, got %S" label expected actual)

let check_bytes label expected actual =
  if not (Bytes.equal expected actual) then
    fail
      (Printf.sprintf "%s: expected %S, got %S" label
         (Bytes.to_string expected) (Bytes.to_string actual))

let check_kind label expected actual =
  if expected <> actual then fail (label ^ ": unexpected error kind")

let expect_structured_error label fn assert_error =
  match Qmljs.Error.capture fn with
  | Ok _ -> fail (label ^ ": expected structured error")
  | Error error -> assert_error error

let run tests =
  let failures = ref [] in
  List.iter
    (fun (name, test) ->
      match test () with
      | () -> Printf.eprintf "PASS: %s\n%!" name
      | exception exn ->
          failures := (name, Printexc.to_string exn) :: !failures;
          Printf.eprintf "RED: %s: %s\n%!" name (Printexc.to_string exn))
    tests;
  match List.rev !failures with
  | [] -> ()
  | failures ->
      Printf.eprintf "%d/%d qmljs API specs are red\n%!"
        (List.length failures)
        (List.length tests);
      exit 1
