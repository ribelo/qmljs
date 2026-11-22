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

let expect_invalid_arg label fn =
  match fn () with
  | () -> fail (label ^ ": expected Invalid_argument")
  | exception Invalid_argument _ -> ()
  | exception exn ->
      fail
        (Printf.sprintf "%s: expected Invalid_argument, got %s" label
           (Printexc.to_string exn))

(* Runs [f] on the Async scheduler and returns its value, re-raising the
   original exception so structured qmljs errors keep their classification. *)
let run_async f =
  match
    Async.Thread_safe.block_on_async (fun () ->
      Async.Monitor.try_with ~extract_exn:true f)
  with
  | Ok (Ok value) -> value
  | Ok (Error exn) | Error exn -> raise exn

(* Blocking forms of the Async module and promise APIs for specs written in
   direct style. *)
let module_eval_sync ?filename context source =
  run_async (fun () -> Qmljs.Module.eval ?filename context source)

let module_eval_with_meta_sync ?filename context source =
  run_async (fun () -> Qmljs.Module.eval_with_meta ?filename context source)

let await_result_sync ?yield_every promise =
  run_async (fun () -> Qmljs.Promise.await_result ?yield_every promise)

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
