module Loader = Qmljs__Qmljs_module.Loader

let fail message = raise (Failure message)

let create_temp_dir () =
  let rec loop attempts =
    if attempts = 0 then fail "could not create loader acquisition test directory";
    let root =
      Filename.concat (Filename.get_temp_dir_name ())
        (Printf.sprintf "qmljs-acquisition-%d-%d" (Unix.getpid ()) (Random.bits ()))
    in
    try
      Unix.mkdir root 0o700;
      root
    with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> loop (attempts - 1)
  in
  loop 16

let write_file path source =
  let channel = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel source)

let rec wait_bounded pid attempts =
  match Unix.waitpid [ Unix.WNOHANG ] pid with
  | 0, _ when attempts > 0 ->
      ignore (Unix.select [] [] [] 0.01);
      wait_bounded pid (attempts - 1)
  | 0, _ -> None
  | _, status -> Some status

let reject_fifo_in_child root specifier path =
  (* No QuickJS handles are created or inherited. The child exercises only the
     actual Unix acquisition leaf, and bypasses finalizers with [_exit]. *)
  match Unix.fork () with
  | 0 ->
      Sys.Safe.set_signal Sys.sigalrm Sys.Signal_default;
      ignore (Unix.alarm 2);
      let code =
        match Loader.read_checked_file root specifier path with
        | None -> 0
        | Some _ -> 2
        | exception _ -> 3
      in
      Unix._exit code
  | pid ->
      let reaped = ref false in
      Fun.protect
        ~finally:(fun () ->
          if not !reaped then begin
            Unix.kill pid Sys.sigkill;
            match wait_bounded pid 100 with
            | Some _ -> reaped := true
            | None -> fail "could not reap loader acquisition child after SIGKILL"
          end)
        (fun () ->
          match wait_bounded pid 400 with
          | None -> fail "loader acquisition child exceeded parent watchdog"
          | Some status ->
              reaped := true;
              match status with
              | Unix.WEXITED 0 -> ()
              | Unix.WEXITED 2 -> fail "loader acquisition accepted a replacement FIFO"
              | Unix.WEXITED code ->
                  fail (Printf.sprintf "loader acquisition child failed (exit %d)" code)
              | Unix.WSIGNALED signal when signal = Sys.sigalrm ->
                  fail "loader acquisition timed out opening replacement FIFO"
              | Unix.WSIGNALED signal | Unix.WSTOPPED signal ->
                  fail (Printf.sprintf "loader acquisition child interrupted (signal %d)" signal))

let test_checked_file_acquisition () =
  let root = create_temp_dir () in
  let specifier = "module.js" in
  let path = Filename.concat root specifier in
  Fun.protect
    ~finally:(fun () ->
      (try Unix.unlink path with Unix.Unix_error (Unix.ENOENT, _, _) -> ());
      Unix.rmdir root)
    (fun () ->
      let source = "export default 42;\n" in
      write_file path source;
      let checked_path =
        match Loader.filesystem_path root specifier with
        | Some checked_path -> checked_path
        | None -> fail "regular module file failed initial validation"
      in
      if Loader.read_checked_file root specifier checked_path <> Some source then
        fail "checked acquisition did not preserve regular module source";
      (* Deterministically reproduce replacement after the initial path check,
         without a timing race or a production-only test hook. No FIFO writer
         exists, so a blocking read-only open cannot complete. *)
      Unix.unlink path;
      Unix.mkfifo path 0o600;
      reject_fifo_in_child root specifier checked_path)

let () =
  test_checked_file_acquisition ();
  Printf.eprintf "PASS: checked loader acquisition rejects replacement FIFO without blocking\n%!"
