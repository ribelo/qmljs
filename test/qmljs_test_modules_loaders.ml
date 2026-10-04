[@@@alert "-do_not_spawn_domains"]
[@@@alert "-unsafe_multidomain"]

open Qmljs_test_common

let test_native_module_api () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let evaluated =
    Qmljs.Module.eval_with_meta ~filename:"meta-main.js" context
      {|import.meta.qmljs = "visible";
        export const answer = 11;
        export default import.meta.qmljs;|}
  in
  Qmljs.Context.set_global context ~name:"metaNamespace"
    evaluated.Qmljs.Module.namespace;
  check_int "evaluated module namespace is a value" 11
    (Qmljs.Value.to_int (Qmljs.Context.eval context "metaNamespace.answer"));
  Qmljs.Context.set_global context ~name:"metaObject"
    evaluated.Qmljs.Module.import_meta;
  check_string "evaluated module import.meta object" "visible"
    (Qmljs.Value.to_string
       (Qmljs.Context.eval context "metaObject.qmljs"));
  let evaluations = ref 0 in
  let native_math =
    Qmljs.Module.Native.define
      (fun declarations ->
        Qmljs.Module.Native.declare declarations "answer";
        Qmljs.Module.Native.declare declarations "default")
      ~evaluate:(fun exports ->
        incr evaluations;
        Qmljs.Module.Native.export exports ~name:"answer"
          (Qmljs.Value.int context 42);
        Qmljs.Module.Native.export exports ~name:"default"
          (Qmljs.Value.int context 7))
  in
  Qmljs.Module.declare_native context ~name:"native:math" native_math;
  let namespace =
    Qmljs.Module.eval ~filename:"native-main.js" context
      {|import defaultValue, { answer } from "native:math";
        import { answer as answerAgain } from "native:math";
        export const named = answer;
        export default defaultValue + answer + answerAgain;|}
  in
  check_int "native module default export" 91
    (Qmljs.Value.to_int (Qmljs.Module.default namespace));
  (match Qmljs.Module.get namespace ~name:"named" with
   | Some value -> check_int "native module named export" 42 (Qmljs.Value.to_int value)
   | None -> fail "native module namespace did not expose named export");
  check_int "native module evaluated once" 1 !evaluations;
  let bad_module =
    Qmljs.Module.Native.define
      (fun declarations ->
        Qmljs.Module.Native.declare declarations "default")
      ~evaluate:(fun _exports ->
        raise
          (Qmljs.Error.host ~message:"native module boom"
             ~metadata:[ "module", "native:bad" ]))
  in
  Qmljs.Module.declare_native context ~name:"native:bad" bad_module;
  expect_structured_error "native module evaluation defect"
    (fun () ->
      ignore
        (Qmljs.Module.eval ~filename:"native-bad-main.js" context
           {|import value from "native:bad"; export default value;|}
         : Qmljs.Module.namespace))
    (fun error ->
      check_kind "native module defect kind" Qmljs.Error.Host_callback_defect
        (Qmljs.Error.kind error);
      check_string "native module defect message" "native module boom"
        (Qmljs.Error.message error))

let create_temp_dir () =
  let rec loop attempts =
    if attempts = 0 then fail "could not create temporary module loader directory";
    let name =
      Printf.sprintf "qmljs-loader-%d-%d" (Unix.getpid ()) (Random.bits ())
    in
    let root = Filename.concat (Filename.get_temp_dir_name ()) name in
    try
      Unix.mkdir root 0o700;
      root
    with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> loop (attempts - 1)
  in
  loop 16

let write_file path source =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel source)

let mkdir path =
  try Unix.mkdir path 0o700 with
  | Unix.Unix_error (Unix.EEXIST, _, _) -> ()

let rec remove_tree path =
  if Sys.file_exists path then
    if Sys.is_directory path then begin
      Sys.readdir path
      |> Array.iter (fun name -> remove_tree (Filename.concat path name));
      Unix.rmdir path
    end else Sys.remove path

let with_temp_loader_dir fn =
  let root = create_temp_dir () in
  Fun.protect
    ~finally:(fun () ->
      try remove_tree root with
      | Sys_error _ | Unix.Unix_error _ -> ())
    (fun () -> fn root)

let test_filesystem_loader_rejects_symlink_escape () =
  with_temp_loader_dir @@ fun root ->
  let outside = Filename.temp_file "qmljs-loader-outside-" ".js" in
  Fun.protect
    ~finally:(fun () ->
      try Sys.remove outside with
      | Sys_error _ | Unix.Unix_error _ -> ())
    (fun () ->
      write_file outside "export default 99;";
      Unix.symlink outside (Filename.concat root "outside.js");
      Qmljs.Runtime.with_ @@ fun runtime ->
      Qmljs.Context.with_ runtime @@ fun context ->
      let loader =
        Qmljs.Module.Loader.filesystem ~root ~allow:(fun ~specifier:_ -> true)
      in
      Qmljs.Runtime.set_module_loader runtime loader;
      expect_structured_error "filesystem loader symlink escape"
        (fun () ->
          ignore
            (Qmljs.Module.eval ~filename:"symlink-main.js" context
               {|import value from "outside.js"; export default value;|}
             : Qmljs.Module.namespace))
        (fun _error ->
          check_int "context reusable after symlink escape" 2
            (Qmljs.Value.to_int (Qmljs.Context.eval context "1 + 1"))))

let test_filesystem_loader_rejects_symlink_swap_race () =
  with_temp_loader_dir @@ fun root ->
  let victim = Filename.concat root "victim.js" in
  let outside = Filename.temp_file "qmljs-loader-outside-" ".js" in
  Fun.protect
    ~finally:(fun () ->
      try Sys.remove outside with
      | Sys_error _ | Unix.Unix_error _ -> ())
    (fun () ->
      let inside_source = "export default 7;" in
      write_file victim inside_source;
      write_file outside "export default 99;";
      let stop = Atomic.make false in
      let swapper =
        Domain.spawn (fun () ->
          while not (Atomic.get stop) do
            (try Sys.remove victim with
             | Sys_error _ | Unix.Unix_error _ -> ());
            (try Unix.symlink outside victim with
             | Unix.Unix_error _ -> ());
            Domain.cpu_relax ();
            (try Sys.remove victim with
             | Sys_error _ | Unix.Unix_error _ -> ());
            (try write_file victim inside_source with
             | Sys_error _ | Unix.Unix_error _ -> ());
            Domain.cpu_relax ()
          done)
      in
      Fun.protect
        ~finally:(fun () ->
          Atomic.set stop true;
          Domain.join swapper)
        (fun () ->
          let saw_outside = ref false in
          for attempt = 1 to 300 do
            match
              Qmljs.Error.capture (fun () ->
                Qmljs.Runtime.with_ @@ fun runtime ->
                Qmljs.Context.with_ runtime @@ fun context ->
                let loader =
                  Qmljs.Module.Loader.filesystem ~root
                    ~allow:(fun ~specifier:_ -> true)
                in
                Qmljs.Runtime.set_module_loader runtime loader;
                let namespace =
                  Qmljs.Module.eval
                    ~filename:(Printf.sprintf "symlink-swap-%d.js" attempt)
                    context
                    {|import value from "victim.js"; export default value;|}
                in
                Qmljs.Value.to_int (Qmljs.Module.default namespace))
            with
            | Ok 7 -> ()
            | Ok 99 -> saw_outside := true
            | Ok value ->
                fail (Printf.sprintf "unexpected module value %d" value)
            | Error _ -> ()
          done;
          check_bool "filesystem loader did not read swapped symlink target" false
            !saw_outside))

let test_filesystem_loader_does_not_scan_root_without_imports () =
  with_temp_loader_dir @@ fun root ->
  let unreadable = Filename.concat root "unreadable" in
  mkdir unreadable;
  Fun.protect
    ~finally:(fun () ->
      try Unix.chmod unreadable 0o700 with
      | Unix.Unix_error _ -> ())
    (fun () ->
      Unix.chmod unreadable 0o000;
      Qmljs.Runtime.with_ @@ fun runtime ->
      Qmljs.Context.with_ runtime @@ fun context ->
      let loader =
        Qmljs.Module.Loader.filesystem ~root ~allow:(fun ~specifier:_ -> true)
      in
      Qmljs.Runtime.set_module_loader runtime loader;
      let namespace =
        Qmljs.Module.eval ~filename:"eval.mjs" context "export default 7;"
      in
      check_int "filesystem loader no-import default" 7
        (Qmljs.Value.to_int (Qmljs.Module.default namespace)))

let test_loader_request_attributes_are_per_import_not_stale () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let load_requests = ref [] in
  let loader =
    Qmljs.Module.Loader.callback
      ~normalize:(fun ~base:_ ~specifier ~attributes ->
        if String.equal specifier "as-json" then begin
          check_bool "json import attributes reach normalize" true
            (List.mem ("type", "json") attributes);
          Ok "shared-data"
        end else if String.equal specifier "as-js" then begin
          check_bool "js import attributes reach normalize" true
            (List.mem ("type", "js") attributes);
          Ok "shared-data"
        end else Ok specifier)
      ~load:(fun request ->
        load_requests := request :: !load_requests;
        if List.mem ("type", "json") request.Qmljs.Module.Loader.attributes
        then Ok (Some {|{"answer":41}|})
        else if List.mem ("type", "js") request.attributes
        then Ok (Some "export default { answer: 41 };")
        else Ok None)
  in
  Qmljs.Runtime.set_module_loader runtime loader;
  expect_structured_error "loader request attribute conflict"
    (fun () ->
      ignore
        (Qmljs.Module.eval ~filename:"stale-loader-request-main.js" context
           {|import jsData from "as-js" with { type: "js" };
             import jsonData from "as-json" with { type: "json" };
             export default jsonData.answer + jsData.answer;|}
         : Qmljs.Module.namespace))
    (fun error ->
      check_bool "attribute conflict is policy violation" true
        (Qmljs.Error.is_policy_violation error);
      check_string "attribute conflict specifier" "as-json"
        (Qmljs.Error.denied_module_specifier_exn error));
  check_bool "incompatible shared module is not loaded twice" true
    (List.length !load_requests <= 1);
  check_int "context reusable after loader request conflict" 2
    (Qmljs.Value.to_int (Qmljs.Context.eval context "1 + 1"))

let test_loader_request_base_and_original_specifier_do_not_go_stale () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let original_loads = ref [] in
  let original_loader =
    Qmljs.Module.Loader.callback
      ~normalize:(fun ~base:_ ~specifier ~attributes ->
        check_bool "original request attributes are empty" true
          (List.is_empty attributes);
        if String.equal specifier "alias-a"
           || String.equal specifier "alias-b"
        then Ok "shared-original"
        else Ok specifier)
      ~load:(fun request ->
        original_loads := request :: !original_loads;
        Ok (Some "export default 21;"))
  in
  Qmljs.Runtime.set_module_loader runtime original_loader;
  expect_structured_error "loader request original specifier conflict"
    (fun () ->
      ignore
        (Qmljs.Module.eval ~filename:"request-original-main.js" context
           {|import a from "alias-a";
             import b from "alias-b";
             export default a + b;|}
          : Qmljs.Module.namespace))
    (fun error ->
      check_bool "original specifier conflict is policy violation" true
        (Qmljs.Error.is_policy_violation error);
      check_string "original specifier conflict denied specifier" "alias-b"
        (Qmljs.Error.denied_module_specifier_exn error));
  check_bool "original specifier conflict did not reload shared module" true
    (List.length !original_loads <= 1);
  let base_loads = ref [] in
  let base_loader =
    Qmljs.Module.Loader.callback
      ~normalize:(fun ~base:_ ~specifier ~attributes ->
        check_bool "base request attributes are empty" true
          (List.is_empty attributes);
        if String.equal specifier "./alias" then Ok "shared-base"
        else Ok specifier)
      ~load:(fun request ->
        base_loads := request :: !base_loads;
        match request.Qmljs.Module.Loader.normalized with
        | "left/main.js" | "right/main.js" ->
            Ok (Some {|import value from "./alias"; export default value;|})
        | "shared-base" -> Ok (Some "export default 7;")
        | _ -> Ok None)
  in
  Qmljs.Runtime.set_module_loader runtime base_loader;
  expect_structured_error "loader request base conflict"
    (fun () ->
      ignore
        (Qmljs.Module.eval ~filename:"request-base-main.js" context
           {|import left from "left/main.js";
             import right from "right/main.js";
             export default left + right;|}
          : Qmljs.Module.namespace))
    (fun error ->
      check_bool "base conflict is policy violation" true
        (Qmljs.Error.is_policy_violation error);
      check_string "base conflict denied specifier" "./alias"
        (Qmljs.Error.denied_module_specifier_exn error));
  check_int "context reusable after loader request identity conflicts" 2
    (Qmljs.Value.to_int (Qmljs.Context.eval context "1 + 1"))

let test_module_loader_reentrant_replace_is_safe () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let replacement specifier =
    Qmljs.Module.Loader.memory [ specifier, "export default 99;" ]
  in
  let replace specifier =
    Qmljs.Runtime.set_module_loader runtime (replacement specifier)
  in
  let expect_reentrant_defect label specifier loader =
    Qmljs.Runtime.set_module_loader runtime loader;
    expect_structured_error label
      (fun () ->
        ignore
          (Qmljs.Module.eval
             ~filename:(label ^ ".js")
             context
             (Printf.sprintf "import value from %S; export default value;"
                specifier)
           : Qmljs.Module.namespace))
      (fun error ->
        check_kind (label ^ " kind")
          Qmljs.Error.Host_callback_defect (Qmljs.Error.kind error);
        check_string (label ^ " message")
          "cannot replace module loader during module loader callback"
          (Qmljs.Error.message error));
    check_int (label ^ " context reusable") 2
      (Qmljs.Value.to_int (Qmljs.Context.eval context "1 + 1"))
  in
  expect_reentrant_defect "module loader normalize reentrant replacement"
    "normalize-dep.js"
    (Qmljs.Module.Loader.callback
       ~normalize:(fun ~base:_ ~specifier ~attributes:_ ->
         replace specifier;
         Ok specifier)
       ~load:(fun _request -> Ok (Some "export default 7;")));
  expect_reentrant_defect "module loader deny reentrant replacement"
    "deny-dep.js"
    (Qmljs.Module.Loader.memory [ "deny-dep.js", "export default 7;" ]
     |> Qmljs.Module.Loader.deny (fun specifier ->
       replace specifier;
       false));
  expect_reentrant_defect "module loader load reentrant replacement"
    "load-dep.js"
    (Qmljs.Module.Loader.callback
       ~normalize:(fun ~base:_ ~specifier ~attributes:_ -> Ok specifier)
       ~load:(fun request ->
         replace request.Qmljs.Module.Loader.normalized;
         Ok (Some "export default 7;")))

let test_module_normalize_rejects_nul_normalized_specifier () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let load_requests = ref [] in
  let loader =
    Qmljs.Module.Loader.callback
      ~normalize:(fun ~base:_ ~specifier:_ ~attributes:_ -> Ok "safe\000evil")
      ~load:(fun request ->
        load_requests := request :: !load_requests;
        Ok (Some "export default 1;"))
  in
  Qmljs.Runtime.set_module_loader runtime loader;
  expect_structured_error "NUL normalized module specifier"
    (fun () ->
      ignore
        (Qmljs.Module.eval ~filename:"nul-normalize-main.js" context
           {|import value from "entry"; export default value;|}
         : Qmljs.Module.namespace))
    (fun error ->
      check_bool "NUL normalized specifier is policy violation" true
        (Qmljs.Error.is_policy_violation error);
      check_string "NUL normalized specifier denied import" "entry"
        (Qmljs.Error.denied_module_specifier_exn error));
  check_bool "NUL normalized specifier is not loaded" true
    (List.is_empty !load_requests);
  let expect_nul_dynamic_import label expected ?base specifier =
    expect_structured_error label
      (fun () ->
        ignore (Qmljs.Module.dynamic_import ?base context specifier : Qmljs.Value.t))
      (fun error ->
        check_bool (label ^ " is policy violation") true
          (Qmljs.Error.is_policy_violation error);
        check_string (label ^ " denied specifier") expected
          (Qmljs.Error.denied_module_specifier_exn error))
  in
  expect_nul_dynamic_import "dynamic import NUL specifier"
    "safe\000evil" "safe\000evil";
  expect_nul_dynamic_import "dynamic import NUL base"
    "base\000evil" ~base:"base\000evil" "./child.js";
  check_int "context reusable after NUL module specifiers" 2
    (Qmljs.Value.to_int (Qmljs.Context.eval context "1 + 1"))

let test_json_import_attribute_does_not_execute_js_source () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let loader =
    Qmljs.Module.Loader.memory
      [ "data.json", "globalThis.pwned = 1"
      ; "proto.json", {|{"__proto__":{"polluted":true},"answer":41}|}
      ]
  in
  Qmljs.Runtime.set_module_loader runtime loader;
  ignore (Qmljs.Context.eval context "globalThis.pwned = 0" : Qmljs.Value.t);
  expect_structured_error "JSON import rejects JavaScript source"
    (fun () ->
      ignore
        (Qmljs.Module.eval ~filename:"json-source-main.js" context
           {|import data from "data.json" with { type: "json" };
             export default data;|}
         : Qmljs.Module.namespace))
    (fun _error -> ());
  check_int "invalid JSON import did not execute source" 0
    (Qmljs.Value.to_int (Qmljs.Context.eval context "globalThis.pwned"));
  let namespace =
    Qmljs.Module.eval ~filename:"json-proto-main.js" context
      {|import data from "proto.json" with { type: "json" };
        export default Object.prototype.hasOwnProperty.call(data, "__proto__")
          && data.answer === 41
          && data.__proto__.polluted === true;|}
  in
  check_bool "JSON import parses object data" true
    (Qmljs.Value.to_bool (Qmljs.Module.default namespace))

let test_fallback_loader_preserves_child_normalized_identity () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  ignore (Qmljs.Context.eval context "globalThis.evaluations = 0" : Qmljs.value);
  let child_loader =
    Qmljs.Module.Loader.callback
      ~normalize:(fun ~base:_ ~specifier ~attributes:_ ->
        match specifier with
        | "alias-a" | "alias-b" -> Ok "shared"
        | _ -> Ok specifier)
      ~load:(fun request ->
        if String.equal request.Qmljs.Module.Loader.normalized "shared" then
          Ok
            (Some
               {|globalThis.evaluations = globalThis.evaluations + 1;
                 export default globalThis.evaluations;|})
        else Ok None)
  in
  Qmljs.Runtime.set_module_loader runtime
    (Qmljs.Module.Loader.fallback [ child_loader ]);
  let namespace =
    Qmljs.Module.eval ~filename:"fallback-identity-main.js" context
      {|import a from "alias-a";
        import b from "alias-b";
        export default globalThis.evaluations;|}
  in
  check_int "fallback child module evaluated once" 1
    (Qmljs.Value.to_int (Qmljs.Module.default namespace))

let test_module_loader_filesystem_api () =
  with_temp_loader_dir @@ fun root ->
  mkdir (Filename.concat root "pages");
  write_file (Filename.concat root "dep.js") "export const answer = 42;";
  write_file (Filename.concat root "data.json") {|{"answer":41}|};
  write_file (Filename.concat root "blocked.js") "export default 0;";
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let loader =
    Qmljs.Module.Loader.filesystem ~root ~allow:(fun ~specifier ->
        not (String.equal specifier "blocked.js"))
    |> Qmljs.Module.Loader.normalize (fun ~base:_ ~specifier ->
        if String.equal specifier "alias" then Ok "dep.js" else Ok specifier)
  in
  Qmljs.Runtime.set_module_loader runtime loader;
  let namespace =
    Qmljs.Module.eval ~filename:"main.js" context
      {|import { answer } from "alias"; export default answer;|}
  in
  check_int "filesystem module default" 42
    (Qmljs.Value.to_int (Qmljs.Module.default namespace));
  let relative_namespace =
    Qmljs.Module.eval ~filename:"pages/main.js" context
      {|import { answer } from "../dep.js"; export default answer + 1;|}
  in
  check_int "filesystem loader relative import" 43
    (Qmljs.Value.to_int (Qmljs.Module.default relative_namespace));
  let json_namespace =
    Qmljs.Module.eval ~filename:"json-main.js" context
      {|import data from "data.json" with { type: "json" };
        export default data.answer;|}
  in
  check_int "filesystem loader JSON import attribute" 41
    (Qmljs.Value.to_int (Qmljs.Module.default json_namespace));
  let repeated_namespace =
    Qmljs.Module.eval ~filename:"repeated-main.js" context
      {|import { answer as a } from "dep.js";
        import { answer as b } from "alias";
        export default a + b;|}
  in
  check_int "filesystem loader repeated import" 84
    (Qmljs.Value.to_int (Qmljs.Module.default repeated_namespace));
  let callback_requests = ref [] in
  let callback_loader =
    Qmljs.Module.Loader.callback
      ~normalize:(fun ~base ~specifier ~attributes ->
        check_bool "callback loader sees import attribute" true
          (List.mem ("type", "json") attributes);
        match base, specifier with
        | Some "pages/callback-main.js", "./virtual" -> Ok "callback.json"
        | _ -> fail "callback loader received wrong normalize request")
      ~load:(fun request ->
        callback_requests := request :: !callback_requests;
        if String.equal request.Qmljs.Module.Loader.normalized "callback.json"
           && String.equal request.specifier "./virtual"
           && request.base = Some "pages/callback-main.js"
           && List.mem ("type", "json") request.attributes
        then Ok (Some {|{"answer":44}|})
        else Ok None)
  in
  Qmljs.Runtime.set_module_loader runtime callback_loader;
  let callback_namespace =
    Qmljs.Module.eval ~filename:"pages/callback-main.js" context
      {|import data from "./virtual" with { type: "json" };
        export default data.answer;|}
  in
  check_int "callback loader JSON default" 44
    (Qmljs.Value.to_int (Qmljs.Module.default callback_namespace));
  check_int "callback loader called once" 1 (List.length !callback_requests);
  let composed_virtual_loads = ref 0 in
  let composed_virtual_loader =
    Qmljs.Module.Loader.callback
      ~normalize:(fun ~base:_ ~specifier ~attributes ->
        if String.equal specifier "virtual:data" then
          check_bool "composed loader sees import attribute" true
            (List.mem ("type", "json") attributes);
        Ok specifier)
      ~load:(fun request ->
        if String.equal request.Qmljs.Module.Loader.normalized "virtual:data"
        then begin
          incr composed_virtual_loads;
          Ok (Some {|{"answer":4}|})
        end else Ok None)
  in
  let native_composed =
    Qmljs.Module.Native.define
      (fun declarations -> Qmljs.Module.Native.declare declarations "default")
      ~evaluate:(fun exports ->
        Qmljs.Module.Native.export exports ~name:"default"
          (Qmljs.Value.int context 6))
  in
  Qmljs.Module.declare_native context ~name:"native:composed" native_composed;
  let composed_loader =
    Qmljs.Module.Loader.fallback
      [ Qmljs.Module.Loader.memory
          [ "memory-only.js", "export const memory = 10;" ]
      ; composed_virtual_loader
      ; loader
      ]
  in
  Qmljs.Runtime.set_module_loader runtime composed_loader;
  let composed_namespace =
    Qmljs.Module.eval ~filename:"pages/composed-main.js" context
      {|import { memory } from "memory-only.js";
        import data from "virtual:data" with { type: "json" };
        import { answer } from "../dep.js";
        import nativeValue from "native:composed";
        export default memory + data.answer + answer + nativeValue;|}
  in
  check_int "composed loader default" 62
    (Qmljs.Value.to_int (Qmljs.Module.default composed_namespace));
  check_int "composed callback loader called once" 1 !composed_virtual_loads;
  expect_structured_error "composed loader deny"
    (fun () ->
      ignore
        (Qmljs.Module.eval ~filename:"composed-blocked-main.js" context
           {|import blocked from "blocked.js"; export default blocked;|}
          : Qmljs.Module.namespace))
    (fun error ->
      check_bool "composed denied module is policy violation" true
        (Qmljs.Error.is_policy_violation error);
      check_string "composed denied specifier" "blocked.js"
        (Qmljs.Error.denied_module_specifier_exn error));
  Qmljs.Runtime.set_module_loader runtime loader;
  let dynamic_namespace =
    match Qmljs.Promise.await_result
            (Qmljs.Module.dynamic_import ~base:"dynamic-main.js" context "dep.js")
    with
    | Ok namespace -> namespace
    | Error error ->
        fail ("dynamic import rejected: " ^ Qmljs.Error.message error)
  in
  (match Qmljs.Module.get dynamic_namespace ~name:"answer" with
   | Some value ->
       check_int "dynamic import namespace" 42 (Qmljs.Value.to_int value)
   | None -> fail "dynamic import did not resolve to a namespace");
  (match
     Qmljs.Promise.await_result
       (Qmljs.Module.dynamic_import ~base:"dynamic-blocked-main.js" context
          "blocked.js")
   with
   | Ok _ -> fail "dynamic import of denied module resolved"
   | Error error ->
       check_bool "dynamic import deny is policy violation" true
         (Qmljs.Error.is_policy_violation error);
       check_string "dynamic import denied specifier" "blocked.js"
         (Qmljs.Error.denied_module_specifier_exn error));
  expect_structured_error "filesystem loader deny"
    (fun () ->
      ignore
        (Qmljs.Module.eval ~filename:"blocked-main.js" context
           {|import blocked from "blocked.js"; export default blocked;|}
          : Qmljs.Module.namespace))
    (fun error ->
      check_bool "blocked module is policy violation" true
        (Qmljs.Error.is_policy_violation error);
      check_string "blocked module specifier" "blocked.js"
        (Qmljs.Error.denied_module_specifier_exn error));
  expect_structured_error "filesystem loader not found"
    (fun () ->
      ignore
        (Qmljs.Module.eval ~filename:"missing-main.js" context
           {|import missing from "missing.js"; export default missing;|}
          : Qmljs.Module.namespace))
    (fun error ->
      check_kind "missing module is JavaScript failure" Qmljs.Error.JavaScript
        (Qmljs.Error.kind error));
  let defective_loader =
    Qmljs.Module.Loader.memory [ "defect.js", "export default 1;" ]
    |> Qmljs.Module.Loader.deny (fun _specifier ->
        failwith "loader defect")
  in
  Qmljs.Runtime.set_module_loader runtime defective_loader;
  expect_structured_error "filesystem loader defect"
    (fun () ->
      ignore
        (Qmljs.Module.eval ~filename:"defect-main.js" context
           {|import value from "defect.js"; export default value;|}
          : Qmljs.Module.namespace))
    (fun error ->
      check_kind "loader defect kind" Qmljs.Error.Host_callback_defect
        (Qmljs.Error.kind error));
  Qmljs.Runtime.set_module_loader runtime loader;
  expect_structured_error "filesystem loader root escape"
    (fun () ->
      ignore
        (Qmljs.Module.eval ~filename:"pages/escape-main.js" context
           {|import escaped from "../../dep.js"; export default escaped;|}
          : Qmljs.Module.namespace))
    (fun error ->
      check_bool "root escape is policy violation" true
        (Qmljs.Error.is_policy_violation error);
      check_string "root escape specifier" "../../dep.js"
        (Qmljs.Error.denied_module_specifier_exn error));
  expect_structured_error "filesystem loader unknown import attribute"
    (fun () ->
      ignore
        (Qmljs.Module.eval ~filename:"unknown-attribute-main.js" context
           {|import { answer } from "dep.js" with { integrity: "dev" };
             export default answer;|}
          : Qmljs.Module.namespace))
    (fun error ->
      check_kind "unknown import attribute kind" Qmljs.Error.JavaScript
        (Qmljs.Error.kind error))

let test_loader_normalization_survives_minor_gc () =
  let control = Gc.get () in
  Fun.protect ~finally:(fun () -> Gc.set control) (fun () ->
    (* The import window below allocates about one nursery: the [Some] wrapper
       is allocated before the base and specifier strings are copied, so a
       minor collection can move the wrapper while the C loader still holds a
       plain C alias of it. Gc.set clamps to the runtime minimum, and the
       stress is only meaningful while the nursery stays small. *)
    Gc.set { control with minor_heap_size = 512 };
    let nursery_words = (Gc.get ()).minor_heap_size in
    if nursery_words > 1024 then
      fail
        (Printf.sprintf
           "GC-stressed import window needs a small nursery, runtime clamped \
            minor_heap_size to %d words"
           nursery_words);
    Qmljs.Runtime.with_ @@ fun runtime ->
    Qmljs.Context.with_ runtime @@ fun context ->
    let expected_base = ref "" in
    let expected_specifier = ref "" in
    let normalizations = ref 0 in
    let loader =
      Qmljs.Module.Loader.callback
        ~normalize:(fun ~base ~specifier ~attributes ->
          (* Recycle more than a whole nursery before the first collection
             sees this callback's arguments as roots: a collection inside the
             C loader promotes its rooted slots, so an alias the C code still
             holds points at nursery memory this churn overwrites. *)
          let rec churn blocks =
            if blocks <= 0 then ()
            else begin
              ignore (Sys.opaque_identity (String.make 232 'x'));
              churn (blocks - 1)
            end
          in
          churn ((nursery_words / 30) + 64);
          Gc.minor ();
          incr normalizations;
          (match base with
           | Some base -> check_string "GC-stressed import base" !expected_base base
           | None -> fail "GC-stressed import lost its base");
          check_string "GC-stressed import specifier" !expected_specifier specifier;
          check_bool "GC-stressed import attributes" true
            (List.mem ("type", "js") attributes);
          Ok specifier)
        ~load:(fun request ->
          check_string "GC-stressed load specifier" !expected_specifier
            request.Qmljs.Module.Loader.specifier;
          Ok (Some "export default 42;"))
    in
    Qmljs.Runtime.set_module_loader runtime loader;
    (* The sweep fills the nursery with 30-word minor-heap blocks before every
       import so the [Some] wrapper and the copied import names cross the
       minor heap limit inside the C loader path, covering every alignment. *)
    let fill_nursery words =
      let block = String.make 232 'x' in
      let rec loop remaining acc =
        if remaining <= 0 then acc else loop (remaining - 30) (block :: acc)
      in
      loop words []
    in
    for attempt = 0 to 1999 do
      (* Names of roughly 250 words stay in the minor heap and keep the C
         window wide; longer names are allocated directly in the major heap
         and never take part in it. *)
      expected_base := Printf.sprintf "%s-main-%d.js"
        (String.make (1900 + attempt mod 100) 'b') attempt;
      expected_specifier := Printf.sprintf "%s-dep-%d.js"
        (String.make (1900 + (attempt * 17) mod 100) 's') attempt;
      let source = Printf.sprintf
        "import value from %S with { type: \"js\" }; export default value;"
        !expected_specifier in
      let fill = Sys.opaque_identity (fill_nursery (30 * (attempt mod 150))) in
      let namespace = Qmljs.Module.eval ~filename:!expected_base context source in
      ignore fill;
      check_int "GC-stressed import result" 42
        (Qmljs.Value.to_int (Qmljs.Module.default namespace))
    done;
    check_int "GC-stressed normalize callback count" 2000 !normalizations)

let tests =
  [ "native module API", test_native_module_api
  ; "loader request attributes are per import"
  , test_loader_request_attributes_are_per_import_not_stale
  ; "loader request base and original specifier do not go stale"
  , test_loader_request_base_and_original_specifier_do_not_go_stale
  ; "module loader reentrant replacement is safe"
  , test_module_loader_reentrant_replace_is_safe
  ; "module normalize rejects NUL normalized specifier"
  , test_module_normalize_rejects_nul_normalized_specifier
  ; "json import attribute does not execute JS source"
  , test_json_import_attribute_does_not_execute_js_source
  ; "fallback loader preserves child normalized identity"
  , test_fallback_loader_preserves_child_normalized_identity
  ; "filesystem loader does not scan root without imports"
  , test_filesystem_loader_does_not_scan_root_without_imports
  ; "filesystem loader rejects symlink escape"
  , test_filesystem_loader_rejects_symlink_escape
  ; "filesystem loader rejects symlink swap race"
  , test_filesystem_loader_rejects_symlink_swap_race
  ; "module loader filesystem API", test_module_loader_filesystem_api
  ; "loader normalization survives minor GC", test_loader_normalization_survives_minor_gc
  ]
