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

let tests =
  [ "native module API", test_native_module_api
  ; "loader request attributes are per import"
  , test_loader_request_attributes_are_per_import_not_stale
  ; "loader request base and original specifier do not go stale"
  , test_loader_request_base_and_original_specifier_do_not_go_stale
  ; "json import attribute does not execute JS source"
  , test_json_import_attribute_does_not_execute_js_source
  ; "filesystem loader does not scan root without imports"
  , test_filesystem_loader_does_not_scan_root_without_imports
  ; "filesystem loader rejects symlink escape"
  , test_filesystem_loader_rejects_symlink_escape
  ; "module loader filesystem API", test_module_loader_filesystem_api
  ]
