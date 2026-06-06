module Loader = struct
  type t = Qmljs_types.module_loader

  let memory sources =
    let table = Hashtbl.create (List.length sources) in
    List.iter (fun (specifier, source) -> Hashtbl.replace table specifier source) sources;
    { Qmljs_types.sources = table; deny = (fun _ -> false) }

  let deny predicate loader = { loader with Qmljs_types.deny = predicate }
end

type namespace = Qmljs_types.value

let eval ?(filename = "<module>") context source =
  { Qmljs_types.context
  ; raw =
      Qmljs_raw.context_eval_module_namespace
        (Qmljs_types.context_raw context)
        source
        filename
  }

let default namespace =
  let context = Qmljs_types.value_context namespace in
  Qmljs_object.get namespace ~key:(Qmljs_value.string context "default")

let get namespace ~name =
  let context = Qmljs_types.value_context namespace in
  let key = Qmljs_value.string context name in
  if Qmljs_object.has namespace ~key then Some (Qmljs_object.get namespace ~key)
  else None

let keys = Qmljs_object.keys

let default_export_to_int = Qmljs_value.to_int
