module Loader = struct
  type t = Qmljs_types.module_loader

  let memory sources =
    let table = Hashtbl.create (List.length sources) in
    List.iter (fun (specifier, source) -> Hashtbl.replace table specifier source) sources;
    { Qmljs_types.sources = table; deny = (fun _ -> false) }

  let deny predicate loader = { loader with Qmljs_types.deny = predicate }
end

let default_export_to_int = Qmljs_value.to_int
