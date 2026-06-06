type t = Qmljs_types.value

let global context description =
  { Qmljs_types.context
  ; raw = Qmljs_raw.symbol_global (Qmljs_types.context_raw context) description
  }

let well_known context name =
  { Qmljs_types.context
  ; raw = Qmljs_raw.symbol_well_known (Qmljs_types.context_raw context) name
  }

let iterator context = well_known context "iterator"
let to_string_tag context = well_known context "toStringTag"
let has_instance context = well_known context "hasInstance"
