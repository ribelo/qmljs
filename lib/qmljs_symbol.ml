type t = Qmljs_types.value

let global context description =
  { Qmljs_types.context
  ; raw = Qmljs_raw.symbol_global (Qmljs_types.context_raw context) description
  }
