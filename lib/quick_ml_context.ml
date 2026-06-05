type t = Quick_ml_types.context

let create runtime =
  { Quick_ml_types.runtime
  ; raw = Quick_ml_raw.context_create (Quick_ml_types.runtime_raw runtime)
  }

let eval ?(filename = "<eval>") ?(strict = false) ?(async = false)
    ?(module_ = false) t source =
  let flags =
    (if module_ then 1 else 0)
    lor (if strict then 1 lsl 3 else 0)
    lor (if async then 1 lsl 7 else 0)
  in
  { Quick_ml_types.context = t
  ; raw = Quick_ml_raw.context_eval (Quick_ml_types.context_raw t) source filename flags
  }

let global t =
  { Quick_ml_types.context = t
  ; raw = Quick_ml_raw.context_global (Quick_ml_types.context_raw t)
  }

let set_global t ~name value =
  Quick_ml_raw.context_set_global
    (Quick_ml_types.context_raw t)
    name
    (Quick_ml_types.value_raw value)
