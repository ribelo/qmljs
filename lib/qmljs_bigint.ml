type t = Qmljs_types.value

let of_string context value = Qmljs_context.eval context (value ^ "n")
