type runtime
type context
type value

external runtime_create : unit -> runtime = "qml_runtime_create"

external runtime_is_job_pending : runtime -> bool = "qml_runtime_is_job_pending"
[@@noalloc]

external runtime_execute_pending_job : runtime -> bool
  = "qml_runtime_execute_pending_job"

external context_create : runtime -> context = "qml_context_create"

external context_eval : context -> string -> string -> int -> value
  = "qml_context_eval"

external context_global : context -> value = "qml_context_global"
external context_set_global : context -> string -> value -> unit = "qml_context_set_global"

external function_make : context -> string -> int -> (value array -> value) -> value
  = "qml_function_make"

external value_undefined : context -> value = "qml_value_undefined"
external value_null : context -> value = "qml_value_null"
external value_bool : context -> bool -> value = "qml_value_bool"
external value_int : context -> int -> value = "qml_value_int"
external value_float : context -> float -> value = "qml_value_float"
external value_string : context -> string -> value = "qml_value_string"

external value_to_bool : context -> value -> bool = "qml_value_to_bool"
external value_to_int : context -> value -> int = "qml_value_to_int"
external value_to_float : context -> value -> float = "qml_value_to_float"
external value_to_string : context -> value -> string = "qml_value_to_string"

external value_is_promise : value -> bool = "qml_value_is_promise"
[@@noalloc]

external value_promise_state : context -> value -> int = "qml_value_promise_state"
[@@noalloc]

external value_promise_result : context -> value -> value
  = "qml_value_promise_result"
