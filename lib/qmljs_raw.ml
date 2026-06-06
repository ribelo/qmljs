type runtime
type context
type value

external runtime_create : unit -> runtime = "qml_runtime_create"

external runtime_set_memory_limit : runtime -> int -> unit
  = "qml_runtime_set_memory_limit"

external runtime_set_max_stack_size : runtime -> int -> unit
  = "qml_runtime_set_max_stack_size"

external runtime_install_module_loader :
  runtime -> (string * string) array -> (string -> bool) -> unit
  = "qml_runtime_install_module_loader"

external runtime_drain_deferred : runtime -> unit = "qml_runtime_drain_deferred"
external runtime_close : runtime -> bool = "qml_runtime_close"

external runtime_is_job_pending : runtime -> bool = "qml_runtime_is_job_pending"

external runtime_execute_pending_job : runtime -> bool
  = "qml_runtime_execute_pending_job"

external runtime_request_interrupt : runtime -> unit = "qml_runtime_request_interrupt"
[@@noalloc]

external runtime_set_interrupt_timeout : runtime -> float -> unit
  = "qml_runtime_set_interrupt_timeout"

external runtime_clear_interrupt_timeout : runtime -> unit
  = "qml_runtime_clear_interrupt_timeout"

external runtime_clear_interrupt : runtime -> unit = "qml_runtime_clear_interrupt"

external runtime_interrupt_requested : runtime -> bool
  = "qml_runtime_interrupt_requested"

external context_create : runtime -> context = "qml_context_create"
external context_close : context -> bool = "qml_context_close"

external context_eval : context -> string -> string -> int -> value
  = "qml_context_eval"

external context_eval_module : context -> string -> string -> value
  = "qml_context_eval_module"

external context_eval_module_namespace : context -> string -> string -> value
  = "qml_context_eval_module_namespace"

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
external object_create : context -> value = "qml_object_create"
external object_set : context -> value -> string -> value -> unit = "qml_object_set"

external object_get : context -> value -> value -> value = "qml_object_get"
external object_has : context -> value -> value -> bool = "qml_object_has"
external object_delete : context -> value -> value -> bool = "qml_object_delete"

external object_define_data_property : context -> value -> value -> value -> int -> unit
  = "qml_object_define_data_property"

external object_get_own_property_descriptor :
  context ->
  value ->
  value ->
  (value option * bool option * bool * bool * value option * value option) option
  = "qml_object_get_own_property_descriptor"

external object_keys : context -> value -> string array = "qml_object_keys"

external array_of_values : context -> value array -> value = "qml_array_of_values"
external symbol_global : context -> string -> value = "qml_symbol_global"

external typed_array_uint8_of_bytes_copy : context -> bytes -> value
  = "qml_typed_array_uint8_of_bytes_copy"

external value_to_bool : context -> value -> bool = "qml_value_to_bool"
external value_to_int : context -> value -> int = "qml_value_to_int"
external value_to_float : context -> value -> float = "qml_value_to_float"
external value_to_string : context -> value -> string = "qml_value_to_string"
external value_call : context -> value -> value array -> value = "qml_value_call"

external value_is_promise : context -> value -> bool = "qml_value_is_promise"

external value_promise_state : context -> value -> int = "qml_value_promise_state"

external value_promise_result : context -> value -> value
  = "qml_value_promise_result"

external value_raise_as_exception : context -> value -> unit
  = "qml_value_raise_as_exception"

external promise_create : context -> value * value * value = "qml_promise_create"

external diagnostics_live_runtimes : unit -> int
  = "qml_diagnostics_live_runtimes"
[@@noalloc]

external diagnostics_live_contexts : unit -> int
  = "qml_diagnostics_live_contexts"
[@@noalloc]

external diagnostics_live_values : unit -> int = "qml_diagnostics_live_values"
[@@noalloc]
