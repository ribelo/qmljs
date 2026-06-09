type runtime
type context
type value
type atom
type native_exports
type class_id

external runtime_create : unit -> runtime = "qmljs_runtime_create"

external runtime_set_memory_limit : runtime -> int -> unit
  = "qmljs_runtime_set_memory_limit"

external runtime_set_max_stack_size : runtime -> int -> unit
  = "qmljs_runtime_set_max_stack_size"

external runtime_memory_usage : runtime -> int64 array
  = "qmljs_runtime_memory_usage"

external runtime_run_gc : runtime -> unit = "qmljs_runtime_run_gc"
external runtime_get_gc_threshold : runtime -> int = "qmljs_runtime_get_gc_threshold"

external runtime_set_info : runtime -> string -> unit = "qmljs_runtime_set_info"

external runtime_set_promise_rejection_tracking : runtime -> bool -> unit
  = "qmljs_runtime_set_promise_rejection_tracking"

external runtime_take_promise_rejections :
  runtime -> (Qmljs_error.Structured.t * bool) array
  = "qmljs_runtime_take_promise_rejections"

external runtime_set_promise_hook_tracking : runtime -> bool -> unit
  = "qmljs_runtime_set_promise_hook_tracking"

external runtime_take_promise_hooks : runtime -> (int * int * bool) array
  = "qmljs_runtime_take_promise_hooks"

external runtime_set_gc_threshold : runtime -> int -> unit
  = "qmljs_runtime_set_gc_threshold"

external runtime_install_module_loader :
  runtime ->
  (string -> (bool, Qmljs_error.marker) result) ->
  (string option ->
   string ->
   (string * string) array ->
   (string, Qmljs_error.marker) result) ->
  (string ->
   (string * string) array ->
   (string option, Qmljs_error.marker) result) ->
  unit
  = "qmljs_runtime_install_module_loader"

external runtime_drain_deferred : runtime -> unit = "qmljs_runtime_drain_deferred"

external runtime_drain_deferred_runtimes : unit -> unit
  = "qmljs_runtime_drain_deferred_runtimes"

external runtime_close : runtime -> bool = "qmljs_runtime_close"

external runtime_is_job_pending : runtime -> bool = "qmljs_runtime_is_job_pending"

external runtime_execute_pending_job : runtime -> bool
  = "qmljs_runtime_execute_pending_job"

external runtime_request_interrupt : runtime -> unit = "qmljs_runtime_request_interrupt"
[@@noalloc]

external runtime_set_interrupt_timeout : runtime -> float -> unit
  = "qmljs_runtime_set_interrupt_timeout"

external runtime_clear_interrupt_timeout : runtime -> unit
  = "qmljs_runtime_clear_interrupt_timeout"

external runtime_clear_interrupt : runtime -> unit = "qmljs_runtime_clear_interrupt"

external runtime_interrupt_requested : runtime -> bool
  = "qmljs_runtime_interrupt_requested"

external context_create : runtime -> context = "qmljs_context_create"
external context_create_raw : runtime -> int array -> context
  = "qmljs_context_create_raw"

external context_close : context -> bool = "qmljs_context_close"

external context_eval : context -> string -> string -> int -> value
  = "qmljs_context_eval"

external context_eval_module_namespace : context -> string -> string -> value * value
  = "qmljs_context_eval_module_namespace"

external context_eval_module_with_meta :
  context -> string -> string -> value * value * value
  = "qmljs_context_eval_module_with_meta"

external context_global : context -> value = "qmljs_context_global"
external context_set_global : context -> string -> value -> unit = "qmljs_context_set_global"

external module_declare_native :
  context ->
  string ->
  string array ->
  (native_exports -> (unit, Qmljs_error.marker) result) ->
  unit
  = "qmljs_module_declare_native"

external module_native_export : context -> native_exports -> string -> value -> unit
  = "qmljs_module_native_export"

external module_dynamic_import : context -> string -> string -> value
  = "qmljs_module_dynamic_import"

external function_make : context -> string -> int -> (value array -> value) -> value
  = "qmljs_function_make"

external function_make_with_this :
  context -> string -> int -> (value -> value array -> value option -> value) -> value
  = "qmljs_function_make_with_this"

external function_call : context -> value -> value -> value array -> value
  = "qmljs_function_call"

external function_construct : context -> value -> value array -> value
  = "qmljs_function_construct"

external function_defer : context -> value -> value -> value array -> unit
  = "qmljs_function_defer"

external function_name : context -> value -> string = "qmljs_function_name"
external function_set_name : context -> value -> string -> unit = "qmljs_function_set_name"
external function_length : context -> value -> int = "qmljs_function_length"
external function_set_length : context -> value -> int -> unit = "qmljs_function_set_length"
external function_prototype : context -> value = "qmljs_function_prototype"

external function_is_constructor : context -> value -> bool
  = "qmljs_function_is_constructor"

external function_set_constructor : context -> value -> bool -> unit
  = "qmljs_function_set_constructor"

external function_host_error : context -> Qmljs_error.marker -> value
  = "qmljs_function_host_error"

external value_undefined : context -> value = "qmljs_value_undefined"
external value_null : context -> value = "qmljs_value_null"
external value_bool : context -> bool -> value = "qmljs_value_bool"
external value_int : context -> int -> value = "qmljs_value_int"
external value_float : context -> float -> value = "qmljs_value_float"
external value_string : context -> string -> value = "qmljs_value_string"
external object_create : context -> value = "qmljs_object_create"
external object_create_proto : context -> value -> value = "qmljs_object_create_proto"
external object_set : context -> value -> string -> value -> unit = "qmljs_object_set"

external object_get : context -> value -> value -> value = "qmljs_object_get"
external object_has : context -> value -> value -> bool = "qmljs_object_has"
external object_has_property : context -> value -> value -> bool
  = "qmljs_object_has_property"
external object_delete : context -> value -> value -> bool = "qmljs_object_delete"
external object_set_atom : context -> value -> atom -> value -> unit
  = "qmljs_object_set_atom"

external object_get_atom : context -> value -> atom -> value = "qmljs_object_get_atom"
external object_has_atom : context -> value -> atom -> bool = "qmljs_object_has_atom"
external object_delete_atom : context -> value -> atom -> bool
  = "qmljs_object_delete_atom"

external object_get_prototype : context -> value -> value = "qmljs_object_get_prototype"

external object_set_prototype : context -> value -> value -> unit
  = "qmljs_object_set_prototype"

external object_define_data_property : context -> value -> value -> value -> int -> unit
  = "qmljs_object_define_data_property"

external object_define_data_property_atom :
  context -> value -> atom -> value -> int -> unit
  = "qmljs_object_define_data_property_atom"

external object_define_accessor_property :
  context -> value -> value -> (value option * value option) -> int -> unit
  = "qmljs_object_define_accessor_property"

external object_define_accessor_property_atom :
  context -> value -> atom -> (value option * value option) -> int -> unit
  = "qmljs_object_define_accessor_property_atom"

external object_get_own_property_descriptor :
  context ->
  value ->
  value ->
  (value option * bool option * bool * bool * value option * value option) option
  = "qmljs_object_get_own_property_descriptor"

external object_get_own_property_descriptor_atom :
  context ->
  value ->
  atom ->
  (value option * bool option * bool * bool * value option * value option) option
  = "qmljs_object_get_own_property_descriptor_atom"

external object_keys : context -> value -> string array = "qmljs_object_keys"
external object_own_keys : context -> value -> int -> value array = "qmljs_object_own_keys"

external atom_of_string : context -> string -> atom = "qmljs_atom_of_string"
external atom_of_uint32 : context -> int64 -> atom = "qmljs_atom_of_uint32"
external atom_to_string : context -> atom -> string = "qmljs_atom_to_string"
external atom_to_value : context -> atom -> value = "qmljs_atom_to_value"
external atom_equal : atom -> atom -> bool = "qmljs_atom_equal"

external proxy_create : context -> value -> value -> value = "qmljs_proxy_create"
external proxy_is : context -> value -> bool = "qmljs_proxy_is"
external proxy_target : context -> value -> value option = "qmljs_proxy_target"
external proxy_handler : context -> value -> value option = "qmljs_proxy_handler"

external class_create : context -> string -> ('a -> unit) option -> class_id
  = "qmljs_class_create"
external class_prototype : context -> class_id -> value = "qmljs_class_prototype"
external class_new_instance : context -> class_id -> 'a -> value
  = "qmljs_class_new_instance"

external class_get : context -> class_id -> value -> 'a option = "qmljs_class_get"
external class_is_instance : context -> class_id -> value -> bool
  = "qmljs_class_is_instance"

external array_create : context -> value = "qmljs_array_create"
external array_of_values : context -> value array -> value = "qmljs_array_of_values"
external array_length : context -> value -> int = "qmljs_array_length"
external array_get : context -> value -> int -> value = "qmljs_array_get"
external array_set : context -> value -> int -> value -> unit = "qmljs_array_set"
external array_push : context -> value -> value array -> int = "qmljs_array_push"

external array_buffer_of_bytes_copy : context -> bytes -> value
  = "qmljs_array_buffer_of_bytes_copy"

external array_buffer_of_bigarray_copy :
  context ->
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  value
  = "qmljs_array_buffer_of_bigarray_copy"

external array_buffer_byte_length : context -> value -> int
  = "qmljs_array_buffer_byte_length"

external array_buffer_to_bytes_copy : context -> value -> bytes
  = "qmljs_array_buffer_to_bytes_copy"

external array_buffer_detach : context -> value -> unit
  = "qmljs_array_buffer_detach"

external symbol_global : context -> string -> value = "qmljs_symbol_global"
external symbol_well_known : context -> string -> value = "qmljs_symbol_well_known"

external typed_array_uint8_of_bytes_copy : context -> bytes -> value
  = "qmljs_typed_array_uint8_of_bytes_copy"

external typed_array_uint8_of_array_buffer :
  context -> value -> int -> int option -> value
  = "qmljs_typed_array_uint8_of_array_buffer"

external typed_array_of_array_buffer : context -> int -> value -> int -> int option -> value
  = "qmljs_typed_array_of_array_buffer"

external typed_array_length : context -> int -> value -> int = "qmljs_typed_array_length"

external typed_array_byte_offset : context -> int -> value -> int
  = "qmljs_typed_array_byte_offset"

external typed_array_byte_length : context -> int -> value -> int
  = "qmljs_typed_array_byte_length"

external typed_array_buffer : context -> int -> value -> value
  = "qmljs_typed_array_buffer"

external typed_array_get_int : context -> int -> value -> int -> int
  = "qmljs_typed_array_get_int"

external typed_array_set_int : context -> int -> value -> int -> int -> unit
  = "qmljs_typed_array_set_int"

external typed_array_get_float : context -> int -> value -> int -> float
  = "qmljs_typed_array_get_float"

external typed_array_set_float : context -> int -> value -> int -> float -> unit
  = "qmljs_typed_array_set_float"

external typed_array_get_bigint : context -> int -> value -> int -> value
  = "qmljs_typed_array_get_bigint"

external typed_array_set_bigint : context -> int -> value -> int -> value -> unit
  = "qmljs_typed_array_set_bigint"

external typed_array_uint8_length : context -> value -> int
  = "qmljs_typed_array_uint8_length"

external typed_array_uint8_byte_offset : context -> value -> int
  = "qmljs_typed_array_uint8_byte_offset"

external typed_array_uint8_byte_length : context -> value -> int
  = "qmljs_typed_array_uint8_byte_length"

external typed_array_uint8_buffer : context -> value -> value
  = "qmljs_typed_array_uint8_buffer"

external typed_array_uint8_get : context -> value -> int -> int
  = "qmljs_typed_array_uint8_get"

external typed_array_uint8_set : context -> value -> int -> int -> unit
  = "qmljs_typed_array_uint8_set"

external typed_array_uint8_to_bytes_copy : context -> value -> bytes
  = "qmljs_typed_array_uint8_to_bytes_copy"

external json_parse : context -> string -> string -> value = "qmljs_json_parse"

external json_stringify : context -> value -> int option -> string
  = "qmljs_json_stringify"

external value_kind : context -> value -> int = "qmljs_value_kind"

external value_to_bool : context -> value -> bool = "qmljs_value_to_bool"
external value_to_int : context -> value -> int = "qmljs_value_to_int"
external value_to_float : context -> value -> float = "qmljs_value_to_float"
external value_to_string : context -> value -> string = "qmljs_value_to_string"
external bigint_of_string : context -> string -> value = "qmljs_bigint_of_string"
external value_call : context -> value -> value array -> value = "qmljs_value_call"

external value_strict_equal : context -> value -> value -> bool
  = "qmljs_value_strict_equal"

external value_same_value : context -> value -> value -> bool
  = "qmljs_value_same_value"

external value_same_value_zero : context -> value -> value -> bool
  = "qmljs_value_same_value_zero"

external value_is_promise : context -> value -> bool = "qmljs_value_is_promise"

external value_promise_state : context -> value -> int = "qmljs_value_promise_state"

external value_promise_result : context -> value -> value
  = "qmljs_value_promise_result"

external value_raise_as_exception : context -> value -> unit
  = "qmljs_value_raise_as_exception"

external promise_create : context -> value * value * value = "qmljs_promise_create"

external diagnostics_live_runtimes : unit -> int
  = "qmljs_diagnostics_live_runtimes"
[@@noalloc]

external diagnostics_live_contexts : unit -> int
  = "qmljs_diagnostics_live_contexts"
[@@noalloc]

external diagnostics_live_values : unit -> int = "qmljs_diagnostics_live_values"
[@@noalloc]

external diagnostics_live_native_runtimes : unit -> int
  = "qmljs_diagnostics_live_native_runtimes"
[@@noalloc]

external diagnostics_live_argv_arrays : unit -> int
  = "qmljs_diagnostics_live_argv_arrays"
[@@noalloc]
