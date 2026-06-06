#include "qmljs_stubs.h"

#include <caml/printexc.h>
#include <string.h>
#include <time.h>

struct qml_module_loader {
  struct qml_runtime_state *state;
  value deny;
  value normalize;
  value load;
};

struct qml_promise_rejection_event {
  value reason;
  int is_handled;
  struct qml_promise_rejection_event *next;
};

struct qml_promise_hook_event {
  int kind;
  int state;
  int has_parent;
  struct qml_promise_hook_event *next;
};

struct qml_native_module_data {
  struct qml_runtime_state *state;
  value evaluate;
};

struct qml_native_exports {
  JSContext *ctx;
  JSModuleDef *module;
};

static int qml_define_string_property(JSContext *ctx, JSValueConst object,
                                      const char *name, const char *value)
{
  JSValue string = JS_NewString(ctx, value);
  if (JS_IsException(string)) {
    return -1;
  }
  return JS_DefinePropertyValueStr(ctx, object, name, string, JS_PROP_C_W_E);
}

static JSValue qml_throw_marked_error(JSContext *ctx, const char *message,
                                      const char *kind,
                                      const char *denied_specifier)
{
  JSValue error = JS_NewError(ctx);
  if (JS_IsException(error)) {
    return error;
  }
  if (qml_define_string_property(ctx, error, "message", message) < 0 ||
      qml_define_string_property(ctx, error, "_qmljs_kind", kind) < 0 ||
      (denied_specifier != NULL &&
       qml_define_string_property(ctx, error,
                                  "_qmljs_denied_module_specifier",
                                  denied_specifier) < 0)) {
    JS_FreeValue(ctx, error);
    return JS_EXCEPTION;
  }
  return JS_Throw(ctx, error);
}

static void qml_native_module_data_free(struct qml_native_module_data *data)
{
  caml_remove_generational_global_root(&data->evaluate);
  qml_state_unref(data->state);
  caml_stat_free(data);
}

static void qml_native_module_private_finalizer(JSRuntime *rt, JSValueConst value)
{
  (void)rt;
  struct qml_native_module_data *data = JS_GetOpaque(value, JS_GetClassID(value));
  if (data != NULL) {
    qml_native_module_data_free(data);
  }
}

static const JSClassDef qml_native_module_class_def = {
  .class_name = "QmljsNativeModule",
  .finalizer = qml_native_module_private_finalizer,
};

static JSClassID qml_ensure_native_module_class(value context, JSContext *ctx)
{
  struct qml_runtime_state *state = qml_context_state(context);
  if (!state->native_module_class_registered) {
    JSRuntime *rt = JS_GetRuntime(ctx);
    JSClassID class_id = JS_NewClassID(rt, &state->native_module_class_id);
    if (JS_NewClass(rt, class_id, &qml_native_module_class_def) < 0) {
      qml_raise_string("could not register qmljs native module class");
    }
    state->native_module_class_registered = 1;
  }
  return state->native_module_class_id;
}

#define Qml_native_exports_val(v) ((struct qml_native_exports *)Data_custom_val(v))

static struct custom_operations qml_native_exports_ops = {
  "qmljs.native_exports",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static value qml_alloc_native_exports(JSContext *ctx, JSModuleDef *module)
{
  CAMLparam0();
  CAMLlocal1(block);
  block = caml_alloc_custom(&qml_native_exports_ops,
                            sizeof(struct qml_native_exports), 0, 1);
  Qml_native_exports_val(block)->ctx = ctx;
  Qml_native_exports_val(block)->module = module;
  CAMLreturn(block);
}

static int qml_native_module_eval(JSContext *ctx, JSModuleDef *module)
{
  CAMLparam0();
  CAMLlocal3(exports, result, error);
  JSValue private_value = JS_GetModulePrivateValue(ctx, module);
  if (JS_IsException(private_value)) {
    CAMLreturnT(int, -1);
  }

  struct qml_native_module_data *data =
    JS_IsObject(private_value)
      ? JS_GetOpaque(private_value, JS_GetClassID(private_value))
      : NULL;
  if (data == NULL) {
    JS_FreeValue(ctx, private_value);
    (void)qml_throw_marked_error(ctx, "invalid qmljs native module",
                                 "host_callback_defect", NULL);
    CAMLreturnT(int, -1);
  }

  exports = qml_alloc_native_exports(ctx, module);
  qml_enter_ocaml_callback(data->state);
  result = caml_callback_exn(data->evaluate, exports);
  qml_leave_ocaml_callback(data->state);
  JS_FreeValue(ctx, private_value);
  if (Is_exception_result(result)) {
    char *message = caml_format_exception(Extract_exception(result));
    (void)qml_throw_marked_error(ctx, message, "host_callback_defect", NULL);
    caml_stat_free(message);
    CAMLreturnT(int, -1);
  }

  if (Tag_val(result) == 1) {
    error = Field(result, 0);
    value message = Field(error, 0);
    value kind = Field(error, 1);
    value denied_specifier = Field(error, 2);
    (void)qml_throw_marked_error(
      ctx, String_val(message), String_val(kind),
      caml_string_length(denied_specifier) == 0
        ? NULL
        : String_val(denied_specifier));
    CAMLreturnT(int, -1);
  }

  CAMLreturnT(int, 0);
}

CAMLprim value qml_module_declare_native(value context, value name,
                                         value exports, value evaluate)
{
  CAMLparam4(context, name, exports, evaluate);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  JSClassID class_id = qml_ensure_native_module_class(context, ctx);

  JSModuleDef *module = JS_NewCModule(ctx, String_val(name),
                                      qml_native_module_eval);
  if (module == NULL) {
    qml_raise_exception(ctx);
  }

  mlsize_t export_count = Wosize_val(exports);
  for (mlsize_t i = 0; i < export_count; i++) {
    if (JS_AddModuleExport(ctx, module, String_val(Field(exports, i))) < 0) {
      qml_raise_exception(ctx);
    }
  }

  struct qml_native_module_data *data =
    caml_stat_alloc(sizeof(struct qml_native_module_data));
  data->state = qml_context_state(context);
  qml_state_ref(data->state);
  data->evaluate = evaluate;
  caml_register_generational_global_root(&data->evaluate);

  JSValue private_value = JS_NewObjectClass(ctx, class_id);
  if (JS_IsException(private_value)) {
    qml_native_module_data_free(data);
    qml_raise_exception(ctx);
  }
  if (JS_SetOpaque(private_value, data) < 0) {
    JS_FreeValue(ctx, private_value);
    qml_native_module_data_free(data);
    qml_raise_string("could not attach qmljs native module data");
  }
  if (JS_SetModulePrivateValue(ctx, module, private_value) < 0) {
    JS_FreeValue(ctx, private_value);
    qml_raise_exception(ctx);
  }

  CAMLreturn(Val_unit);
}

CAMLprim value qml_module_native_export(value context, value exports,
                                        value name, value wrapped)
{
  CAMLparam4(context, exports, name, wrapped);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  qml_ensure_same_context(context, wrapped);
  struct qml_native_exports *native_exports = Qml_native_exports_val(exports);
  if (native_exports->ctx != ctx) {
    qml_raise_string("native module exports belong to a different context");
  }

  JSValue js_value = JS_DupValue(ctx, qml_js_value(wrapped));
  if (JS_SetModuleExport(ctx, native_exports->module, String_val(name),
                         js_value) < 0) {
    qml_raise_exception(ctx);
  }
  CAMLreturn(Val_unit);
}

static int qml_module_loader_denies(JSContext *ctx,
                                    struct qml_module_loader *loader,
                                    const char *module_name)
{
  CAMLparam0();
  CAMLlocal1(arg);
  arg = caml_copy_string(module_name);
  qml_enter_ocaml_callback(loader->state);
  value result = caml_callback_exn(loader->deny, arg);
  qml_leave_ocaml_callback(loader->state);
  if (Is_exception_result(result)) {
    char *message = caml_format_exception(Extract_exception(result));
    (void)qml_throw_marked_error(ctx, message, "host_callback_defect", NULL);
    caml_stat_free(message);
    CAMLreturnT(int, -1);
  }
  CAMLreturnT(int, Bool_val(result));
}

static int qml_import_attributes_to_ocaml(JSContext *ctx,
                                          JSValueConst attributes, value *out)
{
  CAMLparam0();
  CAMLlocal4(array, pair, name_value, attribute_value);
  if (JS_IsUndefined(attributes) || JS_IsNull(attributes)) {
    array = caml_alloc(0, 0);
    *out = array;
    CAMLreturnT(int, 0);
  }

  JSPropertyEnum *properties = NULL;
  uint32_t property_count = 0;
  if (JS_GetOwnPropertyNames(ctx, &properties, &property_count, attributes,
                             JS_GPN_ENUM_ONLY | JS_GPN_STRING_MASK) < 0) {
    CAMLreturnT(int, -1);
  }

  array = caml_alloc(property_count, 0);
  for (uint32_t i = 0; i < property_count; i++) {
    const char *name = JS_AtomToCString(ctx, properties[i].atom);
    if (name == NULL) {
      JS_FreePropertyEnum(ctx, properties, property_count);
      CAMLreturnT(int, -1);
    }

    JSValue js_value = JS_GetProperty(ctx, attributes, properties[i].atom);
    if (JS_IsException(js_value)) {
      JS_FreeCString(ctx, name);
      JS_FreePropertyEnum(ctx, properties, property_count);
      CAMLreturnT(int, -1);
    }

    const char *js_string = JS_ToCString(ctx, js_value);
    if (js_string == NULL) {
      JS_FreeValue(ctx, js_value);
      JS_FreeCString(ctx, name);
      JS_FreePropertyEnum(ctx, properties, property_count);
      CAMLreturnT(int, -1);
    }

    pair = caml_alloc_tuple(2);
    name_value = caml_copy_string(name);
    attribute_value = caml_copy_string(js_string);
    Store_field(pair, 0, name_value);
    Store_field(pair, 1, attribute_value);
    Store_field(array, i, pair);

    JS_FreeCString(ctx, js_string);
    JS_FreeValue(ctx, js_value);
    JS_FreeCString(ctx, name);
  }

  JS_FreePropertyEnum(ctx, properties, property_count);
  *out = array;
  CAMLreturnT(int, 0);
}

static char *qml_module_loader_normalize(JSContext *ctx,
                                         const char *module_base_name,
                                         const char *module_name,
                                         JSValueConst attributes,
                                         void *opaque)
{
  struct qml_module_loader *loader = opaque;
  CAMLparam0();
  CAMLlocalN(roots, 9);
  value base;
  value specifier;
  value normalized;
  value some;
  value base_string;
  value error;
  value message;
  value kind;
  value denied_specifier;

  if (module_base_name == NULL) {
    roots[0] = Val_int(0);
  } else {
    roots[3] = caml_alloc(1, 0);
    some = roots[3];
    Store_field(some, 0, Val_unit);
    roots[4] = caml_copy_string(module_base_name);
    base_string = roots[4];
    Store_field(some, 0, base_string);
    roots[0] = some;
  }
  base = roots[0];
  roots[1] = caml_copy_string(module_name);
  specifier = roots[1];
  if (qml_import_attributes_to_ocaml(ctx, attributes, &roots[8]) < 0) {
    CAMLreturnT(char *, NULL);
  }

  qml_enter_ocaml_callback(loader->state);
  value result =
    caml_callback3_exn(loader->normalize, base, specifier, roots[8]);
  qml_leave_ocaml_callback(loader->state);
  if (Is_exception_result(result)) {
    char *message = caml_format_exception(Extract_exception(result));
    (void)qml_throw_marked_error(ctx, message, "host_callback_defect", NULL);
    caml_stat_free(message);
    CAMLreturnT(char *, NULL);
  }
  roots[2] = result;

  if (Tag_val(result) == 1) {
    error = Field(result, 0);
    roots[5] = error;
    message = Field(error, 0);
    kind = Field(error, 1);
    denied_specifier = Field(error, 2);
    (void)qml_throw_marked_error(
      ctx, String_val(message), String_val(kind),
      caml_string_length(denied_specifier) == 0
        ? NULL
        : String_val(denied_specifier));
    CAMLreturnT(char *, NULL);
  }

  normalized = Field(result, 0);
  size_t length = caml_string_length(normalized);
  char *copy = js_malloc(ctx, length + 1);
  if (copy == NULL) {
    JS_ThrowOutOfMemory(ctx);
    CAMLreturnT(char *, NULL);
  }
  memcpy(copy, String_val(normalized), length);
  copy[length] = '\0';
  CAMLreturnT(char *, copy);
}

static int qml_check_import_attributes(JSContext *ctx, void *opaque,
                                       JSValueConst attributes)
{
  (void)opaque;
  if (JS_IsUndefined(attributes) || JS_IsNull(attributes)) {
    return 0;
  }

  JSPropertyEnum *properties = NULL;
  uint32_t property_count = 0;
  if (JS_GetOwnPropertyNames(ctx, &properties, &property_count, attributes,
                             JS_GPN_ENUM_ONLY | JS_GPN_STRING_MASK) < 0) {
    return -1;
  }

  for (uint32_t i = 0; i < property_count; i++) {
    size_t name_length = 0;
    const char *name =
      JS_AtomToCStringLen(ctx, &name_length, properties[i].atom);
    if (name == NULL) {
      JS_FreePropertyEnum(ctx, properties, property_count);
      return -1;
    }
    int supported = name_length == 4 && memcmp(name, "type", 4) == 0;
    if (!supported) {
      JS_ThrowTypeError(ctx, "unsupported import attribute: %s", name);
      JS_FreeCString(ctx, name);
      JS_FreePropertyEnum(ctx, properties, property_count);
      return -1;
    }
    JS_FreeCString(ctx, name);
  }
  JS_FreePropertyEnum(ctx, properties, property_count);

  JSValue type = JS_GetPropertyStr(ctx, attributes, "type");
  if (JS_IsException(type)) {
    return -1;
  }
  if (JS_IsUndefined(type) || JS_IsNull(type)) {
    JS_FreeValue(ctx, type);
    return 0;
  }

  const char *type_string = JS_ToCString(ctx, type);
  if (type_string == NULL) {
    JS_FreeValue(ctx, type);
    return -1;
  }
  int ok = strcmp(type_string, "js") == 0 || strcmp(type_string, "json") == 0;
  JS_FreeCString(ctx, type_string);
  JS_FreeValue(ctx, type);

  if (!ok) {
    JS_ThrowTypeError(ctx, "unsupported import attribute type");
    return -1;
  }
  return 0;
}

static int qml_import_attribute_is_json(JSContext *ctx,
                                        JSValueConst attributes,
                                        int *is_json)
{
  *is_json = 0;
  if (JS_IsUndefined(attributes) || JS_IsNull(attributes)) {
    return 0;
  }

  JSValue type = JS_GetPropertyStr(ctx, attributes, "type");
  if (JS_IsException(type)) {
    return -1;
  }
  if (JS_IsUndefined(type) || JS_IsNull(type)) {
    JS_FreeValue(ctx, type);
    return 0;
  }

  const char *type_string = JS_ToCString(ctx, type);
  if (type_string == NULL) {
    JS_FreeValue(ctx, type);
    return -1;
  }
  *is_json = strcmp(type_string, "json") == 0;
  JS_FreeCString(ctx, type_string);
  JS_FreeValue(ctx, type);
  return 0;
}

static JSValue qml_compile_loaded_module(JSContext *ctx, const char *source,
                                         size_t source_length,
                                         const char *module_name, int is_json)
{
  if (!is_json) {
    return JS_Eval(ctx, source, source_length, module_name,
                   JS_EVAL_TYPE_MODULE | JS_EVAL_FLAG_COMPILE_ONLY);
  }

  static const char prefix[] = "export default ";
  static const char suffix[] = ";";
  size_t prefix_length = sizeof(prefix) - 1;
  size_t suffix_length = sizeof(suffix) - 1;
  size_t wrapped_length = prefix_length + source_length + suffix_length;
  char *wrapped_source = caml_stat_alloc(wrapped_length + 1);
  memcpy(wrapped_source, prefix, prefix_length);
  memcpy(wrapped_source + prefix_length, source, source_length);
  memcpy(wrapped_source + prefix_length + source_length, suffix, suffix_length);
  wrapped_source[wrapped_length] = '\0';

  JSValue compiled = JS_Eval(ctx, wrapped_source, wrapped_length, module_name,
                             JS_EVAL_TYPE_MODULE |
                               JS_EVAL_FLAG_COMPILE_ONLY);
  caml_stat_free(wrapped_source);
  return compiled;
}

static JSModuleDef *qml_load_module(JSContext *ctx, const char *module_name,
                                    void *opaque, JSValueConst attributes)
{
  struct qml_module_loader *loader = opaque;
  CAMLparam0();
  CAMLlocalN(roots, 7);
  value result;
  value error;
  value message;
  value kind;
  value denied_specifier;
  value source_option;
  value source;

  int is_json = 0;
  if (qml_import_attribute_is_json(ctx, attributes, &is_json) < 0) {
    CAMLreturnT(JSModuleDef *, NULL);
  }

  int denied = qml_module_loader_denies(ctx, loader, module_name);
  if (denied < 0) {
    CAMLreturnT(JSModuleDef *, NULL);
  }
  if (denied) {
    (void)qml_throw_marked_error(ctx, "module policy violation",
                                 "policy_violation", module_name);
    CAMLreturnT(JSModuleDef *, NULL);
  }

  roots[0] = caml_copy_string(module_name);
  if (qml_import_attributes_to_ocaml(ctx, attributes, &roots[1]) < 0) {
    CAMLreturnT(JSModuleDef *, NULL);
  }

  qml_enter_ocaml_callback(loader->state);
  result = caml_callback2_exn(loader->load, roots[0], roots[1]);
  qml_leave_ocaml_callback(loader->state);
  if (Is_exception_result(result)) {
    char *message = caml_format_exception(Extract_exception(result));
    (void)qml_throw_marked_error(ctx, message, "host_callback_defect", NULL);
    caml_stat_free(message);
    CAMLreturnT(JSModuleDef *, NULL);
  }
  roots[2] = result;

  if (Tag_val(result) == 1) {
    error = Field(result, 0);
    roots[3] = error;
    message = Field(error, 0);
    kind = Field(error, 1);
    denied_specifier = Field(error, 2);
    (void)qml_throw_marked_error(
      ctx, String_val(message), String_val(kind),
      caml_string_length(denied_specifier) == 0
        ? NULL
        : String_val(denied_specifier));
    CAMLreturnT(JSModuleDef *, NULL);
  }

  source_option = Field(result, 0);
  roots[4] = source_option;
  if (source_option == Val_int(0)) {
    JS_ThrowReferenceError(ctx, "module not found: %s", module_name);
    CAMLreturnT(JSModuleDef *, NULL);
  }

  source = Field(source_option, 0);
  roots[5] = source;
  JSValue compiled =
    qml_compile_loaded_module(ctx, String_val(source),
                              caml_string_length(source), module_name,
                              is_json);
  if (JS_IsException(compiled)) {
    CAMLreturnT(JSModuleDef *, NULL);
  }
  JSModuleDef *module = JS_VALUE_GET_PTR(compiled);
  JS_FreeValue(ctx, compiled);
  CAMLreturnT(JSModuleDef *, module);
}

static long long qml_monotonic_time_ns(void)
{
  struct timespec time;
  clock_gettime(CLOCK_MONOTONIC, &time);
  return ((long long)time.tv_sec * 1000000000LL) + (long long)time.tv_nsec;
}

static int qml_interrupt_handler(JSRuntime *rt, void *opaque)
{
  (void)rt;
  struct qml_runtime_state *state = opaque;
  if (atomic_load_explicit(&state->interrupt_requested, memory_order_relaxed)) {
    return 1;
  }
  long long deadline =
    atomic_load_explicit(&state->interrupt_deadline_ns, memory_order_relaxed);
  if (deadline > 0 && qml_monotonic_time_ns() >= deadline) {
    atomic_store_explicit(&state->interrupt_requested, 1,
                          memory_order_relaxed);
    return 1;
  }
  return 0;
}

CAMLprim value qml_runtime_create(value unit)
{
  CAMLparam1(unit);
  qml_drain_deferred_runtimes();
  struct qml_runtime_state *state =
    caml_stat_alloc(sizeof(struct qml_runtime_state));
  atomic_init(&state->refs, 1); /* the runtime wrapper's reference */
  state->owner_domain = Caml_state;
  state->rt = NULL;
  atomic_init(&state->native_runtime_alive, 1);
  atomic_init(&state->closed_to_user, 0);
  atomic_init(&state->open_contexts, 0);
  atomic_init(&state->live_values, 0);
  atomic_init(&state->live_atoms, 0);
  atomic_init(&state->ocaml_callback_depth, 0);
  atomic_init(&state->deferred_drain_depth, 0);
  atomic_flag_clear(&state->list_lock);
  state->contexts = NULL;
  state->values = NULL;
  state->atoms = NULL;
  atomic_init(&state->interrupt_requested, 0);
  atomic_init(&state->interrupt_deadline_ns, 0);
  state->runtime_info = NULL;
  state->module_loader = NULL;
  state->promise_rejection_tracking = 0;
  state->promise_rejections_head = NULL;
  state->promise_rejections_tail = NULL;
  state->promise_hook_tracking = 0;
  state->promise_hooks_head = NULL;
  state->promise_hooks_tail = NULL;
  state->host_function_class_id = 0;
  state->host_function_class_registered = 0;
  state->native_module_class_id = 0;
  state->native_module_class_registered = 0;
  atomic_init(&state->deferred_values, NULL);
  atomic_init(&state->deferred_atoms, NULL);
  atomic_init(&state->deferred_contexts, NULL);

  JSRuntime *rt = JS_NewRuntime();
  if (rt == NULL) {
    caml_stat_free(state);
    qml_raise_string("could not allocate QuickJS runtime");
  }
  JS_SetInterruptHandler(rt, qml_interrupt_handler, state);
  CAMLreturn(qml_alloc_runtime(rt, state));
}

CAMLprim value qml_runtime_set_memory_limit(value runtime, value limit)
{
  CAMLparam2(runtime, limit);
  long bytes = Long_val(limit);
  if (bytes < 0) {
    qml_raise_string("memory limit must be non-negative");
  }
  JS_SetMemoryLimit(qml_runtime(runtime), (size_t)bytes);
  CAMLreturn(Val_unit);
}

CAMLprim value qml_runtime_set_max_stack_size(value runtime, value size)
{
  CAMLparam2(runtime, size);
  long bytes = Long_val(size);
  if (bytes < 0) {
    qml_raise_string("max stack size must be non-negative");
  }
  JS_SetMaxStackSize(qml_runtime(runtime), (size_t)bytes);
  CAMLreturn(Val_unit);
}

static void qml_store_int64_field(value array, int index, int64_t field)
{
  Store_field(array, index, caml_copy_int64(field));
}

static void qml_promise_rejection_events_free(struct qml_runtime_state *state)
{
  struct qml_promise_rejection_event *event = state->promise_rejections_head;
  while (event != NULL) {
    struct qml_promise_rejection_event *next = event->next;
    caml_remove_generational_global_root(&event->reason);
    caml_stat_free(event);
    event = next;
  }
  state->promise_rejections_head = NULL;
  state->promise_rejections_tail = NULL;
}

static void qml_promise_hook_events_free(struct qml_runtime_state *state)
{
  struct qml_promise_hook_event *event = state->promise_hooks_head;
  while (event != NULL) {
    struct qml_promise_hook_event *next = event->next;
    caml_stat_free(event);
    event = next;
  }
  state->promise_hooks_head = NULL;
  state->promise_hooks_tail = NULL;
}

void qml_promise_events_free(struct qml_runtime_state *state)
{
  qml_promise_rejection_events_free(state);
  qml_promise_hook_events_free(state);
}

static void qml_append_promise_rejection(struct qml_runtime_state *state,
                                         value reason, int is_handled)
{
  struct qml_promise_rejection_event *event =
    caml_stat_alloc(sizeof(struct qml_promise_rejection_event));
  event->reason = reason;
  event->is_handled = is_handled;
  event->next = NULL;
  caml_register_generational_global_root(&event->reason);
  if (state->promise_rejections_tail == NULL) {
    state->promise_rejections_head = event;
  } else {
    state->promise_rejections_tail->next = event;
  }
  state->promise_rejections_tail = event;
}

static void qml_append_promise_hook(struct qml_runtime_state *state, int kind,
                                    int promise_state, int has_parent)
{
  struct qml_promise_hook_event *event =
    caml_stat_alloc(sizeof(struct qml_promise_hook_event));
  event->kind = kind;
  event->state = promise_state;
  event->has_parent = has_parent;
  event->next = NULL;
  if (state->promise_hooks_tail == NULL) {
    state->promise_hooks_head = event;
  } else {
    state->promise_hooks_tail->next = event;
  }
  state->promise_hooks_tail = event;
}

static void qml_promise_rejection_tracker(JSContext *ctx, JSValueConst promise,
                                          JSValueConst reason, bool is_handled,
                                          void *opaque)
{
  (void)promise;
  CAMLparam0();
  CAMLlocal1(structured);
  struct qml_runtime_state *state = opaque;
  if (state == NULL || !state->promise_rejection_tracking) {
    CAMLreturn0;
  }
  structured = qml_structured_of_exception_value(ctx, reason);
  qml_append_promise_rejection(state, structured, is_handled ? 1 : 0);
  CAMLreturn0;
}

static void qml_promise_hook(JSContext *ctx, JSPromiseHookType type,
                             JSValueConst promise, JSValueConst parent,
                             void *opaque)
{
  struct qml_runtime_state *state = opaque;
  if (state == NULL || !state->promise_hook_tracking) {
    return;
  }
  qml_append_promise_hook(state, (int)type, (int)JS_PromiseState(ctx, promise),
                          JS_IsUndefined(parent) ? 0 : 1);
}

CAMLprim value qml_runtime_memory_usage(value runtime)
{
  CAMLparam1(runtime);
  CAMLlocal1(result);
  JSRuntime *rt = qml_runtime(runtime);
  JSMemoryUsage usage;
  JS_ComputeMemoryUsage(rt, &usage);

  result = caml_alloc(26, 0);
  qml_store_int64_field(result, 0, usage.malloc_size);
  qml_store_int64_field(result, 1, usage.malloc_limit);
  qml_store_int64_field(result, 2, usage.memory_used_size);
  qml_store_int64_field(result, 3, usage.malloc_count);
  qml_store_int64_field(result, 4, usage.memory_used_count);
  qml_store_int64_field(result, 5, usage.atom_count);
  qml_store_int64_field(result, 6, usage.atom_size);
  qml_store_int64_field(result, 7, usage.str_count);
  qml_store_int64_field(result, 8, usage.str_size);
  qml_store_int64_field(result, 9, usage.obj_count);
  qml_store_int64_field(result, 10, usage.obj_size);
  qml_store_int64_field(result, 11, usage.prop_count);
  qml_store_int64_field(result, 12, usage.prop_size);
  qml_store_int64_field(result, 13, usage.shape_count);
  qml_store_int64_field(result, 14, usage.shape_size);
  qml_store_int64_field(result, 15, usage.js_func_count);
  qml_store_int64_field(result, 16, usage.js_func_size);
  qml_store_int64_field(result, 17, usage.js_func_code_size);
  qml_store_int64_field(result, 18, usage.js_func_pc2line_count);
  qml_store_int64_field(result, 19, usage.js_func_pc2line_size);
  qml_store_int64_field(result, 20, usage.c_func_count);
  qml_store_int64_field(result, 21, usage.array_count);
  qml_store_int64_field(result, 22, usage.fast_array_count);
  qml_store_int64_field(result, 23, usage.fast_array_elements);
  qml_store_int64_field(result, 24, usage.binary_object_count);
  qml_store_int64_field(result, 25, usage.binary_object_size);
  CAMLreturn(result);
}

CAMLprim value qml_runtime_run_gc(value runtime)
{
  CAMLparam1(runtime);
  JS_RunGC(qml_runtime(runtime));
  CAMLreturn(Val_unit);
}

CAMLprim value qml_runtime_get_gc_threshold(value runtime)
{
  CAMLparam1(runtime);
  size_t threshold = JS_GetGCThreshold(qml_runtime(runtime));
  if (threshold > (size_t)Max_long) {
    qml_raise_string("GC threshold is outside the OCaml int range");
  }
  CAMLreturn(Val_long((intnat)threshold));
}

CAMLprim value qml_runtime_set_gc_threshold(value runtime, value threshold)
{
  CAMLparam2(runtime, threshold);
  long threshold_value = Long_val(threshold);
  if (threshold_value < 0) {
    qml_raise_string("GC threshold must be non-negative");
  }
  JS_SetGCThreshold(qml_runtime(runtime), (size_t)threshold_value);
  CAMLreturn(Val_unit);
}

CAMLprim value qml_runtime_set_info(value runtime, value info)
{
  CAMLparam2(runtime, info);
  JSRuntime *rt = qml_runtime(runtime);
  struct qml_runtime_state *state = Qml_runtime_val(runtime)->state;
  size_t length = caml_string_length(info);
  if (memchr(String_val(info), '\0', length) != NULL) {
    qml_raise_string("runtime info cannot contain NUL bytes");
  }

  char *copy = caml_stat_alloc(length + 1);
  memcpy(copy, String_val(info), length);
  copy[length] = '\0';

  char *previous = state->runtime_info;
  state->runtime_info = copy;
  JS_SetRuntimeInfo(rt, copy);
  if (previous != NULL) {
    caml_stat_free(previous);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value qml_runtime_set_promise_rejection_tracking(value runtime,
                                                          value enabled)
{
  CAMLparam2(runtime, enabled);
  JSRuntime *rt = qml_runtime(runtime);
  struct qml_runtime_state *state = Qml_runtime_val(runtime)->state;
  if (Bool_val(enabled)) {
    state->promise_rejection_tracking = 1;
    JS_SetHostPromiseRejectionTracker(rt, qml_promise_rejection_tracker, state);
  } else {
    state->promise_rejection_tracking = 0;
    JS_SetHostPromiseRejectionTracker(rt, NULL, NULL);
    qml_promise_rejection_events_free(state);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value qml_runtime_take_promise_rejections(value runtime)
{
  CAMLparam1(runtime);
  CAMLlocal2(result, tuple);
  (void)qml_runtime(runtime);
  struct qml_runtime_state *state = Qml_runtime_val(runtime)->state;
  mlsize_t length = 0;
  for (struct qml_promise_rejection_event *event =
         state->promise_rejections_head;
       event != NULL; event = event->next) {
    length++;
  }

  result = caml_alloc(length, 0);
  struct qml_promise_rejection_event *event = state->promise_rejections_head;
  state->promise_rejections_head = NULL;
  state->promise_rejections_tail = NULL;
  for (mlsize_t i = 0; i < length; i++) {
    struct qml_promise_rejection_event *next = event->next;
    tuple = caml_alloc_tuple(2);
    Store_field(tuple, 0, event->reason);
    Store_field(tuple, 1, Val_bool(event->is_handled));
    Store_field(result, i, tuple);
    caml_remove_generational_global_root(&event->reason);
    caml_stat_free(event);
    event = next;
  }
  CAMLreturn(result);
}

CAMLprim value qml_runtime_set_promise_hook_tracking(value runtime,
                                                     value enabled)
{
  CAMLparam2(runtime, enabled);
  JSRuntime *rt = qml_runtime(runtime);
  struct qml_runtime_state *state = Qml_runtime_val(runtime)->state;
  if (Bool_val(enabled)) {
    state->promise_hook_tracking = 1;
    JS_SetPromiseHook(rt, qml_promise_hook, state);
  } else {
    state->promise_hook_tracking = 0;
    JS_SetPromiseHook(rt, NULL, NULL);
    qml_promise_hook_events_free(state);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value qml_runtime_take_promise_hooks(value runtime)
{
  CAMLparam1(runtime);
  CAMLlocal2(result, tuple);
  (void)qml_runtime(runtime);
  struct qml_runtime_state *state = Qml_runtime_val(runtime)->state;
  mlsize_t length = 0;
  for (struct qml_promise_hook_event *event = state->promise_hooks_head;
       event != NULL; event = event->next) {
    length++;
  }

  result = caml_alloc(length, 0);
  struct qml_promise_hook_event *event = state->promise_hooks_head;
  state->promise_hooks_head = NULL;
  state->promise_hooks_tail = NULL;
  for (mlsize_t i = 0; i < length; i++) {
    struct qml_promise_hook_event *next = event->next;
    tuple = caml_alloc_tuple(3);
    Store_field(tuple, 0, Val_int(event->kind));
    Store_field(tuple, 1, Val_int(event->state));
    Store_field(tuple, 2, Val_bool(event->has_parent));
    Store_field(result, i, tuple);
    caml_stat_free(event);
    event = next;
  }
  CAMLreturn(result);
}

void qml_module_loader_free(struct qml_runtime_state *state)
{
  struct qml_module_loader *loader = state->module_loader;
  if (loader == NULL) {
    return;
  }

  caml_remove_generational_global_root(&loader->deny);
  caml_remove_generational_global_root(&loader->normalize);
  caml_remove_generational_global_root(&loader->load);
  caml_stat_free(loader);
  state->module_loader = NULL;
}

CAMLprim value qml_runtime_install_module_loader(value runtime, value deny,
                                                 value normalize, value load)
{
  CAMLparam4(runtime, deny, normalize, load);
  JSRuntime *rt = qml_runtime(runtime);
  struct qml_runtime_state *state = Qml_runtime_val(runtime)->state;
  qml_module_loader_free(state);

  struct qml_module_loader *loader =
    caml_stat_alloc(sizeof(struct qml_module_loader));
  loader->state = state;
  loader->deny = deny;
  loader->normalize = normalize;
  loader->load = load;

  caml_register_generational_global_root(&loader->deny);
  caml_register_generational_global_root(&loader->normalize);
  caml_register_generational_global_root(&loader->load);
  state->module_loader = loader;
  JS_SetModuleLoaderFunc2(rt, NULL, qml_load_module,
                          qml_check_import_attributes, loader);
  JS_SetModuleNormalizeFunc2(rt, qml_module_loader_normalize);
  CAMLreturn(Val_unit);
}

CAMLprim value qml_runtime_drain_deferred(value runtime)
{
  CAMLparam1(runtime);
  qml_drain_deferred(Qml_runtime_val(runtime)->state);
  CAMLreturn(Val_unit);
}

CAMLprim value qml_runtime_close(value runtime)
{
  CAMLparam1(runtime);
  struct qml_runtime_state *state = Qml_runtime_val(runtime)->state;
  atomic_store_explicit(&state->closed_to_user, 1, memory_order_release);
  if (atomic_load_explicit(&state->native_runtime_alive, memory_order_acquire)
      != 0 && state->rt != NULL) {
    JS_SetHostPromiseRejectionTracker(state->rt, NULL, NULL);
    JS_SetPromiseHook(state->rt, NULL, NULL);
  }
  state->promise_rejection_tracking = 0;
  state->promise_hook_tracking = 0;
  qml_promise_events_free(state);
  qml_drain_deferred(state);
  CAMLreturn(Val_bool(qml_runtime_free_native(state)));
}

CAMLprim value qml_runtime_is_job_pending(value runtime)
{
  CAMLparam1(runtime);
  CAMLreturn(Val_bool(JS_IsJobPending(qml_runtime(runtime))));
}

CAMLprim value qml_runtime_execute_pending_job(value runtime)
{
  CAMLparam1(runtime);
  JSContext *ctx = NULL;
  JSRuntime *rt = qml_runtime(runtime);
  JS_UpdateStackTop(rt);
  int result = JS_ExecutePendingJob(rt, &ctx);
  if (result < 0) {
    if (ctx != NULL) {
      qml_raise_exception(ctx);
    }
    qml_raise_string("QuickJS job failed");
  }
  CAMLreturn(Val_bool(result > 0));
}

CAMLprim value qml_runtime_request_interrupt(value runtime)
{
  atomic_store_explicit(&Qml_runtime_val(runtime)->state->interrupt_requested, 1,
                        memory_order_relaxed);
  return Val_unit;
}

CAMLprim value qml_runtime_set_interrupt_timeout(value runtime, value seconds)
{
  CAMLparam2(runtime, seconds);
  struct qml_runtime_state *state = Qml_runtime_val(runtime)->state;
  (void)qml_runtime(runtime);
  double timeout = Double_val(seconds);
  if (timeout < 0.0) {
    qml_raise_string("timeout must be non-negative");
  }
  long long delta = (long long)(timeout * 1000000000.0);
  long long deadline = qml_monotonic_time_ns() + delta;
  atomic_store_explicit(&state->interrupt_deadline_ns, deadline,
                        memory_order_relaxed);
  CAMLreturn(Val_unit);
}

CAMLprim value qml_runtime_clear_interrupt_timeout(value runtime)
{
  CAMLparam1(runtime);
  struct qml_runtime_state *state = Qml_runtime_val(runtime)->state;
  (void)qml_runtime(runtime);
  atomic_store_explicit(&state->interrupt_deadline_ns, 0,
                        memory_order_relaxed);
  CAMLreturn(Val_unit);
}

CAMLprim value qml_runtime_clear_interrupt(value runtime)
{
  CAMLparam1(runtime);
  struct qml_runtime_state *state = Qml_runtime_val(runtime)->state;
  (void)qml_runtime(runtime);
  atomic_store_explicit(&state->interrupt_requested, 0, memory_order_relaxed);
  atomic_store_explicit(&state->interrupt_deadline_ns, 0,
                        memory_order_relaxed);
  CAMLreturn(Val_unit);
}

CAMLprim value qml_runtime_interrupt_requested(value runtime)
{
  CAMLparam1(runtime);
  struct qml_runtime_state *state = Qml_runtime_val(runtime)->state;
  (void)qml_runtime(runtime);
  CAMLreturn(Val_bool(
    atomic_load_explicit(&state->interrupt_requested, memory_order_relaxed)));
}
