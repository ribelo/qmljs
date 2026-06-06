#include "qmljs_stubs.h"

#include <caml/printexc.h>
#include <string.h>
#include <time.h>

struct qml_module_entry {
  char *specifier;
  char *source;
  size_t source_length;
};

struct qml_module_loader {
  value deny;
  struct qml_module_entry *entries;
  mlsize_t entry_count;
};

static char *qml_copy_stat_string(value input, size_t *length)
{
  size_t len = caml_string_length(input);
  char *copy = caml_stat_alloc(len + 1);
  memcpy(copy, String_val(input), len);
  copy[len] = '\0';
  if (length != NULL) {
    *length = len;
  }
  return copy;
}

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

static int qml_module_loader_denies(JSContext *ctx,
                                    struct qml_module_loader *loader,
                                    const char *module_name)
{
  CAMLparam0();
  CAMLlocal1(arg);
  arg = caml_copy_string(module_name);
  value result = caml_callback_exn(loader->deny, arg);
  if (Is_exception_result(result)) {
    char *message = caml_format_exception(Extract_exception(result));
    (void)qml_throw_marked_error(ctx, message, "host_callback_defect", NULL);
    caml_stat_free(message);
    CAMLreturnT(int, -1);
  }
  CAMLreturnT(int, Bool_val(result));
}

static int qml_check_import_attributes(JSContext *ctx, void *opaque,
                                       JSValueConst attributes)
{
  (void)opaque;
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

static JSValue qml_compile_loaded_module(JSContext *ctx,
                                         struct qml_module_entry *entry,
                                         const char *module_name,
                                         int is_json)
{
  if (!is_json) {
    return JS_Eval(ctx, entry->source, entry->source_length, module_name,
                   JS_EVAL_TYPE_MODULE | JS_EVAL_FLAG_COMPILE_ONLY);
  }

  static const char prefix[] = "export default ";
  static const char suffix[] = ";";
  size_t prefix_length = sizeof(prefix) - 1;
  size_t suffix_length = sizeof(suffix) - 1;
  size_t source_length = prefix_length + entry->source_length + suffix_length;
  char *source = caml_stat_alloc(source_length);
  memcpy(source, prefix, prefix_length);
  memcpy(source + prefix_length, entry->source, entry->source_length);
  memcpy(source + prefix_length + entry->source_length, suffix, suffix_length);

  JSValue compiled = JS_Eval(ctx, source, source_length, module_name,
                             JS_EVAL_TYPE_MODULE |
                               JS_EVAL_FLAG_COMPILE_ONLY);
  caml_stat_free(source);
  return compiled;
}

static JSModuleDef *qml_load_module(JSContext *ctx, const char *module_name,
                                    void *opaque, JSValueConst attributes)
{
  struct qml_module_loader *loader = opaque;
  int is_json = 0;
  if (qml_import_attribute_is_json(ctx, attributes, &is_json) < 0) {
    return NULL;
  }

  int denied = qml_module_loader_denies(ctx, loader, module_name);
  if (denied < 0) {
    return NULL;
  }
  if (denied) {
    (void)qml_throw_marked_error(ctx, "module policy violation",
                                 "policy_violation", module_name);
    return NULL;
  }

  for (mlsize_t i = 0; i < loader->entry_count; i++) {
    struct qml_module_entry *entry = &loader->entries[i];
    if (strcmp(entry->specifier, module_name) == 0) {
      JSValue compiled =
        qml_compile_loaded_module(ctx, entry, module_name, is_json);
      if (JS_IsException(compiled)) {
        return NULL;
      }
      JSModuleDef *module = JS_VALUE_GET_PTR(compiled);
      JS_FreeValue(ctx, compiled);
      return module;
    }
  }

  JS_ThrowReferenceError(ctx, "module not found: %s", module_name);
  return NULL;
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
  struct qml_runtime_state *state =
    caml_stat_alloc(sizeof(struct qml_runtime_state));
  atomic_init(&state->refs, 1); /* the runtime wrapper's reference */
  state->owner_domain = Caml_state;
  state->rt = NULL;
  atomic_init(&state->native_runtime_alive, 1);
  atomic_init(&state->closed_to_user, 0);
  atomic_init(&state->open_contexts, 0);
  atomic_init(&state->live_values, 0);
  atomic_flag_clear(&state->list_lock);
  state->contexts = NULL;
  state->values = NULL;
  atomic_init(&state->interrupt_requested, 0);
  atomic_init(&state->interrupt_deadline_ns, 0);
  state->module_loader = NULL;
  atomic_init(&state->deferred_values, NULL);
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

void qml_module_loader_free(struct qml_runtime_state *state)
{
  struct qml_module_loader *loader = state->module_loader;
  if (loader == NULL) {
    return;
  }

  caml_remove_generational_global_root(&loader->deny);
  for (mlsize_t i = 0; i < loader->entry_count; i++) {
    caml_stat_free(loader->entries[i].specifier);
    caml_stat_free(loader->entries[i].source);
  }
  caml_stat_free(loader->entries);
  caml_stat_free(loader);
  state->module_loader = NULL;
}

CAMLprim value qml_runtime_install_module_loader(value runtime, value entries,
                                                 value deny)
{
  CAMLparam3(runtime, entries, deny);
  JSRuntime *rt = qml_runtime(runtime);
  struct qml_runtime_state *state = Qml_runtime_val(runtime)->state;
  qml_module_loader_free(state);

  struct qml_module_loader *loader =
    caml_stat_alloc(sizeof(struct qml_module_loader));
  mlsize_t count = Wosize_val(entries);
  loader->deny = deny;
  loader->entry_count = count;
  loader->entries = caml_stat_alloc(sizeof(struct qml_module_entry) * count);

  for (mlsize_t i = 0; i < count; i++) {
    value pair = Field(entries, i);
    loader->entries[i].specifier = qml_copy_stat_string(Field(pair, 0), NULL);
    loader->entries[i].source =
      qml_copy_stat_string(Field(pair, 1), &loader->entries[i].source_length);
  }

  caml_register_generational_global_root(&loader->deny);
  state->module_loader = loader;
  JS_SetModuleLoaderFunc2(rt, NULL, qml_load_module,
                          qml_check_import_attributes, loader);
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
