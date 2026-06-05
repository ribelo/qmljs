#include "qmljs_stubs.h"

#include <string.h>

static atomic_int qml_live_runtimes;
static atomic_int qml_live_contexts;
static atomic_int qml_live_values;

struct qml_deferred_value {
  JSContext *ctx;
  JSValue value;
  struct qml_deferred_value *next;
};

struct qml_deferred_context {
  JSContext *ctx;
  struct qml_deferred_context *next;
};

void qml_ensure_owner(struct qml_runtime_state *state)
{
  if (state == NULL) {
    qml_raise_string("QuickJS runtime state is closed");
  }
  if (atomic_load_explicit(&state->closed, memory_order_acquire) != 0) {
    qml_raise_string("QuickJS runtime is closed");
  }
  if (Caml_state != state->owner_domain) {
    qml_raise_string("QuickJS runtime used from a non-owner domain");
  }
}

static void qml_defer_value(struct qml_runtime_state *state, JSContext *ctx,
                            JSValue value)
{
  if (state == NULL ||
      atomic_load_explicit(&state->closed, memory_order_acquire) != 0) {
    return;
  }
  struct qml_deferred_value *node =
    caml_stat_alloc(sizeof(struct qml_deferred_value));
  node->ctx = ctx;
  node->value = value;
  struct qml_deferred_value *head =
    atomic_load_explicit(&state->deferred_values, memory_order_acquire);
  do {
    node->next = head;
  } while (!atomic_compare_exchange_weak_explicit(
    &state->deferred_values, &head, node, memory_order_release,
    memory_order_acquire));
}

static void qml_defer_context(struct qml_runtime_state *state, JSContext *ctx)
{
  if (state == NULL ||
      atomic_load_explicit(&state->closed, memory_order_acquire) != 0) {
    return;
  }
  struct qml_deferred_context *node =
    caml_stat_alloc(sizeof(struct qml_deferred_context));
  node->ctx = ctx;
  struct qml_deferred_context *head =
    atomic_load_explicit(&state->deferred_contexts, memory_order_acquire);
  do {
    node->next = head;
  } while (!atomic_compare_exchange_weak_explicit(
    &state->deferred_contexts, &head, node, memory_order_release,
    memory_order_acquire));
}

void qml_drain_deferred(struct qml_runtime_state *state)
{
  qml_ensure_owner(state);

  struct qml_deferred_value *values =
    atomic_exchange_explicit(&state->deferred_values, NULL, memory_order_acquire);
  while (values != NULL) {
    struct qml_deferred_value *next = values->next;
    JS_FreeValue(values->ctx, values->value);
    caml_stat_free(values);
    values = next;
  }

  struct qml_deferred_context *contexts =
    atomic_exchange_explicit(&state->deferred_contexts, NULL, memory_order_acquire);
  while (contexts != NULL) {
    struct qml_deferred_context *next = contexts->next;
    JS_FreeContext(contexts->ctx);
    caml_stat_free(contexts);
    contexts = next;
  }
}

static void qml_runtime_finalize(value v)
{
  struct qml_runtime *runtime = Qml_runtime_val(v);
  if (runtime->rt != NULL) {
    if (runtime->state != NULL &&
        Caml_state == runtime->state->owner_domain) {
      qml_drain_deferred(runtime->state);
      JS_FreeRuntime(runtime->rt);
    }
    runtime->rt = NULL;
  }
  if (runtime->state != NULL) {
    caml_stat_free(runtime->state);
    runtime->state = NULL;
  }
  atomic_fetch_sub_explicit(&qml_live_runtimes, 1, memory_order_relaxed);
}

static void qml_context_finalize(value v)
{
  struct qml_context *context = Qml_context_val(v);
  if (context->ctx != NULL) {
    qml_defer_context(context->state, context->ctx);
    context->ctx = NULL;
  }
  caml_remove_generational_global_root(&context->runtime);
  atomic_fetch_sub_explicit(&qml_live_contexts, 1, memory_order_relaxed);
}

static void qml_value_finalize(value v)
{
  struct qml_value *wrapped = Qml_value_val(v);
  if (wrapped->ctx != NULL) {
    qml_defer_value(wrapped->state, wrapped->ctx, wrapped->value);
    wrapped->ctx = NULL;
  }
  caml_remove_generational_global_root(&wrapped->context);
  atomic_fetch_sub_explicit(&qml_live_values, 1, memory_order_relaxed);
}

static struct custom_operations qml_runtime_ops = {
  "qmljs.runtime",
  qml_runtime_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static struct custom_operations qml_context_ops = {
  "qmljs.context",
  qml_context_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static struct custom_operations qml_value_ops = {
  "qmljs.value",
  qml_value_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

value qml_alloc_runtime(JSRuntime *rt, struct qml_runtime_state *state)
{
  CAMLparam0();
  CAMLlocal1(block);
  block = caml_alloc_custom(&qml_runtime_ops, sizeof(struct qml_runtime), 0, 1);
  Qml_runtime_val(block)->rt = rt;
  Qml_runtime_val(block)->state = state;
  atomic_fetch_add_explicit(&qml_live_runtimes, 1, memory_order_relaxed);
  CAMLreturn(block);
}

value qml_alloc_context(value runtime, JSContext *ctx)
{
  CAMLparam1(runtime);
  CAMLlocal1(block);
  block = caml_alloc_custom(&qml_context_ops, sizeof(struct qml_context), 0, 1);
  Qml_context_val(block)->ctx = ctx;
  Qml_context_val(block)->runtime = runtime;
  Qml_context_val(block)->state = Qml_runtime_val(runtime)->state;
  caml_register_generational_global_root(&Qml_context_val(block)->runtime);
  atomic_fetch_add_explicit(&qml_live_contexts, 1, memory_order_relaxed);
  CAMLreturn(block);
}

value qml_alloc_value(value context, JSValue js_value)
{
  CAMLparam1(context);
  CAMLlocal1(block);
  JSContext *ctx = qml_context(context);
  block = caml_alloc_custom(&qml_value_ops, sizeof(struct qml_value), 0, 1);
  Qml_value_val(block)->ctx = ctx;
  Qml_value_val(block)->value = js_value;
  Qml_value_val(block)->context = context;
  Qml_value_val(block)->state = Qml_context_val(context)->state;
  caml_register_generational_global_root(&Qml_value_val(block)->context);
  atomic_fetch_add_explicit(&qml_live_values, 1, memory_order_relaxed);
  CAMLreturn(block);
}

CAMLprim value qml_diagnostics_live_runtimes(value unit)
{
  (void)unit;
  return Val_int(
    atomic_load_explicit(&qml_live_runtimes, memory_order_relaxed));
}

CAMLprim value qml_diagnostics_live_contexts(value unit)
{
  (void)unit;
  return Val_int(
    atomic_load_explicit(&qml_live_contexts, memory_order_relaxed));
}

CAMLprim value qml_diagnostics_live_values(value unit)
{
  (void)unit;
  return Val_int(atomic_load_explicit(&qml_live_values, memory_order_relaxed));
}

void qml_raise_string(const char *message)
{
  CAMLparam0();
  CAMLlocal1(arg);
  static const value *exception = NULL;
  if (exception == NULL) {
    exception = caml_named_value("qmljs.error");
  }
  arg = caml_copy_string(message);
  caml_raise_with_arg(*exception, arg);
  CAMLreturn0;
}

static const char *qml_property_cstring(JSContext *ctx, JSValueConst object,
                                        const char *name, JSValue *slot)
{
  *slot = JS_GetPropertyStr(ctx, object, name);
  if (JS_IsException(*slot) || JS_IsUndefined(*slot) || JS_IsNull(*slot)) {
    if (JS_IsException(*slot)) {
      *slot = JS_UNDEFINED;
    }
    return NULL;
  }
  return JS_ToCString(ctx, *slot);
}

static value qml_copy_optional_string(const char *text)
{
  return caml_copy_string(text == NULL ? "" : text);
}

void qml_raise_exception(JSContext *ctx)
{
  CAMLparam0();
  CAMLlocalN(args, 8);
  static const value *raise_exception = NULL;
  JSValue js_exception;
  JSValue js_name;
  JSValue js_message;
  JSValue js_stack;
  JSValue js_cause;
  JSValue js_cause_name;
  JSValue js_cause_message;
  JSValue js_cause_stack;
  JSValue js_cause_ocaml_frame;
  const char *name;
  const char *message;
  const char *exception_text;
  const char *stack;
  const char *cause_name;
  const char *cause_message;
  const char *cause_stack;
  const char *cause_ocaml_frame;

  js_exception = JS_GetException(ctx);
  name = qml_property_cstring(ctx, js_exception, "name", &js_name);
  message = qml_property_cstring(ctx, js_exception, "message", &js_message);
  stack = qml_property_cstring(ctx, js_exception, "stack", &js_stack);
  exception_text = JS_ToCString(ctx, js_exception);

  js_cause = JS_GetPropertyStr(ctx, js_exception, "cause");
  if (JS_IsException(js_cause) || JS_IsUndefined(js_cause) ||
      JS_IsNull(js_cause)) {
    js_cause = JS_UNDEFINED;
    cause_name = NULL;
    cause_message = NULL;
    cause_stack = NULL;
    cause_ocaml_frame = NULL;
    js_cause_name = JS_UNDEFINED;
    js_cause_message = JS_UNDEFINED;
    js_cause_stack = JS_UNDEFINED;
    js_cause_ocaml_frame = JS_UNDEFINED;
  } else {
    cause_name = qml_property_cstring(ctx, js_cause, "name", &js_cause_name);
    cause_message =
      qml_property_cstring(ctx, js_cause, "message", &js_cause_message);
    cause_stack = qml_property_cstring(ctx, js_cause, "stack", &js_cause_stack);
    cause_ocaml_frame =
      qml_property_cstring(ctx, js_cause, "_qmljs_ocaml_frame",
                           &js_cause_ocaml_frame);
  }

  if ((message == NULL || message[0] == '\0') && exception_text != NULL) {
    if (message != NULL) {
      JS_FreeCString(ctx, message);
    }
    message = exception_text;
  }

  args[0] = qml_copy_optional_string(name);
  args[1] = qml_copy_optional_string(message);
  args[2] = qml_copy_optional_string(stack);
  args[3] = caml_copy_string("");
  args[4] = qml_copy_optional_string(cause_name);
  args[5] = qml_copy_optional_string(cause_message);
  args[6] = qml_copy_optional_string(cause_stack);
  args[7] = qml_copy_optional_string(cause_ocaml_frame);

  if (cause_ocaml_frame != NULL) {
    JS_FreeCString(ctx, cause_ocaml_frame);
  }
  if (cause_stack != NULL) {
    JS_FreeCString(ctx, cause_stack);
  }
  if (cause_message != NULL) {
    JS_FreeCString(ctx, cause_message);
  }
  if (cause_name != NULL) {
    JS_FreeCString(ctx, cause_name);
  }
  if (stack != NULL) {
    JS_FreeCString(ctx, stack);
  }
  if (message != NULL && message != exception_text) {
    JS_FreeCString(ctx, message);
  }
  if (name != NULL) {
    JS_FreeCString(ctx, name);
  }
  if (exception_text != NULL) {
    JS_FreeCString(ctx, exception_text);
  }
  JS_FreeValue(ctx, js_cause_ocaml_frame);
  JS_FreeValue(ctx, js_cause_stack);
  JS_FreeValue(ctx, js_cause_message);
  JS_FreeValue(ctx, js_cause_name);
  JS_FreeValue(ctx, js_cause);
  JS_FreeValue(ctx, js_stack);
  JS_FreeValue(ctx, js_message);
  JS_FreeValue(ctx, js_name);
  JS_FreeValue(ctx, js_exception);

  if (raise_exception == NULL) {
    raise_exception = caml_named_value("qmljs.raise_js_exception");
  }
  if (raise_exception == NULL) {
    qml_raise_string("JavaScript exception");
  }

  value result = caml_callbackN_exn(*raise_exception, 8, args);
  if (Is_exception_result(result)) {
    caml_raise(Extract_exception(result));
  }

  qml_raise_string("JavaScript exception callback returned unexpectedly");
  CAMLreturn0;
}

JSRuntime *qml_runtime(value runtime)
{
  struct qml_runtime *wrapped = Qml_runtime_val(runtime);
  JSRuntime *rt = wrapped->rt;
  if (rt == NULL) {
    qml_raise_string("QuickJS runtime is closed");
  }
  qml_drain_deferred(wrapped->state);
  return rt;
}

JSContext *qml_context(value context)
{
  struct qml_context *wrapped = Qml_context_val(context);
  JSContext *ctx = wrapped->ctx;
  if (ctx == NULL) {
    qml_raise_string("QuickJS context is closed");
  }
  qml_drain_deferred(wrapped->state);
  return ctx;
}

JSValue qml_js_value(value wrapped)
{
  struct qml_value *wrapped_value = Qml_value_val(wrapped);
  if (wrapped_value->ctx == NULL) {
    qml_raise_string("QuickJS value is closed");
  }
  qml_ensure_owner(wrapped_value->state);
  return wrapped_value->value;
}

void qml_update_stack_top(JSContext *ctx)
{
  JS_UpdateStackTop(JS_GetRuntime(ctx));
}
