#include "qmljs_stubs.h"

#include <string.h>

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
}

static void qml_context_finalize(value v)
{
  struct qml_context *context = Qml_context_val(v);
  if (context->ctx != NULL) {
    qml_defer_context(context->state, context->ctx);
    context->ctx = NULL;
  }
  caml_remove_generational_global_root(&context->runtime);
}

static void qml_value_finalize(value v)
{
  struct qml_value *wrapped = Qml_value_val(v);
  if (wrapped->ctx != NULL) {
    qml_defer_value(wrapped->state, wrapped->ctx, wrapped->value);
    wrapped->ctx = NULL;
  }
  caml_remove_generational_global_root(&wrapped->context);
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
  CAMLreturn(block);
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

void qml_raise_exception(JSContext *ctx)
{
  CAMLparam0();
  CAMLlocal1(arg);
  static const value *exception = NULL;
  JSValue js_exception;
  JSValue js_stack;
  JSValue js_cause;
  JSValue js_cause_stack;
  const char *exception_text;
  const char *stack;
  const char *cause_text;
  const char *cause_stack;

  if (exception == NULL) {
    exception = caml_named_value("qmljs.error");
  }

  js_exception = JS_GetException(ctx);
  js_stack = JS_GetPropertyStr(ctx, js_exception, "stack");
  stack = JS_IsException(js_stack) || JS_IsUndefined(js_stack) || JS_IsNull(js_stack)
            ? NULL
            : JS_ToCString(ctx, js_stack);
  js_cause = JS_GetPropertyStr(ctx, js_exception, "cause");
  cause_text =
    JS_IsException(js_cause) || JS_IsUndefined(js_cause) || JS_IsNull(js_cause)
      ? NULL
      : JS_ToCString(ctx, js_cause);
  js_cause_stack =
    JS_IsException(js_cause) || JS_IsUndefined(js_cause) || JS_IsNull(js_cause)
      ? JS_UNDEFINED
      : JS_GetPropertyStr(ctx, js_cause, "stack");
  cause_stack =
    JS_IsException(js_cause_stack) || JS_IsUndefined(js_cause_stack) || JS_IsNull(js_cause_stack)
      ? NULL
      : JS_ToCString(ctx, js_cause_stack);
  exception_text = JS_ToCString(ctx, js_exception);

  if (exception_text != NULL || stack != NULL || cause_text != NULL ||
      cause_stack != NULL) {
    const char caused_by[] = "\nCaused by: ";
    size_t total_length = 0;
    if (exception_text != NULL) {
      total_length += strlen(exception_text);
    }
    if (stack != NULL) {
      total_length += (total_length == 0 ? 0 : 1) + strlen(stack);
    }
    if (cause_text != NULL) {
      total_length += sizeof(caused_by) - 1 + strlen(cause_text);
    }
    if (cause_stack != NULL) {
      total_length += 1 + strlen(cause_stack);
    }

    arg = caml_alloc_string(total_length);
    char *base = (char *)Bytes_val(arg);
    char *cursor = base;
    if (exception_text != NULL) {
      size_t length = strlen(exception_text);
      memcpy(cursor, exception_text, length);
      cursor += length;
    }
    if (stack != NULL) {
      if (cursor != base) {
        *cursor++ = '\n';
      }
      size_t length = strlen(stack);
      memcpy(cursor, stack, length);
      cursor += length;
    }
    if (cause_text != NULL) {
      size_t prefix_length = sizeof(caused_by) - 1;
      memcpy(cursor, caused_by, prefix_length);
      cursor += prefix_length;
      size_t length = strlen(cause_text);
      memcpy(cursor, cause_text, length);
      cursor += length;
    }
    if (cause_stack != NULL) {
      *cursor++ = '\n';
      size_t length = strlen(cause_stack);
      memcpy(cursor, cause_stack, length);
    }
  } else {
    arg = caml_copy_string("JavaScript exception");
  }

  if (cause_stack != NULL) {
    JS_FreeCString(ctx, cause_stack);
  }
  if (cause_text != NULL) {
    JS_FreeCString(ctx, cause_text);
  }
  if (stack != NULL) {
    JS_FreeCString(ctx, stack);
  }
  if (exception_text != NULL) {
    JS_FreeCString(ctx, exception_text);
  }
  JS_FreeValue(ctx, js_cause_stack);
  JS_FreeValue(ctx, js_cause);
  JS_FreeValue(ctx, js_stack);
  JS_FreeValue(ctx, js_exception);

  caml_raise_with_arg(*exception, arg);
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
