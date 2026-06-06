#include "qmljs_stubs.h"

#include <string.h>

/* Diagnostics count live OCaml wrappers (custom blocks). They increment on
   allocation and decrement in the finalizer, so a full GC returns them to the
   baseline. They measure wrapper leaks, not native bytes. */
static atomic_int qml_live_runtime_wrappers;
static atomic_int qml_live_context_wrappers;
static atomic_int qml_live_value_wrappers;

struct qml_deferred_value {
  struct qml_runtime_state *state;
  JSValue value;
  struct qml_deferred_value *next;
};

struct qml_deferred_context {
  struct qml_runtime_state *state;
  JSContext *ctx;
  struct qml_deferred_context *next;
};

void qml_state_ref(struct qml_runtime_state *state)
{
  atomic_fetch_add_explicit(&state->refs, 1, memory_order_relaxed);
}

void qml_state_unref(struct qml_runtime_state *state)
{
  if (atomic_fetch_sub_explicit(&state->refs, 1, memory_order_acq_rel) == 1) {
    caml_stat_free(state);
  }
}

void qml_ensure_owner(struct qml_runtime_state *state)
{
  if (state == NULL) {
    qml_raise_string("QuickJS runtime state is closed");
  }
  if (Caml_state != state->owner_domain) {
    qml_raise_string("QuickJS runtime used from a non-owner domain");
  }
  /* Deterministic close: once the user has closed the runtime, every handle
     raises, even if escaped value wrappers have delayed the native
     JS_FreeRuntime. native_runtime_alive guards only the internal free path. */
  if (atomic_load_explicit(&state->closed_to_user, memory_order_acquire) != 0
      || atomic_load_explicit(&state->native_runtime_alive,
                              memory_order_acquire) == 0) {
    qml_raise_string("QuickJS runtime is closed");
  }
}

static void qml_defer_value(struct qml_runtime_state *state, JSValue value)
{
  if (atomic_load_explicit(&state->native_runtime_alive, memory_order_acquire)
      == 0) {
    /* Runtime already torn down: the JSValue is gone with it. This branch is
       unreachable in the consistent state machine because JS_FreeRuntime only
       runs when live_values == 0. */
    atomic_fetch_sub_explicit(&state->live_values, 1, memory_order_relaxed);
    return;
  }
  struct qml_deferred_value *node =
    caml_stat_alloc(sizeof(struct qml_deferred_value));
  node->state = state;
  node->value = value;
  qml_state_ref(state);
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
  if (atomic_load_explicit(&state->native_runtime_alive, memory_order_acquire)
      == 0) {
    atomic_fetch_sub_explicit(&state->open_contexts, 1, memory_order_relaxed);
    return;
  }
  struct qml_deferred_context *node =
    caml_stat_alloc(sizeof(struct qml_deferred_context));
  node->state = state;
  node->ctx = ctx;
  qml_state_ref(state);
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
  if (state == NULL || Caml_state != state->owner_domain) {
    /* Finalizers may run on any domain; only the owner domain may touch
       QuickJS. Leave the queues for the owner to drain. */
    return;
  }

  /* Keep state alive across the drain even if the last node releases it. */
  qml_state_ref(state);

  int runtime_alive =
    atomic_load_explicit(&state->native_runtime_alive, memory_order_acquire);

  struct qml_deferred_value *values =
    atomic_exchange_explicit(&state->deferred_values, NULL,
                             memory_order_acquire);
  while (values != NULL) {
    struct qml_deferred_value *next = values->next;
    if (runtime_alive && state->rt != NULL) {
      JS_FreeValueRT(state->rt, values->value);
    }
    atomic_fetch_sub_explicit(&state->live_values, 1, memory_order_relaxed);
    qml_state_unref(values->state);
    caml_stat_free(values);
    values = next;
  }

  struct qml_deferred_context *contexts =
    atomic_exchange_explicit(&state->deferred_contexts, NULL,
                             memory_order_acquire);
  while (contexts != NULL) {
    struct qml_deferred_context *next = contexts->next;
    if (runtime_alive && contexts->ctx != NULL) {
      JS_FreeContext(contexts->ctx);
    }
    atomic_fetch_sub_explicit(&state->open_contexts, 1, memory_order_relaxed);
    qml_state_unref(contexts->state);
    caml_stat_free(contexts);
    contexts = next;
  }

  qml_state_unref(state);
}

int qml_runtime_free_native(struct qml_runtime_state *state)
{
  if (atomic_load_explicit(&state->native_runtime_alive, memory_order_acquire)
      == 0) {
    return 1;
  }
  if (atomic_load_explicit(&state->open_contexts, memory_order_acquire) != 0
      || atomic_load_explicit(&state->live_values, memory_order_acquire) != 0) {
    return 0;
  }
  JS_FreeRuntime(state->rt);
  state->rt = NULL;
  atomic_store_explicit(&state->native_runtime_alive, 0, memory_order_release);
  qml_module_loader_free(state);
  return 1;
}

static void qml_runtime_finalize(value v)
{
  struct qml_runtime *runtime = Qml_runtime_val(v);
  struct qml_runtime_state *state = runtime->state;
  atomic_fetch_sub_explicit(&qml_live_runtime_wrappers, 1, memory_order_relaxed);
  if (state == NULL) {
    return;
  }
  if (Caml_state == state->owner_domain) {
    qml_drain_deferred(state);
    (void)qml_runtime_free_native(state);
  }
  runtime->state = NULL;
  qml_state_unref(state);
}

static void qml_context_finalize(value v)
{
  struct qml_context *context = Qml_context_val(v);
  struct qml_runtime_state *state = context->state;
  atomic_fetch_sub_explicit(&qml_live_context_wrappers, 1, memory_order_relaxed);
  if (state == NULL) {
    return;
  }
  if (context->ctx != NULL) {
    qml_defer_context(state, context->ctx);
    context->ctx = NULL;
  }
  context->state = NULL;
  qml_state_unref(state);
}

static void qml_value_finalize(value v)
{
  struct qml_value *wrapped = Qml_value_val(v);
  struct qml_runtime_state *state = wrapped->state;
  atomic_fetch_sub_explicit(&qml_live_value_wrappers, 1, memory_order_relaxed);
  if (state == NULL) {
    return;
  }
  qml_defer_value(state, wrapped->value);
  wrapped->state = NULL;
  qml_state_unref(state);
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
  /* state->refs was initialized to 1 for this wrapper at creation. */
  state->rt = rt;
  block = caml_alloc_custom(&qml_runtime_ops, sizeof(struct qml_runtime), 0, 1);
  Qml_runtime_val(block)->state = state;
  atomic_fetch_add_explicit(&qml_live_runtime_wrappers, 1, memory_order_relaxed);
  CAMLreturn(block);
}

value qml_alloc_context(value runtime, JSContext *ctx)
{
  CAMLparam1(runtime);
  CAMLlocal1(block);
  struct qml_runtime_state *state = Qml_runtime_val(runtime)->state;
  block = caml_alloc_custom(&qml_context_ops, sizeof(struct qml_context), 0, 1);
  Qml_context_val(block)->ctx = ctx;
  Qml_context_val(block)->state = state;
  qml_state_ref(state);
  atomic_fetch_add_explicit(&state->open_contexts, 1, memory_order_relaxed);
  atomic_fetch_add_explicit(&qml_live_context_wrappers, 1, memory_order_relaxed);
  CAMLreturn(block);
}

value qml_alloc_value(value context, JSValue js_value)
{
  CAMLparam1(context);
  CAMLlocal1(block);
  struct qml_runtime_state *state = Qml_context_val(context)->state;
  block = caml_alloc_custom(&qml_value_ops, sizeof(struct qml_value), 0, 1);
  Qml_value_val(block)->value = js_value;
  Qml_value_val(block)->state = state;
  qml_state_ref(state);
  atomic_fetch_add_explicit(&state->live_values, 1, memory_order_relaxed);
  atomic_fetch_add_explicit(&qml_live_value_wrappers, 1, memory_order_relaxed);
  CAMLreturn(block);
}

CAMLprim value qml_diagnostics_live_runtimes(value unit)
{
  (void)unit;
  return Val_int(
    atomic_load_explicit(&qml_live_runtime_wrappers, memory_order_relaxed));
}

CAMLprim value qml_diagnostics_live_contexts(value unit)
{
  (void)unit;
  return Val_int(
    atomic_load_explicit(&qml_live_context_wrappers, memory_order_relaxed));
}

CAMLprim value qml_diagnostics_live_values(value unit)
{
  (void)unit;
  return Val_int(
    atomic_load_explicit(&qml_live_value_wrappers, memory_order_relaxed));
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

static int qml_error_kind_of_marker(const char *marker)
{
  if (marker == NULL) {
    return 0;
  }
  if (strcmp(marker, "out_of_memory") == 0) {
    return 1;
  }
  if (strcmp(marker, "stack_overflow") == 0) {
    return 2;
  }
  if (strcmp(marker, "resource_limit") == 0 ||
      strcmp(marker, "timeout") == 0 ||
      strcmp(marker, "interrupt") == 0) {
    return 3;
  }
  if (strcmp(marker, "policy_violation") == 0) {
    return 4;
  }
  if (strcmp(marker, "host_callback_defect") == 0) {
    return 5;
  }
  return 0;
}

static int qml_error_kind_of_message(const char *message)
{
  if (message == NULL) {
    return 0;
  }
  if (strcmp(message, "out of memory") == 0) {
    return 1;
  }
  if (strstr(message, "stack overflow") != NULL ||
      strstr(message, "Maximum call stack size exceeded") != NULL) {
    return 2;
  }
  if (strstr(message, "interrupted") != NULL) {
    return 3;
  }
  return 0;
}

static value qml_some(value input)
{
  CAMLparam1(input);
  CAMLlocal1(result);
  result = caml_alloc(1, 0);
  Store_field(result, 0, input);
  CAMLreturn(result);
}

static value qml_build_structured_exception(JSContext *ctx,
                                            JSValueConst js_exception,
                                            int depth,
                                            const value *make_structured)
{
  CAMLparam0();
  CAMLlocalN(args, 7);
  CAMLlocal2(cause, result);
  JSValue js_name = JS_UNDEFINED;
  JSValue js_message = JS_UNDEFINED;
  JSValue js_stack = JS_UNDEFINED;
  JSValue js_ocaml_frame = JS_UNDEFINED;
  JSValue js_kind = JS_UNDEFINED;
  JSValue js_denied_specifier = JS_UNDEFINED;
  JSValue js_cause = JS_UNDEFINED;
  const char *name = NULL;
  const char *message = NULL;
  const char *exception_text = NULL;
  const char *fallback_message = NULL;
  const char *stack = NULL;
  const char *ocaml_frame = NULL;
  const char *kind_marker = NULL;
  const char *denied_specifier = NULL;
  int kind = 0;

  if (JS_IsNull(js_exception)) {
    kind = 1;
    fallback_message = "out of memory";
  }

  if (JS_IsObject(js_exception)) {
    name = qml_property_cstring(ctx, js_exception, "name", &js_name);
    message = qml_property_cstring(ctx, js_exception, "message", &js_message);
    stack = qml_property_cstring(ctx, js_exception, "stack", &js_stack);
    ocaml_frame = qml_property_cstring(ctx, js_exception, "_qmljs_ocaml_frame",
                                       &js_ocaml_frame);
    kind_marker =
      qml_property_cstring(ctx, js_exception, "_qmljs_kind", &js_kind);
    denied_specifier =
      qml_property_cstring(ctx, js_exception, "_qmljs_denied_module_specifier",
                           &js_denied_specifier);

    js_cause = JS_GetPropertyStr(ctx, js_exception, "cause");
    if (JS_IsException(js_cause) || JS_IsUndefined(js_cause) ||
        JS_IsNull(js_cause) || depth >= 16) {
      if (JS_IsException(js_cause)) {
        js_cause = JS_UNDEFINED;
      }
      cause = Val_int(0);
    } else {
      value nested =
        qml_build_structured_exception(ctx, js_cause, depth + 1,
                                       make_structured);
      cause = qml_some(nested);
    }
  } else {
    cause = Val_int(0);
  }

  exception_text = JS_ToCString(ctx, js_exception);
  if ((message == NULL || message[0] == '\0') && exception_text != NULL) {
    fallback_message = exception_text;
  }

  kind = qml_error_kind_of_marker(kind_marker);
  if (kind == 0 && JS_IsNull(js_exception)) {
    kind = 1;
  }
  if (kind == 0) {
    kind = qml_error_kind_of_message(message);
  }
  if (kind == 0) {
    kind = qml_error_kind_of_message(fallback_message);
  }

  args[0] = qml_copy_optional_string(name);
  args[1] =
    qml_copy_optional_string(message != NULL && message[0] != '\0'
                               ? message
                               : fallback_message);
  args[2] = qml_copy_optional_string(stack);
  args[3] = qml_copy_optional_string(ocaml_frame);
  args[4] = Val_int(kind);
  args[5] = qml_copy_optional_string(denied_specifier);
  args[6] = cause;

  if (denied_specifier != NULL) {
    JS_FreeCString(ctx, denied_specifier);
  }
  if (kind_marker != NULL) {
    JS_FreeCString(ctx, kind_marker);
  }
  if (ocaml_frame != NULL) {
    JS_FreeCString(ctx, ocaml_frame);
  }
  if (stack != NULL) {
    JS_FreeCString(ctx, stack);
  }
  if (message != NULL) {
    JS_FreeCString(ctx, message);
  }
  if (name != NULL) {
    JS_FreeCString(ctx, name);
  }
  if (exception_text != NULL) {
    JS_FreeCString(ctx, exception_text);
  }
  JS_FreeValue(ctx, js_cause);
  JS_FreeValue(ctx, js_denied_specifier);
  JS_FreeValue(ctx, js_kind);
  JS_FreeValue(ctx, js_ocaml_frame);
  JS_FreeValue(ctx, js_stack);
  JS_FreeValue(ctx, js_message);
  JS_FreeValue(ctx, js_name);

  result = caml_callbackN_exn(*make_structured, 7, args);
  if (Is_exception_result(result)) {
    caml_raise(Extract_exception(result));
  }
  CAMLreturn(result);
}

void qml_raise_exception(JSContext *ctx)
{
  CAMLparam0();
  CAMLlocal1(structured);
  static const value *make_structured = NULL;
  static const value *raise_structured = NULL;
  JSValue js_exception = JS_GetException(ctx);

  if (make_structured == NULL) {
    make_structured = caml_named_value("qmljs.make_structured");
  }
  if (raise_structured == NULL) {
    raise_structured = caml_named_value("qmljs.raise_structured");
  }
  if (make_structured == NULL || raise_structured == NULL) {
    JS_FreeValue(ctx, js_exception);
    qml_raise_string("JavaScript exception");
  }

  structured = qml_build_structured_exception(ctx, js_exception, 0,
                                              make_structured);
  JS_FreeValue(ctx, js_exception);

  value result = caml_callback_exn(*raise_structured, structured);
  if (Is_exception_result(result)) {
    caml_raise(Extract_exception(result));
  }

  qml_raise_string("JavaScript exception callback returned unexpectedly");
  CAMLreturn0;
}

JSRuntime *qml_runtime(value runtime)
{
  struct qml_runtime_state *state = Qml_runtime_val(runtime)->state;
  qml_ensure_owner(state);
  qml_drain_deferred(state);
  return state->rt;
}

JSContext *qml_context(value context)
{
  struct qml_context *wrapped = Qml_context_val(context);
  qml_ensure_owner(wrapped->state);
  if (wrapped->ctx == NULL) {
    qml_raise_string("QuickJS context is closed");
  }
  qml_drain_deferred(wrapped->state);
  return wrapped->ctx;
}

JSValue qml_js_value(value wrapped)
{
  struct qml_value *wrapped_value = Qml_value_val(wrapped);
  if (wrapped_value->state == NULL) {
    qml_raise_string("QuickJS value is closed");
  }
  qml_ensure_owner(wrapped_value->state);
  return wrapped_value->value;
}

void qml_ensure_same_runtime(struct qml_runtime_state *expected, value wrapped)
{
  struct qml_value *actual = Qml_value_val(wrapped);
  if (actual->state == NULL) {
    qml_raise_string("QuickJS value is closed");
  }
  if (actual->state != expected) {
    qml_raise_string("QuickJS value belongs to a different runtime");
  }
}

void qml_update_stack_top(JSContext *ctx)
{
  JS_UpdateStackTop(JS_GetRuntime(ctx));
}
