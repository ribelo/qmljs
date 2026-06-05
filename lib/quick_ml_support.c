#include "quick_ml_stubs.h"

#include <string.h>

static void qml_runtime_finalize(value v)
{
  struct qml_runtime *runtime = Qml_runtime_val(v);
  if (runtime->rt != NULL) {
    JS_FreeRuntime(runtime->rt);
    runtime->rt = NULL;
  }
}

static void qml_context_finalize(value v)
{
  struct qml_context *context = Qml_context_val(v);
  if (context->ctx != NULL) {
    JS_FreeContext(context->ctx);
    context->ctx = NULL;
  }
}

static void qml_value_finalize(value v)
{
  struct qml_value *wrapped = Qml_value_val(v);
  if (wrapped->ctx != NULL) {
    JS_FreeValue(wrapped->ctx, wrapped->value);
    wrapped->ctx = NULL;
  }
}

static struct custom_operations qml_runtime_ops = {
  "quick_ml.runtime",
  qml_runtime_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static struct custom_operations qml_context_ops = {
  "quick_ml.context",
  qml_context_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static struct custom_operations qml_value_ops = {
  "quick_ml.value",
  qml_value_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

value qml_alloc_runtime(JSRuntime *rt)
{
  CAMLparam0();
  CAMLlocal1(block);
  block = caml_alloc_custom(&qml_runtime_ops, sizeof(struct qml_runtime), 0, 1);
  Qml_runtime_val(block)->rt = rt;
  CAMLreturn(block);
}

value qml_alloc_context(JSContext *ctx)
{
  CAMLparam0();
  CAMLlocal1(block);
  block = caml_alloc_custom(&qml_context_ops, sizeof(struct qml_context), 0, 1);
  Qml_context_val(block)->ctx = ctx;
  CAMLreturn(block);
}

value qml_alloc_value(JSContext *ctx, JSValue js_value)
{
  CAMLparam0();
  CAMLlocal1(block);
  block = caml_alloc_custom(&qml_value_ops, sizeof(struct qml_value), 0, 1);
  Qml_value_val(block)->ctx = ctx;
  Qml_value_val(block)->value = js_value;
  CAMLreturn(block);
}

void qml_raise_string(const char *message)
{
  CAMLparam0();
  CAMLlocal1(arg);
  static const value *exception = NULL;
  if (exception == NULL) {
    exception = caml_named_value("quick_ml.error");
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
  const char *exception_text;
  const char *stack;

  if (exception == NULL) {
    exception = caml_named_value("quick_ml.error");
  }

  js_exception = JS_GetException(ctx);
  js_stack = JS_GetPropertyStr(ctx, js_exception, "stack");
  stack = JS_IsException(js_stack) || JS_IsUndefined(js_stack) || JS_IsNull(js_stack)
            ? NULL
            : JS_ToCString(ctx, js_stack);
  exception_text = JS_ToCString(ctx, js_exception);

  if (exception_text != NULL && stack != NULL) {
    size_t exception_length = strlen(exception_text);
    size_t stack_length = strlen(stack);
    arg = caml_alloc_string(exception_length + 1 + stack_length);
    memcpy(Bytes_val(arg), exception_text, exception_length);
    Bytes_val(arg)[exception_length] = '\n';
    memcpy(Bytes_val(arg) + exception_length + 1, stack, stack_length);
  } else {
    const char *message =
      exception_text != NULL ? exception_text :
      stack != NULL ? stack :
      "JavaScript exception";
    arg = caml_copy_string(message);
  }

  if (stack != NULL) {
    JS_FreeCString(ctx, stack);
  }
  if (exception_text != NULL) {
    JS_FreeCString(ctx, exception_text);
  }
  JS_FreeValue(ctx, js_stack);
  JS_FreeValue(ctx, js_exception);

  caml_raise_with_arg(*exception, arg);
  CAMLreturn0;
}

JSRuntime *qml_runtime(value runtime)
{
  JSRuntime *rt = Qml_runtime_val(runtime)->rt;
  if (rt == NULL) {
    qml_raise_string("QuickJS runtime is closed");
  }
  return rt;
}

JSContext *qml_context(value context)
{
  JSContext *ctx = Qml_context_val(context)->ctx;
  if (ctx == NULL) {
    qml_raise_string("QuickJS context is closed");
  }
  return ctx;
}

JSValue qml_js_value(value wrapped)
{
  struct qml_value *wrapped_value = Qml_value_val(wrapped);
  if (wrapped_value->ctx == NULL) {
    qml_raise_string("QuickJS value is closed");
  }
  return wrapped_value->value;
}
