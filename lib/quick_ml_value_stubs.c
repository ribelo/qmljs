#include "quick_ml_stubs.h"

#include <stdint.h>
#include <string.h>

CAMLprim value qml_value_undefined(value context)
{
  CAMLparam1(context);
  JSContext *ctx = qml_context(context);
  CAMLreturn(qml_alloc_value(ctx, JS_UNDEFINED));
}

CAMLprim value qml_value_null(value context)
{
  CAMLparam1(context);
  JSContext *ctx = qml_context(context);
  CAMLreturn(qml_alloc_value(ctx, JS_NULL));
}

CAMLprim value qml_value_bool(value context, value input)
{
  CAMLparam2(context, input);
  JSContext *ctx = qml_context(context);
  CAMLreturn(qml_alloc_value(ctx, JS_NewBool(ctx, Bool_val(input))));
}

CAMLprim value qml_value_int(value context, value input)
{
  CAMLparam2(context, input);
  JSContext *ctx = qml_context(context);
  CAMLreturn(qml_alloc_value(ctx, JS_NewInt64(ctx, Long_val(input))));
}

CAMLprim value qml_value_float(value context, value input)
{
  CAMLparam2(context, input);
  JSContext *ctx = qml_context(context);
  CAMLreturn(qml_alloc_value(ctx, JS_NewFloat64(ctx, Double_val(input))));
}

CAMLprim value qml_value_string(value context, value input)
{
  CAMLparam2(context, input);
  JSContext *ctx = qml_context(context);
  JSValue result =
    JS_NewStringLen(ctx, String_val(input), caml_string_length(input));
  if (JS_IsException(result)) {
    qml_raise_exception(ctx);
  }
  CAMLreturn(qml_alloc_value(ctx, result));
}

CAMLprim value qml_value_to_bool(value context, value wrapped)
{
  CAMLparam2(context, wrapped);
  JSContext *ctx = qml_context(context);
  int result = JS_ToBool(ctx, qml_js_value(wrapped));
  if (result < 0) {
    qml_raise_exception(ctx);
  }
  CAMLreturn(Val_bool(result));
}

CAMLprim value qml_value_to_int(value context, value wrapped)
{
  CAMLparam2(context, wrapped);
  int32_t result = 0;
  JSContext *ctx = qml_context(context);
  if (JS_ToInt32(ctx, &result, qml_js_value(wrapped)) < 0) {
    qml_raise_exception(ctx);
  }
  CAMLreturn(Val_long(result));
}

CAMLprim value qml_value_to_float(value context, value wrapped)
{
  CAMLparam2(context, wrapped);
  CAMLlocal1(result);
  double number = 0.0;
  JSContext *ctx = qml_context(context);
  if (JS_ToFloat64(ctx, &number, qml_js_value(wrapped)) < 0) {
    qml_raise_exception(ctx);
  }
  result = caml_copy_double(number);
  CAMLreturn(result);
}

CAMLprim value qml_value_to_string(value context, value wrapped)
{
  CAMLparam2(context, wrapped);
  CAMLlocal1(result);
  size_t length = 0;
  JSContext *ctx = qml_context(context);
  const char *string = JS_ToCStringLen(ctx, &length, qml_js_value(wrapped));
  if (string == NULL) {
    qml_raise_exception(ctx);
  }
  result = caml_alloc_string(length);
  memcpy(Bytes_val(result), string, length);
  JS_FreeCString(ctx, string);
  CAMLreturn(result);
}

CAMLprim value qml_value_is_promise(value wrapped)
{
  return Val_bool(JS_IsPromise(Qml_value_val(wrapped)->value));
}

CAMLprim value qml_value_promise_state(value context, value wrapped)
{
  return Val_int(JS_PromiseState(Qml_context_val(context)->ctx,
                                 Qml_value_val(wrapped)->value));
}

CAMLprim value qml_value_promise_result(value context, value wrapped)
{
  CAMLparam2(context, wrapped);
  JSContext *ctx = qml_context(context);
  JSValue result = JS_PromiseResult(ctx, qml_js_value(wrapped));
  if (JS_IsException(result)) {
    qml_raise_exception(ctx);
  }
  CAMLreturn(qml_alloc_value(ctx, result));
}
