#include "qmljs_stubs.h"

#include <stdint.h>
#include <string.h>

CAMLprim value qml_value_undefined(value context)
{
  CAMLparam1(context);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  CAMLreturn(qml_alloc_value(context, JS_UNDEFINED));
}

CAMLprim value qml_value_null(value context)
{
  CAMLparam1(context);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  CAMLreturn(qml_alloc_value(context, JS_NULL));
}

CAMLprim value qml_value_bool(value context, value input)
{
  CAMLparam2(context, input);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  CAMLreturn(qml_alloc_value(context, JS_NewBool(ctx, Bool_val(input))));
}

CAMLprim value qml_value_int(value context, value input)
{
  CAMLparam2(context, input);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  CAMLreturn(qml_alloc_value(context, JS_NewInt64(ctx, Long_val(input))));
}

CAMLprim value qml_value_float(value context, value input)
{
  CAMLparam2(context, input);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  CAMLreturn(qml_alloc_value(context, JS_NewFloat64(ctx, Double_val(input))));
}

CAMLprim value qml_value_string(value context, value input)
{
  CAMLparam2(context, input);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  JSValue result =
    JS_NewStringLen(ctx, String_val(input), caml_string_length(input));
  if (JS_IsException(result)) {
    qml_raise_exception(ctx);
  }
  CAMLreturn(qml_alloc_value(context, result));
}

CAMLprim value qml_object_create(value context)
{
  CAMLparam1(context);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  JSValue result = JS_NewObject(ctx);
  if (JS_IsException(result)) {
    qml_raise_exception(ctx);
  }
  CAMLreturn(qml_alloc_value(context, result));
}

CAMLprim value qml_object_set(value context, value object, value name, value wrapped)
{
  CAMLparam4(context, object, name, wrapped);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  JSValue property_value = JS_DupValue(ctx, qml_js_value(wrapped));
  int result =
    JS_SetPropertyStr(ctx, qml_js_value(object), String_val(name), property_value);
  if (result < 0) {
    qml_raise_exception(ctx);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value qml_object_define_data_property(value context, value object,
                                               value key, value wrapped,
                                               value input_flags)
{
  CAMLparam5(context, object, key, wrapped, input_flags);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  JSAtom atom = JS_ValueToAtom(ctx, qml_js_value(key));
  if (atom == JS_ATOM_NULL) {
    qml_raise_exception(ctx);
  }

  int options = Int_val(input_flags);
  int flags = JS_PROP_HAS_VALUE | JS_PROP_HAS_WRITABLE |
              JS_PROP_HAS_ENUMERABLE | JS_PROP_HAS_CONFIGURABLE;
  if ((options & 1) != 0) {
    flags |= JS_PROP_WRITABLE;
  }
  if ((options & 2) != 0) {
    flags |= JS_PROP_ENUMERABLE;
  }
  if ((options & 4) != 0) {
    flags |= JS_PROP_CONFIGURABLE;
  }

  int result = JS_DefineProperty(ctx, qml_js_value(object), atom,
                                 qml_js_value(wrapped), JS_UNDEFINED,
                                 JS_UNDEFINED, flags);
  JS_FreeAtom(ctx, atom);
  if (result < 0) {
    qml_raise_exception(ctx);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value qml_array_of_values(value context, value values)
{
  CAMLparam2(context, values);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  JSValue result = JS_NewArray(ctx);
  if (JS_IsException(result)) {
    qml_raise_exception(ctx);
  }

  mlsize_t length = Wosize_val(values);
  for (mlsize_t i = 0; i < length; i++) {
    JSValue element = JS_DupValue(ctx, qml_js_value(Field(values, i)));
    if (JS_SetPropertyUint32(ctx, result, (uint32_t)i, element) < 0) {
      JS_FreeValue(ctx, result);
      qml_raise_exception(ctx);
    }
  }

  CAMLreturn(qml_alloc_value(context, result));
}

CAMLprim value qml_symbol_global(value context, value description)
{
  CAMLparam2(context, description);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  JSValue result = JS_NewSymbol(ctx, String_val(description), true);
  if (JS_IsException(result)) {
    qml_raise_exception(ctx);
  }
  CAMLreturn(qml_alloc_value(context, result));
}

CAMLprim value qml_typed_array_uint8_of_bytes_copy(value context, value bytes)
{
  CAMLparam2(context, bytes);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  JSValue result = JS_NewUint8ArrayCopy(
    ctx, (const uint8_t *)Bytes_val(bytes), caml_string_length(bytes));
  if (JS_IsException(result)) {
    qml_raise_exception(ctx);
  }
  CAMLreturn(qml_alloc_value(context, result));
}

CAMLprim value qml_value_to_bool(value context, value wrapped)
{
  CAMLparam2(context, wrapped);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  int result = JS_ToBool(ctx, qml_js_value(wrapped));
  if (result < 0) {
    qml_raise_exception(ctx);
  }
  CAMLreturn(Val_bool(result));
}

CAMLprim value qml_value_to_int(value context, value wrapped)
{
  CAMLparam2(context, wrapped);
  int64_t result = 0;
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  if (JS_ToInt64(ctx, &result, qml_js_value(wrapped)) < 0) {
    qml_raise_exception(ctx);
  }
  if (result < (int64_t)Min_long || result > (int64_t)Max_long) {
    qml_raise_string("JavaScript integer is outside the OCaml int range");
  }
  CAMLreturn(Val_long((intnat)result));
}

CAMLprim value qml_value_to_float(value context, value wrapped)
{
  CAMLparam2(context, wrapped);
  CAMLlocal1(result);
  double number = 0.0;
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
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
  qml_update_stack_top(ctx);
  const char *string = JS_ToCStringLen(ctx, &length, qml_js_value(wrapped));
  if (string == NULL) {
    qml_raise_exception(ctx);
  }
  result = caml_alloc_string(length);
  memcpy(Bytes_val(result), string, length);
  JS_FreeCString(ctx, string);
  CAMLreturn(result);
}

CAMLprim value qml_value_call(value context, value function, value args)
{
  CAMLparam3(context, function, args);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  mlsize_t argc = Wosize_val(args);
  JSValueConst *argv = NULL;
  if (argc > 0) {
    argv = caml_stat_alloc(sizeof(JSValueConst) * argc);
    for (mlsize_t i = 0; i < argc; i++) {
      argv[i] = qml_js_value(Field(args, i));
    }
  }

  JSValue result = JS_Call(ctx, qml_js_value(function), JS_UNDEFINED,
                           (int)argc, argv);
  if (argv != NULL) {
    caml_stat_free(argv);
  }
  if (JS_IsException(result)) {
    qml_raise_exception(ctx);
  }
  CAMLreturn(qml_alloc_value(context, result));
}

CAMLprim value qml_value_is_promise(value wrapped)
{
  CAMLparam1(wrapped);
  CAMLreturn(Val_bool(JS_IsPromise(qml_js_value(wrapped))));
}

CAMLprim value qml_value_promise_state(value context, value wrapped)
{
  CAMLparam2(context, wrapped);
  JSContext *ctx = qml_context(context);
  CAMLreturn(Val_int(JS_PromiseState(ctx, qml_js_value(wrapped))));
}

CAMLprim value qml_value_promise_result(value context, value wrapped)
{
  CAMLparam2(context, wrapped);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  JSValue result = JS_PromiseResult(ctx, qml_js_value(wrapped));
  if (JS_IsException(result)) {
    qml_raise_exception(ctx);
  }
  CAMLreturn(qml_alloc_value(context, result));
}

CAMLprim value qml_promise_create(value context)
{
  CAMLparam1(context);
  CAMLlocal1(result);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  JSValue resolving_functions[2];
  JSValue promise = JS_NewPromiseCapability(ctx, resolving_functions);
  if (JS_IsException(promise)) {
    qml_raise_exception(ctx);
  }

  result = caml_alloc_tuple(3);
  Store_field(result, 0, qml_alloc_value(context, promise));
  Store_field(result, 1, qml_alloc_value(context, resolving_functions[0]));
  Store_field(result, 2, qml_alloc_value(context, resolving_functions[1]));
  CAMLreturn(result);
}
