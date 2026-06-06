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
  qml_ensure_same_context(context, object);
  qml_ensure_same_context(context, wrapped);
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
  qml_ensure_same_context(context, object);
  qml_ensure_same_context(context, key);
  qml_ensure_same_context(context, wrapped);
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

static value qml_option(value input)
{
  CAMLparam1(input);
  CAMLlocal1(result);
  result = caml_alloc(1, 0);
  Store_field(result, 0, input);
  CAMLreturn(result);
}

static value qml_optional_bool(int input)
{
  CAMLparam0();
  CAMLlocal1(result);
  result = caml_alloc(1, 0);
  Store_field(result, 0, Val_bool(input));
  CAMLreturn(result);
}

static value qml_optional_wrapped_value(value context, JSContext *ctx,
                                        JSValueConst js_value)
{
  CAMLparam1(context);
  CAMLlocal2(wrapped, result);
  if (JS_IsUndefined(js_value)) {
    CAMLreturn(Val_int(0));
  }
  wrapped = qml_alloc_value(context, JS_DupValue(ctx, js_value));
  result = qml_option(wrapped);
  CAMLreturn(result);
}

CAMLprim value qml_object_get(value context, value object, value key)
{
  CAMLparam3(context, object, key);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  qml_ensure_same_context(context, object);
  qml_ensure_same_context(context, key);
  JSAtom atom = JS_ValueToAtom(ctx, qml_js_value(key));
  if (atom == JS_ATOM_NULL) {
    qml_raise_exception(ctx);
  }
  JSValue result = JS_GetProperty(ctx, qml_js_value(object), atom);
  JS_FreeAtom(ctx, atom);
  if (JS_IsException(result)) {
    qml_raise_exception(ctx);
  }
  CAMLreturn(qml_alloc_value(context, result));
}

CAMLprim value qml_object_has(value context, value object, value key)
{
  CAMLparam3(context, object, key);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  qml_ensure_same_context(context, object);
  qml_ensure_same_context(context, key);
  JSAtom atom = JS_ValueToAtom(ctx, qml_js_value(key));
  if (atom == JS_ATOM_NULL) {
    qml_raise_exception(ctx);
  }
  int result = JS_HasProperty(ctx, qml_js_value(object), atom);
  JS_FreeAtom(ctx, atom);
  if (result < 0) {
    qml_raise_exception(ctx);
  }
  CAMLreturn(Val_bool(result));
}

CAMLprim value qml_object_delete(value context, value object, value key)
{
  CAMLparam3(context, object, key);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  qml_ensure_same_context(context, object);
  qml_ensure_same_context(context, key);
  JSAtom atom = JS_ValueToAtom(ctx, qml_js_value(key));
  if (atom == JS_ATOM_NULL) {
    qml_raise_exception(ctx);
  }
  int result = JS_DeleteProperty(ctx, qml_js_value(object), atom, 0);
  JS_FreeAtom(ctx, atom);
  if (result < 0) {
    qml_raise_exception(ctx);
  }
  CAMLreturn(Val_bool(result));
}

CAMLprim value qml_object_get_own_property_descriptor(value context,
                                                      value object,
                                                      value key)
{
  CAMLparam3(context, object, key);
  CAMLlocal2(tuple, result);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  qml_ensure_same_context(context, object);
  qml_ensure_same_context(context, key);
  JSAtom atom = JS_ValueToAtom(ctx, qml_js_value(key));
  if (atom == JS_ATOM_NULL) {
    qml_raise_exception(ctx);
  }

  JSPropertyDescriptor descriptor;
  descriptor.flags = 0;
  descriptor.value = JS_UNDEFINED;
  descriptor.getter = JS_UNDEFINED;
  descriptor.setter = JS_UNDEFINED;

  int found = JS_GetOwnProperty(ctx, &descriptor, qml_js_value(object), atom);
  JS_FreeAtom(ctx, atom);
  if (found < 0) {
    qml_raise_exception(ctx);
  }
  if (found == 0) {
    CAMLreturn(Val_int(0));
  }

  tuple = caml_alloc_tuple(6);
  Store_field(tuple, 0, Val_unit);
  Store_field(tuple, 1, Val_unit);
  Store_field(tuple, 2, Val_bool((descriptor.flags & JS_PROP_ENUMERABLE) != 0));
  Store_field(tuple, 3, Val_bool((descriptor.flags & JS_PROP_CONFIGURABLE) != 0));
  Store_field(tuple, 4, Val_unit);
  Store_field(tuple, 5, Val_unit);

  if ((descriptor.flags & JS_PROP_GETSET) == 0) {
    Store_field(tuple, 0,
                qml_optional_wrapped_value(context, ctx, descriptor.value));
    Store_field(tuple, 1,
                qml_optional_bool((descriptor.flags & JS_PROP_WRITABLE) != 0));
  } else {
    Store_field(tuple, 4,
                qml_optional_wrapped_value(context, ctx, descriptor.getter));
    Store_field(tuple, 5,
                qml_optional_wrapped_value(context, ctx, descriptor.setter));
  }

  JS_FreeValue(ctx, descriptor.value);
  JS_FreeValue(ctx, descriptor.getter);
  JS_FreeValue(ctx, descriptor.setter);
  result = qml_option(tuple);
  CAMLreturn(result);
}

CAMLprim value qml_object_keys(value context, value object)
{
  CAMLparam2(context, object);
  CAMLlocal2(result, string);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  qml_ensure_same_context(context, object);

  JSPropertyEnum *properties = NULL;
  uint32_t length = 0;
  if (JS_GetOwnPropertyNames(ctx, &properties, &length, qml_js_value(object),
                             JS_GPN_STRING_MASK) < 0) {
    qml_raise_exception(ctx);
  }

  result = caml_alloc(length, 0);
  for (uint32_t i = 0; i < length; i++) {
    Store_field(result, i, Val_unit);
  }
  for (uint32_t i = 0; i < length; i++) {
    const char *name = JS_AtomToCString(ctx, properties[i].atom);
    if (name == NULL) {
      JS_FreePropertyEnum(ctx, properties, length);
      qml_raise_exception(ctx);
    }
    string = caml_copy_string(name);
    JS_FreeCString(ctx, name);
    Store_field(result, i, string);
  }

  JS_FreePropertyEnum(ctx, properties, length);
  CAMLreturn(result);
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
    qml_ensure_same_context(context, Field(values, i));
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

static uint8_t *qml_uint8_array_data(JSContext *ctx, JSValueConst value,
                                     size_t *length)
{
  if (JS_GetTypedArrayType(value) != JS_TYPED_ARRAY_UINT8) {
    qml_raise_string("value is not a Uint8Array");
  }
  uint8_t *data = JS_GetUint8Array(ctx, length, value);
  if (data == NULL && *length > 0) {
    qml_raise_exception(ctx);
  }
  return data;
}

CAMLprim value qml_typed_array_uint8_length(value context, value wrapped)
{
  CAMLparam2(context, wrapped);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  qml_ensure_same_context(context, wrapped);
  size_t length = 0;
  (void)qml_uint8_array_data(ctx, qml_js_value(wrapped), &length);
  if (length > (size_t)Max_long) {
    qml_raise_string("Uint8Array length is outside the OCaml int range");
  }
  CAMLreturn(Val_long((intnat)length));
}

CAMLprim value qml_typed_array_uint8_get(value context, value wrapped,
                                         value index)
{
  CAMLparam3(context, wrapped, index);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  qml_ensure_same_context(context, wrapped);
  size_t length = 0;
  uint8_t *data = qml_uint8_array_data(ctx, qml_js_value(wrapped), &length);
  long offset = Long_val(index);
  if (offset < 0 || (size_t)offset >= length) {
    qml_raise_string("Uint8Array index out of bounds");
  }
  CAMLreturn(Val_int(data[offset]));
}

CAMLprim value qml_typed_array_uint8_set(value context, value wrapped,
                                         value index, value byte)
{
  CAMLparam4(context, wrapped, index, byte);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  qml_ensure_same_context(context, wrapped);
  long input = Long_val(byte);
  if (input < 0 || input > 255) {
    qml_raise_string("Uint8Array byte is outside 0..255");
  }
  size_t length = 0;
  uint8_t *data = qml_uint8_array_data(ctx, qml_js_value(wrapped), &length);
  long offset = Long_val(index);
  if (offset < 0 || (size_t)offset >= length) {
    qml_raise_string("Uint8Array index out of bounds");
  }
  data[offset] = (uint8_t)input;
  CAMLreturn(Val_unit);
}

CAMLprim value qml_typed_array_uint8_to_bytes_copy(value context, value wrapped)
{
  CAMLparam2(context, wrapped);
  CAMLlocal1(result);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  qml_ensure_same_context(context, wrapped);
  size_t length = 0;
  uint8_t *data = qml_uint8_array_data(ctx, qml_js_value(wrapped), &length);
  if (length > (size_t)Max_long) {
    qml_raise_string("Uint8Array length is outside the OCaml bytes range");
  }
  result = caml_alloc_string(length);
  memcpy(Bytes_val(result), data, length);
  CAMLreturn(result);
}

CAMLprim value qml_json_parse(value context, value input, value filename)
{
  CAMLparam3(context, input, filename);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  JSValue result =
    JS_ParseJSON(ctx, String_val(input), caml_string_length(input),
                 String_val(filename));
  if (JS_IsException(result)) {
    qml_raise_exception(ctx);
  }
  CAMLreturn(qml_alloc_value(context, result));
}

CAMLprim value qml_json_stringify(value context, value wrapped, value input_space)
{
  CAMLparam3(context, wrapped, input_space);
  CAMLlocal1(result);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  qml_ensure_same_context(context, wrapped);
  JSValue space = JS_UNDEFINED;
  if (Is_block(input_space)) {
    space = JS_NewInt32(ctx, Int_val(Field(input_space, 0)));
  }
  JSValue json =
    JS_JSONStringify(ctx, qml_js_value(wrapped), JS_UNDEFINED, space);
  JS_FreeValue(ctx, space);
  if (JS_IsException(json)) {
    qml_raise_exception(ctx);
  }
  size_t length = 0;
  const char *string = JS_ToCStringLen(ctx, &length, json);
  if (string == NULL) {
    JS_FreeValue(ctx, json);
    qml_raise_string("JSON.stringify returned undefined");
  }
  result = caml_alloc_string(length);
  memcpy(Bytes_val(result), string, length);
  JS_FreeCString(ctx, string);
  JS_FreeValue(ctx, json);
  CAMLreturn(result);
}

CAMLprim value qml_value_kind(value context, value wrapped)
{
  CAMLparam2(context, wrapped);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  qml_ensure_same_context(context, wrapped);
  JSValueConst js_value = qml_js_value(wrapped);
  int tag = JS_VALUE_GET_TAG(js_value);
  if (tag == JS_TAG_UNDEFINED) {
    CAMLreturn(Val_int(0));
  }
  if (tag == JS_TAG_NULL) {
    CAMLreturn(Val_int(1));
  }
  if (tag == JS_TAG_BOOL) {
    CAMLreturn(Val_int(2));
  }
  if (tag == JS_TAG_INT) {
    CAMLreturn(Val_int(3));
  }
  if (JS_TAG_IS_FLOAT64(tag)) {
    CAMLreturn(Val_int(4));
  }
  if (JS_IsString(js_value)) {
    CAMLreturn(Val_int(5));
  }
  if (JS_IsSymbol(js_value)) {
    CAMLreturn(Val_int(6));
  }
  if (JS_IsBigInt(js_value)) {
    CAMLreturn(Val_int(7));
  }
  if (JS_IsPromise(js_value)) {
    CAMLreturn(Val_int(11));
  }
  if (JS_IsArray(js_value)) {
    CAMLreturn(Val_int(9));
  }
  if (JS_IsFunction(ctx, js_value)) {
    CAMLreturn(Val_int(10));
  }
  if (JS_GetTypedArrayType(js_value) >= 0) {
    CAMLreturn(Val_int(12));
  }
  if (JS_IsObject(js_value)) {
    CAMLreturn(Val_int(8));
  }
  CAMLreturn(Val_int(8));
}

CAMLprim value qml_value_to_bool(value context, value wrapped)
{
  CAMLparam2(context, wrapped);
  JSContext *ctx = qml_context(context);
  qml_ensure_same_context(context, wrapped);
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
  qml_ensure_same_context(context, wrapped);
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
  qml_ensure_same_context(context, wrapped);
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
  qml_ensure_same_context(context, wrapped);
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
  qml_ensure_same_context(context, function);
  mlsize_t argc = Wosize_val(args);
  JSValueConst *argv = NULL;
  if (argc > 0) {
    argv = caml_stat_alloc(sizeof(JSValueConst) * argc);
    for (mlsize_t i = 0; i < argc; i++) {
      qml_ensure_same_context(context, Field(args, i));
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

CAMLprim value qml_value_strict_equal(value context, value left, value right)
{
  CAMLparam3(context, left, right);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  qml_ensure_same_context(context, left);
  qml_ensure_same_context(context, right);
  CAMLreturn(Val_bool(
    JS_IsStrictEqual(ctx, qml_js_value(left), qml_js_value(right))));
}

CAMLprim value qml_value_same_value(value context, value left, value right)
{
  CAMLparam3(context, left, right);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  qml_ensure_same_context(context, left);
  qml_ensure_same_context(context, right);
  CAMLreturn(Val_bool(
    JS_IsSameValue(ctx, qml_js_value(left), qml_js_value(right))));
}

CAMLprim value qml_value_same_value_zero(value context, value left, value right)
{
  CAMLparam3(context, left, right);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  qml_ensure_same_context(context, left);
  qml_ensure_same_context(context, right);
  CAMLreturn(Val_bool(
    JS_IsSameValueZero(ctx, qml_js_value(left), qml_js_value(right))));
}

CAMLprim value qml_value_is_promise(value context, value wrapped)
{
  CAMLparam2(context, wrapped);
  (void)qml_context(context);
  qml_ensure_same_context(context, wrapped);
  CAMLreturn(Val_bool(JS_IsPromise(qml_js_value(wrapped))));
}

CAMLprim value qml_value_promise_state(value context, value wrapped)
{
  CAMLparam2(context, wrapped);
  JSContext *ctx = qml_context(context);
  qml_ensure_same_context(context, wrapped);
  CAMLreturn(Val_int(JS_PromiseState(ctx, qml_js_value(wrapped))));
}

CAMLprim value qml_value_promise_result(value context, value wrapped)
{
  CAMLparam2(context, wrapped);
  JSContext *ctx = qml_context(context);
  qml_ensure_same_context(context, wrapped);
  qml_update_stack_top(ctx);
  JSValue result = JS_PromiseResult(ctx, qml_js_value(wrapped));
  if (JS_IsException(result)) {
    qml_raise_exception(ctx);
  }
  CAMLreturn(qml_alloc_value(context, result));
}

CAMLprim value qml_value_raise_as_exception(value context, value wrapped)
{
  CAMLparam2(context, wrapped);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  qml_ensure_same_context(context, wrapped);
  qml_raise_value_exception(ctx, qml_js_value(wrapped));
  CAMLreturn(Val_unit);
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
  Store_field(result, 0, Val_unit);
  Store_field(result, 1, Val_unit);
  Store_field(result, 2, Val_unit);
  Store_field(result, 0, qml_alloc_value(context, promise));
  Store_field(result, 1, qml_alloc_value(context, resolving_functions[0]));
  Store_field(result, 2, qml_alloc_value(context, resolving_functions[1]));
  CAMLreturn(result);
}
