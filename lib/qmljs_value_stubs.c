#include "qmljs_stubs.h"

#include <caml/bigarray.h>

#include <stdint.h>
#include <string.h>

CAMLprim value qmljs_value_undefined(value context)
{
  CAMLparam1(context);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  CAMLreturn(qmljs_alloc_value(context, JS_UNDEFINED));
}

CAMLprim value qmljs_value_null(value context)
{
  CAMLparam1(context);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  CAMLreturn(qmljs_alloc_value(context, JS_NULL));
}

CAMLprim value qmljs_value_bool(value context, value input)
{
  CAMLparam2(context, input);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  CAMLreturn(qmljs_alloc_value(context, JS_NewBool(ctx, Bool_val(input))));
}

CAMLprim value qmljs_value_int(value context, value input)
{
  CAMLparam2(context, input);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  CAMLreturn(qmljs_alloc_value(context, JS_NewInt64(ctx, Long_val(input))));
}

CAMLprim value qmljs_value_float(value context, value input)
{
  CAMLparam2(context, input);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  CAMLreturn(qmljs_alloc_value(context, JS_NewFloat64(ctx, Double_val(input))));
}

CAMLprim value qmljs_value_string(value context, value input)
{
  CAMLparam2(context, input);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  JSValue result =
    JS_NewStringLen(ctx, String_val(input), caml_string_length(input));
  if (JS_IsException(result)) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(qmljs_alloc_value(context, result));
}

CAMLprim value qmljs_object_create(value context)
{
  CAMLparam1(context);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  JSValue result = JS_NewObject(ctx);
  if (JS_IsException(result)) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(qmljs_alloc_value(context, result));
}

CAMLprim value qmljs_object_create_proto(value context, value prototype)
{
  CAMLparam2(context, prototype);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, prototype);
  JSValue result = JS_NewObjectProto(ctx, qmljs_js_value(prototype));
  if (JS_IsException(result)) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(qmljs_alloc_value(context, result));
}

CAMLprim value qmljs_object_set(value context, value object, value name, value wrapped)
{
  CAMLparam4(context, object, name, wrapped);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, object);
  qmljs_ensure_same_context(context, wrapped);
  JSValue property_value = JS_DupValue(ctx, qmljs_js_value(wrapped));
  int result = qmljs_set_property_ocaml_string(ctx, qmljs_js_value(object), name,
                                             property_value);
  if (result < 0) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(Val_unit);
}

static JSAtom qmljs_atom_for_context(value context, value atom)
{
  qmljs_ensure_atom_runtime(qmljs_context_state(context), atom);
  return qmljs_js_atom(atom);
}

static int qmljs_data_property_flags(value input_flags)
{
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
  return flags;
}

static int qmljs_accessor_property_flags(value input_flags)
{
  int options = Int_val(input_flags);
  int flags = JS_PROP_HAS_GET | JS_PROP_HAS_SET |
              JS_PROP_HAS_ENUMERABLE | JS_PROP_HAS_CONFIGURABLE;
  if ((options & 1) != 0) {
    flags |= JS_PROP_ENUMERABLE;
  }
  if ((options & 2) != 0) {
    flags |= JS_PROP_CONFIGURABLE;
  }
  return flags;
}

CAMLprim value qmljs_object_set_atom(value context, value object, value atom,
                                   value wrapped)
{
  CAMLparam4(context, object, atom, wrapped);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, object);
  qmljs_ensure_same_context(context, wrapped);
  JSValue property_value = JS_DupValue(ctx, qmljs_js_value(wrapped));
  int result = JS_SetProperty(ctx, qmljs_js_value(object),
                              qmljs_atom_for_context(context, atom),
                              property_value);
  if (result < 0) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value qmljs_object_define_data_property(value context, value object,
                                               value key, value wrapped,
                                               value input_flags)
{
  CAMLparam5(context, object, key, wrapped, input_flags);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, object);
  qmljs_ensure_same_context(context, key);
  qmljs_ensure_same_context(context, wrapped);
  JSAtom atom = JS_ValueToAtom(ctx, qmljs_js_value(key));
  if (atom == JS_ATOM_NULL) {
    qmljs_raise_exception(ctx);
  }

  int flags = qmljs_data_property_flags(input_flags);

  int result = JS_DefineProperty(ctx, qmljs_js_value(object), atom,
                                 qmljs_js_value(wrapped), JS_UNDEFINED,
                                 JS_UNDEFINED, flags);
  JS_FreeAtom(ctx, atom);
  if (result < 0) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value qmljs_object_define_data_property_atom(value context, value object,
                                                    value atom, value wrapped,
                                                    value input_flags)
{
  CAMLparam5(context, object, atom, wrapped, input_flags);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, object);
  qmljs_ensure_same_context(context, wrapped);

  int result =
    JS_DefineProperty(ctx, qmljs_js_value(object),
                      qmljs_atom_for_context(context, atom),
                      qmljs_js_value(wrapped), JS_UNDEFINED, JS_UNDEFINED,
                      qmljs_data_property_flags(input_flags));
  if (result < 0) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value qmljs_object_define_accessor_property(value context, value object,
                                                   value key, value accessors,
                                                   value input_flags)
{
  CAMLparam5(context, object, key, accessors, input_flags);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, object);
  qmljs_ensure_same_context(context, key);

  value getter_option = Field(accessors, 0);
  value setter_option = Field(accessors, 1);
  JSValueConst getter = JS_UNDEFINED;
  JSValueConst setter = JS_UNDEFINED;

  if (Is_block(getter_option)) {
    value wrapped_getter = Field(getter_option, 0);
    qmljs_ensure_same_context(context, wrapped_getter);
    getter = qmljs_js_value(wrapped_getter);
    if (!JS_IsFunction(ctx, getter)) {
      qmljs_raise_string("getter is not a function");
    }
  }

  if (Is_block(setter_option)) {
    value wrapped_setter = Field(setter_option, 0);
    qmljs_ensure_same_context(context, wrapped_setter);
    setter = qmljs_js_value(wrapped_setter);
    if (!JS_IsFunction(ctx, setter)) {
      qmljs_raise_string("setter is not a function");
    }
  }

  JSAtom atom = JS_ValueToAtom(ctx, qmljs_js_value(key));
  if (atom == JS_ATOM_NULL) {
    qmljs_raise_exception(ctx);
  }

  int flags = qmljs_accessor_property_flags(input_flags);

  int result = JS_DefineProperty(ctx, qmljs_js_value(object), atom,
                                 JS_UNDEFINED, getter, setter, flags);
  JS_FreeAtom(ctx, atom);
  if (result < 0) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value qmljs_object_define_accessor_property_atom(value context,
                                                        value object,
                                                        value atom,
                                                        value accessors,
                                                        value input_flags)
{
  CAMLparam5(context, object, atom, accessors, input_flags);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, object);

  value getter_option = Field(accessors, 0);
  value setter_option = Field(accessors, 1);
  JSValueConst getter = JS_UNDEFINED;
  JSValueConst setter = JS_UNDEFINED;

  if (Is_block(getter_option)) {
    value wrapped_getter = Field(getter_option, 0);
    qmljs_ensure_same_context(context, wrapped_getter);
    getter = qmljs_js_value(wrapped_getter);
    if (!JS_IsFunction(ctx, getter)) {
      qmljs_raise_string("getter is not a function");
    }
  }

  if (Is_block(setter_option)) {
    value wrapped_setter = Field(setter_option, 0);
    qmljs_ensure_same_context(context, wrapped_setter);
    setter = qmljs_js_value(wrapped_setter);
    if (!JS_IsFunction(ctx, setter)) {
      qmljs_raise_string("setter is not a function");
    }
  }

  int result =
    JS_DefineProperty(ctx, qmljs_js_value(object),
                      qmljs_atom_for_context(context, atom), JS_UNDEFINED,
                      getter, setter,
                      qmljs_accessor_property_flags(input_flags));
  if (result < 0) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(Val_unit);
}

static value qmljs_option(value input)
{
  CAMLparam1(input);
  CAMLlocal1(result);
  result = caml_alloc(1, 0);
  Store_field(result, 0, input);
  CAMLreturn(result);
}

static value qmljs_optional_bool(int input)
{
  CAMLparam0();
  CAMLlocal1(result);
  result = caml_alloc(1, 0);
  Store_field(result, 0, Val_bool(input));
  CAMLreturn(result);
}

static value qmljs_optional_wrapped_value(value context, JSContext *ctx,
                                        JSValueConst js_value)
{
  CAMLparam1(context);
  CAMLlocal2(wrapped, result);
  if (JS_IsUndefined(js_value)) {
    CAMLreturn(Val_int(0));
  }
  wrapped = qmljs_alloc_value(context, JS_DupValue(ctx, js_value));
  result = qmljs_option(wrapped);
  CAMLreturn(result);
}

CAMLprim value qmljs_object_get(value context, value object, value key)
{
  CAMLparam3(context, object, key);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, object);
  qmljs_ensure_same_context(context, key);
  JSAtom atom = JS_ValueToAtom(ctx, qmljs_js_value(key));
  if (atom == JS_ATOM_NULL) {
    qmljs_raise_exception(ctx);
  }
  JSValue result = JS_GetProperty(ctx, qmljs_js_value(object), atom);
  JS_FreeAtom(ctx, atom);
  if (JS_IsException(result)) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(qmljs_alloc_value(context, result));
}

CAMLprim value qmljs_object_get_atom(value context, value object, value atom)
{
  CAMLparam3(context, object, atom);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, object);
  JSValue result =
    JS_GetProperty(ctx, qmljs_js_value(object),
                   qmljs_atom_for_context(context, atom));
  if (JS_IsException(result)) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(qmljs_alloc_value(context, result));
}

CAMLprim value qmljs_object_has(value context, value object, value key)
{
  CAMLparam3(context, object, key);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, object);
  qmljs_ensure_same_context(context, key);
  JSAtom atom = JS_ValueToAtom(ctx, qmljs_js_value(key));
  if (atom == JS_ATOM_NULL) {
    qmljs_raise_exception(ctx);
  }
  int result = JS_HasProperty(ctx, qmljs_js_value(object), atom);
  JS_FreeAtom(ctx, atom);
  if (result < 0) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(Val_bool(result));
}

CAMLprim value qmljs_object_has_atom(value context, value object, value atom)
{
  CAMLparam3(context, object, atom);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, object);
  int result =
    JS_HasProperty(ctx, qmljs_js_value(object),
                   qmljs_atom_for_context(context, atom));
  if (result < 0) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(Val_bool(result));
}

CAMLprim value qmljs_object_delete(value context, value object, value key)
{
  CAMLparam3(context, object, key);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, object);
  qmljs_ensure_same_context(context, key);
  JSAtom atom = JS_ValueToAtom(ctx, qmljs_js_value(key));
  if (atom == JS_ATOM_NULL) {
    qmljs_raise_exception(ctx);
  }
  int result = JS_DeleteProperty(ctx, qmljs_js_value(object), atom, 0);
  JS_FreeAtom(ctx, atom);
  if (result < 0) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(Val_bool(result));
}

CAMLprim value qmljs_object_delete_atom(value context, value object, value atom)
{
  CAMLparam3(context, object, atom);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, object);
  int result =
    JS_DeleteProperty(ctx, qmljs_js_value(object),
                      qmljs_atom_for_context(context, atom), 0);
  if (result < 0) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(Val_bool(result));
}

CAMLprim value qmljs_object_get_prototype(value context, value object)
{
  CAMLparam2(context, object);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, object);
  JSValue result = JS_GetPrototype(ctx, qmljs_js_value(object));
  if (JS_IsException(result)) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(qmljs_alloc_value(context, result));
}

CAMLprim value qmljs_object_set_prototype(value context, value object,
                                        value prototype)
{
  CAMLparam3(context, object, prototype);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, object);
  qmljs_ensure_same_context(context, prototype);
  int result =
    JS_SetPrototype(ctx, qmljs_js_value(object), qmljs_js_value(prototype));
  if (result < 0) {
    qmljs_raise_exception(ctx);
  }
  if (result == 0) {
    qmljs_raise_string("could not set object prototype");
  }
  CAMLreturn(Val_unit);
}

CAMLprim value qmljs_object_get_own_property_descriptor(value context,
                                                      value object,
                                                      value key)
{
  CAMLparam3(context, object, key);
  CAMLlocal2(tuple, result);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, object);
  qmljs_ensure_same_context(context, key);
  JSAtom atom = JS_ValueToAtom(ctx, qmljs_js_value(key));
  if (atom == JS_ATOM_NULL) {
    qmljs_raise_exception(ctx);
  }

  JSPropertyDescriptor descriptor;
  descriptor.flags = 0;
  descriptor.value = JS_UNDEFINED;
  descriptor.getter = JS_UNDEFINED;
  descriptor.setter = JS_UNDEFINED;

  int found = JS_GetOwnProperty(ctx, &descriptor, qmljs_js_value(object), atom);
  JS_FreeAtom(ctx, atom);
  if (found < 0) {
    qmljs_raise_exception(ctx);
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
                qmljs_optional_wrapped_value(context, ctx, descriptor.value));
    Store_field(tuple, 1,
                qmljs_optional_bool((descriptor.flags & JS_PROP_WRITABLE) != 0));
  } else {
    Store_field(tuple, 4,
                qmljs_optional_wrapped_value(context, ctx, descriptor.getter));
    Store_field(tuple, 5,
                qmljs_optional_wrapped_value(context, ctx, descriptor.setter));
  }

  JS_FreeValue(ctx, descriptor.value);
  JS_FreeValue(ctx, descriptor.getter);
  JS_FreeValue(ctx, descriptor.setter);
  result = qmljs_option(tuple);
  CAMLreturn(result);
}

CAMLprim value qmljs_object_get_own_property_descriptor_atom(value context,
                                                           value object,
                                                           value atom)
{
  CAMLparam3(context, object, atom);
  CAMLlocal2(tuple, result);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, object);

  JSPropertyDescriptor descriptor;
  descriptor.flags = 0;
  descriptor.value = JS_UNDEFINED;
  descriptor.getter = JS_UNDEFINED;
  descriptor.setter = JS_UNDEFINED;

  int found =
    JS_GetOwnProperty(ctx, &descriptor, qmljs_js_value(object),
                      qmljs_atom_for_context(context, atom));
  if (found < 0) {
    qmljs_raise_exception(ctx);
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
                qmljs_optional_wrapped_value(context, ctx, descriptor.value));
    Store_field(tuple, 1,
                qmljs_optional_bool((descriptor.flags & JS_PROP_WRITABLE) != 0));
  } else {
    Store_field(tuple, 4,
                qmljs_optional_wrapped_value(context, ctx, descriptor.getter));
    Store_field(tuple, 5,
                qmljs_optional_wrapped_value(context, ctx, descriptor.setter));
  }

  JS_FreeValue(ctx, descriptor.value);
  JS_FreeValue(ctx, descriptor.getter);
  JS_FreeValue(ctx, descriptor.setter);
  result = qmljs_option(tuple);
  CAMLreturn(result);
}

CAMLprim value qmljs_object_keys(value context, value object)
{
  CAMLparam2(context, object);
  CAMLlocal2(result, string);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, object);

  JSPropertyEnum *properties = NULL;
  uint32_t length = 0;
  if (JS_GetOwnPropertyNames(ctx, &properties, &length, qmljs_js_value(object),
                             JS_GPN_STRING_MASK) < 0) {
    qmljs_raise_exception(ctx);
  }

  result = caml_alloc(length, 0);
  for (uint32_t i = 0; i < length; i++) {
    Store_field(result, i, Val_unit);
  }
  for (uint32_t i = 0; i < length; i++) {
    const char *name = JS_AtomToCString(ctx, properties[i].atom);
    if (name == NULL) {
      JS_FreePropertyEnum(ctx, properties, length);
      qmljs_raise_exception(ctx);
    }
    string = caml_copy_string(name);
    JS_FreeCString(ctx, name);
    Store_field(result, i, string);
  }

  JS_FreePropertyEnum(ctx, properties, length);
  CAMLreturn(result);
}

CAMLprim value qmljs_object_own_keys(value context, value object, value input_flags)
{
  CAMLparam3(context, object, input_flags);
  CAMLlocal2(result, key);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, object);

  int options = Int_val(input_flags);
  int flags = 0;
  if ((options & 1) != 0) {
    flags |= JS_GPN_STRING_MASK;
  }
  if ((options & 2) != 0) {
    flags |= JS_GPN_SYMBOL_MASK;
  }
  if ((options & 4) != 0) {
    flags |= JS_GPN_ENUM_ONLY;
  }

  JSPropertyEnum *properties = NULL;
  uint32_t length = 0;
  if (JS_GetOwnPropertyNames(ctx, &properties, &length, qmljs_js_value(object),
                             flags) < 0) {
    qmljs_raise_exception(ctx);
  }

  result = caml_alloc(length, 0);
  for (uint32_t i = 0; i < length; i++) {
    Store_field(result, i, Val_unit);
  }
  for (uint32_t i = 0; i < length; i++) {
    JSValue js_key = JS_AtomToValue(ctx, properties[i].atom);
    if (JS_IsException(js_key)) {
      JS_FreePropertyEnum(ctx, properties, length);
      qmljs_raise_exception(ctx);
    }
    key = qmljs_alloc_value(context, js_key);
    Store_field(result, i, key);
  }

  JS_FreePropertyEnum(ctx, properties, length);
  CAMLreturn(result);
}

CAMLprim value qmljs_atom_of_string(value context, value name)
{
  CAMLparam2(context, name);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  JSAtom atom =
    JS_NewAtomLen(ctx, String_val(name), caml_string_length(name));
  if (atom == JS_ATOM_NULL) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(qmljs_alloc_atom(qmljs_context_state(context), atom));
}

CAMLprim value qmljs_atom_of_uint32(value context, value input)
{
  CAMLparam2(context, input);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  int64_t index = Int64_val(input);
  if (index < 0 || index > UINT32_MAX) {
    qmljs_raise_string("atom index is outside the uint32 range");
  }
  JSAtom atom = JS_NewAtomUInt32(ctx, (uint32_t)index);
  if (atom == JS_ATOM_NULL) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(qmljs_alloc_atom(qmljs_context_state(context), atom));
}

CAMLprim value qmljs_atom_to_string(value context, value atom)
{
  CAMLparam2(context, atom);
  CAMLlocal1(result);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  size_t length = 0;
  const char *name =
    JS_AtomToCStringLen(ctx, &length, qmljs_atom_for_context(context, atom));
  if (name == NULL) {
    qmljs_raise_exception(ctx);
  }
  result = caml_alloc_string(length);
  memcpy(Bytes_val(result), name, length);
  JS_FreeCString(ctx, name);
  CAMLreturn(result);
}

CAMLprim value qmljs_atom_to_value(value context, value atom)
{
  CAMLparam2(context, atom);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  JSValue result = JS_AtomToValue(ctx, qmljs_atom_for_context(context, atom));
  if (JS_IsException(result)) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(qmljs_alloc_value(context, result));
}

CAMLprim value qmljs_proxy_create(value context, value target, value handler)
{
  CAMLparam3(context, target, handler);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, target);
  qmljs_ensure_same_context(context, handler);
  JSValue result = JS_NewProxy(ctx, qmljs_js_value(target), qmljs_js_value(handler));
  if (JS_IsException(result)) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(qmljs_alloc_value(context, result));
}

CAMLprim value qmljs_proxy_is(value context, value proxy)
{
  CAMLparam2(context, proxy);
  (void)qmljs_context(context);
  qmljs_ensure_same_context(context, proxy);
  CAMLreturn(Val_bool(JS_IsProxy(qmljs_js_value(proxy))));
}

CAMLprim value qmljs_proxy_target(value context, value proxy)
{
  CAMLparam2(context, proxy);
  CAMLlocal2(wrapped, result);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, proxy);
  if (!JS_IsProxy(qmljs_js_value(proxy))) {
    CAMLreturn(Val_int(0));
  }
  JSValue target = JS_GetProxyTarget(ctx, qmljs_js_value(proxy));
  if (JS_IsException(target)) {
    qmljs_raise_exception(ctx);
  }
  wrapped = qmljs_alloc_value(context, target);
  result = qmljs_option(wrapped);
  CAMLreturn(result);
}

CAMLprim value qmljs_proxy_handler(value context, value proxy)
{
  CAMLparam2(context, proxy);
  CAMLlocal2(wrapped, result);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, proxy);
  if (!JS_IsProxy(qmljs_js_value(proxy))) {
    CAMLreturn(Val_int(0));
  }
  JSValue handler = JS_GetProxyHandler(ctx, qmljs_js_value(proxy));
  if (JS_IsException(handler)) {
    qmljs_raise_exception(ctx);
  }
  wrapped = qmljs_alloc_value(context, handler);
  result = qmljs_option(wrapped);
  CAMLreturn(result);
}

CAMLprim value qmljs_array_create(value context)
{
  CAMLparam1(context);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  JSValue result = JS_NewArray(ctx);
  if (JS_IsException(result)) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(qmljs_alloc_value(context, result));
}

static void qmljs_ensure_array(JSValueConst value)
{
  if (!JS_IsArray(value)) {
    qmljs_raise_string("value is not an Array");
  }
}

static uint32_t qmljs_array_index(value index)
{
  long offset = Long_val(index);
  if (offset < 0) {
    qmljs_raise_string("array index is negative");
  }
  if ((uint64_t)offset > UINT32_MAX) {
    qmljs_raise_string("array index is outside the uint32 range");
  }
  return (uint32_t)offset;
}

static int64_t qmljs_array_length_checked(JSContext *ctx, JSValueConst array)
{
  int64_t length = 0;
  if (JS_GetLength(ctx, array, &length) < 0) {
    qmljs_raise_exception(ctx);
  }
  if (length < 0) {
    qmljs_raise_string("array length is negative");
  }
  if (length > (int64_t)Max_long) {
    qmljs_raise_string("array length is outside the OCaml int range");
  }
  return length;
}

CAMLprim value qmljs_array_of_values(value context, value values)
{
  CAMLparam2(context, values);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  mlsize_t length = Wosize_val(values);
  JSValueConst *elements = NULL;
  if (length > 0) {
    elements = qmljs_alloc_argv((size_t)length);
    for (mlsize_t i = 0; i < length; i++) {
      qmljs_argv_set(context, elements, (size_t)i, Field(values, i));
    }
  }

  JSValue result = JS_NewArray(ctx);
  if (JS_IsException(result)) {
    qmljs_free_argv(elements);
    qmljs_raise_exception(ctx);
  }

  for (mlsize_t i = 0; i < length; i++) {
    JSValue element = JS_DupValue(ctx, elements[i]);
    if (JS_SetPropertyUint32(ctx, result, (uint32_t)i, element) < 0) {
      qmljs_free_argv(elements);
      JS_FreeValue(ctx, result);
      qmljs_raise_exception(ctx);
    }
  }
  qmljs_free_argv(elements);

  CAMLreturn(qmljs_alloc_value(context, result));
}

CAMLprim value qmljs_array_length(value context, value array)
{
  CAMLparam2(context, array);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, array);
  qmljs_ensure_array(qmljs_js_value(array));
  int64_t length = qmljs_array_length_checked(ctx, qmljs_js_value(array));
  CAMLreturn(Val_long((intnat)length));
}

CAMLprim value qmljs_array_get(value context, value array, value index)
{
  CAMLparam3(context, array, index);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, array);
  qmljs_ensure_array(qmljs_js_value(array));
  JSValue result =
    JS_GetPropertyUint32(ctx, qmljs_js_value(array), qmljs_array_index(index));
  if (JS_IsException(result)) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(qmljs_alloc_value(context, result));
}

CAMLprim value qmljs_array_set(value context, value array, value index,
                             value wrapped)
{
  CAMLparam4(context, array, index, wrapped);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, array);
  qmljs_ensure_same_context(context, wrapped);
  qmljs_ensure_array(qmljs_js_value(array));
  JSValue element = JS_DupValue(ctx, qmljs_js_value(wrapped));
  if (JS_SetPropertyUint32(ctx, qmljs_js_value(array), qmljs_array_index(index),
                           element) < 0) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value qmljs_array_push(value context, value array, value values)
{
  CAMLparam3(context, array, values);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, array);
  qmljs_ensure_array(qmljs_js_value(array));

  mlsize_t count = Wosize_val(values);
  for (mlsize_t i = 0; i < count; i++) {
    qmljs_ensure_same_context(context, Field(values, i));
  }

  int64_t length = qmljs_array_length_checked(ctx, qmljs_js_value(array));
  uint64_t new_length = (uint64_t)length + (uint64_t)count;
  if (new_length > UINT32_MAX) {
    qmljs_raise_string("array length would exceed the JavaScript limit");
  }

  for (mlsize_t i = 0; i < count; i++) {
    JSValue element = JS_DupValue(ctx, qmljs_js_value(Field(values, i)));
    if (JS_SetPropertyUint32(ctx, qmljs_js_value(array),
                             (uint32_t)((uint64_t)length + (uint64_t)i),
                             element) < 0) {
      qmljs_raise_exception(ctx);
    }
  }

  length = qmljs_array_length_checked(ctx, qmljs_js_value(array));
  CAMLreturn(Val_long((intnat)length));
}

CAMLprim value qmljs_array_buffer_of_bytes_copy(value context, value bytes)
{
  CAMLparam2(context, bytes);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  JSValue result = JS_NewArrayBufferCopy(
    ctx, (const uint8_t *)Bytes_val(bytes), caml_string_length(bytes));
  if (JS_IsException(result)) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(qmljs_alloc_value(context, result));
}

CAMLprim value qmljs_array_buffer_of_bigarray_copy(value context, value bigarray)
{
  CAMLparam2(context, bigarray);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  struct caml_ba_array *ba = Caml_ba_array_val(bigarray);
  if (ba->num_dims != 1) {
    qmljs_raise_string("ArrayBuffer bigarray source must be one-dimensional");
  }
  if ((ba->flags & CAML_BA_KIND_MASK) != CAML_BA_UINT8) {
    qmljs_raise_string("ArrayBuffer bigarray source must have unsigned-byte elements");
  }
  if ((ba->flags & CAML_BA_LAYOUT_MASK) != CAML_BA_C_LAYOUT) {
    qmljs_raise_string("ArrayBuffer bigarray source must use C layout");
  }

  uintnat length = caml_ba_byte_size(ba);
  JSValue result = JS_NewArrayBufferCopy(
    ctx, (const uint8_t *)Caml_ba_data_val(bigarray), (size_t)length);
  if (JS_IsException(result)) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(qmljs_alloc_value(context, result));
}

static uint8_t *qmljs_array_buffer_data(JSContext *ctx, JSValueConst value,
                                      size_t *length)
{
  if (!JS_IsArrayBuffer(value)) {
    qmljs_raise_string("value is not an ArrayBuffer");
  }
  *length = 0;
  uint8_t *data = JS_GetArrayBuffer(ctx, length, value);
  if (data == NULL) {
    qmljs_raise_string("ArrayBuffer is detached");
  }
  return data;
}

CAMLprim value qmljs_array_buffer_byte_length(value context, value wrapped)
{
  CAMLparam2(context, wrapped);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, wrapped);
  size_t length = 0;
  (void)qmljs_array_buffer_data(ctx, qmljs_js_value(wrapped), &length);
  if (length > (size_t)Max_long) {
    qmljs_raise_string("ArrayBuffer length is outside the OCaml int range");
  }
  CAMLreturn(Val_long((intnat)length));
}

CAMLprim value qmljs_array_buffer_to_bytes_copy(value context, value wrapped)
{
  CAMLparam2(context, wrapped);
  CAMLlocal1(result);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, wrapped);
  size_t length = 0;
  uint8_t *data = qmljs_array_buffer_data(ctx, qmljs_js_value(wrapped), &length);
  if (length > (size_t)Max_long) {
    qmljs_raise_string("ArrayBuffer length is outside the OCaml bytes range");
  }
  result = caml_alloc_string(length);
  memcpy(Bytes_val(result), data, length);
  CAMLreturn(result);
}

CAMLprim value qmljs_array_buffer_detach(value context, value wrapped)
{
  CAMLparam2(context, wrapped);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, wrapped);
  if (!JS_IsArrayBuffer(qmljs_js_value(wrapped))) {
    qmljs_raise_string("value is not an ArrayBuffer");
  }
  JS_DetachArrayBuffer(ctx, qmljs_js_value(wrapped));
  CAMLreturn(Val_unit);
}

CAMLprim value qmljs_symbol_global(value context, value description)
{
  CAMLparam2(context, description);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  JSValue global = JS_GetGlobalObject(ctx);
  if (JS_IsException(global)) {
    qmljs_raise_exception(ctx);
  }
  JSValue symbol = JS_GetPropertyStr(ctx, global, "Symbol");
  JS_FreeValue(ctx, global);
  if (JS_IsException(symbol)) {
    qmljs_raise_exception(ctx);
  }
  JSValue for_ = JS_GetPropertyStr(ctx, symbol, "for");
  JS_FreeValue(ctx, symbol);
  if (JS_IsException(for_)) {
    qmljs_raise_exception(ctx);
  }
  if (!JS_IsFunction(ctx, for_)) {
    JS_FreeValue(ctx, for_);
    qmljs_raise_string("Symbol.for is not available");
  }

  JSValue argument =
    JS_NewStringLen(ctx, String_val(description), caml_string_length(description));
  if (JS_IsException(argument)) {
    JS_FreeValue(ctx, for_);
    qmljs_raise_exception(ctx);
  }

  JSValueConst argv[1] = { argument };
  JSValue result = JS_Call(ctx, for_, JS_UNDEFINED, 1, argv);
  JS_FreeValue(ctx, argument);
  JS_FreeValue(ctx, for_);
  if (JS_IsException(result)) {
    qmljs_raise_exception(ctx);
  }
  if (!JS_IsSymbol(result)) {
    JS_FreeValue(ctx, result);
    qmljs_raise_string("Symbol.for did not return a symbol");
  }
  CAMLreturn(qmljs_alloc_value(context, result));
}

CAMLprim value qmljs_symbol_well_known(value context, value name)
{
  CAMLparam2(context, name);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  JSValue global = JS_GetGlobalObject(ctx);
  if (JS_IsException(global)) {
    qmljs_raise_exception(ctx);
  }
  JSValue symbol = JS_GetPropertyStr(ctx, global, "Symbol");
  JS_FreeValue(ctx, global);
  if (JS_IsException(symbol)) {
    qmljs_raise_exception(ctx);
  }
  JSValue result = JS_GetPropertyStr(ctx, symbol, String_val(name));
  JS_FreeValue(ctx, symbol);
  if (JS_IsException(result)) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(qmljs_alloc_value(context, result));
}

CAMLprim value qmljs_typed_array_uint8_of_bytes_copy(value context, value bytes)
{
  CAMLparam2(context, bytes);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  JSValue result = JS_NewUint8ArrayCopy(
    ctx, (const uint8_t *)Bytes_val(bytes), caml_string_length(bytes));
  if (JS_IsException(result)) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(qmljs_alloc_value(context, result));
}

CAMLprim value qmljs_typed_array_uint8_of_array_buffer(value context,
                                                     value array_buffer,
                                                     value byte_offset,
                                                     value input_length)
{
  CAMLparam4(context, array_buffer, byte_offset, input_length);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, array_buffer);
  if (!JS_IsArrayBuffer(qmljs_js_value(array_buffer))) {
    qmljs_raise_string("value is not an ArrayBuffer");
  }

  long offset = Long_val(byte_offset);
  if (offset < 0) {
    qmljs_raise_string("Uint8Array byte offset is negative");
  }

  JSValue args[3];
  int argc = 2;
  args[0] = qmljs_js_value(array_buffer);
  args[1] = JS_NewInt64(ctx, offset);
  args[2] = JS_UNDEFINED;
  if (JS_IsException(args[1])) {
    qmljs_raise_exception(ctx);
  }

  if (Is_block(input_length)) {
    long length = Long_val(Field(input_length, 0));
    if (length < 0) {
      JS_FreeValue(ctx, args[1]);
      qmljs_raise_string("Uint8Array length is negative");
    }
    args[2] = JS_NewInt64(ctx, length);
    if (JS_IsException(args[2])) {
      JS_FreeValue(ctx, args[1]);
      qmljs_raise_exception(ctx);
    }
    argc = 3;
  }

  JSValue result = JS_NewTypedArray(ctx, argc, args, JS_TYPED_ARRAY_UINT8);
  JS_FreeValue(ctx, args[2]);
  JS_FreeValue(ctx, args[1]);
  if (JS_IsException(result)) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(qmljs_alloc_value(context, result));
}

static const char *qmljs_typed_array_name(int array_type)
{
  switch (array_type) {
  case JS_TYPED_ARRAY_UINT8C:
    return "Uint8ClampedArray";
  case JS_TYPED_ARRAY_INT8:
    return "Int8Array";
  case JS_TYPED_ARRAY_UINT8:
    return "Uint8Array";
  case JS_TYPED_ARRAY_INT16:
    return "Int16Array";
  case JS_TYPED_ARRAY_UINT16:
    return "Uint16Array";
  case JS_TYPED_ARRAY_INT32:
    return "Int32Array";
  case JS_TYPED_ARRAY_UINT32:
    return "Uint32Array";
  case JS_TYPED_ARRAY_BIG_INT64:
    return "BigInt64Array";
  case JS_TYPED_ARRAY_BIG_UINT64:
    return "BigUint64Array";
  case JS_TYPED_ARRAY_FLOAT32:
    return "Float32Array";
  case JS_TYPED_ARRAY_FLOAT64:
    return "Float64Array";
  default:
    return NULL;
  }
}

static void qmljs_ensure_typed_array_type(JSValueConst value, int array_type)
{
  const char *name = qmljs_typed_array_name(array_type);
  if (name == NULL) {
    qmljs_raise_string("unsupported typed array type");
  }
  if (JS_GetTypedArrayType(value) != array_type) {
    qmljs_raise_string("value is not the expected typed array type");
  }
}

CAMLprim value qmljs_typed_array_of_array_buffer(value context, value input_type,
                                               value array_buffer,
                                               value byte_offset,
                                               value input_length)
{
  CAMLparam5(context, input_type, array_buffer, byte_offset, input_length);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, array_buffer);
  if (!JS_IsArrayBuffer(qmljs_js_value(array_buffer))) {
    qmljs_raise_string("value is not an ArrayBuffer");
  }
  int array_type = Int_val(input_type);
  if (qmljs_typed_array_name(array_type) == NULL) {
    qmljs_raise_string("unsupported typed array type");
  }

  long offset = Long_val(byte_offset);
  if (offset < 0) {
    qmljs_raise_string("typed array byte offset is negative");
  }

  JSValue args[3];
  int argc = 2;
  args[0] = qmljs_js_value(array_buffer);
  args[1] = JS_NewInt64(ctx, offset);
  args[2] = JS_UNDEFINED;
  if (JS_IsException(args[1])) {
    qmljs_raise_exception(ctx);
  }

  if (Is_block(input_length)) {
    long length = Long_val(Field(input_length, 0));
    if (length < 0) {
      JS_FreeValue(ctx, args[1]);
      qmljs_raise_string("typed array length is negative");
    }
    args[2] = JS_NewInt64(ctx, length);
    if (JS_IsException(args[2])) {
      JS_FreeValue(ctx, args[1]);
      qmljs_raise_exception(ctx);
    }
    argc = 3;
  }

  JSValue result = JS_NewTypedArray(ctx, argc, args, array_type);
  JS_FreeValue(ctx, args[2]);
  JS_FreeValue(ctx, args[1]);
  if (JS_IsException(result)) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(qmljs_alloc_value(context, result));
}

static uint8_t *qmljs_uint8_array_data(JSContext *ctx, JSValueConst value,
                                     size_t *length)
{
  if (JS_GetTypedArrayType(value) != JS_TYPED_ARRAY_UINT8) {
    qmljs_raise_string("value is not a Uint8Array");
  }
  uint8_t *data = JS_GetUint8Array(ctx, length, value);
  if (data == NULL && *length > 0) {
    qmljs_raise_exception(ctx);
  }
  return data;
}

static JSValue qmljs_uint8_array_buffer(JSContext *ctx, JSValueConst value,
                                      size_t *byte_offset,
                                      size_t *byte_length)
{
  if (JS_GetTypedArrayType(value) != JS_TYPED_ARRAY_UINT8) {
    qmljs_raise_string("value is not a Uint8Array");
  }
  size_t bytes_per_element = 0;
  JSValue buffer =
    JS_GetTypedArrayBuffer(ctx, value, byte_offset, byte_length,
                           &bytes_per_element);
  if (JS_IsException(buffer)) {
    qmljs_raise_exception(ctx);
  }
  if (bytes_per_element != 1) {
    JS_FreeValue(ctx, buffer);
    qmljs_raise_string("Uint8Array has an unexpected element size");
  }
  return buffer;
}

static JSValue qmljs_typed_array_buffer_checked(JSContext *ctx, JSValueConst value,
                                              int array_type,
                                              size_t *byte_offset,
                                              size_t *byte_length,
                                              size_t *bytes_per_element)
{
  qmljs_ensure_typed_array_type(value, array_type);
  JSValue buffer =
    JS_GetTypedArrayBuffer(ctx, value, byte_offset, byte_length,
                           bytes_per_element);
  if (JS_IsException(buffer)) {
    qmljs_raise_exception(ctx);
  }
  if (*bytes_per_element == 0) {
    JS_FreeValue(ctx, buffer);
    qmljs_raise_string("typed array has an invalid element size");
  }
  return buffer;
}

static size_t qmljs_typed_array_element_length(JSContext *ctx, JSValueConst value,
                                             int array_type)
{
  size_t byte_offset = 0;
  size_t byte_length = 0;
  size_t bytes_per_element = 0;
  JSValue buffer =
    qmljs_typed_array_buffer_checked(ctx, value, array_type, &byte_offset,
                                   &byte_length, &bytes_per_element);
  JS_FreeValue(ctx, buffer);
  return byte_length / bytes_per_element;
}

static uint32_t qmljs_typed_array_index(JSContext *ctx, JSValueConst js_value,
                                      int array_type, value index)
{
  long offset = Long_val(index);
  if (offset < 0) {
    qmljs_raise_string("typed array index is negative");
  }
  size_t length = qmljs_typed_array_element_length(ctx, js_value, array_type);
  if ((uint64_t)offset >= length) {
    qmljs_raise_string("typed array index out of bounds");
  }
  return (uint32_t)offset;
}

CAMLprim value qmljs_typed_array_length(value context, value input_type,
                                      value wrapped)
{
  CAMLparam3(context, input_type, wrapped);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, wrapped);
  size_t length =
    qmljs_typed_array_element_length(ctx, qmljs_js_value(wrapped),
                                   Int_val(input_type));
  if (length > (size_t)Max_long) {
    qmljs_raise_string("typed array length is outside the OCaml int range");
  }
  CAMLreturn(Val_long((intnat)length));
}

CAMLprim value qmljs_typed_array_byte_offset(value context, value input_type,
                                           value wrapped)
{
  CAMLparam3(context, input_type, wrapped);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, wrapped);
  size_t byte_offset = 0;
  size_t byte_length = 0;
  size_t bytes_per_element = 0;
  JSValue buffer =
    qmljs_typed_array_buffer_checked(ctx, qmljs_js_value(wrapped),
                                   Int_val(input_type), &byte_offset,
                                   &byte_length, &bytes_per_element);
  JS_FreeValue(ctx, buffer);
  if (byte_offset > (size_t)Max_long) {
    qmljs_raise_string("typed array byte offset is outside the OCaml int range");
  }
  CAMLreturn(Val_long((intnat)byte_offset));
}

CAMLprim value qmljs_typed_array_byte_length(value context, value input_type,
                                           value wrapped)
{
  CAMLparam3(context, input_type, wrapped);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, wrapped);
  size_t byte_offset = 0;
  size_t byte_length = 0;
  size_t bytes_per_element = 0;
  JSValue buffer =
    qmljs_typed_array_buffer_checked(ctx, qmljs_js_value(wrapped),
                                   Int_val(input_type), &byte_offset,
                                   &byte_length, &bytes_per_element);
  JS_FreeValue(ctx, buffer);
  if (byte_length > (size_t)Max_long) {
    qmljs_raise_string("typed array byte length is outside the OCaml int range");
  }
  CAMLreturn(Val_long((intnat)byte_length));
}

CAMLprim value qmljs_typed_array_buffer(value context, value input_type,
                                      value wrapped)
{
  CAMLparam3(context, input_type, wrapped);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, wrapped);
  size_t byte_offset = 0;
  size_t byte_length = 0;
  size_t bytes_per_element = 0;
  JSValue buffer =
    qmljs_typed_array_buffer_checked(ctx, qmljs_js_value(wrapped),
                                   Int_val(input_type), &byte_offset,
                                   &byte_length, &bytes_per_element);
  CAMLreturn(qmljs_alloc_value(context, buffer));
}

CAMLprim value qmljs_typed_array_get_int(value context, value input_type,
                                       value wrapped, value index)
{
  CAMLparam4(context, input_type, wrapped, index);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, wrapped);
  uint32_t offset =
    qmljs_typed_array_index(ctx, qmljs_js_value(wrapped), Int_val(input_type),
                          index);
  JSValue element = JS_GetPropertyUint32(ctx, qmljs_js_value(wrapped), offset);
  if (JS_IsException(element)) {
    qmljs_raise_exception(ctx);
  }
  int64_t result = 0;
  if (JS_ToInt64(ctx, &result, element) < 0) {
    JS_FreeValue(ctx, element);
    qmljs_raise_exception(ctx);
  }
  JS_FreeValue(ctx, element);
  if (result < (int64_t)Min_long || result > (int64_t)Max_long) {
    qmljs_raise_string("typed array integer is outside the OCaml int range");
  }
  CAMLreturn(Val_long((intnat)result));
}

CAMLprim value qmljs_typed_array_set_int(value context, value input_type,
                                       value wrapped, value index,
                                       value input)
{
  CAMLparam5(context, input_type, wrapped, index, input);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, wrapped);
  uint32_t offset =
    qmljs_typed_array_index(ctx, qmljs_js_value(wrapped), Int_val(input_type),
                          index);
  JSValue element = JS_NewInt64(ctx, Long_val(input));
  if (JS_IsException(element)) {
    qmljs_raise_exception(ctx);
  }
  if (JS_SetPropertyUint32(ctx, qmljs_js_value(wrapped), offset, element) < 0) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value qmljs_typed_array_get_float(value context, value input_type,
                                         value wrapped, value index)
{
  CAMLparam4(context, input_type, wrapped, index);
  CAMLlocal1(result);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, wrapped);
  uint32_t offset =
    qmljs_typed_array_index(ctx, qmljs_js_value(wrapped), Int_val(input_type),
                          index);
  JSValue element = JS_GetPropertyUint32(ctx, qmljs_js_value(wrapped), offset);
  if (JS_IsException(element)) {
    qmljs_raise_exception(ctx);
  }
  double number = 0.0;
  if (JS_ToFloat64(ctx, &number, element) < 0) {
    JS_FreeValue(ctx, element);
    qmljs_raise_exception(ctx);
  }
  JS_FreeValue(ctx, element);
  result = caml_copy_double(number);
  CAMLreturn(result);
}

CAMLprim value qmljs_typed_array_set_float(value context, value input_type,
                                         value wrapped, value index,
                                         value input)
{
  CAMLparam5(context, input_type, wrapped, index, input);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, wrapped);
  uint32_t offset =
    qmljs_typed_array_index(ctx, qmljs_js_value(wrapped), Int_val(input_type),
                          index);
  JSValue element = JS_NewFloat64(ctx, Double_val(input));
  if (JS_IsException(element)) {
    qmljs_raise_exception(ctx);
  }
  if (JS_SetPropertyUint32(ctx, qmljs_js_value(wrapped), offset, element) < 0) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value qmljs_typed_array_get_bigint(value context, value input_type,
                                          value wrapped, value index)
{
  CAMLparam4(context, input_type, wrapped, index);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, wrapped);
  uint32_t offset =
    qmljs_typed_array_index(ctx, qmljs_js_value(wrapped), Int_val(input_type),
                          index);
  JSValue result = JS_GetPropertyUint32(ctx, qmljs_js_value(wrapped), offset);
  if (JS_IsException(result)) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(qmljs_alloc_value(context, result));
}

CAMLprim value qmljs_typed_array_set_bigint(value context, value input_type,
                                          value wrapped, value index,
                                          value input)
{
  CAMLparam5(context, input_type, wrapped, index, input);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, wrapped);
  qmljs_ensure_same_context(context, input);
  if (!JS_IsBigInt(qmljs_js_value(input))) {
    qmljs_raise_string("typed array BigInt value is not a BigInt");
  }
  uint32_t offset =
    qmljs_typed_array_index(ctx, qmljs_js_value(wrapped), Int_val(input_type),
                          index);
  JSValue element = JS_DupValue(ctx, qmljs_js_value(input));
  if (JS_SetPropertyUint32(ctx, qmljs_js_value(wrapped), offset, element) < 0) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value qmljs_typed_array_uint8_length(value context, value wrapped)
{
  CAMLparam2(context, wrapped);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, wrapped);
  size_t length = 0;
  (void)qmljs_uint8_array_data(ctx, qmljs_js_value(wrapped), &length);
  if (length > (size_t)Max_long) {
    qmljs_raise_string("Uint8Array length is outside the OCaml int range");
  }
  CAMLreturn(Val_long((intnat)length));
}

CAMLprim value qmljs_typed_array_uint8_byte_offset(value context, value wrapped)
{
  CAMLparam2(context, wrapped);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, wrapped);
  size_t byte_offset = 0;
  size_t byte_length = 0;
  JSValue buffer =
    qmljs_uint8_array_buffer(ctx, qmljs_js_value(wrapped), &byte_offset,
                           &byte_length);
  JS_FreeValue(ctx, buffer);
  if (byte_offset > (size_t)Max_long) {
    qmljs_raise_string("Uint8Array byte offset is outside the OCaml int range");
  }
  CAMLreturn(Val_long((intnat)byte_offset));
}

CAMLprim value qmljs_typed_array_uint8_byte_length(value context, value wrapped)
{
  CAMLparam2(context, wrapped);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, wrapped);
  size_t byte_offset = 0;
  size_t byte_length = 0;
  JSValue buffer =
    qmljs_uint8_array_buffer(ctx, qmljs_js_value(wrapped), &byte_offset,
                           &byte_length);
  JS_FreeValue(ctx, buffer);
  if (byte_length > (size_t)Max_long) {
    qmljs_raise_string("Uint8Array byte length is outside the OCaml int range");
  }
  CAMLreturn(Val_long((intnat)byte_length));
}

CAMLprim value qmljs_typed_array_uint8_buffer(value context, value wrapped)
{
  CAMLparam2(context, wrapped);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, wrapped);
  size_t byte_offset = 0;
  size_t byte_length = 0;
  JSValue buffer =
    qmljs_uint8_array_buffer(ctx, qmljs_js_value(wrapped), &byte_offset,
                           &byte_length);
  CAMLreturn(qmljs_alloc_value(context, buffer));
}

CAMLprim value qmljs_typed_array_uint8_get(value context, value wrapped,
                                         value index)
{
  CAMLparam3(context, wrapped, index);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, wrapped);
  size_t length = 0;
  uint8_t *data = qmljs_uint8_array_data(ctx, qmljs_js_value(wrapped), &length);
  long offset = Long_val(index);
  if (offset < 0 || (size_t)offset >= length) {
    qmljs_raise_string("Uint8Array index out of bounds");
  }
  CAMLreturn(Val_int(data[offset]));
}

CAMLprim value qmljs_typed_array_uint8_set(value context, value wrapped,
                                         value index, value byte)
{
  CAMLparam4(context, wrapped, index, byte);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, wrapped);
  long input = Long_val(byte);
  if (input < 0 || input > 255) {
    qmljs_raise_string("Uint8Array byte is outside 0..255");
  }
  size_t length = 0;
  uint8_t *data = qmljs_uint8_array_data(ctx, qmljs_js_value(wrapped), &length);
  long offset = Long_val(index);
  if (offset < 0 || (size_t)offset >= length) {
    qmljs_raise_string("Uint8Array index out of bounds");
  }
  data[offset] = (uint8_t)input;
  CAMLreturn(Val_unit);
}

CAMLprim value qmljs_typed_array_uint8_to_bytes_copy(value context, value wrapped)
{
  CAMLparam2(context, wrapped);
  CAMLlocal1(result);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, wrapped);
  size_t length = 0;
  uint8_t *data = qmljs_uint8_array_data(ctx, qmljs_js_value(wrapped), &length);
  if (length > (size_t)Max_long) {
    qmljs_raise_string("Uint8Array length is outside the OCaml bytes range");
  }
  result = caml_alloc_string(length);
  memcpy(Bytes_val(result), data, length);
  CAMLreturn(result);
}

CAMLprim value qmljs_json_parse(value context, value input, value filename)
{
  CAMLparam3(context, input, filename);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  JSValue result =
    JS_ParseJSON(ctx, String_val(input), caml_string_length(input),
                 String_val(filename));
  if (JS_IsException(result)) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(qmljs_alloc_value(context, result));
}

CAMLprim value qmljs_json_stringify(value context, value wrapped, value input_space)
{
  CAMLparam3(context, wrapped, input_space);
  CAMLlocal1(result);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, wrapped);
  JSValue space = JS_UNDEFINED;
  if (Is_block(input_space)) {
    space = JS_NewInt32(ctx, Int_val(Field(input_space, 0)));
  }
  JSValue json =
    JS_JSONStringify(ctx, qmljs_js_value(wrapped), JS_UNDEFINED, space);
  JS_FreeValue(ctx, space);
  if (JS_IsException(json)) {
    qmljs_raise_exception(ctx);
  }
  if (JS_IsUndefined(json)) {
    JS_FreeValue(ctx, json);
    qmljs_raise_string("JSON.stringify returned undefined");
  }
  size_t length = 0;
  const char *string = JS_ToCStringLen(ctx, &length, json);
  if (string == NULL) {
    JS_FreeValue(ctx, json);
    qmljs_raise_string("JSON.stringify returned undefined");
  }
  result = caml_alloc_string(length);
  memcpy(Bytes_val(result), string, length);
  JS_FreeCString(ctx, string);
  JS_FreeValue(ctx, json);
  CAMLreturn(result);
}

CAMLprim value qmljs_value_kind(value context, value wrapped)
{
  CAMLparam2(context, wrapped);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, wrapped);
  JSValueConst js_value = qmljs_js_value(wrapped);
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

CAMLprim value qmljs_value_to_bool(value context, value wrapped)
{
  CAMLparam2(context, wrapped);
  JSContext *ctx = qmljs_context(context);
  qmljs_ensure_same_context(context, wrapped);
  qmljs_update_stack_top(ctx);
  int result = JS_ToBool(ctx, qmljs_js_value(wrapped));
  if (result < 0) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(Val_bool(result));
}

CAMLprim value qmljs_value_to_int(value context, value wrapped)
{
  CAMLparam2(context, wrapped);
  int64_t result = 0;
  JSContext *ctx = qmljs_context(context);
  qmljs_ensure_same_context(context, wrapped);
  qmljs_update_stack_top(ctx);
  if (JS_ToInt64(ctx, &result, qmljs_js_value(wrapped)) < 0) {
    qmljs_raise_exception(ctx);
  }
  if (result < (int64_t)Min_long || result > (int64_t)Max_long) {
    qmljs_raise_string("JavaScript integer is outside the OCaml int range");
  }
  CAMLreturn(Val_long((intnat)result));
}

CAMLprim value qmljs_value_to_float(value context, value wrapped)
{
  CAMLparam2(context, wrapped);
  CAMLlocal1(result);
  double number = 0.0;
  JSContext *ctx = qmljs_context(context);
  qmljs_ensure_same_context(context, wrapped);
  qmljs_update_stack_top(ctx);
  if (JS_ToFloat64(ctx, &number, qmljs_js_value(wrapped)) < 0) {
    qmljs_raise_exception(ctx);
  }
  result = caml_copy_double(number);
  CAMLreturn(result);
}

CAMLprim value qmljs_value_to_string(value context, value wrapped)
{
  CAMLparam2(context, wrapped);
  CAMLlocal1(result);
  size_t length = 0;
  JSContext *ctx = qmljs_context(context);
  qmljs_ensure_same_context(context, wrapped);
  qmljs_update_stack_top(ctx);
  const char *string = JS_ToCStringLen(ctx, &length, qmljs_js_value(wrapped));
  if (string == NULL) {
    qmljs_raise_exception(ctx);
  }
  result = caml_alloc_string(length);
  memcpy(Bytes_val(result), string, length);
  JS_FreeCString(ctx, string);
  CAMLreturn(result);
}

CAMLprim value qmljs_bigint_of_string(value context, value input)
{
  CAMLparam2(context, input);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);

  JSValue global = JS_GetGlobalObject(ctx);
  if (JS_IsException(global)) {
    qmljs_raise_exception(ctx);
  }
  JSValue constructor = JS_GetPropertyStr(ctx, global, "BigInt");
  JS_FreeValue(ctx, global);
  if (JS_IsException(constructor)) {
    qmljs_raise_exception(ctx);
  }
  if (!JS_IsFunction(ctx, constructor)) {
    JS_FreeValue(ctx, constructor);
    qmljs_raise_string("BigInt constructor is not available");
  }

  JSValue argument =
    JS_NewStringLen(ctx, String_val(input), caml_string_length(input));
  if (JS_IsException(argument)) {
    JS_FreeValue(ctx, constructor);
    qmljs_raise_exception(ctx);
  }

  JSValueConst argv[1] = { argument };
  JSValue result = JS_Call(ctx, constructor, JS_UNDEFINED, 1, argv);
  JS_FreeValue(ctx, argument);
  JS_FreeValue(ctx, constructor);
  if (JS_IsException(result)) {
    qmljs_raise_exception(ctx);
  }
  if (!JS_IsBigInt(result)) {
    JS_FreeValue(ctx, result);
    qmljs_raise_string("BigInt constructor did not return a BigInt");
  }
  CAMLreturn(qmljs_alloc_value(context, result));
}

CAMLprim value qmljs_value_call(value context, value function, value args)
{
  CAMLparam3(context, function, args);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, function);
  JSValue js_function = qmljs_js_value(function);
  size_t arg_count = Wosize_val(args);
  int argc = qmljs_argc_of_size(arg_count);
  JSValueConst *argv = NULL;
  if (arg_count > 0) {
    for (size_t i = 0; i < arg_count; i++) {
      qmljs_argv_check(context, Field(args, i));
    }
    argv = qmljs_alloc_argv(arg_count);
    for (size_t i = 0; i < arg_count; i++) {
      qmljs_argv_set(context, argv, i, Field(args, i));
    }
  }

  JSValue result = JS_Call(ctx, js_function, JS_UNDEFINED, argc, argv);
  if (argv != NULL) {
    qmljs_free_argv(argv);
  }
  if (JS_IsException(result)) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(qmljs_alloc_value(context, result));
}

CAMLprim value qmljs_value_strict_equal(value context, value left, value right)
{
  CAMLparam3(context, left, right);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, left);
  qmljs_ensure_same_context(context, right);
  CAMLreturn(Val_bool(
    JS_IsStrictEqual(ctx, qmljs_js_value(left), qmljs_js_value(right))));
}

CAMLprim value qmljs_value_same_value(value context, value left, value right)
{
  CAMLparam3(context, left, right);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, left);
  qmljs_ensure_same_context(context, right);
  CAMLreturn(Val_bool(
    JS_IsSameValue(ctx, qmljs_js_value(left), qmljs_js_value(right))));
}

CAMLprim value qmljs_value_same_value_zero(value context, value left, value right)
{
  CAMLparam3(context, left, right);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, left);
  qmljs_ensure_same_context(context, right);
  CAMLreturn(Val_bool(
    JS_IsSameValueZero(ctx, qmljs_js_value(left), qmljs_js_value(right))));
}

CAMLprim value qmljs_value_is_promise(value context, value wrapped)
{
  CAMLparam2(context, wrapped);
  (void)qmljs_context(context);
  qmljs_ensure_same_context(context, wrapped);
  CAMLreturn(Val_bool(JS_IsPromise(qmljs_js_value(wrapped))));
}

CAMLprim value qmljs_value_promise_state(value context, value wrapped)
{
  CAMLparam2(context, wrapped);
  JSContext *ctx = qmljs_context(context);
  qmljs_ensure_same_context(context, wrapped);
  CAMLreturn(Val_int(JS_PromiseState(ctx, qmljs_js_value(wrapped))));
}

CAMLprim value qmljs_value_promise_result(value context, value wrapped)
{
  CAMLparam2(context, wrapped);
  JSContext *ctx = qmljs_context(context);
  qmljs_ensure_same_context(context, wrapped);
  qmljs_update_stack_top(ctx);
  JSValue result = JS_PromiseResult(ctx, qmljs_js_value(wrapped));
  if (JS_IsException(result)) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(qmljs_alloc_value(context, result));
}

CAMLprim value qmljs_value_raise_as_exception(value context, value wrapped)
{
  CAMLparam2(context, wrapped);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, wrapped);
  qmljs_raise_value_exception(ctx, qmljs_js_value(wrapped));
  CAMLreturn(Val_unit);
}

CAMLprim value qmljs_promise_create(value context)
{
  CAMLparam1(context);
  CAMLlocal1(result);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  JSValue resolving_functions[2];
  JSValue promise = JS_NewPromiseCapability(ctx, resolving_functions);
  if (JS_IsException(promise)) {
    qmljs_raise_exception(ctx);
  }

  result = caml_alloc_tuple(3);
  Store_field(result, 0, Val_unit);
  Store_field(result, 1, Val_unit);
  Store_field(result, 2, Val_unit);
  Store_field(result, 0, qmljs_alloc_value(context, promise));
  Store_field(result, 1, qmljs_alloc_value(context, resolving_functions[0]));
  Store_field(result, 2, qmljs_alloc_value(context, resolving_functions[1]));
  CAMLreturn(result);
}
