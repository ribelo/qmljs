#include "qmljs_stubs.h"

#include <caml/printexc.h>
#include <limits.h>
#include <stdint.h>
#include <string.h>

struct qml_callback {
  value fn;
  value context;
  char *name;
  int mode;
};

static void qml_callback_finalize(void *opaque)
{
  struct qml_callback *callback = opaque;
  caml_remove_generational_global_root(&callback->fn);
  caml_remove_generational_global_root(&callback->context);
  caml_stat_free(callback->name);
  caml_stat_free(callback);
}

static JSValue qml_throw_ocaml_exception(JSContext *ctx, value exception,
                                         const char *name)
{
  CAMLparam1(exception);
  CAMLlocal3(message_value, metadata_value, metadata_pair);
  static const value *message_fn = NULL;
  static const value *metadata_fn = NULL;
  char *fallback = NULL;
  const char *message = NULL;
  size_t message_length = 0;

  if (message_fn == NULL) {
    message_fn = caml_named_value("qmljs.host_exception_message");
  }
  if (metadata_fn == NULL) {
    metadata_fn = caml_named_value("qmljs.host_exception_metadata");
  }

  if (message_fn != NULL) {
    value result = caml_callback_exn(*message_fn, exception);
    if (Is_exception_result(result)) {
      fallback = caml_format_exception(Extract_exception(result));
      message = fallback;
      message_length = strlen(fallback);
    } else {
      message_value = result;
      message = String_val(message_value);
      message_length = caml_string_length(message_value);
    }
  } else {
    fallback = caml_format_exception(exception);
    message = fallback;
    message_length = strlen(fallback);
  }

  JSValue error = JS_NewError(ctx);
  if (JS_IsException(error)) {
    if (fallback != NULL) {
      caml_stat_free(fallback);
    }
    CAMLreturnT(JSValue, error);
  }

  JSValue js_message = JS_NewStringLen(ctx, message, message_length);
  if (JS_IsException(js_message)) {
    JS_FreeValue(ctx, error);
    if (fallback != NULL) {
      caml_stat_free(fallback);
    }
    CAMLreturnT(JSValue, JS_EXCEPTION);
  }
  if (JS_DefinePropertyValueStr(ctx, error, "message", js_message,
                                JS_PROP_C_W_E) < 0) {
    JS_FreeValue(ctx, error);
    if (fallback != NULL) {
      caml_stat_free(fallback);
    }
    CAMLreturnT(JSValue, JS_EXCEPTION);
  }

  JSValue frame = JS_NewString(ctx, name);
  if (JS_IsException(frame)) {
    JS_FreeValue(ctx, error);
    if (fallback != NULL) {
      caml_stat_free(fallback);
    }
    CAMLreturnT(JSValue, JS_EXCEPTION);
  }
  if (JS_DefinePropertyValueStr(ctx, error, "_qmljs_ocaml_frame", frame,
                                JS_PROP_C_W_E) < 0) {
    JS_FreeValue(ctx, error);
    if (fallback != NULL) {
      caml_stat_free(fallback);
    }
    CAMLreturnT(JSValue, JS_EXCEPTION);
  }

  JSValue kind = JS_NewString(ctx, "host_callback_defect");
  if (JS_IsException(kind)) {
    JS_FreeValue(ctx, error);
    if (fallback != NULL) {
      caml_stat_free(fallback);
    }
    CAMLreturnT(JSValue, JS_EXCEPTION);
  }
  if (JS_DefinePropertyValueStr(ctx, error, "_qmljs_kind", kind,
                                JS_PROP_C_W_E) < 0) {
    JS_FreeValue(ctx, error);
    if (fallback != NULL) {
      caml_stat_free(fallback);
    }
    CAMLreturnT(JSValue, JS_EXCEPTION);
  }

  if (metadata_fn != NULL) {
    value metadata_result = caml_callback_exn(*metadata_fn, exception);
    if (!Is_exception_result(metadata_result)) {
      metadata_value = metadata_result;
      mlsize_t length = Wosize_val(metadata_value);
      if (length > 0) {
        JSValue metadata = JS_NewArray(ctx);
        if (JS_IsException(metadata)) {
          JS_FreeValue(ctx, error);
          if (fallback != NULL) {
            caml_stat_free(fallback);
          }
          CAMLreturnT(JSValue, JS_EXCEPTION);
        }

        for (mlsize_t i = 0; i < length; i++) {
          metadata_pair = Field(metadata_value, i);
          value key = Field(metadata_pair, 0);
          value value_ = Field(metadata_pair, 1);
          JSValue js_pair = JS_NewArray(ctx);
          if (JS_IsException(js_pair)) {
            JS_FreeValue(ctx, metadata);
            JS_FreeValue(ctx, error);
            if (fallback != NULL) {
              caml_stat_free(fallback);
            }
            CAMLreturnT(JSValue, JS_EXCEPTION);
          }

          JSValue js_key =
            JS_NewStringLen(ctx, String_val(key), caml_string_length(key));
          JSValue js_value =
            JS_NewStringLen(ctx, String_val(value_), caml_string_length(value_));
          if (JS_IsException(js_key) || JS_IsException(js_value)) {
            JS_FreeValue(ctx, js_value);
            JS_FreeValue(ctx, js_key);
            JS_FreeValue(ctx, js_pair);
            JS_FreeValue(ctx, metadata);
            JS_FreeValue(ctx, error);
            if (fallback != NULL) {
              caml_stat_free(fallback);
            }
            CAMLreturnT(JSValue, JS_EXCEPTION);
          }
          if (JS_SetPropertyUint32(ctx, js_pair, 0, js_key) < 0) {
            JS_FreeValue(ctx, js_value);
            JS_FreeValue(ctx, js_pair);
            JS_FreeValue(ctx, metadata);
            JS_FreeValue(ctx, error);
            if (fallback != NULL) {
              caml_stat_free(fallback);
            }
            CAMLreturnT(JSValue, JS_EXCEPTION);
          }
          if (JS_SetPropertyUint32(ctx, js_pair, 1, js_value) < 0) {
            JS_FreeValue(ctx, js_pair);
            JS_FreeValue(ctx, metadata);
            JS_FreeValue(ctx, error);
            if (fallback != NULL) {
              caml_stat_free(fallback);
            }
            CAMLreturnT(JSValue, JS_EXCEPTION);
          }
          if (JS_SetPropertyUint32(ctx, metadata, (uint32_t)i, js_pair) < 0) {
            JS_FreeValue(ctx, metadata);
            JS_FreeValue(ctx, error);
            if (fallback != NULL) {
              caml_stat_free(fallback);
            }
            CAMLreturnT(JSValue, JS_EXCEPTION);
          }
        }

        if (JS_DefinePropertyValueStr(ctx, error, "_qmljs_metadata", metadata,
                                      JS_PROP_C_W_E) < 0) {
          JS_FreeValue(ctx, error);
          if (fallback != NULL) {
            caml_stat_free(fallback);
          }
          CAMLreturnT(JSValue, JS_EXCEPTION);
        }
      }
    }
  }

  if (fallback != NULL) {
    caml_stat_free(fallback);
  }
  CAMLreturnT(JSValue, JS_Throw(ctx, error));
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

static int qml_define_ocaml_string_property(JSContext *ctx, JSValueConst object,
                                            const char *name, value input)
{
  JSValue string =
    JS_NewStringLen(ctx, String_val(input), caml_string_length(input));
  if (JS_IsException(string)) {
    return -1;
  }
  return JS_DefinePropertyValueStr(ctx, object, name, string, JS_PROP_C_W_E);
}

static int qml_define_metadata_property(JSContext *ctx, JSValueConst object,
                                        value metadata_value)
{
  mlsize_t length = Wosize_val(metadata_value);
  if (length == 0) {
    return 0;
  }

  JSValue metadata = JS_NewArray(ctx);
  if (JS_IsException(metadata)) {
    return -1;
  }

  for (mlsize_t i = 0; i < length; i++) {
    value metadata_pair = Field(metadata_value, i);
    value key = Field(metadata_pair, 0);
    value value_ = Field(metadata_pair, 1);
    JSValue js_pair = JS_NewArray(ctx);
    if (JS_IsException(js_pair)) {
      JS_FreeValue(ctx, metadata);
      return -1;
    }

    JSValue js_key =
      JS_NewStringLen(ctx, String_val(key), caml_string_length(key));
    JSValue js_value =
      JS_NewStringLen(ctx, String_val(value_), caml_string_length(value_));
    if (JS_IsException(js_key) || JS_IsException(js_value)) {
      JS_FreeValue(ctx, js_value);
      JS_FreeValue(ctx, js_key);
      JS_FreeValue(ctx, js_pair);
      JS_FreeValue(ctx, metadata);
      return -1;
    }
    if (JS_SetPropertyUint32(ctx, js_pair, 0, js_key) < 0) {
      JS_FreeValue(ctx, js_value);
      JS_FreeValue(ctx, js_pair);
      JS_FreeValue(ctx, metadata);
      return -1;
    }
    if (JS_SetPropertyUint32(ctx, js_pair, 1, js_value) < 0) {
      JS_FreeValue(ctx, js_pair);
      JS_FreeValue(ctx, metadata);
      return -1;
    }
    if (JS_SetPropertyUint32(ctx, metadata, (uint32_t)i, js_pair) < 0) {
      JS_FreeValue(ctx, metadata);
      return -1;
    }
  }

  return JS_DefinePropertyValueStr(ctx, object, "_qmljs_metadata", metadata,
                                   JS_PROP_C_W_E);
}

static JSValue qml_throw_host_deadline_error(JSContext *ctx)
{
  JSValue error = JS_NewError(ctx);
  if (JS_IsException(error)) {
    return error;
  }
  if (qml_define_string_property(ctx, error, "message",
                                 "JavaScript execution timed out") < 0 ||
      qml_define_string_property(ctx, error, "_qmljs_kind",
                                 "timeout") < 0) {
    JS_FreeValue(ctx, error);
    return JS_EXCEPTION;
  }
  return JS_Throw(ctx, error);
}

static JSValue qml_throw_host_defect_message(JSContext *ctx, const char *message,
                                             const char *name)
{
  JSValue error = JS_NewError(ctx);
  if (JS_IsException(error)) {
    return error;
  }
  if (qml_define_string_property(ctx, error, "message", message) < 0 ||
      qml_define_string_property(ctx, error, "_qmljs_ocaml_frame", name) < 0 ||
      qml_define_string_property(ctx, error, "_qmljs_kind",
                                 "host_callback_defect") < 0) {
    JS_FreeValue(ctx, error);
    return JS_EXCEPTION;
  }
  return JS_Throw(ctx, error);
}

CAMLprim value qml_function_host_error(value context, value name, value message,
                                       value metadata)
{
  CAMLparam4(context, name, message, metadata);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  JSValue error = JS_NewError(ctx);
  if (JS_IsException(error)) {
    qml_raise_exception(ctx);
  }

  if (qml_define_ocaml_string_property(ctx, error, "message", message) < 0 ||
      qml_define_ocaml_string_property(ctx, error, "_qmljs_ocaml_frame",
                                       name) < 0 ||
      qml_define_string_property(ctx, error, "_qmljs_kind",
                                 "host_callback_defect") < 0 ||
      qml_define_metadata_property(ctx, error, metadata) < 0) {
    JS_FreeValue(ctx, error);
    qml_raise_exception(ctx);
  }

  CAMLreturn(qml_alloc_value(context, error));
}

static value qml_option(value input)
{
  CAMLparam1(input);
  CAMLlocal1(result);
  result = caml_alloc(1, 0);
  Store_field(result, 0, input);
  CAMLreturn(result);
}

static value qml_optional_wrapped_new_target(value context, JSContext *ctx,
                                             JSValueConst new_target)
{
  CAMLparam1(context);
  CAMLlocal1(wrapped);
  if (JS_IsUndefined(new_target)) {
    CAMLreturn(Val_int(0));
  }
  wrapped = qml_alloc_value(context, JS_DupValue(ctx, new_target));
  CAMLreturn(qml_option(wrapped));
}

static JSValue qml_host_call_callback(JSContext *ctx,
                                      struct qml_callback *callback,
                                      JSValueConst this_value, int argc,
                                      JSValueConst *argv,
                                      JSValueConst new_target)
{
  CAMLparam0();
  CAMLlocal4(args, result, this_, new_target_option);
  struct qml_runtime_state *state = qml_context_state(callback->context);

  if (atomic_load_explicit(&state->interrupt_deadline_ns,
                           memory_order_relaxed) > 0) {
    atomic_store_explicit(&state->interrupt_requested, 1,
                          memory_order_relaxed);
    CAMLreturnT(JSValue, qml_throw_host_deadline_error(ctx));
  }

  args = caml_alloc(argc, 0);
  for (int i = 0; i < argc; i++) {
    Store_field(args, i, Val_unit);
  }
  for (int i = 0; i < argc; i++) {
    Store_field(args, i,
                qml_alloc_value(callback->context, JS_DupValue(ctx, argv[i])));
  }

  qml_enter_ocaml_callback(state);
  if (callback->mode == 0) {
    result = caml_callback_exn(callback->fn, args);
  } else {
    this_ = qml_alloc_value(callback->context, JS_DupValue(ctx, this_value));
    new_target_option =
      qml_optional_wrapped_new_target(callback->context, ctx, new_target);
    result = caml_callback3_exn(callback->fn, this_, args, new_target_option);
  }
  qml_leave_ocaml_callback(state);

  if (Is_exception_result(result)) {
    JSValue exception =
      qml_throw_ocaml_exception(ctx, Extract_exception(result), callback->name);
    CAMLreturnT(JSValue, exception);
  }

  JSValue raw_result = JS_UNDEFINED;
  const char *return_error =
    qml_js_value_for_context(callback->context, result, &raw_result);
  if (return_error != NULL) {
    CAMLreturnT(JSValue,
                qml_throw_host_defect_message(ctx, return_error,
                                              callback->name));
  }
  JSValue js_result = JS_DupValue(ctx, raw_result);
  CAMLreturnT(JSValue, js_result);
}

static void qml_host_function_finalizer(JSRuntime *rt, JSValue val)
{
  (void)rt;
  struct qml_callback *callback =
    JS_GetOpaque(val, JS_GetClassID(val));
  if (callback != NULL) {
    qml_callback_finalize(callback);
  }
}

static JSValue qml_host_function_call(JSContext *ctx, JSValueConst func_obj,
                                      JSValueConst this_value, int argc,
                                      JSValueConst *argv, int flags)
{
  struct qml_callback *callback =
    JS_GetOpaque(func_obj, JS_GetClassID(func_obj));
  if (callback == NULL) {
    return JS_ThrowTypeError(ctx, "invalid qmljs host function");
  }

  if ((flags & JS_CALL_FLAG_CONSTRUCTOR) != 0) {
    return qml_host_call_callback(ctx, callback, JS_UNDEFINED, argc, argv,
                                  this_value);
  }
  return qml_host_call_callback(ctx, callback, this_value, argc, argv,
                                JS_UNDEFINED);
}

static const JSClassDef qml_host_function_class_def = {
  .class_name = "QmljsHostFunction",
  .finalizer = qml_host_function_finalizer,
  .call = qml_host_function_call,
};

static JSClassID qml_ensure_host_function_class(value context, JSContext *ctx)
{
  struct qml_runtime_state *state = qml_context_state(context);
  if (!state->host_function_class_registered) {
    JSRuntime *rt = JS_GetRuntime(ctx);
    JSClassID class_id = JS_NewClassID(rt, &state->host_function_class_id);
    if (JS_NewClass(rt, class_id, &qml_host_function_class_def) < 0) {
      qml_raise_string("could not register qmljs host function class");
    }
    state->host_function_class_registered = 1;
  }
  return state->host_function_class_id;
}

static int qml_define_host_function_metadata(JSContext *ctx,
                                             JSValueConst function,
                                             value name, value arity)
{
  JSValue js_name =
    JS_NewStringLen(ctx, String_val(name), caml_string_length(name));
  if (JS_IsException(js_name)) {
    return -1;
  }
  if (JS_DefinePropertyValueStr(ctx, function, "name", js_name,
                                JS_PROP_CONFIGURABLE) < 0) {
    return -1;
  }

  JSValue js_length = JS_NewInt64(ctx, Long_val(arity));
  if (JS_IsException(js_length)) {
    return -1;
  }
  if (JS_DefinePropertyValueStr(ctx, function, "length", js_length,
                                JS_PROP_CONFIGURABLE) < 0) {
    return -1;
  }
  return 0;
}

static value qml_function_make_mode(value context, value name, value arity,
                                    value fn, int mode)
{
  CAMLparam4(context, name, arity, fn);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  JSClassID class_id = qml_ensure_host_function_class(context, ctx);

  long length = Long_val(arity);
  if (length < 0 || length > INT32_MAX) {
    qml_raise_string("function arity is outside the supported range");
  }

  struct qml_callback *callback = caml_stat_alloc(sizeof(struct qml_callback));
  size_t name_length = caml_string_length(name);
  callback->fn = fn;
  callback->context = context;
  callback->mode = mode;
  callback->name = caml_stat_alloc(name_length + 1);
  memcpy(callback->name, String_val(name), name_length);
  callback->name[name_length] = '\0';
  caml_register_generational_global_root(&callback->fn);
  caml_register_generational_global_root(&callback->context);

  JSValue function_proto = JS_GetFunctionProto(ctx);
  if (JS_IsException(function_proto)) {
    qml_callback_finalize(callback);
    qml_raise_exception(ctx);
  }

  JSValue function =
    JS_NewObjectProtoClass(ctx, function_proto, class_id);
  JS_FreeValue(ctx, function_proto);

  if (JS_IsException(function)) {
    qml_callback_finalize(callback);
    qml_raise_exception(ctx);
  }
  if (JS_SetOpaque(function, callback) < 0) {
    JS_FreeValue(ctx, function);
    qml_callback_finalize(callback);
    qml_raise_string("could not attach qmljs host callback");
  }

  if (qml_define_host_function_metadata(ctx, function, name, arity) < 0) {
    JS_FreeValue(ctx, function);
    qml_raise_exception(ctx);
  }

  CAMLreturn(qml_alloc_value(context, function));
}

CAMLprim value qml_function_make(value context, value name, value arity, value fn)
{
  return qml_function_make_mode(context, name, arity, fn, 0);
}

CAMLprim value qml_function_make_with_this(value context, value name,
                                           value arity, value fn)
{
  return qml_function_make_mode(context, name, arity, fn, 1);
}

static void qml_ensure_function(JSContext *ctx, JSValueConst value)
{
  if (!JS_IsFunction(ctx, value)) {
    qml_raise_string("value is not a Function");
  }
}

static JSValueConst *qml_function_args(value context, value args, int *argc)
{
  size_t arg_count = Wosize_val(args);
  *argc = qml_argc_of_size(arg_count);
  if (*argc == 0) {
    return NULL;
  }
  for (size_t i = 0; i < arg_count; i++) {
    qmljs_argv_check(context, Field(args, i));
  }
  JSValueConst *argv = qml_alloc_argv(arg_count);
  for (size_t i = 0; i < arg_count; i++) {
    qml_argv_set(context, argv, i, Field(args, i));
  }
  return argv;
}

CAMLprim value qml_function_call(value context, value function, value this_,
                                 value args)
{
  CAMLparam4(context, function, this_, args);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  qml_ensure_same_context(context, function);
  qml_ensure_same_context(context, this_);
  JSValue js_function = qml_js_value(function);
  JSValue js_this = qml_js_value(this_);
  qml_ensure_function(ctx, js_function);

  int argc = 0;
  JSValueConst *argv = qml_function_args(context, args, &argc);
  JSValue result = JS_Call(ctx, js_function, js_this, argc, argv);
  if (argv != NULL) {
    qml_free_argv(argv);
  }
  if (JS_IsException(result)) {
    qml_raise_exception(ctx);
  }
  CAMLreturn(qml_alloc_value(context, result));
}

CAMLprim value qml_function_construct(value context, value function, value args)
{
  CAMLparam3(context, function, args);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  qml_ensure_same_context(context, function);
  JSValue js_function = qml_js_value(function);

  int argc = 0;
  JSValueConst *argv = qml_function_args(context, args, &argc);
  JSValue result = JS_CallConstructor(ctx, js_function, argc, argv);
  if (argv != NULL) {
    qml_free_argv(argv);
  }
  if (JS_IsException(result)) {
    qml_raise_exception(ctx);
  }
  CAMLreturn(qml_alloc_value(context, result));
}

static JSValue qml_defer_call_job(JSContext *ctx, int argc, JSValueConst *argv)
{
  JSValueConst function = argv[argc - 1];
  JSValueConst this_ = argv[argc - 2];
  return JS_Call(ctx, function, this_, argc - 2, argv);
}

CAMLprim value qml_function_defer(value context, value function, value this_,
                                  value args)
{
  CAMLparam4(context, function, this_, args);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  qml_ensure_same_context(context, function);
  qml_ensure_same_context(context, this_);
  JSValue js_function = qml_js_value(function);
  JSValue js_this = qml_js_value(this_);
  qml_ensure_function(ctx, js_function);

  size_t arg_count = Wosize_val(args);
  int argc =
    qml_argc_of_size(arg_count > (size_t)INT_MAX - 2
                       ? (size_t)INT_MAX + 1
                       : arg_count + 2);
  for (size_t i = 0; i < arg_count; i++) {
    qmljs_argv_check(context, Field(args, i));
  }
  JSValueConst *argv = qml_alloc_argv((size_t)argc);
  for (size_t i = 0; i < arg_count; i++) {
    qml_argv_set(context, argv, i, Field(args, i));
  }
  argv[argc - 2] = js_this;
  argv[argc - 1] = js_function;

  int result = JS_EnqueueJob(ctx, qml_defer_call_job, argc, argv);
  qml_free_argv(argv);
  if (result < 0) {
    qml_raise_exception(ctx);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value qml_function_name(value context, value function)
{
  CAMLparam2(context, function);
  CAMLlocal1(result);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  qml_ensure_same_context(context, function);
  qml_ensure_function(ctx, qml_js_value(function));

  JSValue name = JS_GetPropertyStr(ctx, qml_js_value(function), "name");
  if (JS_IsException(name)) {
    qml_raise_exception(ctx);
  }
  size_t length = 0;
  const char *string = JS_ToCStringLen(ctx, &length, name);
  if (string == NULL) {
    JS_FreeValue(ctx, name);
    qml_raise_exception(ctx);
  }
  result = caml_alloc_string(length);
  memcpy(Bytes_val(result), string, length);
  JS_FreeCString(ctx, string);
  JS_FreeValue(ctx, name);
  CAMLreturn(result);
}

CAMLprim value qml_function_set_name(value context, value function, value input)
{
  CAMLparam3(context, function, input);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  qml_ensure_same_context(context, function);
  qml_ensure_function(ctx, qml_js_value(function));

  JSValue name =
    JS_NewStringLen(ctx, String_val(input), caml_string_length(input));
  if (JS_IsException(name)) {
    qml_raise_exception(ctx);
  }
  if (JS_DefinePropertyValueStr(ctx, qml_js_value(function), "name", name,
                                JS_PROP_CONFIGURABLE | JS_PROP_THROW) < 0) {
    qml_raise_exception(ctx);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value qml_function_length(value context, value function)
{
  CAMLparam2(context, function);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  qml_ensure_same_context(context, function);
  qml_ensure_function(ctx, qml_js_value(function));

  JSValue length = JS_GetPropertyStr(ctx, qml_js_value(function), "length");
  if (JS_IsException(length)) {
    qml_raise_exception(ctx);
  }
  int64_t result = 0;
  if (JS_ToInt64(ctx, &result, length) < 0) {
    JS_FreeValue(ctx, length);
    qml_raise_exception(ctx);
  }
  JS_FreeValue(ctx, length);
  if (result < 0 || result > (int64_t)Max_long) {
    qml_raise_string("function length is outside the OCaml int range");
  }
  CAMLreturn(Val_long((intnat)result));
}

CAMLprim value qml_function_set_length(value context, value function,
                                       value input)
{
  CAMLparam3(context, function, input);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  qml_ensure_same_context(context, function);
  qml_ensure_function(ctx, qml_js_value(function));

  long length = Long_val(input);
  if (length < 0) {
    qml_raise_string("function length is negative");
  }
  JSValue js_length = JS_NewInt64(ctx, length);
  if (JS_IsException(js_length)) {
    qml_raise_exception(ctx);
  }
  if (JS_DefinePropertyValueStr(ctx, qml_js_value(function), "length",
                                js_length,
                                JS_PROP_CONFIGURABLE | JS_PROP_THROW) < 0) {
    qml_raise_exception(ctx);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value qml_function_prototype(value context)
{
  CAMLparam1(context);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  JSValue result = JS_GetFunctionProto(ctx);
  if (JS_IsException(result)) {
    qml_raise_exception(ctx);
  }
  CAMLreturn(qml_alloc_value(context, result));
}

CAMLprim value qml_function_is_constructor(value context, value function)
{
  CAMLparam2(context, function);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  qml_ensure_same_context(context, function);
  CAMLreturn(Val_bool(JS_IsConstructor(ctx, qml_js_value(function))));
}

CAMLprim value qml_function_set_constructor(value context, value function,
                                            value input)
{
  CAMLparam3(context, function, input);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  qml_ensure_same_context(context, function);
  JS_SetConstructorBit(ctx, qml_js_value(function), Bool_val(input));
  CAMLreturn(Val_unit);
}
