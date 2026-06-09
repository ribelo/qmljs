#include "qmljs_stubs.h"

#include <caml/printexc.h>
#include <limits.h>
#include <stdint.h>
#include <string.h>

struct qmljs_callback {
  value fn;
  value context;
  char *name;
  int mode;
};

struct qmljs_host_call_frame {
  JSContext *ctx;
  struct qmljs_callback *callback;
  JSValueConst this_value;
  int argc;
  JSValueConst *argv;
  JSValueConst new_target;
  struct qmljs_runtime_state *state;
};

static _Thread_local struct qmljs_host_call_frame *qmljs_current_host_call =
  NULL;

enum {
  QMLJS_HOST_CALLBACK_FAILPOINT_DISABLED = 0,
  QMLJS_HOST_CALLBACK_FAILPOINT_ARGUMENT = 1,
  QMLJS_HOST_CALLBACK_FAILPOINT_THIS = 2,
  QMLJS_HOST_CALLBACK_FAILPOINT_NEW_TARGET = 3
};

static atomic_int qmljs_test_host_callback_failpoint_site =
  QMLJS_HOST_CALLBACK_FAILPOINT_DISABLED;

static void qmljs_test_raise_host_callback_failpoint(int site)
{
  int expected = site;
  if (atomic_compare_exchange_strong_explicit(
        &qmljs_test_host_callback_failpoint_site, &expected,
        QMLJS_HOST_CALLBACK_FAILPOINT_DISABLED, memory_order_acq_rel,
        memory_order_acquire)) {
    caml_failwith("qmljs test host callback wrapper allocation failpoint");
  }
}

static void qmljs_callback_finalize(void *opaque)
{
  struct qmljs_callback *callback = opaque;
  caml_remove_generational_global_root(&callback->fn);
  caml_remove_generational_global_root(&callback->context);
  caml_stat_free(callback->name);
  caml_stat_free(callback);
}

static JSValue qmljs_error_marker_to_js(JSContext *ctx, value marker, int depth);

static JSValue qmljs_throw_host_defect_message(JSContext *ctx,
                                             const char *message,
                                             const char *name);

static JSValue qmljs_throw_ocaml_exception(JSContext *ctx, value exception,
                                         const char *name)
{
  CAMLparam1(exception);
  CAMLlocal2(name_value, marker);
  static const value *marker_fn = NULL;

  if (marker_fn == NULL) {
    marker_fn = caml_named_value("qmljs.host_exception_marker");
  }
  if (marker_fn == NULL) {
    char *fallback = caml_format_exception(exception);
    JSValue error = qmljs_throw_host_defect_message(ctx, fallback, name);
    caml_stat_free(fallback);
    CAMLreturnT(JSValue, error);
  }

  name_value = caml_copy_string(name);
  value marker_result = caml_callback2_exn(*marker_fn, name_value, exception);
  if (Is_exception_result(marker_result)) {
    char *fallback = caml_format_exception(Extract_exception(marker_result));
    JSValue error = qmljs_throw_host_defect_message(ctx, fallback, name);
    caml_stat_free(fallback);
    CAMLreturnT(JSValue, error);
  }

  marker = marker_result;
  JSValue error = qmljs_error_marker_to_js(ctx, marker, 0);
  if (JS_IsException(error)) {
    CAMLreturnT(JSValue, error);
  }
  CAMLreturnT(JSValue, JS_Throw(ctx, error));
}

static int qmljs_define_string_property(JSContext *ctx, JSValueConst object,
                                      const char *name, const char *value)
{
  JSValue string = JS_NewString(ctx, value);
  if (JS_IsException(string)) {
    return -1;
  }
  return JS_DefinePropertyValueStr(ctx, object, name, string, JS_PROP_C_W_E);
}

static int qmljs_define_ocaml_string_property(JSContext *ctx, JSValueConst object,
                                            const char *name, value input)
{
  JSValue string =
    JS_NewStringLen(ctx, String_val(input), caml_string_length(input));
  if (JS_IsException(string)) {
    return -1;
  }
  return JS_DefinePropertyValueStr(ctx, object, name, string, JS_PROP_C_W_E);
}

static int qmljs_define_metadata_property(JSContext *ctx, JSValueConst object,
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

enum {
  QMLJS_ERROR_MARKER_NAME = 0,
  QMLJS_ERROR_MARKER_MESSAGE = 1,
  QMLJS_ERROR_MARKER_KIND = 2,
  QMLJS_ERROR_MARKER_DENIED_SPECIFIER = 3,
  QMLJS_ERROR_MARKER_STACK = 4,
  QMLJS_ERROR_MARKER_OCAML_FRAME = 5,
  QMLJS_ERROR_MARKER_METADATA = 6,
  QMLJS_ERROR_MARKER_CAUSE = 7
};

static int qmljs_define_error_marker_cause(JSContext *ctx,
                                         JSValueConst error,
                                         value cause, int depth)
{
  if (cause == Val_int(0) || depth >= 64) {
    return 0;
  }

  JSValue js_cause = qmljs_error_marker_to_js(ctx, Field(cause, 0), depth + 1);
  if (JS_IsException(js_cause)) {
    return -1;
  }
  return JS_DefinePropertyValueStr(ctx, error, "cause", js_cause,
                                   JS_PROP_C_W_E);
}

static JSValue qmljs_error_marker_to_js(JSContext *ctx, value marker, int depth)
{
  JSValue error = qmljs_new_error_marker(ctx);
  if (JS_IsException(error)) {
    return error;
  }

  value name = Field(marker, QMLJS_ERROR_MARKER_NAME);
  value message = Field(marker, QMLJS_ERROR_MARKER_MESSAGE);
  value kind = Field(marker, QMLJS_ERROR_MARKER_KIND);
  value denied_specifier = Field(marker, QMLJS_ERROR_MARKER_DENIED_SPECIFIER);
  value stack = Field(marker, QMLJS_ERROR_MARKER_STACK);
  value ocaml_frame = Field(marker, QMLJS_ERROR_MARKER_OCAML_FRAME);
  value metadata = Field(marker, QMLJS_ERROR_MARKER_METADATA);
  value cause = Field(marker, QMLJS_ERROR_MARKER_CAUSE);

  if ((caml_string_length(name) > 0 &&
       qmljs_define_ocaml_string_property(ctx, error, "name", name) < 0) ||
      qmljs_define_ocaml_string_property(ctx, error, "message", message) < 0 ||
      qmljs_define_ocaml_string_property(ctx, error, "_qmljs_kind", kind) < 0 ||
      (caml_string_length(denied_specifier) > 0 &&
       qmljs_define_ocaml_string_property(
         ctx, error, "_qmljs_denied_module_specifier", denied_specifier) < 0) ||
      (caml_string_length(stack) > 0 &&
       qmljs_define_ocaml_string_property(ctx, error, "stack", stack) < 0) ||
      (caml_string_length(ocaml_frame) > 0 &&
       qmljs_define_ocaml_string_property(ctx, error, "_qmljs_ocaml_frame",
                                        ocaml_frame) < 0) ||
      qmljs_define_metadata_property(ctx, error, metadata) < 0 ||
      qmljs_define_error_marker_cause(ctx, error, cause, depth) < 0) {
    JS_FreeValue(ctx, error);
    return JS_EXCEPTION;
  }

  return error;
}

static JSValue qmljs_throw_host_deadline_error(JSContext *ctx)
{
  JSValue error = qmljs_new_error_marker(ctx);
  if (JS_IsException(error)) {
    return error;
  }
  if (qmljs_define_string_property(ctx, error, "message",
                                 "JavaScript execution timed out") < 0 ||
      qmljs_define_string_property(ctx, error, "_qmljs_kind",
                                 "timeout") < 0) {
    JS_FreeValue(ctx, error);
    return JS_EXCEPTION;
  }
  return JS_Throw(ctx, error);
}

static JSValue qmljs_throw_host_defect_message(JSContext *ctx, const char *message,
                                             const char *name)
{
  JSValue error = qmljs_new_error_marker(ctx);
  if (JS_IsException(error)) {
    return error;
  }
  if (qmljs_define_string_property(ctx, error, "message", message) < 0 ||
      qmljs_define_string_property(ctx, error, "_qmljs_ocaml_frame", name) < 0 ||
      qmljs_define_string_property(ctx, error, "_qmljs_kind",
                                 "host_callback_defect") < 0) {
    JS_FreeValue(ctx, error);
    return JS_EXCEPTION;
  }
  return JS_Throw(ctx, error);
}

CAMLprim value qmljs_function_host_error(value context, value marker)
{
  CAMLparam2(context, marker);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  JSValue error = qmljs_error_marker_to_js(ctx, marker, 0);
  if (JS_IsException(error)) {
    qmljs_raise_exception(ctx);
  }

  CAMLreturn(qmljs_alloc_value(context, error));
}

static value qmljs_option(value input)
{
  CAMLparam1(input);
  CAMLlocal1(result);
  result = caml_alloc(1, 0);
  Store_field(result, 0, input);
  CAMLreturn(result);
}

static value qmljs_optional_wrapped_new_target(value context, JSContext *ctx,
                                             JSValueConst new_target)
{
  CAMLparam1(context);
  CAMLlocal1(wrapped);
  if (JS_IsUndefined(new_target)) {
    CAMLreturn(Val_int(0));
  }
  qmljs_test_raise_host_callback_failpoint(
    QMLJS_HOST_CALLBACK_FAILPOINT_NEW_TARGET);
  wrapped = qmljs_alloc_value(context, JS_DupValue(ctx, new_target));
  CAMLreturn(qmljs_option(wrapped));
}

CAMLprim value qmljs_host_call_trampoline(value unit)
{
  CAMLparam1(unit);
  CAMLlocal4(args, result, this_, new_target_option);
  struct qmljs_host_call_frame *frame = qmljs_current_host_call;
  if (frame == NULL) {
    caml_failwith("qmljs host callback trampoline called outside host callback");
  }

  struct qmljs_callback *callback = frame->callback;
  args = caml_alloc(frame->argc, 0);
  for (int i = 0; i < frame->argc; i++) {
    Store_field(args, i, Val_unit);
  }
  for (int i = 0; i < frame->argc; i++) {
    qmljs_test_raise_host_callback_failpoint(
      QMLJS_HOST_CALLBACK_FAILPOINT_ARGUMENT);
    Store_field(args, i,
                qmljs_alloc_value(callback->context,
                                JS_DupValue(frame->ctx, frame->argv[i])));
  }

  if (callback->mode == 0) {
    qmljs_enter_ocaml_callback(frame->state);
    result = caml_callback_exn(callback->fn, args);
    qmljs_leave_ocaml_callback(frame->state);
  } else {
    qmljs_test_raise_host_callback_failpoint(
      QMLJS_HOST_CALLBACK_FAILPOINT_THIS);
    this_ =
      qmljs_alloc_value(callback->context,
                      JS_DupValue(frame->ctx, frame->this_value));
    new_target_option =
      qmljs_optional_wrapped_new_target(callback->context, frame->ctx,
                                      frame->new_target);
    qmljs_enter_ocaml_callback(frame->state);
    result =
      caml_callback3_exn(callback->fn, this_, args, new_target_option);
    qmljs_leave_ocaml_callback(frame->state);
  }

  if (Is_exception_result(result)) {
    caml_raise(Extract_exception(result));
  }
  CAMLreturn(result);
}

static JSValue qmljs_host_call_callback(JSContext *ctx,
                                      struct qmljs_callback *callback,
                                      JSValueConst this_value, int argc,
                                      JSValueConst *argv,
                                      JSValueConst new_target)
{
  CAMLparam0();
  CAMLlocal1(result);
  struct qmljs_runtime_state *state = qmljs_context_state(callback->context);

  if (atomic_load_explicit(&state->interrupt_deadline_ns,
                           memory_order_relaxed) > 0) {
    atomic_store_explicit(&state->interrupt_requested, 1,
                          memory_order_relaxed);
    CAMLreturnT(JSValue, qmljs_throw_host_deadline_error(ctx));
  }

  const value *trampoline_fn = caml_named_value("qmljs.host_call_trampoline");
  if (trampoline_fn == NULL) {
    CAMLreturnT(JSValue,
                qmljs_throw_host_defect_message(
                  ctx, "qmljs host callback trampoline is not registered",
                  callback->name));
  }

  struct qmljs_host_call_frame frame = {
    .ctx = ctx,
    .callback = callback,
    .this_value = this_value,
    .argc = argc,
    .argv = argv,
    .new_target = new_target,
    .state = state
  };
  struct qmljs_host_call_frame *previous = qmljs_current_host_call;
  qmljs_current_host_call = &frame;
  value trampoline_result = caml_callback_exn(*trampoline_fn, Val_unit);
  qmljs_current_host_call = previous;

  if (Is_exception_result(trampoline_result)) {
    JSValue exception =
      qmljs_throw_ocaml_exception(ctx, Extract_exception(trampoline_result),
                                callback->name);
    CAMLreturnT(JSValue, exception);
  }

  result = trampoline_result;
  JSValue raw_result = JS_UNDEFINED;
  const char *return_error =
    qmljs_js_value_for_context(callback->context, result, &raw_result);
  if (return_error != NULL) {
    CAMLreturnT(JSValue,
                qmljs_throw_host_defect_message(ctx, return_error,
                                              callback->name));
  }
  JSValue js_result = JS_DupValue(ctx, raw_result);
  CAMLreturnT(JSValue, js_result);
}

static void qmljs_host_function_finalizer(JSRuntime *rt, JSValue val)
{
  (void)rt;
  struct qmljs_callback *callback =
    JS_GetOpaque(val, JS_GetClassID(val));
  if (callback != NULL) {
    qmljs_callback_finalize(callback);
  }
}

static JSValue qmljs_host_function_call(JSContext *ctx, JSValueConst func_obj,
                                      JSValueConst this_value, int argc,
                                      JSValueConst *argv, int flags)
{
  struct qmljs_callback *callback =
    JS_GetOpaque(func_obj, JS_GetClassID(func_obj));
  if (callback == NULL) {
    return JS_ThrowTypeError(ctx, "invalid qmljs host function");
  }

  if ((flags & JS_CALL_FLAG_CONSTRUCTOR) != 0) {
    return qmljs_host_call_callback(ctx, callback, JS_UNDEFINED, argc, argv,
                                  this_value);
  }
  return qmljs_host_call_callback(ctx, callback, this_value, argc, argv,
                                JS_UNDEFINED);
}

static const JSClassDef qmljs_host_function_class_def = {
  .class_name = "QmljsHostFunction",
  .finalizer = qmljs_host_function_finalizer,
  .call = qmljs_host_function_call,
};

static JSClassID qmljs_ensure_host_function_class(value context, JSContext *ctx)
{
  struct qmljs_runtime_state *state = qmljs_context_state(context);
  if (!state->host_function_class_registered) {
    JSRuntime *rt = JS_GetRuntime(ctx);
    JSClassID class_id = JS_NewClassID(rt, &state->host_function_class_id);
    if (JS_NewClass(rt, class_id, &qmljs_host_function_class_def) < 0) {
      qmljs_raise_string("could not register qmljs host function class");
    }
    state->host_function_class_registered = 1;
  }
  return state->host_function_class_id;
}

static int qmljs_define_host_function_metadata(JSContext *ctx,
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

static value qmljs_function_make_mode(value context, value name, value arity,
                                    value fn, int mode)
{
  CAMLparam4(context, name, arity, fn);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  JSClassID class_id = qmljs_ensure_host_function_class(context, ctx);

  long length = Long_val(arity);
  if (length < 0 || length > INT32_MAX) {
    qmljs_raise_string("function arity is outside the supported range");
  }

  struct qmljs_callback *callback = caml_stat_alloc(sizeof(struct qmljs_callback));
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
    qmljs_callback_finalize(callback);
    qmljs_raise_exception(ctx);
  }

  JSValue function =
    JS_NewObjectProtoClass(ctx, function_proto, class_id);
  JS_FreeValue(ctx, function_proto);

  if (JS_IsException(function)) {
    qmljs_callback_finalize(callback);
    qmljs_raise_exception(ctx);
  }
  if (JS_SetOpaque(function, callback) < 0) {
    JS_FreeValue(ctx, function);
    qmljs_callback_finalize(callback);
    qmljs_raise_string("could not attach qmljs host callback");
  }

  if (qmljs_define_host_function_metadata(ctx, function, name, arity) < 0) {
    JS_FreeValue(ctx, function);
    qmljs_raise_exception(ctx);
  }

  CAMLreturn(qmljs_alloc_value(context, function));
}

CAMLprim value qmljs_function_make(value context, value name, value arity, value fn)
{
  return qmljs_function_make_mode(context, name, arity, fn, 0);
}

CAMLprim value qmljs_function_make_with_this(value context, value name,
                                           value arity, value fn)
{
  return qmljs_function_make_mode(context, name, arity, fn, 1);
}

static void qmljs_ensure_function(JSContext *ctx, JSValueConst value)
{
  if (!JS_IsFunction(ctx, value)) {
    qmljs_raise_string("value is not a Function");
  }
}

static JSValueConst *qmljs_function_args(value context, value args, int *argc)
{
  size_t arg_count = Wosize_val(args);
  *argc = qmljs_argc_of_size(arg_count);
  if (*argc == 0) {
    return NULL;
  }
  for (size_t i = 0; i < arg_count; i++) {
    qmljs_argv_check(context, Field(args, i));
  }
  JSValueConst *argv = qmljs_alloc_argv(arg_count);
  for (size_t i = 0; i < arg_count; i++) {
    qmljs_argv_set(context, argv, i, Field(args, i));
  }
  return argv;
}

CAMLprim value qmljs_function_call(value context, value function, value this_,
                                 value args)
{
  CAMLparam4(context, function, this_, args);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, function);
  qmljs_ensure_same_context(context, this_);
  JSValue js_function = qmljs_js_value(function);
  JSValue js_this = qmljs_js_value(this_);
  qmljs_ensure_function(ctx, js_function);

  int argc = 0;
  JSValueConst *argv = qmljs_function_args(context, args, &argc);
  JSValue result = JS_Call(ctx, js_function, js_this, argc, argv);
  if (argv != NULL) {
    qmljs_free_argv(argv);
  }
  if (JS_IsException(result)) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(qmljs_alloc_value(context, result));
}

CAMLprim value qmljs_function_construct(value context, value function, value args)
{
  CAMLparam3(context, function, args);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, function);
  JSValue js_function = qmljs_js_value(function);

  int argc = 0;
  JSValueConst *argv = qmljs_function_args(context, args, &argc);
  JSValue result = JS_CallConstructor(ctx, js_function, argc, argv);
  if (argv != NULL) {
    qmljs_free_argv(argv);
  }
  if (JS_IsException(result)) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(qmljs_alloc_value(context, result));
}

static JSValue qmljs_defer_call_job(JSContext *ctx, int argc, JSValueConst *argv)
{
  JSValueConst function = argv[argc - 1];
  JSValueConst this_ = argv[argc - 2];
  return JS_Call(ctx, function, this_, argc - 2, argv);
}

CAMLprim value qmljs_function_defer(value context, value function, value this_,
                                  value args)
{
  CAMLparam4(context, function, this_, args);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, function);
  qmljs_ensure_same_context(context, this_);
  JSValue js_function = qmljs_js_value(function);
  JSValue js_this = qmljs_js_value(this_);
  qmljs_ensure_function(ctx, js_function);

  size_t arg_count = Wosize_val(args);
  int argc =
    qmljs_argc_of_size(arg_count > (size_t)INT_MAX - 2
                       ? (size_t)INT_MAX + 1
                       : arg_count + 2);
  for (size_t i = 0; i < arg_count; i++) {
    qmljs_argv_check(context, Field(args, i));
  }
  JSValueConst *argv = qmljs_alloc_argv((size_t)argc);
  for (size_t i = 0; i < arg_count; i++) {
    qmljs_argv_set(context, argv, i, Field(args, i));
  }
  argv[argc - 2] = js_this;
  argv[argc - 1] = js_function;

  int result = JS_EnqueueJob(ctx, qmljs_defer_call_job, argc, argv);
  qmljs_free_argv(argv);
  if (result < 0) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value qmljs_function_name(value context, value function)
{
  CAMLparam2(context, function);
  CAMLlocal1(result);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, function);
  qmljs_ensure_function(ctx, qmljs_js_value(function));

  JSValue name = JS_GetPropertyStr(ctx, qmljs_js_value(function), "name");
  if (JS_IsException(name)) {
    qmljs_raise_exception(ctx);
  }
  size_t length = 0;
  const char *string = JS_ToCStringLen(ctx, &length, name);
  if (string == NULL) {
    JS_FreeValue(ctx, name);
    qmljs_raise_exception(ctx);
  }
  result = caml_alloc_string(length);
  memcpy(Bytes_val(result), string, length);
  JS_FreeCString(ctx, string);
  JS_FreeValue(ctx, name);
  CAMLreturn(result);
}

CAMLprim value qmljs_function_set_name(value context, value function, value input)
{
  CAMLparam3(context, function, input);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, function);
  qmljs_ensure_function(ctx, qmljs_js_value(function));

  JSValue name =
    JS_NewStringLen(ctx, String_val(input), caml_string_length(input));
  if (JS_IsException(name)) {
    qmljs_raise_exception(ctx);
  }
  if (JS_DefinePropertyValueStr(ctx, qmljs_js_value(function), "name", name,
                                JS_PROP_CONFIGURABLE | JS_PROP_THROW) < 0) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value qmljs_function_length(value context, value function)
{
  CAMLparam2(context, function);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, function);
  qmljs_ensure_function(ctx, qmljs_js_value(function));

  JSValue length = JS_GetPropertyStr(ctx, qmljs_js_value(function), "length");
  if (JS_IsException(length)) {
    qmljs_raise_exception(ctx);
  }
  int64_t result = 0;
  if (JS_ToInt64(ctx, &result, length) < 0) {
    JS_FreeValue(ctx, length);
    qmljs_raise_exception(ctx);
  }
  JS_FreeValue(ctx, length);
  if (result < 0 || result > (int64_t)Max_long) {
    qmljs_raise_string("function length is outside the OCaml int range");
  }
  CAMLreturn(Val_long((intnat)result));
}

CAMLprim value qmljs_function_set_length(value context, value function,
                                       value input)
{
  CAMLparam3(context, function, input);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, function);
  qmljs_ensure_function(ctx, qmljs_js_value(function));

  long length = Long_val(input);
  if (length < 0) {
    qmljs_raise_string("function length is negative");
  }
  JSValue js_length = JS_NewInt64(ctx, length);
  if (JS_IsException(js_length)) {
    qmljs_raise_exception(ctx);
  }
  if (JS_DefinePropertyValueStr(ctx, qmljs_js_value(function), "length",
                                js_length,
                                JS_PROP_CONFIGURABLE | JS_PROP_THROW) < 0) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value qmljs_function_prototype(value context)
{
  CAMLparam1(context);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  JSValue result = JS_GetFunctionProto(ctx);
  if (JS_IsException(result)) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(qmljs_alloc_value(context, result));
}

CAMLprim value qmljs_function_is_constructor(value context, value function)
{
  CAMLparam2(context, function);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, function);
  CAMLreturn(Val_bool(JS_IsConstructor(ctx, qmljs_js_value(function))));
}

CAMLprim value qmljs_function_set_constructor(value context, value function,
                                            value input)
{
  CAMLparam3(context, function, input);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, function);
  JS_SetConstructorBit(ctx, qmljs_js_value(function), Bool_val(input));
  CAMLreturn(Val_unit);
}

CAMLprim value qmljs_test_set_host_callback_failpoint(value site)
{
  CAMLparam1(site);
  atomic_store_explicit(&qmljs_test_host_callback_failpoint_site, Int_val(site),
                        memory_order_release);
  CAMLreturn(Val_unit);
}

CAMLprim value qmljs_test_host_callback_depth(value runtime)
{
  CAMLparam1(runtime);
  struct qmljs_runtime_state *state = Qml_runtime_val(Field(runtime, 0))->state;
  CAMLreturn(Val_int(atomic_load_explicit(&state->ocaml_callback_depth,
                                          memory_order_acquire)));
}
