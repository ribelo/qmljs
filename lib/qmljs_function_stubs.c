#include "qmljs_stubs.h"

#include <caml/printexc.h>
#include <string.h>

struct qml_callback {
  value fn;
  value context;
  char *name;
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
  CAMLlocal1(message_value);
  static const value *message_fn = NULL;
  char *fallback = NULL;
  const char *message = NULL;
  size_t message_length = 0;

  if (message_fn == NULL) {
    message_fn = caml_named_value("qmljs.host_exception_message");
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

  if (fallback != NULL) {
    caml_stat_free(fallback);
  }
  CAMLreturnT(JSValue, JS_Throw(ctx, error));
}

static JSValue qml_host_call(JSContext *ctx, JSValueConst this_value, int argc,
                             JSValueConst *argv, int magic, void *opaque)
{
  (void)this_value;
  (void)magic;

  CAMLparam0();
  CAMLlocal2(args, result);
  struct qml_callback *callback = opaque;

  args = caml_alloc(argc, 0);
  for (int i = 0; i < argc; i++) {
    Store_field(args, i, Val_unit);
  }
  for (int i = 0; i < argc; i++) {
    Store_field(args, i,
                qml_alloc_value(callback->context, JS_DupValue(ctx, argv[i])));
  }

  result = caml_callback_exn(callback->fn, args);
  if (Is_exception_result(result)) {
    JSValue exception =
      qml_throw_ocaml_exception(ctx, Extract_exception(result), callback->name);
    CAMLreturnT(JSValue, exception);
  }

  qml_ensure_same_runtime(qml_context_state(callback->context), result);
  JSValue js_result = JS_DupValue(ctx, qml_js_value(result));
  CAMLreturnT(JSValue, js_result);
}

CAMLprim value qml_function_make(value context, value name, value arity, value fn)
{
  CAMLparam4(context, name, arity, fn);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  struct qml_callback *callback = caml_stat_alloc(sizeof(struct qml_callback));
  size_t name_length = caml_string_length(name);
  callback->fn = fn;
  callback->context = context;
  callback->name = caml_stat_alloc(name_length + 1);
  memcpy(callback->name, String_val(name), name_length);
  callback->name[name_length] = '\0';
  caml_register_generational_global_root(&callback->fn);
  caml_register_generational_global_root(&callback->context);

  JSValue function =
    JS_NewCClosure(ctx, qml_host_call, String_val(name), qml_callback_finalize,
                   Int_val(arity), 0, callback);

  if (JS_IsException(function)) {
    qml_callback_finalize(callback);
    qml_raise_exception(ctx);
  }

  CAMLreturn(qml_alloc_value(context, function));
}
