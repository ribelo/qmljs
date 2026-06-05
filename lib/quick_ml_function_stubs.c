#include "quick_ml_stubs.h"

#include <caml/printexc.h>

struct qml_callback {
  value fn;
};

static void qml_callback_finalize(void *opaque)
{
  struct qml_callback *callback = opaque;
  caml_remove_generational_global_root(&callback->fn);
  caml_stat_free(callback);
}

static JSValue qml_throw_ocaml_exception(JSContext *ctx, value exception)
{
  char *message = caml_format_exception(exception);
  JSValue result = JS_ThrowInternalError(ctx, "%s", message);
  caml_stat_free(message);
  return result;
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
    Store_field(args, i, qml_alloc_value(ctx, JS_DupValue(ctx, argv[i])));
  }

  result = caml_callback_exn(callback->fn, args);
  if (Is_exception_result(result)) {
    JSValue exception =
      qml_throw_ocaml_exception(ctx, Extract_exception(result));
    CAMLreturnT(JSValue, exception);
  }

  JSValue js_result = JS_DupValue(ctx, qml_js_value(result));
  CAMLreturnT(JSValue, js_result);
}

CAMLprim value qml_function_make(value context, value name, value arity, value fn)
{
  CAMLparam4(context, name, arity, fn);
  JSContext *ctx = qml_context(context);
  struct qml_callback *callback = caml_stat_alloc(sizeof(struct qml_callback));
  callback->fn = fn;
  caml_register_generational_global_root(&callback->fn);

  JSValue function =
    JS_NewCClosure(ctx, qml_host_call, String_val(name), qml_callback_finalize,
                   Int_val(arity), 0, callback);

  if (JS_IsException(function)) {
    qml_callback_finalize(callback);
    qml_raise_exception(ctx);
  }

  CAMLreturn(qml_alloc_value(ctx, function));
}
