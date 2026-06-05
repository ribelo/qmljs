#include "quick_ml_stubs.h"

CAMLprim value qml_context_create(value runtime)
{
  CAMLparam1(runtime);
  JSContext *ctx = JS_NewContext(qml_runtime(runtime));
  if (ctx == NULL) {
    qml_raise_string("could not allocate QuickJS context");
  }
  CAMLreturn(qml_alloc_context(ctx));
}

CAMLprim value qml_context_eval(value context, value source, value filename, value flags)
{
  CAMLparam4(context, source, filename, flags);
  JSContext *ctx = qml_context(context);
  JSValue result =
    JS_Eval(ctx, String_val(source), caml_string_length(source),
            String_val(filename), Int_val(flags));
  if (JS_IsException(result)) {
    qml_raise_exception(ctx);
  }
  CAMLreturn(qml_alloc_value(ctx, result));
}

CAMLprim value qml_context_global(value context)
{
  CAMLparam1(context);
  JSContext *ctx = qml_context(context);
  JSValue global = JS_GetGlobalObject(ctx);
  if (JS_IsException(global)) {
    qml_raise_exception(ctx);
  }
  CAMLreturn(qml_alloc_value(ctx, global));
}

CAMLprim value qml_context_set_global(value context, value name, value wrapped)
{
  CAMLparam3(context, name, wrapped);
  JSContext *ctx = qml_context(context);
  JSValue global = JS_GetGlobalObject(ctx);
  if (JS_IsException(global)) {
    qml_raise_exception(ctx);
  }
  JSValue property_value = JS_DupValue(ctx, qml_js_value(wrapped));
  int result = JS_SetPropertyStr(ctx, global, String_val(name), property_value);
  JS_FreeValue(ctx, global);
  if (result < 0) {
    qml_raise_exception(ctx);
  }
  CAMLreturn(Val_unit);
}
