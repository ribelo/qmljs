#include "qmljs_stubs.h"

CAMLprim value qml_context_create(value runtime)
{
  CAMLparam1(runtime);
  JSContext *ctx = JS_NewContext(qml_runtime(runtime));
  if (ctx == NULL) {
    qml_raise_string("could not allocate QuickJS context");
  }
  CAMLreturn(qml_alloc_context(runtime, ctx));
}

CAMLprim value qml_context_close(value context)
{
  CAMLparam1(context);
  struct qml_context *wrapped = Qml_context_val(context);
  struct qml_runtime_state *state = wrapped->state;
  if (wrapped->ctx == NULL) {
    CAMLreturn(Val_true);
  }
  qml_ensure_owner(state);
  /* Free any values whose wrappers were already collected, then free this
     context deterministically on the owner domain. Values that escaped the
     scope keep their JSValues live; they are released later with
     JS_FreeValueRT, which does not need this context. */
  qml_drain_deferred(state);
  JS_FreeContext(wrapped->ctx);
  wrapped->ctx = NULL;
  atomic_fetch_sub_explicit(&state->open_contexts, 1, memory_order_relaxed);
  CAMLreturn(Val_true);
}

CAMLprim value qml_context_eval(value context, value source, value filename, value flags)
{
  CAMLparam4(context, source, filename, flags);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  JSValue result =
    JS_Eval(ctx, String_val(source), caml_string_length(source),
            String_val(filename), Int_val(flags));
  if (JS_IsException(result)) {
    qml_raise_exception(ctx);
  }
  CAMLreturn(qml_alloc_value(context, result));
}

CAMLprim value qml_context_eval_module(value context, value source, value filename)
{
  CAMLparam3(context, source, filename);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);

  JSValue compiled =
    JS_Eval(ctx, String_val(source), caml_string_length(source),
            String_val(filename),
            JS_EVAL_TYPE_MODULE | JS_EVAL_FLAG_COMPILE_ONLY);
  if (JS_IsException(compiled)) {
    qml_raise_exception(ctx);
  }

  JSModuleDef *module = JS_VALUE_GET_PTR(compiled);
  JSValue eval_result = JS_EvalFunction(ctx, compiled);
  if (JS_IsException(eval_result)) {
    qml_raise_exception(ctx);
  }
  JS_FreeValue(ctx, eval_result);

  JSValue namespace = JS_GetModuleNamespace(ctx, module);
  if (JS_IsException(namespace)) {
    qml_raise_exception(ctx);
  }
  JSValue default_export = JS_GetPropertyStr(ctx, namespace, "default");
  JS_FreeValue(ctx, namespace);
  if (JS_IsException(default_export)) {
    qml_raise_exception(ctx);
  }

  CAMLreturn(qml_alloc_value(context, default_export));
}

CAMLprim value qml_context_global(value context)
{
  CAMLparam1(context);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  JSValue global = JS_GetGlobalObject(ctx);
  if (JS_IsException(global)) {
    qml_raise_exception(ctx);
  }
  CAMLreturn(qml_alloc_value(context, global));
}

CAMLprim value qml_context_set_global(value context, value name, value wrapped)
{
  CAMLparam3(context, name, wrapped);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  qml_ensure_same_runtime(Qml_context_val(context)->state, wrapped);
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
