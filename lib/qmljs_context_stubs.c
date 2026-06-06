#include "qmljs_stubs.h"

#include <stdio.h>

#define QML_MODULE_EVAL_JOB_LIMIT 1000000

CAMLprim value qmljs_context_create(value runtime)
{
  CAMLparam1(runtime);
  JSContext *ctx = JS_NewContext(qmljs_runtime(runtime));
  if (ctx == NULL) {
    qmljs_raise_string("could not allocate QuickJS context");
  }
  CAMLreturn(qmljs_alloc_context(runtime, ctx));
}

static int qmljs_add_intrinsic(JSContext *ctx, int tag, const char **name)
{
  switch (tag) {
  case 0:
    *name = "Date";
    return JS_AddIntrinsicDate(ctx);
  case 1:
    *name = "Eval";
    return JS_AddIntrinsicEval(ctx);
  case 2:
    *name = "RegExpCompiler";
    JS_AddIntrinsicRegExpCompiler(ctx);
    return 0;
  case 3:
    *name = "RegExp";
    return JS_AddIntrinsicRegExp(ctx);
  case 4:
    *name = "JSON";
    return JS_AddIntrinsicJSON(ctx);
  case 5:
    *name = "Proxy";
    return JS_AddIntrinsicProxy(ctx);
  case 6:
    *name = "MapSet";
    return JS_AddIntrinsicMapSet(ctx);
  case 7:
    *name = "TypedArrays";
    return JS_AddIntrinsicTypedArrays(ctx);
  case 8:
    *name = "Promise";
    return JS_AddIntrinsicPromise(ctx);
  case 9:
    *name = "BigInt";
    return JS_AddIntrinsicBigInt(ctx);
  case 10:
    *name = "WeakRef";
    return JS_AddIntrinsicWeakRef(ctx);
  case 11:
    *name = "Performance";
    return JS_AddPerformance(ctx);
  case 12:
    *name = "DOMException";
    return JS_AddIntrinsicDOMException(ctx);
  default:
    *name = "unknown";
    return -1;
  }
}

static void qmljs_free_context_and_raise_intrinsic(JSContext *ctx,
                                                 const char *name)
{
  char message[128];
  snprintf(message, sizeof(message), "could not add QuickJS %s intrinsic", name);
  JS_FreeContext(ctx);
  qmljs_raise_string(message);
}

CAMLprim value qmljs_context_create_raw(value runtime, value intrinsics)
{
  CAMLparam2(runtime, intrinsics);
  JSContext *ctx = JS_NewContextRaw(qmljs_runtime(runtime));
  if (ctx == NULL) {
    qmljs_raise_string("could not allocate raw QuickJS context");
  }
  if (JS_AddIntrinsicBaseObjects(ctx) < 0) {
    qmljs_free_context_and_raise_intrinsic(ctx, "base objects");
  }

  mlsize_t length = Wosize_val(intrinsics);
  for (mlsize_t i = 0; i < length; i++) {
    const char *name = NULL;
    int result = qmljs_add_intrinsic(ctx, Int_val(Field(intrinsics, i)), &name);
    if (result < 0) {
      qmljs_free_context_and_raise_intrinsic(ctx, name);
    }
  }

  CAMLreturn(qmljs_alloc_context(runtime, ctx));
}

CAMLprim value qmljs_context_close(value context)
{
  CAMLparam1(context);
  CAMLreturn(Val_bool(qmljs_context_close_native(context)));
}

CAMLprim value qmljs_test_context_close(value context)
{
  CAMLparam1(context);
  CAMLreturn(qmljs_context_close(Field(context, 1)));
}

CAMLprim value qmljs_context_eval(value context, value source, value filename, value flags)
{
  CAMLparam4(context, source, filename, flags);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  JSValue result =
    JS_Eval(ctx, String_val(source), caml_string_length(source),
            String_val(filename), Int_val(flags));
  if (JS_IsException(result)) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(qmljs_alloc_value(context, result));
}

static void qmljs_raise_rejected_promise(JSContext *ctx, JSValue promise)
{
  JSValue reason = JS_PromiseResult(ctx, promise);
  JS_FreeValue(ctx, promise);
  (void)JS_Throw(ctx, reason);
  qmljs_raise_exception(ctx);
}

static int qmljs_check_module_eval_result(JSContext *ctx, JSValue result)
{
  if (!JS_IsPromise(result)) {
    return 0;
  }

  JSRuntime *rt = JS_GetRuntime(ctx);
  int jobs = 0;
  while (JS_PromiseState(ctx, result) == JS_PROMISE_PENDING &&
         JS_IsJobPending(rt)) {
    if (jobs >= QML_MODULE_EVAL_JOB_LIMIT) {
      JS_FreeValue(ctx, result);
      qmljs_raise_string("QuickJS promise job limit exceeded");
    }
    JSContext *job_ctx = NULL;
    JS_UpdateStackTop(rt);
    int job_result = JS_ExecutePendingJob(rt, &job_ctx);
    if (job_result < 0) {
      JS_FreeValue(ctx, result);
      if (job_ctx != NULL) {
        qmljs_raise_exception(job_ctx);
      }
      qmljs_raise_string("QuickJS job failed");
    }
    if (job_result == 0) {
      break;
    }
    jobs++;
  }

  if (JS_PromiseState(ctx, result) == JS_PROMISE_REJECTED) {
    qmljs_raise_rejected_promise(ctx, result);
  }

  return JS_PromiseState(ctx, result) == JS_PROMISE_PENDING;
}

CAMLprim value qmljs_context_eval_module(value context, value source, value filename)
{
  CAMLparam3(context, source, filename);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);

  JSValue compiled =
    JS_Eval(ctx, String_val(source), caml_string_length(source),
            String_val(filename),
            JS_EVAL_TYPE_MODULE | JS_EVAL_FLAG_COMPILE_ONLY);
  if (JS_IsException(compiled)) {
    qmljs_raise_exception(ctx);
  }

  JSModuleDef *module = JS_VALUE_GET_PTR(compiled);
  JSValue eval_result = JS_EvalFunction(ctx, compiled);
  if (JS_IsException(eval_result)) {
    qmljs_raise_exception(ctx);
  }
  (void)qmljs_check_module_eval_result(ctx, eval_result);
  JS_FreeValue(ctx, eval_result);

  JSValue namespace = JS_GetModuleNamespace(ctx, module);
  if (JS_IsException(namespace)) {
    qmljs_raise_exception(ctx);
  }
  JSValue default_export = JS_GetPropertyStr(ctx, namespace, "default");
  JS_FreeValue(ctx, namespace);
  if (JS_IsException(default_export)) {
    qmljs_raise_exception(ctx);
  }

  CAMLreturn(qmljs_alloc_value(context, default_export));
}

CAMLprim value qmljs_context_eval_module_namespace(value context, value source,
                                                 value filename)
{
  CAMLparam3(context, source, filename);
  CAMLlocal2(result, wrapped_namespace);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);

  JSValue compiled =
    JS_Eval(ctx, String_val(source), caml_string_length(source),
            String_val(filename),
            JS_EVAL_TYPE_MODULE | JS_EVAL_FLAG_COMPILE_ONLY);
  if (JS_IsException(compiled)) {
    qmljs_raise_exception(ctx);
  }

  JSModuleDef *module = JS_VALUE_GET_PTR(compiled);
  JSValue eval_result = JS_EvalFunction(ctx, compiled);
  if (JS_IsException(eval_result)) {
    qmljs_raise_exception(ctx);
  }
  int pending = qmljs_check_module_eval_result(ctx, eval_result);
  JS_FreeValue(ctx, eval_result);

  JSValue namespace = JS_GetModuleNamespace(ctx, module);
  if (JS_IsException(namespace)) {
    qmljs_raise_exception(ctx);
  }

  wrapped_namespace = qmljs_alloc_value(context, namespace);
  result = caml_alloc_tuple(2);
  Store_field(result, 0, wrapped_namespace);
  Store_field(result, 1, Val_bool(pending));
  CAMLreturn(result);
}

CAMLprim value qmljs_context_eval_module_with_meta(value context, value source,
                                                 value filename)
{
  CAMLparam3(context, source, filename);
  CAMLlocal3(result, wrapped_namespace, wrapped_meta);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);

  JSValue compiled =
    JS_Eval(ctx, String_val(source), caml_string_length(source),
            String_val(filename),
            JS_EVAL_TYPE_MODULE | JS_EVAL_FLAG_COMPILE_ONLY);
  if (JS_IsException(compiled)) {
    qmljs_raise_exception(ctx);
  }

  JSModuleDef *module = JS_VALUE_GET_PTR(compiled);
  JSValue eval_result = JS_EvalFunction(ctx, compiled);
  if (JS_IsException(eval_result)) {
    qmljs_raise_exception(ctx);
  }
  int pending = qmljs_check_module_eval_result(ctx, eval_result);
  JS_FreeValue(ctx, eval_result);

  JSValue namespace = JS_GetModuleNamespace(ctx, module);
  if (JS_IsException(namespace)) {
    qmljs_raise_exception(ctx);
  }
  JSValue import_meta = JS_GetImportMeta(ctx, module);
  if (JS_IsException(import_meta)) {
    JS_FreeValue(ctx, namespace);
    qmljs_raise_exception(ctx);
  }

  wrapped_namespace = qmljs_alloc_value(context, namespace);
  wrapped_meta = qmljs_alloc_value(context, import_meta);
  result = caml_alloc_tuple(3);
  Store_field(result, 0, wrapped_namespace);
  Store_field(result, 1, wrapped_meta);
  Store_field(result, 2, Val_bool(pending));
  CAMLreturn(result);
}

CAMLprim value qmljs_module_dynamic_import(value context, value base,
                                         value specifier)
{
  CAMLparam3(context, base, specifier);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  JSValue promise = JS_LoadModule(ctx, String_val(base), String_val(specifier));
  if (JS_IsException(promise)) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(qmljs_alloc_value(context, promise));
}

CAMLprim value qmljs_context_global(value context)
{
  CAMLparam1(context);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  JSValue global = JS_GetGlobalObject(ctx);
  if (JS_IsException(global)) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(qmljs_alloc_value(context, global));
}

CAMLprim value qmljs_context_set_global(value context, value name, value wrapped)
{
  CAMLparam3(context, name, wrapped);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  qmljs_ensure_same_context(context, wrapped);
  JSValue global = JS_GetGlobalObject(ctx);
  if (JS_IsException(global)) {
    qmljs_raise_exception(ctx);
  }
  JSValue property_value = JS_DupValue(ctx, qmljs_js_value(wrapped));
  int result = qmljs_set_property_ocaml_string(ctx, global, name, property_value);
  JS_FreeValue(ctx, global);
  if (result < 0) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(Val_unit);
}
