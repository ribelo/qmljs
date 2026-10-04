#include "qmljs_stubs.h"

CAMLprim value qmljs_context_create(value runtime)
{
  CAMLparam1(runtime);
  JSContext *ctx = JS_NewContext(qmljs_runtime(runtime));
  if (ctx == NULL) {
    qmljs_raise_out_of_memory();
  }
  CAMLreturn(qmljs_alloc_context(runtime, ctx));
}

static int qmljs_add_intrinsic(JSContext *ctx, int tag)
{
  switch (tag) {
  case 0:
    return JS_AddIntrinsicDate(ctx);
  case 1:
    return JS_AddIntrinsicEval(ctx);
  case 2:
    JS_AddIntrinsicRegExpCompiler(ctx);
    return 0;
  case 3:
    return JS_AddIntrinsicRegExp(ctx);
  case 4:
    return JS_AddIntrinsicJSON(ctx);
  case 5:
    return JS_AddIntrinsicProxy(ctx);
  case 6:
    return JS_AddIntrinsicMapSet(ctx);
  case 7:
    return JS_AddIntrinsicTypedArrays(ctx);
  case 8:
    return JS_AddIntrinsicPromise(ctx);
  case 9:
    return JS_AddIntrinsicBigInt(ctx);
  case 10:
    return JS_AddIntrinsicWeakRef(ctx);
  case 11:
    return JS_AddPerformance(ctx);
  case 12:
    return JS_AddIntrinsicDOMException(ctx);
  default:
    JS_FreeContext(ctx);
    caml_invalid_argument("unknown QuickJS intrinsic");
  }
}

CAMLprim value qmljs_context_create_raw(value runtime, value intrinsics)
{
  CAMLparam2(runtime, intrinsics);
  JSContext *ctx = JS_NewContextRaw(qmljs_runtime(runtime));
  if (ctx == NULL) {
    qmljs_raise_out_of_memory();
  }
  if (JS_AddIntrinsicBaseObjects(ctx) < 0) {
    qmljs_free_context_and_raise_intrinsic_oom(ctx);
  }

  mlsize_t length = Wosize_val(intrinsics);
  for (mlsize_t i = 0; i < length; i++) {
    int result = qmljs_add_intrinsic(ctx, Int_val(Field(intrinsics, i)));
    if (result < 0) {
      qmljs_free_context_and_raise_intrinsic_oom(ctx);
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

static JSValue qmljs_async_eval_completion_value(JSContext *ctx,
                                                JSValueConst this_val,
                                                int argc,
                                                JSValueConst *argv)
{
  (void)this_val;
  if (argc < 1) {
    return JS_UNDEFINED;
  }
  return JS_GetPropertyStr(ctx, argv[0], "value");
}

static JSValue qmljs_unwrap_async_eval_result(JSContext *ctx,
                                               JSValueConst then_fn,
                                               JSValue promise)
{
  /* Use the pristine Promise.prototype.then captured at context creation, not a
     prototype-chain lookup, so the host-level async-eval result does not depend
     on user-mutable Promise.prototype.then. */
  if (!JS_IsFunction(ctx, then_fn)) {
    JS_FreeValue(ctx, promise);
    JS_ThrowTypeError(
      ctx, "async eval unwrap unavailable: Promise.prototype.then was not "
           "captured for this context");
    return JS_EXCEPTION;
  }

  JSValue unwrap =
    JS_NewCFunction(ctx, qmljs_async_eval_completion_value,
                    "qmljs_async_eval_completion_value", 1);
  if (JS_IsException(unwrap)) {
    JS_FreeValue(ctx, promise);
    return JS_EXCEPTION;
  }

  JSValue unwrapped = JS_Call(ctx, then_fn, promise, 1, &unwrap);
  JS_FreeValue(ctx, unwrap);
  JS_FreeValue(ctx, promise);
  return unwrapped;
}

CAMLprim value qmljs_context_eval(value context, value source, value filename, value flags)
{
  CAMLparam4(context, source, filename, flags);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  int eval_flags = Int_val(flags);
  JSValue result =
    JS_Eval(ctx, String_val(source), caml_string_length(source),
            String_val(filename), eval_flags);
  if (JS_IsException(result)) {
    qmljs_raise_exception(ctx);
  }
  if ((eval_flags & JS_EVAL_FLAG_ASYNC) != 0) {
    result = qmljs_unwrap_async_eval_result(
      ctx, qmljs_context_promise_then(context), result);
  }
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
  if (JS_PromiseState(ctx, result) == JS_PROMISE_REJECTED) {
    qmljs_raise_rejected_promise(ctx, result);
  }

  return JS_PromiseState(ctx, result) == JS_PROMISE_PENDING;
}

CAMLprim value qmljs_context_eval_module_namespace(value context, value source,
                                                 value filename)
{
  CAMLparam3(context, source, filename);
  CAMLlocal3(result, wrapped_namespace, wrapped_eval_result);
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
  /* Take ownership of each raw value before producing the next one, so the
     only unwrapped JSValue at any raising point is the one inside the
     qmljs_alloc_value that fails (which frees it). Already-wrapped values are
     owned by OCaml and freed by GC on unwind. */
  wrapped_eval_result = qmljs_alloc_value(context, eval_result);

  JSValue namespace = JS_GetModuleNamespace(ctx, module);
  if (JS_IsException(namespace)) {
    qmljs_raise_exception(ctx);
  }
  wrapped_namespace = qmljs_alloc_value(context, namespace);

  result = caml_alloc_tuple(2);
  Store_field(result, 0, wrapped_namespace);
  Store_field(result, 1, wrapped_eval_result);
  CAMLreturn(result);
}

CAMLprim value qmljs_context_eval_module_with_meta(value context, value source,
                                                 value filename)
{
  CAMLparam3(context, source, filename);
  CAMLlocal4(result, wrapped_namespace, wrapped_meta, wrapped_eval_result);
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
  /* Wrap each raw value before producing the next (see the namespace variant):
     at most one unwrapped JSValue is live at any raising point. */
  wrapped_eval_result = qmljs_alloc_value(context, eval_result);

  JSValue namespace = JS_GetModuleNamespace(ctx, module);
  if (JS_IsException(namespace)) {
    qmljs_raise_exception(ctx);
  }
  wrapped_namespace = qmljs_alloc_value(context, namespace);

  JSValue import_meta = JS_GetImportMeta(ctx, module);
  if (JS_IsException(import_meta)) {
    qmljs_raise_exception(ctx);
  }
  wrapped_meta = qmljs_alloc_value(context, import_meta);

  result = caml_alloc_tuple(3);
  Store_field(result, 0, wrapped_namespace);
  Store_field(result, 1, wrapped_meta);
  Store_field(result, 2, wrapped_eval_result);
  CAMLreturn(result);
}

CAMLprim value qmljs_module_dynamic_import(value context, value base,
                                         value specifier)
{
  CAMLparam3(context, base, specifier);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  /* JS_LoadModule forwards both names to the runtime normalize hook, which
     runs OCaml code: a minor collection there moves the caller's strings, so
     the [String_val] pointers must not reach QuickJS. Pin C copies for the
     whole load instead. NUL would make that copy silently truncate, so it is
     rejected rather than loading a different module. */
  size_t base_length = caml_string_length(base);
  size_t specifier_length = caml_string_length(specifier);
  if (memchr(String_val(base), '\0', base_length) != NULL) {
    caml_invalid_argument("dynamic import base name cannot contain NUL bytes");
  }
  if (memchr(String_val(specifier), '\0', specifier_length) != NULL) {
    caml_invalid_argument("dynamic import specifier cannot contain NUL bytes");
  }

  char *base_copy = js_malloc(ctx, base_length + 1);
  char *specifier_copy = js_malloc(ctx, specifier_length + 1);
  if (base_copy == NULL || specifier_copy == NULL) {
    js_free(ctx, base_copy);
    js_free(ctx, specifier_copy);
    qmljs_raise_exception(ctx);
  }
  memcpy(base_copy, String_val(base), base_length);
  base_copy[base_length] = '\0';
  memcpy(specifier_copy, String_val(specifier), specifier_length);
  specifier_copy[specifier_length] = '\0';

  JSValue promise = JS_LoadModule(ctx, base_copy, specifier_copy);
  js_free(ctx, base_copy);
  js_free(ctx, specifier_copy);
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
