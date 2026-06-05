#include "quick_ml_stubs.h"

CAMLprim value qml_runtime_create(value unit)
{
  CAMLparam1(unit);
  JSRuntime *rt = JS_NewRuntime();
  if (rt == NULL) {
    qml_raise_string("could not allocate QuickJS runtime");
  }
  CAMLreturn(qml_alloc_runtime(rt));
}

CAMLprim value qml_runtime_is_job_pending(value runtime)
{
  return Val_bool(JS_IsJobPending(Qml_runtime_val(runtime)->rt));
}

CAMLprim value qml_runtime_execute_pending_job(value runtime)
{
  CAMLparam1(runtime);
  JSContext *ctx = NULL;
  int result = JS_ExecutePendingJob(qml_runtime(runtime), &ctx);
  if (result < 0) {
    if (ctx != NULL) {
      qml_raise_exception(ctx);
    }
    qml_raise_string("QuickJS job failed");
  }
  CAMLreturn(Val_bool(result > 0));
}
