#include "qmljs_stubs.h"

static int qml_interrupt_handler(JSRuntime *rt, void *opaque)
{
  (void)rt;
  struct qml_runtime_state *state = opaque;
  return atomic_load_explicit(&state->interrupt_requested, memory_order_relaxed);
}

CAMLprim value qml_runtime_create(value unit)
{
  CAMLparam1(unit);
  struct qml_runtime_state *state =
    caml_stat_alloc(sizeof(struct qml_runtime_state));
  atomic_init(&state->interrupt_requested, 0);

  JSRuntime *rt = JS_NewRuntime();
  if (rt == NULL) {
    caml_stat_free(state);
    qml_raise_string("could not allocate QuickJS runtime");
  }
  JS_SetInterruptHandler(rt, qml_interrupt_handler, state);
  CAMLreturn(qml_alloc_runtime(rt, state));
}

CAMLprim value qml_runtime_is_job_pending(value runtime)
{
  return Val_bool(JS_IsJobPending(Qml_runtime_val(runtime)->rt));
}

CAMLprim value qml_runtime_execute_pending_job(value runtime)
{
  CAMLparam1(runtime);
  JSContext *ctx = NULL;
  JSRuntime *rt = qml_runtime(runtime);
  JS_UpdateStackTop(rt);
  int result = JS_ExecutePendingJob(rt, &ctx);
  if (result < 0) {
    if (ctx != NULL) {
      qml_raise_exception(ctx);
    }
    qml_raise_string("QuickJS job failed");
  }
  CAMLreturn(Val_bool(result > 0));
}

CAMLprim value qml_runtime_request_interrupt(value runtime)
{
  atomic_store_explicit(&Qml_runtime_val(runtime)->state->interrupt_requested, 1,
                        memory_order_relaxed);
  return Val_unit;
}

CAMLprim value qml_runtime_clear_interrupt(value runtime)
{
  atomic_store_explicit(&Qml_runtime_val(runtime)->state->interrupt_requested, 0,
                        memory_order_relaxed);
  return Val_unit;
}

CAMLprim value qml_runtime_interrupt_requested(value runtime)
{
  return Val_bool(
    atomic_load_explicit(&Qml_runtime_val(runtime)->state->interrupt_requested,
                         memory_order_relaxed));
}
