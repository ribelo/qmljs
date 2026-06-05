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
  state->owner_domain = Caml_state;
  atomic_init(&state->closed, 0);
  atomic_init(&state->deferred_values, NULL);
  atomic_init(&state->deferred_contexts, NULL);

  JSRuntime *rt = JS_NewRuntime();
  if (rt == NULL) {
    caml_stat_free(state);
    qml_raise_string("could not allocate QuickJS runtime");
  }
  JS_SetInterruptHandler(rt, qml_interrupt_handler, state);
  CAMLreturn(qml_alloc_runtime(rt, state));
}

CAMLprim value qml_runtime_set_memory_limit(value runtime, value limit)
{
  CAMLparam2(runtime, limit);
  long bytes = Long_val(limit);
  if (bytes < 0) {
    qml_raise_string("memory limit must be non-negative");
  }
  JS_SetMemoryLimit(qml_runtime(runtime), (size_t)bytes);
  CAMLreturn(Val_unit);
}

CAMLprim value qml_runtime_set_max_stack_size(value runtime, value size)
{
  CAMLparam2(runtime, size);
  long bytes = Long_val(size);
  if (bytes < 0) {
    qml_raise_string("max stack size must be non-negative");
  }
  JS_SetMaxStackSize(qml_runtime(runtime), (size_t)bytes);
  CAMLreturn(Val_unit);
}

CAMLprim value qml_runtime_drain_deferred(value runtime)
{
  CAMLparam1(runtime);
  qml_drain_deferred(Qml_runtime_val(runtime)->state);
  CAMLreturn(Val_unit);
}

CAMLprim value qml_runtime_close(value runtime)
{
  CAMLparam1(runtime);
  struct qml_runtime *wrapped = Qml_runtime_val(runtime);
  if (wrapped->rt != NULL) {
    qml_drain_deferred(wrapped->state);
    atomic_store_explicit(&wrapped->state->closed, 1, memory_order_release);
    JS_FreeRuntime(wrapped->rt);
    wrapped->rt = NULL;
  }
  CAMLreturn(Val_unit);
}

CAMLprim value qml_runtime_is_job_pending(value runtime)
{
  CAMLparam1(runtime);
  CAMLreturn(Val_bool(JS_IsJobPending(qml_runtime(runtime))));
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
