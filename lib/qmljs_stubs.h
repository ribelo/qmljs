#ifndef QMLJS_STUBS_H
#define QMLJS_STUBS_H

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <quickjs.h>

#include <stdatomic.h>

struct qml_runtime_state {
  atomic_int interrupt_requested;
};

struct qml_runtime {
  JSRuntime *rt;
  struct qml_runtime_state *state;
};

struct qml_context {
  JSContext *ctx;
};

struct qml_value {
  JSContext *ctx;
  JSValue value;
};

#define Qml_runtime_val(v) ((struct qml_runtime *)Data_custom_val(v))
#define Qml_context_val(v) ((struct qml_context *)Data_custom_val(v))
#define Qml_value_val(v) ((struct qml_value *)Data_custom_val(v))

value qml_alloc_runtime(JSRuntime *rt, struct qml_runtime_state *state);
value qml_alloc_context(JSContext *ctx);
value qml_alloc_value(JSContext *ctx, JSValue js_value);

void qml_raise_string(const char *message);
void qml_raise_exception(JSContext *ctx);

JSRuntime *qml_runtime(value runtime);
JSContext *qml_context(value context);
JSValue qml_js_value(value wrapped);
void qml_update_stack_top(JSContext *ctx);

#endif
