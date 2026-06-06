#ifndef QMLJS_STUBS_H
#define QMLJS_STUBS_H

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/domain_state.h>

#include <quickjs.h>

#include <stdatomic.h>
#include <stddef.h>

struct qml_deferred_value;
struct qml_deferred_atom;
struct qml_deferred_context;
struct qml_promise_rejection_event;
struct qml_promise_hook_event;
struct qml_context_handle;
struct qml_value_handle;
struct qml_atom_handle;

/* Single source of truth for one QuickJS runtime. Reference counted so it
   outlives every wrapper (runtime/context/value) and every deferred native
   release node that can read it. Custom blocks never store OCaml values; the
   OCaml record chain (value -> context -> runtime in Qmljs_types) keeps the
   parents alive for the GC. The C layer only keeps native pointers and the
   shared state pointer. */
struct qml_runtime_state {
  atomic_uint refs;                  /* state memory lifetime */
  caml_domain_state *owner_domain;
  JSRuntime *rt;                     /* NULL once JS_FreeRuntime has run */
  atomic_int native_runtime_alive;   /* 1 until JS_FreeRuntime */
  atomic_int closed_to_user;         /* 1 once close has been requested */
  atomic_int open_contexts;          /* live JSContexts not yet freed */
  atomic_int live_values;            /* live JSValues not yet freed */
  atomic_int live_atoms;             /* live JSAtoms not yet freed */
  atomic_int ocaml_callback_depth;   /* OCaml currently entered from QuickJS */
  atomic_int deferred_drain_depth;   /* prevents recursive deferred drains */
  atomic_flag list_lock;
  struct qml_context_handle *contexts;
  struct qml_value_handle *values;
  struct qml_atom_handle *atoms;
  atomic_int interrupt_requested;
  atomic_llong interrupt_deadline_ns;
  char *runtime_info;                /* retained for JS_SetRuntimeInfo */
  void *module_loader;               /* struct qml_module_loader *, owner-freed */
  int promise_rejection_tracking;
  struct qml_promise_rejection_event *promise_rejections_head;
  struct qml_promise_rejection_event *promise_rejections_tail;
  int promise_hook_tracking;
  struct qml_promise_hook_event *promise_hooks_head;
  struct qml_promise_hook_event *promise_hooks_tail;
  JSClassID host_function_class_id;
  int host_function_class_registered;
  JSClassID native_module_class_id;
  int native_module_class_registered;
  _Atomic(struct qml_deferred_value *) deferred_values;
  _Atomic(struct qml_deferred_atom *) deferred_atoms;
  _Atomic(struct qml_deferred_context *) deferred_contexts;
};

struct qml_runtime {
  struct qml_runtime_state *state;
};

struct qml_context {
  struct qml_context_handle *handle;
};

struct qml_value {
  struct qml_value_handle *handle;
};

struct qml_atom {
  struct qml_atom_handle *handle;
};

#define Qml_runtime_val(v) ((struct qml_runtime *)Data_custom_val(v))
#define Qml_context_val(v) ((struct qml_context *)Data_custom_val(v))
#define Qml_value_val(v) ((struct qml_value *)Data_custom_val(v))
#define Qml_atom_val(v) ((struct qml_atom *)Data_custom_val(v))

value qml_alloc_runtime(JSRuntime *rt, struct qml_runtime_state *state);
value qml_alloc_context(value runtime, JSContext *ctx);
value qml_alloc_value(value context, JSValue js_value);
value qml_alloc_atom(struct qml_runtime_state *state, JSAtom atom);

void qml_raise_string(const char *message);
void qml_raise_exception(JSContext *ctx);
void qml_raise_value_exception(JSContext *ctx, JSValueConst exception);
value qml_structured_of_exception_value(JSContext *ctx, JSValueConst exception);

JSRuntime *qml_runtime(value runtime);
JSContext *qml_context(value context);
struct qml_runtime_state *qml_context_state(value context);
JSValue qml_js_value(value wrapped);
const char *qml_js_value_for_context(value context, value wrapped, JSValue *out);
JSAtom qml_js_atom(value wrapped);
void qml_update_stack_top(JSContext *ctx);
void qml_ensure_owner(struct qml_runtime_state *state);
void qml_ensure_same_runtime(struct qml_runtime_state *expected, value wrapped);
void qml_ensure_same_context(value context, value wrapped);
void qml_ensure_atom_runtime(struct qml_runtime_state *expected, value wrapped);
void qml_drain_deferred(struct qml_runtime_state *state);
void qml_drain_deferred_runtimes(void);
void qml_enter_ocaml_callback(struct qml_runtime_state *state);
void qml_leave_ocaml_callback(struct qml_runtime_state *state);
JSValueConst *qml_alloc_argv(size_t argc);
void qml_free_argv(JSValueConst *argv);
int qml_argc_of_size(size_t argc);
void qml_argv_set(value context, JSValueConst *argv, size_t index,
                  value wrapped);
int qml_set_property_ocaml_string(JSContext *ctx, JSValueConst object,
                                  value name, JSValue property_value);

/* Refcounted state lifetime. */
void qml_state_ref(struct qml_runtime_state *state);
void qml_state_unref(struct qml_runtime_state *state);

/* Native runtime teardown helpers shared by close and finalizers. */
int qml_runtime_free_native(struct qml_runtime_state *state);
int qml_context_close_native(value context);

/* Release the installed module loader (owner domain only). */
void qml_module_loader_free(struct qml_runtime_state *state);
void qml_promise_events_free(struct qml_runtime_state *state);

#endif
