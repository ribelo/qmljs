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

struct qmljs_deferred_value;
struct qmljs_deferred_atom;
struct qmljs_deferred_context;
struct qmljs_class_instance;
struct qmljs_promise_rejection_event;
struct qmljs_promise_hook_event;
struct qmljs_context_handle;
struct qmljs_value_handle;
struct qmljs_atom_handle;

/* Single source of truth for one QuickJS runtime. Reference counted so it
   outlives every wrapper (runtime/context/value) and every deferred native
   release node that can read it. Custom blocks never store OCaml values; the
   OCaml record chain (value -> context -> runtime in Qmljs_types) keeps the
   parents alive for the GC. The C layer only keeps native pointers and the
   shared state pointer. */
struct qmljs_runtime_state {
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
  struct qmljs_context_handle *contexts;
  struct qmljs_value_handle *values;
  struct qmljs_atom_handle *atoms;
  atomic_int interrupt_requested;
  atomic_llong interrupt_deadline_ns;
  char *runtime_info;                /* retained for JS_SetRuntimeInfo */
  void *module_loader;               /* struct qmljs_module_loader *, owner-freed */
  int promise_rejection_tracking;
  struct qmljs_promise_rejection_event *promise_rejections_head;
  struct qmljs_promise_rejection_event *promise_rejections_tail;
  int promise_hook_tracking;
  struct qmljs_promise_hook_event *promise_hooks_head;
  struct qmljs_promise_hook_event *promise_hooks_tail;
  JSClassID host_function_class_id;
  int host_function_class_registered;
  JSClassID native_module_class_id;
  int native_module_class_registered;
  JSClassID error_marker_class_id;
  int error_marker_class_registered;
  _Atomic(struct qmljs_deferred_value *) deferred_values;
  _Atomic(struct qmljs_deferred_atom *) deferred_atoms;
  _Atomic(struct qmljs_deferred_context *) deferred_contexts;
  _Atomic(struct qmljs_class_instance *) deferred_class_finalizers;
};

struct qmljs_runtime {
  struct qmljs_runtime_state *state;
};

struct qmljs_context {
  struct qmljs_context_handle *handle;
};

struct qmljs_value {
  struct qmljs_value_handle *handle;
};

struct qmljs_atom {
  struct qmljs_atom_handle *handle;
};

#define Qml_runtime_val(v) ((struct qmljs_runtime *)Data_custom_val(v))
#define Qml_context_val(v) ((struct qmljs_context *)Data_custom_val(v))
#define Qml_value_val(v) ((struct qmljs_value *)Data_custom_val(v))
#define Qml_atom_val(v) ((struct qmljs_atom *)Data_custom_val(v))

value qmljs_alloc_runtime(JSRuntime *rt, struct qmljs_runtime_state *state);
value qmljs_alloc_context(value runtime, JSContext *ctx);
value qmljs_alloc_value(value context, JSValue js_value);
value qmljs_alloc_atom(struct qmljs_runtime_state *state, JSAtom atom);

void qmljs_raise_string(const char *message);
void qmljs_raise_exception(JSContext *ctx);
void qmljs_raise_value_exception(JSContext *ctx, JSValueConst exception);
value qmljs_structured_of_exception_value(JSContext *ctx, JSValueConst exception);

JSRuntime *qmljs_runtime(value runtime);
JSContext *qmljs_context(value context);
struct qmljs_runtime_state *qmljs_context_state(value context);
JSValue qmljs_js_value(value wrapped);
const char *qmljs_js_value_for_context(value context, value wrapped, JSValue *out);
JSAtom qmljs_js_atom(value wrapped);
void qmljs_update_stack_top(JSContext *ctx);
void qmljs_ensure_owner(struct qmljs_runtime_state *state);
void qmljs_ensure_same_runtime(struct qmljs_runtime_state *expected, value wrapped);
void qmljs_ensure_same_context(value context, value wrapped);
void qmljs_ensure_atom_runtime(struct qmljs_runtime_state *expected, value wrapped);
void qmljs_drain_deferred(struct qmljs_runtime_state *state);
void qmljs_drain_deferred_runtimes(void);
void qmljs_drain_class_finalizers(struct qmljs_runtime_state *state);
void qmljs_enter_ocaml_callback(struct qmljs_runtime_state *state);
void qmljs_leave_ocaml_callback(struct qmljs_runtime_state *state);
JSValue qmljs_new_error_marker(JSContext *ctx);
int qmljs_is_error_marker(JSContext *ctx, JSValueConst value);
JSValueConst *qmljs_alloc_argv(size_t argc);
void qmljs_free_argv(JSValueConst *argv);
int qmljs_argc_of_size(size_t argc);
void qmljs_argv_check(value context, value wrapped);
void qmljs_argv_set(value context, JSValueConst *argv, size_t index,
                  value wrapped);
int qmljs_set_property_ocaml_string(JSContext *ctx, JSValueConst object,
                                  value name, JSValue property_value);

/* Refcounted state lifetime. */
void qmljs_state_ref(struct qmljs_runtime_state *state);
void qmljs_state_unref(struct qmljs_runtime_state *state);

/* Native runtime teardown helpers shared by close and finalizers. */
int qmljs_runtime_free_native(struct qmljs_runtime_state *state);
int qmljs_context_close_native(value context);

/* Release the installed module loader (owner domain only). */
void qmljs_module_loader_free(struct qmljs_runtime_state *state);
void qmljs_promise_events_free(struct qmljs_runtime_state *state);

#endif
