#include "qmljs_stubs.h"

#include <limits.h>
#include <string.h>

/* Diagnostics count live OCaml wrappers and selected native resources whose
   lifetime should return to baseline at safe boundaries. This is leak evidence,
   not a native heap profiler. */
static atomic_int qmljs_live_runtime_wrappers;
static atomic_int qmljs_live_context_wrappers;
static atomic_int qmljs_live_value_wrappers;
static atomic_int qmljs_live_native_runtimes;
static atomic_int qmljs_live_argv_arrays;

struct qmljs_deferred_value {
  struct qmljs_runtime_state *state;
  JSValue value;
  struct qmljs_deferred_value *next;
};

struct qmljs_deferred_atom {
  struct qmljs_runtime_state *state;
  JSAtom atom;
  struct qmljs_deferred_atom *next;
};

struct qmljs_deferred_context {
  struct qmljs_runtime_state *state;
  JSContext *ctx;
  struct qmljs_deferred_context *next;
};

struct qmljs_deferred_runtime {
  struct qmljs_runtime_state *state;
  struct qmljs_deferred_runtime *next;
};

static _Atomic(struct qmljs_deferred_runtime *) qmljs_deferred_runtimes;
static atomic_int qmljs_deferred_runtime_drain_depth;

JSValueConst *qmljs_alloc_argv(size_t argc)
{
  JSValueConst *argv = caml_stat_alloc(sizeof(JSValueConst) * argc);
  atomic_fetch_add_explicit(&qmljs_live_argv_arrays, 1, memory_order_relaxed);
  return argv;
}

void qmljs_free_argv(JSValueConst *argv)
{
  if (argv != NULL) {
    atomic_fetch_sub_explicit(&qmljs_live_argv_arrays, 1, memory_order_relaxed);
    caml_stat_free(argv);
  }
}

int qmljs_argc_of_size(size_t argc)
{
  if (argc > (size_t)INT_MAX) {
    qmljs_raise_string("function argument count exceeds QuickJS argc range");
  }
  return (int)argc;
}

CAMLprim value qmljs_test_reject_arg_count(value argc)
{
  CAMLparam1(argc);
  intnat count = Nativeint_val(argc);
  if (count < 0) {
    qmljs_raise_string("function argument count exceeds QuickJS argc range");
  }
  (void)qmljs_argc_of_size((size_t)count);
  CAMLreturn(Val_unit);
}

void qmljs_argv_check(value context, value wrapped)
{
  JSValue js_value;
  const char *error = qmljs_js_value_for_context(context, wrapped, &js_value);
  if (error != NULL) {
    qmljs_raise_string(error);
  }
}

void qmljs_argv_set(value context, JSValueConst *argv, size_t index,
                  value wrapped)
{
  JSValue js_value;
  const char *error = qmljs_js_value_for_context(context, wrapped, &js_value);
  if (error != NULL) {
    qmljs_free_argv(argv);
    qmljs_raise_string(error);
  }
  argv[index] = js_value;
}

int qmljs_set_property_ocaml_string(JSContext *ctx, JSValueConst object,
                                  value name, JSValue property_value)
{
  JSAtom atom = JS_NewAtomLen(ctx, String_val(name), caml_string_length(name));
  if (atom == JS_ATOM_NULL) {
    JS_FreeValue(ctx, property_value);
    return -1;
  }
  int result = JS_SetProperty(ctx, object, atom, property_value);
  JS_FreeAtom(ctx, atom);
  return result;
}

struct qmljs_context_handle {
  struct qmljs_runtime_state *state;
  JSContext *ctx;
  atomic_int live;
  int linked;
  struct qmljs_context_handle *prev;
  struct qmljs_context_handle *next;
};

struct qmljs_value_handle {
  struct qmljs_runtime_state *state;
  struct qmljs_context_handle *context;
  JSValue value;
  atomic_int live;
  int linked;
  struct qmljs_value_handle *prev;
  struct qmljs_value_handle *next;
};

struct qmljs_atom_handle {
  struct qmljs_runtime_state *state;
  JSAtom atom;
  atomic_int live;
  int linked;
  struct qmljs_atom_handle *prev;
  struct qmljs_atom_handle *next;
};

static void qmljs_lock_lists(struct qmljs_runtime_state *state)
{
  while (atomic_flag_test_and_set_explicit(&state->list_lock,
                                           memory_order_acquire)) {
  }
}

static void qmljs_unlock_lists(struct qmljs_runtime_state *state)
{
  atomic_flag_clear_explicit(&state->list_lock, memory_order_release);
}

static void qmljs_link_context(struct qmljs_runtime_state *state,
                             struct qmljs_context_handle *handle)
{
  qmljs_lock_lists(state);
  handle->prev = NULL;
  handle->next = state->contexts;
  if (state->contexts != NULL) {
    state->contexts->prev = handle;
  }
  state->contexts = handle;
  handle->linked = 1;
  qmljs_unlock_lists(state);
}

static void qmljs_unlink_context(struct qmljs_runtime_state *state,
                               struct qmljs_context_handle *handle)
{
  qmljs_lock_lists(state);
  if (handle->linked) {
    if (handle->prev != NULL) {
      handle->prev->next = handle->next;
    } else {
      state->contexts = handle->next;
    }
    if (handle->next != NULL) {
      handle->next->prev = handle->prev;
    }
    handle->prev = NULL;
    handle->next = NULL;
    handle->linked = 0;
  }
  qmljs_unlock_lists(state);
}

static void qmljs_link_value(struct qmljs_runtime_state *state,
                           struct qmljs_value_handle *handle)
{
  qmljs_lock_lists(state);
  handle->prev = NULL;
  handle->next = state->values;
  if (state->values != NULL) {
    state->values->prev = handle;
  }
  state->values = handle;
  handle->linked = 1;
  qmljs_unlock_lists(state);
}

static void qmljs_unlink_value(struct qmljs_runtime_state *state,
                             struct qmljs_value_handle *handle)
{
  qmljs_lock_lists(state);
  if (handle->linked) {
    if (handle->prev != NULL) {
      handle->prev->next = handle->next;
    } else {
      state->values = handle->next;
    }
    if (handle->next != NULL) {
      handle->next->prev = handle->prev;
    }
    handle->prev = NULL;
    handle->next = NULL;
    handle->linked = 0;
  }
  qmljs_unlock_lists(state);
}

static void qmljs_link_atom(struct qmljs_runtime_state *state,
                          struct qmljs_atom_handle *handle)
{
  qmljs_lock_lists(state);
  handle->prev = NULL;
  handle->next = state->atoms;
  if (state->atoms != NULL) {
    state->atoms->prev = handle;
  }
  state->atoms = handle;
  handle->linked = 1;
  qmljs_unlock_lists(state);
}

static void qmljs_unlink_atom(struct qmljs_runtime_state *state,
                            struct qmljs_atom_handle *handle)
{
  qmljs_lock_lists(state);
  if (handle->linked) {
    if (handle->prev != NULL) {
      handle->prev->next = handle->next;
    } else {
      state->atoms = handle->next;
    }
    if (handle->next != NULL) {
      handle->next->prev = handle->prev;
    }
    handle->prev = NULL;
    handle->next = NULL;
    handle->linked = 0;
  }
  qmljs_unlock_lists(state);
}

void qmljs_state_ref(struct qmljs_runtime_state *state)
{
  atomic_fetch_add_explicit(&state->refs, 1, memory_order_relaxed);
}

void qmljs_state_unref(struct qmljs_runtime_state *state)
{
  if (atomic_fetch_sub_explicit(&state->refs, 1, memory_order_acq_rel) == 1) {
    caml_stat_free(state);
  }
}

void qmljs_enter_ocaml_callback(struct qmljs_runtime_state *state)
{
  atomic_fetch_add_explicit(&state->ocaml_callback_depth, 1,
                            memory_order_relaxed);
}

void qmljs_leave_ocaml_callback(struct qmljs_runtime_state *state)
{
  atomic_fetch_sub_explicit(&state->ocaml_callback_depth, 1,
                            memory_order_relaxed);
}

static int qmljs_can_release_now(struct qmljs_runtime_state *state)
{
  return state != NULL && Caml_state == state->owner_domain &&
         atomic_load_explicit(&state->ocaml_callback_depth,
                              memory_order_acquire) == 0 &&
         atomic_load_explicit(&state->deferred_drain_depth,
                              memory_order_acquire) == 0;
}

void qmljs_ensure_owner(struct qmljs_runtime_state *state)
{
  if (state == NULL) {
    qmljs_raise_string("QuickJS runtime state is closed");
  }
  if (Caml_state != state->owner_domain) {
    qmljs_raise_string("QuickJS runtime used from a non-owner domain");
  }
  /* Deterministic close: once the user has closed the runtime, every handle
     raises, even if escaped value wrappers have delayed the native
     JS_FreeRuntime. native_runtime_alive guards only the internal free path. */
  if (atomic_load_explicit(&state->closed_to_user, memory_order_acquire) != 0
      || atomic_load_explicit(&state->native_runtime_alive,
                              memory_order_acquire) == 0) {
    qmljs_raise_string("QuickJS runtime is closed");
  }
}

static void qmljs_defer_runtime(struct qmljs_runtime_state *state)
{
  if (atomic_load_explicit(&state->native_runtime_alive, memory_order_acquire)
      == 0) {
    return;
  }
  struct qmljs_deferred_runtime *node =
    caml_stat_alloc(sizeof(struct qmljs_deferred_runtime));
  node->state = state;
  qmljs_state_ref(state);
  struct qmljs_deferred_runtime *head =
    atomic_load_explicit(&qmljs_deferred_runtimes, memory_order_acquire);
  do {
    node->next = head;
  } while (!atomic_compare_exchange_weak_explicit(
    &qmljs_deferred_runtimes, &head, node, memory_order_release,
    memory_order_acquire));
}

static void qmljs_requeue_deferred_runtime(struct qmljs_deferred_runtime *node)
{
  struct qmljs_deferred_runtime *head =
    atomic_load_explicit(&qmljs_deferred_runtimes, memory_order_acquire);
  do {
    node->next = head;
  } while (!atomic_compare_exchange_weak_explicit(
    &qmljs_deferred_runtimes, &head, node, memory_order_release,
    memory_order_acquire));
}

static void qmljs_defer_value(struct qmljs_runtime_state *state, JSValue value)
{
  if (atomic_load_explicit(&state->native_runtime_alive, memory_order_acquire)
      == 0) {
    /* Runtime already torn down: the JSValue is gone with it. This branch is
       unreachable in the consistent state machine because JS_FreeRuntime only
       runs when live_values == 0. */
    atomic_fetch_sub_explicit(&state->live_values, 1, memory_order_relaxed);
    return;
  }
  struct qmljs_deferred_value *node =
    caml_stat_alloc(sizeof(struct qmljs_deferred_value));
  node->state = state;
  node->value = value;
  qmljs_state_ref(state);
  struct qmljs_deferred_value *head =
    atomic_load_explicit(&state->deferred_values, memory_order_acquire);
  do {
    node->next = head;
  } while (!atomic_compare_exchange_weak_explicit(
    &state->deferred_values, &head, node, memory_order_release,
    memory_order_acquire));
}

static void qmljs_defer_atom(struct qmljs_runtime_state *state, JSAtom atom)
{
  if (atomic_load_explicit(&state->native_runtime_alive, memory_order_acquire)
      == 0) {
    atomic_fetch_sub_explicit(&state->live_atoms, 1, memory_order_relaxed);
    return;
  }
  struct qmljs_deferred_atom *node =
    caml_stat_alloc(sizeof(struct qmljs_deferred_atom));
  node->state = state;
  node->atom = atom;
  qmljs_state_ref(state);
  struct qmljs_deferred_atom *head =
    atomic_load_explicit(&state->deferred_atoms, memory_order_acquire);
  do {
    node->next = head;
  } while (!atomic_compare_exchange_weak_explicit(
    &state->deferred_atoms, &head, node, memory_order_release,
    memory_order_acquire));
}

static void qmljs_defer_context(struct qmljs_runtime_state *state, JSContext *ctx)
{
  if (atomic_load_explicit(&state->native_runtime_alive, memory_order_acquire)
      == 0) {
    atomic_fetch_sub_explicit(&state->open_contexts, 1, memory_order_relaxed);
    return;
  }
  struct qmljs_deferred_context *node =
    caml_stat_alloc(sizeof(struct qmljs_deferred_context));
  node->state = state;
  node->ctx = ctx;
  qmljs_state_ref(state);
  struct qmljs_deferred_context *head =
    atomic_load_explicit(&state->deferred_contexts, memory_order_acquire);
  do {
    node->next = head;
  } while (!atomic_compare_exchange_weak_explicit(
    &state->deferred_contexts, &head, node, memory_order_release,
    memory_order_acquire));
}

static void qmljs_release_value_for_scope(struct qmljs_value_handle *handle)
{
  struct qmljs_runtime_state *state = handle->state;
  if (atomic_exchange_explicit(&handle->live, 0, memory_order_acq_rel) == 0) {
    return;
  }
  qmljs_unlink_value(state, handle);
  if (atomic_load_explicit(&state->native_runtime_alive, memory_order_acquire)
      != 0 && state->rt != NULL) {
    JS_FreeValueRT(state->rt, handle->value);
  }
  handle->value = JS_UNDEFINED;
  atomic_fetch_sub_explicit(&state->live_values, 1, memory_order_relaxed);
}

static void qmljs_release_atom_for_scope(struct qmljs_atom_handle *handle)
{
  struct qmljs_runtime_state *state = handle->state;
  if (atomic_exchange_explicit(&handle->live, 0, memory_order_acq_rel) == 0) {
    return;
  }
  qmljs_unlink_atom(state, handle);
  if (atomic_load_explicit(&state->native_runtime_alive, memory_order_acquire)
      != 0 && state->rt != NULL) {
    JS_FreeAtomRT(state->rt, handle->atom);
  }
  handle->atom = JS_ATOM_NULL;
  atomic_fetch_sub_explicit(&state->live_atoms, 1, memory_order_relaxed);
}

static void qmljs_release_context_for_scope(struct qmljs_context_handle *handle)
{
  struct qmljs_runtime_state *state = handle->state;
  if (atomic_exchange_explicit(&handle->live, 0, memory_order_acq_rel) == 0) {
    return;
  }
  qmljs_unlink_context(state, handle);
  if (atomic_load_explicit(&state->native_runtime_alive, memory_order_acquire)
      != 0 && handle->ctx != NULL) {
    JS_FreeContext(handle->ctx);
  }
  handle->ctx = NULL;
  atomic_fetch_sub_explicit(&state->open_contexts, 1, memory_order_relaxed);
}

static void qmljs_release_context_values_for_scope(
  struct qmljs_runtime_state *state, struct qmljs_context_handle *context)
{
  struct qmljs_value_handle *current;
  do {
    current = NULL;
    qmljs_lock_lists(state);
    for (struct qmljs_value_handle *value = state->values; value != NULL;
         value = value->next) {
      if (value->context == context &&
          atomic_load_explicit(&value->live, memory_order_acquire) != 0) {
        current = value;
        break;
      }
    }
    qmljs_unlock_lists(state);
    if (current != NULL) {
      qmljs_release_value_for_scope(current);
    }
  } while (current != NULL);
}

static void qmljs_release_all_values_for_scope(struct qmljs_runtime_state *state)
{
  struct qmljs_value_handle *current;
  do {
    qmljs_lock_lists(state);
    current = state->values;
    qmljs_unlock_lists(state);
    if (current != NULL) {
      qmljs_release_value_for_scope(current);
    }
  } while (current != NULL);
}

static void qmljs_release_all_atoms_for_scope(struct qmljs_runtime_state *state)
{
  struct qmljs_atom_handle *current;
  do {
    qmljs_lock_lists(state);
    current = state->atoms;
    qmljs_unlock_lists(state);
    if (current != NULL) {
      qmljs_release_atom_for_scope(current);
    }
  } while (current != NULL);
}

static void qmljs_release_all_contexts_for_scope(struct qmljs_runtime_state *state)
{
  struct qmljs_context_handle *current;
  do {
    qmljs_lock_lists(state);
    current = state->contexts;
    qmljs_unlock_lists(state);
    if (current != NULL) {
      qmljs_release_context_for_scope(current);
    }
  } while (current != NULL);
}

void qmljs_drain_deferred(struct qmljs_runtime_state *state)
{
  if (!qmljs_can_release_now(state)) {
    /* Finalizers may run on any domain, and also while QuickJS is already on
       the C stack underneath an OCaml callback. Leave the queues for a
       non-reentrant owner-domain boundary. */
    return;
  }
  if (atomic_fetch_add_explicit(&state->deferred_drain_depth, 1,
                                memory_order_acq_rel) != 0) {
    atomic_fetch_sub_explicit(&state->deferred_drain_depth, 1,
                              memory_order_acq_rel);
    return;
  }

  /* Keep state alive across the drain even if the last node releases it. */
  qmljs_state_ref(state);

  int runtime_alive =
    atomic_load_explicit(&state->native_runtime_alive, memory_order_acquire);

  struct qmljs_deferred_value *values =
    atomic_exchange_explicit(&state->deferred_values, NULL,
                             memory_order_acquire);
  while (values != NULL) {
    struct qmljs_deferred_value *next = values->next;
    if (runtime_alive && state->rt != NULL) {
      JS_FreeValueRT(state->rt, values->value);
    }
    atomic_fetch_sub_explicit(&state->live_values, 1, memory_order_relaxed);
    qmljs_state_unref(values->state);
    caml_stat_free(values);
    values = next;
  }

  struct qmljs_deferred_atom *atoms =
    atomic_exchange_explicit(&state->deferred_atoms, NULL,
                             memory_order_acquire);
  while (atoms != NULL) {
    struct qmljs_deferred_atom *next = atoms->next;
    if (runtime_alive && state->rt != NULL && atoms->atom != JS_ATOM_NULL) {
      JS_FreeAtomRT(state->rt, atoms->atom);
    }
    atomic_fetch_sub_explicit(&state->live_atoms, 1, memory_order_relaxed);
    qmljs_state_unref(atoms->state);
    caml_stat_free(atoms);
    atoms = next;
  }

  struct qmljs_deferred_context *contexts =
    atomic_exchange_explicit(&state->deferred_contexts, NULL,
                             memory_order_acquire);
  while (contexts != NULL) {
    struct qmljs_deferred_context *next = contexts->next;
    if (runtime_alive && contexts->ctx != NULL) {
      JS_FreeContext(contexts->ctx);
    }
    atomic_fetch_sub_explicit(&state->open_contexts, 1, memory_order_relaxed);
    qmljs_state_unref(contexts->state);
    caml_stat_free(contexts);
    contexts = next;
  }

  atomic_fetch_sub_explicit(&state->deferred_drain_depth, 1,
                            memory_order_acq_rel);
  qmljs_state_unref(state);
}

void qmljs_drain_deferred_runtimes(void)
{
  if (atomic_fetch_add_explicit(&qmljs_deferred_runtime_drain_depth, 1,
                                memory_order_acq_rel) != 0) {
    atomic_fetch_sub_explicit(&qmljs_deferred_runtime_drain_depth, 1,
                              memory_order_acq_rel);
    return;
  }

  struct qmljs_deferred_runtime *runtimes =
    atomic_exchange_explicit(&qmljs_deferred_runtimes, NULL,
                             memory_order_acquire);
  while (runtimes != NULL) {
    struct qmljs_deferred_runtime *next = runtimes->next;
    if (qmljs_can_release_now(runtimes->state) &&
        qmljs_runtime_free_native(runtimes->state)) {
      qmljs_state_unref(runtimes->state);
      caml_stat_free(runtimes);
    } else {
      qmljs_requeue_deferred_runtime(runtimes);
    }
    runtimes = next;
  }

  atomic_fetch_sub_explicit(&qmljs_deferred_runtime_drain_depth, 1,
                            memory_order_acq_rel);
}

int qmljs_runtime_free_native(struct qmljs_runtime_state *state)
{
  if (atomic_load_explicit(&state->native_runtime_alive, memory_order_acquire)
      == 0) {
    return 1;
  }
  if (!qmljs_can_release_now(state)) {
    return 0;
  }
  qmljs_drain_deferred(state);
  qmljs_release_all_values_for_scope(state);
  qmljs_release_all_atoms_for_scope(state);
  qmljs_release_all_contexts_for_scope(state);
  if (atomic_load_explicit(&state->open_contexts, memory_order_acquire) != 0
      || atomic_load_explicit(&state->live_values, memory_order_acquire) != 0
      || atomic_load_explicit(&state->live_atoms, memory_order_acquire) != 0) {
    return 0;
  }
  JS_FreeRuntime(state->rt);
  state->rt = NULL;
  atomic_fetch_sub_explicit(&qmljs_live_native_runtimes, 1, memory_order_relaxed);
  if (state->runtime_info != NULL) {
    caml_stat_free(state->runtime_info);
    state->runtime_info = NULL;
  }
  atomic_store_explicit(&state->native_runtime_alive, 0, memory_order_release);
  qmljs_module_loader_free(state);
  qmljs_promise_events_free(state);
  return 1;
}

static void qmljs_runtime_finalize(value v)
{
  struct qmljs_runtime *runtime = Qml_runtime_val(v);
  struct qmljs_runtime_state *state = runtime->state;
  atomic_fetch_sub_explicit(&qmljs_live_runtime_wrappers, 1, memory_order_relaxed);
  if (state == NULL) {
    return;
  }
  atomic_store_explicit(&state->closed_to_user, 1, memory_order_release);
  qmljs_defer_runtime(state);
  runtime->state = NULL;
  qmljs_state_unref(state);
}

static void qmljs_context_finalize(value v)
{
  struct qmljs_context *context = Qml_context_val(v);
  atomic_fetch_sub_explicit(&qmljs_live_context_wrappers, 1, memory_order_relaxed);
  struct qmljs_context_handle *handle = context->handle;
  if (handle == NULL) {
    return;
  }
  struct qmljs_runtime_state *state = handle->state;
  if (atomic_exchange_explicit(&handle->live, 0, memory_order_acq_rel) != 0) {
    qmljs_unlink_context(state, handle);
    qmljs_defer_context(state, handle->ctx);
  }
  context->handle = NULL;
  qmljs_state_unref(state);
  caml_stat_free(handle);
}

static void qmljs_value_finalize(value v)
{
  struct qmljs_value *wrapped = Qml_value_val(v);
  atomic_fetch_sub_explicit(&qmljs_live_value_wrappers, 1, memory_order_relaxed);
  struct qmljs_value_handle *handle = wrapped->handle;
  if (handle == NULL) {
    return;
  }
  struct qmljs_runtime_state *state = handle->state;
  if (atomic_exchange_explicit(&handle->live, 0, memory_order_acq_rel) != 0) {
    qmljs_unlink_value(state, handle);
    qmljs_defer_value(state, handle->value);
  }
  wrapped->handle = NULL;
  qmljs_state_unref(state);
  caml_stat_free(handle);
}

static void qmljs_atom_finalize(value v)
{
  struct qmljs_atom *wrapped = Qml_atom_val(v);
  struct qmljs_atom_handle *handle = wrapped->handle;
  if (handle == NULL) {
    return;
  }
  struct qmljs_runtime_state *state = handle->state;
  if (atomic_exchange_explicit(&handle->live, 0, memory_order_acq_rel) != 0) {
    qmljs_unlink_atom(state, handle);
    qmljs_defer_atom(state, handle->atom);
  }
  wrapped->handle = NULL;
  qmljs_state_unref(state);
  caml_stat_free(handle);
}

static struct custom_operations qmljs_runtime_ops = {
  "qmljs.runtime",
  qmljs_runtime_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static struct custom_operations qmljs_context_ops = {
  "qmljs.context",
  qmljs_context_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static struct custom_operations qmljs_value_ops = {
  "qmljs.value",
  qmljs_value_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static struct custom_operations qmljs_atom_ops = {
  "qmljs.atom",
  qmljs_atom_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

value qmljs_alloc_runtime(JSRuntime *rt, struct qmljs_runtime_state *state)
{
  CAMLparam0();
  CAMLlocal1(block);
  /* state->refs was initialized to 1 for this wrapper at creation. */
  state->rt = rt;
  atomic_fetch_add_explicit(&qmljs_live_native_runtimes, 1, memory_order_relaxed);
  block = caml_alloc_custom(&qmljs_runtime_ops, sizeof(struct qmljs_runtime), 0, 1);
  Qml_runtime_val(block)->state = state;
  atomic_fetch_add_explicit(&qmljs_live_runtime_wrappers, 1, memory_order_relaxed);
  CAMLreturn(block);
}

value qmljs_alloc_context(value runtime, JSContext *ctx)
{
  CAMLparam1(runtime);
  CAMLlocal1(block);
  struct qmljs_runtime_state *state = Qml_runtime_val(runtime)->state;
  struct qmljs_context_handle *handle =
    caml_stat_alloc(sizeof(struct qmljs_context_handle));
  handle->state = state;
  handle->ctx = ctx;
  atomic_init(&handle->live, 1);
  handle->linked = 0;
  handle->prev = NULL;
  handle->next = NULL;
  qmljs_state_ref(state);
  qmljs_link_context(state, handle);
  block = caml_alloc_custom(&qmljs_context_ops, sizeof(struct qmljs_context), 0, 1);
  Qml_context_val(block)->handle = handle;
  atomic_fetch_add_explicit(&state->open_contexts, 1, memory_order_relaxed);
  atomic_fetch_add_explicit(&qmljs_live_context_wrappers, 1, memory_order_relaxed);
  CAMLreturn(block);
}

value qmljs_alloc_value(value context, JSValue js_value)
{
  CAMLparam1(context);
  CAMLlocal1(block);
  struct qmljs_context_handle *context_handle = Qml_context_val(context)->handle;
  struct qmljs_runtime_state *state = context_handle->state;
  struct qmljs_value_handle *handle =
    caml_stat_alloc(sizeof(struct qmljs_value_handle));
  handle->state = state;
  handle->context = context_handle;
  handle->value = js_value;
  atomic_init(&handle->live, 1);
  handle->linked = 0;
  handle->prev = NULL;
  handle->next = NULL;
  qmljs_state_ref(state);
  qmljs_link_value(state, handle);
  block = caml_alloc_custom(&qmljs_value_ops, sizeof(struct qmljs_value), 0, 1);
  Qml_value_val(block)->handle = handle;
  atomic_fetch_add_explicit(&state->live_values, 1, memory_order_relaxed);
  atomic_fetch_add_explicit(&qmljs_live_value_wrappers, 1, memory_order_relaxed);
  CAMLreturn(block);
}

value qmljs_alloc_atom(struct qmljs_runtime_state *state, JSAtom atom)
{
  CAMLparam0();
  CAMLlocal1(block);
  struct qmljs_atom_handle *handle =
    caml_stat_alloc(sizeof(struct qmljs_atom_handle));
  handle->state = state;
  handle->atom = atom;
  atomic_init(&handle->live, 1);
  handle->linked = 0;
  handle->prev = NULL;
  handle->next = NULL;
  qmljs_state_ref(state);
  qmljs_link_atom(state, handle);
  block = caml_alloc_custom(&qmljs_atom_ops, sizeof(struct qmljs_atom), 0, 1);
  Qml_atom_val(block)->handle = handle;
  atomic_fetch_add_explicit(&state->live_atoms, 1, memory_order_relaxed);
  CAMLreturn(block);
}

int qmljs_context_close_native(value context)
{
  struct qmljs_context_handle *handle = Qml_context_val(context)->handle;
  if (handle == NULL) {
    return 1;
  }
  struct qmljs_runtime_state *state = handle->state;
  qmljs_ensure_owner(state);
  if (!qmljs_can_release_now(state)) {
    return 0;
  }
  qmljs_drain_deferred(state);
  qmljs_release_context_values_for_scope(state, handle);
  qmljs_release_context_for_scope(handle);
  return 1;
}

CAMLprim value qmljs_diagnostics_live_runtimes(value unit)
{
  (void)unit;
  return Val_int(
    atomic_load_explicit(&qmljs_live_runtime_wrappers, memory_order_relaxed));
}

CAMLprim value qmljs_diagnostics_live_contexts(value unit)
{
  (void)unit;
  return Val_int(
    atomic_load_explicit(&qmljs_live_context_wrappers, memory_order_relaxed));
}

CAMLprim value qmljs_diagnostics_live_values(value unit)
{
  (void)unit;
  return Val_int(
    atomic_load_explicit(&qmljs_live_value_wrappers, memory_order_relaxed));
}

CAMLprim value qmljs_diagnostics_live_native_runtimes(value unit)
{
  (void)unit;
  return Val_int(
    atomic_load_explicit(&qmljs_live_native_runtimes, memory_order_relaxed));
}

CAMLprim value qmljs_diagnostics_live_argv_arrays(value unit)
{
  (void)unit;
  return Val_int(
    atomic_load_explicit(&qmljs_live_argv_arrays, memory_order_relaxed));
}

void qmljs_raise_string(const char *message)
{
  CAMLparam0();
  CAMLlocal1(arg);
  static const value *exception = NULL;
  if (exception == NULL) {
    exception = caml_named_value("qmljs.error");
  }
  arg = caml_copy_string(message);
  caml_raise_with_arg(*exception, arg);
  CAMLreturn0;
}

static void qmljs_clear_pending_exception(JSContext *ctx)
{
  JSValue exception = JS_GetException(ctx);
  JS_FreeValue(ctx, exception);
}

static const char *qmljs_cstring_or_clear_exception(JSContext *ctx,
                                                  JSValueConst value)
{
  const char *string = JS_ToCString(ctx, value);
  if (string == NULL) {
    qmljs_clear_pending_exception(ctx);
  }
  return string;
}

static const char *qmljs_optional_cstring(JSContext *ctx, JSValueConst value)
{
  if (JS_IsException(value)) {
    qmljs_clear_pending_exception(ctx);
    return NULL;
  }
  return qmljs_cstring_or_clear_exception(ctx, value);
}

static const char *qmljs_property_cstring(JSContext *ctx, JSValueConst object,
                                        const char *name, JSValue *slot)
{
  *slot = JS_GetPropertyStr(ctx, object, name);
  if (JS_IsException(*slot) || JS_IsUndefined(*slot) || JS_IsNull(*slot)) {
    if (JS_IsException(*slot)) {
      qmljs_clear_pending_exception(ctx);
      *slot = JS_UNDEFINED;
    }
    return NULL;
  }
  return qmljs_cstring_or_clear_exception(ctx, *slot);
}

static value qmljs_copy_optional_string(const char *text)
{
  return caml_copy_string(text == NULL ? "" : text);
}

static int qmljs_error_kind_of_marker(const char *marker)
{
  if (marker == NULL) {
    return 0;
  }
  if (strcmp(marker, "out_of_memory") == 0) {
    return 1;
  }
  if (strcmp(marker, "stack_overflow") == 0) {
    return 2;
  }
  if (strcmp(marker, "resource_limit") == 0 ||
      strcmp(marker, "interrupt") == 0) {
    return 3;
  }
  if (strcmp(marker, "policy_violation") == 0) {
    return 4;
  }
  if (strcmp(marker, "host_callback_defect") == 0) {
    return 5;
  }
  if (strcmp(marker, "timeout") == 0) {
    return 6;
  }
  if (strcmp(marker, "closed_handle") == 0) {
    return 7;
  }
  if (strcmp(marker, "wrong_domain") == 0) {
    return 8;
  }
  return 0;
}

static int qmljs_error_kind_of_engine_error(const char *name,
                                          const char *message)
{
  if (name == NULL || message == NULL) {
    return 0;
  }
  if (strcmp(name, "InternalError") == 0 &&
      strcmp(message, "out of memory") == 0) {
    return 1;
  }
  if ((strcmp(name, "InternalError") == 0 ||
       strcmp(name, "RangeError") == 0) &&
      (strstr(message, "stack overflow") != NULL ||
       strstr(message, "Maximum call stack size exceeded") != NULL)) {
    return 2;
  }
  if (strcmp(name, "InternalError") == 0 &&
      strstr(message, "interrupted") != NULL) {
    return 6;
  }
  return 0;
}

static value qmljs_some(value input)
{
  CAMLparam1(input);
  CAMLlocal1(result);
  result = caml_alloc(1, 0);
  Store_field(result, 0, input);
  CAMLreturn(result);
}

static value qmljs_metadata_array(JSContext *ctx, JSValueConst js_metadata)
{
  CAMLparam0();
  CAMLlocal4(result, pair, key, value_);
  uint32_t length = 0;

  if (!JS_IsObject(js_metadata)) {
    CAMLreturn(caml_alloc(0, 0));
  }

  JSValue js_length = JS_GetPropertyStr(ctx, js_metadata, "length");
  if (JS_IsException(js_length) ||
      JS_ToUint32(ctx, &length, js_length) < 0) {
    qmljs_clear_pending_exception(ctx);
    JS_FreeValue(ctx, js_length);
    CAMLreturn(caml_alloc(0, 0));
  }
  JS_FreeValue(ctx, js_length);

  result = caml_alloc(length, 0);
  for (uint32_t i = 0; i < length; i++) {
    Store_field(result, i, Val_unit);
  }

  for (uint32_t i = 0; i < length; i++) {
    JSValue js_pair = JS_GetPropertyUint32(ctx, js_metadata, i);
    if (JS_IsException(js_pair) || !JS_IsObject(js_pair)) {
      if (JS_IsException(js_pair)) {
        qmljs_clear_pending_exception(ctx);
      }
      JS_FreeValue(ctx, js_pair);
      pair = caml_alloc_tuple(2);
      Store_field(pair, 0, Val_unit);
      Store_field(pair, 1, Val_unit);
      key = caml_copy_string("");
      value_ = caml_copy_string("");
      Store_field(pair, 0, key);
      Store_field(pair, 1, value_);
      Store_field(result, i, pair);
      continue;
    }

    JSValue js_key = JS_GetPropertyUint32(ctx, js_pair, 0);
    JSValue js_value = JS_GetPropertyUint32(ctx, js_pair, 1);
    const char *key_string = qmljs_optional_cstring(ctx, js_key);
    const char *value_string = qmljs_optional_cstring(ctx, js_value);

    key = caml_copy_string(key_string == NULL ? "" : key_string);
    value_ = caml_copy_string(value_string == NULL ? "" : value_string);
    pair = caml_alloc_tuple(2);
    Store_field(pair, 0, key);
    Store_field(pair, 1, value_);
    Store_field(result, i, pair);

    if (value_string != NULL) {
      JS_FreeCString(ctx, value_string);
    }
    if (key_string != NULL) {
      JS_FreeCString(ctx, key_string);
    }
    JS_FreeValue(ctx, js_value);
    JS_FreeValue(ctx, js_key);
    JS_FreeValue(ctx, js_pair);
  }

  CAMLreturn(result);
}

static value qmljs_build_structured_exception(JSContext *ctx,
                                            JSValueConst js_exception,
                                            int depth,
                                            const value *make_structured)
{
  CAMLparam0();
  CAMLlocalN(args, 8);
  CAMLlocal4(cause, nested, metadata, result);
  JSValue js_name = JS_UNDEFINED;
  JSValue js_message = JS_UNDEFINED;
  JSValue js_stack = JS_UNDEFINED;
  JSValue js_ocaml_frame = JS_UNDEFINED;
  JSValue js_kind = JS_UNDEFINED;
  JSValue js_denied_specifier = JS_UNDEFINED;
  JSValue js_metadata = JS_UNDEFINED;
  JSValue js_cause = JS_UNDEFINED;
  const char *name = NULL;
  const char *message = NULL;
  const char *exception_text = NULL;
  const char *fallback_message = NULL;
  const char *stack = NULL;
  const char *ocaml_frame = NULL;
  const char *kind_marker = NULL;
  const char *denied_specifier = NULL;
  int kind = 0;

  if (JS_IsNull(js_exception)) {
    kind = 1;
    fallback_message = "out of memory";
  }

  if (JS_IsObject(js_exception)) {
    name = qmljs_property_cstring(ctx, js_exception, "name", &js_name);
    message = qmljs_property_cstring(ctx, js_exception, "message", &js_message);
    stack = qmljs_property_cstring(ctx, js_exception, "stack", &js_stack);
    ocaml_frame = qmljs_property_cstring(ctx, js_exception, "_qmljs_ocaml_frame",
                                       &js_ocaml_frame);
    kind_marker =
      qmljs_property_cstring(ctx, js_exception, "_qmljs_kind", &js_kind);
    denied_specifier =
      qmljs_property_cstring(ctx, js_exception, "_qmljs_denied_module_specifier",
                           &js_denied_specifier);
    js_metadata = JS_GetPropertyStr(ctx, js_exception, "_qmljs_metadata");
    if (JS_IsException(js_metadata)) {
      qmljs_clear_pending_exception(ctx);
      js_metadata = JS_UNDEFINED;
    }
    metadata = qmljs_metadata_array(ctx, js_metadata);

    js_cause = JS_GetPropertyStr(ctx, js_exception, "cause");
    if (JS_IsException(js_cause) || JS_IsUndefined(js_cause) ||
        JS_IsNull(js_cause) || depth >= 64) {
      if (JS_IsException(js_cause)) {
        qmljs_clear_pending_exception(ctx);
        js_cause = JS_UNDEFINED;
      }
      cause = Val_int(0);
    } else {
      nested = qmljs_build_structured_exception(ctx, js_cause, depth + 1,
                                              make_structured);
      cause = qmljs_some(nested);
    }
  } else {
    cause = Val_int(0);
    metadata = caml_alloc(0, 0);
  }

  exception_text = qmljs_cstring_or_clear_exception(ctx, js_exception);
  if ((message == NULL || message[0] == '\0') && exception_text != NULL) {
    fallback_message = exception_text;
  }

  kind = qmljs_error_kind_of_marker(kind_marker);
  if (kind == 0 && JS_IsNull(js_exception)) {
    kind = 1;
  }
  if (kind == 0) {
    kind = qmljs_error_kind_of_engine_error(name, message);
  }

  args[0] = qmljs_copy_optional_string(name);
  args[1] =
    qmljs_copy_optional_string(message != NULL && message[0] != '\0'
                               ? message
                               : fallback_message);
  args[2] = qmljs_copy_optional_string(stack);
  args[3] = qmljs_copy_optional_string(ocaml_frame);
  args[4] = Val_int(kind);
  args[5] = qmljs_copy_optional_string(denied_specifier);
  args[6] = cause;
  args[7] = metadata;

  if (denied_specifier != NULL) {
    JS_FreeCString(ctx, denied_specifier);
  }
  if (kind_marker != NULL) {
    JS_FreeCString(ctx, kind_marker);
  }
  if (ocaml_frame != NULL) {
    JS_FreeCString(ctx, ocaml_frame);
  }
  if (stack != NULL) {
    JS_FreeCString(ctx, stack);
  }
  if (message != NULL) {
    JS_FreeCString(ctx, message);
  }
  if (name != NULL) {
    JS_FreeCString(ctx, name);
  }
  if (exception_text != NULL) {
    JS_FreeCString(ctx, exception_text);
  }
  JS_FreeValue(ctx, js_cause);
  JS_FreeValue(ctx, js_metadata);
  JS_FreeValue(ctx, js_denied_specifier);
  JS_FreeValue(ctx, js_kind);
  JS_FreeValue(ctx, js_ocaml_frame);
  JS_FreeValue(ctx, js_stack);
  JS_FreeValue(ctx, js_message);
  JS_FreeValue(ctx, js_name);

  result = caml_callbackN_exn(*make_structured, 8, args);
  if (Is_exception_result(result)) {
    caml_raise(Extract_exception(result));
  }
  CAMLreturn(result);
}

static void qmljs_raise_structured_value(JSContext *ctx, JSValueConst js_exception)
{
  CAMLparam0();
  CAMLlocal1(structured);
  static const value *make_structured = NULL;
  static const value *raise_structured = NULL;

  if (make_structured == NULL) {
    make_structured = caml_named_value("qmljs.make_structured");
  }
  if (raise_structured == NULL) {
    raise_structured = caml_named_value("qmljs.raise_structured");
  }
  if (make_structured == NULL || raise_structured == NULL) {
    qmljs_raise_string("JavaScript exception");
  }

  structured = qmljs_build_structured_exception(ctx, js_exception, 0,
                                              make_structured);

  value result = caml_callback_exn(*raise_structured, structured);
  if (Is_exception_result(result)) {
    caml_raise(Extract_exception(result));
  }

  qmljs_raise_string("JavaScript exception callback returned unexpectedly");
  CAMLreturn0;
}

value qmljs_structured_of_exception_value(JSContext *ctx, JSValueConst exception)
{
  CAMLparam0();
  CAMLlocal1(structured);
  static const value *make_structured = NULL;

  if (make_structured == NULL) {
    make_structured = caml_named_value("qmljs.make_structured");
  }
  if (make_structured == NULL) {
    qmljs_raise_string("JavaScript exception");
  }

  structured = qmljs_build_structured_exception(ctx, exception, 0, make_structured);
  CAMLreturn(structured);
}

void qmljs_raise_exception(JSContext *ctx)
{
  CAMLparam0();
  CAMLlocal1(structured);
  static const value *make_structured = NULL;
  static const value *raise_structured = NULL;
  JSValue js_exception = JS_GetException(ctx);

  if (make_structured == NULL) {
    make_structured = caml_named_value("qmljs.make_structured");
  }
  if (raise_structured == NULL) {
    raise_structured = caml_named_value("qmljs.raise_structured");
  }
  if (make_structured == NULL || raise_structured == NULL) {
    JS_FreeValue(ctx, js_exception);
    qmljs_raise_string("JavaScript exception");
  }

  structured = qmljs_build_structured_exception(ctx, js_exception, 0,
                                              make_structured);
  JS_FreeValue(ctx, js_exception);

  value result = caml_callback_exn(*raise_structured, structured);
  if (Is_exception_result(result)) {
    caml_raise(Extract_exception(result));
  }

  qmljs_raise_string("JavaScript exception callback returned unexpectedly");
  CAMLreturn0;
}

void qmljs_raise_value_exception(JSContext *ctx, JSValueConst exception)
{
  qmljs_raise_structured_value(ctx, exception);
}

JSRuntime *qmljs_runtime(value runtime)
{
  struct qmljs_runtime_state *state = Qml_runtime_val(runtime)->state;
  qmljs_ensure_owner(state);
  qmljs_drain_deferred_runtimes();
  qmljs_drain_deferred(state);
  return state->rt;
}

JSContext *qmljs_context(value context)
{
  struct qmljs_context *wrapped = Qml_context_val(context);
  struct qmljs_context_handle *handle = wrapped->handle;
  if (handle == NULL) {
    qmljs_raise_string("QuickJS context is closed");
  }
  qmljs_ensure_owner(handle->state);
  if (atomic_load_explicit(&handle->live, memory_order_acquire) == 0 ||
      handle->ctx == NULL) {
    qmljs_raise_string("QuickJS context is closed");
  }
  qmljs_drain_deferred_runtimes();
  qmljs_drain_deferred(handle->state);
  return handle->ctx;
}

struct qmljs_runtime_state *qmljs_context_state(value context)
{
  struct qmljs_context_handle *handle = Qml_context_val(context)->handle;
  if (handle == NULL) {
    qmljs_raise_string("QuickJS context is closed");
  }
  return handle->state;
}

JSValue qmljs_js_value(value wrapped)
{
  struct qmljs_value *wrapped_value = Qml_value_val(wrapped);
  struct qmljs_value_handle *handle = wrapped_value->handle;
  if (handle == NULL) {
    qmljs_raise_string("QuickJS value is closed");
  }
  qmljs_ensure_owner(handle->state);
  if (atomic_load_explicit(&handle->live, memory_order_acquire) == 0) {
    qmljs_raise_string("QuickJS value is closed");
  }
  return handle->value;
}

const char *qmljs_js_value_for_context(value context, value wrapped, JSValue *out)
{
  struct qmljs_context_handle *expected = Qml_context_val(context)->handle;
  struct qmljs_value_handle *actual = Qml_value_val(wrapped)->handle;
  if (expected == NULL ||
      atomic_load_explicit(&expected->live, memory_order_acquire) == 0 ||
      expected->ctx == NULL) {
    return "QuickJS context is closed";
  }
  if (actual == NULL ||
      atomic_load_explicit(&actual->live, memory_order_acquire) == 0) {
    return "QuickJS value is closed";
  }
  if (Caml_state != actual->state->owner_domain) {
    return "QuickJS runtime used from a non-owner domain";
  }
  if (atomic_load_explicit(&actual->state->closed_to_user,
                           memory_order_acquire) != 0 ||
      atomic_load_explicit(&actual->state->native_runtime_alive,
                           memory_order_acquire) == 0) {
    return "QuickJS runtime is closed";
  }
  if (actual->context != expected) {
    return "QuickJS value belongs to a different context";
  }
  *out = actual->value;
  return NULL;
}

JSAtom qmljs_js_atom(value wrapped)
{
  struct qmljs_atom *wrapped_atom = Qml_atom_val(wrapped);
  struct qmljs_atom_handle *handle = wrapped_atom->handle;
  if (handle == NULL) {
    qmljs_raise_string("QuickJS atom is closed");
  }
  qmljs_ensure_owner(handle->state);
  if (atomic_load_explicit(&handle->live, memory_order_acquire) == 0 ||
      handle->atom == JS_ATOM_NULL) {
    qmljs_raise_string("QuickJS atom is closed");
  }
  return handle->atom;
}

CAMLprim value qmljs_atom_equal(value lhs, value rhs)
{
  CAMLparam2(lhs, rhs);
  struct qmljs_atom_handle *left = Qml_atom_val(lhs)->handle;
  struct qmljs_atom_handle *right = Qml_atom_val(rhs)->handle;
  if (left == NULL ||
      atomic_load_explicit(&left->live, memory_order_acquire) == 0 ||
      left->atom == JS_ATOM_NULL) {
    qmljs_raise_string("QuickJS atom is closed");
  }
  if (right == NULL ||
      atomic_load_explicit(&right->live, memory_order_acquire) == 0 ||
      right->atom == JS_ATOM_NULL) {
    qmljs_raise_string("QuickJS atom is closed");
  }
  qmljs_ensure_owner(left->state);
  qmljs_ensure_owner(right->state);
  CAMLreturn(Val_bool(left->state == right->state && left->atom == right->atom));
}

void qmljs_ensure_same_runtime(struct qmljs_runtime_state *expected, value wrapped)
{
  struct qmljs_value_handle *actual = Qml_value_val(wrapped)->handle;
  if (actual == NULL ||
      atomic_load_explicit(&actual->live, memory_order_acquire) == 0) {
    qmljs_raise_string("QuickJS value is closed");
  }
  if (actual->state != expected) {
    qmljs_raise_string("QuickJS value belongs to a different runtime");
  }
}

void qmljs_ensure_atom_runtime(struct qmljs_runtime_state *expected, value wrapped)
{
  struct qmljs_atom_handle *actual = Qml_atom_val(wrapped)->handle;
  if (actual == NULL ||
      atomic_load_explicit(&actual->live, memory_order_acquire) == 0) {
    qmljs_raise_string("QuickJS atom is closed");
  }
  if (actual->state != expected) {
    qmljs_raise_string("QuickJS atom belongs to a different runtime");
  }
}

void qmljs_ensure_same_context(value context, value wrapped)
{
  struct qmljs_context_handle *expected = Qml_context_val(context)->handle;
  struct qmljs_value_handle *actual = Qml_value_val(wrapped)->handle;
  if (expected == NULL) {
    qmljs_raise_string("QuickJS context is closed");
  }
  if (actual == NULL ||
      atomic_load_explicit(&actual->live, memory_order_acquire) == 0) {
    qmljs_raise_string("QuickJS value is closed");
  }
  if (actual->context != expected) {
    qmljs_raise_string("QuickJS value belongs to a different context");
  }
}

void qmljs_update_stack_top(JSContext *ctx)
{
  JS_UpdateStackTop(JS_GetRuntime(ctx));
}
