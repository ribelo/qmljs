#include "qmljs_stubs.h"

struct qmljs_host_class {
  atomic_uint refs;
  struct qmljs_runtime_state *state;
  JSClassID class_id;
  int has_finalizer;
  value finalizer;
};

struct qmljs_class {
  struct qmljs_host_class *klass;
};

struct qmljs_class_instance {
  struct qmljs_host_class *klass;
  value payload;
  struct qmljs_class_instance *next_finalizer;
};

#define Qml_class_val(v) ((struct qmljs_class *)Data_custom_val(v))

static void qmljs_host_class_ref(struct qmljs_host_class *klass)
{
  atomic_fetch_add_explicit(&klass->refs, 1, memory_order_relaxed);
}

static void qmljs_host_class_unref(struct qmljs_host_class *klass)
{
  if (atomic_fetch_sub_explicit(&klass->refs, 1, memory_order_acq_rel) == 1) {
    if (klass->has_finalizer) {
      caml_remove_generational_global_root(&klass->finalizer);
    }
    qmljs_state_unref(klass->state);
    caml_stat_free(klass);
  }
}

static void qmljs_class_wrapper_finalize(value wrapped_value)
{
  struct qmljs_class *wrapped = Qml_class_val(wrapped_value);
  if (wrapped->klass != NULL) {
    qmljs_host_class_unref(wrapped->klass);
    wrapped->klass = NULL;
  }
}

static struct custom_operations qmljs_class_ops = {
  "qmljs.class",
  qmljs_class_wrapper_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static value qmljs_alloc_class(struct qmljs_host_class *klass)
{
  CAMLparam0();
  CAMLlocal1(block);
  block = caml_alloc_custom(&qmljs_class_ops, sizeof(struct qmljs_class), 0, 1);
  Qml_class_val(block)->klass = klass;
  CAMLreturn(block);
}

static void qmljs_class_instance_release(struct qmljs_class_instance *instance,
                                       int run_finalizer)
{
  CAMLparam0();
  if (run_finalizer && instance->klass->has_finalizer) {
    qmljs_enter_ocaml_callback(instance->klass->state);
    value result = caml_callback_exn(instance->klass->finalizer,
                                     instance->payload);
    qmljs_leave_ocaml_callback(instance->klass->state);
    (void)result;
  }
  caml_remove_generational_global_root(&instance->payload);
  qmljs_host_class_unref(instance->klass);
  caml_stat_free(instance);
  CAMLreturn0;
}

static void qmljs_defer_class_finalizer(struct qmljs_class_instance *instance)
{
  struct qmljs_runtime_state *state = instance->klass->state;
  struct qmljs_class_instance *head = atomic_load_explicit(
    &state->deferred_class_finalizers, memory_order_acquire);
  do {
    instance->next_finalizer = head;
  } while (!atomic_compare_exchange_weak_explicit(
    &state->deferred_class_finalizers, &head, instance, memory_order_release,
    memory_order_acquire));
}

int qmljs_drain_class_finalizers(struct qmljs_runtime_state *state)
{
  int drained = 0;
  struct qmljs_class_instance *instances = atomic_exchange_explicit(
    &state->deferred_class_finalizers, NULL, memory_order_acquire);
  while (instances != NULL) {
    struct qmljs_class_instance *next = instances->next_finalizer;
    instances->next_finalizer = NULL;
    qmljs_class_instance_release(instances, 1);
    drained = 1;
    instances = next;
  }
  return drained;
}

static void qmljs_class_instance_finalizer(JSRuntime *rt, JSValueConst js_value)
{
  (void)rt;
  struct qmljs_class_instance *instance =
    JS_GetOpaque(js_value, JS_GetClassID(js_value));
  if (instance != NULL) {
    if (instance->klass->has_finalizer) {
      /* QuickJS finalizers are not a safe place to run arbitrary OCaml. */
      qmljs_defer_class_finalizer(instance);
    } else {
      qmljs_class_instance_release(instance, 0);
    }
  }
}

static const JSClassDef qmljs_class_def = {
  .class_name = "QmljsClass",
  .finalizer = qmljs_class_instance_finalizer,
};

static struct qmljs_host_class *qmljs_class(value class_)
{
  struct qmljs_host_class *klass = Qml_class_val(class_)->klass;
  if (klass == NULL) {
    qmljs_raise_string("qmljs class is closed");
  }
  qmljs_ensure_owner(klass->state);
  return klass;
}

static void qmljs_ensure_class_context(value context,
                                     struct qmljs_host_class *klass)
{
  if (qmljs_context_state(context) != klass->state) {
    qmljs_raise_string("qmljs class belongs to a different runtime");
  }
}

CAMLprim value qmljs_class_create(value context, value name, value finalizer)
{
  CAMLparam3(context, name, finalizer);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);

  struct qmljs_host_class *klass =
    caml_stat_alloc(sizeof(struct qmljs_host_class));
  atomic_init(&klass->refs, 1);
  klass->state = qmljs_context_state(context);
  klass->class_id = 0;
  klass->has_finalizer = 0;
  klass->finalizer = Val_unit;
  qmljs_state_ref(klass->state);
  if (Is_block(finalizer)) {
    klass->has_finalizer = 1;
    klass->finalizer = Field(finalizer, 0);
    caml_register_generational_global_root(&klass->finalizer);
  }

  JSRuntime *rt = JS_GetRuntime(ctx);
  JS_NewClassID(rt, &klass->class_id);
  if (JS_NewClass(rt, klass->class_id, &qmljs_class_def) < 0) {
    qmljs_host_class_unref(klass);
    qmljs_raise_string("could not register qmljs class");
  }

  JSValue prototype = JS_NewObject(ctx);
  if (JS_IsException(prototype)) {
    qmljs_host_class_unref(klass);
    qmljs_raise_exception(ctx);
  }
  JS_SetClassProto(ctx, klass->class_id, prototype);
  CAMLreturn(qmljs_alloc_class(klass));
}

CAMLprim value qmljs_class_prototype(value context, value class_)
{
  CAMLparam2(context, class_);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  struct qmljs_host_class *klass = qmljs_class(class_);
  qmljs_ensure_class_context(context, klass);

  JSValue prototype = JS_GetClassProto(ctx, klass->class_id);
  if (JS_IsException(prototype)) {
    qmljs_raise_exception(ctx);
  }
  CAMLreturn(qmljs_alloc_value(context, prototype));
}

CAMLprim value qmljs_class_new_instance(value context, value class_,
                                      value payload)
{
  CAMLparam3(context, class_, payload);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  struct qmljs_host_class *klass = qmljs_class(class_);
  qmljs_ensure_class_context(context, klass);

  struct qmljs_class_instance *instance =
    caml_stat_alloc(sizeof(struct qmljs_class_instance));
  instance->klass = klass;
  instance->payload = payload;
  instance->next_finalizer = NULL;
  qmljs_host_class_ref(klass);
  caml_register_generational_global_root(&instance->payload);

  JSValue object = JS_NewObjectClass(ctx, klass->class_id);
  if (JS_IsException(object)) {
    qmljs_class_instance_release(instance, 0);
    qmljs_raise_exception(ctx);
  }
  if (JS_SetOpaque(object, instance) < 0) {
    JS_FreeValue(ctx, object);
    qmljs_class_instance_release(instance, 0);
    qmljs_raise_string("could not attach qmljs class payload");
  }
  CAMLreturn(qmljs_alloc_value(context, object));
}

CAMLprim value qmljs_class_get(value context, value class_, value object)
{
  CAMLparam3(context, class_, object);
  CAMLlocal1(result);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  struct qmljs_host_class *klass = qmljs_class(class_);
  qmljs_ensure_class_context(context, klass);
  qmljs_ensure_same_context(context, object);

  struct qmljs_class_instance *instance =
    JS_GetOpaque(qmljs_js_value(object), klass->class_id);
  if (instance == NULL) {
    CAMLreturn(Val_int(0));
  }

  result = caml_alloc(1, 0);
  Store_field(result, 0, instance->payload);
  CAMLreturn(result);
}

CAMLprim value qmljs_class_is_instance(value context, value class_, value object)
{
  CAMLparam3(context, class_, object);
  JSContext *ctx = qmljs_context(context);
  qmljs_update_stack_top(ctx);
  struct qmljs_host_class *klass = qmljs_class(class_);
  qmljs_ensure_class_context(context, klass);
  qmljs_ensure_same_context(context, object);

  CAMLreturn(Val_bool(
    JS_GetOpaque(qmljs_js_value(object), klass->class_id) != NULL));
}
