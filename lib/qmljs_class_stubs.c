#include "qmljs_stubs.h"

struct qml_host_class {
  atomic_uint refs;
  struct qml_runtime_state *state;
  JSClassID class_id;
  int has_finalizer;
  value finalizer;
};

struct qml_class {
  struct qml_host_class *klass;
};

struct qml_class_instance {
  struct qml_host_class *klass;
  value payload;
};

#define Qml_class_val(v) ((struct qml_class *)Data_custom_val(v))

static void qml_host_class_ref(struct qml_host_class *klass)
{
  atomic_fetch_add_explicit(&klass->refs, 1, memory_order_relaxed);
}

static void qml_host_class_unref(struct qml_host_class *klass)
{
  if (atomic_fetch_sub_explicit(&klass->refs, 1, memory_order_acq_rel) == 1) {
    if (klass->has_finalizer) {
      caml_remove_generational_global_root(&klass->finalizer);
    }
    qml_state_unref(klass->state);
    caml_stat_free(klass);
  }
}

static void qml_class_wrapper_finalize(value wrapped_value)
{
  struct qml_class *wrapped = Qml_class_val(wrapped_value);
  if (wrapped->klass != NULL) {
    qml_host_class_unref(wrapped->klass);
    wrapped->klass = NULL;
  }
}

static struct custom_operations qml_class_ops = {
  "qmljs.class",
  qml_class_wrapper_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static value qml_alloc_class(struct qml_host_class *klass)
{
  CAMLparam0();
  CAMLlocal1(block);
  block = caml_alloc_custom(&qml_class_ops, sizeof(struct qml_class), 0, 1);
  Qml_class_val(block)->klass = klass;
  CAMLreturn(block);
}

static void qml_class_instance_release(struct qml_class_instance *instance,
                                       int run_finalizer)
{
  CAMLparam0();
  if (run_finalizer && instance->klass->has_finalizer) {
    qml_enter_ocaml_callback(instance->klass->state);
    value result = caml_callback_exn(instance->klass->finalizer,
                                     instance->payload);
    qml_leave_ocaml_callback(instance->klass->state);
    (void)result;
  }
  caml_remove_generational_global_root(&instance->payload);
  qml_host_class_unref(instance->klass);
  caml_stat_free(instance);
  CAMLreturn0;
}

static void qml_class_instance_finalizer(JSRuntime *rt, JSValueConst js_value)
{
  (void)rt;
  struct qml_class_instance *instance =
    JS_GetOpaque(js_value, JS_GetClassID(js_value));
  if (instance != NULL) {
    qml_class_instance_release(instance, 1);
  }
}

static const JSClassDef qml_class_def = {
  .class_name = "QmljsClass",
  .finalizer = qml_class_instance_finalizer,
};

static struct qml_host_class *qml_class(value class_)
{
  struct qml_host_class *klass = Qml_class_val(class_)->klass;
  if (klass == NULL) {
    qml_raise_string("qmljs class is closed");
  }
  qml_ensure_owner(klass->state);
  return klass;
}

static void qml_ensure_class_context(value context,
                                     struct qml_host_class *klass)
{
  if (qml_context_state(context) != klass->state) {
    qml_raise_string("qmljs class belongs to a different runtime");
  }
}

CAMLprim value qml_class_create(value context, value name, value finalizer)
{
  CAMLparam3(context, name, finalizer);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);

  struct qml_host_class *klass =
    caml_stat_alloc(sizeof(struct qml_host_class));
  atomic_init(&klass->refs, 1);
  klass->state = qml_context_state(context);
  klass->class_id = 0;
  klass->has_finalizer = 0;
  klass->finalizer = Val_unit;
  qml_state_ref(klass->state);
  if (Is_block(finalizer)) {
    klass->has_finalizer = 1;
    klass->finalizer = Field(finalizer, 0);
    caml_register_generational_global_root(&klass->finalizer);
  }

  JSRuntime *rt = JS_GetRuntime(ctx);
  JS_NewClassID(rt, &klass->class_id);
  if (JS_NewClass(rt, klass->class_id, &qml_class_def) < 0) {
    qml_host_class_unref(klass);
    qml_raise_string("could not register qmljs class");
  }

  JSValue prototype = JS_NewObject(ctx);
  if (JS_IsException(prototype)) {
    qml_host_class_unref(klass);
    qml_raise_exception(ctx);
  }
  JS_SetClassProto(ctx, klass->class_id, prototype);
  CAMLreturn(qml_alloc_class(klass));
}

CAMLprim value qml_class_prototype(value context, value class_)
{
  CAMLparam2(context, class_);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  struct qml_host_class *klass = qml_class(class_);
  qml_ensure_class_context(context, klass);

  JSValue prototype = JS_GetClassProto(ctx, klass->class_id);
  if (JS_IsException(prototype)) {
    qml_raise_exception(ctx);
  }
  CAMLreturn(qml_alloc_value(context, prototype));
}

CAMLprim value qml_class_new_instance(value context, value class_,
                                      value payload)
{
  CAMLparam3(context, class_, payload);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  struct qml_host_class *klass = qml_class(class_);
  qml_ensure_class_context(context, klass);

  struct qml_class_instance *instance =
    caml_stat_alloc(sizeof(struct qml_class_instance));
  instance->klass = klass;
  instance->payload = payload;
  qml_host_class_ref(klass);
  caml_register_generational_global_root(&instance->payload);

  JSValue object = JS_NewObjectClass(ctx, klass->class_id);
  if (JS_IsException(object)) {
    qml_class_instance_release(instance, 0);
    qml_raise_exception(ctx);
  }
  if (JS_SetOpaque(object, instance) < 0) {
    JS_FreeValue(ctx, object);
    qml_class_instance_release(instance, 0);
    qml_raise_string("could not attach qmljs class payload");
  }
  CAMLreturn(qml_alloc_value(context, object));
}

CAMLprim value qml_class_get(value context, value class_, value object)
{
  CAMLparam3(context, class_, object);
  CAMLlocal1(result);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  struct qml_host_class *klass = qml_class(class_);
  qml_ensure_class_context(context, klass);
  qml_ensure_same_context(context, object);

  struct qml_class_instance *instance =
    JS_GetOpaque(qml_js_value(object), klass->class_id);
  if (instance == NULL) {
    CAMLreturn(Val_int(0));
  }

  result = caml_alloc(1, 0);
  Store_field(result, 0, instance->payload);
  CAMLreturn(result);
}

CAMLprim value qml_class_is_instance(value context, value class_, value object)
{
  CAMLparam3(context, class_, object);
  JSContext *ctx = qml_context(context);
  qml_update_stack_top(ctx);
  struct qml_host_class *klass = qml_class(class_);
  qml_ensure_class_context(context, klass);
  qml_ensure_same_context(context, object);

  CAMLreturn(Val_bool(
    JS_GetOpaque(qml_js_value(object), klass->class_id) != NULL));
}
