type 'a t =
  { context : Qmljs_types.context
  ; raw : Qmljs_raw.class_id
  }

type 'a method_call =
  { self : Qmljs_types.value
  ; state : 'a
  ; args : Qmljs_types.value array
  }

let create ?finalize context ~name =
  { context
  ; raw = Qmljs_raw.class_create (Qmljs_types.context_raw context) name finalize
  }

let wrap context raw = { Qmljs_types.context; raw }

let prototype t =
  wrap t.context
    (Qmljs_raw.class_prototype (Qmljs_types.context_raw t.context) t.raw)

let instance t state =
  wrap t.context
    (Qmljs_raw.class_new_instance
       (Qmljs_types.context_raw t.context)
       t.raw
       state)

let get t value =
  Qmljs_raw.class_get
    (Qmljs_types.context_raw t.context)
    t.raw
    (Qmljs_types.value_raw value)

let get_exn t value =
  match get t value with
  | Some state -> state
  | None -> invalid_arg "value is not an instance of this qmljs class"

let is_instance t value =
  Qmljs_raw.class_is_instance
    (Qmljs_types.context_raw t.context)
    t.raw
    (Qmljs_types.value_raw value)

let method_call t ~name call =
  match get t call.Qmljs_function.this_ with
  | Some state ->
      { self = call.this_
      ; state
      ; args = call.args
      }
  | None -> invalid_arg ("invalid receiver for method " ^ name)

let define_method t ~name ~arity callback =
  if arity < 0 then invalid_arg "arity must be non-negative";
  let fn =
    Qmljs_function.make_with_this t.context ~name ~arity @@ fun call ->
    callback (method_call t ~name call)
  in
  Qmljs_object.define_data_property
    (prototype t)
    ~key:(Qmljs_value.string t.context name)
    ~value:fn
    ~writable:true
    ~enumerable:false
    ~configurable:true

let define_accessor t ~name ?get:getter ?set:setter ?(enumerable = false)
    ?(configurable = true) () =
  let make_get getter =
    Qmljs_function.make_with_this t.context ~name:("get " ^ name) ~arity:0
    @@ fun call -> getter (method_call t ~name call)
  in
  let make_set setter =
    Qmljs_function.make_with_this t.context ~name:("set " ^ name) ~arity:1
    @@ fun call ->
    let value =
      match Array.get call.Qmljs_function.args 0 with
      | value -> value
      | exception Invalid_argument _ -> Qmljs_value.undefined t.context
    in
    setter (method_call t ~name call) value;
    Qmljs_value.undefined t.context
  in
  Qmljs_object.define_accessor_property
    (prototype t)
    ~key:(Qmljs_value.string t.context name)
    ?get:(Option.map make_get getter)
    ?set:(Option.map make_set setter)
    ~enumerable
    ~configurable
    ()

let constructor t ~name ~arity initialize =
  if arity < 0 then invalid_arg "arity must be non-negative";
  let fn =
    Qmljs_function.make_with_this t.context ~name ~arity @@ fun call ->
    match call.Qmljs_function.new_target with
    | None -> invalid_arg (name ^ " must be called with new")
    | Some _ -> instance t (initialize call.args)
  in
  Qmljs_function.set_constructor fn true;
  let prototype = prototype t in
  Qmljs_object.define_data_property
    fn
    ~key:(Qmljs_value.string t.context "prototype")
    ~value:prototype
    ~writable:false
    ~enumerable:false
    ~configurable:false;
  Qmljs_object.define_data_property
    prototype
    ~key:(Qmljs_value.string t.context "constructor")
    ~value:fn
    ~writable:true
    ~enumerable:false
    ~configurable:true;
  fn
