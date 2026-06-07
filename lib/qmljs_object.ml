type t = Qmljs_types.value

type descriptor =
  { value : Qmljs_types.value option
  ; writable : bool option
  ; enumerable : bool
  ; configurable : bool
  ; get : Qmljs_types.value option
  ; set : Qmljs_types.value option
  }

let create ?prototype context =
  let raw =
    match prototype with
    | None -> Qmljs_raw.object_create (Qmljs_types.context_raw context)
    | Some prototype ->
        Qmljs_raw.object_create_proto
          (Qmljs_types.context_raw context)
          (Qmljs_types.value_raw prototype)
  in
  { Qmljs_types.context; raw }

let set t ~name value =
  let context = Qmljs_types.value_context t in
  Qmljs_raw.object_set
    (Qmljs_types.context_raw context)
    (Qmljs_types.value_raw t)
    name
    (Qmljs_types.value_raw value)

let set_string t name value = set t ~name value

let set_atom t ~key value =
  let context = Qmljs_types.value_context t in
  Qmljs_raw.object_set_atom
    (Qmljs_types.context_raw context)
    (Qmljs_types.value_raw t)
    (Qmljs_types.atom_raw key)
    (Qmljs_types.value_raw value)

let define_data_property t ~key ~value ~writable ~enumerable ~configurable =
  let flags =
    (if writable then 1 else 0)
    lor (if enumerable then 2 else 0)
    lor (if configurable then 4 else 0)
  in
  let context = Qmljs_types.value_context t in
  Qmljs_raw.object_define_data_property
    (Qmljs_types.context_raw context)
    (Qmljs_types.value_raw t)
    (Qmljs_types.value_raw key)
    (Qmljs_types.value_raw value)
    flags

let define_data_property_atom t ~key ~value ~writable ~enumerable
    ~configurable =
  let flags =
    (if writable then 1 else 0)
    lor (if enumerable then 2 else 0)
    lor (if configurable then 4 else 0)
  in
  let context = Qmljs_types.value_context t in
  Qmljs_raw.object_define_data_property_atom
    (Qmljs_types.context_raw context)
    (Qmljs_types.value_raw t)
    (Qmljs_types.atom_raw key)
    (Qmljs_types.value_raw value)
    flags

let define_accessor_property t ~key ?get ?set ~enumerable ~configurable () =
  let flags =
    (if enumerable then 1 else 0)
    lor (if configurable then 2 else 0)
  in
  let context = Qmljs_types.value_context t in
  Qmljs_raw.object_define_accessor_property
    (Qmljs_types.context_raw context)
    (Qmljs_types.value_raw t)
    (Qmljs_types.value_raw key)
    (Option.map Qmljs_types.value_raw get, Option.map Qmljs_types.value_raw set)
    flags

let define_accessor_property_atom t ~key ?get ?set ~enumerable ~configurable
    () =
  let flags =
    (if enumerable then 1 else 0)
    lor (if configurable then 2 else 0)
  in
  let context = Qmljs_types.value_context t in
  Qmljs_raw.object_define_accessor_property_atom
    (Qmljs_types.context_raw context)
    (Qmljs_types.value_raw t)
    (Qmljs_types.atom_raw key)
    (Option.map Qmljs_types.value_raw get, Option.map Qmljs_types.value_raw set)
    flags

let wrap context raw = { Qmljs_types.context; raw }

let get t ~key =
  let context = Qmljs_types.value_context t in
  wrap context
    (Qmljs_raw.object_get
       (Qmljs_types.context_raw context)
       (Qmljs_types.value_raw t)
       (Qmljs_types.value_raw key))

let get_atom t ~key =
  let context = Qmljs_types.value_context t in
  wrap context
    (Qmljs_raw.object_get_atom
       (Qmljs_types.context_raw context)
       (Qmljs_types.value_raw t)
       (Qmljs_types.atom_raw key))

let has t ~key =
  let context = Qmljs_types.value_context t in
  Qmljs_raw.object_has
    (Qmljs_types.context_raw context)
    (Qmljs_types.value_raw t)
    (Qmljs_types.value_raw key)

let has_atom t ~key =
  let context = Qmljs_types.value_context t in
  Qmljs_raw.object_has_atom
    (Qmljs_types.context_raw context)
    (Qmljs_types.value_raw t)
    (Qmljs_types.atom_raw key)

let get_string t name =
  let context = Qmljs_types.value_context t in
  let key = Qmljs_value.string context name in
  (* Convenience reader: follows the prototype chain like JS [[Get]], so an
     inherited property is visible. Public [has] is own-property only. *)
  let present =
    Qmljs_raw.object_has_property
      (Qmljs_types.context_raw context)
      (Qmljs_types.value_raw t)
      (Qmljs_types.value_raw key)
  in
  if present then Some (get t ~key) else None

let delete t ~key =
  let context = Qmljs_types.value_context t in
  Qmljs_raw.object_delete
    (Qmljs_types.context_raw context)
    (Qmljs_types.value_raw t)
    (Qmljs_types.value_raw key)

let delete_atom t ~key =
  let context = Qmljs_types.value_context t in
  Qmljs_raw.object_delete_atom
    (Qmljs_types.context_raw context)
    (Qmljs_types.value_raw t)
    (Qmljs_types.atom_raw key)

let delete_string t name =
  let context = Qmljs_types.value_context t in
  delete t ~key:(Qmljs_value.string context name)

let get_prototype t =
  let context = Qmljs_types.value_context t in
  wrap context
    (Qmljs_raw.object_get_prototype
       (Qmljs_types.context_raw context)
       (Qmljs_types.value_raw t))

let set_prototype t ~prototype =
  Qmljs_raw.object_set_prototype
    (Qmljs_types.context_raw (Qmljs_types.value_context t))
    (Qmljs_types.value_raw t)
    (Qmljs_types.value_raw prototype)

let descriptor_of_raw context = function
  | None -> None
  | Some (value, writable, enumerable, configurable, get, set) ->
      let wrap_option = Option.map (wrap context) in
      Some
        { value = wrap_option value
        ; writable
        ; enumerable
        ; configurable
        ; get = wrap_option get
        ; set = wrap_option set
        }

let get_own_property_descriptor t ~key =
  let context = Qmljs_types.value_context t in
  descriptor_of_raw context
    (Qmljs_raw.object_get_own_property_descriptor
       (Qmljs_types.context_raw context)
       (Qmljs_types.value_raw t)
       (Qmljs_types.value_raw key))

let get_own_property_descriptor_atom t ~key =
  let context = Qmljs_types.value_context t in
  descriptor_of_raw context
    (Qmljs_raw.object_get_own_property_descriptor_atom
       (Qmljs_types.context_raw context)
       (Qmljs_types.value_raw t)
       (Qmljs_types.atom_raw key))

let keys t =
  let context = Qmljs_types.value_context t in
  Qmljs_raw.object_keys
    (Qmljs_types.context_raw context)
    (Qmljs_types.value_raw t)
  |> Array.to_list

let own_key_flags ?(strings = true) ?(symbols = false)
    ?(enumerable_only = false) () =
  (if strings then 1 else 0)
  lor (if symbols then 2 else 0)
  lor (if enumerable_only then 4 else 0)

let own_keys ?strings ?symbols ?enumerable_only t =
  let context = Qmljs_types.value_context t in
  Qmljs_raw.object_own_keys
    (Qmljs_types.context_raw context)
    (Qmljs_types.value_raw t)
    (own_key_flags ?strings ?symbols ?enumerable_only ())
  |> Array.map (wrap context)
  |> Array.to_list

let entries ?strings ?symbols ?enumerable_only t =
  own_keys ?strings ?symbols ?enumerable_only t
  |> List.map (fun key -> key, get t ~key)

let values ?strings ?symbols ?enumerable_only t =
  entries ?strings ?symbols ?enumerable_only t
  |> List.map snd
