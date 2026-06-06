type t = Qmljs_types.atom

val runtime : t -> Qmljs_types.runtime
val of_string : Qmljs_types.context -> string -> t
val of_int : Qmljs_types.context -> int -> t
val of_uint32 : Qmljs_types.context -> int64 -> t
val to_string : Qmljs_types.context -> t -> string
val to_value : Qmljs_types.context -> t -> Qmljs_types.value
val equal : t -> t -> bool
val length : Qmljs_types.context -> t
val prototype : Qmljs_types.context -> t
val constructor : Qmljs_types.context -> t
val then_ : Qmljs_types.context -> t
val catch : Qmljs_types.context -> t
val default : Qmljs_types.context -> t
