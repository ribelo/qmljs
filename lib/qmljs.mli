exception Error of string

type runtime
type context
type value
type module_loader

type promise_state =
  | Not_a_promise
  | Pending
  | Fulfilled
  | Rejected

module Runtime : sig
  type t = runtime

  val create : unit -> t
  val with_ :
    ?memory_limit:int ->
    ?max_stack_size:int ->
    ?promise_job_limit:int ->
    (t -> 'a) ->
    'a

  val is_job_pending : t -> bool
  val execute_pending_job : t -> bool
  val drain_jobs : t -> int
  val request_interrupt : t -> unit
  val clear_interrupt : t -> unit
  val interrupt_requested : t -> bool
  val set_module_loader : t -> module_loader -> unit
end

module Context : sig
  type t = context

  val create : Runtime.t -> t
  val with_ : Runtime.t -> (t -> 'a) -> 'a

  val eval :
    ?filename:string ->
    ?strict:bool ->
    ?async:bool ->
    ?module_:bool ->
    t ->
    string ->
    value

  val eval_with_timeout :
    timeout:float ->
    ?filename:string ->
    ?strict:bool ->
    ?async:bool ->
    ?module_:bool ->
    t ->
    string ->
    value

  val eval_module : ?filename:string -> t -> string -> value

  val eval_module_with_timeout :
    timeout:float ->
    ?filename:string ->
    t ->
    string ->
    value

  val global : t -> value
  val set_global : t -> name:string -> value -> unit
end

module Value : sig
  type t = value

  type kind =
    | Undefined
    | Null
    | Bool
    | Int
    | Float
    | String
    | Symbol
    | Bigint
    | Object
    | Array
    | Function
    | Promise
    | Typed_array

  val undefined : Context.t -> t
  val null : Context.t -> t
  val bool : Context.t -> bool -> t
  val int : Context.t -> int -> t
  val float : Context.t -> float -> t
  val string : Context.t -> string -> t
  val object_ : t -> t
  val array : t -> t
  val bigint : t -> t
  val typed_array : t -> t

  val to_bool : t -> bool
  val to_int : t -> int
  val to_float : t -> float
  val to_string : t -> string

  val kind : t -> kind
  val is_null : t -> bool
  val is_undefined : t -> bool
  val is_function : t -> bool
  val is_array : t -> bool
  val strict_equal : t -> t -> bool
  val same_value : t -> t -> bool
  val same_value_zero : t -> t -> bool

  val call : t -> t array -> t
end

module Error : sig
  type t

  type kind =
    | JavaScript
    | Host_callback_defect
    | Policy_violation of { denied_module_specifier : string option }
    | Out_of_memory
    | Stack_overflow
    | Resource_limit
    | Timeout
    | Closed_handle
    | Wrong_domain

  val capture : (unit -> 'a) -> ('a, t) result
  val host : message:string -> metadata:(string * string) list -> exn
  val message : t -> string
  val kind : t -> kind
  val metadata : t -> (string * string) list
  val cause : t -> t option
  val cause_exn : t -> t
  val raise : t -> 'a
  val has_js_frame : t -> function_name:string -> bool
  val has_ocaml_frame : t -> function_name:string -> bool
  val is_policy_violation : t -> bool
  val denied_module_specifier_exn : t -> string
  val is_out_of_memory : t -> bool
  val is_stack_overflow : t -> bool
  val is_resource_limit : t -> bool
end

module Diagnostics : sig
  type snapshot

  val snapshot : unit -> snapshot
  val assert_no_leaks : before:snapshot -> after:snapshot -> unit
end

module Eio : sig
  type driver

  val with_runtime :
    sw:Eio.Switch.t ->
    clock:_ Eio.Time.clock ->
    ?memory_limit:int ->
    ?max_stack_size:int ->
    ?promise_job_limit:int ->
    (driver -> Runtime.t -> Context.t -> 'a) ->
    'a

  val cancel : driver -> unit
  val drain_until_idle : driver -> int
end

module Object : sig
  type t = value

  type descriptor =
    { value : value option
    ; writable : bool option
    ; enumerable : bool
    ; configurable : bool
    ; get : value option
    ; set : value option
    }

  val create : Context.t -> t
  val set : t -> name:string -> value -> unit
  val set_string : t -> string -> value -> unit
  val get : t -> key:value -> value
  val has : t -> key:value -> bool
  val get_string : t -> string -> value option
  val delete : t -> key:value -> bool
  val delete_string : t -> string -> bool
  val get_own_property_descriptor : t -> key:value -> descriptor option
  val define_data_property :
    t ->
    key:value ->
    value:value ->
    writable:bool ->
    enumerable:bool ->
    configurable:bool ->
    unit

  val keys : t -> string list
end

module Symbol : sig
  type t = value

  val global : Context.t -> string -> t
end

module Key : sig
  val symbol : Symbol.t -> value
end

module Array : sig
  type t = value

  val of_list : Context.t -> value list -> t
end

module Bigint : sig
  type t = value

  val of_string : Context.t -> string -> t
  val of_int64 : Context.t -> int64 -> t
  val to_string : t -> string
  val to_int64 : t -> int64 option
end

module Typed_array : sig
  module Uint8Array : sig
    type t = value

    val of_bytes_copy : Context.t -> bytes -> t
    val length : t -> int
    val get : t -> int -> int
    val set : t -> int -> int -> unit
    val to_bytes_copy : t -> bytes
  end
end

module Json : sig
  val parse : ?filename:string -> Context.t -> string -> value
  val stringify : ?space:int -> value -> string
  val of_string : ?filename:string -> Context.t -> string -> (value, Error.t) result
  val to_string : ?space:int -> value -> (string, Error.t) result
end

module Module : sig
  type namespace

  module Loader : sig
    type t = module_loader

    val memory : (string * string) list -> t
    val deny : (string -> bool) -> t -> t
    val filesystem : root:string -> allow:(specifier:string -> bool) -> t
    val normalize :
      (base:string option ->
       specifier:string ->
       (string, Error.t) result) ->
      t ->
      t
  end

  val eval : ?filename:string -> Context.t -> string -> namespace
  val default : namespace -> value
  val get : namespace -> name:string -> value option
  val keys : namespace -> string list
  val default_export_to_int : value -> int
end

module Function : sig
  val make :
    Context.t ->
    name:string ->
    arity:int ->
    (value array -> value) ->
    value

  val set_global :
    Context.t ->
    name:string ->
    arity:int ->
    (value array -> value) ->
    unit

  val set_global_async :
    Context.t ->
    name:string ->
    arity:int ->
    (value array -> value) ->
    unit
end

module Promise : sig
  type state = promise_state =
    | Not_a_promise
    | Pending
    | Fulfilled
    | Rejected

  val is : value -> bool
  val state : value -> state
  val result : value -> value
  val create : Context.t -> value * value * value
  val await_result :
    ?yield_every:int ->
    value ->
    (value, Error.t) result

  val await_exn : ?yield_every:int -> value -> value
  val await : ?yield_every:int -> value -> value
end

module Async : sig
  val drain_jobs : ?yield_every:int -> Runtime.t -> int
end
