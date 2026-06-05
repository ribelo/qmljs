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

  val eval_in_domain :
    domain_mgr:_ Eio.Domain_manager.t ->
    ?filename:string ->
    ?strict:bool ->
    ?async:bool ->
    ?module_:bool ->
    t ->
    string ->
    value

  val eval_with_timeout :
    domain_mgr:_ Eio.Domain_manager.t ->
    clock:_ Eio.Time.clock ->
    timeout:float ->
    ?filename:string ->
    ?strict:bool ->
    ?async:bool ->
    ?module_:bool ->
    t ->
    string ->
    value

  val eval_module : t -> string -> value

  val eval_module_with_timeout :
    domain_mgr:_ Eio.Domain_manager.t ->
    clock:_ Eio.Time.clock ->
    timeout:float ->
    t ->
    string ->
    value

  val global : t -> value
  val set_global : t -> name:string -> value -> unit
end

module Value : sig
  type t = value

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

  val call : t -> t array -> t
end

module Error : sig
  type t

  val capture : (unit -> 'a) -> ('a, t) result
  val host : message:string -> metadata:(string * string) list -> exn
  val message : t -> string
  val cause_exn : t -> t
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
  val with_runtime :
    domain_mgr:'domain_mgr ->
    clock:'clock ->
    timeout:float ->
    (Runtime.t -> Context.t -> 'a) ->
    'a
end

module Object : sig
  type t = value

  val create : Context.t -> t
  val set : t -> name:string -> value -> unit
  val define_data_property :
    t ->
    key:value ->
    value:value ->
    writable:bool ->
    enumerable:bool ->
    configurable:bool ->
    unit
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
end

module Typed_array : sig
  module Uint8Array : sig
    type t = value

    val of_bytes_copy : Context.t -> bytes -> t
  end
end

module Module : sig
  module Loader : sig
    type t = module_loader

    val memory : (string * string) list -> t
    val deny : (string -> bool) -> t -> t
  end

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
  val await : ?yield_every:int -> value -> value
end

module Async : sig
  val drain_jobs : ?yield_every:int -> Runtime.t -> int
end
