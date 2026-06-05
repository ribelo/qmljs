exception Error of string

type runtime
type context
type value

type promise_state =
  | Not_a_promise
  | Pending
  | Fulfilled
  | Rejected

module Runtime : sig
  type t = runtime

  val create : unit -> t
  val is_job_pending : t -> bool
  val execute_pending_job : t -> bool
  val drain_jobs : t -> int
end

module Context : sig
  type t = context

  val create : Runtime.t -> t

  val eval :
    ?filename:string ->
    ?strict:bool ->
    ?async:bool ->
    ?module_:bool ->
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

  val to_bool : t -> bool
  val to_int : t -> int
  val to_float : t -> float
  val to_string : t -> string
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
  val await : ?yield_every:int -> value -> value
end

module Async : sig
  val drain_jobs : ?yield_every:int -> Runtime.t -> int
end
