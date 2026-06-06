exception Error of string

type runtime
type context
type value
type atom
type module_loader
type error = Qmljs_error.Structured.t

type promise_state =
  | Not_a_promise
  | Pending
  | Fulfilled
  | Rejected

module Runtime : sig
  type t = runtime

  type memory_usage =
    { malloc_size : int64
    ; malloc_limit : int64
    ; memory_used_size : int64
    ; malloc_count : int64
    ; memory_used_count : int64
    ; atom_count : int64
    ; atom_size : int64
    ; str_count : int64
    ; str_size : int64
    ; obj_count : int64
    ; obj_size : int64
    ; prop_count : int64
    ; prop_size : int64
    ; shape_count : int64
    ; shape_size : int64
    ; js_func_count : int64
    ; js_func_size : int64
    ; js_func_code_size : int64
    ; js_func_pc2line_count : int64
    ; js_func_pc2line_size : int64
    ; c_func_count : int64
    ; array_count : int64
    ; fast_array_count : int64
    ; fast_array_elements : int64
    ; binary_object_count : int64
    ; binary_object_size : int64
    }

  type promise_rejection =
    { reason : error
    ; is_handled : bool
    }

  type promise_hook_kind =
    | Promise_init
    | Promise_before
    | Promise_after
    | Promise_resolve

  type promise_hook =
    { hook : promise_hook_kind
    ; state : promise_state
    ; has_parent : bool
    }

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
  val memory_usage : t -> memory_usage
  val run_gc : t -> unit
  val gc_threshold : t -> int
  val set_info : t -> string -> unit
  val set_promise_rejection_tracking : t -> bool -> unit
  val take_promise_rejections : t -> promise_rejection list
  val set_promise_hook_tracking : t -> bool -> unit
  val take_promise_hooks : t -> promise_hook list
  val set_gc_threshold : t -> int -> unit
  val request_interrupt : t -> unit
  val clear_interrupt : t -> unit
  val interrupt_requested : t -> bool
  val set_module_loader : t -> module_loader -> unit
end

module Context : sig
  type t = context

  type intrinsic =
    | Date
    | Eval
    | Reg_exp_compiler
    | Reg_exp
    | Json
    | Proxy
    | Map_set
    | Typed_arrays
    | Promise
    | Big_int
    | Weak_ref
    | Performance
    | Dom_exception

  val create : Runtime.t -> t
  val create_raw : ?intrinsics:intrinsic list -> Runtime.t -> t
  val with_ : Runtime.t -> (t -> 'a) -> 'a
  val with_raw :
    ?intrinsics:intrinsic list ->
    Runtime.t ->
    (t -> 'a) ->
    'a

  val eval :
    ?filename:string ->
    ?strict:bool ->
    ?async:bool ->
    ?module_:bool ->
    ?backtrace_barrier:bool ->
    t ->
    string ->
    value

  val eval_with_timeout :
    timeout:float ->
    ?filename:string ->
    ?strict:bool ->
    ?async:bool ->
    ?module_:bool ->
    ?backtrace_barrier:bool ->
    t ->
    string ->
    value

  val eval_file :
    ?strict:bool ->
    ?async:bool ->
    ?module_:bool ->
    ?backtrace_barrier:bool ->
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

  val eval_module_file : t -> string -> value

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

  (** [int ctx n] creates a JS [Number] from the OCaml int [n].

      JS numbers are IEEE-754 doubles, so only integers with
      [abs n <= 9_007_199_254_740_991] ([Number.MAX_SAFE_INTEGER], i.e.
      [2^53 - 1]) are representable exactly. OCaml's native int is 63-bit and can
      hold larger magnitudes, which a JS number cannot store without loss, so
      [int] raises [Invalid_argument] for [abs n > MAX_SAFE_INTEGER] rather than
      silently rounding. Use {!Bigint.of_int64} when you need an exact large
      integer (as a JS [BigInt], with BigInt — not Number — semantics). *)
  val int : Context.t -> int -> t

  (** [float ctx x] creates a JS [Number] from [x] with no range restriction. *)
  val float : Context.t -> float -> t
  val string : Context.t -> string -> t
  val object_ : t -> t
  val array : t -> t
  val bigint : t -> t
  val typed_array : t -> t

  val to_bool : t -> bool

  (** [to_int t] reads [t] as an OCaml int. Numbers follow JS [ToInteger]
      (truncation toward zero); a [BigInt] is read exactly. Raises if the
      integer value falls outside the OCaml int range. Because {!int} only
      accepts safe-range integers, [to_int (int ctx n) = n] holds for every [n]
      that {!int} accepts. *)
  val to_int : t -> int
  val to_float : t -> float
  val to_string : t -> string

  type 'a encoder = Context.t -> 'a -> t
  type 'a decoder = t -> ('a, error) result

  val as_bool : bool decoder
  val as_int : int decoder
  val as_float : float decoder
  val as_string : string decoder
  val as_bigint : t decoder
  val as_object : t decoder
  val as_array : t decoder
  val as_typed_array : t decoder

  val of_list : 'a encoder -> Context.t -> 'a list -> t
  val to_list : 'a decoder -> t -> ('a list, error) result
  val of_array : 'a encoder -> Context.t -> 'a array -> t
  val to_array : 'a decoder -> t -> ('a array, error) result
  val of_option : 'a encoder -> Context.t -> 'a option -> t
  val to_option : 'a decoder -> t -> ('a option, error) result
  val of_result :
    ok:'a encoder ->
    error:'e encoder ->
    Context.t ->
    ('a, 'e) result ->
    t
  val to_result :
    ok:'a decoder ->
    error:'e decoder ->
    t ->
    (('a, 'e) result, error) result
  val of_assoc : 'a encoder -> Context.t -> (string * 'a) list -> t
  val to_assoc : 'a decoder -> t -> ((string * 'a) list, error) result
  val of_record : 'a encoder -> Context.t -> (string * 'a) list -> t
  val to_record : 'a decoder -> t -> ((string * 'a) list, error) result

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
  type t = error

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
  val conversion : string -> t
  val message : t -> string
  val kind : t -> kind
  val metadata : t -> (string * string) list
  val cause : t -> t option
  val suppressed : t -> t list
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
  val native_runtimes : snapshot -> int
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
  val promise :
    ?name:string ->
    driver ->
    Context.t ->
    (unit -> value) ->
    value
end

module Atom : sig
  type t = atom

  val of_string : Context.t -> string -> t
  val of_int : Context.t -> int -> t
  val of_uint32 : Context.t -> int64 -> t
  val to_string : Context.t -> t -> string
  val to_value : Context.t -> t -> value
  val equal : t -> t -> bool
  val length : Context.t -> t
  val prototype : Context.t -> t
  val constructor : Context.t -> t
  val then_ : Context.t -> t
  val catch : Context.t -> t
  val default : Context.t -> t
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

  val create : ?prototype:value -> Context.t -> t
  val set : t -> name:string -> value -> unit
  val set_string : t -> string -> value -> unit
  val set_atom : t -> key:atom -> value -> unit
  val get : t -> key:value -> value
  val get_atom : t -> key:atom -> value
  val has : t -> key:value -> bool
  val has_atom : t -> key:atom -> bool
  val get_string : t -> string -> value option
  val delete : t -> key:value -> bool
  val delete_atom : t -> key:atom -> bool
  val delete_string : t -> string -> bool
  val get_prototype : t -> value
  val set_prototype : t -> prototype:value -> unit
  val get_own_property_descriptor : t -> key:value -> descriptor option
  val get_own_property_descriptor_atom : t -> key:atom -> descriptor option
  val define_data_property :
    t ->
    key:value ->
    value:value ->
    writable:bool ->
    enumerable:bool ->
    configurable:bool ->
    unit

  val define_data_property_atom :
    t ->
    key:atom ->
    value:value ->
    writable:bool ->
    enumerable:bool ->
    configurable:bool ->
    unit

  val define_accessor_property :
    t ->
    key:value ->
    ?get:value ->
    ?set:value ->
    enumerable:bool ->
    configurable:bool ->
    unit ->
    unit

  val define_accessor_property_atom :
    t ->
    key:atom ->
    ?get:value ->
    ?set:value ->
    enumerable:bool ->
    configurable:bool ->
    unit ->
    unit

  val own_keys :
    ?strings:bool ->
    ?symbols:bool ->
    ?enumerable_only:bool ->
    t ->
    value list

  val entries :
    ?strings:bool ->
    ?symbols:bool ->
    ?enumerable_only:bool ->
    t ->
    (value * value) list

  val values :
    ?strings:bool ->
    ?symbols:bool ->
    ?enumerable_only:bool ->
    t ->
    value list

  val keys : t -> string list
end

module Proxy : sig
  type t = value

  val create : target:value -> handler:value -> t
  val is_proxy : value -> bool
  val target : value -> value option
  val handler : value -> value option
end

module Symbol : sig
  type t = value

  val global : Context.t -> string -> t
  val iterator : Context.t -> t
  val to_string_tag : Context.t -> t
  val has_instance : Context.t -> t
end

module Key : sig
  val symbol : Symbol.t -> value
end

module Iterable : sig
  type t = value

  val of_step : Context.t -> (unit -> value option) -> t
  val to_list : ?max_steps:int -> value -> (value list, Error.t) result
end

module Array : sig
  type t = value

  val create : Context.t -> t
  val of_array : Context.t -> value array -> t
  val of_list : Context.t -> value list -> t
  val length : t -> int
  val get : t -> int -> value
  val set : t -> int -> value -> unit
  val push : t -> value -> int
  val append : t -> value list -> int
  val to_array : t -> value array
  val to_list : t -> value list
end

module Array_buffer : sig
  type t = value

  type byte_bigarray =
    (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  val of_bytes_copy : Context.t -> bytes -> t
  val of_bigarray_copy : Context.t -> byte_bigarray -> t
  val byte_length : t -> int
  val to_bytes_copy : t -> bytes
  val detach : t -> unit
end

module Bigint : sig
  type t = value

  val of_string : Context.t -> string -> t
  val of_int64 : Context.t -> int64 -> t
  val to_string : t -> string
  val to_int64 : t -> int64 option
end

module Typed_array : sig
  module type Int_view = sig
    type t = value

    val of_array_buffer : ?byte_offset:int -> ?length:int -> value -> t
    val length : t -> int
    val byte_offset : t -> int
    val byte_length : t -> int
    val buffer : t -> value
    val get : t -> int -> int
    val set : t -> int -> int -> unit
  end

  module type Float_view = sig
    type t = value

    val of_array_buffer : ?byte_offset:int -> ?length:int -> value -> t
    val length : t -> int
    val byte_offset : t -> int
    val byte_length : t -> int
    val buffer : t -> value
    val get : t -> int -> float
    val set : t -> int -> float -> unit
  end

  module type Bigint_view = sig
    type t = value

    val of_array_buffer : ?byte_offset:int -> ?length:int -> value -> t
    val length : t -> int
    val byte_offset : t -> int
    val byte_length : t -> int
    val buffer : t -> value
    val get : t -> int -> Bigint.t
    val set : t -> int -> Bigint.t -> unit
  end

  module Int8Array : Int_view

  module Uint8Array : sig
    include Int_view

    val of_bytes_copy : Context.t -> bytes -> t
    val to_bytes_copy : t -> bytes
  end

  module Uint8ClampedArray : Int_view
  module Int16Array : Int_view
  module Uint16Array : Int_view
  module Int32Array : Int_view
  module Uint32Array : Int_view
  module BigInt64Array : Bigint_view
  module BigUint64Array : Bigint_view
  module Float32Array : Float_view
  module Float64Array : Float_view
end

module Json : sig
  val parse : ?filename:string -> Context.t -> string -> value
  val stringify : ?space:int -> value -> string
  val of_string : ?filename:string -> Context.t -> string -> (value, Error.t) result
  val to_string : ?space:int -> value -> (string, Error.t) result
end

module Module : sig
  type namespace = value

  type evaluated =
    { namespace : namespace
    ; import_meta : value
    }

  module Native : sig
    type t
    type declarations
    type exports

    val define :
      (declarations -> unit) ->
      evaluate:(exports -> unit) ->
      t

    val declare : declarations -> string -> unit
    val export : exports -> name:string -> value -> unit
  end

  module Loader : sig
    type t = module_loader
    type import_attributes = (string * string) list

    type request =
      { base : string option
      ; specifier : string
      ; normalized : string
      ; attributes : import_attributes
      }

    val memory : (string * string) list -> t
    val deny : (string -> bool) -> t -> t
    val callback :
      normalize:
        (base:string option ->
         specifier:string ->
         attributes:import_attributes ->
         (string, Error.t) result) ->
      load:(request -> (string option, Error.t) result) ->
      t
    val fallback : t list -> t
    val filesystem : root:string -> allow:(specifier:string -> bool) -> t
    val normalize :
      (base:string option ->
       specifier:string ->
       (string, Error.t) result) ->
      t ->
      t
  end

  val declare_native : Context.t -> name:string -> Native.t -> unit
  val dynamic_import : ?base:string -> Context.t -> string -> value
  val eval_with_meta : ?filename:string -> Context.t -> string -> evaluated
  val eval : ?filename:string -> Context.t -> string -> namespace
  val default : namespace -> value
  val get : namespace -> name:string -> value option
  val keys : namespace -> string list
  val default_export_to_int : value -> int
end

module Function : sig
  type call =
    { this_ : value
    ; args : value array
    ; new_target : value option
    }

  val make :
    Context.t ->
    name:string ->
    arity:int ->
    (value array -> value) ->
    value

  val make_with_this :
    Context.t ->
    name:string ->
    arity:int ->
    (call -> value) ->
    value

  val make_async :
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

  val set_global_with_this :
    Context.t ->
    name:string ->
    arity:int ->
    (call -> value) ->
    unit

  val set_global_async :
    Context.t ->
    name:string ->
    arity:int ->
    (value array -> value) ->
    unit

  val call : ?this_:value -> value -> value array -> value
  val construct : value -> value array -> value
  val defer : ?this_:value -> value -> value array -> unit
  val name : value -> string
  val set_name : value -> string -> unit
  val length : value -> int
  val set_length : value -> int -> unit
  val prototype : Context.t -> value
  val is_constructor : value -> bool
  val set_constructor : value -> bool -> unit
end

module Class : sig
  type 'a t

  type 'a method_call =
    { self : value
    ; state : 'a
    ; args : value array
    }

  (** [finalize] runs when QuickJS releases an instance on the owner domain.
      Exceptions raised by [finalize] are ignored because no JS caller exists. *)
  val create : ?finalize:('a -> unit) -> Context.t -> name:string -> 'a t
  val prototype : 'a t -> value
  val instance : 'a t -> 'a -> value
  val get : 'a t -> value -> 'a option
  val get_exn : 'a t -> value -> 'a
  val is_instance : 'a t -> value -> bool
  val define_method :
    'a t ->
    name:string ->
    arity:int ->
    ('a method_call -> value) ->
    unit

  val define_accessor :
    'a t ->
    name:string ->
    ?get:('a method_call -> value) ->
    ?set:('a method_call -> value -> unit) ->
    ?enumerable:bool ->
    ?configurable:bool ->
    unit ->
    unit

  val constructor :
    'a t ->
    name:string ->
    arity:int ->
    (value array -> 'a) ->
    value
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
