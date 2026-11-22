exception Error = Qmljs_error.Error

type runtime = Qmljs_types.runtime
type context = Qmljs_types.context
type value = Qmljs_types.value
type atom = Qmljs_types.atom
type module_loader = Qmljs_types.module_loader
type error = Qmljs_error.Structured.t

type promise_state = Qmljs_types.promise_state =
  | Not_a_promise
  | Pending
  | Fulfilled
  | Rejected

module Runtime = Qmljs_runtime
module Context = Qmljs_context
module Value = Qmljs_value
module Function = Qmljs_function
module Promise = Qmljs_promise
module Async = Qmljs_async
module Error = Qmljs_error.Structured
module Diagnostics = Qmljs_diagnostics
module Atom = Qmljs_atom
module Object = Qmljs_object
module Class = Qmljs_class
module Proxy = Qmljs_proxy
module Symbol = Qmljs_symbol
module Key = Qmljs_key
module Iterable = Qmljs_iterable
module Array = Qmljs_array
module Array_buffer = Qmljs_array_buffer
module Bigint = Qmljs_bigint
module Typed_array = Qmljs_typed_array
module Json = Qmljs_json
module Module = Qmljs_module
