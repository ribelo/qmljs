exception Error = Qmljs_error.Error

type runtime = Qmljs_types.runtime
type context = Qmljs_types.context
type value = Qmljs_types.value

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
