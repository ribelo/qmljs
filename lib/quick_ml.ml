exception Error = Quick_ml_error.Error

type runtime = Quick_ml_types.runtime
type context = Quick_ml_types.context
type value = Quick_ml_types.value

type promise_state = Quick_ml_types.promise_state =
  | Not_a_promise
  | Pending
  | Fulfilled
  | Rejected

module Runtime = Quick_ml_runtime
module Context = Quick_ml_context
module Value = Quick_ml_value
module Promise = Quick_ml_promise
module Async = Quick_ml_async
