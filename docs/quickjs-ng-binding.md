# QuickJS-NG Binding Notes

## Shape

The public library facade is `Qmljs`. Implementation modules are split by
binding responsibility:

- `Qmljs_runtime`: runtime creation, QuickJS job execution, and queued host
  jobs.
- `Qmljs_context`: context creation, eval, global object access, and global
  property installation.
- `Qmljs_value`: primitive construction, conversion, and function calls.
- `Qmljs_function`: synchronous host functions and Eio-driven host promise
  functions.
- `Qmljs_promise`: promise creation, state, result, and Eio-compatible await.
- `Qmljs_raw` plus `*_stubs.c`: direct C FFI. This layer is not exposed by
  `Qmljs`.

## Ownership

QuickJS values are wrapped in custom blocks with finalizers. OCaml records keep
the parent context and runtime alive while child values exist:

- a context record references its runtime;
- a value record references its context;
- each raw value finalizer calls `JS_FreeValue` on the owning context.

Host callbacks duplicate JS arguments before wrapping them for OCaml. Return
values are duplicated before they are handed back to QuickJS, so returned OCaml
wrappers keep their normal lifetime.

## Async Model

QuickJS promise jobs are driven by `Qmljs.Async.drain_jobs` and
`Qmljs.Promise.await`. Synchronous host callbacks must not perform Eio
effects because they are called through a C frame.

Use `Qmljs.Context.eval_in_domain` to run JavaScript on an Eio domain manager
so other fibers in the calling domain can continue. Use
`Qmljs.Context.eval_with_timeout` when a long-running script needs a deadline;
it requests a QuickJS interrupt from the calling domain and clears the interrupt
flag before returning.

For Eio work, use `Qmljs.Function.set_global_async`. It returns a JavaScript
Promise immediately and queues the OCaml callback on the runtime. The queued job
is run by `Qmljs.Async.drain_jobs` or `Qmljs.Promise.await`, where Eio
effects are legal.

## Allocation Evidence

The allocation probe in `bench/alloc.ml` measures representative hot paths:

```sh
nix develop .#oxcaml -c dune exec bench/alloc.exe
```

Current evidence:

- `Qmljs.Runtime.interrupt_requested`: `0.000000 words/call`
- `Qmljs.Runtime.is_job_pending`: `0.000000 words/call`
- `Qmljs.Value.to_int`: `0.000000 words/call`
- `Qmljs.Promise.state`: `0.000000 words/call`.

## Current Limitations

- `Context.eval` is synchronous. Use `Context.eval_in_domain` or
  `Context.eval_with_timeout` when the caller domain must keep running.
- Module loading, typed arrays, objects, atoms, classes, and bytecode APIs are
  not exposed yet.
- The runtime should be treated as single-driver state. Use the Eio driver
  functions to progress queued host jobs and QuickJS promise jobs.
