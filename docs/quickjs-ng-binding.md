# QuickJS-NG Binding Notes

## Shape

The public library facade is `Quick_ml`. Implementation modules are split by
binding responsibility:

- `Quick_ml_runtime`: runtime creation, QuickJS job execution, and queued host
  jobs.
- `Quick_ml_context`: context creation, eval, global object access, and global
  property installation.
- `Quick_ml_value`: primitive construction, conversion, and function calls.
- `Quick_ml_function`: synchronous host functions and Eio-driven host promise
  functions.
- `Quick_ml_promise`: promise creation, state, result, and Eio-compatible await.
- `Quick_ml_raw` plus `*_stubs.c`: direct C FFI. This layer is not exposed by
  `Quick_ml`.

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

QuickJS promise jobs are driven by `Quick_ml.Async.drain_jobs` and
`Quick_ml.Promise.await`. Synchronous host callbacks must not perform Eio
effects because they are called through a C frame.

For Eio work, use `Quick_ml.Function.set_global_async`. It returns a JavaScript
Promise immediately and queues the OCaml callback on the runtime. The queued job
is run by `Quick_ml.Async.drain_jobs` or `Quick_ml.Promise.await`, where Eio
effects are legal.

## Allocation Evidence

The allocation probe in `bench/alloc.ml` measures the current hot integer
conversion path:

```sh
nix develop .#oxcaml -c dune exec bench/alloc.exe
```

Current evidence: `Value.to_int: 0.000000 words/call`.

## Current Limitations

- Long-running JavaScript evaluation is still synchronous once entered; the
  binding has not yet added QuickJS interrupt/deadline integration.
- Module loading, typed arrays, objects, atoms, classes, and bytecode APIs are
  not exposed yet.
- The runtime should be treated as single-driver state. Use the Eio driver
  functions to progress queued host jobs and QuickJS promise jobs.
