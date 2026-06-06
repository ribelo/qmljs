# QuickJS-NG Binding Notes

## Shape

The public library facade is `Qmljs`. Implementation modules are split by
binding responsibility:

- `Qmljs_runtime`: runtime creation, QuickJS job execution, and queued host
  jobs.
- `Qmljs_context`: context creation, eval, module namespace eval, global object
  access, and global property installation.
- `Qmljs_value`: primitive construction, conversion, object introspection, and
  function calls.
- `Qmljs_function`: synchronous host functions and Eio-driven host promise
  functions.
- `Qmljs_promise`: promise creation, state, result, and Eio-compatible await.
- `Qmljs_raw` plus `*_stubs.c`: direct C FFI. This layer is not exposed by
  `Qmljs`.

## Ownership

QuickJS runtimes are single-owner resources. A runtime is permanently bound to
the domain that creates it. That domain is the only domain allowed to touch the
QuickJS runtime, contexts, or values belonging to it. C entry points enforce
that owner-domain invariant before touching QuickJS.

QuickJS values are wrapped in custom blocks with finalizers. Custom block
payloads contain only native pointers and refcounted runtime state. The OCaml
record chain keeps parents alive:

- a value record points to its context record;
- a context record points to its runtime record;
- raw value/context finalizers never call QuickJS directly from an arbitrary
  OCaml domain;
- scoped close detaches live native values and contexts on the owner domain;
- finalizers are the backup path and enqueue native releases on the runtime for
  the owner domain to drain.

Prefer `Runtime.with_` and `Context.with_` for deterministic scoped ownership.
Values should not escape the runtime/context scope that created them.

Host callbacks duplicate JS arguments before wrapping them for OCaml. Return
values are duplicated before they are handed back to QuickJS, so returned OCaml
wrappers keep their normal lifetime.

## Async Model

QuickJS promise jobs are driven by one owner-domain driver at a time:
`Qmljs.Async.drain_jobs`, `Qmljs.Promise.await`, or an explicit
`Qmljs.Eio.with_runtime ~sw ~clock` driver. Synchronous host callbacks must not
perform Eio effects because they are called through a C frame.

`Qmljs.Context.eval` and `Qmljs.Context.eval_with_timeout` always execute on the
runtime owner domain. `eval_with_timeout` installs a native monotonic deadline
read by QuickJS's interrupt handler; it never moves the runtime, context, or
value graph to another domain. This interrupts JavaScript bytecode execution.
While a deadline is armed, synchronous host callbacks are rejected before entry
with a structured resource-limit error. Use `Qmljs.Function.set_global_async`
for Eio work or callbacks that may block.

`Qmljs.Function.set_global_async` returns a JavaScript Promise immediately and
queues the OCaml callback on the runtime. The queued job is run by
`Qmljs.Async.drain_jobs`, `Qmljs.Promise.await`, or `Qmljs.Eio.drain_until_idle`,
where Eio effects are legal.

Promise rejection uses the same structured error path as thrown JavaScript
exceptions. `Qmljs.Promise.await_result` returns `(value, Qmljs.Error.t) result`
and `Qmljs.Promise.await_exn` raises a structured error through the public
`Qmljs.Error` facade.

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

- `Context.eval` is synchronous and owner-domain bound.
- The module loader supports in-memory ES modules, policy denial, `type: "js"`,
  and `type: "json"` import attributes. Full bytecode loading is not exposed yet.
- Module namespace objects are available through `Qmljs.Module.eval`,
  `Qmljs.Module.default`, `Qmljs.Module.get`, and `Qmljs.Module.keys`.
- Classes and bytecode APIs are not exposed yet.
