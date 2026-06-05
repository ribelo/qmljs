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

QuickJS runtimes are single-owner resources. A runtime is permanently bound to
the domain that creates it. That domain is the only domain allowed to touch the
QuickJS runtime, contexts, or values belonging to it. C entry points enforce
that owner-domain invariant before touching QuickJS.

QuickJS values are wrapped in custom blocks with finalizers. The C wrappers keep
the parent context and runtime rooted while child values exist:

- a context wrapper roots its runtime wrapper;
- a value wrapper roots its context wrapper;
- raw value/context finalizers never call QuickJS directly from an arbitrary
  OCaml domain;
- finalizers enqueue native releases on the runtime, and the owner domain drains
  those releases before normal API calls and during scoped runtime close.

Prefer `Runtime.with_` and `Context.with_` for deterministic scoped ownership.
Values should not escape the runtime/context scope that created them.

Host callbacks duplicate JS arguments before wrapping them for OCaml. Return
values are duplicated before they are handed back to QuickJS, so returned OCaml
wrappers keep their normal lifetime.

## Async Model

QuickJS promise jobs are driven by `Qmljs.Async.drain_jobs` and
`Qmljs.Promise.await`. Synchronous host callbacks must not perform Eio
effects because they are called through a C frame.

`Qmljs.Context.eval_in_domain` does not move an existing `JSContext` onto a
worker domain; doing so would race QuickJS state and custom finalizers. It keeps
execution on the runtime owner. `Qmljs.Context.eval_with_timeout` preserves that
same owner-domain rule. Its timeout worker uses the supplied Eio domain manager
and clock only to sleep and flip the runtime's atomic interrupt flag; it never
touches the QuickJS runtime, context, or value graph.

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

- `Context.eval` is synchronous and owner-domain bound.
- The module loader currently covers in-memory modules needed by the readiness
  contract; full QuickJS module normalization and bytecode loading are still
  future work.
- Classes and bytecode APIs are not exposed yet.
