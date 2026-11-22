# Repository Guidelines

## Project Structure & Module Organization

qmljs is an OxCaml binding to quickjs-ng. The public library is qmljs and the
public facade module is Qmljs.

Core OCaml code lives in lib/. Keep modules split by binding responsibility:

- Qmljs_runtime: runtime ownership, resource limits, interrupts, QuickJS jobs,
  and queued host jobs.
- Qmljs_context: context lifecycle, eval, modules, globals, and scoped context
  ownership.
- Qmljs_value: primitive construction, conversion, and wrapper ownership.
- Qmljs_function: synchronous host callbacks and Async-backed async host
  callbacks.
- Qmljs_promise: promise state, result extraction, and Async await.
- Qmljs_driver (private): the one place that drains QuickJS and host jobs and
  runs host work as Async workers.
- Qmljs_async: the Qmljs.Async driver scope, cancellation and job draining.
- Qmljs_error: structured failures crossing the JS/C/OxCaml boundary.
- Qmljs_raw plus *_stubs.c: private FFI surface. Do not expose this as the
  user-facing API.

Tests live under test/. Allocation probes live under bench/. Design notes live
under docs/. Generated build output belongs in _build/. Development uses the
existing shared opam switch; do not create a repository-local switch.

Do not grow one giant binding file. Add a focused module when a new QuickJS area
gets enough surface to justify it, and pair public API changes with .mli
changes.

## Package Boundary Policy

The root qmljs package must stay small: QuickJS runtime/context/value bindings,
structured errors, and the minimum Async integration needed for the current
async API. The library depends on async_kernel only; async_unix belongs to the
application that runs the scheduler.

Optional adapters belong in separate packages when they add real dependencies
or policy:

- qmljs_async or another adapter package may own richer Async driver behavior
  if core Async usage grows beyond the minimum surface.
- Test helpers belong outside the runtime core if they need extra dependencies.
- Provider-specific loaders, codecs, or framework integrations must not be
  added to the root package.

Least astonishment rule: the opam package name, Dune public library name, and
OCaml top-level module should line up. Prefer qmljs -> Qmljs and qmljs_feature
-> Qmljs_feature.

## OxCaml Requirement

qmljs is OxCaml, not mainline OCaml. Keep the package pinned to:

- ocaml (= 5.4.0)
- ocaml-variants (= 5.4.0+ox)

The default and oxcaml development shells activate switch "5.4.0+ox" with
OPAMROOT=$HOME/.cache/opam. qmljs-oxcaml-init only validates that shared switch
and its existing dependencies; it does not create switches or install packages.
Do not install, upgrade, or pin anything in the shared switch for this project.
If a required package is missing, report it and stop. Keep the exact compiler
pin; mainline OCaml is not supported.

qmljs_ppx remains in the repository but its library, interface fixture, and test
are excluded from the 5.4 build. It depends on ppxlib, whose OxCaml package guard
requires ocaml < 5.3. Do not replace it with compiler-libs or weaken core tests.
Exergy consumes qmljs (including its Async driver, async host callback, and
promise APIs),
not qmljs_ppx.

Use OxCaml features where they carry a real invariant or performance win:
portability, allocation control, stack allocation, uniqueness, unboxed types,
and mode/kind annotations. Do not add annotation noise that does not strengthen
the boundary.

Hot C stubs may use @@noalloc only when they still preserve all safety checks
needed by the API. Correct closed-handle and owner-domain checks beat a fake
fast path.

## Reference Code

Use local reference projects before inventing patterns:

- /home/ribelo/projects/ribelo/quick-ml/.reference/rquickjs - prior art for a
  mature QuickJS binding shape, module loading, value ownership, and JS
  semantics. Treat it as a behavior and design reference for the binding shape.

qmljs core stands alone: QuickJS runtime/context/value bindings, structured
errors, and the minimum Async integration the async API needs.

## QuickJS Ownership Invariants

QuickJS runtimes, contexts, and values are single-owner resources. A runtime is
permanently bound to the domain that creates it. Every context and value derived
from that runtime belongs to the same owner domain.

The owner-domain rule is structural:

- never expose an API that intentionally hands a live runtime, context, value,
  object, promise, function, array, symbol, bigint, or typed array to another
  domain;
- only already-extracted immutable data may cross domains, such as ints,
  strings, bytes copies, records of primitive data, or structured error data;
- Runtime.request_interrupt is the only cross-domain escape hatch, and it must
  remain atomic-only;
- all other QuickJS entry points must check owner-domain and closed-handle state
  before touching QuickJS.

Finalizers may run on any OCaml domain. They must not call QuickJS directly.
They may enqueue native releases for the owner domain to drain. The wrapper
root chain must keep parents alive while children exist:

- value roots context;
- context roots runtime;
- runtime owns the deferred native release queue.

## Boundary Error Model

Build failure structure where the failure happens, while the live QuickJS
values (.name, .message, .stack, .cause) are still in hand.

The C boundary classifies the error there and hands OxCaml structured data. The
public error model must distinguish at least:

- JS threw an exception;
- QuickJS out of memory;
- QuickJS stack overflow;
- qmljs policy violation, including denied module specifiers;
- timeout or interrupt;
- resource limit;
- OCaml host callback defect.

JS failures and OCaml host callback defects are distinct kinds. Preserve the
cause chain and frame kind (Js_frame vs Ocaml_frame) so callers inspect the
boundary through structured data.

Legacy string exceptions may exist only as explicit legacy fallback. New code
must use structured errors.

## No Silent Fallback

If qmljs cannot honor a contract, it must fail loudly and specifically. It must
not return plausible data from a fake implementation.

Forbidden patterns:

- timeout by source-string sniffing;
- module loading by hardcoded export values;
- resource limits by matching snippets such as while (true) or new Array(...);
- silently defaulting unknown errors to JavaScript errors;
- no-op diagnostics that claim safety evidence;
- compatibility shims that keep stale APIs alive after the contract changes.

Red tests are allowed to design API and DX. Green tests must be backed by real
QuickJS behavior, explicit structured failure, or an intentionally documented
unsupported error. Do not game tests.

## Scoped Lifecycle

Runtime.with_ and Context.with_ are real brackets that own setup and teardown.

They close deterministically on:

- success;
- JS failure;
- OCaml exception;
- driver cancellation and close (Qmljs.Async.with_runtime joins every worker
  before the context closes).

Cleanup runs on the owner domain. GC finalizers are the backup path.

When the body fails and cleanup also fails, preserve the body failure and attach
the cleanup failure as suppressed structure. Do not drop either failure and do
not replace the body error with cleanup noise.

## Async and Driver Model

There is one explicit driver for each runtime. That driver is the owned place
that advances QuickJS promise jobs and qmljs host jobs.

Promise.await, Async.drain_jobs, timeout handling, and async host callback
completion all delegate to that one driver's semantics.

Timeouts must keep QuickJS on the owner domain. The current deadline path uses
a native monotonic deadline read by the QuickJS interrupt handler; any future
worker may only flip the atomic interrupt flag and must not touch the runtime,
context, or value graph.

Queued host jobs and workers are tied to the runtime's driver scope
(Qmljs.Async.with_runtime). Cancellation propagates to queued work, which
rejects with a structured cancellation defect; failures are observable by the
owner; no job outlives the scope that made it meaningful. Async cannot interrupt
a running Deferred, so workers stop cooperatively on Qmljs.Async.cancelled and
the scope joins them before the context closes.

Runtimes, contexts, values and drivers are used by Async jobs on the owner
domain, never concurrently. Driver state is a sum type (Open, Cancelled,
Closed) mutated only by those jobs; wakeups use a generation plus an Ivar snapshotted before promise state
is read, so no wakeup is lost and no waiter spins while a driver exists.

## Build, Test, and Development Commands

Use the OxCaml shell for normal gates:

    nix develop .#oxcaml -c dune build
    nix develop .#oxcaml -c dune runtest
    nix develop .#oxcaml -c dune exec bench/alloc.exe
    nix flake check --no-build

opam lint qmljs.opam is useful, but do not invent package synopsis,
description, homepage, license, or bug-report metadata just to satisfy it.

## Coding Style & Naming Conventions

- Two-space indentation; snake_case values; PascalCase modules.
- Every public module pairs a `.ml` with an explicit `.mli`. Change them together.
- Prefer structured APIs and QuickJS-provided data over ad hoc string parsing.
- Comments explain invariants and non-obvious ownership, not what the code says.
- Make small, surgical diffs. Do not reformat or refactor untouched code in the
  same change.

## Testing Guidelines

Tests should exercise production contracts, not implementation accidents.

Important test classes:

- lifetime and GC stress with no native wrapper leaks;
- structured cross-boundary errors with JS cause chains and OCaml callback
  defects;
- many runtimes/fibers cancelling cleanly without cross-domain QuickJS access;
- value-surface round trips matching JavaScript semantics;
- module loader policy, import attributes, and denied specifiers;
- resource limits, deadlines, recovery after failure, and allocation hot paths.

Run the OxCaml gates before handoff when touching code. For docs-only changes,
run at least nix flake check --no-build unless it is unrelated to the edit.

## Engineering Rules

The ownership, error, async, and no-fallback contracts above are the rules. This
section adds only what they do not already cover:

- **Delete old paths; do not deprecate them.** The commit history is the
  migration guide. No dead behavior lives in the library.
- **Churn is not a design objection.** Do the correct fix and update callers.
  Do not keep a wrong path because changing it is annoying.
- **Performance claims need evidence.** Zero-allocation and `@@noalloc` claims
  require a benchmark or allocation probe. If a safe path allocates, say so and
  measure it.

## Commit & Pull Request Guidelines

Workers do not use git or commit changes. The lead owns commits. Handoffs
list changed paths, exact check commands and exit codes, and remaining risks.

Pull requests or handoffs should list the exact gates run and call out any known
limitation that still violates these rules.

## Agent-Specific Instructions

Do not treat qmljs as an application framework. It is a binding with strict
ownership, error, async, and allocation contracts.

If the current implementation violates this file, fix the implementation or
mark the gap explicitly. Do not write docs that make fake behavior sound
production-ready.
