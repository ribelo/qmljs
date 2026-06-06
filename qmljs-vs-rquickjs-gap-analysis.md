# qmljs vs rquickjs - Actionable Gap Backlog

**Date**: 2026-06-06  
**Purpose**: Keep only rquickjs-inspired gaps that fit qmljs as an
OCaml/OxCaml QuickJS binding. Rust-specific mechanisms, proc-macro ergonomics,
borrow-checker lifetime machinery, no_std support, and other implementation
details are intentionally removed.

qmljs should not try to become rquickjs in OCaml. The useful comparison is the
QuickJS behavior surface rquickjs exposes, filtered through qmljs's stricter
ownership, structured-error, Eio, and small-root-package contracts.

## Existing qmljs Surface To Preserve

qmljs already has enough surface that several original rows are not real gaps:

- Runtime/context/value scoped ownership with owner-domain checks.
- Structured errors with JS and OCaml frame information.
- Memory limit, stack limit, interrupt, timeout, promise-job limit, and recovery
  after failure.
- Promise creation, state/result inspection, await, and Eio-backed async host
  callbacks.
- Module namespace eval plus default/get/keys helpers.
- Memory and filesystem module loaders with deny/normalize hooks.
- Basic import attribute support for type: js and type: json.
- Object create/get/set/delete, string-key convenience, descriptors, and symbol
  keys.
- Primitive values, BigInt, JSON parse/stringify, arrays from lists, and
  Uint8Array copy/read/write support.
- Leak diagnostics at the qmljs wrapper layer.

The unchecked items below are the remaining work that fits the binding.

## 1. Host-Backed JS Objects And Classes

**Reference**: .reference/rquickjs/core/src/class.rs,
.reference/rquickjs/examples/class-methods/,
.reference/rquickjs/examples/exotic/

qmljs has no public API for host-backed JS objects with a QuickJS class id,
opaque OCaml payload, constructor, prototype, methods, accessors, finalizer, or
instanceof behavior.

Keep this as an OCaml API gap:

- [x] Define an OCaml-side class/object registration API backed by QuickJS
  JSClassDef.
- [x] Attach opaque OCaml state to JS objects without allowing it to cross runtime
  or domain ownership boundaries.
- [x] Support constructors, prototype methods, accessors, static properties, and
  instanceof.
- [x] Support custom finalization through the runtime's deferred owner-domain
  release path.

Do not port rquickjs's Rust machinery directly. JsClass, Trace, JsCell, borrow
markers, Readable/Writable, lifetime derives, and class proc macros are
Rust-specific solutions, not qmljs gaps. Exotic QuickJS class hooks are not a
root-package requirement unless qmljs later needs objects whose property
semantics cannot be expressed through ordinary classes, methods, accessors, and
Proxy values.

## 2. OCaml Conversion Helpers

**Reference**: .reference/rquickjs/core/src/value/convert.rs

qmljs exposes low-level constructors/extractors and a small typed conversion
layer for common OCaml values. Broader semantic round-trip coverage remains
tracked here.

Keep this as a focused qmljs gap:

- [x] Value.of_* / Value.to_* helpers for OCaml lists, arrays, options, results,
  association lists, and string-keyed object records.
- [x] Explicit non-coercing conversion by default, with separate named helpers for
  JavaScript coercion where qmljs intentionally exposes it.
- [x] Structured conversion failures using Qmljs.Error.t, not string exceptions.
- [x] Bigint.of_string treats input as data and does not splice it into
  executable JavaScript source.
- [x] Json.stringify rejects JavaScript undefined results instead of returning
  non-JSON text.
- [x] Public OCaml string keys and global symbol descriptions preserve embedded
  NUL bytes instead of silently truncating through C-string APIs.
- [x] Round-trip tests that match JavaScript semantics for null, undefined, arrays,
  objects, BigInt, typed arrays, and exceptions.

Do not copy rquickjs's trait catalog. FromJs, IntoJs, atom conversion traits,
Rust collection impls, Rc, Arc, Cell, RefCell, tuple impl families, and
IteratorJs are Rust API ergonomics. The OCaml version should be small and
explicit.

## 3. Module API

**Reference**: .reference/rquickjs/core/src/value/module.rs

qmljs can evaluate ES modules and inspect namespace exports, but it cannot yet
define native OCaml modules or expose the full QuickJS module lifecycle.

Keep these module gaps:

- [x] Native OCaml module definitions with explicit declaration and evaluation
  phases.
- [x] Export declaration and export value setting without source-string generation.
- [x] Dynamic import() from OCaml, with structured failure on loader rejection.
- [x] import.meta and namespace object access for evaluated modules.
- [x] Rejected top-level-await module evaluation frees eval promises before
  raising the structured rejection.
- [x] Native module evaluation failures preserve structured metadata, cause
  chains, and OCaml frame data.

Do not carry over rquickjs's module_init! macro or dynamic-library module
loading as qmljs root-package requirements. QuickJS bytecode read/write support
is intentionally out of scope for the root package until there is a concrete
consumer and a compatibility envelope that accounts for QuickJS version,
endianness, and bytecode safety.

## 4. Loader Composition And Import Attributes

**Reference**: .reference/rquickjs/core/src/loader.rs,
.reference/rquickjs/examples/import-attributes/

qmljs has memory and filesystem loaders plus deny/normalize hooks. The remaining
gap is not Rust-style trait composition; it is a more expressive OCaml loader
contract.

Keep these loader gaps:

- [x] A loader callback that receives base, normalized specifier, and parsed import
  attributes.
- [x] A clear distinction between "not found", "denied by policy", and "loader
  defect" in structured errors.
- [x] Explicit composition of memory, filesystem, native-OCaml-module, and fallback
  loaders.
- [x] Tests for relative resolution, root escape denial, JSON modules, unknown
  attributes, and repeated imports.
- [x] Filesystem loader scanning does not follow symlinks out of the configured
  root.
- [x] Repeated imports that normalize to the same module reject incompatible
  import attributes instead of reusing stale request metadata.
- [x] Loader callback failures preserve structured metadata, cause chains, and
  OCaml frame data.

Do not keep rquickjs tuple composition, PHF bundle loaders, compile-time bundle
macros, or native dynamic-library loading as qmljs gaps.

## 5. ArrayBuffer And Typed Arrays

**Reference**: .reference/rquickjs/core/src/value/array_buffer.rs,
.reference/rquickjs/core/src/value/typed_array.rs

Track binary-data coverage against the reference binding. The default qmljs
boundary should stay copy-based unless zero-copy ownership is specified
explicitly.

Keep these binary-data gaps:

- [x] ArrayBuffer creation from copied OCaml bytes/bigarrays.
- [x] ArrayBuffer readback as copied bytes.
- [x] ArrayBuffer.detach and detached-buffer error handling.
- [x] Typed arrays for the standard numeric families: Int8Array, Uint8Array,
  Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array,
  BigInt64Array, BigUint64Array, Float32Array, and Float64Array.
- [x] Typed array creation from ArrayBuffer with offset/length checks.
- [x] Access to a typed array's backing ArrayBuffer, byte offset, byte length, and
  element length.

Zero-copy external memory should not be the default. Add it only with explicit
ownership rules, owner-domain checks, finalization tests, and allocation
evidence.

## 6. Object, Property, And Array Surface

**Reference**: .reference/rquickjs/core/src/value/object.rs,
.reference/rquickjs/core/src/value/object/property.rs,
.reference/rquickjs/core/src/value/array.rs

qmljs has basic object operations, but the public object and array APIs still
miss common QuickJS behavior.

Keep these gaps:

- [x] Create objects with null or explicit prototypes.
- [x] Get and set prototypes, including cycle/error handling.
- [x] Define accessor properties with getter/setter functions.
- [x] Define and inspect complete property descriptors for string and symbol keys.
- [x] Enumerate own keys with filters for string keys, symbols, enumerable-only,
  and non-enumerable properties.
- [x] Iterate object key/value pairs and values.
- [x] Array create, length, get, set, push/append, and conversion to an OCaml list
  or array.
- [x] Array construction releases partially created JS arrays when element
  validation fails.

Do not model rquickjs iterator trait details such as exact-sized,
double-ended, or fused iterators. Those are Rust collection guarantees.

## 7. Function And Constructor API

**Reference**: .reference/rquickjs/core/src/value/function.rs,
.reference/rquickjs/core/src/value/function/args.rs,
.reference/rquickjs/core/src/value/function/types.rs

qmljs can create synchronous host callbacks and Eio-backed async globals. Track
the remaining host-function surface here.

Keep these gaps:

- [x] Host callback APIs that expose this, argument list, and constructor-call
  state.
- [x] Function call helpers that accept this.
- [x] Constructor calls from OCaml with new semantics.
- [x] Function metadata helpers for name, length, prototype, and constructor
  status.
- [x] Queue-microtask/defer helper if it maps to QuickJS jobs without bypassing the
  qmljs driver.
- [x] Async host callback failures should preserve structured host defects rather
  than reject with Printexc.to_string only.
- [x] Native argv temporaries are released when call, construct, defer, or value
  call argument validation fails.

Do not port rquickjs's Func, MutFn, OnceFn, Rest, This, Opt, IntoArg, or
IntoJsFunc traits. OCaml can expose these as explicit callback records or helper
functions when needed.

## 8. Promise And Eio Interop

**Reference**: .reference/rquickjs/core/src/value/promise.rs,
.reference/rquickjs/core/src/runtime/async.rs

qmljs intentionally uses Eio as its async integration point. Generic Rust
executor support is not a qmljs gap.

Keep these Eio-shaped gaps:

- [x] A direct helper to wrap an Eio fiber/job as a JS Promise without first
  installing a global host function.
- [x] Cancellation propagation from the Eio switch to queued host work.
- [x] Tests proving queued host jobs cannot outlive the switch/scope that created
  them.
- [x] Structured rejection for OCaml host defects.
- [x] Structured rejection for Eio cancellation.

Do not keep AsyncRuntime, AsyncContext, Future, Promised<T>, MaybePromise<T>,
generic scheduler, or Rust Waker plumbing as qmljs gaps.

## 9. Iterables

**Reference**: .reference/rquickjs/core/src/value/iterable.rs

qmljs lacks explicit helpers for JavaScript iterator protocol interop.

Keep these gaps:

- [x] Create a JS iterable from an OCaml step function.
- [x] Consume any JS iterable or iterator from OCaml with structured failure on
  protocol violations.
- [x] Expose well-known Symbol.iterator through the public Symbol/Key surface.
- [x] Ensure iteration keeps QuickJS values on the owner domain and respects
  context/runtime lifetimes.

Do not copy Rust iterator adapter types directly.

## 10. Proxy API

**Reference**: .reference/rquickjs/core/src/value/proxy.rs

qmljs has no public Proxy helpers.

Keep these gaps:

- [x] Create a Proxy from target and handler values.
- [x] Inspect target and handler when QuickJS exposes them safely.

OCaml trap callbacks do not need a separate root API right now: callers can
construct a handler object with existing Object and Function APIs and pass it to
Proxy.create.

## 11. Atom And Well-Known Symbol Keys

**Reference**: .reference/rquickjs/core/src/value/atom.rs,
.reference/rquickjs/core/src/value/atom/predefined.rs

qmljs uses strings and symbol values for property keys. It does not expose
QuickJS atoms or the full well-known symbol set.

Keep these gaps:

- [x] Interned atom/property-key API for repeated property access.
- [x] Predefined atoms for common names such as length, prototype, then, catch,
  name, value, and done.
- [x] Well-known symbols such as iterator, toStringTag, and hasInstance.
- [x] Correct atom lifetime management through the owner-domain release queue.

## 12. Runtime Hooks And Diagnostics

**Reference**: .reference/rquickjs/core/src/runtime/base.rs

qmljs has limits, interrupts, and wrapper leak diagnostics. It still lacks some
runtime-level QuickJS controls and observability.

Keep these runtime gaps:

- [x] QuickJS memory usage snapshot.
- [x] Manual GC run and configurable GC threshold.
- [x] Runtime info string.
- [x] Promise rejection tracker and promise lifecycle hooks, if they can report
  structured errors without violating the driver model.
- [x] Runtime.with_ and Context.with_ report cleanup failures, and preserve body
  failures with cleanup failures attached as suppressed structured data.

Do not keep custom Rust allocators or Rust global allocator integration as a
current qmljs requirement. Add allocator customization only if there is a
measured need and a clear OCaml ownership story. QuickJS dump flags stay out of
the public API unless the linked QuickJS build exposes reliable, observable
development diagnostics; production qmljs behavior must not depend on them.

## 13. Context Creation And Eval Options

**Reference**: .reference/rquickjs/core/src/context/builder.rs,
.reference/rquickjs/core/src/context/ctx.rs

qmljs creates normal QuickJS contexts and exposes partial eval flags.

Keep these context gaps:

- [x] Minimal/raw context creation with selected QuickJS intrinsics.
- [x] Explicit intrinsics for eval, Promise, typed arrays, BigInt, JSON, and
  other built-ins when QuickJS supports separate installation. Module eval is
  covered for raw contexts because QuickJS exposes modules through eval flags
  rather than a separate intrinsic.
- [x] Full eval option coverage, including backtrace barriers and clear
  top-level-await behavior.
- [x] Successful timeout-wrapped eval does not drain pending promise jobs or
  microtasks before an explicit driver/drain call.
- [x] Timeout-wrapped eval preserves the original structured error as a cause
  when an interrupt flag and JS exception are both present.
- [x] User JavaScript error messages are not reclassified as resource or timeout
  failures by string matching.
- [x] Throwing error-property getters do not poison the context while qmljs
  structures the original JavaScript error.
- [x] Module/file eval helpers only where they add structured errors or ownership
  behavior beyond ordinary OCaml file reading.

Do not port rquickjs's builder traits or tuple-based intrinsic composition.

## Priority Order

| Priority | Gap |
|---|---|
| P0 | Object/property/array surface needed by ordinary JS interop |
| P0 | Function/constructor callback shape, including structured async failures |
| P0 | ArrayBuffer and typed array family |
| P1 | Native OCaml modules and loader contract improvements |
| P1 | Host-backed JS objects/classes |
| P1 | OCaml conversion helpers |
| P1 | Promise/Eio cancellation and direct Promise wrapping |
| P2 | Context intrinsics and full eval options |
| P2 | Runtime hooks and diagnostics |
| P2 | Iterables |
| P3 | Atom/well-known symbol API |
| P3 | Proxy helpers |
