# qmljs

OxCaml bindings to [quickjs-ng](https://github.com/quickjs-ng/quickjs): embed a
JavaScript runtime, move values across the boundary, call host functions from
JS, and drive promises with Eio.

## Why

- Embed QuickJS in OxCaml without treating the C API as the product surface.
- Keep ownership, domain affinity, and failure kinds explicit at the FFI
  boundary.
- Support Eio-backed async host callbacks and promise await without moving
  QuickJS off its owner domain.
- Keep the root package small; put the PPX and heavier adapters in separate
  packages.

Constraints:

| Constraint | Detail |
| --- | --- |
| Compiler | OxCaml `5.2.0+ox` only (`ocaml = 5.2.0`, `ocaml-variants = 5.2.0+ox`) |
| Runtime model | One QuickJS runtime per owner domain; values stay on that domain |
| Async | Eio for host work; one explicit job driver per runtime |
| Native dep | `quickjs-ng` via pkg-config (`-lqjs`) |

## Status

Pre-release. The public API is broad and covered by tests, but the package is
not opam-published and the opam metadata is incomplete on purpose.

| Item | Reality |
| --- | --- |
| Source | https://github.com/ribelo/qmljs |
| opam | Not published. `dune-project` sets `(generate_opam_files false)`. Hand-written `qmljs.opam` / `qmljs_ppx.opam` list depends and build only — no synopsis, description, homepage, license, or bug-reports |
| License | Unlicense (see `LICENSE`) |
| Packages | `qmljs` (library + C stubs), `qmljs_ppx` (ppx_rewriter, depends on `ppxlib`) |
| Runtime deps | `eio`, `unix`, system `quickjs-ng` |
| Test deps | `eio_main`, `qcheck` / `qcheck-core` |

## Quick start

Requires an OxCaml `5.2.0+ox` switch and `quickjs-ng` development headers.

```bash
# Nix shell (pins pkg-config + quickjs-ng.dev; uses OPAMROOT ~/.cache/opam)
nix develop .#oxcaml

# First-time OxCaml switch + deps (from the flake helper)
qmljs-oxcaml-init

# Build / test
nix develop .#oxcaml -c dune build
nix develop .#oxcaml -c dune runtest
```

Without Nix: create switch `5.2.0+ox` from the OxCaml opam repository, install
`quickjs-ng` for your OS, then `opam install . --deps-only --with-test`.

Mainline OCaml is not supported. Do not relax the `= 5.2.0+ox` pin.

### Minimal eval

```ocaml
let () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let value = Qmljs.Context.eval context "40 + 2" in
  assert (Qmljs.Value.to_int value = 42)
```

### Host function + promise await

```ocaml
let () =
  Eio_main.run @@ fun _env ->
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  Qmljs.Function.set_global context ~name:"add1" ~arity:1 (fun args ->
      Qmljs.Value.int context (Qmljs.Value.to_int args.(0) + 1));
  let result = Qmljs.Context.eval context "add1(41)" in
  assert (Qmljs.Value.to_int result = 42);

  Qmljs.Function.set_global_async context ~name:"later_add1" ~arity:1
    (fun args ->
      Eio.Fiber.yield ();
      Qmljs.Value.int context (Qmljs.Value.to_int args.(0) + 1));
  let promise = Qmljs.Context.eval context "later_add1(40).then(x => x + 1)" in
  let value = Qmljs.Promise.await ~yield_every:1 promise in
  assert (Qmljs.Value.to_int value = 42)
```

### Eio driver

```ocaml
let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Qmljs.Eio.with_runtime ~sw ~clock:(Eio.Stdenv.clock env)
    (fun driver _runtime context ->
      let promise =
        Qmljs.Eio.promise driver context (fun () ->
            Eio.Fiber.yield ();
            Qmljs.Value.int context 7)
      in
      match Qmljs.Promise.await_result ~yield_every:1 promise with
      | Ok value -> assert (Qmljs.Value.to_int value = 7)
      | Error error -> failwith (Qmljs.Error.message error))
```

### Modules and loader policy

```ocaml
let () =
  Qmljs.Runtime.with_ @@ fun runtime ->
  Qmljs.Context.with_ runtime @@ fun context ->
  let loader =
    Qmljs.Module.Loader.memory
      [ "math", "export const answer = 42;"
      ; "policy", "export default 'allowed';"
      ]
    |> Qmljs.Module.Loader.deny (fun specifier -> String.equal specifier "fs")
  in
  Qmljs.Runtime.set_module_loader runtime loader;
  let value =
    Qmljs.Context.eval_module context
      {|import { answer } from "math" with { type: "js" };
        export default answer;|}
  in
  assert (Qmljs.Module.default_export_to_int value = 42);
  match
    Qmljs.Error.capture (fun () ->
        ignore (Qmljs.Context.eval_module context {| import "fs"; |} : Qmljs.value))
  with
  | Error error ->
      assert (Qmljs.Error.is_policy_violation error);
      assert (String.equal (Qmljs.Error.denied_module_specifier_exn error) "fs")
  | Ok _ -> failwith "expected policy violation"
```

### PPX (`qmljs_ppx`)

Depend on both packages and preprocess with `(pps qmljs_ppx)`:

```ocaml
type profile =
  { id : int
  ; name : string
  ; tags : string list
  ; nickname : string option
  ; display_name : string [@qmljs.name "displayName"]
  }
[@@deriving qmljs]

let add : int -> int -> int = fun x y -> x + y [@@qmljs.global "add"]
let later : int -> int = fun x -> x + 1 [@@qmljs.global_async "later"]

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Qmljs.Eio.with_runtime ~sw ~clock:(Eio.Stdenv.clock env)
    (fun _driver _runtime context ->
      qmljs_register context;
      assert (Qmljs.Value.to_int (Qmljs.Context.eval context "add(20, 22)") = 42);
      match Qmljs.Promise.await_result (Qmljs.Context.eval context "later(41)") with
      | Ok value -> assert (Qmljs.Value.to_int value = 42)
      | Error error -> failwith (Qmljs.Error.message error))
```

Generated names for `type profile`: `qmljs_encode_profile`,
`qmljs_decode_profile`. For `type t`: `qmljs_encode` / `qmljs_decode`. Each
`[@@qmljs.global]` / `[@@qmljs.global_async]` export also gets
`qmljs_register_<name>`; `qmljs_register` registers every export seen in the
module.

Supported PPX v1 forms: `unit`, `bool`, `int`, `float`, `string`, `option`,
`list`, `array`, `result`, tuples, records, closed regular variants, and
same-module named types that also derive `qmljs`. Variants encode as
`{ tag, args }`.

## Public surface

Facade module: `Qmljs`. Implementation modules stay private; call through the
facade.

| Module | Responsibility |
| --- | --- |
| `Runtime` | create/close, memory limits, GC, interrupt flag, job drain hooks, module loader install |
| `Context` | eval / eval_file / eval_module, timeouts, globals, optional intrinsics |
| `Value` | primitives, kind tests, list/array/option/result/assoc codecs, `call` |
| `Object` / `Array` / `Atom` / `Symbol` / `Key` / `Proxy` | object and key surface |
| `Array_buffer` / `Typed_array` / `Bigint` / `Json` | binary and JSON |
| `Function` | sync host callbacks, async host callbacks, call/construct/defer |
| `Class` | OCaml-state classes exposed to JS (create, methods, accessors, constructor) |
| `Promise` / `Async` | promise state/result/await; `Async.drain_jobs` |
| `Eio` | `with_runtime`, `drain_until_idle`, `promise`, `cancel` |
| `Module` | ES modules, native modules, memory/filesystem/callback loaders, import attributes |
| `Error` | structured boundary failures |
| `Diagnostics` | native wrapper leak snapshots for tests |

Raw FFI (`Qmljs_raw`, `*_stubs.c`) is not part of the user-facing API.

## Safety / footguns

### Ownership and domains

- A runtime is permanently bound to the domain that creates it. Contexts and
  values from that runtime stay on the same owner domain.
- Crossing domains with a live runtime/context/value raises
  `Error.Wrong_domain` (or fails closed-handle checks). Only already-extracted
  immutable data may cross domains.
- `Runtime.request_interrupt` is the only intentional cross-domain escape hatch;
  it is atomic-only. Every other QuickJS entry point checks owner domain and
  closed-handle state first.
- Prefer `Runtime.with_` and `Context.with_`. Values that escape those brackets
  are closed with the scope and later use raises (closed-handle path). GC
  finalizers are backup; they must not call QuickJS directly and only enqueue
  native releases for the owner domain.

### Async

- There is one driver per runtime. `Promise.await`, `Async.drain_jobs`, timeout
  paths, and async host completion all share that driver's semantics.
- Synchronous host callbacks run under a C frame: do not perform Eio effects
  inside `Function.make` / `set_global`. Use `make_async` / `set_global_async`.
- While `eval_with_timeout` has a deadline armed, synchronous host callbacks are
  rejected before entry with a structured resource-limit error. Use async host
  functions for work that may block.
- Timeouts keep QuickJS on the owner domain (native monotonic deadline in the
  interrupt handler). Workers may only flip the interrupt atomic.

### Values and conversion

- `Value.int` rejects magnitudes above `Number.MAX_SAFE_INTEGER`
  (`2^53 - 1`); use `Bigint` for exact large integers.
- `Value.string` requires valid UTF-8; invalid UTF-8 raises
  `Invalid_argument`. Move arbitrary bytes through `Array_buffer` / typed
  arrays.
- Values belong to one context. Returning or passing a value from another
  context is a structured defect (`"QuickJS value belongs to a different
  context"`).

### Errors

- Failures crossing JS/C/OxCaml are structured (`Qmljs.Error.t`), not opaque
  strings. Kinds include `JavaScript`, `Host_callback_defect`,
  `Policy_violation`, `Out_of_memory`, `Stack_overflow`, `Resource_limit`,
  `Timeout`, `Closed_handle`, and `Wrong_domain`.
- Capture with `Error.capture`. Inspect `message`, `kind`, `metadata`, `cause`,
  `suppressed`, and JS vs OCaml frames.
- Legacy `exception Error of string` still exists as a fallback path; new code
  should raise/return structured errors (`Error.host`, conversion errors, etc.).
- No silent fallback: if a contract cannot be honored, the library fails with a
  specific structured error rather than inventing plausible data.

### Package boundary

- Root `qmljs` stays small: runtime/context/value bindings, structured errors,
  and the minimum Eio surface for the async API.
- Optional adapters, provider loaders, and framework integrations belong in
  separate packages when they add real dependencies or policy.
- `qmljs_ppx` is separate so applications that skip the PPX do not pull
  `ppxlib`.

## Design choices

| Choice | Why |
| --- | --- |
| OxCaml, not mainline OCaml | Ownership/modes and allocation control matter at the FFI boundary; pin is exact (`5.2.0+ox`) |
| Single-owner runtime + domain checks | QuickJS is not multi-domain safe; enforce it at every entry point |
| Scoped `with_` brackets | Deterministic teardown on success, JS failure, OCaml exception, and Eio cancellation |
| Structured errors built at the boundary | Preserve JS cause chains, OCaml frames, and policy metadata while live values still exist |
| One explicit async driver | Avoid competing job pumps and lost wakeups between await, drain, and host jobs |
| Separate `qmljs_ppx` package | Codecs/registration without making `ppxlib` a core runtime dependency |
| No silent fakes | Tests and production share the same contracts; red tests must not be greened with stubs |

Rejected or deferred:

- Handing live QuickJS handles across domains.
- Growing the root package into an application framework.
- Full bytecode module loading (not exposed yet).
- Inventing opam synopsis/homepage metadata only to silence `opam lint`.

## Current limits

- OxCaml-only; no mainline OCaml CI path.
- Not published to the public opam repository; install from source.
- Module loader covers in-memory ES modules, filesystem loader with escape
  checks, policy denial, `type: "js"`, and `type: "json"` import attributes.
  Full bytecode loading is not exposed.
- PPX v1 type coverage is listed above; open/polymorphic/GADT forms are out of
  scope for the current rewriter.
- `docs/quickjs-ng-binding.md` still says "Classes … are not exposed yet"; the
  public `Qmljs.Class` API and tests exist. Prefer the `.mli` over that sentence
  until the design note is updated.
- Allocation claims are only as strong as `bench/alloc.exe`. Documented zero-word
  paths today: `Runtime.interrupt_requested`, `Runtime.is_job_pending`,
  `Value.to_int`, `Promise.state`.

## Development

| Task | Command |
| --- | --- |
| Build | `nix develop .#oxcaml -c dune build` |
| Tests | `nix develop .#oxcaml -c dune runtest` |
| Alloc probe | `nix develop .#oxcaml -c dune exec bench/alloc.exe` |
| Flake check | `nix flake check --no-build` |

Layout:

| Path | Contents |
| --- | --- |
| `lib/` | Public library `qmljs` + C stubs |
| `ppx/` | `qmljs_ppx` rewriter |
| `test/` | Contract, readiness, PPX, property, and bug-hunt suites |
| `bench/` | Allocation probes |
| `docs/` | Binding and PPX design notes |
| `AGENTS.md` | Ownership, error, async, and package-boundary policy |

## Troubleshooting

| Symptom | Likely cause | Fix |
| --- | --- | --- |
| Switch/compiler errors on 5.x mainline | Not OxCaml | Use `5.2.0+ox` via the flake or OxCaml opam repo |
| Link failures on `-lqjs` | Missing quickjs-ng | Install `quickjs-ng` dev package; enter `nix develop .#oxcaml` |
| `Wrong_domain` / closed-handle errors | Handle used off owner domain or after `with_` | Keep all QuickJS work on the creating domain inside the bracket |
| Host callback cannot yield / blocks the runtime | Sync callback used for Eio or blocking work | Switch to `Function.make_async` / `set_global_async` and drive jobs |
| Timeout path rejects host callbacks | Deadline armed during `eval_with_timeout` | Do blocking work in async host functions outside the timed eval |
| PPX unknown | Only `qmljs` depended | Add `qmljs_ppx` and `(preprocess (pps qmljs_ppx))` |
| Policy violation on import | Loader `deny` or attribute/base conflict | Inspect `Error.denied_module_specifier_exn` and loader policy |

## Further reading

- `AGENTS.md` — non-negotiable ownership, error, async, and package rules
- `docs/quickjs-ng-binding.md` — binding shape, ownership, async model
- `docs/qmljs-ppx.md` — deriving and global export details
- `lib/qmljs.mli` — authoritative public API
