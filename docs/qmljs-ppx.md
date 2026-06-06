# qmljs PPX

`qmljs_ppx` is the high-level bridge for ordinary qmljs use. It generates
OCaml-to-QuickJS value codecs and registration functions for OCaml functions
that should be callable from JavaScript.

Keep the PPX in a separate package from `qmljs`: applications depend on both
`qmljs` and `qmljs_ppx`, while the root binding stays free of a `ppxlib`
runtime dependency.

## Derived Codecs

Use `[@@deriving qmljs]` on records, tuples, and closed regular variants:

```ocaml
type user =
  { id : int
  ; name : string
  ; tags : string list
  ; nickname : string option
  ; display_name : string [@qmljs.name "displayName"]
  }
[@@deriving qmljs]
```

For `type user`, the PPX emits:

```ocaml
val qmljs_encode_user : Qmljs.Context.t -> user -> Qmljs.Value.t
val qmljs_decode_user : Qmljs.Value.t -> (user, Qmljs.Error.t) result
```

For `type t`, the emitted names are `qmljs_encode` and `qmljs_decode`.

Supported v1 type forms are `unit`, `bool`, `int`, `float`, `string`, `option`,
`list`, `array`, `result`, tuples, records, closed regular variants, and
same-module named types that also derive `qmljs`.

Records encode as JavaScript objects. Variants encode as:

```js
{ tag: "Constructor", args: [ ... ] }
```

## Function Exports

Use `[@@qmljs.global]` or `[@@qmljs.global_async]` on a fully annotated
function:

```ocaml
let add (x : int) (y : int) : int = x + y [@@qmljs.global "add"]

let later : int -> int =
  fun x -> x + 1 [@@qmljs.global_async "later"]
```

The PPX emits one registration function per export:

```ocaml
val qmljs_register_add : Qmljs.Context.t -> unit
val qmljs_register_later : Qmljs.Context.t -> unit
```

It also emits `qmljs_register : Qmljs.Context.t -> unit`, which registers all
exports seen so far in the module.

Generated host callbacks decode JS arguments with the derived/public decoders.
Decode failures raise structured `Qmljs.Error.Host_callback_defect` errors with
`function` and `argument` metadata, so QuickJS sees a JavaScript exception rather
than an OCaml exception escaping through the C callback boundary.
