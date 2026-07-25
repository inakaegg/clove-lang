# Phase2 decisions

Japanese version: [DECISIONS.ja.md](DECISIONS.ja.md)

- Updated: 2026-07-25

A record of what was decided while designing and implementing the native build
path (Phase2). The higher-level reason for having two implementations at all is
in the [design note](../design-notes/two-phase-implementation.md); this document
holds the individual specifications decided on top of it.

> **Decided is not the same as implemented.** Several entries below are decided
> but not yet implemented in `clove build`. For what actually builds today, see
> [known limitations and defects](../tooling/build.md#4-known-limitations-and-defects).

---

## Evaluation: eager collections (vector only)

- `cons`, `vec`, and `into` currently support **`Vec` only** (extending to Seq,
  Map, and Str comes later)
- `repeat` and `repeatedly` are **finite only** (a `count` is required) and
  return a `Vec`
- `take` and `take-while` handle the **generator forms** of `range`, `repeat`,
  `repeatedly`, and `iterate` eagerly
- With two arguments, `iterate` provisionally generates 1024 elements eagerly
  (specifying `count` is recommended)

Lazy sequences are not supported on the native path.

### Future: lazy sequences for IO

Phase2 proceeds eagerly overall, but IO-oriented sequences where it matters for
large inputs — `io::line-seq` and similar — are planned to use a LazySeq built
on Rust's `Iterator`. Resource release and evaluation timing are a separate task.

---

## `mut` / `imut` and in-place updates

The language default is persistent data structures
([design note](../design-notes/mutability.md)). In a native build, the two are
treated as an **optimization contract**.

- `imut` is immutable as observed
  - In-place optimization is permitted only where sharing can be proven absent
  - Where sharing exists or cannot be proven absent, a new value is always
    returned
- `mut` **requires** in-place update
  - Where sharing exists or cannot be proven absent, the **build fails**
  - It never quietly returns a new value instead
- The `mut` / `imut` boundary **cannot be switched at runtime** in a native build
- An explicit way to break sharing is provided
  - It allocates a fresh container and shallow-copies the elements
  - `vec` / `into` / `map` return new values without guaranteeing the absence of
    sharing; the two are kept distinct

**Not implemented.** The current C backend does not accept `mut` at all
(`unsupported call in phase2 C build: mut`).

---

## Dynamic features forbidden in native builds

Because the whole program must be fixed at build time, the following are
rejected:

- Dynamic evaluation and loading: `eval`, `load-string`, `load-file`,
  `read-string`
- Runtime redefinition and dynamic vars: `set!`, `redef`, `with-redefs`,
  `with-dyn`
- Runtime namespace manipulation: `current-ns`, `create-ns`, `resolve`, and
  friends
- `require` and `require-native` are **resolved statically at build time**
- Native artifacts neither embed source nor run `eval`

The REPL and script execution (Dynamic) continue to allow all of these.

### Replacement for string parsing

`read-string` is not used natively. Use the existing conversion functions
instead — `int`, `float`, `str`, `bool` — including their string forms.

---

## Predicate semantics

- `coll?` covers `Vec`, `Map`, and `Str`
- `sequential?` covers `Vec` only

---

## Provisional type decisions

### Return types of higher-order builders

`constantly`, `partial`, `comp`, and `juxt` return a **function type containing
`Any`**. Strict mode reports the presence of `Any`, so this is to be redesigned
once generics land.

### Functions that may return `nil`

Where a function's specification allows `nil`, it is treated as **`T?`**. The
map and path family — `get`, `get-in`, `update`, `update-in`, `assoc-in` — are
the representative cases. Passing a `T?` to a non-optional function is a type
error under strict mode.

The default native level is **strict**, for both the CLI and the LSP.

### `def-foreign`

Declarations are permitted, but calls raise a **not-implemented error**.
Foreign-language interop under native execution is a separate task.

---

## Printing

`println` follows the **same display rules as the interpreter**: top-level
strings unquoted, strings inside collections quoted.

---

## Performance acceptance criteria

- Must be **faster and lighter than Clojure**
- The goal is to approach Go and Rust
- Falling below Clojure is **reported as a failure**
- Each implementation step runs the per-language benchmarks in
  [`docs/phase2/bench/`](bench/README.md) for comparison
- **Both time and memory** are compared (max RSS recorded in human-readable MiB)

Working rules for measurement and optimization are in
[AGENTS.md](../../AGENTS.md).

---

## Backend for the run path

- `clove --vm` runs on a lightweight VM specific to the native build path; the
  interpreter's VM is not reused
- The VM is **typed-opcode centric**, minimizing dependence on `Value`
- `clove build` continues to use native codegen; the VM is for `run` only
- `run` prefers typed opcodes while permitting a dynamic fallback
- The REPL prefers dynamic execution, applying typed opcodes only where they
  optimize
- `Value` is not used on the build hot path

`Value` is limited to:

- REPL, `eval`, dynamic loading
- Plugins, foreign calls, dynamic fallback
- Dynamic features that do not fit the typed IR

---

## Design review (2026-03-07)

- The root problem is not the backend's implementation language but the **split
  between frontend, types, lowering, and runtime**
- `clove-build-core` is the source of truth; the reader, syntax rewriting, type
  inference, and typed-IR lowering are consolidated there
- `clove-build-front` is a **provisional adapter** and will eventually be removed
- `clove-build-backend-c` moves to receiving the **typed IR** rather than a
  `FrontProgram`
- `run` also converges on **typed IR → bytecode** as the canonical path
- The decision to switch to the C backend stands (no return to the Rust backend)
- The REPL may keep using the interpreter's runtime for now, but shares the
  reader, syntax, and type information with the native build

**Clove is implemented as one language, one frontend, and two
backends/runtimes.**

### Priority

Passing `build` > `run` (VM) > LSP / REPL. If `build` is failing, improving
`build` takes priority and VM work waits.

---

## See also

- [Two implementations](../design-notes/two-phase-implementation.md) — why there are two
- [`mut` and `imut`](../design-notes/mutability.md)
- [Known limitations and defects](../tooling/build.md#4-known-limitations-and-defects) — measured current state
- [Phase2 design and current status](README.md)
