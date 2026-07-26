# Two implementations: interpreter and native build

Japanese version: [two-phase-implementation.ja.md](two-phase-implementation.ja.md)

- Updated: 2026-07-25

## Decision

Clove carries **two implementations** of the same language.

| Path | Crates | Used for |
| --- | --- | --- |
| Interpreter (phase1) | `clove-core` | REPL, script execution, LSP |
| Native build (phase2) | `clove-build-core`, `clove-build-front`, `clove-build-backend-c`, `clove-build-runtime-c` | Standalone binaries via `clove build` |

The readers and the type machinery are separate implementations. The native path
handles a **subset** of the language and **stops with an error** when it meets
syntax or functions it does not support.

## Reasoning

**Performance and memory were the real motivation.** In the interpreter, data
construction and function calls are expensive, and even simple applications
reached gigabytes of memory. Improving the existing implementation was judged
insufficient, so a separate path that can optimize against type information was
built.

**The interpreter was kept because its job is different.** The REPL, the LSP,
and script execution need dynamic flexibility, which is incompatible with the
native build's requirement that the whole program be known ahead of time.
Collapsing to either one degrades the other.

**No silent fallback.** When the native build meets an unsupported feature, it
could have fallen back to the interpreter and kept running. That was rejected:
it produces "I built natively but it isn't fast" with no visible cause.
Unsupported stays unsupported, and it fails at build time.

```bash
clove build app.clv
# lower error: set is not supported in typed IR yet
# unsupported call in phase2 C build: eval
# lower error: foreign block is not supported in typed IR yet
```

## What the native path forbids

To keep the premise that the whole program is fixed at build time, dynamic
features are unavailable:

- `eval`, `load-string`, `load-file`, `read-string`
- `set!`, `redef`, `with-redefs`, `with-dyn`
- Runtime namespace manipulation (`create-ns`, `resolve`, and friends)
- `require` is resolved statically at build time

This is a restriction, but it points the same direction as the decision to have
[no macros](no-macros.md). If code cannot change at runtime, all of it is
visible at build time.

## Current coverage

**The native path still handles only part of the language.** As verified on
2026-07-25, `clove build` accepts top-level forms and function definitions with
one to three parameters.

| Example | Result |
| --- | --- |
| `(println "hi")` | Builds |
| `(defn f [a] a)` | Builds |
| `(defn f [] 1)` / `(defn f [a b c d] a)` | `lambda currently supports one to three params` |
| `(defn -main [] ...)` | Same, and the produced binary does not call `-main` |
| `#{1 2}` (set literal) | `set is not supported in typed IR yet` |
| `$rb{...}` (foreign block) | `foreign block is not supported in typed IR yet` |
| Self-recursive functions | The build itself aborts with a stack overflow (known defect) |

Coverage is moving. See [docs/phase2/](../phase2/README.md) for the current
state.

## The cost of two implementations

**Specifications drift.** On 2026-07-25 the native reader was found to still
accept the [`/` separator that had been removed](namespace-separator.md). The
reader was written two and a half months after the decision and never picked it
up.

Drift of this kind arrives as "one side got fixed and nobody noticed". The
countermeasure is a test that runs the same corpus through both readers and
asserts that **the native reader is never more permissive than the
interpreter's**.

```
crates/clove-lang/tests/reader_parity.rs
  phase2_is_never_more_permissive_than_phase1
```

The native path being a subset of the interpreter is by design, so only one
direction is forbidden. The other direction — native accepts what the
interpreter rejects — means the native path implements syntax that is not part
of the language, and the test fails.

## See also

- [Phase2 native build path](../phase2/README.md)
- [clove build](../tooling/build.md)
