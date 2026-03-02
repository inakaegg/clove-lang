# Phase2 Handoff (Entry)

Japanese version: [README.ja.md](README.ja.md)

This directory is the **single entry point for Phase2 specs, progress, and decisions**.
It is organized so that a Codex agent can operate by reading only this file.

---

## 1. What is Clove?

- An S-expression language in the Clojure family (a Lisp dialect)
- Aims to support both REPL/script usage and native builds (fast execution)
- The compiler/tooling implementation is in Rust, and native build output is generated through the C backend

---

## 2. What is Phase1?

- The current generation (existing Clove)
- Accumulated features caused performance and memory pressure
- Dynamic features are flexible for REPL/scripts but hard to optimize
- Main code: [crates/clove-core](/crates/clove-core), [crates/clove-lang](/crates/clove-lang), [crates/clove-lsp](/crates/clove-lsp)

---

## 3. What is Phase2?

- A **redesign / reimplementation** generation (native-first is the priority)
- Type-informed lowering/codegen through the C backend (native binary output)
- Dynamic features are allowed **only for development**; build-time is static
- `mut` is the default (for build). `imut` means observational immutability; `mut` requires in-place updates (error if shared)
- Main code: [crates/clove-build-core](/crates/clove-build-core), [crates/clove-build-front](/crates/clove-build-front), [crates/clove-build-backend-c](/crates/clove-build-backend-c), [crates/clove-build-runtime-c](/crates/clove-build-runtime-c)

---

## 4. Key Differences from Phase1

- Native-first (C backend path) is the top priority; Dynamic is intentionally reduced
- Avoid `Any`; use `Dyn` **only at boundaries**
- The same function can be optimized or not depending on `mut/imut`
- Native builds **forbid** dynamic eval/load and redefinition
- Require `def-foreign` to make external boundaries explicit
- Introduce strict / warn / allow build levels
- LSP diagnostics/completion/type display are assumed in the design

---

## 5. Current Status (Summary)

- `clove` supports `run` / `build` / `check` (build is integrated with the phase2 C backend path)
- CLI flags: `--native`, `--mode`, `--mut` can be overridden
- `time` / `bench` added to eval (return type follows the wrapped function)
- LSP: diagnostics + completion + hover + go-to-definition + symbol list (top-level)

Detailed tracking is omitted from public docs.

---

## 6. Public Layout

- Public information is **consolidated into README**
- Public benchmark is **one file** under [docs/phase2/bench/records/bench_release_20260127.md](/docs/phase2/bench/records/bench_release_20260127.md)

---

## 7. Core Commands

```bash
# Build/Run CLI
cargo install --path crates/clove-lang

# Run
clove path/to/a.clv

# Build (native)
clove build path/to/a.clv --out target/clove/bin/a

# Type check (strict/warn/allow)
clove check path/to/a.clv --native=strict

# LSP
cargo install --path crates/clove-lsp
```

Notes:

- clove defaults to **strict** (both CLI and LSP)
- `use native :warn` / `:allow` or `--native=warn` / `--native=allow` relaxes it

---

## 7.1 Mode Summary (run / REPL / build)

### Command Defaults (simplified)

Assume `--mode` is **not specified** in normal use.

| Command | mode | native | Notes |
| --- | --- | --- | --- |
| build | native | strict | strict only by default |
| run | native | strict / warn | warn is transitional |
| repl | dynamic | warn / allow | strict is not used |

Conclusion: **In practice, `--native=warn` for run is usually sufficient**.

### When You Must Specify Explicitly

Use `--mode/--native/--mut` only when required and follow these constraints:

- `mode=native` => `native=allow` is **invalid**
- `mode=dynamic` => `native=strict` is **invalid**

### `--mut`

- `soft` : error if shared (mut baseline)
- `hard` : allow destructive updates even if shared (design only for now)

### run vs REPL

- **run**: executes on the clove2 VM (typed opcode first + dynamic fallback)
  - `--mode/--native/--mut` apply directly
  - optimized for speed (target: at least Ruby)
- **REPL**: dynamic-first (flexible)
  - typed opcode only where possible
  - dynamic features always allowed

### Same Code, Different Behaviors

Sample:

```
(def m {:a 1})
(def n (get m :b)) ; nil is returned
(println (inc n))
```

| Command | Expected behavior |
| --- | --- |
| build (strict) | **Type error** (`inc` requires Number; `n` may be Nil) |
| run (strict) | **Type error** (same) |
| run (warn) | **Warning** (execution allowed, but runtime error possible) |
| repl (allow) | **No type error**, but **runtime error** for `inc nil` |

Dynamic example:

```
(eval "(+ 1 2)")
```

- `mode=native` : **Error** (dynamic features forbidden)
- `mode=dynamic` : **Allowed**

---

## 8. Work Rules (excerpt)

- See `AGENTS.md` in the repository root
