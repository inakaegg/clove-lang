# Build (`clove build`)

Japanese version: [build.ja.md](build.ja.md)

- Updated: 2026-07-25

`clove build` compiles a Clove source file with the current C backend and emits a native binary.

## 1. Usage

```bash
clove build path/to/app.clv
```

Default output:

```text
target/clove/bin/<file-stem>
```

## 2. Build options

```bash
clove build path/to/app.clv --out ./bin/app
clove build path/to/app.clv --emit-c
```

- `--out PATH`
  - Output binary path.
- `--emit-c`
  - Still builds the binary, but prints the generated C file path (`<out>.c`) instead of the binary path.
- `-o`, `--output`
  - Alias of `--out`.

## 3. Unsupported legacy options

The following legacy options are no longer available in `clove build`:

- `--opt`
- `--static`
- `--embed-ruby`
- `--embed-python`
- `--strict-types`
- `--emit-typed-ir`
- `--allow-native-plugins`
- `--plugin-dir`

Use `clove` runtime options for script execution, and `clove --help` / `clove build --help` for current CLI behavior.

## 4. Known limitations and defects

`clove build` handles only a **subset** of the language. When it meets an
unsupported feature it stops with a build error instead of falling back to the
interpreter ([design note](../design-notes/two-phase-implementation.md)).

The following was measured on 2026-07-25. Hitting one of these is not a new bug.

### 4.1 Defects (to be fixed)

| Symptom | Reproduction |
| --- | --- |
| **A self-recursive function makes the build abort with a stack overflow** | `(defn f [n] (if (< n 2) 1 (f (dec n))))` |
| **Zero-parameter functions do not build** | `(defn f [] 1)` → `lambda currently supports one to three params` |
| **Functions with four or more parameters do not build** | `(defn f [a b c d] a)` → same |
| **`-main` is never called** | `(defn -main [& args] ...)` builds, but the binary prints nothing. Calling `(-main)` explicitly gives `-main expects 1 arg` |
| **Return-type annotations do not build** | `(defn add :int [x<Int> y<Int>] ...)` → `syntax error: params must be a vector`. Postfix argument annotations (`x<Int>`) do work |

None of these affect script execution (`clove app.clv`). They are specific to the
native build path.

### 4.2 Not implemented yet (being added incrementally)

| Feature | Status |
| --- | --- |
| `#{...}` set literals | `set is not supported in typed IR yet` |
| `deftype` | `deftype is not supported in phase2 C backend yet` |
| `match` | `unsupported call in phase2 C build: match` |
| `mut` / `imut` | `unsupported call in phase2 C build: mut` |
| `atom` | `unsupported call in phase2 C build: atom` |
| `$rb{...}` / `$py{...}` foreign blocks | `foreign block is not supported in typed IR yet` |
| `(range n)` with one argument | `range currently expects 2 args`. `(range 0 n)` works |
| Lazy sequences | Not supported; everything is eager |

### 4.3 Dynamic features (intentionally unsupported)

The whole program must be fixed at build time, so these are unavailable by
design:

- `eval`, `load-string`, `load-file`, `read-string`
- `set!`, `redef`, `with-redefs`, `with-dyn`
- Runtime namespace manipulation (`create-ns`, `resolve`, and friends)

### 4.4 Confirmed working

Top-level forms, functions with one to three parameters, variadic parameters,
`loop` / `recur`, vector and map literals, `(range 0 n)`, `reduce`, `take`,
`map`, `str`, and postfix argument type annotations (type mismatches fail the
build).

---
<!-- NAV:START -->
**Previous:** [Formatter (fmt / rubocop/syntax_tree)](formatter.md)
**Next:** [LSP (clove-lsp)](lsp.md)
<!-- NAV:END -->

