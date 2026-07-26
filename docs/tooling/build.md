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
- `--main`
  - Call `(-main)` after the top-level forms. Without it the binary runs only the
    top-level forms, and defining `-main` without passing this prints a warning at
    build time. This mirrors the interpreter's `clove --main`.
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

The following was measured on 2026-07-26. Hitting one of these is not a new bug.

### 4.1 Recursion

Calls are compiled by expanding the function body at the call site, so a recursive
function cannot be compiled. The build reports it:

```clojure
(defn f [n] (if (< n 2) 1 (* n (f (dec n)))))
; => recursive function 'f' is not supported by the C backend yet (calls are inlined)
```

Mutual recursion is reported the same way. Rewrite the loop with
`(loop ... (recur ...))`, which does compile, or run the program with `clove app.clv`.

Fixed on 2026-07-26 (previously the build process aborted with a stack overflow, and
zero- or four-or-more-parameter functions and return-type annotations did not build at
all).

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
| Lazy sequences | Not supported; everything is eager |

### 4.3 Dynamic features (intentionally unsupported)

The whole program must be fixed at build time, so these are unavailable by
design:

- `eval`, `load-string`, `load-file`, `read-string`
- `set!`, `redef`, `with-redefs`, `with-dyn`
- Runtime namespace manipulation (`create-ns`, `resolve`, and friends)

### 4.4 Confirmed working

Top-level forms, functions with any number of parameters (including none),
`(-main)` via `--main`, return-type annotations, variadic parameters,
`loop` / `recur`, vector and map literals, `(range n)` / `(range 0 n)`, `reduce`, `take`,
`map`, `str`, and postfix argument type annotations (type mismatches fail the
build).

---
<!-- NAV:START -->
**Previous:** [Formatter (fmt / rubocop/syntax_tree)](formatter.md)
**Next:** [LSP (clove-lsp)](lsp.md)
<!-- NAV:END -->

