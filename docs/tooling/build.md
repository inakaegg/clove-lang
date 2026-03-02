# Build (`clove build`)

Japanese version: [build.ja.md](build.ja.md)

- Updated: 2026-03-02

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

---
<!-- NAV:START -->
**Previous:** [Formatter (fmt / rubocop/syntax_tree)](formatter.md)
**Next:** [LSP (clove-lsp)](lsp.md)
<!-- NAV:END -->

