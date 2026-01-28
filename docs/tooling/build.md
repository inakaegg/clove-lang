# Build (`clove build`)

Japanese version: [build.ja.md](build.ja.md)

- Updated: 2025-12-21

`clove build` generates a **Rust project** from a Clove file.

## 1. Usage

```bash
clove build path/to/app.clv
```

In the generated directory (e.g., `build/`), run:

```bash
cargo build --release
```

This produces an executable.

## 2. `--opt=typed`

`--opt=typed` enables “more static code generation” based on typed IR.

- Currently experimental; you may see compatibility warnings from `hm`.

## 3. Static link / embedding

- `--static` builds a statically linked binary (Linux musl).
- `--embed-ruby` / `--embed-python` embed the foreign engine.

> Constraints:
> - `--embed-*` currently requires `--static`.
> - Scripts with foreign blocks will error on a non-embed static build.

## 4. Native plugins

Generated binaries accept `--allow-native-plugins` / `--plugin-dir`.
Default is trusted dirs only; package plugins require lock sha256 match.
(Operational policy matters: e.g., lock to bundled plugins only.)

## 5. Common pitfalls

- `--main` changes whether `-main` is called
- foreign blocks require embed or proper engine setup

---
<!-- NAV:START -->
**Previous:** [Formatter (fmt / rubocop/syntax_tree)](formatter.md)
**Next:** [LSP (clove-lsp)](lsp.md)
<!-- NAV:END -->

