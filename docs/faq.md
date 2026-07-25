# FAQ

Japanese version: [faq.ja.md](faq.ja.md)

- Updated: 2025-12-21

## Q. Is it the same as Clojure?

Similar, but **not the same**.
Clove is inspired by Clojure, and extends the reader level with:
- dot-chain
- indexer
- foreign blocks
- OOP syntax

## Q. Where should I read docs?

- Concepts/usage -> [docs/](./)
- Exact function signatures -> REPL `:doc` / `doc`

## Q. What does `x[0]` expand to?

It is reader sugar and is lowered roughly to `(clove.syntax::index-get x [0])`,
which then behaves like `get` / `get-in` at evaluation time.
Comma-separated forms like `x[1,2]` expand to `(clove.syntax::index-get-many ...)`.
If the target is a sequence, it behaves like `nth`; for maps/nested structures,
it behaves like multi-path `get-in` (different from `x[1 2]`).

See: [indexer](language/indexer.md)

## Q. Foreign blocks do not work

With a default build of `clove`, `$rb{...}` / `$py{...}` run without any extra flag
(the `ruby` and `python` features are on by default). If they fail, check:

- **The CLI was built without those features** — rebuild without `--no-default-features`.
- **The Ruby / Python toolchain is missing.** The Ruby bridge needs Ruby 3.x; see the
  install notes in the repository README.
- **You are running `clove build`.** The native C backend does not support foreign
  blocks yet and reports `foreign block is not supported in typed IR yet`.
  Run such scripts with the interpreter instead.
- **The tag-less `${...}` picked the wrong language.** Set it with `:lang rb` / `:lang py`
  in the REPL, or `(use default-interop ...)` in a file.

See: [Interop](language/interop_foreign.md), [Build](tooling/build.md)

## Q. What are “special forms”?

Syntax that the evaluator treats specially (e.g., `if`, `let`, `fn`, `quote`, `try`, `async-scope`).

List: [Control flow](language/control_flow.md)

---
<!-- NAV:START -->
**Previous:** [Code style (release-facing)](contributing/code_style.md)
**Next:** [Glossary](glossary.md)
<!-- NAV:END -->

