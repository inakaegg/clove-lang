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

- Concepts/usage -> [docs/](/docs/)
- Exact function signatures -> REPL `:doc` / `doc`

## Q. What does `x[0]` expand to?

It is reader sugar and is lowered roughly to `(clove.syntax::index-get x [0])`,
which then behaves like `get` / `get-in` at evaluation time.
Comma-separated forms like `x[1,2]` expand to `(clove.syntax::index-get-many ...)`.
If the target is a sequence, it behaves like `nth`; for maps/nested structures,
it behaves like multi-path `get-in` (different from `x[1 2]`).

See: [indexer](language/indexer.md)

## Q. Foreign blocks do not work

For security, the foreign engine is not loaded by default.
- REPL: set `:lang rb` / `:lang py`
- Run: consider `clove --auto-fallback` (fallback) or build-time `--embed-*`

See: [Interop](language/interop_foreign.md), [Build](tooling/build.md)

## Q. What are “special forms”?

Syntax that the evaluator treats specially (e.g., `if`, `let`, `fn`, `quote`, `try`, `async-scope`).

List: [Control flow](language/control_flow.md)

---
<!-- NAV:START -->
**Previous:** [How to write docs](contributing/docs_style.md)
**Next:** [Glossary](glossary.md)
<!-- NAV:END -->

