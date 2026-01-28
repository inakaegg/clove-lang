# Repository layout

Japanese version: [repo_layout.ja.md](repo_layout.ja.md)

- Updated: 2026-01-14

Clove is a workspace containing multiple crates and assets.

- [crates/clove-core](/crates/clove-core) ... core of Reader/Eval/standard functions
- [crates/clove-lang](/crates/clove-lang) ... CLI / REPL / build / fmt / plugins
- [crates/clove-lsp](/crates/clove-lsp) ... LSP server
- [data/clove_docs/](/data/clove_docs/) ... docs data (source for function docs)
- [docs/](/docs/) ... this documentation
- [examples/](/examples/) ... examples

## Doc precedence

Docs are roughly displayed in the following priority order:

1. Rust-side `fn_meta` registration
2. [data/clove_docs/clove-docs.json](/data/clove_docs/clove-docs.json)
3. runtime docstring

(Related to REPL completion / `:doc` display.)

---
<!-- NAV:START -->
**Previous:** [Dev environment setup](dev_setup.md)
**Next:** [Testing policy](testing.md)
<!-- NAV:END -->

