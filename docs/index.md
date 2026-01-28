# Clove Documentation

Japanese version: [index.ja.md](index.ja.md)

- Updated: 2026-01-14

Clove is based on a Clojure-style Lisp, and adds more implementation-oriented features such as
**dot-chain / indexer / foreign blocks / OOP syntax**.

## Start Here

- [Getting Started](getting_started.md)
- [Language Basics](language/basics.md)
- [REPL Guide](tooling/repl.md)

## Language

- Basics
  - [Reader / Syntax Overview](language/reader_syntax.md)
  - [Literals (numbers, strings, etc.)](language/literals.md)
  - [Collections (list/vector/map/set)](language/collections.md)
  - [Functions (defn / fn / #() / variadic)](language/functions.md)
  - [Destructuring](language/destructuring.md)
  - [Control flow (if/when/cond/for/while/try, etc.)](language/control_flow.md)
  - [Threading / pipeline forms (-> / cond-> / some->)](language/threading.md)
  - [Namespaces (ns/require/resolve/load, etc.)](language/namespaces.md)

- Syntax sugar
  - [indexer: `x[0]`, `x[0 1]`, `x[0,1]`, `x[0||d]`, `a..b`](language/indexer.md)
  - [dot-chain: `x.(f ?)`, `x.(f *?)`, etc.](language/dot_chain.md)
  - [OOP syntax (`obj.method(...)`)](language/oop_syntax.md)
  - [OOP methods (method / where / self)](language/oop_methods.md)
  - [OOP methods quick reference (new syntax)](language/oop_methods_quickref.md)
  - [Map shorthand (JS-style)](language/map_shorthand.md)
  - [Regex / Duration literals](language/regex_duration.md)

- Core features
  - [Concurrency / async (chan / future / go-loop / scope-loop / async-scope)](language/concurrency.md)
  - [Interop (foreign blocks / Ruby / Python)](language/interop_foreign.md)
  - [Types/enum/match (deftype/defenum/match)](language/types_enum_match.md)
  - [Standard library `std`](language/stdlib.md)

## Tooling

- [CLI (clove / clove fmt / clove build)](tooling/cli.md)
- [Package management (Phase1)](packages.md)
- [Formatter (fmt / rubocop/syntax_tree)](tooling/formatter.md)
- [Build (--opt=typed / --static / embed)](tooling/build.md)
- [LSP (clove-lsp)](tooling/lsp.md)
- [VS Code Extension](tooling/vscode.md)
- [Troubleshooting](tooling/troubleshooting.md)

## Examples

- [SDL2 examples](../examples/sdl2/README.md)

## Advanced Topics

- [Types (design, type hints, typed build assumptions)](advanced/typing.md)
- [Namespace design notes](advanced/namespaces_design.md)
- [Memoization and persistent cache (memo / memoize)](advanced/memoization.md)
- [Runtime settings (use / use-syntax)](advanced/runtime_settings.md)

## Development / Contributing

- [Dev environment setup](contributing/dev_setup.md)
- [Repository layout](contributing/repo_layout.md)
- [Testing policy](contributing/testing.md)
- [How to write docs](contributing/docs_style.md)

## FAQ / Glossary

- [FAQ](faq.md)
- [Glossary](glossary.md)
