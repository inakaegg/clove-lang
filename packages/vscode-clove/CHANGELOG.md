# Change Log

All notable changes to this project will be documented in this file.

## [Unreleased]

* Enabled hover documentation and builtin symbol completion via the `clove-lsp`
  server (configurable through `clove.lsp.serverPath`).
* Completion candidates now show documentation/examples immediately—the VS Code
  suggest details pane is auto-opened by default (toggle via
  `clove.suggest.autoShowDocumentation`).
* LSP completions prioritize workspace functions so your `tmp/*.clv` helpers
  always appear alongside built-ins.
* Selection debug output no longer auto-opens on first activation; open it via
  "Clove: Show Selection Debug Log" when you need diagnostics.
* Selection expansion inside quoted strings now selects the inner content first,
  then the entire string literal (quotes included).
* Structural selection no longer treats single quotes as string delimiters, so
  forms like `(resolve 'foo)` expand correctly without swallowing the rest of
  the file.
* Selection expansion on namespace-qualified symbols now expands the local name
  first, then the fully qualified form (e.g. `create-canvas` → `sdl2::create-canvas`).
* Added automatic parameter-hint triggering when typing `(` or a space inside a
  call so signature help appears as you write forms like `(interleave ...)`.
* Go to Definition now resolves both workspace definitions and the generated
  stubs for Rust built-ins (e.g. `println`), making cross-file navigation work
  out of the box.

## [0.1.0] - 2024-XX-XX

* Initial scaffolding with syntax highlighting, language configuration, and structural selection commands.
