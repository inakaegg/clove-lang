# About docs/

Japanese version: [README.ja.md](README.ja.md)

This directory collects documentation for **the Clove language and tools**.

- Updated: 2026-01-27
- Principle: **match the implementation** (drafts/unimplemented items must be marked as planned)

## Structure

- [docs/index.md](/docs/index.md) ... Table of contents (entry)
- [docs/getting_started.md](/docs/getting_started.md) ... Quick start
- [docs/language/](/docs/language/) ... Language specs (syntax/reader/eval/sugar)
- [docs/tooling/](/docs/tooling/) ... CLI / REPL / fmt / build / LSP / VS Code
- [docs/advanced/](/docs/advanced/) ... Design notes, future AOT, etc.
- [docs/contributing/](/docs/contributing/) ... Development/contribution guide

## Documentation Policy

1. "Works now" is the priority. Do not include examples that do not run.
2. Draft specs must be isolated as **Planned** sections.
3. Examples should be REPL-friendly (copy/paste and run).
4. Large spec changes (syntax/namespace) must update:
   - [docs/index.md](/docs/index.md) TOC
   - compatibility/migration sections on impacted pages

## Relation to Generated Docs

- API-level details are owned by REPL `:doc` / `doc`.
- [docs/](/docs/) focuses on concepts, design intent, pitfalls, and usage examples.
