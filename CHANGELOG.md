# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased]

### Added

- `docs/design-notes/` records the reasoning behind Clove's distinctive design
  decisions — no macros, `::` instead of `/` as the namespace separator, what was
  not taken from Clojure, the two implementations, optional type annotations,
  `mut` / `imut`, and the notation borrowed from other languages. Each note states
  the decision, the reasoning, the alternatives that were dropped, and how to
  verify the behavior.
- `docs/phase2/DECISIONS.md` collects the specifications decided while building the
  native path, marking which of them are decided but not yet implemented.
- `AGENTS.md` documents the repository-specific rules for working here: the
  verification commands, the places an implementation must be updated together, and
  how to propose a change to a recorded design decision.
- `tools/` carries the developer scripts that run standalone in this repository:
  a namespace-alignment audit, a collection microbenchmark, and two builtin
  coverage reports for the native build path.

### Documentation

- Documented the measured limitations and defects of `clove build` in
  `docs/tooling/build.md`, separating current defects (self-recursive functions
  abort the build, zero- and four-or-more-parameter functions do not build, `-main`
  is never called, return-type annotations do not build) from features that are not
  implemented yet and from dynamic features that are unsupported by design.
- Added a Phase2 section to the documentation index; `docs/phase2/` was previously
  unreachable from it.
- Removed leaked generator-script fragments and a duplicated document from
  `docs/language/basics.ja.md` and `docs/language/interop_foreign.ja.md`.
- Corrected the `cargo install --git` command in both READMEs (`cargo install` has no
  `--package` flag).
- Corrected `clove build` documentation: it emits a native binary through the C backend
  at `target/clove/bin/<file-stem>`, and the legacy `--opt` / `--static` / `--embed-*`
  options no longer exist.
- Corrected `clove fmt` documentation: it writes to stdout and never overwrites files,
  foreign-block formatting is automatic with no `--lang` flag.
- Corrected concurrency documentation: `sleep` takes a duration (there is no `sleep-ms`),
  `select` takes a vector of cases, `async-scope` returns a scope handle, and `timeout`
  yields `nil`.
- Corrected argument type-hint syntax (`[x<Int>]`, not `[x :int]`), vector out-of-range
  call behavior, `core::pi` var access, regex literal escaping in the Japanese doc, and
  the `:keys` / `:as` notes in the map-shorthand doc.
- Replaced root-absolute Markdown links (`](/docs/...)`), which do not resolve on GitHub,
  with repository-relative links; Japanese pages now link to Japanese pages.
- Added English back-links to Japanese pages and wired `contributing/code_style` into the
  documentation index.
- Aligned example namespaces with their file paths so examples run without warnings.

### Fixed

- Prevented call environments from being retained by directly bound local closures and local
  functions.
- Shared lambda payloads across value clones to avoid repeatedly cloning function ASTs.
