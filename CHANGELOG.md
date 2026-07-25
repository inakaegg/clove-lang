# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased]

### Documentation

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
