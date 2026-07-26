# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased]

### Performance

- Startup dropped from 514ms to 25ms for `clove examples/hello.clv`, and RSS from 38MB
  to 20MB. Importing a namespace symbol built a full builtin environment and threw it
  away (344 environments per runtime, now 22), the doc store forced itself to be built
  during startup and spun up a second runtime that evaluated the standard library again,
  and the embedded Ruby VM booted even for scripts that never touch Ruby.
- The evaluator is 2.8x faster on `loop`/`recur` (3M iterations: 25.2s to 8.9s) and 2.4x
  on `fib 25` (2.55s to 1.05s). The syntax feature toggles rebuilt a `HashMap` with six
  fresh strings on every check, every symbol resolution resolved the current namespace to
  look for an enum variant, `recur` (carried by an error variant) paid for a captured
  stack on each loop iteration, and the memory guard read the clock once per evaluated
  form.
- `Value` shrank from 96 to 40 bytes and `Result<Value, CloveError>` from 192 to 64, so a
  1M-element integer vector costs 60MB instead of 136MB, and the project's 2M-element
  benchmark 419MB instead of 979MB.
- `clove-lsp` `initialize` dropped from 4.85s to 0.38s (RSS 45MB to 30MB): generalization
  during type inference cloned the whole substitution and rebuilt every scheme in the type
  environment, which was quadratic in the number of definitions.

### Fixed

- Deep recursion reports a Clove error instead of crashing. The evaluator ran on the 8MB
  main thread and died with `SIGSEGV` at a depth of about 350 — an idiomatic recursive
  function over a 1000-element list — and because the embedded Ruby VM was already
  booted, Ruby's signal handler printed a `ruby ... [BUG] Segmentation fault` dump for a
  Clove program. Evaluation now runs on a thread with a configurable stack (`--stack`,
  512M by default), and a depth of 30000 works.
- The memory guard covers builtins that materialize a collection in one call. `(range 0
  1000000000)`, `(repeat 1000000000 1)`, and `(* "x" 1000000000)` used to allocate past
  the cap (measured 5.26GB against a 4GB cap) and could not be interrupted. The default
  cap is now 40% of installed memory (still capped at 4GB, floor 512MB), because taking
  4GB on an 8GB machine means swapping.
- `set::subset?` and `set::superset?` failed with `Unbound symbol: 'every?'`: the bundled
  set library referenced a function that only exists in the standard library, which loads
  afterwards and into another namespace.
- `clove build` handles functions with no parameters and with four or more, and accepts
  return-type annotations (`(defn add :int [x<Int> y<Int>] ...)`). Recursive functions are
  reported as unsupported instead of aborting the build process with a stack overflow.
  `(range n)` with one argument builds.

### Added

- `clove build --main` calls `(-main)` after the top-level forms, mirroring the
  interpreter's `clove --main`. Defining `-main` without passing it warns at build time;
  passing it without defining `-main` is an error. Native builds used to build such a
  program successfully and print nothing.
- `clove --stack SIZE` sets how much native stack the evaluator may use.

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

- Documented the measured limitations of `clove build` in `docs/tooling/build.md`,
  separating what the backend cannot compile today (recursion) from features that are not
  implemented yet and from dynamic features that are unsupported by design. The defects
  recorded there on 2026-07-25 have since been fixed; see Fixed above.
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
