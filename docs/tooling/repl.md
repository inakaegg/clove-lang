# REPL Guide

Japanese version: [repl.ja.md](repl.ja.md)

- Updated: 2026-01-14

Clove REPL is an interactive environment that integrates “eval + completion + doc display”.

## 1. Start

```bash
clove --repl
```

## 2. Main commands

- `:help` ... help
- `:env` / `:vars` ... current vars
- `:doc SYMBOL` ... docs
- `:load FILE` ... evaluate a file
- `:source PATH` ... set source name for inputs (important for error locations / LSP)
- `:lang TAG` ... set default foreign language for foreign blocks (e.g., `rb`, `py`)
- `:whereami` ... show current source
- `:backtrace` ... backtrace of last error
- `:continue` ... resume from error REPL
- `:q` ... quit
- `(nav QUERY ...)` ... unified search (name/docs), nicely rendered in REPL

## 3. `:source` vs `:load`

- `:source` sets “which file future inputs are from” **for display/location info**.
- `:load` actually reads and evaluates a file.

When using “Send Selection to REPL” from VS Code,
if `source` is misaligned you may see warnings and unstable LSP jumps.

## 4. dot-chain continuation (REPL helper)

In REPL, the **last evaluated value** is stored in `*repl-last*`.
If you write `.(...)` at top-level, it is implicitly interpreted as `*repl-last*.(...)`.

```clojure
(range 10)
.(take 3 ?)
```

A line-ending `.` is treated as a continuation.
The next line can be `.(...)` or a **dot-less** form like `map(inc)`.

```clojure
(range 10).
.(take 3 ?)
```

```clojure
(range 10).
map(inc)
```

## 5. doc / source

You can also use:

- `(doc 'symbol)`
- `(source 'symbol)`

## 6. nav / lookup (unified search)

`nav` (alias: `lookup`) searches **namespace / var / doc** together.

```clojure
(nav 'disj)                 ; search all
(nav "disj" :var)            ; vars only
(nav /take|drop/ :var :doc)  ; regex is OK
```

- `QUERY` accepts Symbol / String / Regex (`/.../` preferred; `#/.../` if ambiguous)
- `:ns :var :doc` can be specified (default is all)
- If too many results, it uses an **auto pager**
- Matches are highlighted, and badges like `[name,doc]` show match kinds

## 7. Error REPL (debug helper)

With `(use repl-on-error true)`, a REPL opens when an error occurs.
See: [Runtime settings](../advanced/runtime_settings.md)

In error REPL, these vars are available:

- `?` / `?v` / `*?` / `*1` ... target value (first arg)
- `*f` / `?f` ... called function
- `*args` / `?args` ... call args (vector)
- `*call` / `?call` ... call info (file/loc/form)
- `*e` / `?e` ... error string

`(repl x)` also sets `?` / `?v` / `*?` / `*1`.

## 8. Forms that open REPL

```clojure
(repl)         ; open REPL here
(repl value)   ; open with value bound to ? etc.
(debug)        ; alias of repl
(break)        ; simple breakpoint (debug REPL)
```

- `break` stops execution and opens REPL on the spot.
- Exit with `:q` / `:quit`.

---
<!-- NAV:START -->
**Previous:** [Language Basics](../language/basics.md)
**Next:** [Reader / Syntax Overview](../language/reader_syntax.md)
<!-- NAV:END -->

