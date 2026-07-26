# No macros

Japanese version: [no-macros.ja.md](no-macros.ja.md)

- Updated: 2026-07-25

## Decision

Clove has no macro system. There is no `defmacro`, no syntax-quote (`` ` ``),
and no user-defined reader macros.

What macros would have covered is split into two fixed mechanisms:

- **Special forms** — syntax the evaluator knows directly. This includes forms
  that are macros in Clojure: `defn`, `let`, `when`, `cond`, `->`, and so on
- **Reader sugar** — notation written into the reader itself: `#(...)`, `#{...}`,
  `#/.../`, `$rb{...}`

Neither can be extended by user code. The set of syntax is fixed by the
implementation.

```clojure
(defmacro twice [x] (list 'do x x))
;; => Unbound symbol: 'defmacro'
```

## Reasoning

**Static analysis stays honest.** Without macros, the AST you read and the AST
that runs are nearly identical. Clove treats LSP support — completion,
go-to-definition, type hints, diagnostics — as a primary feature, and all of it
depends on deciding what a form means without expanding it. Macros would leave
both the LSP and the formatter unable to understand code without running user
expansion logic.

**The formatter does not break.** `clove fmt` reads and reformats source. In a
language where users can add syntax, a formatter cannot safely handle syntax it
has never seen.

**One mental model instead of two.** Readers never have to ask "is this a
function, a special form, or a macro?" There are only two categories.

**The native build path keeps its premise.** The
[native build path](two-phase-implementation.md) assumes the whole program is
known at build time. A mechanism that generates code at runtime breaks that
assumption.

## Alternatives not taken

**User-defined reader macros.** Scoping them to a single file was considered and
rejected. Load-order problems remain, and there is no good answer for how the
LSP and formatter would learn a given file's reader configuration. Tools outside
the implementation would always be one step behind.

**A Clojure-compatible macro system.** Adding `defmacro` would make Clojure code
easier to port, at the cost of everything above. Clojure compatibility is not a
goal ([what was not taken from Clojure](differences-from-clojure.md)).

**Leaving the door open for macros later.** "We don't need it now, but let's
reserve the mechanism" was also rejected — the moment the door is open, the LSP
and formatter must account for expansion. When syntax turns out to be missing, a
special form gets added instead. `when-let`, `cond->`, `some->`, `doto`, and
`with-open` all arrived that way.

## Consequences

Adding syntax means changing the implementation. That is the intended
constraint. The procedure for adding a special form is in
[AGENTS.md](../../AGENTS.md) — five places must be updated together (evaluator,
compiler, type inference, docs, and REPL completion).

`'quote` still exists. It is there for treating S-expressions as data, not for
macros.

## How to verify

```bash
clove -e '(defmacro f [x] x)'   # Unbound symbol: 'defmacro'
clove -e '(println `(1 2))'     # Unbound symbol: '`'
clove -e '(println #myTag{1})'  # Parse error: unknown reader tag: #myTag
clove -e "(println '(1 2))"     # => (1 2)
```
