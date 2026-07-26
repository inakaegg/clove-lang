# No `/` as a namespace separator

Japanese version: [namespace-separator.ja.md](namespace-separator.ja.md)

- Updated: 2026-07-25

## Decision

The namespace separator is `::`. `/` cannot be used as one. Both `foo/bar` and
`:foo/bar` are **parse errors**.

```bash
clove -e '(println :foo/bar)'
# Parse error: namespace separator '/' has been removed; use '::' (e.g. foo::bar).
#              If you meant a regex literal, use #/.../
```

The single exception is a bare `/`, which remains the division operator.

```clojure
(println (/ 6 2))            ; => 3
(println string::upper-case) ; namespaced reference
```

Additionally, **keywords carry no namespace**. `:foo::bar` is not "`bar` in the
`foo` namespace"; it is a keyword whose name is literally `"foo::bar"`.

```bash
clove -e '(println (name :foo::bar))'       # => foo::bar
clove -e '(println (namespace :foo::bar))'  # Unbound symbol: 'namespace'
```

There is no `namespace` function, because there is no notion of extracting a
namespace from a keyword.

## Reasoning

**`/` collides with the operator.** In a Lisp, `/` is division. Using the same
character as a separator forces the reader to decide from context whether it is
part of a symbol or an operator. Clove also has regex literals of the form
`/.../`, which widens the collision further.

**`::` matches other languages.** Rust and C++ use `::`, and Ruby uses it for
constant lookup. Clove borrows notation from Ruby and Rust in several places
([borrowed notation](borrowed-syntax.md)), so the separator follows suit.

**Clojure's namespaced keywords are hard to work with.** In Clojure, `::foo`
means the keyword itself carries a namespace, so the same `:name` can be a
different value depending on context. Keywords used as data are simpler for both
humans and tools when they are globally identical. In Clove, keywords are value
labels, and namespaces belong to **symbols**.

## Alternatives not taken

**Keeping `/` as a migration aid for Clojure users.** This was considered and
rejected. Accepting both forms leaves the reader ambiguity unresolved, so every
reason above would still apply. Erroring out with "use `::`" is also the faster
migration path. The error message points at both the `::` form and the regex
literal.

**Giving keywords a namespace.** Qualifying as `:my.app/status` to avoid
collisions was dropped. Keywords are mostly map keys, and identical-looking keys
that mean different things in different contexts cause more problems than they
solve. When names collide, a longer keyword is enough.

## Consequences

The decision was finalized on 2025-12-17, and the reader, name resolution,
display, documentation, LSP, stdlib, and builtin registration names were all
unified on `::`. `process/sh` became `process::sh`, `json/parse` became
`json::parse`.

On 2026-07-25 it turned out that the reader on the
[native build path](two-phase-implementation.md) had never picked up this
decision and still accepted `:foo/bar`. It has been fixed. To prevent
recurrence, a test now runs the same corpus through both readers and asserts
that **the native reader is never more permissive than the interpreter's**
(`crates/clove-lang/tests/reader_parity.rs`).

## See also

- [Namespaces](../language/namespaces.md) — the current specification
- [Namespace design notes](../advanced/namespaces_design.md) — `ns` and `require`
