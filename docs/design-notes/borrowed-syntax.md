# Borrowed notation

Japanese version: [borrowed-syntax.ja.md](borrowed-syntax.ja.md)

- Updated: 2026-07-25

## Decision

Clove keeps S-expressions as its base but borrows notation from Ruby,
JavaScript, and Rust. Four things are borrowed.

```bash
clove -e '(def xs [10 20 30]) (println xs[0] xs[-1] xs[0...2] xs[9 || :none])'
# 10 30 [10 20] :none

clove -e '(println (range 5).(filter even? ?).(map inc ?))'
# [1 3 5]

clove -e '(println 1.inc "abc".upper-case)'
# 2 ABC

clove -e '(println {name: "a" age: 1})'
# {:name "a" :age 1}
```

| Notation | Borrowed from | Purpose |
| --- | --- | --- |
| Indexer `xs[0]` | Ruby / JS | Short subscript access |
| Dot-chain `.(f ?)` | — | Read a pipeline left to right |
| OOP-style call `1.inc` | Ruby | Put the receiver first |
| Map shorthand `{name: 1}` | JS | Less typing for map literals |

## Reasoning

**S-expressions read in the wrong order.** In
`(map inc (filter even? (range 5)))`, execution order and reading order are
reversed. Clojure solves this with `->>`; Clove adds `.(...)` on top, where `?`
marks where the previous value goes.

```clojure
(range 5).(filter even? ?).(map inc ?)   ; reads left to right
(->> (range 5) (filter even?) (map inc)) ; also available
```

Both work. `->>` was not removed.

**Subscript access is frequent.** `(nth xs 0)` and `(get-in m [:a :b])` are
correct, but they appear constantly in data-handling code and deepen the
parentheses every time. `xs[0]` and `m[:a :b]` express the same thing in less
space.

**Sometimes the receiver should come first.** `"abc".upper-case` works well with
completion and helps you discover what a value can do from the editor.

**All of it stays within Lisp.** Every one of these is expanded by the reader
into ordinary S-expressions; the evaluator sees nothing new. No evaluation rules
were added.

## Alternatives not taken

**Stick to Clojure's notation only.** The parenthesis depth and reading-order
problems remain. Clove prioritized the writing experience over Lisp fidelity.

**Let users add notation.** Rejected for the same reason as
[no macros](no-macros.md). Because the reader is fixed, the LSP and the
formatter can understand every construct in the language.

**Add more dot-chain shorthand.** A form that dropped the keyword — `m.a` — was
tried and removed; it bred bugs. `m.:a` remains. Shortening too far makes it
impossible for both readers and tools to tell a symbol reference from a map
access.

```bash
clove -e '(def m {:a 1}) (println m.a)'
# Runtime error: recv has key :a, but it is not callable.
#                Access it via recv.:a or recv[:a].
```

## Choosing between forms

Several things can be written more than one way. None of them is deprecated.

| Goal | Forms |
| --- | --- |
| Nth element | `(nth xs 0)` / `xs[0]` |
| Nested map lookup | `(get-in m [:a :b])` / `m[:a :b]` |
| Chaining | `(->> ...)` / `.(f ?)` |
| Function application | `(inc 1)` / `1.inc` |

Being consistent within a project is worth doing, but the language accepts
either.

## See also

- [Indexer](../language/indexer.md)
- [Dot-chain](../language/dot_chain.md)
- [OOP-style calls](../language/oop_syntax.md)
- [Map shorthand](../language/map_shorthand.md)
- [Reader syntax](../language/reader_syntax.md)
