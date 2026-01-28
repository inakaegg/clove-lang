# Bracket indexer (`[]`)

Japanese version: [indexer.ja.md](indexer.ja.md)

- Updated: 2025-12-21

Clove's **indexer** is reader sugar that shortens access to arrays/strings/maps/sets/nested structures.

> Depending on environment, it can be toggled as a SyntaxFeature (e.g., `use-syntax indexer true`).
> See **[docs/advanced/runtime_settings.md](../advanced/runtime_settings.md)** for defaults and toggles.

---

## 1) Quick examples

```clojure
;; basic
xs[0]
xs[-1]
xs[1 || :default]

;; map / set / get-in
m[:key]
m[:missing || 42]
m[:nested :inner]
m[:nested :inner, :other]
m[:nested :missing || :fallback]
tags[:x]
tags[:z || :absent]

;; gather by index list
xs[[0 2 4]]
xs[(range 3)]
xs[1,5,7]

;; range (Ruby-like slice)
xs[1..3]
xs[1...3]
xs[..3]
xs[2..]
xs[-3..-1]
s[1..3]

;; expressions
xs[(+ 1 2)]
xs[(do (inc 1))]
xs[i.inc..i.inc]

;; -foo special rule inside indexer
;; if -foo is bound, use it; otherwise treat as (- foo)
xs[-5..-foo]
xs[-5..--foo]
```

---

## 2) Single access: `xs[i]`

- `xs[0]` is index 0
- `xs[-1]` is the last element (negative index)
- You can write **index expressions** like `xs[(+ 1 2)]`

If the target is a vector/string, this behaves like indexed access.

---

## 3) Default: `xs[i || fallback]`

With `||` you can specify a default when missing/failing.

- Useful for missing keys in maps: `m[:missing || 42]`
- Works for sets too: `tags[:z || :absent]`

> What counts as “missing/failing” (nil only, or out-of-range too) depends on
> the target collection and indexer implementation. Out-of-range may error
> (also for slices) in current behavior.

---

## 4) Nested access: `m[:a :b :c]` (get-in)

Multiple keys/indexes inside `[]` mean nested access (like get-in).
**This is different from comma-separated paths.**

```clojure
m[:nested :inner]
m[:nested :missing || :fallback]
```

---

## 5) Comma-separated multi-paths: `xs[1,5,7]` / `m[:a :b, :c]`

Comma-separated forms are treated as **multiple paths**.
If the target is a sequence and each path is a single index,
it behaves like multiple `nth`.

- Result is always a vector
- Order and duplicates are preserved
- Defaults via `||` are allowed
- `xs[1 5 7]` is still **get-in** (different meaning)

```clojure
(def xs [10 11 12 13 14 15])
xs[1,5]         ; => [11 15]
xs[0,99 || :ng] ; => [10 :ng]
(def m {:a {:b 1} :c 2})
m[:a :b, :c]    ; => [1 2]
```

---

## 6) gather: `xs[[0 2 4]]` / `xs[(range 3)]`

Passing an “index list” gathers multiple positions.

- Literal vector: `xs[[0 2 4]]`
- Expression-generated seq: `xs[(range 3)]`

> Result type (vector or not) and out-of-range handling depend on implementation.

---

## 7) Range slice: `xs[a..b]` / `xs[a...b]`

Ruby-like slice notation is supported.

- `a..b` : end **inclusive**
- `a...b` : end **exclusive**
- `xs[..3]` : omit start (from beginning)
- `xs[2..]` : omit end (to last)
- `xs[-3..-1]` : negative indices from end

Strings can be sliced similarly (e.g., `s[1..3]`).

> While syntax is Ruby-like, boundary checks follow Clove implementation.
> e.g., `xs[0..10]` may error if out of range.

---

## 8) Special rule for `-foo` (inside indexer only)

Inside indexer, `-foo` is treated specially for convenience.

- If `-foo` is **bound**, refer to that symbol
- Otherwise treat it as `(- foo)` (unary minus)

```clojure
xs[-5..-foo]   ; if -foo is bound, use it; else (- foo)
xs[-5..--foo]  ; --foo can mean (- (- foo))
```

---

## 9) Internal expansion (reference)

Indexer is desugared by the reader and lowered to dedicated function calls.
User code does not need to depend on internal symbols.

---

*Updated: 2025-12-21*

---
<!-- NAV:START -->
**Previous:** [Namespaces (ns/require/resolve/load, etc.)](namespaces.md)
**Next:** [dot-chain: `x.(f ?)`, `x.(f *?)`, etc.](dot_chain.md)
<!-- NAV:END -->
