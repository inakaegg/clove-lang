# Reader / syntax overview

Japanese version: [reader_syntax.ja.md](reader_syntax.ja.md)

- Updated: 2026-01-14

This page summarizes “what the Clove reader can parse” (literals, sugar, comments, quotes).

> **Important:** The desugaring described here **matches the implementation**.
> Internal symbol names may change in the future, but user-facing behavior is preserved.

## 1. Comments

- Line comment: `;` to end of line
- Form comment: `#_ FORM`

```clojure
(+ 1 2) ; => 3
(map #_skip inc (range 3))
```

## 2. Delimiters and data structures

- list: `(a b c)`
- vector: `[a b c]`
- map: `{:a 1 :b 2}`
- set: `#{:a :b}`

### 2.1 spread

Writing `*x` inside collections means “spread”.

```clojure
(def xs [1 2])
[0 *xs 3]     ; => [0 1 2 3]
{:a 1 *m :z 9}
#{*xs 9}
```

`*x` / `* expr` can also be used in function args.
`*(...)` counts as spread when `*` is **adjacent to the expression**.

## 3. Quotes

- quote: `'form` / `(quote form)`
- quasiquote: `` `form `` / `(quasiquote form)`
- unquote: `~x` / `(unquote x)`
- unquote-splicing: `~@xs` / `(unquote-splicing xs)`

> `quasiquote` forms are handled as special forms by the evaluator.

## 4. deref

- `@x` expands to `(deref x)`.

`deref` primarily supports:

- atom
- promise / future / task
- agent
- delay

## 5. Reader extensions (syntax sugar)

Clove's notable sugars are expanded by the reader.

- indexer: `x[0]`, `x[0 1]`, `x[0,1]`, `x[0||d]`, `a..b` ... [indexer](indexer.md)
- dot-chain: `x.(f ?)` / `x.(f *?)` / `expr.(...)` ... [dot-chain](dot_chain.md)
- OOP syntax: `obj.method(...)` ... [OOP syntax](oop_syntax.md)
- dot-indexer: `obj:key`, `obj.:key`, `obj."key"`, `obj.0` ... [OOP syntax](oop_syntax.md)
- call sugar: `sym(...)` -> `(sym ...)` (only when `(` follows a symbol; `,` is ignored)
- foreign blocks: `$rb{...}` / `$py{...}` / `${...}` (default interop; Ruby if unset) ... [Interop](interop_foreign.md)
- map-refs: `{:a 1 :b (+ &^:a 2)}` / `&:screen:h` (enable: `(use map-refs true)`)
- function composition: `(. f g h)` / `(f g h .)`

These can be toggled with `use-syntax`.

```clojure
(use-syntax dot-chain true)
(use-syntax indexer true)
(use-syntax dot-indexer true)
(use-syntax foreign-block true)
(use-syntax oop-syntax true)
(use-syntax map-refs true)
```

## 6. Function composition sugar

```clojure
(. f g h)   ; => (comp h g f)
(f g h .)   ; => (comp f g h)
```

- The idea is “feed from the dot side in order”.

### 5.1 dot-chain line continuation

`expr.(...)` can be continued by a **line-start `.(...)`** after a newline
(blank line separates forms).

```clojure
(glob* "lib/*.json")
.(map (fn [path] (println path) path) ?)
.(take 3 ?)
```

### 5.2 map-refs

map-refs is sugar to **reference other keys inside a map literal**.
It uses `&`-prefixed paths, resolved against the **current map (this)** or **root map (root)**.

> NOTE: map-refs is only valid inside map literals. Using it outside is an error.

#### Basic example (this / root)

```clojure
(do
  (use map-refs true)
  {:screen {:h 100}
   :pipe   {:gap 10
            :gap-half (int (/ &^:gap 2))
            :gap-max  (- &:screen:h &^:gap-half)}})
```

- `&^:gap` refers to `:gap` in **current map** (`:pipe`)
- `&:screen:h` refers to root map `:screen` -> `:h`
- `&ref:...` / `&ref^:...` are aliases of `&:...` / `&^:...`
- Moving through non-map values errors
- Reference order can be forward/backward; evaluation happens when needed
- Cycles or undefined refs are errors

#### Use the root map itself (`&ref`)

`&ref` returns the **root map itself**. You can use it with `get` or indexer.

```clojure
(do
  (use map-refs true)
  {:gap 10
   :gap-half (int (/ (get &ref :gap) 2))
   :gap2 &ref[:gap]})
```

#### `^` and parent reference (`../`)

`^` is the **relative (this)** origin. `../` moves up one level.

```clojure
(do
  (use map-refs true)
  {:a {:x 1
       :child {:y (+ &^../:x 2)}}}) ; => :y is 3
```

- `&^:x` = current map (this)
- `&^../:x` = parent map
- `&^../../:x` = grandparent map
- `&ref^:...` / `&ref^../:...` are aliases of `&^:...`

#### Segment specification

Segments in map-ref are **keywords only**.

```clojure
(do
  (use map-refs true)
  {:gap 1
   "gap" 2
   :val &^:gap}) ; => 1
```

`&x` / `&"x"` / `&root` / `&.` are deprecated and error.

## 6. `#(...)` short functions

As in Clojure, you can write anonymous functions with `#(...)`.

```clojure
(map #( + % 10) (range 3))
```

> `%`, `%1`, `%2`, ... are expanded to arguments by the reader.

## 7. Regex / Duration

- regex: `/.../` or `#/.../`
- duration literal: `10ms`, `2s`, etc. (**integers only**)

See: [Regex / Duration](regex_duration.md)

## 8. Data section (`__DATA__` / `__END__`)

If a source contains a `__DATA__` / `__END__` line, everything after becomes a **data section**.
The data section content can be referenced as the string `__DATA__`.

```clojure
(def payload __DATA__)

__DATA__
{"hello":"world"}
```

- If `__DATA__` does not exist, it is `nil`.

---
<!-- NAV:START -->
**Previous:** [REPL Guide](../tooling/repl.md)
**Next:** [Literals (numbers, strings, etc.)](literals.md)
<!-- NAV:END -->

