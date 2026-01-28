# dot-chain (`x.(f ?)`)

Japanese version: [dot_chain.ja.md](dot_chain.ja.md)

- Updated: 2026-01-14

dot-chain is a reader sugar for “method-chain-like” readability.

> Enable it with `use-syntax dot-chain true` before use (defaults may vary by environment).

## 1. Basics

```clojure
x.(f ?).(g 1 ?).(h ?)
```

- A syntax that **chains** `expr.(...)` stages.
- The `?` in each stage is replaced with the **previous value**.

### 1.1 Stage requirements

- In general, **`?` or `*?` is required**.
- Exceptions: `.(as x)` / `.(let x)` / `.(repl)` / `.(debug)`
- Symbols match short-fn conventions: `?` == `%`, `*?` == splat

### 1.2 With short functions (`#(...)`)

`?`/`%` inside a short function are **placeholders for that function**.
Because dot-chain `?` is **not expanded inside** the short function,
you must place a separate `?` as the stage argument.

```clojure
(range 5)
.(filter #(> % 2) ?)
;;          ^ short fn arg (% )
;;                       ^ dot-chain placeholder
```

### 1.3 Inserting `repl` / `debug`

`(repl)` / `(debug)` can open a REPL at that point **without `?`**.
`.(repl x)` is shorthand for `.(let x).repl`: it saves to `*x` then opens REPL.

```clojure
(range 5)
.(map inc ?)
.(repl)
.(take ? 3)
```

### 1.4 `as` / `let` directives

- `.(as x)` binds the current value to `x` for later stages.
- `.(let x)` saves the current value to `*x`, and passes the value through unchanged.

## 2. `*?` (apply)

`*?` means “call with expanded args (splat)”.

```clojure
x.(f 1 *?)
```

This stage expands roughly as `(apply f 1 prev)`.
The symbol is the same as short-fn `*?` (splat).

> Constraint: `*?` must be the **last argument** in the stage list.

## 3. Typical example

```clojure
(def result
  (range 10)
  .(shuffle ?)
  .(reverse ?)
  .(take ? 3))

result
```

## 4. Differences from `->` / `as->`

- `->` / `as->`: transform existing forms in order
- dot-chain: inject the “previous value” using `?`

Both are usable, so it helps to agree on a team style.

> `(. f g h)` / `(f g h .)` are **function composition sugar**, not dot-chain.

## 5. Internal form (`__set-in-chain`)

Some features like `.(let x)` are internally expanded to `__set-in-chain`.
This is a **special form for the evaluator**, so users do not use it directly.

---
<!-- NAV:START -->
**Previous:** [indexer: `x[0]`, `x[0 1]`, `x[0,1]`, `x[0||d]`, `a..b`](indexer.md)
**Next:** [OOP syntax (`obj.method(...)`)](oop_syntax.md)
<!-- NAV:END -->

