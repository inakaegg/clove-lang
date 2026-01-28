# Threading / pipeline

Japanese version: [threading.ja.md](threading.ja.md)

- Updated: 2026-01-14

Clove provides Clojure-style “threading macro” special forms.

## 1. `->` / `->>`

```clojure
(-> x
    (f 1)
    (g 2))

(->> xs
     (map inc)
     (take 10))
```

## 2. `as->`

```clojure
(as-> (range 10) it
  (shuffle it)
  (take it 3))
```

## 3. `cond->` / `cond->>`

```clojure
(cond-> 1
  true  inc
  false dec
  (> 5 3) (+ 10))
; => 12
```

```clojure
(cond->> [1 2 3]
  true  (map inc)
  false (filter odd?))
; => [2 3 4]
```

- `cond->` / `cond->>` apply the form **only when the test is truthy**.
- Tests/forms are written in **pairs**.

## 4. `some->` / `some->>`

```clojure
(some-> {:a {:b 10}}
  :a
  :b
  (+ 1))
; => 11

(some->> [1 2 3]
  (map inc)
  (filter odd?)
  (reduce + 0))
```

- `some->` / `some->>` return **nil immediately if any step becomes nil**.

## 5. Relationship with dot-chain

- `->` / `as->` transform a sequence of forms
- dot-chain is a method-like staged expression with `?` placeholders

See: [dot-chain](dot_chain.md)

---
<!-- NAV:START -->
**Previous:** [Control flow (if/when/cond/for/while/try, etc.)](control_flow.md)
**Next:** [Namespaces (ns/require/resolve/load, etc.)](namespaces.md)
<!-- NAV:END -->

