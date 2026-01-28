# Destructuring

Japanese version: [destructuring.ja.md](destructuring.ja.md)

- Updated: 2025-12-21

Clove `let` and `fn` support Clojure-style destructuring.

## 1. Vector destructuring

```clojure
(let [[a b] [10 20]]
  [a b])
;; => [10 20]

(let [[a b & rest] [1 2 3 4]]
  [a b rest])
;; => [1 2 [3 4]]
```

## 2. Map destructuring: `:keys`

```clojure
(let [{:keys [a b]} {:a 1 :b 2 :c 3}]
  [a b])
;; => [1 2]
```

## 3. `:as` (keep original value)

```clojure
(let [{:keys [a] :as m} {:a 1 :b 2}]
  [a m])
;; => [1 {:a 1 :b 2}]
```

## 4. `:or` (defaults)

```clojure
(let [{:keys [a b] :or {b 99}} {:a 1}]
  [a b])
;; => [1 99]
```

## 5. “Alias binding”

As in Clojure, you can resolve key/binding name collisions such as

- `keys :keys`
- `as :as`

```clojure
(let [{:keys [keys as]
       keys :keys
       as   :as
       :as  whole}
      {:keys [1 2] :as "foo"}]
  [keys as whole])
```

## 6. Same in function arguments

```clojure
(defn f [{:keys [a] :as m} [x y]]
  [a m x y])
```

---
<!-- NAV:START -->
**Previous:** [Functions (defn / fn / #() / variadic)](functions.md)
**Next:** [Control flow (if/when/cond/for/while/try, etc.)](control_flow.md)
<!-- NAV:END -->

