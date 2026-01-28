# Collections

Japanese version: [collections.ja.md](collections.ja.md)

- Updated: 2026-01-14

Clove has four basic collections:

- list: `(a b c)`
- vector: `[a b c]`
- map: `{:a 1 :b 2}`
- set: `#{:a :b}`

## 1. Common: “Collections are callable”

As in Clojure, some collections can be **called like functions**.
(Example: treat a map as a “function of key to value”.)

```clojure
(def m {:a 1 :b 2})
(m :a)        ; => 1
(m :missing)  ; => nil
(m :missing 0); => 0  (default)
```

Vectors can be called by index as well.

```clojure
(def v [10 20 30])
(v 0)      ; => 10
(v 99)     ; => nil
(v 99 :d)  ; => :d
```

> Combined with the `indexer` sugar (`v[0]`), it reads nicely.

## 2. spread

`*x` inside a collection means “spread”.

```clojure
(def xs [1 2])
[0 *xs 3]      ; => [0 1 2 3]
```

It works for map and set as well.

## 3. seq and persistent data

Clove is based on persistent (immutable) collections, and functions like
`map`, `reduce`, etc. assume **seq input**.

> Whether a function returns a vector or a lazy seq follows the `std` policy.

Related: [Standard library](stdlib.md)

## 4. Binding shorthand for collection functions

For functions like **(f/pred) + coll** (e.g., `map`, `filter`),
if the first argument is a binding vector, it acts as sugar.

```clojure
(map [x [1 2 3]] (+ x 10))
(filter [x [1 2 3 4]] (odd? x))
(remove [x [1 2 3 4]] (odd? x))
(take-while [x [0 1 2 3 0]] (< x 3))
(sort-by [s ["aa" "b" "ccc"]] (count s))
```

- Pattern binding works (same as `for` and `doseq`).
- Internally it builds `fn + let` and calls `core::<name>`.
- Targets: `map` / `filter` / `pmap` / `pfilter` / `remove` / `keep` / `some` / `every?` / `not-any?` / `not-every?` / `take-while` / `drop-while` / `split-with` / `partition-by` / `group-by` / `run!` / `sort-by`
- `sort-by` supports only `(sort-by [x coll] expr)` (no comparator)

## 5. mut / imut (mut is shallow / imut is deep)

`mut` performs a **shallow conversion**.
Only the top level of map/vector/set becomes mutable; children remain immutable.
`assoc-in!` / `update-in!` **mutate only the touched paths** automatically.
By contrast, `imut` is **deep freeze**, recursively converting mutables to immutable.

- Destructive APIs like `assoc!` / `assoc-in!` / `conj!` / `dissoc!` / `disj!` / `pop!` / `update!` / `update-in!` / `merge!` are **allowed only for mut**
- `assoc` / `conj` / `dissoc` / `disj` / `into` accept both mut/imut and **always return a new imut**
- Do not put mutable collections into sets (use `(imut x)`)
- mut sets always keep elements as imut (same for `conj!`)
- You cannot pass mut across thread boundaries such as chan/agent/foreign (use `(imut x)` if needed)

### Example: update world with mut

```clojure
(def world (mut {:player (mut {:x 10 :y 20})
                 :bullets (mut [])
                 :tick 0}))

(assoc! world :tick (+ world.:tick 1))
(assoc! world.:player :x (+ world.:player.:x 3))
(conj!  world.:bullets (mut {:x 100 :y 200 :vx 5 :vy 0}))
```

### Example: update! / update-in!

```clojure
(let [m (mut {:a {:b 1}})]
  (update! m :a (fn [v] (assoc v :c 2)))
  (update-in! m [:a :b] inc)
  (get-in m [:a :b])) ; => 2
```

### Example: assoc-in! / merge!

```clojure
(let [m (mut {:a {:b 1}})]
  (assoc-in! m [:a :b] 2)
  (merge! m {:c 3})
  (get-in m [:a :b])) ; => 2
```

### Example: snapshot at thread boundary

```clojure
(let [snapshot (imut world)]
  (future (fn [] (save-replay snapshot))))
```

### Example: old reference caveat (spec)

```clojure
(let [p world.:player]
  (assoc! world :player (mut {:x 0 :y 0})) ; replace player entirely
  p.:x) ; p keeps the old player
```

### Predicates

```clojure
(mut? (mut {:a 1})) ; => true
(imut? {:a 1})      ; => true
```

---
<!-- NAV:START -->
**Previous:** [Literals (numbers, strings, etc.)](literals.md)
**Next:** [Functions (defn / fn / #() / variadic)](functions.md)
<!-- NAV:END -->

