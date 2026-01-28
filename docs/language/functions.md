# Functions

Japanese version: [functions.ja.md](functions.ja.md)

- Updated: 2026-01-14

## 1. `defn` / `fn`

```clojure
(defn add [a b] (+ a b))
(add 1 2) ; => 3

(def inc2 (fn [x] (+ x 2)))
(inc2 10) ; => 12
```

### 1.1 docstring

`defn` can have a docstring.

```clojure
(defn greet
  "say hello"
  [name]
  (str "hello " name))

(doc 'greet) ; => "say hello" (or nil)
```

### 1.2 Local function `-defn` and `where`

`-defn` defines a **local function only inside the function body**.
It cannot be used at top level.

```clojure
(defn f []
  (-defn i [] 123)
  (i)) ; => 123
```

`where` is grouping for readability and **does not affect the return value**.
It is intended to gather internal functions at the end of a function.

```clojure
(defn f [] 1 (where (-defn i [] 123)))
; (f) => 1

(defn g [] (i) (where (-defn i [] 123)))
; (g) => 123
```

### 1.3 Trailing `err` / `fin` in body

If `err` / `fin` appear at the end of a **body (list of forms)** in `defn` / `fn`,
the body is implicitly wrapped in `try`.

```clojure
(defn f []
  (throw 1)
  (err ?)
  (fin (println "cleanup")))
; (f) => 1
```

- Order is fixed as **err -> fin**.
- `err` / `fin` can appear only at the end.

## 2. Variadic args

```clojure
(defn sum [x & xs]
  (reduce + x xs))
```

## 3. Multiple arity

```clojure
(defn f
  ([x] x)
  ([x y] (+ x y)))
```

## 4. Short function `#(...)`

```clojure
(map #( + % 10) (range 3))
```

### 4.1 Placeholder list

- `%` / `%1` / `%2` ...: positional args (`%` is `%1`)
- `?` / `?1` / `?2` ...: alias of `%`
- `%&`: rest args (variadic)
- `*?` / `*?1` / `*?2` ...: positional args like `?`, but **splat** (expand) when passed

> If the same name is bound by `let` / `fn`, it is not treated as a placeholder.

## 5. `?` placeholder for partial application (used with dot-chain)

Clove dot-chain uses `?` as a placeholder.

```clojure
(def result
  (range 10)
  .(shuffle ?)
  .(reverse ?)
  .(take ? 3))

result
```

See: [dot-chain](dot_chain.md)

---
<!-- NAV:START -->
**Previous:** [Collections (list/vector/map/set)](collections.md)
**Next:** [Destructuring](destructuring.md)
<!-- NAV:END -->

