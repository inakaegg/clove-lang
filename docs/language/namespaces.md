# Namespaces (ns / require)

Japanese version: [namespaces.ja.md](namespaces.ja.md)

- Updated: 2026-01-14

Clove uses namespaces with **`::` separators**.

```clojure
(ns game::snake)

(defn -main []
  (println "hello"))
```

## 1. `ns`

`ns` defines the current namespace.

- If file placement and `ns` do not match, REPL/runtime may warn.
  (This also affects LSP jump/resolve.)

## 2. `require`

```clojure
(require game::util)

(game::util::foo 1)
```

### 2.1 File path target (string)

Passing a string to `require` loads it **as a file module**.
You can still use `:as` / `:refer` / `:rename` like normal.

```clojure
(require "../myns/aaa" :as aaa :refer :*)
```

```clojure
(ns tmp::match_type1
  (:require ["./match_type" :as mt :refer :*]))
```

- If the file has no `(ns ...)`, an **implicit namespace derived from the path** is assigned.
- If you want to “load into the current namespace”, use `load-file` instead.

### 2.2 Importing types

- `:refer` imports **types as well as values**.
- An alias via `:as` can be used in type annotations as `alias::Type`.
- `defenum` implicitly creates **variant type names** like `Noop` / `Quit`
  (unless `qualified-only` is set).
- Unqualified names from `:refer :*` **overwrite names in that namespace**.
  If you need builtins, use **fully qualified** names like `core::`.

> `require` search paths and “where the root is” depend on CLI/REPL `:source` / working_dir.

## 3. Namespace operations / references

### 3.1 `current-ns`

Returns the current namespace as a **Symbol**.

```clojure
(current-ns) ; => user, etc.
```

### 3.2 `ns-map`

Gets **public values as a keyword map** from a namespace.

```clojure
(ns-map)           ; current namespace
(ns-map 'my::ns)   ; specified namespace
```

Returns `nil` if it does not exist.

### 3.3 `create-ns`

**Explicitly creates** a namespace.

```clojure
(create-ns 'my::tmp)
```

### 3.4 `refer`

`refer` is a special form that **imports public exports** into the current namespace.

```clojure
(refer 'my::ns)
(refer 'my::ns :only [foo bar])
(refer 'my::ns :exclude [debug])
```

### 3.5 `resolve`

`resolve` **resolves a Symbol to a value** (returns `nil` if not found).

```clojure
(resolve 'core::map)
(resolve 'my::ns::foo)
(resolve 'foo) ; current-ns first
```

## 4. Dynamic load / eval

### 4.1 `load-file`

`load-file` **reads a file and evaluates forms**.

```clojure
(load-file "./script.clv")
```

- Returns **the last form result**.
- While `require` loads “as a module”, `load-file` **injects into the current environment**.

### 4.2 `load-string`

```clojure
(load-string "(+ 1 2)")
```

- Reads a string and **evaluates sequentially**.

### 4.3 `eval`

`eval` **evaluates code data**.

```clojure
(eval '(+ 1 2))          ; => 3
(eval (read-string "(+ 1 2)"))
```

- Do not pass raw strings; use **`quote` or `read-string`**.

## 5. Separators

- **Recommended:** `aaa::bbb::ccc`
- If legacy `aaa/bbb` separators are mixed, LSP may fail to track correctly.

(We recommend standardizing on `::` across the repo.)

## 6. `use` and `use-syntax`

`use` is an alias of `use-syntax` and controls reader extensions.

```clojure
(use-syntax indexer true)
```

See: [Runtime settings](../advanced/runtime_settings.md)

## 7. Private definitions (`def-` / `defn-`)

`def-` / `defn-` define **private vars**.
They can be referenced/updated only within the same namespace.

```clojure
(ns a)
(def- x 1)
(defn- hidden [] 42)

(defn t [] (set! x 2) x) ; OK within same ns

(ns b)
a::x ; => error (private)
(set! a::x 3) ; => error (private)
```

* Private vars are not resolved even via FQN (`a::x`).
* They are hidden from LSP/completion and `:env` by default.

---
<!-- NAV:START -->
**Previous:** [Threading / pipeline forms (-> / cond-> / some->)](threading.md)
**Next:** [indexer: `x[0]`, `x[0 1]`, `x[0,1]`, `x[0||d]`, `a..b`](indexer.md)
<!-- NAV:END -->

