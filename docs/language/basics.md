# Language basics

Japanese version: [basics.ja.md](basics.ja.md)

This document is a rough overview of the "first things to know" in Clove:

- S-expressions and evaluation
- literals (numbers / strings / collections / regex / Duration)
- variable definitions (`def`, `let`)
- functions (`fn`, `defn`) and type hints
- control forms (`if`, `cond`, `when`, `loop` / `recur`, etc.)
- namespaces (`ns`, `require`)
- small REPL tips

For the type system, external engines, and concurrency, see other docs.

---

## 1. S-expressions and evaluation model

Clove is a Lisp-like language with S-expressions.
An expression is a **list**, where the first element is a function or special form,
and the rest are its arguments.

```clojure
(+ 1 2 3)     ; => 6
(str "a" "b") ; => "ab"
(if cond x y) ; if cond is true, x, else y
```

* Lists are written as `()`.
* Forms like `(+ 1 2)` are called "forms".
* Evaluation is close to Clojure: evaluate the head to a function, then pass the rest as args.

---

## 2. Literals

### 2.1 Basic literals

* integer: `0`, `42`, `-10`
* float: `3.14`, `-0.5`, `1.0e3`
* string: `"hello"`, `"hi"`
* boolean: `true`, `false`
* nil: `nil`

Numeric literals allow `_` separators.

```clojure
1_000_000  ; => 1000000
3_600_000  ; etc.
```

### 2.2 Collections

* vector: `[1 2 3]`
* list: `'(1 2 3)` or `(list 1 2 3)`
* map:

  * Clojure style: `{:x 1 :y 2}`
  * JSON style: `{x: 1 y: 2 name: "Taro"}`
* set: `#{1 2 3}`

```clojure
(def v [1 2 3])
(def m {:x 10 :y 20})

v[0]       ; => 1
m[:x]      ; => 10
(assoc m :x 99) ; => {:x 99 :y 20}
```

In JSON-style maps, keys are treated as keywords.

```clojure
{foo: 1 bar: 2}  ; => {:foo 1 :bar 2}
```

### 2.3 Regex and Duration

* regex:

  * Ruby / JS style: `/foo.*/` (default)
  * explicit form: `#/foo.*/` (disambiguation)

* Duration:

  * `10ms`, `500ms`, `3s`, `2m`, `1h`, `5d`, `1w`, `1y`, etc.
  * `_` separators allowed: `1_000ms`
  * constructors like `(duration 0.5 :sec)` or `(duration-ms 250)` for numeric inputs

See the dedicated doc for details:

* [Regex / Duration literals](regex_duration.md)

### 2.4 Keyword name resolution

Keywords are global in Clove. You can write namespace-like forms such as `:user::flag`,
but they are just part of the string and are **not** bound to a namespace.
This is different from Clojure's auto-resolved keywords like `::myns/abc`.

---

## 3. Variable definition and bindings

### 3.1 `def` - top-level definition

```clojure
(def answer 42)

(println answer) ; => 42
```

* Defines `answer` in the current namespace.
* Re-evaluation overwrites the value.
* `def` is top-level only (error inside functions).

### 3.2 `let` - local binding

```clojure
(let [x 10
      y 20]
  (+ x y))    ; => 30
```

* Alternating name/value pairs in a vector.
* Valid only within the `let` scope.

### 3.3 `set!` / `redef` - update existing var

```clojure
(def x 1)
(set! x 2)
x ; => 2
```

* `set!` updates **an existing namespace var**.
* `set!` to an undefined var is an error (typo detection).
* `redef` is an alias of `set!`.
* You cannot update local bindings (`let` / `-def`) with `set!`.

### 3.4 `-def` - local binding (let* equivalent)

```clojure
(defn f [x]
  (-def a (+ x 1))
  (-def b (+ a 1))
  (+ a b)) ; => 5 (x=1)
```

* `-def` is local-only inside functions.
* It is sequential binding (like `let*`), so later bindings can reference earlier ones.

---

## 4. Functions

### 4.1 Anonymous function `fn`

```clojure
(fn [x] (+ x 1))

((fn [x] (+ x 1)) 10) ; => 11
```

### 4.2 Function definition `defn`

```clojure
(defn add :int [x :int y :int]
  (+ x y))

(add 1 2) ; => 3
```

* `defn name <ret-type?> [arg1 <type?> arg2 <type?> ...] body...`
* `defn` is top-level only (error inside functions).
* Return/arg type hints are **optional**, but useful for:

  * docs (`doc` / `describe`)
  * future LSP / completion
  * light optimizations (planned)

* attr-map can be placed **right after the name** or **after the docstring** (not both).
  * `{:subject-pos N}` or `{:subject-pos :last}`/`-1` fixes receiver position in OOP chains.
  * The attr-map is retrievable via `(meta fn-name)` (attached as data, not evaluated).

```clojure
(defn greet {:subject-pos :last} "docstring" [name suffix]
  (str name suffix))

(meta greet) ; => {:subject-pos :last}
```

### 4.3 Short lambdas

There is a short lambda syntax `#()` (e.g., `#(inc %)`),
but its spec may still change, so details are omitted here.
Prefer `fn` / `defn` for stability.

---

## 5. Control forms

Only representative forms are listed (largely similar to Clojure).

```clojure
(if cond
  then-expr
  else-expr)

(when cond
  expr1
  expr2
  ...)

(cond
  (= x 0) "zero"
  (= x 1) "one"
  :else   "other") ; `_` also works
```

`loop` / `recur` are supported (tail recursion style).

```clojure
(defn sum-to :int [n :int]
  (loop [i 0 acc 0]
    (if (> i n)
      acc
      (recur (inc i) (+ acc i)))))
```

---

## 6. Namespace and `require`

### 6.1 `ns`

```clojure
(ns myapp::core)

(def pi 3.1415)
```

* Namespaces are written with `::` separators.
* Conventionally, align file path and namespace
  (e.g., `myapp/core.clv` -> `(ns myapp::core)`).

### 6.2 `require`

```clojure
(ns myapp::ui
  (require myapp::core :as core))

(core::pi) ; => 3.1415
```

As in Clojure, supports `:as` / `:refer` / `:rename`.

You can also pass a string to `require` to load a file module.
If the file has no `(ns ...)`, an implicit namespace derived from the path is assigned.
To "inject definitions into the current namespace", use `load-file`.

`:refer` imports types as well as values, and `:as` aliases can be used in type hints as `alias::Type`.
`defenum` implicitly creates variant type names like `Noop` / `Quit` (unless `qualified-only`).

---

## 7. Small REPL tips

With `clove --repl`, you can do things like:

```clojure
; symbol docs
:doc map
:doc myapp::core::area

; value/symbol info
(describe +)
(describe {:x 1 :y 2})
```

From VS Code, you can send the current form/selection to REPL via
`Clove: Send Selection to REPL`.

> REPL details (e.g., `:source`) will be consolidated into `tooling/cli.md`.

---
<!-- NAV:START -->
**Previous:** [Getting Started](../getting_started.md)
**Next:** [REPL Guide](../tooling/repl.md)
<!-- NAV:END -->

