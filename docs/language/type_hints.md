# Type hints

Japanese version: [type_hints.ja.md](type_hints.ja.md)

- Updated: 2026-07-25

Type hints are optional annotations. They are **hints**, not runtime checks:
nothing is validated when the code runs.

## 1. Where hints go

### 1.1 Function definitions

The **return** type is a keyword placed right after the name.
**Argument** types use the postfix `name<Type>` form.

```clojure
(defn add :int [x<Int> y<Int>]
  (+ x y))

(add 1 2) ; => 3
```

Either part can be omitted:

```clojure
(defn add [x y] (+ x y))          ; no hints
(defn add :int [x y] (+ x y))     ; return type only
(defn add [x<Int> y<Int>] (+ x y)) ; argument types only
```

> Writing an argument type as a bare keyword (`[x :int]`) is **not** valid syntax
> and fails with `fn params must be symbols, vector destructuring, or map destructuring`.

The same postfix form works for `fn` and `let`:

```clojure
((fn [x<Int>] (+ x 1)) 10)  ; => 11
(let [n<Int> 5] (+ n 1))    ; => 6
```

### 1.2 `<...>` on a symbol

Attach `<...>` to any symbol being bound.

```clojure
(def x<Int> 10)
```

### 1.3 `expr: TYPE` on an expression

In `expr: TYPE`, the reader parses the part after `:` as a **type expression**.

```clojure
(def v [1 2]: [Int Int])
```

## 2. Hints do not change runtime values

`type` reports the **runtime value type**, so it does not follow the hint:

```clojure
(def v [1 2]: [Int Int])
(type v) ; => :core::Vector

(def x<Int> 10)
(type x) ; => :core::Int  ; matches here only because the value really is an Int
```

Nor are hints enforced on call:

```clojure
(defn add :int [x<Int> y<Int>] (+ x y))
(add 1.5 2.5) ; => 4.0  ; no error
```

## 3. What hints are used for

- `doc` / `describe` output
- LSP display and completion
- The typed lowering used by `clove build`

`clove build` lowers a supported subset of the language through a typed IR and emits C.
There is **no `--opt` switch**: the typed path is the only build path, and unsupported
constructs are rejected at build time instead of falling back to the interpreter.
See [Build](../tooling/build.md) for the current option list.

---
<!-- NAV:START -->
**Previous:** [Types/enum/match (deftype/defenum/match)](types_enum_match.md)
**Next:** [Standard library `std`](stdlib.md)
<!-- NAV:END -->
