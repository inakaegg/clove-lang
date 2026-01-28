# Types / enum / match

Japanese version: [types_enum_match.ja.md](types_enum_match.ja.md)

Clove is a Lisp dialect designed around dynamic, map-centric data,
but by combining `deftype` / `defenum` / `match`, you can write in a lightweight ADT style.

Based on current implementation, this document summarizes:

- `deftype` (product type)
- `defenum` (sum type / enum)
- `match` (pattern matching)

> 💡 Differences from Clojure
>
> - Clove has **no** `protocol` / `multimethod`.
> - Instead: “data shape via `deftype` + `defenum`” and “branch logic via `match`”.

---

## 1. `deftype` – named product type

### 1.1 Syntax

```clojure
(deftype Name {:field1 :type1 :field2 :type2 ...})

;; optional doc string
(deftype Name "Doc string"
  {:foo :int :bar :string})

;; flat field list
(deftype Name
  :field1 :type1
  :field2 :type2)
```

* `Name` is a symbol (**in the current ns**; singular form recommended: `Dog`, `Cat`, `Human`)
* Field part is a map of **keyword -> type hint**, or a flat list
* Type hints currently assume roughly:

  * primitives: `:int`, `:float`, `:string`, `:bool`, etc.
  * other type names (`Dog`, `UserId`, etc.)

#### Constructor args (shorthand)

`deftype` constructors accept map form, even-length keyword pairs, and shorthands.

```clojure
(deftype Color :r Int :g Int :b Int :a Int)

(Color {r: 1, g: 2, b: 3, a: 4})
(Color :r 1, :g 2, :b 3, :a 4)
(Color r: 1, g: 2, b: 3, a: 4)

(def r 1) (def g 2)
(Color r:, g:)  ;; => (Color :r r :g g)
(Color :r, :g)  ;; => (Color :r r :g g)
```

* Even-length `:k v` pairs are internally converted to a map
* `k:` / `:k` are shorthand for `:k k` (constructor calls only)
* If you do not want shorthand, use map form `{:k v}`

### 1.1.1 `:from` defines a default instance

You can bind **one** default value alongside `deftype`.

```clojure
(deftype Config :from cfg
  {:screen {:w 480 :h 640}
   :timing {:spawn-frames 92 :tick-ms 16}})

cfg
;; => same as (Config {...})
```

Equivalent `def` form:

```clojure
(deftype Config (def cfg
  {:screen {:w 480 :h 640}
   :timing {:spawn-frames 92 :tick-ms 16}}))
```

Notes:

* `:from` binds **exactly one** name (no auto generation)
* Field types are inferred from the default value structure (used by `describe`)
* If the default contains `deftype :alias` values, **the type name is preserved**

### 1.2 What gets defined

```clojure
(ns spec::types::animals_a)

(deftype Dog {:name :string :age :int})
```

Evaluating this defines in the current ns:

* constructor function **`Dog`**
* predicate function **`Dog?`**
* metadata in the type registry (for `describe-type`)

Example behavior:

```clojure
(ns spec::types::animals_a)

(deftype Dog {:name :string :age :int})

[(type (Dog {:name "Pochi" :age 3}))
 (Dog? (Dog {:name "Pochi" :age 3}))
 (Dog? {:name "Fake" :age 2})]
;; =>
;; ['spec::types::animals_a::Dog
;;  true
;;  false]
```

* `type` ... returns the **fully qualified symbol** of the value's type
* `Dog?` ... checks whether the value is of type `Dog`

#### Type checks

Basic approaches:

```clojure
(Dog? value)                                   ; predicate (auto-generated)
(= (type value) 'spec::types::animals_a::Dog)  ; compare type result
(instance? 'spec::types::animals_a::Dog value) ; type name as symbol/string
```

* `instance?` accepts **symbol/string**, so passing `Dog` (constructor) is an error.
* `=` compares values; use `type` / `Dog?` / `instance?` for type checks.
* `instance?` validates required fields and types for `deftype` (ignores unknown keys).
* If you only want to check the `:type` tag, use `tagged?` (not a safe validation).

Enum variants can be checked as `Enum::Variant`:

```clojure
(match mode
  Mode::Running  1
  Mode::GameOver 2
  _              3)

(= (type mode) 'const::Mode::Running)
(instance? 'const::Mode::Running mode)
```

If a `deftype` with the same name exists in the same namespace,
**unqualified names like `Running` resolve to that first**.
Use `Mode::Running` to be safe.

### 1.3 Values are “normal maps + type tag”

Values from the `Dog` constructor are just maps in appearance.

```clojure
(def pochi (Dog {:name "Pochi" :age 3}))

pochi
;; => {:name "Pochi" :age 3 ...}   ; looks like a map

(type pochi)
;; => 'spec::types::animals_a::Dog

(Dog? pochi)
;; => true
```

Internally, the registry records which `deftype` created the value, so:

* map functions (`get`, `assoc`, `update`, etc.) work as-is
* `type` / `Dog?` / `describe-type` can access type info

### 1.4 Type hints (current status)

Field hints like `:int` / `:string` are **hints only**.

* No strict runtime type checks at the moment
* Intended future uses:

  * LSP completion / type hover
  * formatter / doc generation
  * possible light optimizations (boxing avoidance, etc.)

> Think of it as “documentation + future optimization hints”.

---

## 2. `defenum` – sum type / enum

`defenum` groups multiple `deftype` into a single “category”.
Think of “Dog + Cat = Pets”.

### 2.1 Basic form

```clojure
(ns spec::types::animals_b)

(deftype Dog {:name :string})
(deftype Cat {:name :string})

(defenum Pets
  Dog
  Cat)
```

* `Pets` means “either `Dog` or `Cat`”
* By convention, enum names are plural / collection names

  * e.g., `Pets`, `Mammals`, `Animals`, `ErrorKinds`
* Members must be `deftype` names **in the same ns**

### 2.2 Metadata and introspection

Evaluating `deftype` / `defenum` registers metadata in the type registry.
You can get type info as a map via `describe-type`.

```clojure
(describe-type 'Pets)
;; roughly:
;; {:kind    'sum
;;  :ns      'spec::types::animals_b
;;  :name    'Pets
;;  :fqn     'spec::types::animals_b::Pets
;;  :members ['spec::types::animals_b::Dog
;;            'spec::types::animals_b::Cat]
;;  ...}

(describe-type 'Dog)
;; roughly:
;; {:kind        'product
;;  :ns          'spec::types::animals_b
;;  :name        'Dog
;;  :fqn         'spec::types::animals_b::Dog
;;  :fields      {:name :string}
;;  :belongs-to  ['spec::types::animals_b::Pets]
;;  ...}
```

`describe` accepts **value or type name** and returns type info.
`describe-type` is shorthand for `describe` on type names.

```clojure
(describe (Dog {:name "Pochi"}))
(describe 'Dog)        ; same as describe-type
```

`infer-type` is a helper that **infers expression type and returns a string**.

```clojure
(infer-type (+ 1 2))
(infer-type (fn [x] x))
```

Keys/values follow implementation, but conceptually include
`kind` / `ns` / `name` / `fqn` / `fields` / `belongs-to` / `members`, etc.

There is also `enum-members`, for inspecting enum contents.
Currently it shares the same internal implementation as `describe-type`,
with plans to unify the API later.

### 2.3 Future extension idea (memo)

* A proposal to include another enum via `*Pets` style.

```clojure
(defenum Animals
  *Pets    ;; include all of Pets = Dog | Cat (proposal)
  Salmon)
```

* This is **design-stage** only and not fixed yet.
* When implemented, docs will be updated accordingly.

---

## 3. `match` – pattern matching

`match` tries patterns top-to-bottom and evaluates the first matching clause.

Combined with `deftype` / `defenum`, it feels closer to ML/Haskell style.

### 3.1 Basic form

```clojure
(match value
  pattern-1 expr-1
  pattern-2 expr-2
  ...
  _         default-expr)
```

* Left: **pattern**
* Right: **expression evaluated when matched**
* Evaluated top-down; only the first matching clause runs
* `_` is a wildcard (matches anything)

### 3.2 Type pattern + map pattern

The basic `deftype` pattern is “type + map shape”.

```clojure
(ns spec::types::animals_b)

(deftype Dog {:name :string})
(deftype Cat {:name :string})

(defenum Pets
  Dog
  Cat)

(defn describe [pet]
  (match pet
    (Dog {:name n}) (str n " the dog")
    (Cat {:name n}) (str n " the cat")
    _              "unknown"))
```

* `(Dog {:name n})`

  * “Dog type + bind `:name` to `n`”
* `(Cat {:name n})`

  * matches only when type is `Cat`

#### Field shorthand pattern

When you list **keywords only** like `(Dog :name :age)`, it is shorthand for
`(Dog {:name name :age age})`.

This works in `match` patterns and also `let` binding patterns.

### 3.3 Use `:as` to bind the whole value

Use `:as` if you want the whole value while destructuring fields.

```clojure
(defn describe [pet]
  (match pet
    (Dog {:name n}) :as whole
      (if (Dog? whole)
        (str n " the dog")
        "not a dog")

    (Cat {:name n}) (str n " the cat")
    _               "unknown"))
```

* `(Dog {:name n}) :as whole expr`

  * `:name` binds to `n`
  * the whole value binds to `whole`

### 3.4 `:when` guard conditions

Use `:when` to match only when a condition holds.

```clojure
(defn describe [pet]
  (match pet
    (Dog {:name n}) :as whole
      (if (Dog? whole)
        (str n " the dog")
        "not a dog")

    (Cat {:name n}) :when (= n "Mimi")
      (str n " the picky cat")

    (Cat {:name n})
      (str n " the cat")

    _ "unknown"))
```

In this example:

1. If `Dog`, also check `Dog?` and return dog text
2. If `Cat` and name is `"Mimi"`, return “picky cat”
3. Otherwise `Cat` -> normal cat
4. Else -> "unknown"

### 3.5 Evaluation order and exhaustiveness

`match` evaluation is straightforward:

1. Try patterns top-to-bottom
2. If a pattern matches:

   * If `:when` exists, evaluate guard
   * If guard is true, evaluate RHS and finish
   * If false, continue to next pattern
3. If nothing matches, error unless `_` exists

> ⚠️ There is **no compile-time exhaustiveness check** currently.
> Provide `_` or other fallback patterns yourself.

---

## 4. Usage patterns and design hints

### 4.1 `deftype` as “named maps”

`deftype` is great for giving names to domain data previously represented as maps.

Example:

```clojure
(deftype Player {:id :int :name :string})
(deftype Enemy  {:id :int :kind :string})

(defenum Actors
  Player
  Enemy)
```

* DB records / HTTP responses / configs can remain maps
* Still distinguish types via `Player?` / `Enemy?` / `type`

### 4.2 Use enum + match to centralize branching

It reads better to centralize “branch by type” in one place.

```clojure
(defn handle-event [event]
  (match event
    (Player {:id id}) (handle-player id)
    (Enemy  {:id id}) (handle-enemy  id)
    _                 (handle-unknown event)))
```

Then when adding a new type:

* add one line in `defenum`
* add one line in the `match`

### 4.3 As a replacement for Clojure protocol / multimethod

Clove has no `protocol` / `multimethod`, so the intended style is:

* define data shapes via `deftype`
* group functions that accept that type per namespace
* keep type-based branching inside `match`

```clojure
(ns app::render
  (:require [app::types :as t]))

(defn render-entity [entity]
  (match entity
    (t::Player {:id id}) (render-player entity)
    (t::Enemy  {:id id}) (render-enemy  entity)
    _                    (render-fallback entity)))
```

* Dispatch entry is centralized in `render-entity`
* Data is still maps, so interop with JSON/YAML and external libs is easy

---

## 5. Summary

* `deftype` defines named product types (values are maps) and provides constructor/predicate/metadata.
* `defenum` groups multiple `deftype` into one category.
* `match` performs pattern matching over those types as a special form.
* These are **lightweight ADT-like** while staying map-based, fitting Clove's world view.

In the future, LSP/formatter/doc generation/optimization will further leverage this type metadata.

---
<!-- NAV:START -->
**Previous:** [Interop (foreign blocks / Ruby / Python)](interop_foreign.md)
**Next:** [Standard library `std`](stdlib.md)
<!-- NAV:END -->

