# External engine embedding (Ruby / Python, etc.)

Japanese version: [interop_foreign.ja.md](interop_foreign.ja.md)

One of Clove's features is embedding **external language engines**.
Through Rust-side `ForeignEngine` implementations, Clove code can call Ruby/Python, etc.

This document outlines, based on the current design/implementation:

* How embedding forms work
* Ruby / Python blocks
* Differences from JSON / YAML tags
* Type conversion and caveats

> ⚠️ APIs described here may change.
> Details of conversion/error behavior depend on the Rust implementation.

---

## 1. Overview of embedding forms

There are three broad categories.

1. **Evaluate as an expression** in an external block

   * `$rb{ ... }` / `$py{ ... }` / `${ ... }` (shorthand for Ruby)
   * Round-trip: “Clove value -> external -> Clove value”
2. **Reader tags that convert JSON/YAML to Clove values**

   * `#json{ ... }` / `#yaml{ ... }`
   * Pure data conversion (no code execution)
3. Future extensions

   * Engines for other languages (Node.js / Rust)

This doc focuses on 1 and 2.

---

## 2. Ruby blocks

Example:

```clojure
(ns examples::interop::ruby-basic)

(defn ruby-version []
  $rb{
    RUBY_VERSION
  })

(defn upcase [s]
  $rb{String(s).upcase}

(println "Ruby version:" (ruby-version))
(println (upcase "hello from clove"))
```

### 2.1 Execution model

* Inside `$rb{ ... }` is interpreted/executed as Ruby.
* `${ ... }` is shorthand for the **default external language** (Ruby if unset).
* Implementation-wise, it passes the block as a string to the Rust-side Ruby engine (`clove-ruby`, etc.).
* The block result is converted back into a Clove value.

### 2.1.1 Default external language

* Targets: `${...}` / `$Foo.bar(...)` and other tag-less foreign forms
* Priority: **file extension** -> **(use default-interop/foreign ...)** -> **Ruby**
* Extensions: `*.rb.clv` / `*.py.clv`
* `(use default-interop <lang>)` and `(use default-foreign <lang>)` are aliases
  * Accepted: `"rb"` / `"ruby"` / `:rb` / `:ruby` -> `rb`, and `"py"` / `"python"` / `:py` / `:python` -> `py`
* If extension and `(use ...)` conflict, **extension wins** and a WARNING is emitted
* Decided per file; it does not propagate to `require`d files

### 2.2 Value conversion (roughly)

**Clove -> Ruby** (conceptual)

* `nil` -> `nil`
* int / float -> Ruby `Integer` / `Float`
* string -> Ruby `String`
* vector -> Ruby `Array`
* map -> Ruby `Hash` (keys converted from keywords/strings, etc.)
* bool -> `true` / `false`

**Ruby -> Clove**

* `nil` -> `nil`
* `Integer` / `Float` -> `:int` / `:float`
* `String` / `Symbol` -> string or keyword
* `Array` -> vector
* `Hash` -> map

Exact rules depend on Rust implementation, but “simple serializable values”
are designed to round-trip easily.

### 2.3 Use cases

* Call existing Ruby libraries as-is

  * HTTP clients
  * DB clients
  * text processing
* Write main logic in Clove and offload parts to Ruby

---

## 3. Python blocks

Example:

```clojure
(ns examples::interop::python-basic)

(defn py-sqrt [x]
  $py{
    import math
    math.sqrt(x)
  })

(println (py-sqrt 9)) ; => 3.0
```

* `$py{ ... }` runs as Python.
* Value conversion works similarly to Ruby.

Typical use cases:

* Leverage the Python ecosystem (e.g., ML libraries)
* Reuse existing Python scripts

---

## 4. Differences from JSON / YAML tags

`#json{ ... }` / `#yaml{ ... }` are **data loading**, not code execution.

```clojure
(def config-json
  #json{
    "host": "localhost",
    "port": 8080
  })

(def config-yaml
  #yaml{
    host: localhost
    port: 8080
  })
```

* `#json{ ... }`

  * Parses JSON into Clove values (map / vector / string / number / bool / nil)
* `#yaml{ ... }`

  * Parses YAML similarly

**Safety difference:**

* `$rb{}` / `$py{}` etc. are “arbitrary code execution”
* `#json{}` / `#yaml{}` are “data only”

Use `#json{}` / `#yaml{}` for configs/static data,
and `$rb{}` / `$py{}` for logic or external libraries.

---

## 5. Notes and best practices

### 5.1 Dependencies and deployment

* Ruby/Python embedding requires the respective runtime.
* For `clove build` binaries, how to embed/link those runtimes matters (static vs dynamic).
* If you want a small single binary, minimize external dependencies and move what you can into Clove/Rust.

### 5.2 Avoid over-reliance on types

* Ruby/Python have rich class/type systems,
* but when converting back to Clove, the representation is mostly “simple values” (map/vector/string/number).

Avoid returning raw foreign objects; instead, return only needed info as maps/vectors.

### 5.3 Exceptions / errors

* Exceptions on the foreign engine side are surfaced as Clove errors (`CloveError`).
* How much message/stacktrace is preserved depends on implementation.

---

## 6. Summary

* `$rb{...}` / `$py{...}` let Clove call Ruby/Python directly.
* Conversion is designed around simple serializable values.
* `#json{}` / `#yaml{}` are reader tags for **data**, not code.
* Consider deployment/runtime weight and split “Clove vs external” responsibility wisely.

---
<!-- NAV:START -->
**Previous:** [Concurrency / async (chan / future / go-loop / scope-loop / async-scope)](concurrency.md)
**Next:** [Types/enum/match (deftype/defenum/match)](types_enum_match.md)
<!-- NAV:END -->

