# Namespace Design Notes (`ns` / `require`)

Japanese version: [namespaces_design.ja.md](namespaces_design.ja.md)

This document is a design memo for Clove namespaces (`ns`) and `require`.

* Clove's `ns` is close to Clojure, but with slightly different constraints
* The baseline is “one namespace = one file”
* Mapping between file paths and namespaces
* Variations of `require` and auto-load

---

## 1. Principles

### 1.1 “One namespace = one file”

Current implementation policy:

* One `ns` declaration per file
* Using the same `ns` name in multiple files is an error (or a strong warning)
* Partial namespaces (splitting one namespace across files) are not supported

Reasons:

* Keep the implementation simple
* LSP / link features (go-to-definition, etc.) can treat “this ns == this file” uniquely
* Easier for future compile/build pipelines

We may allow partial namespaces in the future, but for now we assume “1 ns = 1 file”.

### 1.2 Namespace naming rules

Current rules (still under discussion):

* Do not use `-` in `ns` names (future decision pending)
* `_` is allowed
* Unlike Ruby/Clojure, use **`::` separators**

Examples:

```clojure
(ns myapp::core)
(ns myapp::api::server)
(ns my_app::util)
```

---

## 2. Mapping to file paths

### 2.1 Recommended mapping

Recommended convention:

| Namespace | File path |
| --- | --- |
| `myapp::core` | `src/myapp/core.clv` |
| `myapp::api` | `src/myapp/api.clv` |
| `myapp::api::v1` | `src/myapp/api/v1.clv` |

* `::` -> `/` (directory separator)
* The last segment becomes the file name (`.clv`)

The implementation (e.g., `NamespaceStore`) records the mapping and checks that
no namespace is defined by multiple files.

### 2.2 Loose enforcement and warnings

For now, this is **recommended** rather than enforced.
Even if mismatched, it is not an immediate error.

Future direction:

* Warn in a `clove check`-like command when namespace and file path do not match
* Use WARN instead of ERROR for some cases

---

## 3. `require` design

### 3.1 Basic form

```clojure
(ns myapp::ui
  (require myapp::core :as core))

(core::area 10)
```

* `require` takes a namespace symbol (as in Clojure)
* Supports `:as` / `:refer` / `:rename` etc.

### 3.2 About `quote`

In Clove, `require` does not require Clojure-style `'ns-name`.

```clojure
(require myapp::core :as core)
;; OK

(require 'myapp::core :as core)
;; Whether to keep the `'` reader macro is still under discussion
```

* Currently `'` works as `(quote ...)`,
* but we do not yet have a macro system (`defmacro` / quasiquote, etc.),
* so for `require` we prefer “write the symbol directly”.

---

## 4. auto-load and dependency resolution

The runtime side resolves dependencies as follows:

1. Read `ns` declaration and record “this file defines this namespace”
2. On `require`, locate and load the matching file

* Search paths:

  * under project root (e.g., `src/` or [examples/](/examples/))
  * possibly a CLASSPATH-like setting (TBD)
* If a namespace is found in multiple files:

  * error or strong warning

---

## 5. Comparison with Clojure / Ruby

### 5.1 Clojure

* `ns` / `require` are nearly identical concepts
* `clojure.core` is implicitly imported
* Naming rules for `-` / `_` differ slightly from Clove

### 5.2 Ruby

* Module name vs file path mapping is a convention, not enforced
* `require "my_app/core"` style
* Namespace/module and file structure can diverge, which becomes painful to refactor later

Clove is closer to Clojure than Ruby,
but prefers “guided by conventions and warnings” rather than strict hard errors.

---

## 6. Future work

* How far to allow partial namespaces
  * allow only for tests/experiments, or forbid entirely
* How strictly to enforce `ns` <-> file path mapping
* Add static checks like `clove check` / `clove lint`
* LSP integration
  * go-to-definition / find-references depend on namespace info

---

## 7. Summary

* Clove `ns` / `require` are close to Clojure, with some implementation-driven differences.
* The current baseline is “one namespace = one file”.
* `::` separators are recommended, with a mapped file path convention.
* This is a guide for LSP/build/docs, rather than a strict hard rule set.

---
<!-- NAV:START -->
**Previous:** [Types (design, type hints, typed build assumptions)](typing.md)
**Next:** [Memoization and persistent cache (memo / memoize)](memoization.md)
<!-- NAV:END -->

