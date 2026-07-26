# What was not taken from Clojure

Japanese version: [differences-from-clojure.ja.md](differences-from-clojure.ja.md)

- Updated: 2026-07-25

## Decision

Clove is built on a Clojure-style Lisp, but it is **not Clojure-compatible**.
Running Clojure code unchanged is not a goal, and some features are deliberately
absent.

The list below covers what will *not* arrive, so it can be told apart from what
is merely unimplemented.

| Feature | Status | Reason |
| --- | --- | --- |
| `defmacro` / syntax-quote | Not planned | [No macros](no-macros.md) |
| `defprotocol` / `extend-type` | Not planned | No type-extension mechanism. Polymorphism goes through maps carrying functions |
| `defmulti` / `defmethod` | Not planned | No additional dispatch mechanism. Functions and `match` are enough |
| `/` as a namespace separator | Removed | [No `/` as a namespace separator](namespace-separator.md) |
| Namespaced keywords (`::foo`) | Not planned | Same |
| `in-ns` and namespace switching | Not planned | See below |

```bash
clove -e '(defprotocol P)'     # Unbound symbol: 'defprotocol'
clove -e '(defmulti f class)'  # Unbound symbol: 'defmulti'
clove -e '(in-ns (quote foo))' # Unbound symbol: 'in-ns'
```

## No namespace switching

Clojure lets you switch the running namespace with `in-ns`. Clove does not. A
namespace is **fixed per file** and never changes during execution.

- If the file starts with `(ns ...)`, that is the namespace
- Otherwise it is derived from the file path
- Under `clove -e` or `clove --repl` it is `user`

```bash
clove -e '(println (current-ns))'   # => user
```

An `(ns ...)` form that disagrees with the directory layout produces a warning.

```
[WARN] namespace 'my::app' defined in '.../ns2.clv' does not match directory layout
```

The reason is to preserve the "one namespace = one file" rule. With that rule,
the LSP can map a namespace to exactly one file, which makes go-to-definition
and completion reliable. Build-time static resolution relies on the same
premise. See [namespace design notes](../advanced/namespaces_design.md).

## Conversely, Clojure's macros are in the language

While some features are absent, most of what Clojure provides as macros exists
here as special forms:

`when`, `when-not`, `when-let`, `if-let`, `if-some`, `cond`, `cond->`,
`cond->>`, `condp`, `->`, `->>`, `as->`, `some->`, `some->>`, `while`, `doseq`,
`dotimes`, `for`, `doto`, `with-open`, `with-redefs`, `defn`, `defn-`, `ns`,
`delay`.

So "no macros" does not mean "you cannot write Clojure-style code".
`(-> x (assoc :a 1) (update :b inc))` works as written.

## Lazy sequences exist

`range` with no arguments is infinite, and `take` pulls only what is needed.

```bash
clove -e '(println (take 3 (range)))'   # => [0 1 2]
```

The [native build path](two-phase-implementation.md) does not support lazy
evaluation; everything there is eager at present.

## Licensing

Clove does not reuse Clojure source code. It is an independent implementation
that borrows some syntax and function names. Clove itself is dual-licensed under
MIT and Apache-2.0 ([LICENSE-MIT](../../LICENSE-MIT) /
[LICENSE-APACHE](../../LICENSE-APACHE)).

## See also

- [FAQ: Is it the same as Clojure?](../faq.md)
- [Basics](../language/basics.md)
