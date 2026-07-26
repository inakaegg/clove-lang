# Design Notes

Japanese version: [README.ja.md](README.ja.md)

- Updated: 2026-07-25

This directory records **why Clove is the way it is**. The specifications
themselves live in the documents reachable from [docs/index.md](../index.md);
what you find here is the reasoning behind those specifications, including the
alternatives that were considered and dropped.

Clove is built on a Clojure-style Lisp, but some features are deliberately left
out and some notation is borrowed from other languages. When you hit something
that "should work in Clojure" but doesn't, these notes tell you whether it is
unimplemented or intentionally absent.

## Index

| Note | Decision |
| --- | --- |
| [No macros](no-macros.md) | No `defmacro`. Syntax is fixed by special forms and the reader |
| [No `/` as a namespace separator](namespace-separator.md) | `foo/bar` is gone; `foo::bar` is the only form. Keywords carry no namespace |
| [What was not taken from Clojure](differences-from-clojure.md) | Why protocols, multimethods, and namespace switching were left out |
| [Two implementations: interpreter and native build](two-phase-implementation.md) | One language, two implementations. The native path never falls back silently |
| [Types stay optional annotations](gradual-typing.md) | No runtime type checking. Type information serves tooling and native optimization |
| [`mut` and `imut`](mutability.md) | Persistent data structures by default. `mut` is an explicit contract for destructive updates |
| [Borrowed notation](borrowed-syntax.md) | Why indexers, dot-chains, OOP-style calls, and map shorthand exist |

## How to read these

Each note follows the same shape.

- **Decision** — the current specification
- **Reasoning** — why it was decided that way
- **Alternatives not taken** — what was considered and dropped
- **How to verify** — commands to confirm the behavior yourself

The "alternatives not taken" sections exist so the same debate does not get
reopened from scratch. Whether the original reason still holds is a separate
question; if you believe the premises have changed, reconsider.
