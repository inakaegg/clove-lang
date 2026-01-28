# HM type inference and metadata sync policy

Japanese version: [typing.ja.md](typing.ja.md)

In Clove, function signatures (`FnOverload` / `TypeKind`) are registered in `clove-core::fn_meta`,
while HM-based type inference (`typing::infer`) uses a separately hand-written `prelude_env()`.
This dual maintenance causes several issues:

- Updating only one side causes `:doc` and HM inference types to drift.
- Adding a new core function requires updates in two places and is easy to miss.
- The source of truth is unclear, making future generation difficult.

## Goals

1. Make **`FnMeta` the single source of truth** for type information.
   Generate HM prelude (`typing::infer::prelude_env`) from `FnMeta` `TypeKind` data.
2. For builtins without type info yet, temporarily fill in with hand-written schemes.
   This allows gradual migration to `TypeKind`.

## Implementation steps

1. Add an enumeration API like `fn_meta::all()` to access registered metadata. ✔ (done)
2. Reuse `TypeKind -> hm::Type` conversion, and add a helper in `typing::infer`
   to convert `FnOverload` into `Scheme`.
3. In `prelude_env()`:
   - Insert schemes from `FnMeta` by name (`name` / `ns::name`)
   - Insert remaining manual entries only if they do not exist
4. For new builtins, register `overloads` in `FnMeta` and reduce manual schemes over time.

## Concrete tasks

1. **Add conversion helper**
   - Add `fn scheme_from_overload(overload: &FnOverload) -> Option<Scheme>` to `typing::infer`.
   - Define rules for `FnArity` and rest args (`TypeKind::Vector` / `TypeKind::Named`),
     with a temporary fallback to `Type::Any` for `TypeKind::Named`.
2. **Rebuild prelude_env**
   - Extract existing manual `Scheme` creation into `manual_prelude_entries()` and
     use `entry.entry(name).or_insert(scheme)` against the `FnMeta`-built HashMap.
   - Insert entries using both `meta.fq_name()` and `meta.name` to cover `core::*`, `string::*`, `io::*`, etc.
3. **Audit type coverage gaps**
   - Using `fn_meta::all()`, list functions with empty overloads or only `Any`,
     and create a prioritized TODO list for `TypeKind` completion.
   - Keep temporary schemes in `manual_prelude_entries()` with comments for why.
4. **Test plan**
   - Add a test like `prelude_env_coverage` in `typing::infer::tests`
     to ensure a minimum number of FnMeta-derived schemes.
   - Add integration tests for representative builtins (`+`, `map`, `assoc`, etc.)
     to confirm HM inference updates when `fn_meta` TypeKind changes.

Following this flow allows a gradual transition to `FnMeta` as the source of truth,
so docs/LSP/REPL/type inference all reference the same metadata and reduce drift.

---

## Appendix: Type hints (postfix / `:` annotation)

Clove's reader supports type hints at the end of an expression.

### 1) `<...>` form

Attach `<...>` to a symbol, like `symbol<Type>`.

```clojure
(def x<Int> 10)
```

### 2) `: TYPE` form

In `expr: TYPE`, the reader parses the part after `:` as a “type expression”.

```clojure
(def v [1 2]: [Int Int])
```

> Because `type` is designed to return the **runtime value type**,
> `(type v)` may not change even with type hints.

### Type hints are “hints”

Currently they are mainly for:
- LSP display
- typed build

They do not enforce runtime checks.

## Appendix: `clove build --opt=typed`

typed build is an experimental feature that generates Rust from typed IR.

- Incompatible parts emit WARNINGs
- In strict mode, warnings become errors

See [Build](../tooling/build.md) for tool-side specs.

---
<!-- NAV:START -->
**Previous:** [Troubleshooting](../tooling/troubleshooting.md)
**Next:** [Namespace design notes](namespaces_design.md)
<!-- NAV:END -->

