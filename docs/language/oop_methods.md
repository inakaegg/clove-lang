# New OOP spec: object methods / self / method

Japanese version: [oop_methods.ja.md](oop_methods.ja.md)

This page focuses on **object-internal methods** and **self references** in OOP syntax.
For chain basics, see [docs/language/oop_syntax.md](/docs/language/oop_syntax.md).

## Goal

- `.foo` always means **method call**, not key access.
- Treat map / deftype as **objects with methods**.
- Methods can be added/removed via `assoc/merge/dissoc` (immutable).
- Context like `canvas` is read from **self**, not passed as arg.

## `.foo` dispatch order

Resolution for `recv.foo(args...)` is:

1. If `recv` is a map / deftype instance and key `:foo` **exists**:
   - If the value is **callable**:
     - If it is a method (`is_method=true`), call with `recv` injected first.
       - `recv[:foo](recv, args...)`
     - If it is callable but not a method, **call as-is** (no self injection).
       - `recv[:foo](args...)`
   - If the value is **not callable**, error (no fallback).
     - Suggest `recv.:foo` / `recv[:foo]` for value access.
2. Only when `recv` **does not have** `:foo`, fall back to legacy OOP chain and call `foo`.

## How to define a method

### 1) `(method ...)`

```
(method [args...] body...)
```

- `self` is automatically inserted as the first argument.
- The generated function has `is_method=true`.
- `self` is reserved, so **do not write it in args** (implicit).

### 2) `deftype ... (where (defn ...))`

```
(deftype Renderer {canvas: Any}
  (where
    (defn clear [] (sdl2::clear &:canvas))
    (defn set-color [c] (sdl2::set-draw-color &:canvas c.0 c.1 c.2 c.3))))
```

- `defn` inside `where` is **always a method**.
- It is copied into the instance map (can be removed with `dissoc`).
- You can also put `defn` / `method` directly under `deftype` (shorthand for `where`).

```
(deftype Renderer {canvas: Any}
  (defn clear [] (sdl2::clear &:canvas))
  (method set-color [c] (sdl2::set-draw-color &:canvas c.0 c.1 c.2 c.3)))
```

### 3) Infer method when body contains `&`

```
(assoc r :draw (fn [] (draw &:canvas)))
```

- If `&` / `self` appears in the body, it is treated as a self reference and turned into a method.
- It injects self, rewrites `&`, and sets `is_method=true`.

## Self reference sugar

### `&` / `&:key` / `&[:key]`

- `&` is **self itself**.
- `&self` is an explicit form of self (`&`).
- For fields, use **`&:key` or `&[:key]`**.
  - Both are valid.

### Deprecated forms

- `&x` / `&.` / `&root` are **errors**.
  - self is `&`
  - self field access is `&:key` / `&[:key]`
  - map-ref is `&^:key` / `&:key` (map literal only)
  - explicit forms: `&ref:...` / `&ref^:...`
  - root map itself is `&ref` (map literal only)

Example:

```
(method []
  {:x &:x
   :self &})
```

Note: In method context, `&:x` is always self reference (even inside map literals).

## Extending via `assoc/merge/dissoc`

- Insert a `method` via `assoc` / `merge` to call with `.foo`.
- Remove via `dissoc`.

## Example

```
(use oop-syntax true)

(def obj
  {:x 10
   :add (method [n] (+ &:x n))
   :pack (method [] {:x &:x :self &})})

(obj.add 5) ; => 15

(def obj2 (assoc obj :mul (fn [n] (* &:x n))))
(obj2.mul 3) ; => 30

(def obj3 (dissoc obj2 :mul))
(obj3.mul 1) ; => error (no method)

(def obj4 (assoc obj :plain (fn [x] (+ x 1))))
(obj4.plain 10) ; => 11 (no self injection)
```

---
<!-- NAV:START -->
**Previous:** [OOP syntax (`obj.method(...)`)](oop_syntax.md)
**Next:** [OOP methods quick reference (new syntax)](oop_methods_quickref.md)
<!-- NAV:END -->

