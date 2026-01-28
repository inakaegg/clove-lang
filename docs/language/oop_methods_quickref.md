# OOP method syntax quick reference

Japanese version: [oop_methods_quickref.ja.md](oop_methods_quickref.ja.md)

This page summarizes **only the new OOP method syntax**.
See [docs/language/oop_methods.md](/docs/language/oop_methods.md) for details.

## 1. `.foo` call rules

`recv.foo(args...)` resolves in this order:

1. If `recv` is a map / deftype instance and **has** key `:foo`
   - If the value is a **method**, call with `recv` injected first
   - If the value is **callable but not a method**, call as-is
   - If the value is **non-callable**, error (suggest `recv.:foo` / `recv[:foo]`)
2. Only if `recv` **does not have** `:foo`, fall back to legacy OOP chain

## 2. How to write self

- `&` is **self**
- `&self` is an explicit form of self
- Self field access is **`&:key` or `&[:key]`**
- `&x` / `&.` / `&root` are **deprecated** (error)

## 3. How to create a method

- Use `(method [args...] ...)`
- `defn` / `method` inside `deftype` are always methods (`where` or direct)
- If `&` or `self` appears in `fn/defn` body, **method inference** applies
- `self` is reserved; **do not write it in args**

## 4. map-refs (map literal only)

- Enable: `(use map-refs true)`
- **Same map (this)**: `&^:key`
- **Root map**: `&:key`
- Segments must be **keywords only** (e.g., `&:screen:h`)
- `&ref:...` / `&ref^:...` are aliases of `&:...` / `&^:...`
- `&ref` is the **root map itself** (can be used with `get` / indexer)

```clojure
(use map-refs true)
{:a {:x 1
     :child {:y (+ &^../:x 2)}}}
```

> In method context, `&:key` / `&^:key` are rewritten as self references.
> If you want map-ref, use it outside of method context.

## 5. Example

```clojure
(use oop-syntax true)

(let [obj {:x 100
           :add (method [n] (+ &:x n))
           :plain (fn [a] (+ a 1))
           :pack (method [] {:x &:x :self &})}]
  (println "add =" (obj.add 5))
  (println "plain =" (obj.plain 10))
  (println "pack =" (obj.pack)))

(use map-refs true)
(def cfg {:screen {:h 100}
          :pipe {:gap 10
                 :gap-half (int (/ &^:gap 2))
                 :gap-max (- &:screen:h &^:gap-half)}})
```

---
<!-- NAV:START -->
**Previous:** [OOP methods (method / where / self)](oop_methods.md)
**Next:** [Map shorthand (JS-style)](map_shorthand.md)
<!-- NAV:END -->

