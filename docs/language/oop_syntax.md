# OOP method chain and Ruby default tag handling

Japanese version: [oop_syntax.ja.md](oop_syntax.ja.md)

- `oop-syntax` is enabled by default. To disable: `(use oop-syntax false)` (re-enable with `(use oop-syntax true)` if needed).
- The receiver position is resolved in this order: **function meta (`{:subject-pos n}` or `:last`) > `$arg` > argument name inference (`coll`/`xs`/`seq`)**.
  If `FnMeta` exists but position cannot be determined, it is an error (do not silently fall back to 1).
- `:last` (or `-1`) places the receiver at the **end of the argument list**. Use this when you want the last arg even for variadic functions.
- For object methods / `method` / `&` self sugar, see [docs/language/oop_methods.md](/docs/language/oop_methods.md).
- Examples (all work with default settings):
  - `"a".str("b")` -> `"ab"`
  - `{:a 1}.keys` -> `[:a]`
  - `(range 10).map(inc)` -> `[1 2 3 4 5 6 7 8 9 10]`
  - `1.inc` -> `2`
- Longer chain example:

```clojure
(use oop-syntax true)

(let [xs [1 2 3]
      ys [10 20 30]]
  ;; subject-pos = 2
  (println "mapped" xs.map(+ ys))          ; => [11 22 33]
  ;; subject-pos = :last (reduce is last)
  (println "sum" xs.reduce(+ 0))           ; => 6
  ;; dorun is also last-position, so it covers arity consistently
  (println "dorun" xs.dorun())
  (println "dorun with n" xs.dorun(2)))

;; Example of attaching :last to a custom function
;; (attr-map can be placed before or after doc)
(defn append-last {:subject-pos :last} "doc" [x coll] (conj coll x))
(println (str [1 2].append-last(3))) ; => [1 2 3]
```

## dot-indexer (key/index access in OOP segments)

When `dot-indexer` is enabled, you can access keys or indexes inside OOP chains.

```clojure
(use oop-syntax true)
(use dot-indexer true)

(def config {:pipe {:gap-max 485}})
config:pipe:gap-max   ; => 485 (keyword sugar)
config.:pipe.:gap-max ; => 485 (explicit segment)
```

### Explicit segments (always key/index access)

- `x:kw` -> sugar for `x.:kw` (keyword segment only)
- `x:a:b` -> `x.:a.:b`
- `x.:kw` -> `get x :kw`
- `x."str"` -> `get x "str"`
- `x.'sym` -> `get x 'sym`
- `x.0` / `x.-1` -> same as indexer (index for vector/list/string, key=0 for map)
- Sets have no index; `x.0` is an error (use `contains?` / `get`)

### Implicit segments (`x.ident`)

- `x.ident` is **always a method call** (dot-indexer does not search keys)
- For key access, use `x:kw` / `x.:kw` / `x."str"` / `x.'sym`

If you want to force a method call, use `x.(pipe ?)` or `(pipe x)`.

## Bindings inside a chain

- `.(as x)` binds the current value to `x` (within the chain only).
- `.(let x)` saves the current value to `*x` and passes the value through.
- `.repl` / `.(repl)` opens REPL with the current value and continues the chain.
- `.(repl x)` is shorthand for `.(let x).repl`.

```clojure
(use oop-syntax true)

[1 2 3].(as xs).take(2).concat(xs).vec    ; => [1 2 1 2 3]
[1 2 3].(let a).take(2).vec               ; => [1 2]  (*a is [1 2 3])
[1 2 3].map(inc).repl.take(2).vec         ; => [2 3]
```

## Ruby embedding basics

- Always prefix external objects with `$`. For Ruby, use `$rb:Foo.bar` as the base form.
  - Example: after `$rb{require "nokogiri"}`, use `$rb:Nokogiri::HTML.parse(html).search("h1").text`.
- Ruby embedding blocks use `$rb{...}`. `${...}` is shorthand for default interop (Ruby if unspecified).
- Shorthand `$Foo.bar` works **only** when a default tag is set
  (`:lang rb` in REPL / `(use default-interop :rb)` / file name `*.rb.clv`, etc.).

If you see `foreign block requires explicit tag`, add `$rb{...}` or set default tag
via `:lang rb` (REPL), `*.rb.clv`, or `(use default-interop :rb)`.

---
<!-- NAV:START -->
**Previous:** [dot-chain: `x.(f ?)`, `x.(f *?)`, etc.](dot_chain.md)
**Next:** [OOP methods (method / where / self)](oop_methods.md)
<!-- NAV:END -->

