<p align="center">
  <img src="assets/clove_logo.png" alt="Clove logo" width="512">
</p>

# Clove

Japanese version: [README.ja.md](README.ja.md)

Clove is a small language inspired by **Clojure-style S-expressions**, with
**lightweight types (deftype / defenum)**, **pattern matching**, and even
**Ruby / Python embedding** in one place.

It keeps the Clojure-like feel while letting you mix in the outside world (Ruby/Python)
when it matters. That is the "flavor" we aim for.

> **Status: experimental / WIP** — syntax, APIs, and CLI can change.
> Specs live in [`docs/`](docs/), runnable code in [`examples/`](examples/).
> This README is intentionally short and example-focused.

---

## Quick taste

### Hello

```clojure
(ns examples::hello)

(println "Hello from Clove!")
```

```bash
clove examples/hello.clv
```

---

### Dot-chain (`.(...)`) and placeholder `?`

You can write `as->`-style flows as `expr.( ... )`.
`?` is the placeholder for the previous value.

```clojure
(inc 123).(+ 1 ?).(repeat 3 ?).(map inc ?)
; => (126 126 126)
```

`*?` expands the previous value as args (internally `apply`).

```clojure
[inc (range 10)].(map *?)
; => (1 2 3 4 5 6 7 8 9 10)
```

---

### Placeholder `?` (inline function)

Any expression that includes `?` becomes a small inline function.

```clojure
(map (+ ? 10) (range 5))
; => (10 11 12 13 14)

(filter (not= :skip ?) [:ok :skip :ok])
; => (:ok :ok)
```

> For details, see [docs/language/reader_syntax.md](docs/language/reader_syntax.md).

---

### Map shorthand + indexer (`[]`)

Alongside Clojure-style `{:x 1}`, JSON-like `{name: "Taro"}` is supported.

```clojure
(def user {name: "Taro" age: 30})
user[:name] ; => "Taro"
```

Indexers are flexible beyond simple lookup:

```clojure
(def xs [10 11 12 13 14 15])

xs[0]        ; => 10
xs[-1]       ; => 15
xs[99 || :ng]; => :ng   ; default when missing

xs[[0 2 4]]  ; => [10 12 14]  ; gather
xs[1,5,7]    ; => [11 15 17]  ; multiple indexes
xs[..3]      ; => [10 11 12 13] ; open range
xs[2..]      ; => [12 13 14 15]
xs[1...3]    ; => [11 12]       ; end-exclusive

xs[(+ 1 2)]  ; => 13  ; expressions inside indexer
```

> Details (range, `-foo` rule, map/set/get-in, etc.) are in [docs/language/indexer.md](docs/language/indexer.md).

---

### `deftype` / `defenum` / `match` (lightweight types + branching)

We emphasize data + pattern matching instead of protocols/multimethods.

```clojure
(deftype Dog  {:name :string :age :int})
(deftype Cat  {:name :string :lives :int})
(defenum Pet Dog Cat)

(defn pet-name [p]
  (match p
    (Dog {:name n}) n
    (Cat {:name n}) n))

(pet-name (Dog {:name "Pochi" :age 3}))
; => "Pochi"
```

---

### Mix Ruby / Python inline

One of Clove's unique features is embedding external languages.
Ruby is the default external language, so use `$rb{...}` (or `${...}` as an alias).

```clojure
(defn ruby-version []
  $rb{ RUBY_VERSION })

(defn py-sqrt [x]
  $py{
    import math
    math.sqrt(x)
  })

(println (ruby-version))
(println (py-sqrt 9)) ; => 3.0
```

JSON/YAML can also be read via reader tags.

```clojure
(def config
  #json{"host":"localhost","port":8080})

config[:host] ; => "localhost"
```

---

### Concurrency primitives (minimal)

We include `atom` / `chan` / `promise` / `task` / `future` / `agent`.

```clojure
(def c (chan 1))
(chan-put! c :ok)
(chan-take! c) ; => :ok
```

More examples are in [examples/concurrency/](examples/concurrency/) and [docs/language/concurrency.md](docs/language/concurrency.md).

---

## Where to read next

- Quick start: [docs/getting_started.md](docs/getting_started.md)
- Docs entry: [docs/index.md](docs/index.md)
- Examples: [examples/](examples/)
- Run examples:

  ```bash
  clove --main examples/concurrency/async_scope_nested.clv
  ```

---

## Install / Build

### Most reliable (from source)

Currently developed as a Rust workspace.

```bash
git clone https://github.com/inakaegg/clove-lang clove
cd clove
cargo build -p clove-lang --release
./target/release/clove --help
```

To install to PATH:

```bash
cargo install --path crates/clove-lang --force
clove --repl
```

### If Rust is installed (without cloning)

```bash
cargo install --git https://github.com/inakaegg/clove-lang --locked --package clove-lang --bin clove
```

---

## License

Dual-licensed under MIT or Apache-2.0, at your option.  
See [LICENSE-MIT](LICENSE-MIT) and [LICENSE-APACHE](LICENSE-APACHE).
