<p align="center">
  <img src="assets/clove_logo.png" alt="Clove logo" width="512">
</p>

# Clove

[![CI](https://github.com/inakaegg/clove-lang/actions/workflows/ci.yml/badge.svg)](https://github.com/inakaegg/clove-lang/actions/workflows/ci.yml)

🇯🇵 日本語ドキュメント: [README.ja.md](README.ja.md)

Clove is a small Lisp inspired by Clojure. It combines **S-expressions**,
**lightweight types (`deftype` / `defenum`) with pattern matching**, and
**inline Ruby / Python embedding** in a single language, and can also compile
scripts to native binaries.

> **Status: experimental / WIP** — syntax, APIs, and the CLI may still change.
> Specifications live in [`docs/`](docs/), runnable code in [`examples/`](examples/).
> This README stays short and example-focused on purpose.

---

## Quick examples

### Hello

```clojure
(ns examples::hello)

(println "Hello from Clove!")
```

```bash
clove examples/hello.clv
```

---

### Dot-chain (`.(...)`) and the placeholder `?`

`expr.( ... )` is a compact way to write `as->`-style pipelines.
`?` marks where the previous value goes.

```clojure
(inc 123).(+ 1 ?).(repeat 3 ?).(map inc ?)
; => [126 126 126]
```

`*?` spreads the previous value into the argument list (internally `apply`).

```clojure
[inc (range 10)].(map *?)
; => [1 2 3 4 5 6 7 8 9 10]
```

---

### Placeholder `?` (inline functions)

Any expression containing `?` becomes a small anonymous function.

```clojure
(map (+ ? 10) (range 5))
; => [10 11 12 13 14]

(filter (not= :skip ?) [:ok :skip :ok])
; => [:ok :ok]
```

> Details: [docs/language/reader_syntax.md](docs/language/reader_syntax.md)

---

### Map shorthand + indexer (`[]`)

Alongside Clojure-style `{:x 1}`, JSON-like `{name: "Taro"}` is also accepted.

```clojure
(def user {name: "Taro" age: 30})
user[:name] ; => "Taro"
```

Indexers go beyond simple element access:

```clojure
(def xs [10 11 12 13 14 15])

xs[0]        ; => 10
xs[-1]       ; => 15
xs[99 || :ng]; => :ng   ; default when missing

xs[[0 2 4]]  ; => [10 12 14]  ; gather
xs[1,3,5]    ; => [11 13 15]  ; multiple indexes
xs[..3]      ; => [10 11 12 13] ; open range
xs[2..]      ; => [12 13 14 15]
xs[1...3]    ; => [11 12]       ; end-exclusive
xs[(+ 1 2)]  ; => 13  ; any expression works inside the brackets
```

> More on ranges, the `-foo` rule, map/set/get-in, etc.: [docs/language/indexer.md](docs/language/indexer.md)

---

### `deftype` / `defenum` / `match` (lightweight types and branching)

Instead of protocols and multimethods, Clove leans on plain data plus pattern matching.

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

### Inline Ruby / Python

Embedding foreign languages is one of Clove's distinctive features.
Ruby is the default foreign language, so `$rb{...}` and its alias `${...}` are equivalent.

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

JSON / YAML values can be written directly with reader tags.
Note that JSON keys stay strings, so index with `"host"` rather than `:host`.

```clojure
(def config
  #json{"host":"localhost","port":8080})

config["host"] ; => "localhost"
```

---

### Concurrency primitives

Clove ships a minimal set: `atom` / `chan` / `promise` / `future` / `spawn` (task) / `agent`.

```clojure
(def c (chan 1))
(chan-put! c :ok)
(chan-take! c) ; => :ok
```

More examples: [examples/concurrency/](examples/concurrency/) and [docs/language/concurrency.md](docs/language/concurrency.md)

---

## Where to read next

- Quick start: [docs/getting_started.md](docs/getting_started.md)
- Documentation index: [docs/index.md](docs/index.md)
- Runnable samples: [examples/](examples/)
- Run an example:

  ```bash
  clove --main examples/concurrency/async_scope_nested.clv
  ```

---

## Install / Build

Clove is developed as a Rust workspace. A prebuilt binary is the quickest way to
run Clove; build from source when you want the embedded Ruby and Python
runtimes, which the prebuilt binaries leave out.

### Prebuilt binaries

Every release attaches one archive per platform, plus a `SHA256SUMS` file, to
the [Releases page](https://github.com/inakaegg/clove-lang/releases).

| Platform | Archive | Requirements |
| --- | --- | --- |
| macOS, Apple Silicon | `clove-<version>-aarch64-apple-darwin.tar.gz` | arm64 Mac; built on macOS 14 (GitHub runner), run verified on macOS 26 |
| Linux, x86_64 | `clove-<version>-x86_64-unknown-linux-gnu.tar.gz` | glibc 2.35 or newer; built on Ubuntu 22.04 |

```bash
# Check the download first, then unpack it.
# On Linux, use `sha256sum` in place of `shasum -a 256`.
shasum -a 256 --ignore-missing -c SHA256SUMS
tar xzf clove-0.1.0-aarch64-apple-darwin.tar.gz
./clove-0.1.0-aarch64-apple-darwin/clove --version
```

An archive holds the `clove` executable and the two license files (MIT and
Apache-2.0), nothing else.
`clove-lsp` is not distributed this way; build it from source.

#### Limitations and caveats

- **No embedded Ruby or Python.** `$rb{ ... }` and `$py{ ... }` stop at runtime
  with `unknown foreign tag: rb` (or `py`). The rest of the language, `clove
  build` included, behaves as usual. Both runtimes have to be linked in when
  the binary is built, which no redistributable build can do; [the design
  note](docs/design-notes/prebuilt-binaries.md) explains why and what else was
  weighed. If you need the embedding, install from source instead:

  ```bash
  cargo install --git https://github.com/inakaegg/clove-lang --locked clove-lang
  ```

- **`clove build` still needs a C compiler.** The native build path invokes
  `cc`, which the archive does not carry. Install the Xcode Command Line Tools
  on macOS (`xcode-select --install`), or your distribution's build-essential
  package on Linux.

- **macOS: the first run may need a network connection.** macOS marks what a
  browser downloads with a *quarantine* attribute, and a quarantined copy is
  checked with Apple before it runs — offline, that check can fail and the
  binary is refused. Unpacking with `tar` leaves no such attribute, so a
  download from the terminal normally just runs. If macOS does stop the binary,
  clear the attribute yourself:

  ```bash
  xattr -d com.apple.quarantine ./clove
  ```

  The check goes over the network because a lone executable, unlike an app
  bundle, cannot carry its notarization ticket inside itself — Apple calls
  storing it there *stapling* — so the ticket has to be fetched instead.
  Archives are signed with a Developer ID certificate and notarized starting
  with the first release cut after the signing keys were registered; earlier
  ones are unsigned, and an unsigned copy is stopped whether or not you are
  online. The step above is the same either way, and `codesign -dv ./clove`
  tells you which one you have.

### From source

```bash
git clone https://github.com/inakaegg/clove-lang clove
cd clove

# Install the CLI into PATH (add clove-lsp if you want editor/LSP support)
cargo install --path crates/clove-lang --force
cargo install --path crates/clove-lsp --force

# Try it
clove --repl
clove examples/hello.clv

# Compile to a native binary (phase2 C backend)
clove build examples/build/high_value_report.clv --out target/clove/bin/high_value_report
./target/clove/bin/high_value_report
```

To build without installing, use `cargo build -p clove-lang --release` and run
`./target/release/clove` directly.

`clove-lsp` is a stdio language server. It is normally launched by your editor
extension or LSP client, not from the terminal.

### Without cloning

```bash
cargo install --git https://github.com/inakaegg/clove-lang --locked clove-lang
```

### Note for Ruby embedding

The Ruby bridge goes through `rb-sys` / `magnus`, so a **Ruby 3.x** toolchain is
required. Building against older Rubies such as the macOS system
`/usr/bin/ruby` (2.6) may fail.

### About `clove build`

`examples/build/high_value_report.clv` is a sample verified to compile and run
with `clove build`; it computes a simple high-value sales summary.
`examples/` currently mixes interpreter-oriented and build-oriented programs,
so not every example is guaranteed to pass `clove build` yet.

---

## CLI overview

- `clove` / `clove --repl` — start the REPL
- `clove -e '(+ 1 2 3)'` — evaluate a single expression
- `clove path/to/file.clv` — run a file (prints the last value)
- `clove --main path/to/file.clv` — evaluate the file, then call `-main` (used by the examples)
- `clove --repl path/to/file.clv` — evaluate the file, then drop into the REPL
- `clove fmt ...` — formatter
- `clove build ...` — compile to a native binary
- Native plugins are expected to ship inside `plugins/` (`<project>/plugins` and `~/.clove/plugins` are allowed by default; plugins under `pkg` must match the sha256 recorded in the lock file — see [docs/tooling/cli.md](docs/tooling/cli.md))

Run `clove --help` / `clove build --help` for the full option list.

---

## VS Code extension

This repository includes the VS Code extension `vscode-clove`
(source distribution; not yet published on the Marketplace).
It provides syntax highlighting, S-expression-aware selection expansion,
sending forms to the REPL, and `clove fmt` integration.

Setup and keybinding examples: [packages/vscode-clove/README.md](packages/vscode-clove/README.md)

---

## Main differences from Clojure

- No `protocol` / `multimethod`; `deftype` / `defenum` / `match` cover those use cases
- Macros (`defmacro` / syntax quote) are not implemented yet
- `/.../` is the default regex literal (`#/.../` when ambiguous)
- Duration literals such as `10ms` are built in
- Ruby / Python can be embedded via tags and blocks

---

## Contributing

- Dev environment: [docs/contributing/dev_setup.md](docs/contributing/dev_setup.md)
- Testing: [docs/contributing/testing.md](docs/contributing/testing.md)
- Repository layout: [docs/contributing/repo_layout.md](docs/contributing/repo_layout.md)

---

## License

Dual-licensed under MIT or Apache-2.0, at your option.
See [LICENSE-MIT](LICENSE-MIT) and [LICENSE-APACHE](LICENSE-APACHE).
