# Getting Started

Japanese version: [getting_started.ja.md](getting_started.ja.md)

- Updated: 2025-12-21

This page shows the fastest path to: build Clove, run a script, and try the REPL.

## 1. Build

```bash
# repo root
cargo build -p clove-lang --release
```

The output binary is usually `target/release/clove`.

## 2. REPL

```bash
./target/release/clove --repl
```

Common REPL commands:

- `:help` ... help
- `:doc SYMBOL` ... documentation
- `:source PATH` ... set “which file this input came from” (for LSP/error display)
- `:load FILE` ... evaluate a file

See [REPL guide](tooling/repl.md) for details.

## 3. Run a script

### 3.1 Evaluate as-is

```bash
./target/release/clove path/to/app.clv
```

The last evaluated value is printed.

### 3.2 Call `-main` (script style)

```bash
./target/release/clove --main path/to/app.clv arg1 arg2
```

With `--main`, Clove evaluates the file and then calls the **`-main` function** if it exists.
(Arguments `arg...` are passed to `-main`.)

## 4. Format

```bash
./target/release/clove fmt path/to/app.clv > /tmp/app.formatted.clv
```

`clove fmt` writes to **stdout** (currently no in-place overwrite).

See [Formatter](tooling/formatter.md).

## 5. Build (executable)

```bash
./target/release/clove build path/to/app.clv
```

This generates a Rust project under `build/`, and you can run `cargo build --release` to produce an executable.

See [Build](tooling/build.md).

---
<!-- NAV:START -->
**Next:** [Language Basics](language/basics.md)
<!-- NAV:END -->

