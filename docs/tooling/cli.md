# CLI tools (`clove` / `clove fmt` / `clove build`)

Japanese version: [cli.ja.md](cli.ja.md)

This section describes Clove's command-line tools.

* `clove` ... REPL / script execution / one-liner eval
* `clove fmt` ... formatter
* `clove build` ... native binary build

> ⚠️ For exact option specs, follow `clove --help` / `clove build --help`.
> This page focuses on concepts and common patterns.

---

## 1. `clove` – REPL / scripts / one-liners

### 1.1 REPL

```bash
clove --repl
# or simply
clove
```

* Evaluate expressions interactively.
* REPL has extra features like error location and `:source`.

Common REPL commands:

```clojure
:help           ; REPL help
:doc map        ; show doc for a symbol
:doc ns::name    ; doc for fully-qualified symbol
:quit / :q      ; quit
```

### 1.2 Script execution

```bash
clove path/to/script.clv
```

* Loads the file following `ns` declarations and evaluates to the end.
* If there is a `(-main)` function, calling it as the entry point is recommended.

```clojure
(ns myapp::main)

(defn -main []
  (println "hello")
  (cli::exit 0))
```

### 1.3 One-liner eval (`-e`)

```bash
clove -e '(+ 1 2 3)'
# => 6
```

* Evaluates a single expression and prints the result.
* Useful for quick checks or shell scripts.

---

### 1.4 Native plugins (`require-native`)

Native plugins **deny arbitrary paths by default**.
However, the following trusted directories are automatically allowed:

* `<project>/plugins/`
* `<project>/.clove/plugins/`
* `~/.clove/plugins/`
* `~/.clove/pkgs/**/plugins/**` (sha256 must match `clove.lock.json`)

Adding `--allow-native-plugins` allows **all paths** (legacy behavior).

```bash
# Use plugins under project (no flag needed)
clove --main examples/sdl2/01_events_input.clv

# Load from arbitrary path (allow all)
clove --allow-native-plugins --plugin-dir ./plugins --main examples/sdl2/01_events_input.clv
```

* `--plugin-dir` interpretation depends on **current working directory** for relative paths.
* Search paths are basically **`--plugin-dir` (if set) / `CLOVE_PLUGIN_DIR` / `plugins/` next to the executable / `<project>` / `~/.clove`**.
  - If `clove.lock.json` exists, dependency package `plugins/<platform>/` are also added (sha256 match required).
  - `CLOVE_PLUGIN_DIR` supports multiple paths separated by OS delimiter (mac/Linux: `:`, Windows: `;`).
* If you run `clove` from PATH and omit `--plugin-dir`, it searches `~/.cargo/bin/plugins`.
  - This path is **not trusted**, so add `--allow-native-plugins` if needed.

SDL2 plugin example (external package):

```bash
clove pkg install --project <git-url> --pkg owner/clove-sdl2
clove --main examples/sdl2/01_events_input.clv
```

Packages are expected to bundle cdylib under `repo-root/plugins/<platform>/`.

File name conventions:

* macOS: `lib<name>.dylib` / `lib<name>_plugin.dylib`
* Linux: `lib<name>.so` / `lib<name>_plugin.so`
* Windows: `<name>.dll` / `<name>_plugin.dll`

Call on Clove side:

```clojure
(require-native "clove-sdl2")
(require clove-sdl2)
```

SDL2 add-on plugins:

```clojure
(require-native "clove-sdl2-image")
(require clove-sdl2-image)

(require-native "clove-sdl2-ttf")
(require clove-sdl2-ttf)

(require-native "clove-sdl2-mixer")
(require clove-sdl2-mixer)
```

> Note: Do not use SDL_mixer together with core queue-audio.

> Note: SDL2 is provided as an external package.
> `clove pkg install --project ...` installs it, and if sha256 matches `clove.lock.json`, it runs without flags.
> For other locations, add `--allow-native-plugins`.

### 1.5 Distributing SDL2 games (code / binary)

#### Code distribution (.clv + plugins)

* Users install `clove` and run the distributed `.clv`.
* If you bundle plugins (`libclove_sdl2_plugin.*`) under `<project>/plugins/`, no flags are needed.
  For arbitrary paths, run with `--allow-native-plugins`.

```bash
clove --plugin-dir ./plugins --main path/to/game.clv
```

#### Binary distribution (built via `clove build`)

* Binaries produced by `clove build` accept `--allow-native-plugins` and `--plugin-dir`.
* `plugins/` next to the binary is searched, but is **not trusted** by default.

```
mygame
plugins/
  libclove_sdl2_plugin.dylib
```

```bash
./mygame --allow-native-plugins --plugin-dir ./plugins
```

> Note: The SDL2 runtime library must be available on the target environment.

---

## 2. `clove fmt` – formatter

Subcommand to format Clove code.

```bash
# Format file in-place
clove fmt src/main.clv

# Read from stdin, output to stdout
clove fmt --stdin < src/main.clv

# Set indent width
clove fmt --stdin --indent 4 < src/main.clv

# Set line width (max chars per line)
clove fmt --stdin --width 100 < src/main.clv
```

Indent width is `--indent` (default 2), and line width is `--width` (default 120).

From VS Code, you can configure “Format Document” to call `clove fmt`.
(See [VS Code extension](vscode.md).)

---

## 3. `clove build` – native binary generation

`clove build` generates a native binary from Clove code.

```bash
clove build path/to/main.clv
```

High-level flow:

1. Read Clove source
2. Generate IR / LUT, etc.
3. Emit a binary linked with the Rust runtime

**Common option examples** (subject to change with implementation):

* Output file name: `--output ./target/app`
* Static link: `--static`, etc.
* Ruby/Python embed: `--embed-ruby`, `--embed-python`, etc.
* Optimization level: `--release`-like flag (if available)

For the exact option list:

```bash
clove build --help
```

---

## 4. CLI integration with VS Code / REPL

* VS Code extension internally calls `clove` / `clove fmt`.
* REPL integration assumes `clove --repl` in a terminal.

Recommended workflow:

1. Open a Clove workspace in VS Code
2. Use `Clove: Send Selection to REPL` for interactive exploration
3. Keep format with `clove fmt`
4. Finally build with `clove build` for distribution

---

## 5. Future extensions

* `clove doc` ... generate HTML from [docs/](/docs/)
* `clove new` ... create project from template
* `clove test` ... test runner command

There is room to expand CLI support commands,
but current design focuses on `clove` / `clove fmt` / `clove build`.

---

## Important: `clove fmt` outputs to stdout

Currently `clove fmt` **does not overwrite files**.

```bash
clove fmt foo.clv > foo.clv.new
mv foo.clv.new foo.clv
```

## Important: native plugins

When loading native plugins in `clove build` output or runtime,
the default is **trusted directories only**.

- `<project>/plugins/` / `<project>/.clove/plugins/` / `~/.clove/plugins/` are auto-trusted
- `~/.clove/pkgs/**/plugins/**` requires sha256 match in `clove.lock.json`
- For other paths, add `--allow-native-plugins`

> Where you load from (e.g., bundled plugins only) is critical for security design.

---
<!-- NAV:START -->
**Previous:** [Standard library `std`](../language/stdlib.md)
**Next:** [Package management (Phase1)](../packages.md)
<!-- NAV:END -->

