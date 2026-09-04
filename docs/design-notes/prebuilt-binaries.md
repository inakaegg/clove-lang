# Prebuilt binaries ship without embedded Ruby / Python

🇯🇵 日本語ドキュメント: [prebuilt-binaries.ja.md](prebuilt-binaries.ja.md)

- Updated: 2026-09-04

## Decision

The `clove` binaries attached to GitHub Releases are built with
`--no-default-features --features repl_guard`. They contain no embedded Ruby or
Python. Everything else — the interpreter, `clove fmt`, `clove build`, native
plugins, the REPL — is the same as a source build.

```clojure
(println $rb{ RUBY_VERSION })
;; prebuilt binary => Runtime error: unknown foreign tag: rb
```

The binary starts normally; the error appears only when a program reaches a
`$rb{...}` or `$py{...}` form. Anyone who needs the embedding installs from
source with `cargo install --git https://github.com/inakaegg/clove-lang --locked clove-lang`,
which links against the Ruby and Python already on that machine.

The macOS binary is signed with a Developer ID certificate and notarized from
the first release after the signing secrets are registered in CI; releases
before that are unsigned. The signature carries one hardened
runtime entitlement, `com.apple.security.cs.disable-library-validation`, so that
native plugins keep working (see Consequences).

## Reasoning

**Embedded Ruby / Python is resolved before `main` runs.** The Ruby bridge uses
`magnus` + `rb-sys` and the Python bridge uses `pyo3`. Both link `libruby` and
`libpython` at build time. On a machine that lacks the exact library, the dynamic
loader fails before the program starts; there is no point at which `clove` could
print a warning and continue.

**Both the version and the location are baked in.** The minor version (for
example Ruby 3.4, Python 3.12) is an ABI choice fixed at compile time. On macOS
the library's install name — an absolute path — is also written into the binary.
A build made against `~/.rbenv/versions/3.4.1/lib/libruby.3.4.dylib` only starts
on a machine that has that path. Homebrew, rbenv, pyenv, and the python.org
framework all put the same version in different places, so "Ruby 3.4 required"
is not a sufficient statement of the requirement.

**A binary that never links the libraries has none of these problems.** The
`ruby` and `python` Cargo features are optional dependencies. Turning them off
produces a binary whose only dynamic dependencies are the system libraries. It
runs on any Apple Silicon Mac or any x86_64 Linux with glibc 2.35 or newer.

## Alternatives not taken

**Ship a full build pinned to Homebrew's `ruby@3.4` and `python@3.12`.** This
works, and the requirement could be stated honestly in the README. It was set
aside because it turns "download and run" into "install two Homebrew formulae
first, and only those", and the binary silently stops working when Homebrew
moves the formulae.

**A launcher plus two binaries.** A small launcher would read a config file,
check whether the configured `libruby` / `libpython` exist, set
`DYLD_LIBRARY_PATH`, and exec the full binary — or warn and exec the
embedding-free one. This gives a configurable library *path*, but not a
configurable *version*: each minor version still needs its own full binary. It
also needs the `allow-dyld-environment-variables` entitlement under the hardened
runtime. Judged not worth the moving parts for the current audience.

**Move the Ruby / Python bridges into plugins loaded with `dlopen`.** The core
binary would stop linking the libraries and load a bridge plugin on first use,
failing softly if it cannot. This is the cleanest long-term shape, but it is a
functional change to the bridges and to the plugin API, not a packaging change.
It can be revisited if prebuilt users ask for the embedding.

**Not signing the macOS binary.** Since macOS 15, an unsigned executable that
carries the quarantine attribute is blocked on first launch, even from
Terminal, until the user allows it in System Settings. Users who download
through a browser would have to do that, or strip the attribute by hand.

## Consequences

**Native plugins require a relaxed hardened runtime.** The hardened runtime that
notarization requires enables *library validation* by default: the process may
only load dylibs signed by the same Team ID or by Apple. `clove` loads
user-built plugins with `dlopen` — not only with `--allow-native-plugins`, but
by default from `<project>/plugins/`, `<project>/.clove/plugins/`,
`$CLOVE_HOME/plugins/`, and lock-verified package plugin directories
(`crates/clove-lang/src/native_policy.rs`), and `clove plugin meta` /
`clove pkg install` open a plugin to read its metadata without going through
that policy at all. With library validation on, every one of those paths would
work in a source build and fail in the prebuilt binary. The
`disable-library-validation` entitlement removes that difference.

**What the relaxation means.** A prebuilt `clove` run inside an untrusted
project directory will load a dylib placed under that project's `plugins/` —
exactly as a source build does. Signing does not widen that behaviour, and the
policy in `native_policy.rs` is unchanged. On Apple Silicon the dylib itself
still needs at least an ad-hoc signature; `cargo` and `cc` add one
automatically.

**No stapled ticket.** A single Mach-O executable cannot carry a stapled
notarization ticket. Gatekeeper therefore asks Apple online the first time a
quarantined copy runs. Extracting with `tar` on the command line does not set
the quarantine attribute; extracting through Finder does. `xattr -d
com.apple.quarantine ./clove` clears it if needed.

**`clove build` still needs a C compiler** on the user's machine (Xcode Command
Line Tools on macOS), because the native build path shells out to `cc`.

**Tests cannot run with the embedding disabled.** Several integration tests in
`crates/clove-lang/tests/` reference `clove_ruby` and `clove_python` without a
`cfg` guard. The release workflow only builds; the test suite continues to run
with default features in CI.

## How to verify

```bash
# The prebuilt binary links no Ruby or Python
otool -L ./clove | grep -E 'ruby|python'      # (no output)

# A source build with default features does
otool -L ~/.cargo/bin/clove | grep -E 'ruby|python'

# The foreign tags fail at run time, not at start-up
./clove -e '(println (+ 1 2))'                # => 3
./clove -e '(println $rb{ 1 })'               # Runtime error: unknown foreign tag: rb

# The signed macOS binary carries the entitlement
codesign -d --entitlements - ./clove | grep disable-library-validation
```
