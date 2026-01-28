# Troubleshooting

Japanese version: [troubleshooting.ja.md](troubleshooting.ja.md)

This page collects common issues and quick fixes.

## 1. REPL / evaluation issues

- **Namespace mismatch warnings**
  - If `ns` and file path do not match, REPL/LSP may warn.
  - Align `myapp/core.clv` with `(ns myapp::core)`.

- **`:source` confusion**
  - If error locations look wrong, set `:source` in REPL or use VS Code “Send Selection to REPL”.

## 2. `clove fmt` issues

- **Formatter fails**
  - Try `clove fmt --stdin` in a terminal and check the error.
  - Confirm the command path in VS Code settings.

- **Config not applied**
  - Ensure `clovefmt.toml` is discoverable from the start directory.
  - Use `--print-config` to see resolved config.

## 3. LSP / editor issues

- **Go-to-definition does not work**
  - Check namespace/path alignment and avoid legacy `aaa/bbb` separators.
  - Use `::` consistently.

- **Completion is missing**
  - Ensure `clove-lsp` is built and available in PATH.

## 4. Native plugin issues

- **Plugin load denied**
  - By default, only trusted directories are allowed.
  - Use `--allow-native-plugins` if you must load from arbitrary paths.

- **Plugin found but fails**
  - Verify platform-specific filename (`.dylib` / `.so` / `.dll`).
  - Ensure the runtime library (e.g., SDL2) is installed.

---
<!-- NAV:START -->
**Previous:** [VS Code Extension](vscode.md)
**Next:** [Types (design, type hints, typed build assumptions)](../advanced/typing.md)
<!-- NAV:END -->

