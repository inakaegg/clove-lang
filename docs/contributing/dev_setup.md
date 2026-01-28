# Development environment setup

Japanese version: [dev_setup.ja.md](dev_setup.ja.md)

- Updated: 2025-12-21

## 1. Requirements

- Rust toolchain (stable)
- (Optional) Ruby / Python

## 2. Build

```bash
cargo build -p clove-lang
cargo test
```

## 3. LSP

```bash
cargo build -p clove-lsp
```

## 4. VS Code extension

See the extension README ([packages/vscode-clove](/packages/vscode-clove)).

---
<!-- NAV:START -->
**Previous:** [Runtime settings (use / use-syntax)](../advanced/runtime_settings.md)
**Next:** [Repository layout](repo_layout.md)
<!-- NAV:END -->

