# 開発環境セットアップ

- 更新日: 2025-12-21

## 1. 必要なもの

- Rust toolchain（stable）
- （必要なら）Ruby / Python

## 2. ビルド

```bash
cargo build -p clove-lang
cargo test
```

## 3. LSP

```bash
cargo build -p clove-lsp
```

## 4. VS Code 拡張

拡張の README を参照（[packages/vscode-clove](/packages/vscode-clove)）。

---
<!-- NAV:START -->
**前へ:** [実行時設定（use / use-syntax）](../advanced/runtime_settings.ja.md)
**次へ:** [リポジトリ構成](repo_layout.ja.md)
<!-- NAV:END -->

