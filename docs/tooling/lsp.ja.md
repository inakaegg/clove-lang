# LSP（clove-lsp）

- 更新日: 2025-12-21

`clove-lsp` は Clove のための最小 LSP サーバです。

## 1. 提供機能（現状）

- Diagnostics（構文エラー/未定義など）
- Hover（doc / signature）
- Go to Definition（定義ジャンプ）

> 実装は進行中です。複雑な namespace の混在（`/` 区切り等）があると精度が落ちます。

## 2. 使い方（VS Code）

VS Code 拡張が `clove-lsp` を起動し、`.clv` の解析を行います。

- [VS Code 拡張](vscode.ja.md)

## 3. トラブルシュート

- `ns` とファイル配置が一致しない → 警告/解決失敗
- 古い `aaa/bbb` 形式 → 定義追跡が崩れることがある

このあたりは [名前空間](../language/namespaces.ja.md) を参照。

---
<!-- NAV:START -->
**前へ:** [Build（--opt=typed / --static / embed）](build.ja.md)
**次へ:** [VS Code 拡張](vscode.ja.md)
<!-- NAV:END -->

