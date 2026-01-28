# docs/ について

英語版（公式）: `README.md`  
※ 日本語版は `README.ja.md` にまとめています。

このディレクトリは **Clove 言語本体 + ツール群** のドキュメントをまとめたものです。

- 更新日: 2026-01-27
- 原則: **実装に準拠**（仕様案・未実装は「予定」として明記）

## 構成

- [docs/index.md](/docs/index.ja.md) … 目次（入口）
- [docs/getting_started.md](/docs/getting_started.ja.md) … まず動かす
- [docs/language/](/docs/language/) … 言語仕様（読書き・評価・構文糖）
- [docs/tooling/](/docs/tooling/) … CLI / REPL / fmt / build / LSP / VS Code
- [docs/advanced/](/docs/advanced/) … 設計ノート・将来の型AOTなど
- [docs/contributing/](/docs/contributing/) … 開発/貢献ガイド

## ドキュメント運用ルール

1. 「いま動く」ことが第一。動かない例は載せない。
2. 仕様案は **“予定” セクション** に隔離する。
3. 例は REPL で貼ってすぐ試せる形にする。
4. 大きい仕様変更（構文・namespace など）は、
   - [docs/index.md](/docs/index.ja.md) の目次
   - 影響ページの *互換性/移行* 節
   を同時に更新する。

## ドキュメント生成物との関係

- API 仕様（関数ごとの細かい説明）は REPL の `:doc` / `doc` を優先。
- [docs/](/docs/) は「概念/設計/使い方」中心（なぜそうなるか、落とし穴、例）。
