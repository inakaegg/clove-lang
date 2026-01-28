# リポジトリ構成

- 更新日: 2026-01-14

Clove は複数 crate と assets を含む workspace です。

- [crates/clove-core](/crates/clove-core) … Reader/Eval/標準関数のコア
- [crates/clove-lang](/crates/clove-lang) … CLI / REPL / build / fmt / プラグイン
- [crates/clove-lsp](/crates/clove-lsp) … LSP サーバ
- [data/clove_docs/](/data/clove_docs/) … docs データ（関数 doc のソース）
- [docs/](/docs/) … このドキュメント
- [examples/](/examples/) … 例

## ドキュメントの優先順位

ドキュメントは概ね以下の優先順で表示されます。

1. Rust 側 `fn_meta` 登録
2. [data/clove_docs/clove-docs.json](/data/clove_docs/clove-docs.json)
3. ランタイムの docstring

（REPL 補完/`:doc` 表示の話。）

---
<!-- NAV:START -->
**前へ:** [開発環境セットアップ](dev_setup.ja.md)
**次へ:** [テスト方針](testing.ja.md)
<!-- NAV:END -->

