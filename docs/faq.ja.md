# FAQ

- 更新日: 2025-12-21

## Q. Clojure と同じですか？

似ていますが **同じではありません**。
Clove は Clojure の文法/思想を参考にしつつ、
- dot-chain
- indexer
- foreign blocks
- OOP 記法
などを “Reader レベルで” 拡張しています。

## Q. doc はどこを見ればいい？

- 概念/使い方 → [docs/](/docs/)
- 関数ごとの正確な引数 → REPL の `:doc` / `doc`

## Q. `x[0]` は何に展開される？

Reader の構文糖で、概ね `(clove.syntax::index-get x [0])` のような形に下ろされ、
その後評価時に `get` / `get-in` 相当として動作します。
`x[1,2]` のような **カンマ区切り**は `(clove.syntax::index-get-many ...)` に展開され、
対象がシーケンスなら `nth` 相当、マップ/ネスト構造なら `get-in` 相当で複数パスを返します
（`x[1 2]` とは別物）。

詳細: [indexer](language/indexer.ja.md)

## Q. foreign blocks が動かない

セキュリティ上、デフォルトでは foreign engine をロードしません。
- REPL: `:lang rb` / `:lang py` を適切に設定
- 実行: `clove --auto-fallback`（フォールバック）や build 時の `--embed-*` を検討

詳細: [外部連携](language/interop_foreign.ja.md), [Build](tooling/build.ja.md)

## Q. 何が “special form” ですか？

評価器が特別扱いする構文です（例: `if`, `let`, `fn`, `quote`, `try`, `async-scope`）。

一覧: [制御構文](language/control_flow.ja.md)

---
<!-- NAV:START -->
**前へ:** [ドキュメントの書き方](contributing/docs_style.ja.md)
**次へ:** [用語集](glossary.ja.md)
<!-- NAV:END -->

