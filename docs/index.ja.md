# Clove ドキュメント

- 更新日: 2026-01-14

Clove は Clojure 風の Lisp を土台にしつつ、
**dot-chain / indexer / foreign blocks / OOP 記法** など、実装寄りの機能を追加した言語です。

## まず読む

- [Getting Started](getting_started.ja.md)
- [言語の基礎](language/basics.ja.md)
- [REPL ガイド](tooling/repl.ja.md)

## 言語

- 基礎
  - [Reader / 構文一覧](language/reader_syntax.ja.md)
  - [リテラル（数値・文字列など）](language/literals.ja.md)
  - [コレクション（list/vector/map/set）](language/collections.ja.md)
  - [関数（defn / fn / #() / 可変長）](language/functions.ja.md)
  - [デストラクチャリング](language/destructuring.ja.md)
  - [制御構文（if/when/cond/for/while/try など）](language/control_flow.ja.md)
  - [スレッディング / パイプ系（-> / cond-> / some->）](language/threading.ja.md)
  - [名前空間（ns/require/resolve/load など）](language/namespaces.ja.md)

- 構文糖
  - [indexer: `x[0]`, `x[0 1]`, `x[0,1]`, `x[0||d]`, `a..b`](language/indexer.ja.md)
  - [dot-chain: `x.(f ?)`, `x.(f *?)` など](language/dot_chain.ja.md)
  - [OOP 記法（`obj.method(...)`）](language/oop_syntax.ja.md)
  - [OOP メソッド（method / where / self）](language/oop_methods.ja.md)
  - [OOP メソッド新記法クイックリファレンス](language/oop_methods_quickref.ja.md)
  - [マップ省略記法（JS 準拠）](language/map_shorthand.ja.md)
  - [正規表現 / Duration リテラル](language/regex_duration.ja.md)

- 主要機能
  - [並行/非同期（chan / future / go-loop / scope-loop / async-scope）](language/concurrency.ja.md)
  - [外部連携（foreign blocks / Ruby / Python）](language/interop_foreign.ja.md)
  - [型/enum/match（deftype/defenum/match）](language/types_enum_match.ja.md)
  - [標準ライブラリ `std` の使い方](language/stdlib.ja.md)

## ツール

- [CLI（clove / clove fmt / clove build）](tooling/cli.ja.md)
- [パッケージ管理（Phase1）](packages.ja.md)
- [Formatter（fmt / rubocop/syntax_tree）](tooling/formatter.ja.md)
- [Build（--opt=typed / --static / embed）](tooling/build.ja.md)
- [LSP（clove-lsp）](tooling/lsp.ja.md)
- [VS Code 拡張](tooling/vscode.ja.md)
- [トラブルシュート](tooling/troubleshooting.ja.md)

## サンプル

- [SDL2 examples](../examples/sdl2/README.md)

## 応用トピック

- [型（設計・型ヒント・typed build の前提）](advanced/typing.ja.md)
- [名前空間設計ノート](advanced/namespaces_design.ja.md)
- [メモ化と永続キャッシュ（memo / memoize）](advanced/memoization.ja.md)
- [実行時設定（use / use-syntax）](advanced/runtime_settings.ja.md)

## 開発/貢献

- [開発環境セットアップ](contributing/dev_setup.ja.md)
- [リポジトリ構成](contributing/repo_layout.ja.md)
- [テスト方針](contributing/testing.ja.md)
- [ドキュメントの書き方](contributing/docs_style.ja.md)

## FAQ / 用語

- [FAQ](faq.ja.md)
- [用語集](glossary.ja.md)
