# 実行時設定（use / use-syntax）

- 更新日: 2025-12-21

Clove は一部の機能を “設定でON/OFF” できます。

## 1. `use-syntax` / `use`

Reader レベルの拡張を制御します。

```clojure
(use-syntax dot-chain true)
(use-syntax indexer true)
(use-syntax dot-indexer true)
(use-syntax foreign-block true)
(use-syntax oop-syntax true)
(use-syntax map-refs true)
```

- map-refs は `(use map-refs true)` でも有効化できます（`use` は `use-syntax` の別名）。
- `true` で有効化
- `false` で無効化
- `:scope` は廃止済みで受け付けません

環境によってデフォルトが異なる可能性があるため、
チームで “このプロジェクトは常にON” と決めるなら、
entrypoint で明示すると安全です。

## 2. `repl-on-error`

エラー発生時に “その場で REPL を開く” 機能です。

```clojure
(use repl-on-error true)
```

- デバッグ時に便利
- CI や非対話環境では OFF 推奨

REPL からは `:continue` で復帰できます。

## 3. パッケージ設定ファイル（user.toml / user.clv）

設定は **パッケージ単位** で読み込まれます。
パッケージ root は「ファイルパスから親方向に辿って最初に `user.toml` または `user.clv` が見つかったディレクトリ」です。
見つからない場合は `<main>` 扱いになります。

読み込み順と優先順位は以下です。

1. `user.toml`（任意）
2. `user.clv`（任意。上書き）

### user.toml

```toml
[syntax]
dot-chain = true
dot-indexer = true
indexer = true
oop-syntax = true
map-refs = true
foreign = false
repl-on-error = true
```

- `syntax` セクションのキーは feature 名
- 値は `true` / `false` のみ

### user.clv

**`(use <feature> true|false)` だけ** を許可します。
それ以外のフォームがあるとエラーになります。

```clojure
(use dot-chain false)
(use repl-on-error true)
```

- `user.clv` は **設定専用** のため評価されません。`require` 等の任意コードは書けません。
- `std` の切り替え（別ライブラリへの差し替え）は `user.clv` ではできません。必要なら各 `ns` で `require` を明示してください。

---
<!-- NAV:START -->
**前へ:** [メモ化と永続キャッシュ（memo / memoize）](memoization.ja.md)
**次へ:** [開発環境セットアップ](../contributing/dev_setup.ja.md)
<!-- NAV:END -->

