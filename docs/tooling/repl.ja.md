# REPL ガイド

- 更新日: 2026-01-14

Clove の REPL は “評価 + 補完 + doc 表示” を統合した対話環境です。

## 1. 起動

```bash
clove --repl
```

## 2. 主要コマンド

- `:help` … ヘルプ
- `:env` / `:vars` … 現在の変数一覧
- `:doc SYMBOL` … ドキュメント
- `:load FILE` … ファイルを評価
- `:source PATH` … 入力の source 名を設定（エラー位置表示や LSP 連携に重要）
- `:lang TAG` … foreign blocks のデフォルト言語を設定（例: `rb`, `py`）
- `:whereami` … 現在の “source” を表示
- `:backtrace` … 直近エラーのバックトレース
- `:continue` … エラー REPL 中の復帰
- `:q` … 終了
- `(nav QUERY ...)` … 名前/ドキュメントの統合検索（REPL で見やすく表示）

## 3. `:source` と `:load` の違い

- `:source` は “以後の入力がどのファイル由来か” を **表示/位置情報のために** 設定します。
- `:load` は実際にファイルを読んで評価します。

VS Code から “Send Selection to REPL” を使う場合、
`source` 設定がズレると警告が出たり、LSP のジャンプが不安定になることがあります。

## 4. dot-chain 継続（REPL補助）

REPL では **直前の評価結果** が `*repl-last*` に保存されます。  
トップレベルで `.(...)` を書いた場合、暗黙で `*repl-last*.(...)` として解釈されます。

```clojure
(range 10)
.(take 3 ?)
```

また、行末 `.` は「継続」を示す扱いになります。  
次の行は `.(...)` でも `map(inc)` のような **ドット無し** でもOKです。

```clojure
(range 10).
.(take 3 ?)
```

```clojure
(range 10).
map(inc)
```

## 5. doc / source

REPL 上で

- `(doc 'symbol)`
- `(source 'symbol)`

も利用できます。

## 6. nav / lookup（統合検索）

`nav`（alias: `lookup`）は **namespace / var / doc** を一括検索します。

```clojure
(nav 'disj)                 ; 全部検索
(nav "disj" :var)            ; var だけ
(nav /take|drop/ :var :doc)  ; regex もOK
```

- `QUERY` は Symbol / String / Regex を受け付けます（`/.../` が正統、曖昧な場合は `#/.../`）
- `:ns :var :doc` を必要に応じて指定できます（省略時は全部）
- 結果は件数が多い場合 **自動 pager** で表示されます
- 一致箇所は色ハイライト、`[name,doc]` のようなバッジで一致種別が分かります

## 7. エラー REPL（デバッグ補助）

`(use repl-on-error true)` を有効にすると、エラー発生時に REPL を起動できます。  
詳細: [実行時設定](../advanced/runtime_settings.ja.md)

エラー REPL では以下の変数が利用できます。

- `?` / `?v` / `*?` / `*1` … 対象値（先頭引数）
- `*f` / `?f` … 呼び出し関数
- `*args` / `?args` … 呼び出し引数（vector）
- `*call` / `?call` … 呼び出し情報（ファイル/位置/フォーム）
- `*e` / `?e` … エラー文字列

`(repl x)` でも `?` / `?v` / `*?` / `*1` はセットされます。

## 8. REPL を呼び出すフォーム

```clojure
(repl)         ; その場で REPL を開く
(repl value)   ; value を ? などにセットして開く
(debug)        ; repl の別名
(break)        ; 簡易ブレークポイント（デバッグ REPL）
```

- `break` は “実行を止めてその場で REPL を開く” 用途です。
- いずれも `:q` / `:quit` で抜けられます。

---
<!-- NAV:START -->
**前へ:** [言語の基礎](../language/basics.ja.md)
**次へ:** [Reader / 構文一覧](../language/reader_syntax.ja.md)
<!-- NAV:END -->

