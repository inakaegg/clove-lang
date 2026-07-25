<p align="center">
  <img src="assets/clove_logo.png" alt="Clove logo" width="512">
</p>

# Clove

[![CI](https://github.com/inakaegg/clove-lang/actions/workflows/ci.yml/badge.svg)](https://github.com/inakaegg/clove-lang/actions/workflows/ci.yml)

英語版（公式）: [README.md](README.md)

Clove は Clojure に着想を得た小さな Lisp です。**S 式**をベースに、
**軽量な型（`deftype` / `defenum`）とパターンマッチ**、**Ruby / Python のインライン埋め込み**を
1 つの言語にまとめ、スクリプトのネイティブバイナリ化にも対応しています。

> **ステータス: experimental / WIP** — 構文・API・CLI は今後も変わる可能性があります。
> 詳しい仕様は [`docs/`](docs/)、動くコードは [`examples/`](examples/) に集約しています。
> この README は意図的に短く、例を中心に構成しています。

---

## 短い例で見る Clove

### Hello

```clojure
(ns examples::hello)

(println "Hello from Clove!")
```

```bash
clove examples/hello.clv
```

---

### ドットチェーン（`.(...)`）とプレースホルダ `?`

`as->` のように値を流し込む処理を、`expr.( ... )` で短く書けます。
`?` が「直前の値が入る場所」です。

```clojure
(inc 123).(+ 1 ?).(repeat 3 ?).(map inc ?)
; => [126 126 126]
```

`*?` は直前の値を引数列として展開します（内部的には `apply`）。

```clojure
[inc (range 10)].(map *?)
; => [1 2 3 4 5 6 7 8 9 10]
```

---

### `?` プレースホルダ（インライン関数）

`?` を含む式は、その場で小さな無名関数になります。

```clojure
(map (+ ? 10) (range 5))
; => [10 11 12 13 14]

(filter (not= :skip ?) [:ok :skip :ok])
; => [:ok :ok]
```

> 詳細: [`docs/language/reader_syntax.md`](docs/language/reader_syntax.md)

---

### map 省略記法 + indexer（`[]`）

Clojure 風の `{:x 1}` に加えて、JSON 風の `{name: "Taro"}` も書けます。

```clojure
(def user {name: "Taro" age: 30})
user[:name] ; => "Taro"
```

indexer は単純な要素取得にとどまらず、柔軟に使えます。

```clojure
(def xs [10 11 12 13 14 15])

xs[0]        ; => 10
xs[-1]       ; => 15
xs[99 || :ng]; => :ng   ; 見つからなければ既定値

xs[[0 2 4]]  ; => [10 12 14]  ; gather（複数 index）
xs[1,3,5]    ; => [11 13 15]  ; カンマ区切りの複数 index
xs[..3]      ; => [10 11 12 13] ; open range
xs[2..]      ; => [12 13 14 15]
xs[1...3]    ; => [11 12]       ; 末尾 exclusive
xs[(+ 1 2)]  ; => 13  ; indexer 内には式も書ける
```

> range、`-foo` ルール、map/set/get-in などの詳細: [`docs/language/indexer.md`](docs/language/indexer.md)

---

### `deftype` / `defenum` / `match`（軽量な型と分岐）

`protocol` や `multimethod` の代わりに、データ + パターンマッチで書くスタイルを重視しています。

```clojure
(deftype Dog  {:name :string :age :int})
(deftype Cat  {:name :string :lives :int})
(defenum Pet Dog Cat)

(defn pet-name [p]
  (match p
    (Dog {:name n}) n
    (Cat {:name n}) n))

(pet-name (Dog {:name "Pochi" :age 3}))
; => "Pochi"
```

---

### Ruby / Python のインライン埋め込み

外部言語の埋め込みは Clove の特徴の 1 つです。
Ruby がデフォルトの外部言語で、`$rb{...}` とその別名 `${...}` は同じ意味になります。

```clojure
(defn ruby-version []
  $rb{ RUBY_VERSION })

(defn py-sqrt [x]
  $py{
    import math
    math.sqrt(x)
  })

(println (ruby-version))
(println (py-sqrt 9)) ; => 3.0
```

JSON / YAML は reader タグで直接書けます。
JSON のキーは文字列のままなので、`:host` ではなく `"host"` で参照します。

```clojure
(def config
  #json{"host":"localhost","port":8080})

config["host"] ; => "localhost"
```

---

### 並行処理プリミティブ

`atom` / `chan` / `promise` / `task` / `future` / `agent` を最小構成で用意しています。

```clojure
(def c (chan 1))
(chan-put! c :ok)
(chan-take! c) ; => :ok
```

より踏み込んだ例: [`examples/concurrency/`](examples/concurrency/)、[`docs/language/concurrency.md`](docs/language/concurrency.md)

---

## 次に読む場所

- まず動かす: [`docs/getting_started.md`](docs/getting_started.md)
- ドキュメントの入口: [`docs/index.md`](docs/index.md)
- 動くサンプル集: [`examples/`](examples/)
- 例の実行:

  ```bash
  clove --main examples/concurrency/async_scope_nested.clv
  ```

---

## インストール / ビルド

Clove は Rust ワークスペースとして開発しており、ソースからのビルドが基本の導入経路です。

### ソースから

```bash
git clone https://github.com/inakaegg/clove-lang clove
cd clove

# CLI を PATH へインストール（エディタ補完/LSP を使う場合は clove-lsp も）
cargo install --path crates/clove-lang --force
cargo install --path crates/clove-lsp --force

# 動作確認
clove --repl
clove examples/hello.clv

# ネイティブバイナリ生成（phase2 C backend）
clove build examples/build/high_value_report.clv --out target/clove/bin/high_value_report
./target/clove/bin/high_value_report
```

インストールせずに試す場合は `cargo build -p clove-lang --release` でビルドし、
`./target/release/clove` を直接実行してください。

`clove-lsp` は stdio で動く言語サーバーです。通常はエディタ拡張や LSP クライアントから起動し、
ターミナルから直接起動するものではありません。

### clone せずにインストール

```bash
cargo install --git https://github.com/inakaegg/clove-lang --locked --package clove-lang --bin clove
```

### Ruby 埋め込みを使う場合の注意

Ruby ブリッジは `rb-sys` / `magnus` を経由するため、**Ruby 3.x 系**が必要です。
macOS 付属の `/usr/bin/ruby`（2.6）などの古い Ruby ではビルドに失敗することがあります。

### `clove build` について

`examples/build/high_value_report.clv` は `clove build` での生成・実行を確認済みのサンプルで、
簡単な売上集計を行います。現在の `examples/` にはインタプリタ向けの例と build 向けの例が
混在しており、すべての例が `clove build` を通る状態ではありません。

---

## CLI の概要

- `clove` / `clove --repl` — REPL を起動
- `clove -e '(+ 1 2 3)'` — 式を 1 つだけ評価
- `clove path/to/file.clv` — ファイル実行（最後の値を表示）
- `clove --main path/to/file.clv` — ファイル評価後に `-main` を呼ぶ（examples で使用）
- `clove --repl path/to/file.clv` — ファイル評価後、そのまま REPL に入る
- `clove fmt ...` — フォーマッタ
- `clove build ...` — ネイティブバイナリ生成
- ネイティブプラグインは `plugins/` 同梱が前提（`<project>/plugins` / `~/.clove/plugins` はデフォルト許可、pkg 配下は lock の sha256 一致が必須。詳細は [`docs/tooling/cli.md`](docs/tooling/cli.md)）

すべてのオプションは `clove --help` / `clove build --help` を参照してください。

---

## VS Code 拡張

このリポジトリには VS Code 拡張 `vscode-clove` を同梱しています
（ソース配布。現時点では Marketplace 未公開）。
シンタックスハイライト、S 式単位の選択拡張、REPL への送信、`clove fmt` 連携などを提供します。

設定やキーバインド例: [`packages/vscode-clove/README.md`](packages/vscode-clove/README.md)

---

## Clojure との主な違い

- `protocol` / `multimethod` は持たない（`deftype` / `defenum` / `match` で代替）
- マクロ（`defmacro` / 準クォート）は未実装
- `/.../` をデフォルトの正規表現リテラルとして採用（曖昧な場合は `#/.../`）
- `10ms` などの Duration リテラルを標準サポート
- Ruby / Python をタグ / ブロックで埋め込める

---

## Contributing

- 開発環境: [`docs/contributing/dev_setup.md`](docs/contributing/dev_setup.md)
- テスト: [`docs/contributing/testing.md`](docs/contributing/testing.md)
- リポジトリ構成: [`docs/contributing/repo_layout.md`](docs/contributing/repo_layout.md)

---

## ライセンス

MIT または Apache-2.0 のデュアルライセンスです（利用者が選択）。
[LICENSE-MIT](LICENSE-MIT) と [LICENSE-APACHE](LICENSE-APACHE) を参照してください。
