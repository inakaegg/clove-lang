<p align="center">
  <img src="assets/clove_logo.png" alt="Clove logo" width="512">
</p>

# Clove

英語版（公式）: [README.md](README.md)

Clove は **Clojure ライクな S 式**をベースにしつつ、  
**軽量な型（deftype / defenum）**と **パターンマッチ**、さらに **Ruby / Python の埋め込み**までをひとつにまとめた、小さな言語です。

Clojure のような書き味を保ちながら、必要な場面では「外の世界（Ruby/Python）」も気軽に混ぜられる——  
そんな “風味（flavor）” を狙って作っています。

> **ステータス: experimental / WIP** — 構文・API・CLI は今後も変わる可能性があります。  
> 詳しい仕様は [`docs/`](docs/)、動くコードは [`examples/`](examples/) に集約しています。  
> README は「雰囲気がつかめる程度」にとどめます。

---

## まずは雰囲気がわかる短い例

### Hello
```clojure
(ns examples::hello)

(println "Hello from Clove!")
```

```bash
clove examples/hello.clv
```

---

### ドットチェーン（`.(...)`）と穴あき `?`

`as->` の「値を流し込む」書き方を、`expr.( ... )` で短く書けます。
`?` が「直前の値が入る場所」です。

```clojure
(inc 123).(+ 1 ?).(repeat 3 ?).(map inc ?)
; => (126 126 126)
```

`*?` は「直前の値を引数として展開」します（内部的には `apply`）。

```clojure
[inc (range 10)].(map *?)
; => (1 2 3 4 5 6 7 8 9 10)
```

---

### `?` のプレースホルダ（その場関数）

`?` を含む式は「その場で小さな関数」に変換されます。

```clojure
(map (+ ? 10) (range 5))
; => (10 11 12 13 14)

(filter (not= :skip ?) [:ok :skip :ok])
; => (:ok :ok)
```

> `?` の詳細は [`docs/language/reader_syntax.md`](docs/language/reader_syntax.md) を参照してください。

---

### map 省略記法 + indexer（`[]`）

Clojure 風 `{:x 1}` に加えて、JSON っぽい `{name: "Taro"}` も使えます。

```clojure
(def user {name: "Taro" age: 30})
user[:name] ; => "Taro"
```

indexer は “取り出し” だけじゃなく、少し柔軟に使えます。

```clojure
(def xs [10 11 12 13 14 15])

xs[0]        ; => 10
xs[-1]       ; => 15
xs[99 || :ng]; => :ng   ; 見つからなければ既定値

xs[[0 2 4]]  ; => [10 12 14]  ; gather（複数 index）
xs[1,5,7]    ; => [11 15 17]  ; カンマ区切りの複数 index
xs[..3]      ; => [10 11 12 13] ; open range
xs[2..]      ; => [12 13 14 15]
xs[1...3]    ; => [11 12]       ; 末尾 exclusive

xs[(+ 1 2)]  ; => 13  ; indexer 内は式も書ける
```

> indexer の詳細（range、`-foo` ルール、map/set/get-in など）は [`docs/language/indexer.md`](docs/language/indexer.md) にまとめています。

---

### `deftype` / `defenum` / `match`（軽量な型と分岐）

`protocol` や `multimethod` の代わりに、データ＋パターンマッチで書くスタイルを重視しています。

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

### Ruby / Python をその場で混ぜる

Clove の特徴のひとつが「外部言語の埋め込み」です。
Ruby はデフォルト外部言語なので、埋め込みは `$rb{...}` を基本に書きつつ `${...}` でも書けます（同義）。

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

JSON / YAML は reader タグでも読めます。

```clojure
(def config
  #json{"host":"localhost","port":8080})

config[:host] ; => "localhost"
```

---

### 並行プリミティブ（最小）

`atom` / `chan` / `promise` / `task` / `future` / `agent` などを用意しています。

```clojure
(def c (chan 1))
(chan-put! c :ok)
(chan-take! c) ; => :ok
```

より踏み込んだ例は [`examples/concurrency/`](examples/concurrency/) と [`docs/language/concurrency.md`](docs/language/concurrency.md) にあります。

---

## 次に読む場所

* まず動かす: [`docs/getting_started.md`](docs/getting_started.md)
* ドキュメントの入口: [`docs/index.md`](docs/index.md)
* 動くサンプル集: [`examples/`](examples/)
* 例の実行:

  ```bash
  clove --main examples/concurrency/async_scope_nested.clv
  ```

---

## インストール / ビルド

### いちばん確実（ソースから）

現時点では Rust ワークスペースとして開発中です。

```bash
git clone https://github.com/inakaegg/clove-lang clove
cd clove
cargo build -p clove-lang --release
./target/release/clove --help
```

PATH に入れて使いたい場合は次のコマンドでインストールできます。

```bash
cargo install --path crates/clove-lang --force
clove --repl
```

### Rust が入っているなら（clone せずに）

```bash
cargo install --git https://github.com/inakaegg/clove-lang --locked --package clove-lang --bin clove
```

### Ruby 埋め込みを使う場合の注意

Ruby ブリッジは `rb-sys` / `magnus` を経由するため、**Ruby 3.x 系**を使ってください
（macOS 付属の `/usr/bin/ruby` 2.6 などだとビルドに失敗することがあります）。

---

## CLI の使い方（要点）

* `clove` / `clove --repl` : REPL
* `clove -e '(+ 1 2 3)'` : 1 式だけ評価
* `clove path/to/file.clv` : ファイル実行（末尾の式を表示）
* `clove --main path/to/file.clv` : eval 後に `-main` を呼ぶ（examples 用）
* `clove --repl path/to/file.clv` : eval 後、そのまま REPL に入る
* `clove fmt ...` : フォーマッタ
* `clove build ...` : ネイティブバイナリ生成（オプションあり）
* ネイティブプラグインは `plugins/` 同梱が前提（`<project>/plugins` / `~/.clove/plugins` はデフォルト許可、pkg 配下は lock の sha256 一致が必須。詳細は [`docs/tooling/cli.md`](docs/tooling/cli.md)）

詳しいオプションは `clove --help` / `clove build --help` を参照してください。

---

## VS Code 拡張

このリポジトリには VS Code 拡張 `vscode-clove` が含まれています。
主にシンタックスハイライト、S 式の選択拡張、REPL 送信、`clove fmt` 連携などを提供します。

設定やキーバインド例は [`packages/vscode-clove/README.md`](packages/vscode-clove/README.md) を参照してください。

---

## Clojure との主な違い（短く）

* `protocol` / `multimethod` は持たない（代わりに `deftype` / `defenum` / `match` を重視）
* 現時点ではマクロ（`defmacro` / 準クォート）は未実装
* `/.../` をデフォルトの正規表現リテラルとして採用（曖昧な場合は `#/.../`）
* `10ms` などの Duration リテラルを標準サポート
* Ruby / Python をタグ/ブロックで埋め込める

---

## Contributing

* 開発環境: [`docs/contributing/dev_setup.md`](docs/contributing/dev_setup.md)
* テスト: [`docs/contributing/testing.md`](docs/contributing/testing.md)
* リポジトリ構成: [`docs/contributing/repo_layout.md`](docs/contributing/repo_layout.md)

---

## ライセンス

MIT または Apache-2.0 のデュアルライセンスです（利用者が選択）。  
[LICENSE-MIT](LICENSE-MIT) と [LICENSE-APACHE](LICENSE-APACHE) を参照してください。
