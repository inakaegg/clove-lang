# 標準ライブラリ (`std`) 概要

Clove には、コア言語とは別に「電池込み」な標準ライブラリ `std` が付属します。`clove run` / REPL では `clove::core` とともに `std` を読み込むことを想定しています。

## プレリュード

- 主要なシーケンス操作 (`map`/`filter`/`reduce`/`partition*` など)
- 文字列操作 (`clojure.string` 相当)
- 集合操作 (`clojure.set` 相当)
- 汎用ツリー walk (`clojure.walk` 相当)
- 便利ラッパ: `http-get-json`/`http-post-json`、`sh`/`sh!`、`parse-opts`/`parse-opts-adv`、`log-*`、`deftest`/`is`/`run-tests` など

## std の読み込みと差し替え

- `clove run` / REPL では `std` が既定で読み込まれます（実装側の default require）。
- `user.clv` は設定専用のため `require` できません。`std` を別ライブラリに差し替えたい場合は各 `ns` で明示的に `require` してください。
- `:refer :*` で unqualified 名を取り込むと **その `ns` 内で上書き**されます（他の `ns` には影響しません）。
- 既存の組み込みを使いたい場合は **`core::` を明示**すれば常に core 版になります。

例:

```clojure
(ns app::main
  (:require [my::std :as std :refer :*]))

(map inc [1 2])        ; my::std の map が使われる
(std::map inc [1 2])   ; my::std の map
(core::map inc [1 2])  ; 常に core 版
```

## シーケンス API の方針（std と core の住み分け）

- `std` は「普段使いでベクタ返しを優先」する設計。`std::take` / `std::take-while` は常に vec を返し、完全に遅延を保ちたい場合は `core::take` / `core::take-while` を直接使う。
- `std::drop` / `std::drop-while` は無限列や巨大列の全実体化を避けるため Seq のまま返す。必要なら `(vec (drop ...))` で実体化できる。
- `std::map` / `std::filter` は入力に Seq が含まれると Seq を返し、それ以外では vec を返す。遅延パイプラインを維持したい場合は `core::map` / `core::filter` を明示的に使う。
- REPL で `<seq>` 表示を避けたい場合は、`std` 側を使うか `vec` で明示的に実体化する。
- `:doc` は参照したシンボルをそのまま解決するため、`std::take-while` を指定すると `std` の docstring が表示される。`core::take-while` の doc を見たい場合はその名前で問い合わせる。

## ファイル / パス / 時刻

- `slurp-bytes` / `spit-bytes`、`file-exists?` / `file?` / `dir?`、`list-dir`、`mkdir` / `mkdirs`、`delete`、`copy`、`move`
- 標準入力: `read-line` / `read-all`
- リソース: `resource-url` / `resource-bytes` / `resource->tempfile`
- パス操作: `path::cwd|home-dir|temp-dir|join|basename|dirname|extname|normalize|canonicalize|absolute?|relative?|resolve|source-dir`
- ソース: `current-file`（`core::current-file`）
- 時刻/Duration: `time::now`（エポックms）、`time::parse` / `time::format`、`time::plus` / `time::minus`、`time::between`（Duration返却）、`time::sleep`、`duration` / `duration-ms|sec|...`（数値→Duration）、`ms|sec|min|...`（Duration→数値）

## データフォーマット

- JSON: `json::parse` / `generate` / `generate-pretty` / `read-file` / `write-file`
- YAML: `yaml::parse` / `generate` / `read-file` / `write-file`
- TOML: `toml::parse` / `generate` / `read-file` / `write-file`
- INI: `ini::parse` / `generate`
- .env: `env::parse-file`
- 環境変数: `env::vars` / `env::get` / `env::has?`

## 文字列・正規表現

- 既存: `split` / `replace` / `replace-first` / `re-find` / `re-matches` / `split-lines` / `blank?` / `includes?` / `starts-with?` / `ends-with?` / `trim*` / `upper-case` / `lower-case`
- 追加: `reverse` / `capitalize` / `trim-newline` / `escape` (置換マップ) / `index-of` / `last-index-of`
- 正規表現ユーティリティ: `re-pattern`（string→regex）、`re-seq`（全マッチをベクタで返す）、`re-matcher`（簡易マッチャ; regexを返す）

## `contains?` と `includes?`

- `contains?` は map/set のキー、vector/list の **index** を判定する。
- `includes?` は **値の存在** を判定する（vector/list/seq の値探索、set/map はキー、string は substring）。

```clojure
(contains? {:a 1} :a)     ; => true
(contains? [:a :b] 1)     ; => true
(contains? [:a :b] :a)    ; => ERROR (index ではない)

(includes? [:a :b] :a)    ; => true
(includes? #{:a :b} :a)   ; => true
(includes? {:a 1} :a)     ; => true
(includes? "clove" "lo")  ; => true
```

## 集合 (`clojure.set` 相当)

- `union` / `intersection` / `difference`
- `select`（predでフィルタ）、`project`（キー選択）、`rename-keys` / `rename`
- `index`（キーでインデックス）、`join`（自然/キー指定ジョイン）、`map-invert`
- `subset?` / `superset?`

## HTTP

### 基本

```clojure
(http::request
  {:method "GET"
   :url "https://example.com"
   :headers {"Authorization" "Bearer TOKEN"}
   :query {:page 1}
   :timeout-ms 5000})
;; => {:status 200 :headers {...} :body "...raw..." :json {...}/nil}
```

ショートカット: `http-get` / `http-post` / `http-put` / `http-delete`。JSONヘルパー: `http-get-json` / `http-post-json`.

- `:body` には文字列（そのまま送信）か Clove値（JSONに変換）を渡せます。`:json` も利用可。
- 戻り値は `:status` `:headers` `:body` `:json` を含む map。

## シェル / プロセス

- `sh` / `sh!`（後者は非0終了で例外）。オプション `{:dir "...", :env {:KEY "VAL"}, :stdin "...", :stream? true}`。戻り `{:exit :out :err}`。
- `process::run`（出力回収、オプション `{:cwd "...", :env {...}, :clear-env true, :in "...", :out-enc :bytes, :err-enc :bytes}`）
- `process::which`（PATH から探索）
- `shell::split`（シェルワード分割）、`shellwords`（同上のエイリアス）、`shell::escape`（引数をエスケープ）。

## CLI / ログ / テスト

- CLI: `argv` / `env` / `env-get` / `exit`
- オプション解析: `parse-opts argv spec`  
  - `spec` 例: `[{:id :port :short "-p" :long "--port" :arg? true :default "8080" :required? true}]`
  - `--key=value` / `-k=value` 形式も可。必須で未指定の場合は `:errors` に記録。
  - 戻り: `{:options {:port "8080"} :args [...] :errors [...]}`（未知オプションも `:errors` に入る）
- オプション解析（拡張版）: `parse-opts-adv argv spec`  
  - `spec` 追加キー: `:alias`（追加のフラグ文字列）、`:coerce`（`:int`/`:float`/`:bool`/`:string`）、`:collect?`（複数指定を配列化）
  - `-abc` の短縮フラグ結合（**引数なしフラグのみ**）、`--` 以降はすべて positional
  - 例:
    ```clojure
    (def opts
      (parse-opts-adv (argv)
                      [{:id :port :short "-p" :long "--port" :arg? true :coerce :int}
                       {:id :include :short "-I" :arg? true :collect? true}
                       {:id :verbose :alias ["-v" "--verbose"]}]))
    ```
- ログ: `log-trace|debug|info|warn|error`
- デバッグ: `dbg` / `spy` は値を表示してそのまま返す。
- Pretty print: `pp` / `pp-str` / `pprint` / `pprint-str`  
  - `pp` は整形結果を `*out*` に出力して **元の値を返す**。`pp-str` は **整形済み文字列**を返す。
  - `clovefmt.toml` または `.clovefmt.toml` を **カレントディレクトリから探索**し、`indent/width` に反映する（`opts` の `:indent` / `:width` が優先）。
  - 追加の制限: `:max-depth` / `:max-items` / `:max-string` / `:max-bytes`（巨大データ対策）。
  - `#atom<...>` / `<seq>` など Reader が読めない値は **文字列化**して出力する（parseable を保つため）。
- テスト（最小版）: `(deftest foo (fn [] (is (= 1 1))))`, `(testing "context" (fn [] ...))`, `(run-tests)`。`is` はメッセージ任意。

## 簡単な例

```clojure
(ns quick::sample)

;; HTTP + JSON
(let [resp (http-get-json "https://api.example.com/users")]
  (println "users count:" (count resp)))

;; ファイルとパス
(when-not (file-exists? "tmp")
  (mkdir "tmp"))
(spit-bytes (path-join "tmp" "hello.bin") [0 1 2 3])

;; CLI 引数
(def opts (parse-opts (argv)
                      [{:id :mode :long "--mode" :arg? true :default "dev"}]))
(println "mode:" (get-in opts [:options :mode]))

;; シェル
(sh "echo" "hello")

;; テスト
(deftest add-test (fn [] (is 3 (+ 1 2))))
(run-tests)
```

---
<!-- NAV:START -->
**前へ:** [型/enum/match（deftype/defenum/match）](types_enum_match.ja.md)
**次へ:** [CLI（clove / clove fmt / clove build）](../tooling/cli.ja.md)
<!-- NAV:END -->

