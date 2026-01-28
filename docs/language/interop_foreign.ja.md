# 外部エンジン埋め込み (Ruby / Python など)

Clove の特徴のひとつが「外部言語エンジン」を埋め込めることです。
Rust 側の `ForeignEngine` 実装を通じて、Clove のコードから Ruby / Python などを呼び出せます。

この文書では、現時点の設計・実装に基づいて

* 埋め込みフォームの考え方
* Ruby / Python ブロック
* JSON / YAML タグとの違い
* 型変換と注意点

をざっくり整理します。

> ⚠️ ここで説明する API は今後変更される可能性があります。
> 型変換やエラー処理の細かい挙動は Rust 側の実装に依存します。

---

## 1. 埋め込みフォームの全体像

大きく分けて 3 系統あります。

1. **式として評価する** 外部ブロック

   * `$rb{ ... }` / `$py{ ... }` / `${ ... }` など（`${ ... }` は Ruby の省略）
   * 「Clove の値 → 外部 → Clove の値」と往復する
2. **JSON / YAML を Clove の値に変換する reader タグ**

   * `#json{ ... }` / `#yaml{ ... }`
   * 純粋なデータ変換（コード実行はしない）
3. 将来の拡張候補

   * 他言語（Node.js / Rust）向けエンジン

ここでは 1 と 2 をメインで説明します。

---

## 2. Ruby ブロック

例:

```clojure
(ns examples::interop::ruby-basic)

(defn ruby-version []
  $rb{
    RUBY_VERSION
  })

(defn upcase [s]
  $rb{String(s).upcase}

(println "Ruby version:" (ruby-version))
(println (upcase "hello from clove"))
```

### 2.1 実行モデル

* `$rb{ ... }` の内部は、Ruby として解釈・実行されます。
* `${ ... }` は **デフォルト外部言語**の省略記法です（未指定なら Ruby）。
* 実装上は、Rust 側の Ruby エンジン（`clove-ruby` 等）に文字列として渡されます。
* ブロックの評価結果が Clove の値として返ってきます。

### 2.1.1 デフォルト外部言語の決定

* 対象: `${...}` / `$Foo.bar(...)` などのタグ省略 foreign
* 優先順位: **ファイル拡張子** → **(use default-interop/foreign ...)** → **Ruby**
* 拡張子: `*.rb.clv` / `*.py.clv`
* `(use default-interop <lang>)` と `(use default-foreign <lang>)` は同義
  * 受理値: `"rb"` / `"ruby"` / `:rb` / `:ruby` → `rb`、`"py"` / `"python"` / `:py` / `:python` → `py`
* 拡張子と `(use ...)` が衝突した場合は **拡張子を採用**し、WARNING を出します
* ファイル単位で決まり、`require` 先に伝播しません

### 2.2 値の変換（おおまかに）

**Clove → Ruby**（イメージ）

* `nil` → `nil`
* 整数 / 浮動小数 → Ruby の `Integer` / `Float`
* 文字列 → Ruby の `String`
* ベクタ → Ruby の `Array`
* マップ → Ruby の `Hash`（キーはキーワード / 文字列等から変換）
* 真偽値 → `true` / `false`

**Ruby → Clove**

* `nil` → `nil`
* `Integer` / `Float` → `:int` / `:float`
* `String` / `Symbol` → 文字列やキーワード
* `Array` → ベクタ
* `Hash` → マップ

正確な変換ルールは Rust 側実装に依存しますが、
「シリアライズしやすい素朴な値」はそのまま往復しやすい設計になっています。

### 2.3 用途

* 既存の Ruby ライブラリをそのまま呼びたい場合

  * HTTP クライアント
  * DB クライアント
  * テキスト処理
* Clove でメインロジックを書きつつ、一部処理だけ Ruby に逃がす

---

## 3. Python ブロック

例:

```clojure
(ns examples::interop::python-basic)

(defn py-sqrt [x]
  $py{
    import math
    math.sqrt(x)
  })

(println (py-sqrt 9)) ; => 3.0
```

* `$py{ ... }` の中身は Python として実行されます。
* Ruby と同様に Clove 値との相互変換を行います。

用途としては

* 機械学習など Python 側のエコシステムにあるライブラリを呼びたい
* 既存の Python スクリプト資産を活かしたい

といった場面を想定しています。

---

## 4. JSON / YAML タグとの違い

`#json{ ... }` / `#yaml{ ... }` は **コード実行ではなく、データ読み込み** 用です。

```clojure
(def config-json
  #json{
    "host": "localhost",
    "port": 8080
  })

(def config-yaml
  #yaml{
    host: localhost
    port: 8080
  })
```

* `#json{ ... }`

  * JSON をパースして Clove の値（マップ / ベクタ / 文字列 / 数値 / bool / nil）に変換
* `#yaml{ ... }`

  * YAML を同様に変換

**安全性の違い:**

* `$rb{}` / `$py{}` などは「任意コード実行」
* `#json{}` / `#yaml{}` は「データのみ」

設定ファイルや静的データは `#json{}` / `#yaml{}` を使い、
ロジックや外部ライブラリは `$rb{}` / `$py{}` に任せる、という棲み分けを想定しています。

---

## 5. 注意点とベストプラクティス

### 5.1 依存関係とデプロイ

* Ruby / Python 埋め込みを使う場合、それぞれのランタイムが必要です。
* `clove build` でバイナリ化するときも、埋め込みランタイムの扱い（静的リンク / 動的リンク）が問題になります。
* 「なるべく小さい単一バイナリ」を目指す場合は、外部依存を絞るか、
  Rust / Clove 側に移せるところは移すのがおすすめです。

### 5.2 型に依存しすぎない

* Ruby / Python 側ではクラス・型の世界が広がりますが、
* Clove に戻すときの型表現はあくまで「シンプルな値（マップ / ベクタ / 文字列 / 数値など）」が中心です。

外部ライブラリの生オブジェクトを Clove に戻すのではなく、
「必要な情報だけを map や vector に詰める」形にしておくと扱いやすくなります。

### 5.3 例外・エラー

* 外部エンジン側で例外が発生すると、Clove 側ではエラー (`CloveError`) として扱われます。
* どの程度までメッセージやスタックトレースを拾うかは、実装側の方針に依存します。

---

## 6. まとめ

* `$rb{...}` / `$py{...}` などで、Clove から Ruby / Python を直接呼び出せる。
* 値の変換は「素朴なシリアライズ可能な値」を中心に設計されている。
* `#json{}` / `#yaml{}` はコード実行ではなく「データ読み込み」用の reader タグ。
* デプロイ構成や依存ランタイムの重さも考慮しつつ、
  「Clove で書くべきところ」と「既存言語に任せるところ」を切り分けるのが吉。
  EOF
  fi

######################################

# docs/language/concurrency.md

######################################
if [ -e "docs/language/concurrency.md" ]; then
echo "[skip] docs/language/concurrency.md already exists"
else
echo "[create] docs/language/concurrency.md"
cat > docs/language/concurrency.md <<'EOF'

# 非同期 / 並行処理

Clove には、Clojure / core.async に影響を受けた非同期・並行プリミティブが
標準ライブラリとして組み込まれています。

* `atom` … 単一値のスレッドセーフな参照
* `chan` … メッセージパッシング用チャネル
* `future` / `promise` … 将来の値
* `task` … キャンセル可能なジョブ
* `agent` … actor 風の状態マシン
* `select` / `timeout` … 複数チャネル待ち

ここではざっくりした使い方とイメージをまとめます。

> 細かい戻り値の型・エラーメッセージは実装に依存します。
> 以下はおおまかな挙動イメージです。

---

## 1. Atom

### 1.1 基本

```clojure
(def counter (atom 0))

(atom-set! counter 10)         ; reset!
(atom-update! counter inc)     ; swap!
(atom-update! counter + 5)     ; => 15 になる
@counter                        ; deref
```

主なビルトイン:

* `atom` … 初期値から Atom を作る
* `atom?` … Atom かどうか
* `atom-set!` … 値を丸ごと置き換え（`reset!` 相当）
* `atom-update!` … 関数を使って更新（`swap!` 相当）
* `add-watch` / `remove-watch` … Atom や agent / promise / task / future の監視者の追加・削除（`atom-add-watch` / `atom-remove-watch` は互換エイリアス）
* `atom-set-validator!` … validator 関数の設定
* `deref` / `@` … 中身の取得

### 1.2 例

```clojure
(defn inc-counter! []
  (atom-update! counter inc))

(atom-add-watch counter :log
  (fn [key atom old new]
    (println "counter changed:" old "->" new)))
```

---

## 2. Channel (`chan`)

### 2.1 基本操作

```clojure
(def c (chan 1))            ; バッファ 1 のチャネルを作る

(chan-put! c :foo)          ; ブロッキング put
(def value (chan-take! c))  ; ブロッキング take

(chan-close! c)
(chan-closed? c)            ; => true
```

主なビルトイン:

* `chan` … バッファサイズ付きチャネルを作る
* `chan?`
* `chan-put!` … 値を書き込む（成功したかどうかを bool で返す）
* `chan-take!` … 値を読み出す（チャネルが閉じて空なら `nil`）
* `chan-close!`
* `chan-closed?`

### 2.2 `timeout` / `select`

`timeout` は、一定時間後に「閉じるチャネル」を返します。

```clojure
(def t (timeout 500))  ; 500ms 後に閉じるチャネル
```

`select` は **複数のチャネルからの読み書き** をまとめて待つための関数です。

```clojure
(select [c1 c2]
        {:timeout 5s
         :default :idle})
```

* 第 1 引数: 「ケース」のコレクション

  * `chan` … そのチャネルからの take
  * `[:put chan value]` … そのチャネルへの put
* オプション map（任意）:

  * `:timeout` … Duration または秒数（数値）
  * `:default` … どのケースもすぐには準備できないときの戻り値

戻り値のイメージ:

* 値を受信した場合: `[value chan]`
* put が成功した場合: `[true chan]`
* timeout の場合: `nil` または default 指定に応じた値

`:default` を付けた場合は **非ブロッキング（即時ポーリング）** になり、ケースがすぐに準備できていればそちらを返し、全く準備できていない場合のみ `[default nil]` が返ります。省略した場合は、いずれかのケースが完了するまでブロックします。

---

## 3. `spawn` / `task`

`spawn` は、与えた関数を別スレッドで実行し、そのハンドルを `Task` として返します。

```clojure
(def t (spawn (fn [] (heavy-work))))

(task? t)           ; => true
(task-done? t)      ; => 実行完了したかどうか
(task-deref t)      ; => 結果が返ってくる（完了までブロック）
```

主なビルトイン:

* `spawn` … `(spawn (fn [] ...))` で Task を生成
* `task?`
* `task-deref`
* `task-done?`
* `task-cancel!`（キャンセルが間に合えば true を返すイメージ）
* `done?` … promise / task / future / agent をまとめて完了チェックする共通関数

---

## 4. Future

`future` は「遅延評価＋バックグラウンド実行」のようなものです。

```clojure
(def f (future (fn [] (heavy-work))))

(future? f)
(future-done? f)
(future-deref f)      ; 完了するまで待って結果を返す
(future-cancel! f)    ; キャンセルを試みる
```

* `spawn` / `task` に近いですが、より「値志向」な API を持ちます。
* 将来 `future-then` / `future-catch` / `future-finally` 的な combinator が増える可能性があります。
* 完了チェックは `future-done?` のほか `done?` でも共通的に確認できます。

---

## 5. Promise

`promise` は「外部から解決される future」です。

```clojure
(def p (promise))

(spawn
  (fn []
    (chan-put! some-chan :ready)
    (promise-deliver! p 42)))

(def result (promise-deref p)) ; => 42
```

主なビルトイン:

* `promise` … 空の Promise を作る
* `promise?`
* `promise-deref`
* `promise-deliver!` … 値を解決
* `promise-error` … エラーで決着したときのメッセージ取得（promise / task / future 共通）
* `done?` … promise / task / future / agent の完了確認

---

## 6. Agent

`agent` は「状態を持ったワーカー」のようなものです。

```clojure
(def a (agent 0))

(agent-send! a + 1)
(agent-send! a + 2)

(agent-deref a) ; => 3 （キューが全部処理されていれば）
```

特徴:

* `atom` と違い、更新は非同期にキューに積まれる
* 処理は 1 エージェントにつき 1 スレッド（順番は保証される）
* `agent-error` でエラー状態をチェックできる

---

## 7. 設計イメージと使い分け

ざっくりした使い分けのイメージ:

* **atom** … 単純な共有状態。同期的 / 即時更新。
* **chan** … スレッド間のメッセージパッシング。パイプライン構成。
* **spawn / task / future** … バックグラウンドで重い処理を回したいとき。
* **promise** … 「誰かがあとで結果を流し込む」ための箱。
* **agent** … 状態を持つ actor。ログキューや集約処理など。

Clove のコアは Clojure に近い設計思想を持ちつつ、
型情報（`deftype` / `defenum`）や Duration リテラルなどと組み合わせて
「ほどよく型付きの並行コード」を書けるようにすることを目指しています。

---

## セキュリティと実行モード

foreign blocks は強力ですが、外部エンジンのロードは実行環境次第で制限されます。

- `clove` 実行時に `--auto-fallback` を付けると、
  “外部エンジンが使えない場合” に Ruby→Python→Clove の順で代替実行を試みます。
- `clove build --embed-ruby/--embed-python` は、
  生成された実行ファイルにエンジンを同梱するためのオプションです（制約あり）。

詳しくは [Build](../tooling/build.ja.md)。

---
<!-- NAV:START -->
**前へ:** [並行/非同期（chan / future / go-loop / scope-loop / async-scope）](concurrency.ja.md)
**次へ:** [型/enum/match（deftype/defenum/match）](types_enum_match.ja.md)
<!-- NAV:END -->

