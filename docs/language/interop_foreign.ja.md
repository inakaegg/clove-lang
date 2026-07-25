# 外部エンジン埋め込み (Ruby / Python など)

English version: [interop_foreign.md](interop_foreign.md)

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
(ns examples::interop::ruby_basic)

(defn ruby-version []
  $rb{
    RUBY_VERSION
  })

(defn upcase [s]
  $rb{String(s).upcase})

(defn -main []
  (println "Ruby version:" (ruby-version))
  (println (upcase "hello from clove")))
```

`clove --main examples/interop/ruby_basic.clv` で実行できます。

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
(ns examples::interop::python_basic)

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

---
<!-- NAV:START -->
**前へ:** [並行/非同期（chan / future / go-loop / scope-loop / async-scope）](concurrency.ja.md)
**次へ:** [型/enum/match（deftype/defenum/match）](types_enum_match.ja.md)
<!-- NAV:END -->

