# 言語の基礎

English version: [basics.md](basics.md)

このドキュメントは、Clove の「最初に知っておきたい基本文法」をざっくりまとめたものです。

- S 式と評価
- リテラル（数値 / 文字列 / コレクション / 正規表現 / Duration）
- 変数定義 (`def`, `let`)
- 関数 (`fn`, `defn`) と型ヒント
- 制御構文 (`if`, `cond`, `when`, `loop` / `recur` など)
- 名前空間 (`ns`, `require`)
- REPL でのちょっとした操作

詳細な型システムや外部エンジン、並行機能は他のドキュメントを参照してください。

---

## 1. S 式と評価モデル

Clove は Lisp ライクな S 式を持つ言語です。  
式は **リスト (list)** で表現され、最初の要素が「関数や special form」、残りが引数になります。

```clojure
(+ 1 2 3)     ; => 6
(str "a" "b") ; => "ab"
(if cond x y) ; cond が真なら x、偽なら y
````

* リストは `()` で表現されます。
* `(+ 1 2)` のような形を「フォーム (form)」と呼びます。
* 評価ルールの基本は Clojure に近く、「先頭を評価して関数を決め、残りを引数として渡す」です。

---

## 2. リテラル

### 2.1 基本リテラル

* 整数: `0`, `42`, `-10`
* 浮動小数: `3.14`, `-0.5`, `1.0e3`
* 文字列: `"hello"`, `"こんにちは"`
* 真偽値: `true`, `false`
* nil: `nil`

`_` を使った数値リテラルもサポートされます。

```clojure
1_000_000  ; => 1000000
3_600_000  ; など
```

### 2.2 コレクション

* ベクタ: `[1 2 3]`
* リスト: `'(1 2 3)` または `(list 1 2 3)`
* マップ:

  * Clojure 風: `{:x 1 :y 2}`
  * JSON 風: `{x: 1 y: 2 name: "Taro"}`
* セット: `#{1 2 3}`

```clojure
(def v [1 2 3])
(def m {:x 10 :y 20})

v[0]       ; => 1
m[:x]      ; => 10
(assoc m :x 99) ; => {:x 99 :y 20}
```

JSON 風マップでは、キーがキーワードとして扱われます。

```clojure
{foo: 1 bar: 2}  ; => {:foo 1 :bar 2}
```

### 2.3 正規表現と Duration

* 正規表現:

  * Ruby / JS 風: `/foo.*/`（デフォルト）
  * 明示的な形式: `#/foo.*/`（曖昧な場合の回避用）

* Duration:

  * `10ms`, `500ms`, `3s`, `2m`, `1h`, `5d`, `1w`, `1y` など
  * `_` 付きの数値も可: `1_000ms`
  * 数値から作る場合は `(duration 0.5 :sec)` や `(duration-ms 250)` などのコンストラクタも利用できる

正規表現と Duration の詳細は別ドキュメントを参照してください。

* [正規表現 / Duration リテラル](regex_duration.ja.md)

### 2.4 キーワードの名前解決について

Clove ではキーワードはグローバルです。`:user::flag` のように名前空間風の表記は書けますが、あくまで文字列の一部として扱われ、特定の名前空間に紐づくことはありません。Clojure の `::myns/abc` のような自動解決付きのネームスペース付きキーワードとは意味が異なる点に注意してください。

---

## 3. 変数定義と束縛

### 3.1 `def` – トップレベル定義

```clojure
(def answer 42)

(println answer) ; => 42
```

* 現在の名前空間に `answer` を定義します。
* 再評価すると値が上書きされます。
* `def` はトップレベル専用です（関数内で使うとエラー）。

### 3.2 `let` – ローカル束縛

```clojure
(let [x 10
      y 20]
  (+ x y))    ; => 30
```

* ベクタで名前と値を交互に並べます。
* `let` のスコープ内だけで有効です。

### 3.3 `set!` / `redef` – 既存 var の更新

```clojure
(def x 1)
(set! x 2)
x ; => 2
```

* `set!` は **既存の名前空間 var** を更新します。
* 未定義の var への `set!` はエラーです（タイプミス検出）。
* `redef` は `set!` の別名です。
* ローカル束縛（`let` や `-def`）を `set!` で更新することはできません。

### 3.4 `-def` – ローカル束縛（let* 相当）

```clojure
(defn f [x]
  (-def a (+ x 1))
  (-def b (+ a 1))
  (+ a b)) ; => 5 (x=1)
```

* `-def` は関数内専用のローカル束縛です。
* 逐次束縛（`let*` 相当）なので、前に定義した値を参照できます。

---

## 4. 関数

### 4.1 無名関数 `fn`

```clojure
(fn [x] (+ x 1))

((fn [x] (+ x 1)) 10) ; => 11
```

### 4.2 関数定義 `defn`

```clojure
(defn add [x y]
  (+ x y))

(add 1 2) ; => 3
```

型ヒントは任意です。**戻り値**の型は名前の直後にキーワードで、
**引数**の型は後置の `名前<Type>` 形式で書きます。

```clojure
(defn add :int [x<Int> y<Int>]
  (+ x y))

(add 1 2) ; => 3
```

* `defn name <ret-type?> [arg1<Type?> arg2<Type?> ...] body...`
* 引数の型を素のキーワードで書く形（`[x :int]`）は構文として **無効** です。
  [型ヒント](../advanced/typing.ja.md) を参照してください。
* `defn` はトップレベル専用です（関数内で使うとエラー）。
* 型ヒントはドキュメント（`doc` / `describe`）、LSP / 補完、
  typed な `clove build` 経路で使われます。実行時の型チェックは行いません。

* attr-map は Clojure 同様、**名前直後** または **docstring 直後** のどちらにも置けます（複数置くのは非対応）。
  * `{:subject-pos N}` または `{:subject-pos :last}`/`-1` を指定すると OOP チェインでのレシーバ位置を固定できます。
  * 付与した attr-map は `(meta fn-name)` でそのまま取得できます（評価はされずデータとして付く）。

```clojure
(defn greet {:subject-pos :last} "docstring" [name suffix]
  (str name suffix))

(meta greet) ; => {:source-file "..." :subject-pos :last}
```

### 4.3 短縮ラムダ

実装側には「`#()` 形式の短縮ラムダ」があります（`#(inc %)` のような書き方）。
現時点では仕様が変わる可能性があるため、詳細な説明は割愛します。
通常の `fn` / `defn` を優先して使うと安全です。

---

## 5. 制御構文

代表的なものだけ列挙します（Clojure にほぼ近いイメージ）。

```clojure
(if cond
  then-expr
  else-expr)

(when cond
  expr1
  expr2
  ...)

(cond
  (= x 0) "zero"
  (= x 1) "one"
  :else   "other") ; _ でも可
```

`loop` / `recur` もサポートされています（末尾再帰最適化を意識した書き方）。

```clojure
(defn sum-to :int [n<Int>]
  (loop [i 0 acc 0]
    (if (> i n)
      acc
      (recur (inc i) (+ acc i)))))
```

---

## 6. 名前空間と `require`

### 6.1 `ns`

```clojure
(ns myapp::core)

(def pi 3.1415)
```

* 名前空間は `::` 区切りで書きます。
* 慣習として、`myapp/core.clv` → `(ns myapp::core)` のように
  ファイルパスと名前空間を揃えるのがおすすめです。

### 6.2 `require`

```clojure
(ns myapp::ui
  (require myapp::core :as core))

core::pi ; => 3.1415  ; var なので括弧は付けない
```

Clojure と同じく `:as` / `:refer` / `:rename` 形式をサポートします。

また、`require` に文字列を渡すとファイルモジュールとしてロードできます。
ファイル側に `(ns ...)` が無い場合はパス由来の暗黙 namespace が割り当てられます。
「現在の namespace に定義を流し込みたい」場合は `load-file` を使ってください。

`:refer` は値だけでなく型も import し、`:as` alias は型注釈でも `alias::Type` として使えます。
`defenum` は `Noop`/`Quit` のような variant 型名を暗黙に作成します（`qualified-only` のときは除外）。

---

## 7. REPL のちょいテク

`clove --repl` で REPL を起動すると、以下のような操作ができます。

```clojure
; シンボルのドキュメント
:doc map
:doc myapp::core::area

; 値やシンボルの情報
(describe +)
(describe {:x 1 :y 2})
```

VS Code からは `Clove: Send Selection to REPL` コマンドで
現在のフォーム / 選択範囲を REPL に送り込むこともできます。

> REPL の詳細（` :source` などの拡張コマンド）は今後 `tooling/cli.md` 側にまとめていきます。

---
<!-- NAV:START -->
**前へ:** [Getting Started](../getting_started.ja.md)
**次へ:** [REPL ガイド](../tooling/repl.ja.md)
<!-- NAV:END -->

