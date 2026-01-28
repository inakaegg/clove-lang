# 型・enum・match

Clove は「まずは動的・マップ中心」で設計された Lisp 方言ですが、  
`deftype` / `defenum` / `match` を組み合わせることで、軽量な代数的データ型（ADT）風に書けます。

この文書では、現在実装されている仕様をベースに

- `deftype`（product type）
- `defenum`（sum type / 列挙）
- `match`（パターンマッチ）

の使い方と役割をまとめます。

> 💡 Clojure との違い
>
> - Clove には `protocol` / `multimethod` は **ありません**。
> - その代わり「データの形は `deftype` + `defenum`」「分岐ロジックは `match`」というシンプルな構成を想定しています。

---

## 1. `deftype` – 名前付き product type

### 1.1 シンタックス

```clojure
(deftype Name {:field1 :type1 :field2 :type2 ...})

;; 省略可能なドキュメント文字列つき
(deftype Name "Doc string"
  {:foo :int :bar :string})

;; フィールドをフラットに書く
(deftype Name
  :field1 :type1
  :field2 :type2)
````

* `Name` はシンボル（**現在の ns 内**・単数形を推奨: `Dog`, `Cat`, `Human` など）
* フィールド部分は **キーワード → 型ヒント** のマップ、またはフラット形式
* 型ヒントには現在おおよそ以下のようなものを想定

  * プリミティブ: `:int`, `:float`, `:string`, `:bool` など
  * 別の型名（`Dog`, `UserId` など）

#### コンストラクタの引数（省略記法）

`deftype` のコンストラクタは、map 形式に加えてキーワードの偶数列や省略記法を受け付けます。

```clojure
(deftype Color :r Int :g Int :b Int :a Int)

(Color {r: 1, g: 2, b: 3, a: 4})
(Color :r 1, :g 2, :b 3, :a 4)
(Color r: 1, g: 2, b: 3, a: 4)

(def r 1) (def g 2)
(Color r:, g:)  ;; => (Color :r r :g g)
(Color :r, :g)  ;; => (Color :r r :g g)
```

* `:k v` の偶数列は内部的に map 化される
* `k:` / `:k` は `:k k` の省略として扱われる（型コンストラクタ呼び出しのみ）
* 省略したくない場合は `{:k v}` の map 形式を使う

### 1.1.1 `:from` で既定インスタンスを同時定義

`deftype` と同時に「既定値（定数）」を 1 つだけ束縛できます。

```clojure
(deftype Config :from cfg
  {:screen {:w 480 :h 640}
   :timing {:spawn-frames 92 :tick-ms 16}})

cfg
;; => (Config {...}) と等価
```

`def` 形式でも同じ意味になります。

```clojure
(deftype Config (def cfg
  {:screen {:w 480 :h 640}
   :timing {:spawn-frames 92 :tick-ms 16}}))
```

ポイント:

* `:from` の束縛は **明示的に 1 個だけ**（自動生成なし）
* 既定値の構造からフィールド型を推定する（`describe` に反映）
* 既定値に `deftype :alias` の値が入っている場合は **型名を保持** する

### 1.2 何が定義されるか

```clojure
(ns spec::types::animals_a)

(deftype Dog {:name :string :age :int})
```

を評価すると、現在の ns に以下が定義されます。

* コンストラクタ関数 **`Dog`**
* 述語関数 **`Dog?`**
* 型レジストリへのメタ情報（`describe-type` 用）

実際の挙動例:

```clojure
(ns spec::types::animals_a)

(deftype Dog {:name :string :age :int})

[(type (Dog {:name "Pochi" :age 3}))
 (Dog? (Dog {:name "Pochi" :age 3}))
 (Dog? {:name "Fake" :age 2})]
;; =>
;; ['spec::types::animals_a::Dog
;;  true
;;  false]
```

* `type` … 値に紐づく型の **完全修飾シンボル** を返す
* `Dog?` … その値が `Dog` 型かどうかを判定

#### 型判定の方法

基本は以下のいずれかです。

```clojure
(Dog? value)                                   ; 述語（deftype で自動生成）
(= (type value) 'spec::types::animals_a::Dog)  ; type の結果と比較
(instance? 'spec::types::animals_a::Dog value) ; 型名はシンボル/文字列で渡す
```

* `instance?` は **シンボル / 文字列**を受け取るため、`Dog`（コンストラクタ関数）をそのまま渡すとエラーになります。
* `=` は値同士の比較なので、型判定には `type` / `Dog?` / `instance?` を使います。
* `instance?` は `deftype` の必須フィールドと型まで検証します（未知キーは無視）。
* `:type` タグ一致だけを見たい場合は `tagged?` を使います（安全な検証ではありません）。

enum の variant は `Enum::Variant` 形式で判定できます。

```clojure
(match mode
  Mode::Running  1
  Mode::GameOver 2
  _              3)

(= (type mode) 'const::Mode::Running)
(instance? 'const::Mode::Running mode)
```

同じ名前の `deftype` が同一 namespace にある場合、`Running` のような **未修飾名はそちらを優先**して解決されます。
その場合は `Mode::Running` のように **enum 名を付けて**書くのが安全です。

### 1.3 実体は「普通の map＋型タグ」

`Dog` コンストラクタが返す値は、見た目はふつうの map です。

```clojure
(def pochi (Dog {:name "Pochi" :age 3}))

pochi
;; => {:name "Pochi" :age 3 ...}   ; 表示上はただの map

(type pochi)
;; => 'spec::types::animals_a::Dog

(Dog? pochi)
;; => true
```

内部的には「どの `deftype` から作られたか」がレジストリに記録されているため、

* map 系の関数（`get`, `assoc`, `update` など）がそのまま使える
* それでいて `type` / `Dog?` / `describe-type` で型情報にアクセスできる

という構成になっています。

### 1.4 型ヒントの扱い（現時点）

フィールドに書いた `:int` / `:string` などは **あくまでヒント** です。

* 実行時の厳密な型チェックは現時点では行いません
* 将来の用途として:

  * LSP 補完・型ホバー
  * フォーマッタ / ドキュメント生成
  * 実装次第では簡易最適化（ボクシング回避など）

を想定しています。

> 「ドキュメント＋将来的な最適化のヒント」と思って書いておいてください。

---

## 2. `defenum` – Sum type / 列挙

`defenum` は複数の `deftype` をひとつの「カテゴリ」として束ねるための構文です。
イメージとしては「`Dog` と `Cat` をまとめて `Pets` と呼ぶ」といった使い方を想定しています。

### 2.1 基本形

```clojure
(ns spec::types::animals_b)

(deftype Dog {:name :string})
(deftype Cat {:name :string})

(defenum Pets
  Dog
  Cat)
```

* `Pets` … 「`Dog` または `Cat` のどちらか」という意味の enum
* **慣習** として enum 名は複数形 / コレクション名を推奨

  * 例: `Pets`, `Mammals`, `Animals`, `ErrorKinds` など
* メンバーは **同じ ns 内** にある `deftype` 名である必要があります

### 2.2 メタ情報と introspection

`deftype` / `defenum` を評価すると、内部の型レジストリにメタ情報が登録されます。
`describe-type` ビルトインで、型の情報を map として取得できます。

```clojure
(describe-type 'Pets)
;; おおよそ:
;; {:kind    'sum
;;  :ns      'spec::types::animals_b
;;  :name    'Pets
;;  :fqn     'spec::types::animals_b::Pets
;;  :members ['spec::types::animals_b::Dog
;;            'spec::types::animals_b::Cat]
;;  ...}

(describe-type 'Dog)
;; おおよそ:
;; {:kind        'product
;;  :ns          'spec::types::animals_b
;;  :name        'Dog
;;  :fqn         'spec::types::animals_b::Dog
;;  :fields      {:name :string}
;;  :belongs-to  ['spec::types::animals_b::Pets]
;;  ...}
```

`describe` は **値 or 型名**を受け取り、型情報を返します。  
`describe-type` は `describe` の型名向けショートハンドです。

```clojure
(describe (Dog {:name "Pochi"}))
(describe 'Dog)        ; describe-type と同等
```

`infer-type` は **式の型を推定して文字列で返す**ための補助です。

```clojure
(infer-type (+ 1 2))
(infer-type (fn [x] x))
```

※ キーの名前や値は実装に準拠しますが、概念としては上記のような構造です。
`kind` / `ns` / `name` / `fqn` / `fields` / `belongs-to` / `members` などが含まれます。

`enum-members` という名前のビルトインもあり、enum の中身を調べる用途向けです。
現時点では `describe-type` と同じ内部実装を共有しており、将来的に API を整理する予定です。

### 2.3 将来の拡張案（メモ）

* 別の enum を「まとめて含める」ために `*Pets` のような記法を導入する案があります。

```clojure
(defenum Animals
  *Pets    ;; Pets = Dog | Cat を全て含める（案）
  Salmon)
```

* 現在はまだ **設計段階** であり、仕様としては固定していません。
* 実装が入ったら、このドキュメント側も追従して更新する予定です。

---

## 3. `match` – パターンマッチ

`match` は、与えられた値に対して複数のパターンを上から順に試し、
最初にマッチした行の式を評価する special form です。

`deftype` / `defenum` と組み合わせると、ML / Haskell 風の書き心地にかなり寄せられます。

### 3.1 基本形

```clojure
(match value
  pattern-1 expr-1
  pattern-2 expr-2
  ...
  _         default-expr)
```

* 左: **パターン**
* 右: マッチしたときに評価される **式**
* 上から順番に評価し、最初にマッチした 1 行だけが実行されます
* `_` はワイルドカード（なんでもマッチ）

### 3.2 型パターン＋map パターン

`deftype` と一緒に使う基本パターンは「型＋map の形」です。

```clojure
(ns spec::types::animals_b)

(deftype Dog {:name :string})
(deftype Cat {:name :string})

(defenum Pets
  Dog
  Cat)

(defn describe [pet]
  (match pet
    (Dog {:name n}) (str n " the dog")
    (Cat {:name n}) (str n " the cat")
    _              "unknown"))
```

* `(Dog {:name n})`

  * 「`Dog` 型 かつ `:name` フィールドを取り出して `n` に束縛」
* `(Cat {:name n})`

  * 同様に `Cat` 型のときにだけマッチ

#### フィールドの省略パターン

`(Dog :name :age)` のように **キーワードだけ** を並べると、
`(Dog {:name name :age age})` の省略として扱われます。

`match` のパターンだけでなく、`let` の束縛パターンでも使えます。

### 3.3 `:as` で「値全体」も同時に受け取る

フィールドを分解しつつ、元の値全体も見たい場合は `:as` を使います。

```clojure
(defn describe [pet]
  (match pet
    (Dog {:name n}) :as whole
      (if (Dog? whole)
        (str n " the dog")
        "not a dog")

    (Cat {:name n}) (str n " the cat")
    _               "unknown"))
```

* `(Dog {:name n}) :as whole expr`

  * `:name` は `n` に束縛
  * 値全体（`(Dog {...})` の結果）は `whole` に束縛

### 3.4 `:when` でガード条件

さらに「特定の条件を満たすときだけ」マッチさせたい場合は `:when` を使います。

```clojure
(defn describe [pet]
  (match pet
    (Dog {:name n}) :as whole
      (if (Dog? whole)
        (str n " the dog")
        "not a dog")

    (Cat {:name n}) :when (= n "Mimi")
      (str n " the picky cat")

    (Cat {:name n})
      (str n " the cat")

    _ "unknown"))
```

この例では:

1. `Dog` 型なら `Dog?` でもう一度チェックしつつ dog 用文言
2. `Cat` 型かつ名前が `"Mimi"` のときだけ「picky cat」
3. その他の `Cat` はふつうの cat 表記
4. それ以外は `"unknown"`

という順にマッチしていきます。

### 3.5 評価順・完全性

`match` の評価順はシンプルです。

1. パターンを上から順に試す
2. パターンにマッチした行があれば:

   * `:when` があればその条件式を評価
   * 条件が真なら右側の式を評価して即終了
   * 偽なら次のパターンへ
3. どのパターンにもマッチしなければエラー、または `_`（ワイルドカード）があればそれが使われる

> ⚠️ 現時点では **コンパイル時の完全性チェック（exhaustiveness check）はありません**。
> `_` などのフォールバックパターンを自分で用意してください。

---

## 4. 利用パターンと設計のヒント

### 4.1 「名前付き map」としての `deftype`

`deftype` は、もともと map で表現していたドメインデータに「名前を与える」用途に向いています。

例:

```clojure
(deftype Player {:id :int :name :string})
(deftype Enemy  {:id :int :kind :string})

(defenum Actors
  Player
  Enemy)
```

* DB レコード / HTTP レスポンス / 設定値などを map でそのまま扱える
* それでいて `Player?` / `Enemy?` / `type` で型を区別できる

### 4.2 enum + match で「分岐の入口」を一本化

「型ごとに分岐する処理」は、なるべく 1 箇所にまとめておくと読みやすくなります。

```clojure
(defn handle-event [event]
  (match event
    (Player {:id id}) (handle-player id)
    (Enemy  {:id id}) (handle-enemy  id)
    _                 (handle-unknown event)))
```

こうしておくと、新しい型を追加したくなったときも

* `defenum` にメンバーを 1 行追加
* 該当する `match` に分岐を 1 行追加

というパターンで拡張できます。

### 4.3 Clojure の protocol / multimethod の代替として

Clove には `protocol` / `multimethod` が無い代わりに、

* `deftype` でデータ構造（shape）を定義
* 名前空間ごとに「その型を受け取る関数群」をまとめる
* 型による分岐が必要な部分は `match` に閉じ込める

というスタイルを想定しています。

```clojure
(ns app::render
  (:require [app::types :as t]))

(defn render-entity [entity]
  (match entity
    (t::Player {:id id}) (render-player entity)
    (t::Enemy  {:id id}) (render-enemy  entity)
    _                    (render-fallback entity)))
```

* ディスパッチロジックの入口が `render-entity` に集約される
* データはあくまで map なので、外部ライブラリや JSON/YAML との相互運用も簡単

---

## 5. まとめ

* `deftype` は「名前付き product type（中身は map）」を定義し、コンストラクタ・述語・メタ情報を提供する。
* `defenum` は複数の `deftype` をひとつのカテゴリとして束ねる。
* `match` はそれらの型に対してパターンマッチで分岐を書くための special form。
* どれも **軽量な ADT 風** に振る舞いつつ、実体は map ベースなので Clove 全体の世界観とも相性が良い。

今後、LSP・フォーマッタ・ドキュメント生成・最適化などで、この型情報をさらに活用していく予定です。

---
<!-- NAV:START -->
**前へ:** [外部連携（foreign blocks / Ruby / Python）](interop_foreign.ja.md)
**次へ:** [標準ライブラリ `std` の使い方](stdlib.ja.md)
<!-- NAV:END -->

