# 型ヒント

English version: [type_hints.md](type_hints.md)

- 更新日: 2026-07-25

型ヒントは任意の注釈です。あくまで **ヒント** であり、実行時の型チェックは行いません。

## 1. 書ける場所

### 1.1 関数定義

**戻り値**の型は名前の直後にキーワードで、**引数**の型は後置の `名前<Type>` 形式で書きます。

```clojure
(defn add :int [x<Int> y<Int>]
  (+ x y))

(add 1 2) ; => 3
```

どちらも省略できます。

```clojure
(defn add [x y] (+ x y))           ; ヒントなし
(defn add :int [x y] (+ x y))      ; 戻り値だけ
(defn add [x<Int> y<Int>] (+ x y)) ; 引数だけ
```

> 引数の型を素のキーワードで書く形（`[x :int]`）は構文として **無効** で、
> `fn params must be symbols, vector destructuring, or map destructuring` になります。

同じ後置形式が `fn` と `let` でも使えます。

```clojure
((fn [x<Int>] (+ x 1)) 10)  ; => 11
(let [n<Int> 5] (+ n 1))    ; => 6
```

### 1.2 シンボルへの `<...>`

束縛するシンボルには `<...>` を付けられます。

```clojure
(def x<Int> 10)
```

### 1.3 式への `expr: TYPE`

`expr: TYPE` の形では、`:` 以降が **型式** として読まれます。

```clojure
(def v [1 2]: [Int Int])
```

## 2. ヒントは実行時の値を変えない

`type` は **実行時の値の型** を返すので、ヒントには追従しません。

```clojure
(def v [1 2]: [Int Int])
(type v) ; => :core::Vector

(def x<Int> 10)
(type x) ; => :core::Int  ; 値が実際に Int なので一致しているだけ
```

呼び出し時にも強制されません。

```clojure
(defn add :int [x<Int> y<Int>] (+ x y))
(add 1.5 2.5) ; => 4.0  ; エラーにならない
```

## 3. 何に使われるか

- `doc` / `describe` の出力
- LSP の表示と補完
- `clove build` が使う typed な lowering

`clove build` は、対応済みのサブセットを typed IR へ落として C を生成します。
`--opt` のような切り替えはなく、typed 経路が唯一の build 経路です。未対応の構文は
インタプリタへフォールバックせず build 時にエラーになります。
現在のオプション一覧は [Build](../tooling/build.ja.md) を参照してください。

---
<!-- NAV:START -->
**前へ:** [型/enum/match（deftype/defenum/match）](types_enum_match.ja.md)
**次へ:** [標準ライブラリ `std` の使い方](stdlib.ja.md)
<!-- NAV:END -->
