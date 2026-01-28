# OOP 新仕様: オブジェクト内メソッド / self / method

このページは OOP 記法（`obj.method(...)`）のうち、
**オブジェクト内メソッド**と **self 参照**の新仕様だけをまとめたものです。
チェイン自体の基本は [docs/language/oop_syntax.md](/docs/language/oop_syntax.ja.md) を参照してください。

## 目的

- `.foo` は **キーアクセスではなくメソッド呼び出し**に統一。
- map / deftype を **メソッドを持つオブジェクト**として扱う。
- `assoc/merge/dissoc` でメソッドを追加・削除できる（イミュータブル）。
- `canvas` のようなコンテキストは引数ではなく **self から参照**する。

## `.foo` のディスパッチ順

`recv.foo(args...)` の解決は次の順。

1. `recv` が map / deftype インスタンスで、キー `:foo` が **存在する**場合:
   - 値が **callable** なら:
     - method（is_method=true）なら `recv` を先頭に注入して呼ぶ。
       - `recv[:foo](recv, args...)`
     - method ではない callable なら **そのまま呼ぶ**（self 注入しない）。
       - `recv[:foo](args...)`
   - 値が **callable でない**場合はエラー（フォールバックしない）。
     - 値アクセスは `recv.:foo` / `recv[:foo]` を案内。
2. `recv` が `:foo` を **持っていない場合のみ**、従来の OOP chain にフォールバックして `foo` を呼ぶ。

## method の定義方法

### 1) `(method ...)`

```
(method [args...] body...)
```

- `self` が先頭引数に自動で挿入される。
- 生成される関数には `is_method=true` が付く。
- `self` は予約語なので **引数に書かない**（暗黙で利用可）。

### 2) `deftype ... (where (defn ...))`

```
(deftype Renderer {canvas: Any}
  (where
    (defn clear [] (sdl2::clear &:canvas))
    (defn set-color [c] (sdl2::set-draw-color &:canvas c.0 c.1 c.2 c.3))))
```

- `where` 内の `defn` は **常に method**。
- 生成時にインスタンス map へコピーされる（`dissoc` で削除できる）。
- `deftype` 直下でも `defn` / `method` を書ける（`where` の省略記法）。

```
(deftype Renderer {canvas: Any}
  (defn clear [] (sdl2::clear &:canvas))
  (method set-color [c] (sdl2::set-draw-color &:canvas c.0 c.1 c.2 c.3)))
```

### 3) `fn/defn` の本文に `&` が出たら method 推論

```
(assoc r :draw (fn [] (draw &:canvas)))
```

- 本文に `&` / `self` が出たら self 参照とみなし method 化。
- `self` 自動挿入 + `&` 書き換え + `is_method=true` を付与。

## self 参照糖衣

### `&` / `&:key` / `&[:key]`

- `&` は **self 自体**。
- `&self` は **self の明示形**（`&` と同じ）。
- self のフィールド参照は **`&:key` または `&[:key]`** を使う。
  - `&:key` と `&[:key]` はどちらでも OK。

### 廃止された記法

- `&x` / `&.` / `&root` は **即エラー**。
  - self は `&`
  - self のフィールド参照は `&:key` / `&[:key]`
  - map-ref は `&^:key` / `&:key`（map literal 内のみ）
  - 明示したい場合は `&ref:...` / `&ref^:...` も使える
  - ルート map 自体は `&ref`（map literal 内のみ）

例:

```
(method []
  {:x &:x
   :self &})
```

※ `&:x` は method 文脈では self 参照に固定されます（map literal 内でも self 優先）。

## `assoc/merge/dissoc` での拡張

- `assoc` / `merge` で `method` を入れれば `.foo` で呼べる。
- `dissoc` で method を外せる。

## 例

```
(use oop-syntax true)

(def obj
  {:x 10
   :add (method [n] (+ &:x n))
   :pack (method [] {:x &:x :self &})})

(obj.add 5) ; => 15

(def obj2 (assoc obj :mul (fn [n] (* &:x n))))
(obj2.mul 3) ; => 30

(def obj3 (dissoc obj2 :mul))
(obj3.mul 1) ; => エラー（method が無い）

(def obj4 (assoc obj :plain (fn [x] (+ x 1))))
(obj4.plain 10) ; => 11（self 注入なし）
```

---
<!-- NAV:START -->
**前へ:** [OOP 記法（`obj.method(...)`）](oop_syntax.ja.md)
**次へ:** [OOP メソッド新記法クイックリファレンス](oop_methods_quickref.ja.md)
<!-- NAV:END -->

