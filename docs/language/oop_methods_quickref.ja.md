# OOP メソッド新記法クイックリファレンス

このページは **OOP メソッドの新記法だけ**を短くまとめたものです。
詳細は [docs/language/oop_methods.md](/docs/language/oop_methods.ja.md) を参照してください。

## 1. `.foo` の呼び出しルール

`recv.foo(args...)` は次の順で解決します。

1. `recv` が map / deftype インスタンスで、キー `:foo` を **持っている**場合
   - 値が **method** なら `recv` を先頭に注入して呼ぶ
   - 値が **callable だが method ではない**ならそのまま呼ぶ
   - 値が **non-callable** ならエラー（`recv.:foo` / `recv[:foo]` を案内）
2. `recv` が `:foo` を **持っていない**場合のみ、従来の OOP chain へフォールバック

## 2. self の書き方

- `&` は **self**
- `&self` は **self の明示形**
- self のフィールド参照は **`&:key` または `&[:key]`**
- `&x` / `&.` / `&root` は **廃止**（使うとエラー）

## 3. method の作り方

- `(method [args...] ...)` を使う
- `deftype` の `defn` / `method` は常に method（`where` あり/なし両方OK）
- `fn/defn` の本文に `&` または `self` が出たら **method 推論**
- `self` は予約語なので **引数に書かない**

## 4. map-refs（map literal 専用）

- 有効化: `(use map-refs true)`
- **同じ map（this）** は `&^:key`
- **ルート map（root）** は `&:key`
- セグメントは **keyword のみ**（例: `&:screen:h`）
- `&ref:...` / `&ref^:...` はそれぞれ `&:...` / `&^:...` の別名
- `&ref` は **ルート map 自体**（`get` / indexer と併用可）

```clojure
(use map-refs true)
{:a {:x 1
     :child {:y (+ &^../:x 2)}}}
```

> method 文脈では `&:key` / `&^:key` は self 参照に書き換わります。
> map-ref を使いたい場合は method の外で書くのが安全です。

## 5. 例

```clojure
(use oop-syntax true)

(let [obj {:x 100
           :add (method [n] (+ &:x n))
           :plain (fn [a] (+ a 1))
           :pack (method [] {:x &:x :self &})}]
  (println "add =" (obj.add 5))
  (println "plain =" (obj.plain 10))
  (println "pack =" (obj.pack)))

(use map-refs true)
(def cfg {:screen {:h 100}
          :pipe {:gap 10
                 :gap-half (int (/ &^:gap 2))
                 :gap-max (- &:screen:h &^:gap-half)}})
```

---
<!-- NAV:START -->
**前へ:** [OOP メソッド（method / where / self）](oop_methods.ja.md)
**次へ:** [マップ省略記法（JS 準拠）](map_shorthand.ja.md)
<!-- NAV:END -->

