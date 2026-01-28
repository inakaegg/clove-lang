# コレクション

- 更新日: 2026-01-14

Clove の基本コレクションは次の 4 つです。

- list: `(a b c)`
- vector: `[a b c]`
- map: `{:a 1 :b 2}`
- set: `#{:a :b}`

## 1. 共通: “コレクションは callable”

Clojure と同じ発想で、いくつかのコレクションは **関数のように呼べます**。
（例: map を “キーで引く関数” として使う）

```clojure
(def m {:a 1 :b 2})
(m :a)        ; => 1
(m :missing)  ; => nil
(m :missing 0); => 0  (default)
```

vector も index で呼べます。

```clojure
(def v [10 20 30])
(v 0)      ; => 10
(v 99)     ; => nil
(v 99 :d)  ; => :d
```

> `indexer` 構文糖（`v[0]`）と組み合わせると読みやすくなります。

## 2. spread

コレクション内部の `*x` は “展開” です。

```clojure
(def xs [1 2])
[0 *xs 3]      ; => [0 1 2 3]
```

map と set でも使えます。

## 3. seq と永続データ

Clove は永続コレクション（immutable）を基本にし、
`map`, `reduce` などの関数は “seq 入力” を前提にします。

> どの関数が vec を返すか / lazy を返すかは `std` 側の方針に従います。

関連: [標準ライブラリ](stdlib.ja.md)

## 4. コレクション関数の束縛ショートハンド

`map` / `filter` などの **(f/pred) + coll** 型の関数は、**先頭が束縛ベクタ**のとき、糖衣として動きます。

```clojure
(map [x [1 2 3]] (+ x 10))
(filter [x [1 2 3 4]] (odd? x))
(remove [x [1 2 3 4]] (odd? x))
(take-while [x [0 1 2 3 0]] (< x 3))
(sort-by [s ["aa" "b" "ccc"]] (count s))
```

- `for` や `doseq` と同じく **パターン束縛**が使えます。
- 内部的には `fn + let` を組み立てて `core::<name>` を呼びます。
- 対象: `map` / `filter` / `pmap` / `pfilter` / `remove` / `keep` / `some` / `every?` / `not-any?` / `not-every?` / `take-while` / `drop-while` / `split-with` / `partition-by` / `group-by` / `run!` / `sort-by`
- `sort-by` は `(sort-by [x coll] expr)` のみ対応（comparator ありは非対応）

## 5. mut / imut（mut は shallow / imut は deep）

`mut` は **shallow 変換**です。  
map/vector/set の **トップレベルだけ** を mutable 化し、子要素は immutable のまま保持します。  
`assoc-in!` / `update-in!` は **触れた経路だけ** を自動的に mut 化しながら更新します。  
一方 `imut` は **deep freeze** のままで、mutable を再帰的に immutable 化します。

- `assoc!` / `assoc-in!` / `conj!` / `dissoc!` / `disj!` / `pop!` / `update!` / `update-in!` / `merge!` などの破壊的 API は **mut にだけ許可**
- `assoc` / `conj` / `dissoc` / `disj` / `into` は mut/imut どちらも受け、**常に新しい imut を返す**
- set に mutable コレクションを入れるのは禁止（`(imut x)` を使う）
- mut set は要素を常に imut として保持する（`conj!` も同様）
- chan/agent/foreign などのスレッド境界には mut を渡せない（必要なら `(imut x)`）

### 例: world を mut で更新

```clojure
(def world (mut {:player (mut {:x 10 :y 20})
                 :bullets (mut [])
                 :tick 0}))

(assoc! world :tick (+ world.:tick 1))
(assoc! world.:player :x (+ world.:player.:x 3))
(conj!  world.:bullets (mut {:x 100 :y 200 :vx 5 :vy 0}))
```

### 例: update! / update-in! で更新

```clojure
(let [m (mut {:a {:b 1}})]
  (update! m :a (fn [v] (assoc v :c 2)))
  (update-in! m [:a :b] inc)
  (get-in m [:a :b])) ; => 2
```

### 例: assoc-in! / merge! で更新

```clojure
(let [m (mut {:a {:b 1}})]
  (assoc-in! m [:a :b] 2)
  (merge! m {:c 3})
  (get-in m [:a :b])) ; => 2
```

### 例: thread 境界で snapshot

```clojure
(let [snapshot (imut world)]
  (future (fn [] (save-replay snapshot))))
```

### 例: 古い参照の注意（仕様）

```clojure
(let [p world.:player]
  (assoc! world :player (mut {:x 0 :y 0})) ; player を丸ごと差し替え
  p.:x) ; p は古い player を見続ける
```

### 判定

```clojure
(mut? (mut {:a 1})) ; => true
(imut? {:a 1})      ; => true
```

---
<!-- NAV:START -->
**前へ:** [リテラル（数値・文字列など）](literals.ja.md)
**次へ:** [関数（defn / fn / #() / 可変長）](functions.ja.md)
<!-- NAV:END -->

