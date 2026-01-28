# デストラクチャリング

- 更新日: 2025-12-21

Clove の `let` と `fn` は Clojure 風のデストラクチャリングをサポートします。

## 1. vector デストラクチャ

```clojure
(let [[a b] [10 20]]
  [a b])
;; => [10 20]

(let [[a b & rest] [1 2 3 4]]
  [a b rest])
;; => [1 2 [3 4]]
```

## 2. map デストラクチャ: `:keys`

```clojure
(let [{:keys [a b]} {:a 1 :b 2 :c 3}]
  [a b])
;; => [1 2]
```

## 3. `:as`（元の値を保持）

```clojure
(let [{:keys [a] :as m} {:a 1 :b 2}]
  [a m])
;; => [1 {:a 1 :b 2}]
```

## 4. `:or`（デフォルト）

```clojure
(let [{:keys [a b] :or {b 99}} {:a 1}]
  [a b])
;; => [1 99]
```

## 5. “別名束縛”

Clojure 同様に

- `keys :keys`
- `as :as`

のような “キー名と束縛名の衝突” を解決できます。

```clojure
(let [{:keys [keys as]
       keys :keys
       as   :as
       :as  whole}
      {:keys [1 2] :as "foo"}]
  [keys as whole])
```

## 6. 関数引数でも同じ

```clojure
(defn f [{:keys [a] :as m} [x y]]
  [a m x y])
```

---
<!-- NAV:START -->
**前へ:** [関数（defn / fn / #() / 可変長）](functions.ja.md)
**次へ:** [制御構文（if/when/cond/for/while/try など）](control_flow.ja.md)
<!-- NAV:END -->

