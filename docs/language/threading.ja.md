# スレッディング / パイプ

- 更新日: 2026-01-14

Clove は Clojure の “threading macro 風” の special form を持ちます。

## 1. `->` / `->>`

```clojure
(-> x
    (f 1)
    (g 2))

(->> xs
     (map inc)
     (take 10))
```

## 2. `as->`

```clojure
(as-> (range 10) it
  (shuffle it)
  (take it 3))
```

## 3. `cond->` / `cond->>`

```clojure
(cond-> 1
  true  inc
  false dec
  (> 5 3) (+ 10))
; => 12
```

```clojure
(cond->> [1 2 3]
  true  (map inc)
  false (filter odd?))
; => [2 3 4]
```

- `cond->` / `cond->>` は **テストが truthy のときだけ** そのフォームを適用します。
- テスト/フォームは **ペア**で書きます。

## 4. `some->` / `some->>`

```clojure
(some-> {:a {:b 10}}
  :a
  :b
  (+ 1))
; => 11

(some->> [1 2 3]
  (map inc)
  (filter odd?)
  (reduce + 0))
```

- `some->` / `some->>` は **途中で nil になったら即 nil** になります。

## 5. dot-chain との住み分け

- `->`/`as->` は “フォーム列の変形”
- dot-chain は “メソッド風の段階式”

dot-chain の特徴は `?` プレースホルダです。

詳細: [dot-chain](dot_chain.ja.md)

---
<!-- NAV:START -->
**前へ:** [制御構文（if/when/cond/for/while/try など）](control_flow.ja.md)
**次へ:** [名前空間（ns/require/resolve/load など）](namespaces.ja.md)
<!-- NAV:END -->

