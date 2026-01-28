# 関数

- 更新日: 2026-01-14

## 1. `defn` / `fn`

```clojure
(defn add [a b] (+ a b))
(add 1 2) ; => 3

(def inc2 (fn [x] (+ x 2)))
(inc2 10) ; => 12
```

### 1.1 docstring

`defn` は docstring を持てます。

```clojure
(defn greet
  "say hello"
  [name]
  (str "hello " name))

(doc 'greet) ; => "say hello" (または nil)
```

### 1.2 ローカル関数 `-defn` と `where`

`-defn` は **関数本体の中だけで使えるローカル関数** です。  
トップレベルでは使えません。

```clojure
(defn f []
  (-defn i [] 123)
  (i)) ; => 123
```

`where` は見た目を整えるためのグルーピングで、**評価結果は返り値に影響しません**。  
関数の最後に置いて内部関数をまとめる用途を想定しています。

```clojure
(defn f [] 1 (where (-defn i [] 123)))
; (f) => 1

(defn g [] (i) (where (-defn i [] 123)))
; (g) => 123
```

### 1.3 本体末尾の `err` / `fin`

`defn` / `fn` など **body（複数フォーム列）** の末尾に `err` / `fin` を置くと、
暗黙的に `try` に包まれます。

```clojure
(defn f []
  (throw 1)
  (err ?)
  (fin (println "cleanup")))
; (f) => 1
```

- 並びは **err → fin** 固定です。
- `err` / `fin` は末尾にのみ置けます。

## 2. 可変長引数

```clojure
(defn sum [x & xs]
  (reduce + x xs))
```

## 3. 多 arity

```clojure
(defn f
  ([x] x)
  ([x y] (+ x y)))
```

## 4. `#(...)` 短縮関数

```clojure
(map #( + % 10) (range 3))
```

### 4.1 プレースホルダ一覧

- `%` / `%1` / `%2` ...: 位置引数（`%` は `%1` と同じ）
- `?` / `?1` / `?2` ...: `%` と同じ意味の別名
- `%&`: rest 引数（可変長引数）
- `*?` / `*?1` / `*?2` ...: `?` と同じ位置引数だが、値を展開して渡す（splat）

> `let` / `fn` で同名が束縛されている場合、プレースホルダとしては扱われません。

## 5. 部分適用のための `?`（dot-chain と一緒に使う）

Clove の dot-chain は `?` を “プレースホルダ” として使います。

```clojure
(def result
  (range 10)
  .(shuffle ?)
  .(reverse ?)
  .(take ? 3))

result
```

詳細: [dot-chain](dot_chain.ja.md)

---
<!-- NAV:START -->
**前へ:** [コレクション（list/vector/map/set）](collections.ja.md)
**次へ:** [デストラクチャリング](destructuring.ja.md)
<!-- NAV:END -->

