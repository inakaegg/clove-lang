# 制御構文（special forms）

- 更新日: 2026-01-14

Clove は一部の構文を special form として評価器が直接処理します。

## 1. 基本

- `if`
- `do`
- `let`

```clojure
(if cond
  then
  else)

(do
  (println "a")
  (println "b")
  42)

(let [x 1
      y 2]
  (+ x y))
```

- `if` は **truthy 判定**（`nil` と `false` のみ false）。

## 2. 条件分岐のバリエーション

### 2.1 `and` / `or`

```clojure
(and a b c) ; すべて truthy なら最後の値
(or  a b c) ; 最初に truthy になった値
```

- `and` は 0 引数なら `true` を返します。
- `or` は 0 引数なら `nil` を返します（引数がある場合は「最初の truthy / 無ければ false」）。

### 2.2 `when` / `when-not` / `if-not`

```clojure
(when cond
  (println "ok")
  :done)

(when-not cond
  (println "ng"))

(if-not cond
  :else-branch
  :then-branch)
```

### 2.3 `when-let` / `if-let` / `if-some`

```clojure
(when-let [x (find m :k)]
  (println x))

(if-let [x (find m :k)]
  x
  :none)

(if-some [x (get m :k)]
  x
  :missing)
```

- `when-let` / `if-let` は **truthy 判定**。
- `if-some` は **nil だけを弾く** 判定（`false` は通す）。

### 2.4 `cond`

```clojure
(cond
  test1 expr1
  test2 expr2
  :else expr3)
```

- `:else` / `_` がデフォルトです。

### 2.5 `condp`

```clojure
(condp = x
  1 :one
  2 :two
  :else :other)
```

`condp` は **predicate と expr を先に固定**し、
各節で `(pred test expr)` を評価します。

`(:>> f)` 形式で **predicate の結果を変換**できます。

```clojure
(condp re-find s
  /cat/ :>> (fn [m] [:cat m])
  /dog/ :>> (fn [m] [:dog m])
  :else :none)
```

## 3. ループ / 反復

- `loop`
- `recur`

```clojure
(loop [i 0 acc 0]
  (if (< i 10)
    (recur (+ i 1) (+ acc i))
    acc))
```

- `recur` は **tail position** にのみ置けます。
- `try` の `err` / `fin` の中では `recur` は使えません（関数の main body の tail のみ）。

### 3.2 `while`

```clojure
(def i 0)
(while (< i 3)
  (println i)
  (set! i (inc i)))
```

### 3.3 `dotimes`

```clojure
(dotimes [i 3]
  (println i)) ; i = 0,1,2
```

- 回数は **非負の整数**である必要があります。

### 3.4 `doseq`

```clojure
(doseq [x [1 2]
        y [10 20]]
  (println x y))
```

- `doseq` は **副作用用**（戻り値は `nil`）。
- `let` と同じ **パターン束縛**が使えます。

### 3.5 `for`

`for` は **ベクタを返す内包表記**です。

```clojure
(for [x [1 2 3]
      y [10 20]
      :when (odd? x)
      :let [z (+ x y)]]
  z)
; => [11 21 13 23]
```

- `:let` / `:when` / `:while` が使えます。
- `:while` が false になった時点で **その系列の反復を打ち切り**ます。
- Native のコード生成では、`:while` の条件式は **同じ束縛名のみ参照**可能です（外側の束縛参照は未対応）。

### 3.6 `each`

```clojure
(each [x [1 2 3]] (println x)) ; => [1 2 3]
(each println [1 2 3])         ; => [1 2 3]
```

- `each` は **入力コレクションを返す**副作用向けフォームです。
- `(each f coll)` は `f` を各要素に呼び出します。

## 4. スコープ / リソース / 副作用補助

### 4.1 `with-redefs`

`with-redefs` は **一時的に var を上書き**します（主にテスト用途）。

```clojure
(defn greet [] "hi")

(with-redefs [greet (fn [] "yo")]
  (greet)) ; => "yo"
```

### 4.2 `with-open`

`with-open` は **リソースを自動で close**します（逆順）。

```clojure
(with-open [f (io::open "a.txt")]
  (read f))
```

- `:close!` / `:close` メタがあればそれを使用
- なければ `close!` / `close` 関数を探す
- channel の場合は `chan-close!` 相当で close します

### 4.3 `with-dyn`

`with-dyn` は **dynamic var を一時的に束縛**します。

```clojure
(with-dyn [*out* (io::writer "log.txt")]
  (println "hello"))
```

- 現時点の dynamic var は `*out*` / `*err*` です。

### 4.4 `doto`

`doto` は **同じ対象に副作用をまとめて適用**し、対象を返します。

```clojure
(doto (StringBuilder.)
  (.append "a")
  (.append "b"))
```

## 5. 例外

- `try`
- `catch`
- `finally`
- `err`
- `fin`
- `throw`

```clojure
(try
  (do-something)
  (catch e
    (println "error" e)
    :recovered)
  (finally
    (println "cleanup")))
```

### try の明示句（err / fin）

`err` / `fin` は `try` の末尾にだけ置けます。順序は **err → fin** 固定です。

```clojure
(try
  (throw 1)
  (err (do (println "err") ?))
  (fin (println "cleanup")))
```

- `err` は例外時のみ実行され、例外値は `?` で参照できます。
- `fin` は成功/失敗に関わらず **try を抜けるときに 1 回だけ** 実行されます。
- `err` / `fin` がある場合、短縮形（末尾 callable 形式）は併用できません。

### body 末尾の err / fin（暗黙 try）

`err` / `fin` は `try` だけでなく、**body（複数フォーム列）** の末尾にも置けます。  
末尾に置くと、それ以前のフォーム列が暗黙的に `try` で包まれます。

```clojure
;; do
(do
  (throw 1)
  (err ?)
  (fin (println "cleanup")))

;; fn
(fn []
  (throw 2)
  (err ?)
  (fin (println "cleanup")))

;; #(...) は単一フォームなので do で包む
#(do
   (throw 3)
   (err :failed)
   (fin (println "cleanup")))

;; トップレベル（REPL / load-file）
(throw 4)
(err ?)
(fin (println "cleanup"))
```

### try の短縮形

`try` には末尾の callable を使う短縮形があります。末尾 1〜2 フォームが callable のときだけ成立し、
順序は **on-error → on-finally** 固定です。

- `(try body+ on-error)`（on-error は 1 引数 callable）
- `(try body+ on-finally)`（on-finally は 0 引数 callable）
- `(try body+ on-error on-finally)`
- `(try [bindings] expr on-error on-finally)`（bindings short。必須）

例: `(try (throw 1) 42)` は構文エラーになります。代わりに:

```clojure
(try (throw 1) (fn [e] 42)) ; => 42
```

## 6. quote 系

- `quote`
- `quasiquote`
- `unquote`
- `unquote-splicing`

詳しくは [Reader / 構文一覧](reader_syntax.ja.md)。

## 7. `comment`

`comment` は **中身を評価せず** `nil` を返します。

```clojure
(comment
  (println "skip")
  1 2 3)
```

## 8. `async-scope`

`async-scope` は “子タスクのスコープ管理” をする special form です。

詳細: [並行/非同期](concurrency.ja.md)

## 9. デバッグ用フォーム

### 8.1 `p`（デバッグ出力）

`p` は評価結果を **`file:line:col: 値`** の形式で表示し、値を返します。

- `(p f a b ...)` は `(f a b ...)` と同じ評価を行い、表示して返します。
- `(p x)` は `x` を評価して表示・返却します。
  - ただし `x` が **Symbol かつローカル束縛ではない** 場合は、0 引数 callable を **呼び出し** として扱います。

```clojure
(p + 1 2) ; => 3
(p x)     ; => x の評価結果
```

### 8.2 `repl` / `debug` / `break`

```clojure
(repl)         ; その場で REPL を開く
(repl value)   ; value を ? などにセットして開く
(debug)        ; repl の別名
(break)        ; 簡易ブレークポイント（デバッグ REPL）
```

- `repl` / `debug` / `break` は **その場で REPL を起動**します。
- `:q` / `:quit` で抜けられます。
- 使い方の詳細は [REPL ガイド](../tooling/repl.ja.md) を参照してください。

---
<!-- NAV:START -->
**前へ:** [デストラクチャリング](destructuring.ja.md)
**次へ:** [スレッディング / パイプ系（-> / cond-> / some->）](threading.ja.md)
<!-- NAV:END -->

