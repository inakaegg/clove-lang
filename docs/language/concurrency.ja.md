# 並行 / 非同期

- 更新日: 2026-01-14

Clove には、ゲームやツール制作で使える “軽量な並行プリミティブ” が入っています。

- channel (`chan`)
- task (`spawn`)
- future (`future`)
- promise (`promise`)
- agent (`agent`)
- atom (`atom`)
- select (`select` / `scope-select`)
- cancellation (`cancelled?` / `cancel-chan`)
- `async-scope`（special form）

> ここでは「概念と使い方」を説明します。関数ごとの細部は `:doc` を参照してください。

## 1. atom

```clojure
(def a (atom 0))
@a                 ; => 0
(atom-set! a 10)
(swap! a + 1)      ; alias: swap! -> atom-update!
@a                 ; => 11
```

- `atom` は validator と watch を持てます。
- `add-watch`/`remove-watch` は atom にも使えます。

## 2. channel

### 2.1 生成

```clojure
(def c (chan))     ; cap=unbounded
(def c1 (chan 1))  ; cap=1
(def c0 (chan 0))  ; rendezvous (unbuffered)
```

### 2.2 put / take

```clojure
(put! c 1)     ; alias of chan-put!
(take! c)      ; alias of chan-take!

(>!! c 1)      ; alias
(<!! c)
```

- `chan-put!` は `true/false` を返します（close 後は `false`）。
- `chan-take!` は値を返し、close 後に空なら `nil`。

### 2.3 close

```clojure
(chan-close! c)
(chan-closed? c) ; => true/false
```

## 3. timeout

`timeout` は Duration か ms(int) を受け取る “通知チャネル” を返します。

```clojure
(def t (timeout 100))
(<!! t) ; => :timeout など
```

※ 返す値は実装に準拠します。詳細は `:doc timeout`。

## 4. task / future / promise

### 4.1 task: `spawn`

```clojure
(def t (spawn (fn []
               (sleep-ms 10)
               42)))
@t
```

### 4.2 future

```clojure
(def f (future (fn []
                (sleep-ms 10)
                42)))
@f
```

### 4.3 promise

```clojure
(def p (promise))
(promise-deliver! p 42)
@p ; => 42
```

### 4.4 `go-loop`

`go-loop` は **`loop` を別タスクで回す**ための special form です。  
返り値は `task` になります。

```clojure
(def t
  (go-loop [i 0]
    (when (< i 3)
      (println "tick" i)
      (sleep-ms 10)
      (recur (inc i)))))
@t
```

## 5. agent

agent は “状態を持つワーカー” です。

```clojure
(def ag (agent 0))
(agent-send! ag (fn [x] (+ x 1)))
(agent-await ag)
@ag
```

## 6. select

複数のチャネルから “どれかが先に届いたもの” を受け取ります。

```clojure
(select
  [c1 (fn [v] [:c1 v])]
  [c2 (fn [v] [:c2 v])])
```

## 7. cancellation

- `cancelled?` は “現在のキャンセルコンテキストがキャンセルされたか” を返します。
- `cancel-chan` は “キャンセルされたら準備完了になるチャネル” を返します。

これらは `async-scope` の内部で特に重要です。

## 8. `async-scope`

`async-scope` は **子タスク群 + main-body** をまとめ、
main-body が終わったら “キャンセル” を流すことで子を片付けます。

```clojure
(async-scope
  [(future (fn []
             (loop []
               (when (not (cancelled?))
                 (sleep-ms 10)
                 (recur)))))]
  42)
;; => 42
```

### fail-fast 版

- [async-scope-fail-fast](../async-scope-fail-fast.ja.md)

> 子タスクは “キャンセルを監視する” よう書くのがコツです。

## 9. `scope-loop` / `async::scope-loop`

`scope-loop` は **キャンセルを監視する loop** です。  
`async::scope-loop` は同義の別名です。

```clojure
(async-scope
  [(future (fn []
             (scope-loop [i 0]
               (when (< i 3)
                 (println "child" i)
                 (sleep-ms 10)
                 (recur (inc i))))))]
  42)
```

## 10. `pmap` / `pfilter`

`pmap` / `pfilter` は **束縛ベクタ + 本文**で書ける糖衣です。  
`[binding coll]` を取り、内部的に `core::pmap` / `core::pfilter` を呼び出します。

```clojure
(pmap [x [1 2 3]] (+ x 10))
(pfilter [x [1 2 3 4]] (odd? x))
```

オプションマップは **先頭か binding の直後**に置けます（どちらか 1 つだけ）。

```clojure
(pmap {:workers 4} [x xs] (heavy x))
(pfilter [x xs] {:workers 4} (odd? x))
```

`std::pmap` / `std::pfilter` は `dag::pmap` / `dag::pfilter` の alias です。

### 10.1 `dag::pmap` / `dag::pfilter`

`dag::pmap` と `dag::pfilter` は、**固定スレッドプール + チャンク**で動く並列版です。

オプション（`dag::normalize-opts` による正規化）:

- `:workers`（旧 `:max-parallel` の alias）
- `:chunk`
- `:buffer`（チャンク単位のキュー容量）
- `:ordered?`（true なら入力順を維持）
- `:on-error`（`:throw | :cancel | :skip | :collect`）

### 10.2 基本例

```clojure
(dag::pmap {:workers 4} inc [1 2 3])        ; => [2 3 4]
(dag::pfilter {:workers 4} odd? [1 2 3 4])  ; => [1 3]
```

### 10.3 効果測定サンプル

```clojure
(def xs (range 200000))

(defn heavy [x]
  (loop [i 0 acc 0]
    (if (< i 2000)
      (recur (inc i) (+ acc (bit-and x i)))
      acc)))

; 重い処理は :workers の差が出やすい
(time #(do (vec (map heavy xs)) nil))
(time #(do (dag::pmap {:workers 2} heavy xs) nil))
(time #(do (dag::pmap {:workers 8} heavy xs) nil))

; 軽い処理はオーバーヘッドが勝つことがある
(time #(do (vec (map inc xs)) nil))
(time #(do (dag::pmap {:workers 8} inc xs) nil))
```

---
<!-- NAV:START -->
**前へ:** [正規表現 / Duration リテラル](regex_duration.ja.md)
**次へ:** [外部連携（foreign blocks / Ruby / Python）](interop_foreign.ja.md)
<!-- NAV:END -->

