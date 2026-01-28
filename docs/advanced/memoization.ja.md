# メモ化と永続キャッシュ (`memo` / `memoize`)

Clove には「関数の結果をキャッシュする」ための仕組みとして

* `memoize` … プロセス内メモリだけに保持するシンプルなメモ化
* `memo` … TTL やディスクストアを持つ永続メモ化

が用意されています。

このドキュメントでは現時点の実装に基づいて、それぞれの挙動と使い分けを説明します。

---

## 1. `memoize` – メモリ内メモ化

```clojure
(defn slow-add [a b]
  (println "called slow-add")
  (+ a b))

(def fast-add (memoize slow-add))

(fast-add 1 2) ; => "called slow-add" が表示される
(fast-add 1 2) ; => キャッシュから返るので表示されない
```

* 引数のタプル（ベクタ）をキーに、結果をメモリ内に保持します。
* プロセスが終了するとキャッシュは失われます。
* TTL やディスクへの保存は行いません。

---

## 2. `memo` – TTL 付き / 永続キャッシュ

`memo` は、`memoize` に加えて

* TTL（有効期限）
* ディスクへの保存場所

などを指定できる拡張版です。

### 2.1 基本的な使い方

```clojure
(defn fib :int [n :int]
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(def fast-fib
  (memo fib {:ttl 1h}))
```

* 第 1 引数: キャッシュしたい関数（callable）
* 第 2 引数: オプション map（省略可能）

`fast-fib` は、内部的には「`fib` をラップした関数」で、
結果をキャッシュしつつ同じ引数ではキャッシュから返すようになります。

### 2.2 オプション

現時点では、少なくとも次のキーがサポートされています。

* `:ttl` … キャッシュの有効期限

  * `Duration` リテラル (`1h`, `5m` など)
  * 数値（秒単位の `int` / `float`）
  * `nil` の場合は「期限なし」
* `:store` … キャッシュ保存先パス（文字列）

  * 指定すると、ディスクにキャッシュを保存する
  * `~` 展開や相対パス解決等は実装側の `resolve_store_path` に依存

例:

```clojure
(def fast-fib
  (memo fib
        {:ttl   1h
         :store ".clove-memo/fib"}))
```

* 初回実行時に `fib` を評価し、結果をディスクに保存
* 以降、同じ引数では TTL 内であればディスクキャッシュから返る

---

## 3. TTL の単位と解釈

`ttl` オプションの値は次のように解釈されます。

### 3.1 `Duration` リテラル

```clojure
{:ttl 10s}
{:ttl 5m}
{:ttl 1h}
```

内部的には Duration → ミリ秒への変換が行われ、その値が TTL として使われます。

### 3.2 数値（秒）

```clojure
{:ttl 60}    ; 60 秒
{:ttl 0.5}   ; 0.5 秒
```

* `int` / `float` の場合、**秒単位** として扱われます。
* 内部では `seconds * 1000` によるミリ秒変換を行います。

### 3.3 `nil`

```clojure
{:ttl nil}
```

* 「期限なし」として扱われます。
* 実装では `None` として扱われ、保存時刻は記録するものの TTL チェックはスキップされます。

---

## 4. キャッシュキー

`memo` / `memoize` はどちらも、**引数ベクタ** をキーとして使います。

```clojure
(memo f)   ; f の (args...) に対して、[args...] をキーにする
```

* すべての引数が `Vector<Value>` として格納されます。
* そのベクタに対してハッシュを取り、`HashMap` のキーにします。
* 値には `OnceCell` でラップされた「実際の結果＋保存時刻」が入ります。

そのため:

* 「同じ値でも `= / hash` の意味が違う」ような独自型をキーにするときは注意
* 非シリアライズ可能な値（関数 / チャネル / Atom など）を引数に使うとディスクストアとの相性が悪くなる

---

## 5. いつ使うべきか

### 5.1 向いているケース

* 計算コストが高く、引数のパターンが限られている関数

  * 例: `fib`, 組み合わせ爆発しない動的計画法の再帰実装
* 外部 API / DB アクセスを伴う読み取り系関数

  * TTL を付けて「数秒～数分は同じ結果で良い」ようなケース
* 一度計算したらセッションをまたいでも reuse したい重い処理

### 5.2 向いていないケース

* 副作用がメインの関数（ログ出力 / 外部システムへの書き込みなど）
* 引数のバリエーションが非常に多い関数

  * メモリ・ディスクを大量に消費する可能性がある
* 「絶対に最新状態でなければならない」ようなリアルタイム系の処理

---

## 6. パターン集

### 6.1 高コストな pure 関数のキャッシュ

```clojure
(defn heavy-calc [params]
  ;; ... 重い計算 ...
  )

(def cached-heavy-calc
  (memo heavy-calc {:ttl 5m}))
```

### 6.2 I/O を含むがある程度 stale でも良い関数

```clojure
(defn fetch-config-from-remote []
  ;; HTTP 経由で設定を取得
  )

(def cached-config
  (memo fetch-config-from-remote
        {:ttl   60
         :store ".clove-memo/config"}))
```

---

## 7. 実装メモ（開発者向け）

* 内部では

  * `HashMap<Vector<Value>, Arc<OnceCell<MemoValue>>>`
  * `MemoValue` = `{ value: Value, saved_at_ms: i64 }`
* ディスクストアが有効な場合:

  * キーをシリアライズしてファイル名やパスに変換
  * TTL を超えていなければディスクからロード
* `build_memoized` 関数で `Value::Func` をラップした新しい callable を生成

詳細は `crates/clove-lang/src/memo_support.rs` を参照してください。

---

## 8. まとめ

* `memoize` はプロセス内メモリのみ、シンプルなメモ化。
* `memo` は TTL / ディスクストア付きの永続メモ化。
* `:ttl` は Duration リテラルか秒数（`int` / `float`）で指定。
* キャッシュキーは引数の `Vector<Value>`。関数 / 遅延評価など非シリアライズ可能な値をキーにするのは避ける。
* 重い pure 関数や読み取り系 I/O 関数の「ほどよいキャッシュ」に向いている。

────────────────────────────────────────────────────────────────────────────────

---
<!-- NAV:START -->
**前へ:** [名前空間設計ノート](namespaces_design.ja.md)
**次へ:** [実行時設定（use / use-syntax）](runtime_settings.ja.md)
<!-- NAV:END -->

