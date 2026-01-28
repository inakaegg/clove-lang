# 名前空間（ns / require）

- 更新日: 2026-01-14

Clove は **`::` 区切り** の名前空間を基本にします。

```clojure
(ns game::snake)

(defn -main []
  (println "hello"))
```

## 1. `ns`

`ns` は現在の名前空間を定義します。

- ファイル配置と `ns` が一致しない場合、REPL/実行時に警告が出ることがあります。
  （LSP のジャンプ/解決にも影響します。）

## 2. `require`

```clojure
(require game::util)

(game::util::foo 1)
```

### 2.1 ファイルパス指定（文字列ターゲット）

`require` のターゲットに文字列を渡すと、**ファイルモジュールとしてロード**します。
`ns`/`require` と同様に `:as` / `:refer` / `:rename` を指定できます。

```clojure
(require "../myns/aaa" :as aaa :refer :*)
```

```clojure
(ns tmp::match_type1
  (:require ["./match_type" :as mt :refer :*]))
```

- ファイル側に `(ns ...)` が無い場合は、**パス由来の暗黙 namespace** が割り当てられます。
- 「現在の namespace に定義を流し込みたい」用途は `load-file` を使ってください。

### 2.2 型の import

- `:refer` は **値だけでなく型も import** します。
- `:as` で付けた alias は **型注釈でも `alias::Type` として使えます**。
- `defenum` は `Noop`/`Quit` のような **variant 型名を暗黙で作成**します
  （`qualified-only` のときは作りません）。
- `:refer :*` で取り込んだ unqualified 名は **その `ns` 内で既存の名前を上書き**します。
  既存の組み込みを使いたい場合は `core::` のように **完全修飾**してください。

> `require` の探索パスや “どこを root として見るか” は CLI/REPL の `:source` / working_dir に依存します。

## 3. namespace 操作 / 参照

### 3.1 `current-ns`

現在の namespace を **Symbol** で返します。

```clojure
(current-ns) ; => user など
```

### 3.2 `ns-map`

指定 namespace の **公開値を keyword-map として取得**します。

```clojure
(ns-map)           ; 現在の namespace
(ns-map 'my::ns)   ; 指定 namespace
```

存在しない場合は `nil` です。

### 3.3 `create-ns`

namespace を **明示的に作成**します。

```clojure
(create-ns 'my::tmp)
```

### 3.4 `refer`

`refer` は **公開 export を現在の namespace に取り込む** special form です。

```clojure
(refer 'my::ns)
(refer 'my::ns :only [foo bar])
(refer 'my::ns :exclude [debug])
```

### 3.5 `resolve`

`resolve` は **Symbol を値に解決**します（見つからない場合は `nil`）。

```clojure
(resolve 'core::map)
(resolve 'my::ns::foo)
(resolve 'foo) ; current-ns を優先
```

## 4. 動的ロード / 評価

### 4.1 `load-file`

`load-file` は **ファイルを読み、フォームを評価**します。

```clojure
(load-file "./script.clv")
```

- 返り値は **最後のフォームの結果**。
- `require` は “モジュールとしてロード” するのに対し、`load-file` は **現在の環境へ流し込む**用途です。

### 4.2 `load-string`

```clojure
(load-string "(+ 1 2)")
```

- 文字列から読み取って **逐次評価**します。

### 4.3 `eval`

`eval` は **コードデータを評価**します。

```clojure
(eval '(+ 1 2))          ; => 3
(eval (read-string "(+ 1 2)"))
```

- 文字列をそのまま渡すのではなく、**`quote` か `read-string`** を使います。

## 5. 区切り文字について

- **推奨:** `aaa::bbb::ccc`
- 互換/過去資産で `aaa/bbb` のような区切りが混在している場合、
  LSP 等が正しく追跡できないことがあります。

（この点は repo 全体で `::` に統一することを推奨します。）

## 6. `use` と `use-syntax`

`use` は `use-syntax` の別名で、Reader 拡張のON/OFFを制御します。

```clojure
(use-syntax indexer true)
```

詳細: [実行時設定](../advanced/runtime_settings.ja.md)

## 7. private 定義（`def-` / `defn-`）

`def-` / `defn-` は **private な var** を定義します。  
同一 namespace 内からのみ参照・更新できます。

```clojure
(ns a)
(def- x 1)
(defn- hidden [] 42)

(defn t [] (set! x 2) x) ; 同一 ns なら OK

(ns b)
a::x ; => エラー（private）
(set! a::x 3) ; => エラー（private）
```

* FQN 参照（`a::x`）でも private は解決できません。
* LSP/補完や `:env` には原則表示されません。

---
<!-- NAV:START -->
**前へ:** [スレッディング / パイプ系（-> / cond-> / some->）](threading.ja.md)
**次へ:** [indexer: `x[0]`, `x[0 1]`, `x[0,1]`, `x[0||d]`, `a..b`](indexer.ja.md)
<!-- NAV:END -->

