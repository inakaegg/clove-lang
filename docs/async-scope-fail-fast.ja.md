# async-scope の fail-fast キャンセル方針

Clove の async-scope は「誰かが死んだら全員止める」fail-fast をデフォルトにする。子タスク／body が Err を返した瞬間にスコープを cancel し、兄弟タスクにも cancel-chan close で伝播する。await は従来どおり全員を待ってエラーを集約する。

## デフォルトの挙動
- child,future,task,go-loop が Err になった時点で scope を cancel（親→子へ伝播）。
- body が終わった時点でも自動 cancel（従来通り）。
- await は body -> children の順で待ち、エラー優先順位は既存仕様のまま:
  - body Err があれば child を待ち切ってから body Err を返す。
  - body Ok で child Err があれば最初の child Err を返す。
  - それ以外は body Ok を返す。

## 「止めたくない子」を扱う
止めたくない（fail-fastを避けたい）子タスクは自分の中で例外を握ってイベント化する。

```clojure
(future
  (fn []
    (try
      (metrics-loop)
      (catch RuntimeError e
        ;; スコープを殺さずに報告だけする
        (chan-put! events [:metrics-error (str e)]))))
```

## retry について
retry は子タスク側で実装する。キャンセルに素直に反応できるよう cancel-chan を監視する。

```clojure
(defn with-retry [f max]
  (loop [n 0]
    (let [r (try {:ok (f)} (catch RuntimeError e {:err e}))]
      (cond
        (r :ok) (r :ok)
        (>= n max) (throw (r :err))
        :else
        (let [[_ ch] (select [(async::scope-cancel-chan) (timeout 200ms)])]
          (when (= ch (async::scope-cancel-chan))
            (throw (runtime-error "cancelled")))
          (recur (inc n)))))))
```

## 既存APIとの関係
- 新設の fail-fast は async-scope のデフォルト。オプションは増やしていない。
- 止めたくない／縮退運転したいケースは「子で握る」方針で対応する。
- scope cancel 状態/chan は `async::cancelled?` / `async::cancel-chan`（エイリアス: `async::scope-cancelled?` / `async::scope-cancel-chan`）で参照できる。
