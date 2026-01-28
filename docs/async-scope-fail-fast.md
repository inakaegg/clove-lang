# async-scope fail-fast cancellation policy

Japanese version: [async-scope-fail-fast.ja.md](async-scope-fail-fast.ja.md)

Clove's async-scope defaults to “fail-fast: if one dies, stop everyone”.
When a child task/body returns Err, the scope is cancelled and the cancel-chan is closed
so sibling tasks are notified. `await` still waits for all tasks and aggregates errors as before.

## Default behavior

- If child/future/task/go-loop returns Err, cancel the scope (propagate parent -> child).
- When body finishes, it also cancels (same as before).
- `await` waits in body -> children order, and error priority is unchanged:
  - If body Err exists, wait for children and then return body Err.
  - If body Ok and any child Err exists, return the first child Err.
  - Otherwise return body Ok.

## Handling children you do not want to stop

For children that should not fail-fast, catch exceptions inside and turn them into events.

```clojure
(future
  (fn []
    (try
      (metrics-loop)
      (catch RuntimeError e
        ;; report without killing the scope
        (chan-put! events [:metrics-error (str e)]))))
```

## Retry

Retry is implemented on the child side. Monitor cancel-chan so it reacts to cancellation.

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

## Relation to existing APIs

- Fail-fast is the default for async-scope. No extra option is added.
- “Do not stop / degrade mode” is handled by catching inside the child.
- Scope cancel state/chan can be checked via `async::cancelled?` / `async::cancel-chan`
  (aliases: `async::scope-cancelled?` / `async::scope-cancel-chan`).
