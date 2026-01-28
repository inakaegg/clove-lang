# Concurrency / async

Japanese version: [concurrency.ja.md](concurrency.ja.md)

- Updated: 2026-01-14

Clove includes “lightweight concurrency primitives” useful for games and tools.

- channel (`chan`)
- task (`spawn`)
- future (`future`)
- promise (`promise`)
- agent (`agent`)
- atom (`atom`)
- select (`select` / `scope-select`)
- cancellation (`cancelled?` / `cancel-chan`)
- `async-scope` (special form)

> This page explains concepts and usage. For function details, see `:doc`.

## 1. atom

```clojure
(def a (atom 0))
@a                 ; => 0
(atom-set! a 10)
(swap! a + 1)      ; alias: swap! -> atom-update!
@a                 ; => 11
```

- `atom` can have validators and watches.
- `add-watch` / `remove-watch` also work for atoms.

## 2. channel

### 2.1 Create

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

- `chan-put!` returns `true/false` (`false` after close).
- `chan-take!` returns a value, and `nil` when closed and empty.

### 2.3 close

```clojure
(chan-close! c)
(chan-closed? c) ; => true/false
```

## 3. timeout

`timeout` returns a “notification channel” that becomes ready after Duration or ms(int).

```clojure
(def t (timeout 100))
(<!! t) ; => :timeout, etc.
```

Note: return value follows implementation. See `:doc timeout`.

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

`go-loop` is a special form that **runs a `loop` in another task**.
It returns a `task`.

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

An agent is a “stateful worker”.

```clojure
(def ag (agent 0))
(agent-send! ag (fn [x] (+ x 1)))
(agent-await ag)
@ag
```

## 6. select

Receive from “whichever arrives first” among multiple channels.

```clojure
(select
  [c1 (fn [v] [:c1 v])]
  [c2 (fn [v] [:c2 v])])
```

## 7. cancellation

- `cancelled?` returns whether the **current cancel context** has been cancelled.
- `cancel-chan` returns a channel that becomes ready when cancelled.

These are especially important inside `async-scope`.

## 8. `async-scope`

`async-scope` groups **child tasks + main body**,
and when the main body finishes, it signals cancel to clean up children.

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

### fail-fast variant

- [async-scope-fail-fast](../async-scope-fail-fast.md)

> Tip: write child tasks so they **observe cancellation**.

## 9. `scope-loop` / `async::scope-loop`

`scope-loop` is a loop that **monitors cancellation**.
`async::scope-loop` is an alias.

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

`pmap` / `pfilter` are sugar for **binding vector + body**.
They take `[binding coll]` and internally call `core::pmap` / `core::pfilter`.

```clojure
(pmap [x [1 2 3]] (+ x 10))
(pfilter [x [1 2 3 4]] (odd? x))
```

Option map can be placed **at the head or right after the binding** (only one).

```clojure
(pmap {:workers 4} [x xs] (heavy x))
(pfilter [x xs] {:workers 4} (odd? x))
```

`std::pmap` / `std::pfilter` are aliases of `dag::pmap` / `dag::pfilter`.

---
<!-- NAV:START -->
**Previous:** [Regex / Duration literals](regex_duration.md)
**Next:** [Interop (foreign blocks / Ruby / Python)](interop_foreign.md)
<!-- NAV:END -->

