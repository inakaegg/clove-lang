# Memoization and Persistent Cache (`memo` / `memoize`)

Japanese version: [memoization.ja.md](memoization.ja.md)

Clove provides two ways to cache function results:

* `memoize` ... simple in-memory memoization
* `memo` ... persistent memoization with TTL and disk store

This document describes their behavior and how to choose between them.

---

## 1. `memoize` – in-memory memoization

```clojure
(defn slow-add [a b]
  (println "called slow-add")
  (+ a b))

(def fast-add (memoize slow-add))

(fast-add 1 2) ; => prints "called slow-add"
(fast-add 1 2) ; => returns from cache, no print
```

* Uses the argument tuple (vector) as the cache key
* Cache is lost when the process exits
* No TTL or disk persistence

---

## 2. `memo` – TTL + persistent cache

`memo` extends `memoize` with:

* TTL (expiration)
* disk storage path

### 2.1 Basic usage

```clojure
(defn fib :int [n :int]
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(def fast-fib
  (memo fib {:ttl 1h}))
```

* 1st argument: function to memoize
* 2nd argument: optional map of options

`fast-fib` is a wrapper around `fib` that caches results and returns cached values
for the same arguments.

### 2.2 Options

Currently supported keys:

* `:ttl` ... cache expiration

  * Duration literal (`1h`, `5m`, etc.)
  * number (seconds as `int` / `float`)
  * `nil` for “no expiration”
* `:store` ... disk path for cache (string)

  * when specified, cache is stored on disk
  * `~` expansion and relative path resolution depend on `resolve_store_path`

Example:

```clojure
(def fast-fib
  (memo fib
        {:ttl   1h
         :store ".clove-memo/fib"}))
```

* First call evaluates `fib` and stores on disk
* Later calls return from disk cache if within TTL

---

## 3. TTL units and interpretation

`ttl` values are interpreted as follows:

### 3.1 `Duration` literals

```clojure
{:ttl 10s}
{:ttl 5m}
{:ttl 1h}
```

Internally, Duration is converted to milliseconds and used as TTL.

### 3.2 Numbers (seconds)

```clojure
{:ttl 60}    ; 60 seconds
{:ttl 0.5}   ; 0.5 seconds
```

* `int` / `float` are treated as seconds
* internally converted with `seconds * 1000`

### 3.3 `nil`

```clojure
{:ttl nil}
```

* Treated as “no expiration”
* Implementation stores a timestamp but skips TTL checks

---

## 4. Cache keys

Both `memo` and `memoize` use the **argument vector** as the key.

```clojure
(memo f)   ; for (args...), uses [args...] as the key
```

* All arguments are stored as `Vector<Value>`
* A hash is computed and used as the `HashMap` key
* Values store a `OnceCell` with “actual result + saved_at timestamp”

Therefore:

* Be careful when using custom types whose `=` / `hash` semantics differ
* Non-serializable values (functions / channels / atoms, etc.) are not a good fit for disk store

---

## 5. When to use

### 5.1 Good fits

* High-cost functions with a limited argument pattern

  * e.g., `fib` or DP-style recursion without combinatorial explosion
* Read-only functions that access external APIs / DB

  * with TTL to allow “same result for seconds/minutes”
* Heavy computations you want to reuse across sessions

### 5.2 Poor fits

* Side-effect-heavy functions (logging / external writes)
* Functions with extremely high argument variety

  * can consume a lot of memory/disk
* Real-time critical cases that must always be up-to-date

---

## 6. Patterns

### 6.1 Cache a high-cost pure function

```clojure
(defn heavy-calc [params]
  ;; ... heavy computation ...
  )

(def cached-heavy-calc
  (memo heavy-calc {:ttl 5m}))
```

### 6.2 I/O function with acceptable staleness

```clojure
(defn fetch-config-from-remote []
  ;; fetch config over HTTP
  )

(def cached-config
  (memo fetch-config-from-remote
        {:ttl   60
         :store ".clove-memo/config"}))
```

---

## 7. Implementation notes (for developers)

* Internally:

  * `HashMap<Vector<Value>, Arc<OnceCell<MemoValue>>>`
  * `MemoValue` = `{ value: Value, saved_at_ms: i64 }`
* With disk store enabled:

  * serialize keys to file names/paths
  * load from disk if TTL not expired
* `build_memoized` wraps `Value::Func` into a new callable

See `crates/clove-lang/src/memo_support.rs` for details.

---

## 8. Summary

* `memoize`: simple in-memory memoization
* `memo`: persistent memoization with TTL/disk store
* `:ttl` accepts Duration literals or seconds (`int` / `float`)
* Cache key is `Vector<Value>`; avoid non-serializable values
* Best for heavy pure functions or read-only I/O with reasonable staleness

────────────────────────────────────────────────────────────────────────────────

---
<!-- NAV:START -->
**Previous:** [Namespace design notes](namespaces_design.md)
**Next:** [Runtime settings (use / use-syntax)](runtime_settings.md)
<!-- NAV:END -->

