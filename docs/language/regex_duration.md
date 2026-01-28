# Regex / Duration literals

Japanese version: [regex_duration.ja.md](regex_duration.ja.md)

- Updated: 2025-12-21

This page describes **reader literals for `regex` and `duration`**.

## 1. Regex

### 1.1 `/.../`

```clojure
(def re /a+b*/)
(re-find re "aaab")
```

### 1.2 `#/.../` (disambiguation)

```clojure
(def re #/\d+/)
(re-find re "id=123")
```

> `/.../` is read as regex only when "the next token is not whitespace/delimiter"
> and there is a closing `/` on the same line. If ambiguous, use `#/.../`.
> `#"..."` is deprecated.
> For OOP chain regex stages, use `.#/.../` (`./` is parsed as `/` method).

### 1.3 Call as a function

Regex literals are **callable**.
`(/.../ text)` is equivalent to `re-matches`: it returns a value **only on full match**,
otherwise `nil`. This makes it usable in higher-order functions.

```clojure
(/\d+/ "123")    ; => "123"
(/\d+/ "a2")     ; => nil

(map /\d+/ ["1" "a2" "333"]) ; => ["1" nil "333"]
```

### 1.4 String interpolation

Regex literals also support `#{...}` interpolation.
To output `#{` literally, escape it as `\#{`.

```clojure
(let [target "cat"]
  (/.*#{target}.*/ "black cat")) ; => "black cat"
```

## 2. Duration

Clove has `Duration` values used for timeouts, etc.

### 2.1 Literals (**integers only**)

```clojure
10ms
2s
-5m
```

- `ns`, `us`, `ms`, `s`, `m`, `h`, `d`, `w`
- underscore separator: `1_000ms`

**Note:** The current reader does not support fractional literals like `0.1s`.
(`read-string` reads `0.1s` as a `Symbol`.)

### 2.2 Create via function (fractional supported)

Use the `duration` function for fractional values.

```clojure
(duration 0.1 :s)
(duration 1500 :ms)
```

### 2.3 Relation to `timeout`

`timeout` accepts "milliseconds int" or "Duration".

```clojure
(timeout 100)     ; 100ms
(timeout 0.5s)    ; <- not supported currently
(timeout (duration 0.5 :s))
```

---
<!-- NAV:START -->
**Previous:** [Map shorthand (JS-style)](map_shorthand.md)
**Next:** [Concurrency / async (chan / future / go-loop / scope-loop / async-scope)](concurrency.md)
<!-- NAV:END -->

