# Control flow (special forms)

Japanese version: [control_flow.ja.md](control_flow.ja.md)

- Updated: 2026-01-14

Clove evaluates some forms directly as special forms.

## 1. Basics

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

- `if` uses **truthy** semantics (only `nil` and `false` are false).

## 2. Conditional variants

### 2.1 `and` / `or`

```clojure
(and a b c) ; last value if all are truthy
(or  a b c) ; first truthy value
```

- `and` returns `true` with 0 args.
- `or` returns `nil` with 0 args (with args: first truthy, or false).

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

- `when-let` / `if-let` use **truthy** checks.
- `if-some` filters only **nil** (it lets `false` pass).

### 2.4 `cond`

```clojure
(cond
  test1 expr1
  test2 expr2
  :else expr3)
```

- `:else` / `_` is the default.

### 2.5 `condp`

```clojure
(condp = x
  1 :one
  2 :two
  :else :other)
```

`condp` fixes a predicate and expr and evaluates `(pred test expr)` per clause.

With `(:>> f)`, you can transform predicate results.

```clojure
(condp re-find s
  /cat/ :>> (fn [m] [:cat m])
  /dog/ :>> (fn [m] [:dog m])
  :else :none)
```

## 3. Loops / iteration

- `loop`
- `recur`

```clojure
(loop [i 0 acc 0]
  (if (< i 10)
    (recur (+ i 1) (+ acc i))
    acc))
```

- `recur` is allowed **only in tail position**.
- `recur` cannot be used inside `try` `err` / `fin` (only in the main body tail).

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

- The count must be a **non-negative integer**.

### 3.4 `doseq`

```clojure
(doseq [x [1 2]
        y [10 20]]
  (println x y))
```

- `doseq` is **for side effects** (returns `nil`).
- Pattern bindings are supported (same as `let`).

### 3.5 `for`

`for` is a **list comprehension that returns a vector**.

```clojure
(for [x [1 2 3]
      y [10 20]
      :when (odd? x)
      :let [z (+ x y)]]
  z)
; => [11 21 13 23]
```

- `:let` / `:when` / `:while` are supported.
- When `:while` becomes false, it **terminates that series**.
- In native codegen, `:while` condition can reference **only the same bindings** (outer bindings not supported yet).

### 3.6 `each`

```clojure
(each [x [1 2 3]] (println x)) ; => [1 2 3]
(each println [1 2 3])         ; => [1 2 3]
```

- `each` is for side effects and **returns the input collection**.
- `(each f coll)` calls `f` on each element.

## 4. Scope / resources / side-effect helpers

### 4.1 `with-redefs`

`with-redefs` **temporarily overrides vars** (mostly for tests).

```clojure
(defn greet [] "hi")

(with-redefs [greet (fn [] "yo")]
  (greet)) ; => "yo"
```

### 4.2 `with-open`

`with-open` **automatically closes resources** (reverse order).

```clojure
(with-open [f (io::open "a.txt")]
  (read f))
```

- Use `:close!` / `:close` metadata if present
- Otherwise look for `close!` / `close` functions
- For channels, close via `chan-close!`

### 4.3 `with-dyn`

`with-dyn` **temporarily binds dynamic vars**.

```clojure
(with-dyn [*out* (io::writer "log.txt")]
  (println "hello"))
```

- Currently dynamic vars are `*out*` / `*err*`.

### 4.4 `doto`

`doto` **applies side effects to the same target** and returns the target.

```clojure
(doto (StringBuilder.)
  (.append "a")
  (.append "b"))
```

## 5. Exceptions

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

### try explicit clauses (err / fin)

`err` / `fin` can appear only at the end of `try`, in the fixed order **err -> fin**.

```clojure
(try
  (throw 1)
  (err (do (println "err") ?))
  (fin (println "cleanup")))
```

- `err` runs only on exception and can reference the exception as `?`.
- `fin` runs **once on exit** regardless of success/failure.
- If `err` / `fin` exist, you cannot use the short callable form.

### Trailing err / fin in body (implicit try)

`err` / `fin` can appear at the end of **any body (list of forms)**.
When placed at the end, preceding forms are implicitly wrapped with `try`.

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

;; #(...) is a single form, so wrap with do
#(do
   (throw 3)
   (err :failed)
   (fin (println "cleanup")))

;; top-level (REPL / load-file)
(throw 4)
(err ?)
(fin (println "cleanup"))
```

### try short form

`try` also has a short form using trailing callables. It works only when the last
1–2 forms are callable, in the fixed order **on-error -> on-finally**.

- `(try body+ on-error)` (on-error is 1-arg callable)
- `(try body+ on-finally)` (on-finally is 0-arg callable)
- `(try body+ on-error on-finally)`
- `(try [bindings] expr on-error on-finally)` (bindings short, required)

Example: `(try (throw 1) 42)` is a syntax error. Instead:

```clojure
(try (throw 1) (fn [e] 42)) ; => 42
```

## 6. quote family

- `quote`
- `quasiquote`
- `unquote`
- `unquote-splicing`

See [Reader / syntax overview](reader_syntax.md).

## 7. `comment`

`comment` **does not evaluate** its body and returns `nil`.

```clojure
(comment
  (println "skip")
  1 2 3)
```

## 8. `async-scope`

`async-scope` is a special form for **managing child task scopes**.

See: [Concurrency / async](concurrency.md)

## 9. Debug forms

### 8.1 `p` (debug print)

`p` prints the evaluated result as **`file:line:col: value`** and returns it.

- `(p f a b ...)` evaluates `(f a b ...)`, prints, and returns the result.
- `(p x)` evaluates `x`, prints, and returns it.
  - If `x` is a **Symbol and not a local binding**, it treats it as a 0-arg callable and **invokes** it.

```clojure
(p + 1 2) ; => 3
(p x)     ; => result of x
```

### 8.2 `repl` / `debug` / `break`

```clojure
(repl)         ; open REPL here
(repl value)   ; open REPL with value bound to ? etc.
(debug)        ; alias of repl
(break)        ; simple breakpoint (debug REPL)
```

- `repl` / `debug` / `break` **start a REPL on the spot**.
- Exit with `:q` / `:quit`.
- See [REPL Guide](../tooling/repl.md) for details.

---
<!-- NAV:START -->
**Previous:** [Destructuring](destructuring.md)
**Next:** [Threading / pipeline forms (-> / cond-> / some->)](threading.md)
<!-- NAV:END -->

