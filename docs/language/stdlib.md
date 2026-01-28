# Standard library (`std`) overview

Japanese version: [stdlib.ja.md](stdlib.ja.md)

Clove ships a “batteries included” standard library `std` separate from core.
`clove run` / REPL are expected to load `std` along with `clove::core`.

## Prelude

- Major sequence ops (`map`/`filter`/`reduce`/`partition*`, etc.)
- String ops (Clojure `clojure.string` equivalent)
- Set ops (Clojure `clojure.set` equivalent)
- Generic tree walk (Clojure `clojure.walk` equivalent)
- Convenience wrappers: `http-get-json`/`http-post-json`, `sh`/`sh!`, `parse-opts`/`parse-opts-adv`, `log-*`, `deftest`/`is`/`run-tests`, etc.

## Loading and overriding std

- `std` is loaded by default in `clove run` / REPL (default require on the implementation side).
- `user.clv` is config-only and cannot `require`. To replace `std`, explicitly `require` in each `ns`.
- Unqualified names brought by `:refer :*` **override only within that ns** (no effect on other ns).
- If you need builtins, **use `core::` explicitly**.

Example:

```clojure
(ns app::main
  (:require [my::std :as std :refer :*]))

(map inc [1 2])        ; uses my::std map
(std::map inc [1 2])   ; my::std map
(core::map inc [1 2])  ; always core
```

## Sequence API policy (std vs core)

- `std` prefers returning vectors for everyday use. `std::take` / `std::take-while` always return vecs; if you want full laziness, use `core::take` / `core::take-while`.
- `std::drop` / `std::drop-while` return seq to avoid realizing infinite/huge sequences. Use `(vec (drop ...))` when needed.
- `std::map` / `std::filter` return seq if input includes a seq; otherwise return vec. Use `core::map` / `core::filter` to keep lazy pipelines.
- If you want to avoid `<seq>` display in REPL, use `std` or `vec` to realize.
- `:doc` resolves the symbol as-is, so `std::take-while` shows std doc. Query `core::take-while` to see core docs.

## Files / paths / time

- `slurp-bytes` / `spit-bytes`, `file-exists?` / `file?` / `dir?`, `list-dir`, `mkdir` / `mkdirs`, `delete`, `copy`, `move`
- stdin: `read-line` / `read-all`
- resources: `resource-url` / `resource-bytes` / `resource->tempfile`
- path ops: `path::cwd|home-dir|temp-dir|join|basename|dirname|extname|normalize|canonicalize|absolute?|relative?|resolve|source-dir`
- source: `current-file` (`core::current-file`)
- time/Duration: `time::now` (epoch ms), `time::parse` / `time::format`, `time::plus` / `time::minus`, `time::between` (returns Duration), `time::sleep`,
  `duration` / `duration-ms|sec|...` (number -> Duration), `ms|sec|min|...` (Duration -> number)

## Data formats

- JSON: `json::parse` / `generate` / `generate-pretty` / `read-file` / `write-file`
- YAML: `yaml::parse` / `generate` / `read-file` / `write-file`
- TOML: `toml::parse` / `generate` / `read-file` / `write-file`
- INI: `ini::parse` / `generate`
- .env: `env::parse-file`
- environment vars: `env::vars` / `env::get` / `env::has?`

## Strings / regex

- Existing: `split` / `replace` / `replace-first` / `re-find` / `re-matches` / `split-lines` / `blank?` / `includes?` / `starts-with?` / `ends-with?` / `trim*` / `upper-case` / `lower-case`
- Added: `reverse` / `capitalize` / `trim-newline` / `escape` (replace map) / `index-of` / `last-index-of`
- Regex utilities: `re-pattern` (string -> regex), `re-seq` (return all matches as vector), `re-matcher` (simple matcher; returns regex)

## `contains?` vs `includes?`

- `contains?` checks **keys** for map/set and **indexes** for vector/list.
- `includes?` checks **values** (value search in vector/list/seq, key for set/map, substring for string).

```clojure
(contains? {:a 1} :a)     ; => true
(contains? [:a :b] 1)     ; => true
(contains? [:a :b] :a)    ; => ERROR (not an index)

(includes? [:a :b] :a)    ; => true
(includes? #{:a :b} :a)   ; => true
(includes? {:a 1} :a)     ; => true
(includes? "clove" "lo")  ; => true
```

## Sets (Clojure `clojure.set` equivalent)

- `union` / `intersection` / `difference`
- `select` (filter by pred), `project` (select keys), `rename-keys` / `rename`
- `index` (index by keys), `join` (natural/key join), `map-invert`
- `subset?` / `superset?`

## HTTP

### Basic

```clojure
(http::request
  {:method "GET"
   :url "https://example.com"
   :headers {"Authorization" "Bearer TOKEN"}
   :query {:page 1}
   :timeout-ms 5000})
;; => {:status 200 :headers {...} :body "...raw..." :json {...}/nil}
```

Shortcuts: `http-get` / `http-post` / `http-put` / `http-delete`.
JSON helpers: `http-get-json` / `http-post-json`.

- `:body` can be a string (sent as-is) or a Clove value (JSON-encoded). `:json` is also supported.
- Return value is a map with `:status` `:headers` `:body` `:json`.

## Shell / process

- `sh` / `sh!` (`sh!` throws on non-zero exit). Options: `{:dir "...", :env {:KEY "VAL"}, :stdin "...", :stream? true}`.
  Returns `{:exit :out :err}`.
- `process::run` (capture output, options `{:cwd "...", :env {...}, :clear-env true, :in "...", :out-enc :bytes, :err-enc :bytes}`)
- `process::which` (search PATH)
- `shell::split` (shell word split), `shellwords` (alias), `shell::escape` (escape args)

## CLI / logs / tests

- CLI: `argv` / `env` / `env-get` / `exit`
- Option parsing: `parse-opts argv spec`
  - `spec` example: `[{:id :port :short "-p" :long "--port" :arg? true :default "8080" :required? true}]`
  - `--key=value` / `-k=value` forms supported. If required but missing, recorded in `:errors`.
  - Returns `{:options {:port "8080"} :args [...] :errors [...]}` (unknown options also go to `:errors`).
- Option parsing (advanced): `parse-opts-adv argv spec`
  - Extra `spec` keys: `:alias` (extra flags), `:coerce` (`:int`/`:float`/`:bool`/`:string`), `:collect?` (multi args -> array)
  - Short-flag bundling like `-abc` (**flags without args only**), `--` ends option parsing
  - Example:
    ```clojure
    (def opts
      (parse-opts-adv (argv)
                      [{:id :port :short "-p" :long "--port" :arg? true :coerce :int}
                       {:id :include :short "-I" :arg? true :collect? true}
                       {:id :verbose :alias ["-v" "--verbose"]}]))
    ```
- Logs: `log-trace|debug|info|warn|error`
- Debug: `dbg` / `spy` print values and return them.
- Pretty print: `pp` / `pp-str` / `pprint` / `pprint-str`
  - `pp` prints to `*out*` and **returns the original value**. `pp-str` returns the formatted string.
  - Searches `clovefmt.toml` or `.clovefmt.toml` **from the current directory** and applies `indent/width` (opts `:indent` / `:width` override).
  - Extra limits: `:max-depth` / `:max-items` / `:max-string` / `:max-bytes` for huge data.
  - Non-readable values like `#atom<...>` / `<seq>` are **stringified** to keep output parseable.
- Tests (minimal): `(deftest foo (fn [] (is (= 1 1))))`, `(testing "context" (fn [] ...))`, `(run-tests)`.
  `is` accepts an optional message.

## Quick examples

```clojure
(ns quick::sample)

;; HTTP + JSON
(let [resp (http-get-json "https://api.example.com/users")]
  (println "users count:" (count resp)))

;; Files and paths
(when-not (file-exists? "tmp")
  (mkdir "tmp"))
(spit-bytes (path-join "tmp" "hello.bin") [0 1 2 3])

;; CLI args
(def opts (parse-opts (argv)
                      [{:id :mode :long "--mode" :arg? true :default "dev"}]))
(println "mode:" (get-in opts [:options :mode]))

;; Shell
(sh "echo" "hello")

;; Tests
(deftest add-test (fn [] (is 3 (+ 1 2))))
(run-tests)
```

---
<!-- NAV:START -->
**Previous:** [Types/enum/match (deftype/defenum/match)](types_enum_match.md)
**Next:** [CLI (clove / clove fmt / clove build)](../tooling/cli.md)
<!-- NAV:END -->

