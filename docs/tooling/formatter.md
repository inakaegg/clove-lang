# Formatter

Japanese version: [formatter.ja.md](formatter.ja.md)

- Updated: 2026-01-14

Clove has two kinds of formatting:

1. Formatting Clove source itself (`clove fmt`)
2. Formatting inside foreign blocks (Ruby/Python)

## 1. `clove fmt`

```bash
clove fmt path/to/file.clv
```

- Outputs to **stdout**.
- You can read from stdin with `-` or `--stdin`.

```bash
cat file.clv | clove fmt --stdin
```

### Config file (`clovefmt.toml`)

Search order (higher wins):

1. `--config <PATH>` if specified
2. Search upward from the start directory for `clovefmt.toml` then `.clovefmt.toml` (first found)
3. Defaults if none

Start directory:

- For `clove fmt <file>`: parent of that file
- For `--stdin`: `cwd`

Unknown keys are **warned and ignored**. Unsupported `version` is an error.

### Extra options

- `--config <PATH>`: explicit config file
- `--no-config`: skip search/apply
- `--print-config`: print resolved config (defaults + config + CLI) as TOML and exit

### Config example

The same sample is also in [docs/clovefmt.toml](/docs/clovefmt.toml).

```toml
version = 1

[line]
width = 120
indent = 2
trailing_newline = true
line_ending = "auto"
preserve_blank_lines = false

[inline]
depth_limit = 3
map_max_entries = 3
map_max_entries_relaxed = 4
vector_max_items_relaxed = 6
set_max_items_relaxed = 6
allow_nested_collections = true
max_width = 80
width_ratio = 0.0
map_inline_max_width = 0
map_inline_width_ratio = 0.0
flow_head_policy = "inline_if_fit"
flow_head_max_width = 80
flow_head_width_ratio = 0.0

[align]
let_bindings = true
maps = true
cond = true
match = true
inline_budget_multiplier = 2

[shorthand]
map = true
map_exclude_keys = ["keys", "as"]

[comments]
preserve_commas = true
preserve_trailing = true
dangling_policy = "own_line"
spacing = "single"
```

### Config load order

1. defaults (current behavior)
2. config file (if found)
3. CLI flags (`--indent/--width`) override with highest priority

### Config keys (v1)

- phase is a compatibility marker; currently **all keys are active**.

#### Top-level

| key       | type | default | phase | description |
| --------- | ---: | ------: | :---: | ----------- |
| `version` |  int |     `1` |  P1   | config format version; error if unsupported |

#### `[line]`

| key                     | type   | default | phase | description |
| ----------------------- | :----: | ------: | :---: | ----------- |
| `line.width`            |  int   |   `120` |  P1   | max line width (like max_inline_chars) |
| `line.indent`           |  int   |     `2` |  P1   | indent width |
| `line.trailing_newline` |  bool  |  `true` |  P1   | always add trailing newline |
| `line.line_ending`      | string | `auto`  |  P1   | line ending policy (auto keeps input) |
| `line.preserve_blank_lines` | bool | `false` | P1 | keep at most one blank line (even non-top-level) |

#### `[inline]`

| key                               | type | default | phase | description |
| --------------------------------- | ---: | ------: | :---: | ----------- |
| `inline.depth_limit`              |  int |     `3` |  P1   | recursion depth limit for inline decision |
| `inline.map_max_entries`          |  int |     `3` |  P1   | max entries for inline map |
| `inline.map_max_entries_relaxed`  |  int |     `4` |  P1   | relaxed threshold (e.g., function args) |
| `inline.vector_max_items`         |  int |     `4` |  P1   | max items for inline vector |
| `inline.vector_max_items_relaxed` |  int |     `6` |  P1   | relaxed threshold for vector |
| `inline.set_max_items`            |  int |     `0` |  P1   | max items for inline set (0 = unlimited) |
| `inline.set_max_items_relaxed`    |  int |     `6` |  P1   | relaxed threshold for set |
| `inline.allow_nested_collections` | bool |  `true` |  P1   | allow inline even with nested collections |
| `inline.map_allow_complex_values` | bool | `false` |  P1   | allow inline even if map values are complex |
| `inline.max_width`                |  int |    `80` |  P1   | max width for inline (0 = disabled; smaller = harder to inline) |
| `inline.width_ratio`              | float |   `0.0` |  P1   | ratio to line.width (0 = disabled; if both set, smaller wins) |
| `inline.map_inline_max_width`     |  int |     `0` |  P1   | map-specific max width (0 uses common setting) |
| `inline.map_inline_width_ratio`   | float |   `0.0` |  P1   | map-specific ratio (0 uses common setting) |
| `inline.flow_head_policy`         | string | `inline_if_fit` | P1 | output policy for and/or/-> heads (multiline / inline_if_fit) |
| `inline.flow_head_max_width`      |  int |    `80` |  P1   | max width for flow head inline |
| `inline.flow_head_width_ratio`    | float |   `0.0` |  P1   | ratio for flow head (0 = disabled; smaller wins) |

##### What is “relaxed”?

`relaxed` loosens inline decisions slightly. It is used only in places where one-line is still readable,
like function arguments or binding values. It uses `inline.*_relaxed` limits with the shared `inline.depth_limit`.

#### `[align]`

| key                              | type | default | phase | description |
| -------------------------------- | ---: | ------: | :---: | ----------- |
| `align.let_bindings`             | bool |  `true` |  P1   | align `let` bindings vertically |
| `align.maps`                     | bool |  `true` |  P1   | align map keys vertically |
| `align.cond`                     | bool |  `true` |  P1   | align `cond` clauses |
| `align.match`                    | bool |  `true` |  P1   | align `match` clauses |
| `align.inline_budget_multiplier` |  int |     `2` |  P1   | budget multiplier to keep alignment while inlining |

#### `[shorthand]`

| key                          | type     | default              | phase | description |
| ---------------------------- | -------: | -------------------: | :---: | ----------- |
| `shorthand.map`              |    bool  |               `true` |  P1   | allow shorthand output `{:a a}` -> `{:a,}` |
| `shorthand.map_exclude_keys` | string[] | `['keys', 'as']` |  P1   | keys that must not be shortened (safety) |

#### `[comments]`

| key                          | type   | default     | phase | description |
| ---------------------------- | :----: | ----------: | :---: | ----------- |
| `comments.preserve_commas`   |  bool  |     `true`  |  P1   | preserve commas as input |
| `comments.preserve_trailing` |  bool  |     `true`  |  P1   | keep trailing comments on the same line if possible |
| `comments.dangling_policy`   | string | `own_line`  |  P1   | policy for dangling comments |
| `comments.spacing`           | string | `single`    |  P1   | spacing before comments (single or keep) |

### Force one line with empty comment

If you place a comment that is **just semicolons** (`;` / `;;` etc.) at line end,
that form is forced to stay on one line. If it already contains other comments, it is ignored.

**Input**

```clojure
(def aaaaaaaa [[1 2] [3 4] [5 6] [7 8]]) ;
```

**Output (always one line regardless of config)**

```clojure
(def aaaaaaaa [[1 2] [3 4] [5 6] [7 8]]) ;
```

### Mini examples

Below are examples that show “where differences appear”.

#### `line.width`

**Input**

```clojure
(defn f [] (do-something-very-long-name arg1 arg2 arg3 arg4 arg5 arg6))
```

**width=120 (default) -> keep one line (example)**

```clojure
(defn f [] (do-something-very-long-name arg1 arg2 arg3 arg4 arg5 arg6))
```

**width=60 -> split**

```clojure
(defn f []
  (do-something-very-long-name
    arg1 arg2 arg3 arg4 arg5 arg6))
```

#### `line.indent`

**Input**

```clojure
(let [a 1
      b 2]
  (+ a b))
```

**indent=2 (default)**

```clojure
(let [a 1
      b 2]
  (+ a b))
```

**indent=4**

```clojure
(let [a 1
      b 2]
    (+ a b))
```

#### `line.trailing_newline`

**Input**

```clojure
(defn f [] 1)
```

**trailing_newline=true (default)**

```clojure
(defn f [] 1)

```

**trailing_newline=false**

```clojure
(defn f [] 1)
```

#### `line.preserve_blank_lines`

**Input**

```clojure
(def a 1)


(def b 2)
```

**preserve_blank_lines=false (default)**

```clojure
(def a 1)

(def b 2)
```

**preserve_blank_lines=true**

```clojure
(def a 1)

(def b 2)
```

#### `line.line_ending`

**Input**

```clojure
(def a 1)
(def b 2)
```

**line_ending=auto (default)**

```clojure
(def a 1)
(def b 2)
```

**line_ending=lf**

```clojure
(def a 1)
(def b 2)
```

#### `inline.map_max_entries`

**Input**

```clojure
(def m {:a 1 :b 2 :c 3})
```

**map_max_entries=3 (default)**

```clojure
(def m {:a 1 :b 2 :c 3})
```

**map_max_entries=2**

```clojure
(def m
  {:a 1
   :b 2
   :c 3})
```

#### `inline.map_max_entries_relaxed`

**Input**

```clojure
(f {:a 1 :b 2 :c 3})
```

**map_max_entries_relaxed=4 (default)**

```clojure
(f {:a 1 :b 2 :c 3})
```

**map_max_entries_relaxed=2**

```clojure
(f
  {:a 1
   :b 2
   :c 3})
```

#### `inline.map_allow_complex_values`

**Input**

```clojure
(def m {:a (f x) :b (g y)})
```

**map_allow_complex_values=false (default)**

```clojure
(def m
  {:a (f x)
   :b (g y)})
```

**map_allow_complex_values=true**

```clojure
(def m {:a (f x) :b (g y)})
```

#### `inline.vector_max_items`

**Input**

```clojure
(def v [1 2 3 4])
```

**vector_max_items=4 (default)**

```clojure
(def v [1 2 3 4])
```

**vector_max_items=3**

```clojure
(def v
  [1 2 3 4])
```

#### `inline.vector_max_items_relaxed`

**Input**

```clojure
(f [1 2 3 4])
```

**vector_max_items_relaxed=6 (default)**

```clojure
(f [1 2 3 4])
```

**vector_max_items_relaxed=3**

```clojure
(f
  [1 2 3 4])
```

#### `inline.set_max_items`

**Input**

```clojure
(def s #{1 2 3 4})
```

**set_max_items=0 (default, unlimited)**

```clojure
(def s #{1 2 3 4})
```

**set_max_items=3**

```clojure
(def s
  #{1 2 3 4})
```

#### `inline.set_max_items_relaxed`

**Input**

```clojure
(f #{1 2 3 4})
```

**set_max_items_relaxed=6 (default)**

```clojure
(f #{1 2 3 4})
```

**set_max_items_relaxed=3**

```clojure
(f
  #{1 2 3 4})
```

#### `inline.allow_nested_collections`

**Input**

```clojure
(def v [[1 2] [3 4] [5 6]])
```

**allow_nested_collections=true (default)**

```clojure
(def v [[1 2] [3 4] [5 6]])
```

**allow_nested_collections=false**

```clojure
(def v
  [[1 2]
   [3 4]
   [5 6]])
```

#### `inline.max_width` / `inline.width_ratio`

**Input**

```clojure
(def v ["aaaaaaaa" "bbbbbbbb" "cccccccc"])
```

**max_width=80 (default)**

```clojure
(def v ["aaaaaaaa" "bbbbbbbb" "cccccccc"])
```

**max_width=10**

```clojure
(def v
  ["aaaaaaaa" "bbbbbbbb" "cccccccc"])
```

#### `inline.map_inline_max_width` / `inline.map_inline_width_ratio`

**Input**

```clojure
(def m {:a "aaaaaaaa" :b "bbbbbbbb" :c "cccccccc"})
```

**map_inline_max_width=0 (default, use common)**

```clojure
(def m {:a "aaaaaaaa" :b "bbbbbbbb" :c "cccccccc"})
```

**map_inline_max_width=10**

```clojure
(def m
  {:a "aaaaaaaa"
   :b "bbbbbbbb"
   :c "cccccccc"})
```

#### `inline.flow_head_policy`

**Input**

```clojure
(-> x
    (f 1)
    (g 2))
```

**flow_head_policy=inline_if_fit (default)**

```clojure
(-> x
    (f 1)
    (g 2))
```

**flow_head_policy=multiline**

```clojure
(->
  x
  (f 1)
  (g 2))
```

#### `inline.depth_limit`

**Input**

```clojure
(def m {:a {:b {:c {:d 1}}}})
```

**depth_limit=3 (default)**

```clojure
(def m
  {:a {:b {:c {:d 1}}}})
```

**depth_limit=1**

```clojure
(def m
  {:a
   {:b
    {:c
     {:d 1}}}})
```

#### `align.let_bindings`

**Input**

```clojure
(let [a    1
      bb   2
      ccc  3]
  (+ a bb ccc))
```

**let_bindings=true (default)**

```clojure
(let [a    1
      bb   2
      ccc  3]
  (+ a bb ccc))
```

**let_bindings=false**

```clojure
(let [a 1
      bb 2
      ccc 3]
  (+ a bb ccc))
```

#### `align.maps`

**Input**

```clojure
{:aa 1
 :b  2
 :ccc 3}
```

**maps=true (default)**

```clojure
{:aa  1
 :b   2
 :ccc 3}
```

**maps=false**

```clojure
{:aa 1
 :b  2
 :ccc 3}
```

#### `align.cond`

**Input**

```clojure
(cond
  a? (f 1)
  bb? (g 2)
  ccc? (h 3))
```

**cond=true (default)**

```clojure
(cond
  a?   (f 1)
  bb?  (g 2)
  ccc? (h 3))
```

**cond=false**

```clojure
(cond
  a? (f 1)
  bb? (g 2)
  ccc? (h 3))
```

#### `align.match`

**Input**

```clojure
(match x
  Foo    1
  BarBaz 2
  Qux    3)
```

**match=true (default)**

```clojure
(match x
  Foo    1
  BarBaz 2
  Qux    3)
```

**match=false**

```clojure
(match x
  Foo 1
  BarBaz 2
  Qux 3)
```

#### `align.inline_budget_multiplier`

**Input**

```clojure
(let [a       1
      bb      2
      ccc     3
      ddddddd 4]
  (+ a bb ccc ddddddd))
```

**inline_budget_multiplier=2 (default)**

```clojure
(let [a       1
      bb      2
      ccc     3
      ddddddd 4]
  (+ a bb ccc ddddddd))
```

**inline_budget_multiplier=1**

```clojure
(let [a 1
      bb 2
      ccc 3
      ddddddd 4]
  (+ a bb ccc ddddddd))
```

#### `shorthand.map`

**Input**

```clojure
{:a a :b b}
```

**shorthand.map=true (default)**

```clojure
{:a, :b,}
```

**shorthand.map=false**

```clojure
{:a a :b b}
```

#### `shorthand.map_exclude_keys`

**Input**

```clojure
{:keys keys :as as}
```

**map_exclude_keys=["keys", "as"] (default)**

```clojure
{:keys keys :as as}
```

**map_exclude_keys=[]**

```clojure
{:keys, :as,}
```

#### `comments.preserve_commas`

**Input**

```clojure
{:a 1, :b 2, :c 3}
```

**preserve_commas=true (default)**

```clojure
{:a 1, :b 2, :c 3}
```

**preserve_commas=false**

```clojure
{:a 1 :b 2 :c 3}
```

#### `comments.preserve_trailing`

**Input**

```clojure
{:a 1 ; comment
 :b 2}
```

**preserve_trailing=true (default)**

```clojure
{:a 1 ; comment
 :b 2}
```

**preserve_trailing=false**

```clojure
{:a 1
 ; comment
 :b 2}
```

#### `comments.dangling_policy`

**Input**

```clojure
{:a 1
 ; comment
 :b 2}
```

**dangling_policy=own_line (default)**

```clojure
{:a 1
 ; comment
 :b 2}
```

**dangling_policy=attach_prev**

```clojure
{:a 1 ; comment
 :b 2}
```

#### `comments.spacing`

**Input**

```clojure
(+ 1 2);comment
```

**spacing=single (default)**

```clojure
(+ 1 2) ; comment
```

**spacing=preserve**

```clojure
(+ 1 2);comment
```

## 2. Formatting foreign blocks

`clove fmt` can choose a foreign formatter via `--lang`.

- `--lang=clove` ... format as Clove
- `--lang=ruby` ... format as Ruby (may use rubocop / syntax_tree)

> Ruby formatting may require the build feature (`ruby`).

## 3. Policy

- Prefer output that can be “read and run as-is”.
- Keep reader sugar (range, indexer, dot-chain) **as sugar** in output.

---
<!-- NAV:START -->
**Previous:** [Package management (Phase1)](../packages.md)
**Next:** [Build (--opt=typed / --static / embed)](build.md)
<!-- NAV:END -->

