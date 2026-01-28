# Formatter

- 更新日: 2026-01-14

Clove には 2 種類のフォーマットがあります。

1. Clove ソース自体のフォーマット（`clove fmt`）
2. foreign blocks 内（Ruby/Python）のフォーマット

## 1. `clove fmt`

```bash
clove fmt path/to/file.clv
```

- **stdout に出力**します。
- `-` または `--stdin` で標準入力からも読めます。

```bash
cat file.clv | clove fmt --stdin
```

### 設定ファイル（`clovefmt.toml`）

探索順（上から優先）:

1. `--config <PATH>` が指定されている場合はそのファイル
2. 開始ディレクトリから親へ向かって `clovefmt.toml` → `.clovefmt.toml` を探索（最初に見つかったもの）
3. 無ければ defaults

開始ディレクトリ:

- `clove fmt <file>` の場合はそのファイルの親
- `--stdin` の場合は `cwd`

未知キーは **warning（無視）**、`version` が未対応なら error になります。

### 追加オプション

- `--config <PATH>`: 設定ファイルを明示指定
- `--no-config`: 探索/適用を行わない
- `--print-config`: 解決済み（defaults + config + CLI）の設定を TOML で出力して終了

### 設定例

[docs/clovefmt.toml](/docs/clovefmt.toml) にも同じ内容のサンプルを置いています。

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

### 設定の読み込み順

1. defaults（現状と同じ）
2. config file（見つかったら）
3. CLI flags（`--indent/--width`）があれば最優先で上書き

### 設定キー（v1）

- phase は互換性の目安で、現状は **全キーが有効**です。

#### トップレベル

| key       | type | default | phase | 説明                     |
| --------- | ---: | ------: | :---: | ------------------------ |
| `version` |  int |     `1` |  P1   | 設定フォーマットの版。未対応なら error |

#### `[line]`

| key                     | type   | default | phase | 説明                              |
| ----------------------- | :----: | ------: | :---: | --------------------------------- |
| `line.width`            |  int   |   `120` |  P1   | 1行許容幅（max_inline_chars 相当） |
| `line.indent`           |  int   |     `2` |  P1   | インデント幅（indent_width 相当）  |
| `line.trailing_newline` |  bool  |  `true` |  P1   | ファイル末尾に改行を必ず付ける      |
| `line.line_ending`      | string | `auto`  |  P1   | 改行コード方針（auto は入力踏襲）  |
| `line.preserve_blank_lines` | bool | `false` | P1 | 空行を 1 行だけ保持（トップレベル以外も） |

#### `[inline]`

| key                               | type | default | phase | 説明                                 |
| --------------------------------- | ---: | ------: | :---: | ------------------------------------ |
| `inline.depth_limit`              |  int |     `3` |  P1   | インライン判定の再帰深さ上限         |
| `inline.map_max_entries`          |  int |     `3` |  P1   | map を 1行にする最大エントリ数       |
| `inline.map_max_entries_relaxed`  |  int |     `4` |  P1   | “relaxed 判定”用（例: 関数引数など） |
| `inline.vector_max_items`         |  int |     `4` |  P1   | vector を 1行にする最大要素数        |
| `inline.vector_max_items_relaxed` |  int |     `6` |  P1   | vector を relaxed inline できる最大要素数 |
| `inline.set_max_items`            |  int |     `0` |  P1   | set を 1行にする最大要素数（0 は無制限） |
| `inline.set_max_items_relaxed`    |  int |     `6` |  P1   | set の relaxed inline 上限（必要なら） |
| `inline.allow_nested_collections` | bool |  `true` |  P1   | ネストコレクション混在でも inline を許可するか |
| `inline.map_allow_complex_values` | bool | `false` |  P1   | map の値が複雑でも inline 判定を許可するか |
| `inline.max_width`                |  int |    `80` |  P1   | inline の最大幅（0 は無効、短いほど 1行にしにくい） |
| `inline.width_ratio`              | float |   `0.0` |  P1   | line.width に対する比率（0 は無効、両方指定なら小さい方） |
| `inline.map_inline_max_width`     |  int |     `0` |  P1   | map 専用の上限幅（0 は共通設定に従う） |
| `inline.map_inline_width_ratio`   | float |   `0.0` |  P1   | map 専用の比率（0 は共通設定に従う） |
| `inline.flow_head_policy`         | string | `inline_if_fit` | P1 | and/or/-> 系の出力方針（multiline / inline_if_fit） |
| `inline.flow_head_max_width`      |  int |    `80` |  P1   | flow head を 1行にする最大幅（短いほど 1行にしにくい） |
| `inline.flow_head_width_ratio`    | float |   `0.0` |  P1   | line.width に対する比率（0 は無効、両方指定なら小さい方） |

##### relaxed とは

`relaxed` は inline 判定を少し緩めるモードです。関数引数や束縛値など、1行でも読みやすい場所に限って使われます。  
通常の `inline.*` より大きい上限を `inline.*_relaxed` で指定し、`inline.depth_limit` は共通で効きます。

#### `[align]`

| key                              | type | default | phase | 説明                                      |
| -------------------------------- | ---: | ------: | :---: | ----------------------------------------- |
| `align.let_bindings`             | bool |  `true` |  P1   | `let` の束縛列を縦揃えするか               |
| `align.maps`                     | bool |  `true` |  P1   | map を縦揃え（キーの縦整列）するか         |
| `align.cond`                     | bool |  `true` |  P1   | cond の節を揃えるか                        |
| `align.match`                    | bool |  `true` |  P1   | match の節を揃えるか                       |
| `align.inline_budget_multiplier` |  int |     `2` |  P1   | 揃えを維持したまま 1行化の予算倍率          |

#### `[shorthand]`

| key                          | type     | default              | phase | 説明                                 |
| ---------------------------- | -------: | -------------------: | :---: | ------------------------------------ |
| `shorthand.map`              |    bool  |               `true` |  P1   | `{:a a}` → `{:a,}` の省略出力を許可    |
| `shorthand.map_exclude_keys` | string[] | `["keys", "as"]` |  P1   | 省略出力を禁止するキー（安全策）       |

#### `[comments]`

| key                          | type   | default     | phase | 説明                         |
| ---------------------------- | :----: | ----------: | :---: | ---------------------------- |
| `comments.preserve_commas`   |  bool  |     `true`  |  P1   | コンマを入力通りに保持するか        |
| `comments.preserve_trailing` |  bool  |     `true`  |  P1   | 行末コメントを可能なら同じ行に残す |
| `comments.dangling_policy`   | string | `own_line`  |  P1   | 行間コメントの吸着方針            |
| `comments.spacing`           | string | `single`    |  P1   | コメント前の空白（統一 or そのまま） |

### 空コメントで 1 行固定

行末に **セミコロンだけ** のコメント（`;` / `;;` など）を置くと、そのフォームを例外的に 1 行化します。  
内部に別コメントがある場合は無効になります。

**入力**

```clojure
(def aaaaaaaa [[1 2] [3 4] [5 6] [7 8]]) ;
```

**出力（設定に関わらず 1 行維持）**

```clojure
(def aaaaaaaa [[1 2] [3 4] [5 6] [7 8]]) ;
```

### ミニ例

以下は“差分が出るポイント”を示すための例です。

#### `line.width`

**入力**

```clojure
(defn f [] (do-something-very-long-name arg1 arg2 arg3 arg4 arg5 arg6))
```

**width=120（default）→ 1行維持（例）**

```clojure
(defn f [] (do-something-very-long-name arg1 arg2 arg3 arg4 arg5 arg6))
```

**width=60 → 折り返し（例）**

```clojure
(defn f []
  (do-something-very-long-name
    arg1 arg2 arg3 arg4 arg5 arg6))
```

#### `line.indent`

**入力**

```clojure
(defn f []
  (if cond
    (do-a)
    (do-b)))
```

**indent=2（default）**

```clojure
(defn f []
  (if cond
    (do-a)
    (do-b)))
```

**indent=4**

```clojure
(defn f []
    (if cond
        (do-a)
        (do-b)))
```

#### `line.trailing_newline`

**入力**

```clojure
(println 1)
```

**trailing_newline=true（default）→ 末尾改行あり（例）**

```clojure
(println 1)
```

※表示上は同じですが、末尾に改行が付きます。

**trailing_newline=false → 末尾改行なし（例）**

```clojure
(println 1)
```

※末尾に改行を付けません。

#### `line.preserve_blank_lines`

**入力**

```clojure
(defn foo []
  (a)

  (b))
```

**preserve_blank_lines=false（default）→ 空行を詰める（例）**

```clojure
(defn foo []
  (a)
  (b))
```

**preserve_blank_lines=true → 1行空行を保持（例）**

```clojure
(defn foo []
  (a)

  (b))
```

#### `line.line_ending`

**入力**

```clojure
(println 1)
(println 2)
```

※以下は表示用に改行を `\n` / `\r\n` で可視化しています。

**line_ending=lf（例）**

```text
(println 1)\n(println 2)\n
```

**line_ending=crlf → 改行が CRLF（例）**

```text
(println 1)\r\n(println 2)\r\n
```

#### `inline.map_max_entries`

**入力**

```clojure
{:a 1 :b 2 :c 3 :d 4}
```

**map_max_entries=3（default）→ 複数行（例）**

```clojure
{:a 1
 :b 2
 :c 3
 :d 4}
```

**map_max_entries=4 → 1行（例）**

```clojure
{:a 1 :b 2 :c 3 :d 4}
```

#### `inline.map_max_entries_relaxed`

**入力**

```clojure
(let [cfg {:a 1 :b 2 :c 3 :d 4}]
  cfg)
```

**map_max_entries_relaxed=4（default）→ 1行維持（例）**

```clojure
(let [cfg {:a 1 :b 2 :c 3 :d 4}]
  cfg)
```

**map_max_entries_relaxed=2 → map を折る（例）**

```clojure
(let [cfg {:a 1
           :b 2
           :c 3
           :d 4}]
  cfg)
```

#### `inline.map_allow_complex_values`

**入力**

```clojure
{:a [1 2 3] :b (inc 1)}
```

**map_allow_complex_values=false（default）→ 複数行（例）**

```clojure
{:a [1 2 3]
 :b (inc 1)}
```

**map_allow_complex_values=true → 1行（例）**

```clojure
{:a [1 2 3] :b (inc 1)}
```

#### `inline.vector_max_items`

**入力**

```clojure
[1 2 3 4 5]
```

**vector_max_items=4（default）→ 複数行（例）**

```clojure
[1
 2
 3
 4
 5]
```

**vector_max_items=5 → 1行（例）**

```clojure
[1 2 3 4 5]
```

#### `inline.vector_max_items_relaxed`

**入力**

```clojure
[1 2 3 4 5 6 7]
```

**vector_max_items_relaxed=6（default）→ 複数行（例）**

```clojure
[1 2 3 4 5 6
 7]
```

**vector_max_items_relaxed=8 → 1行（例）**

```clojure
[1 2 3 4 5 6 7]
```

#### `inline.set_max_items`

**入力**

```clojure
#{1 2 3 4 5}
```

**set_max_items=0（default, 無制限）→ 幅次第で 1行（例）**

```clojure
#{1 2 3 4 5}
```

**set_max_items=4 → 複数行（例）**

```clojure
#{1
 2
 3
 4
 5}
```

#### `inline.set_max_items_relaxed`

**入力**

```clojure
(let [xs #{1 2 3 4 5 6 7}]
  xs)
```

**set_max_items_relaxed=6（default）→ 複数行（例）**

```clojure
(let [xs #{1
           2
           3
           4
           5
           6
           7}]
  xs)
```

**set_max_items_relaxed=8 → 1行（例）**

```clojure
(let [xs #{1 2 3 4 5 6 7}]
  xs)
```

#### `inline.allow_nested_collections`

**入力**

```clojure
(let [xs [1 {:a 1} 3 4]]
  xs)
```

**allow_nested_collections=true（default）→ ネスト込みでも 1行（例）**

```clojure
(let [xs [1 {:a 1} 3 4]]
  xs)
```

**allow_nested_collections=false → ネストがあると複数行（例）**

```clojure
(let [xs [1
          {:a 1}
          3
          4]]
  xs)
```

#### `inline.max_width` / `inline.width_ratio`

**入力**

```clojure
(foo long-symbol-name another-long-symbol-name)
```

**max_width=80（default）→ 1行（例）**

```clojure
(foo long-symbol-name another-long-symbol-name)
```

**max_width=30 → 複数行（例）**

```clojure
(foo
  long-symbol-name
  another-long-symbol-name)
```

※ `width_ratio` を使う場合は `line.width` との比率で上限を決めます。

#### `inline.map_inline_max_width` / `inline.map_inline_width_ratio`

**入力**

```clojure
{:variables initialGameVariables :objects [initialVicViper]}
```

**map_inline_max_width=0（default）→ 共通の上限に従う（例）**

```clojure
{:variables initialGameVariables :objects [initialVicViper]}
```

**map_inline_max_width=50 → map だけ複数行（例）**

```clojure
{:variables initialGameVariables
 :objects   [initialVicViper]}
```

#### `inline.flow_head_policy`

**入力**

```clojure
(or (Restart? action) (Flap? action))
```

**flow_head_policy=inline_if_fit（default、flow_head_max_width=80 の場合）→ 1行（例）**

```clojure
(or (Restart? action) (Flap? action))
```

**flow_head_policy=multiline → 複数行（例）**

```clojure
(or
  (Restart? action)
  (Flap? action))
```

※ 1行判定は `flow_head_*` と `inline.*` の上限の **小さい方**で行われます。

#### `inline.depth_limit`

**入力**

```clojure
(foo (bar (baz (qux 1 2 3))))
```

**depth_limit=3（default）→ 深いネストで折る（例）**

```clojure
(foo
  (bar
    (baz (qux 1 2 3))))
```

**depth_limit=6 → 1行維持しやすい（例）**

```clojure
(foo (bar (baz (qux 1 2 3))))
```

#### `align.let_bindings`

**入力**

```clojure
(let [a 1
      long-name 2
      z 3]
  (+ a long-name z))
```

**let_bindings=true（default）→ 縦揃え（例）**

```clojure
(let [a         1
      long-name 2
      z         3]
  (+ a long-name z))
```

**let_bindings=false → 揃えない（例）**

```clojure
(let [a 1
      long-name 2
      z 3]
  (+ a long-name z))
```

#### `align.maps`

**入力**

```clojure
{:a 1
 :longer-key 2}
```

**maps=true（default）→ 値の位置を揃える（例）**

```clojure
{:a          1
 :longer-key 2}
```

**maps=false → 揃えない（例）**

```clojure
{:a 1
 :longer-key 2}
```

#### `align.cond`

**入力**

```clojure
(cond
  short 1
  very-long-condition 2
  :else 3)
```

**cond=true（default）→ 値の位置を揃える（例）**

```clojure
(cond
  short               1
  very-long-condition 2
  :else               3)
```

**cond=false → 揃えない（例）**

```clojure
(cond
  short 1
  very-long-condition 2
  :else 3)
```

#### `align.match`

**入力**

```clojure
(match x
  1 :one
  100 :many
  _ :other)
```

**match=true（default）→ 値の位置を揃える（例）**

```clojure
(match x
  1   :one
  100 :many
  _   :other)
```

**match=false → 揃えない（例）**

```clojure
(match x
  1 :one
  100 :many
  _ :other)
```

#### `align.inline_budget_multiplier`

**入力**

```clojure
(let [config (very-long-function-name arg1 arg2 arg3)]
  config)
```

**inline_budget_multiplier=1（line.width=30 の場合）→ 値を折る（例）**

```clojure
(let [config
      (very-long-function-name arg1 arg2 arg3)]
  config)
```

**inline_budget_multiplier=2（line.width=30 の場合）→ inline を維持（例）**

```clojure
(let [config (very-long-function-name arg1 arg2 arg3)]
  config)
```

#### `shorthand.map`

**入力**

```clojure
(let [a 1 b 2] {:a a :b b :c (+ a b)})
```

**shorthand.map=true（default）→ 同名は省略（例）**

```clojure
(let [a 1 b 2] {:a, :b, :c (+ a b)})
```

**shorthand.map=false → 常に通常表記（例）**

```clojure
(let [a 1 b 2] {:a a :b b :c (+ a b)})
```

#### `shorthand.map_exclude_keys`

**入力**

```clojure
{:keys keys :as as}
```

**exclude=["keys","as"]（default）→ 省略しない（例）**

```clojure
{:keys keys :as as}
```

**exclude=[] → 省略し得る（例）**

```clojure
{:keys, :as,}
```

#### `comments.preserve_commas`

**入力**

```clojure
(GameOver :bird, :pipes, :score)
[r: Rect, color: Color]
```

**preserve_commas=true（default）→ カンマを保持（例）**

```clojure
(GameOver :bird, :pipes, :score)
[r: Rect, color: Color]
```

**preserve_commas=false → カンマを落とす（例）**

```clojure
(GameOver :bird :pipes :score)
[r: Rect color: Color]
```

#### `comments.preserve_trailing`

**入力**

```clojure
(+ 1 2) ; note
```

**preserve_trailing=true（default）→ 行末コメントを同じ行に保持（例）**

```clojure
(+ 1 2) ; note
```

**preserve_trailing=false → コメントを次行へ移動（例）**

```clojure
(+ 1 2)
; note
```

#### `comments.dangling_policy`

**入力**

```clojure
(foo
  (very-long-function-name arg1 arg2 arg3 arg4)
  ; end
)
```

**dangling_policy=own_line（default）→ そのまま独立行（例）**

```clojure
(foo
  (very-long-function-name arg1 arg2 arg3 arg4)
  ; end
)
```

**dangling_policy=attach_prev → 直前の式に吸着（例）**

```clojure
(foo
  (very-long-function-name arg1 arg2 arg3 arg4) ; end
)
```

**dangling_policy=attach_next → 次の式が無い場合は own_line と同じ（例）**

```clojure
(foo
  (very-long-function-name arg1 arg2 arg3 arg4)
  ; end
)
```

#### `comments.spacing`

**入力**

```clojure
(foo)    ; note
```

**spacing=single（default）→ 空白を 1つに統一（例）**

```clojure
(foo) ; note
```

**spacing=preserve → 入力の空白を保持（例）**

```clojure
(foo)    ; note
```

## 2. foreign blocks のフォーマット

`clove fmt` は `--lang` で foreign の整形器を選べます。

- `--lang=clove` … Clove として整形
- `--lang=ruby` … Ruby として整形（rubocop / syntax_tree を使用する場合あり）

> Ruby の整形はビルド時の feature（`ruby`）が必要な場合があります。

## 3. 方針

- なるべく “読んでそのまま実行できる” 形にする
- Reader の糖衣（range, indexer, dot-chain）は **糖衣のまま** 出力する

---
<!-- NAV:START -->
**前へ:** [パッケージ管理（Phase1）](../packages.ja.md)
**次へ:** [Build（--opt=typed / --static / embed）](build.ja.md)
<!-- NAV:END -->

