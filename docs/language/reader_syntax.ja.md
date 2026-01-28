# Reader / 構文一覧

- 更新日: 2026-01-14

このページは「Clove の Reader が読み取れる構文（リテラル、糖衣構文、コメント、引用）」をまとめます。

> **重要:** ここに書いてある展開（desugar）は “実装に準拠した説明” です。
> 将来、展開先の内部シンボル名が変わる可能性はありますが、ユーザー向けの挙動は維持します。

## 1. コメント

- 行コメント: `;` 〜行末
- フォームコメント: `#_ FORM`

```clojure
(+ 1 2) ; => 3
(map #_skip inc (range 3))
```

## 2. 区切りとデータ構造

- list: `(a b c)`
- vector: `[a b c]`
- map: `{:a 1 :b 2}`
- set: `#{:a :b}`

### 2.1 spread（展開）

コレクション内部で `*x` を書くと “展開” になります。

```clojure
(def xs [1 2])
[0 *xs 3]     ; => [0 1 2 3]
{:a 1 *m :z 9}
#{*xs 9}
```

関数呼び出しの引数でも `*x` / `* expr` が使えます。  
`*(...)` のように **`*` と式を隣接**して書いても展開になります。

## 3. 引用

- quote: `'form` / `(quote form)`
- quasiquote: `` `form `` / `(quasiquote form)`
- unquote: `~x` / `(unquote x)`
- unquote-splicing: `~@xs` / `(unquote-splicing xs)`

> `quasiquote` 系は special form として評価器が処理します。

## 4. deref

- `@x` は `(deref x)` に展開されます。

`deref` が理解するのは主に以下です。

- atom
- promise / future / task
- agent
- delay

## 5. Reader 拡張（構文糖）

Clove の特徴的な構文糖は Reader で展開されます。

- indexer: `x[0]`, `x[0 1]`, `x[0,1]`, `x[0||d]`, `a..b` … [indexer](indexer.ja.md)
- dot-chain: `x.(f ?)` / `x.(f *?)` / `expr.(...)` … [dot-chain](dot_chain.ja.md)
- OOP 記法: `obj.method(...)` … [OOP 記法](oop_syntax.ja.md)
- dot-indexer: `obj:key`, `obj.:key`, `obj."key"`, `obj.0` … [OOP 記法](oop_syntax.ja.md)
- call sugar: `sym(...)` → `(sym ...)`（**シンボル直後の `(`** のみ結合、`,` は無視）
- foreign blocks: `$rb{...}` / `$py{...}` / `${...}`（デフォルト外部言語の省略。未指定なら Ruby） … [外部連携](interop_foreign.ja.md)
- map-refs: `{:a 1 :b (+ &^:a 2)}` / `&:screen:h`（有効化: `(use map-refs true)`）
- 関数合成: `(. f g h)` / `(f g h .)`

これらは `use-syntax` で有効/無効を切り替えできます。

```clojure
(use-syntax dot-chain true)
(use-syntax indexer true)
(use-syntax dot-indexer true)
(use-syntax foreign-block true)
(use-syntax oop-syntax true)
(use-syntax map-refs true)
```

## 6. 関数合成の糖衣

```clojure
(. f g h)   ; => (comp h g f)
(f g h .)   ; => (comp f g h)
```

- `.` 側から順に関数へ食わせていくイメージです。

### 5.1 dot-chain の改行継続

`expr.(...)` は **改行後の行頭 `.(...)`** で継続できます（空行を挟むと別フォーム）。

```clojure
(glob* "lib/*.json")
.(map (fn [path] (println path) path) ?)
.(take 3 ?)
```

### 5.2 map-refs

map-refs は **map リテラル内で他のキーを参照するための構文糖** です。
`&` から始まるパスで参照し、**現在の map（this）** または **ルート map（root）** を基準に解決します。

> NOTE: map-refs は map リテラルの中でのみ有効です。外側で使うとエラーになります。

#### 基本例（this / root）

```clojure
(do
  (use map-refs true)
  {:screen {:h 100}
   :pipe   {:gap 10
            :gap-half (int (/ &^:gap 2))
            :gap-max  (- &:screen:h &^:gap-half)}})
```

- `&^:gap` は **現在の map（:pipe）** の `:gap` を参照
- `&:screen:h` は **ルート map** から `:screen` → `:h` を参照
- `&ref:...` / `&ref^:...` はそれぞれ `&:...` / `&^:...` の別名
- 途中で map でない値に進むとエラーになります
- 参照順序は前後どちらでもよく、必要なタイミングで評価されます
- 循環参照や未定義参照はエラーになります

#### ルート map 自体を使う (`&ref`)

`&ref` は **ルート map 自体**を返します。`get` や indexer と組み合わせて使えます。

```clojure
(do
  (use map-refs true)
  {:gap 10
   :gap-half (int (/ (get &ref :gap) 2))
   :gap2 &ref[:gap]})
```

#### `^` と親参照（../）

`^` は **相対（this）** の起点です。`../` を付けると 1 つ上の階層へ移動します。

```clojure
(do
  (use map-refs true)
  {:a {:x 1
       :child {:y (+ &^../:x 2)}}}) ; => :y は 3
```

- `&^:x` = 現在の map（this）
- `&^../:x` = 1 つ上の map
- `&^../../:x` = 2 つ上の map
- `&ref^:...` / `&ref^../:...` は `&^:...` の別名

#### セグメントの指定

map-ref のセグメントは **keyword のみ** です。

```clojure
(do
  (use map-refs true)
  {:gap 1
   "gap" 2
   :val &^:gap}) ; => 1
```

`&x` / `&"x"` / `&root` / `&.` は廃止済みで、使うとエラーになります。

## 6. `#(...)` 短縮関数

Clojure 同様に `#(...)` で匿名関数を短く書けます。

```clojure
(map #( + % 10) (range 3))
```

> `%`, `%1`, `%2`, … は Reader が内部的に引数に展開します。

## 7. 正規表現 / Duration

- regex: `/.../` または `#/.../`
- duration literal: `10ms`, `2s` など（**整数のみ**）

詳細: [正規表現 / Duration](regex_duration.ja.md)

## 8. データセクション（`__DATA__` / `__END__`）

ソース中に `__DATA__` / `__END__` 行がある場合、それ以降は **データセクション**になります。  
データセクションの内容は `__DATA__` で文字列として参照できます。

```clojure
(def payload __DATA__)

__DATA__
{"hello":"world"}
```

- `__DATA__` が存在しない場合は `nil` になります。

---
<!-- NAV:START -->
**前へ:** [REPL ガイド](../tooling/repl.ja.md)
**次へ:** [リテラル（数値・文字列など）](literals.ja.md)
<!-- NAV:END -->

