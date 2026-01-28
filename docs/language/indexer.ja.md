# ブラケット indexer（`[]`）

- 更新日: 2025-12-21

Clove の **indexer** は Reader の構文糖で、配列/文字列/map/set/ネスト構造へのアクセスを短く書けます。

> 環境によっては SyntaxFeature として ON/OFF できます（例: `use-syntax indexer true`）。  
> 実際のデフォルトや切り替えは **[docs/advanced/runtime_settings.md](../advanced/runtime_settings.ja.md)** を参照。

---

## 1) まずは例（一覧）

```clojure
;; 基本
xs[0]
xs[-1]
xs[1 || :default]

;; map / set / get-in
m[:key]
m[:missing || 42]
m[:nested :inner]
m[:nested :inner, :other]
m[:nested :missing || :fallback]
tags[:x]
tags[:z || :absent]

;; index 列での gather
xs[[0 2 4]]
xs[(range 3)]
xs[1,5,7]

;; range（Ruby 互換のスライス）
xs[1..3]
xs[1...3]
xs[..3]
xs[2..]
xs[-3..-1]
s[1..3]

;; 式
xs[(+ 1 2)]
xs[(do (inc 1))]
xs[i.inc..i.inc]

;; -foo は indexer 内だけの特別ルール
;; -foo が束縛されていればそれを使い、無ければ (- foo) として扱う
xs[-5..-foo]
xs[-5..--foo]
```

---

## 2) 単一アクセス: `xs[i]`

- `xs[0]` は 0 番目
- `xs[-1]` は末尾（負数インデックス）
- `xs[(+ 1 2)]` のように、**インデックス式**も書けます

対象が vector / string などの場合、概ね「添字アクセス」になります。

---

## 3) デフォルト: `xs[i || fallback]`

`||` を使うと「無い/失敗した場合の既定値」を書けます。

- `m[:missing || 42]` のような **map の欠損**に便利
- set でも `tags[:z || :absent]` のように書けます（含まれない場合に fallback）

> 何を「無い/失敗」とみなすか（`nil` だけか、範囲外も含むか等）は、
> 対象コレクションと indexer の実装に依存します。  
> 現状、範囲外は `index out of bounds` のようなエラーになり得ます（slice でも同様）。

---

## 4) ネストアクセス: `m[:a :b :c]`（get-in）

`[]` の中に複数キー/インデックスを書くと、ネストアクセス（get-in 相当）になります。
**カンマ区切りとは意味が変わる**ので注意してください。

```clojure
m[:nested :inner]
m[:nested :missing || :fallback]
```

---

## 5) カンマ区切りの複数パス: `xs[1,5,7]` / `m[:a :b, :c]`

カンマ区切りは **複数パス取得**として扱われます。
対象がシーケンスで各パスが単一インデックスの場合は **`nth` の複数指定**と同等です。

- 結果は常に vector
- 順序・重複を保持
- `||` で default を指定可能
- `xs[1 5 7]` は **get-in** のまま（別の意味）

```clojure
(def xs [10 11 12 13 14 15])
xs[1,5]         ; => [11 15]
xs[0,99 || :ng] ; => [10 :ng]
(def m {:a {:b 1} :c 2})
m[:a :b, :c]    ; => [1 2]
```

---

## 6) gather: `xs[[0 2 4]]` / `xs[(range 3)]`

「インデックス列」を渡すと、その複数箇所をまとめて取り出せます。

- `xs[[0 2 4]]` のように **リテラル vector**
- `xs[(range 3)]` のように **式で生成した seq**

> gather の結果の型（vector になるか等）や、範囲外をどう扱うかは実装に依存します。

---

## 7) range slice: `xs[a..b]` / `xs[a...b]`

Ruby 互換のスライス表記をサポートします。

- `a..b` : 終端 **含む**（inclusive）
- `a...b` : 終端 **含まない**（exclusive）
- `xs[..3]` : 始点省略（先頭から）
- `xs[2..]` : 終端省略（末尾まで）
- `xs[-3..-1]` : 負数インデックスで末尾基準

文字列も同様にスライスできます（例: `s[1..3]`）。

> 仕様上の見た目は Ruby 互換でも、実装上の境界チェックは Clove 側に依存します。  
> 例: `xs[0..10]` がエラーになることがあります（範囲外）。

---

## 8) `-foo` の特別ルール（indexer 内のみ）

indexer 内では `-foo` を「負数」っぽく書きたいことがよくあります。

そのため `-foo` は以下の特別扱いです:

- `-foo` が **束縛されていれば** それを参照する（シンボル `-foo`）
- 束縛が無ければ `(- foo)` とみなす（単項マイナス）

```clojure
xs[-5..-foo]   ; -foo が束縛されていればそれ、無ければ (- foo)
xs[-5..--foo]  ; --foo は「(- (- foo))」の意味になり得る
```

---

## 9) 内部展開（参考）

indexer は Reader で desugar（展開）され、実際の評価時には専用の関数呼び出しに落ちます。
ユーザーコードが内部シンボルに依存する必要はありません。

---

*更新日: 2025-12-21*

---
<!-- NAV:START -->
**前へ:** [名前空間（ns/require/resolve/load など）](namespaces.ja.md)
**次へ:** [dot-chain: `x.(f ?)`, `x.(f *?)` など](dot_chain.ja.md)
<!-- NAV:END -->
