# 他言語から借りた記法

English version: [borrowed-syntax.md](borrowed-syntax.md)

- 更新日: 2026-07-25

## 決定

Clove は S式を土台にしつつ、Ruby / JavaScript / Rust から記法を借りています。
借りたものは4つです。

```bash
clove -e '(def xs [10 20 30]) (println xs[0] xs[-1] xs[0...2] xs[9 || :none])'
# 10 30 [10 20] :none

clove -e '(println (range 5).(filter even? ?).(map inc ?))'
# [1 3 5]

clove -e '(println 1.inc "abc".upper-case)'
# 2 ABC

clove -e '(println {name: "a" age: 1})'
# {:name "a" :age 1}
```

| 記法 | 借り元 | 何のため |
| --- | --- | --- |
| indexer `xs[0]` | Ruby / JS | 添字アクセスを短く書く |
| dot-chain `.(f ?)` | — | 処理の流れを左から右に読む |
| OOP風呼び出し `1.inc` | Ruby | レシーバを先に書く |
| map 省略記法 `{name: 1}` | JS | map リテラルの記述量を減らす |

## 理由

**S式の弱点は「読む順」です。** `(map inc (filter even? (range 5)))` は
実行順と読む順が逆になります。Clojure はこれを `->>` で解決しますが、
Clove ではさらに `.(...)` を用意しました。`?` が前段の値の位置を示します。

```clojure
(range 5).(filter even? ?).(map inc ?)   ; 左から右に読める
(->> (range 5) (filter even?) (map inc)) ; これも使える
```

どちらも使えます。`->>` を消したわけではありません。

**添字アクセスは頻度が高い。** `(nth xs 0)` や `(get-in m [:a :b])` は正しい書き方ですが、
データを触るコードでは出現回数が多く、そのたびに括弧が深くなります。
`xs[0]` / `m[:a :b]` は同じことを短く書く手段です。

**レシーバを先に書きたい場面がある。** `"abc".upper-case` は補完と相性が良く、
エディタで「この値に何ができるか」を探すのに向いています。

**Lisp らしさを損なわない範囲に留めました。** どの記法もリーダーが S式へ展開するもので、
評価器から見れば通常のフォームです。新しい評価規則は増えていません。

## 採らなかった案

**Clojure の書き方だけに揃える。** 括弧の深さと読む順の問題が残ります。
Clove は Lisp の互換性より、書き味を優先しました。

**ユーザーが記法を追加できるようにする。** [マクロを持たない](no-macros.ja.md)決定と
同じ理由で外しました。リーダーが固定されているからこそ、LSP とフォーマッタが
すべての構文を理解できます。

**dot-chain の省略形を増やす。** `m.a` のような「キーワードを省く」形を一度入れましたが、
バグの温床になったため廃止しました。`m.:a` は残っています。省略しすぎると、
シンボル参照なのか map アクセスなのかが読み手にもツールにも判別できなくなります。

## 使い分け

同じことを複数の書き方でできる箇所があります。どれも正しく、優劣は付けていません。

| やりたいこと | 書き方 |
| --- | --- |
| コレクションの n 番目 | `(nth xs 0)` / `xs[0]` |
| ネストした map の参照 | `(get-in m [:a :b])` / `m[:a :b]` |
| 処理の連結 | `(->> ...)` / `.(f ?)` |
| 関数適用 | `(inc 1)` / `1.inc` |

プロジェクト内では揃えることを勧めますが、言語としてはどちらも受け付けます。

## 関連

- [indexer](../language/indexer.ja.md)
- [dot-chain](../language/dot_chain.ja.md)
- [OOP風の呼び出し](../language/oop_syntax.ja.md)
- [map の省略記法](../language/map_shorthand.ja.md)
- [リーダー構文](../language/reader_syntax.ja.md)
