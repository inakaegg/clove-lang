# Clojure から採らなかったもの

English version: [differences-from-clojure.md](differences-from-clojure.md)

- 更新日: 2026-07-25

## 決定

Clove は Clojure 系の Lisp を土台にしていますが、**Clojure 互換ではありません**。
Clojure のコードがそのまま動くことを目標にしていないため、意図的に外した機能があります。

「未実装でいずれ入る」ものと「入れない」ものを区別できるよう、後者をここに挙げます。

| 機能 | 状態 | 理由 |
| --- | --- | --- |
| `defmacro` / syntax-quote | 入れない | [マクロを持たない](no-macros.ja.md) |
| `defprotocol` / `extend-type` | 入れない | 型拡張の機構を持たない。多態は map に関数を持たせる形で表現する |
| `defmulti` / `defmethod` | 入れない | ディスパッチ機構を増やさない。関数と `match` で足りる |
| `/` 名前空間区切り | 廃止済み | [名前空間区切りに `/` を使わない](namespace-separator.ja.md) |
| 名前空間つきキーワード (`::foo`) | 入れない | 同上 |
| `in-ns` などの名前空間の出入り | 入れない | 後述 |

```bash
clove -e '(defprotocol P)'   # Unbound symbol: 'defprotocol'
clove -e '(defmulti f class)' # Unbound symbol: 'defmulti'
clove -e '(in-ns (quote foo))' # Unbound symbol: 'in-ns'
```

## 名前空間の出入りを持たない

Clojure では `in-ns` で実行中の名前空間を切り替えられます。Clove にはこれがありません。
名前空間は**ファイルごとに決まり、実行中は変わりません**。

- ファイル先頭に `(ns ...)` があればそれ
- なければファイルパスから導出される
- `clove -e` や `clove --repl` では `user`

```bash
clove -e '(println (current-ns))'   # => user
```

`(ns ...)` がディレクトリ構成と合わないと警告が出ます。

```
[WARN] namespace 'my::app' defined in '.../ns2.clv' does not match directory layout
```

理由は「1名前空間 = 1ファイル」という原則を守るためです。この原則があると、
LSP は「この名前空間はこのファイル」と一意に決められるので、定義ジャンプと補完が
確実になります。ビルド時の静的解決にも同じ前提が効きます。詳細は
[名前空間設計ノート](../advanced/namespaces_design.ja.md) にあります。

## 逆に、Clojure でマクロだったものは言語に入っている

外した機能がある一方で、Clojure でマクロとして提供されているものの多くは
special form として言語側に入っています。

`when` `when-not` `when-let` `if-let` `if-some` `cond` `cond->` `cond->>` `condp`
`->` `->>` `as->` `some->` `some->>` `while` `doseq` `dotimes` `for` `doto`
`with-open` `with-redefs` `defn` `defn-` `ns` `delay` — これらはすべて使えます。

つまり「マクロがない」ことと「Clojure らしく書けない」ことは別です。
`(-> x (assoc :a 1) (update :b inc))` はそのまま書けます。

## 遅延シーケンスはある

`range` は引数なしで無限列になり、`take` で必要な分だけ取れます。

```bash
clove -e '(println (take 3 (range)))'   # => [0 1 2]
```

ただし[ネイティブビルド経路](two-phase-implementation.ja.md)では遅延評価を
サポートしていません。ネイティブ側は現時点ですべて eager です。

## ライセンス上の位置づけ

Clove は Clojure のソースコードを流用していません。構文と関数名の一部を参考にした
独立実装です。Clove 自体は MIT / Apache-2.0 のデュアルライセンスです
（[LICENSE-MIT](../../LICENSE-MIT) / [LICENSE-APACHE](../../LICENSE-APACHE)）。

## 関連

- [FAQ: Clojure と同じか？](../faq.ja.md)
- [基本構文](../language/basics.ja.md)
