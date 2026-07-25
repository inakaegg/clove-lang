# 名前空間区切りに `/` を使わない

English version: [namespace-separator.md](namespace-separator.md)

- 更新日: 2026-07-25

## 決定

名前空間の区切りは `::` です。`/` は区切りとして使えません。
`foo/bar` や `:foo/bar` は**パース段階でエラー**になります。

```bash
clove -e '(println :foo/bar)'
# Parse error: namespace separator '/' has been removed; use '::' (e.g. foo::bar).
#              If you meant a regex literal, use #/.../
```

例外は単体の `/` だけで、これは除算演算子として残っています。

```clojure
(println (/ 6 2))        ; => 3
(println string::upper-case) ; 名前空間つきの参照
```

あわせて、**キーワードは名前空間に紐づきません**。`:foo::bar` は「`foo` 名前空間の
`bar`」ではなく、`"foo::bar"` という名前のキーワードそのものです。

```bash
clove -e '(println (name :foo::bar))'   # => foo::bar
clove -e '(println (namespace :foo::bar))'  # Unbound symbol: 'namespace'
```

`namespace` 関数は存在しません。キーワードから名前空間を取り出す概念自体がないためです。

## 理由

**`/` は演算子と衝突する。** Lisp で `/` は除算です。同じ文字を区切りにも使うと、
リーダーは「シンボルの一部か、演算子か」を文脈で判断することになります。
Clove には正規表現リテラル `/.../` もあるため、衝突面はさらに増えます。

**`::` は他言語と見た目が揃う。** Rust と C++ が `::`、Ruby も定数参照は `::` です。
Clove は Ruby / Rust から記法を借りている箇所が多く
（[他言語から借りた記法](borrowed-syntax.ja.md)）、区切りもそちらに合わせました。

**Clojure の名前空間つきキーワードは扱いが難しい。** Clojure の `::foo` は
「キーワード自体が名前空間を持つ」ため、同じ `:name` でも文脈によって別物になり得ます。
データとしてのキーワードは常にグローバルに同じものである方が、人にとってもツールにとっても
単純です。Clove ではキーワードは値のラベルに徹し、名前空間は**シンボル**が持ちます。

## 採らなかった案

**Clojure 経験者のために `/` を移行措置として残す。** 実際に検討しましたが採りませんでした。
両方を受理すると、リーダーの曖昧さが解消されず、上に挙げた理由がすべて残ります。
「エラーで `::` を使えと言う」方が移行としても速いと判断しました。エラーメッセージには
`::` 版と正規表現リテラルの両方の案内を入れてあります。

**キーワードに名前空間を持たせる。** `:my.app/status` のような修飾で衝突を避ける案です。
外しました。キーワードは主に map のキーとして使われるため、同じ見た目のキーが文脈で
別物になる方が事故を生みます。名前が衝突する場合はキーワードを長くすれば済みます。

## 影響

この決定は 2025-12-17 に確定し、リーダー・名前解決・表示・ドキュメント・LSP・stdlib・
builtin 登録名のすべてを `::` に統一しました。`process/sh` は `process::sh`、
`json/parse` は `json::parse` になっています。

2026-07-25 に、[ネイティブビルド経路](two-phase-implementation.ja.md)のリーダーが
この決定を反映しておらず `:foo/bar` を受理していたことが判明し、修正しました。
再発防止として、2つのリーダーへ同じコーパスを通し「ネイティブ側がインタプリタ側より
寛容になっていないこと」を検査するテスト（`crates/clove-lang/tests/reader_parity.rs`）を
追加しています。

## 関連

- [名前空間](../language/namespaces.md) — 現在の仕様
- [名前空間設計ノート](../advanced/namespaces_design.ja.md) — `ns` / `require` の設計
