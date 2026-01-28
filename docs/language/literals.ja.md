# リテラル

- 更新日: 2025-12-21

## 1. 数値

### 1.1 int

- `0`, `42`, `-10`
- 区切り: `1_000_000`

### 1.2 float

- `0.5`, `-3.14`

> `Duration` の “リテラル” は現在 **整数のみ** です。
> 小数秒などは `(duration 0.1 :s)` のような関数で作ります。

## 2. 文字列

- `"..."`
- 文字列補間: `"hello #{name}"`

補間は Reader が `str` 結合相当に展開します。

## 3. キーワード

- `:foo`
- `:ns/foo` のようなスラッシュを含む形も Reader は読み取れますが、
  Clove では namespace 区切りに `::` を推奨します。

## 4. シンボル

- `foo`, `core::map`, `my.ns::f`

`::` は Clove の名前空間区切りです。

## 5. nil / bool

- `nil`
- `true`, `false`

## 6. regex / duration

別ページ: [正規表現 / Duration](regex_duration.ja.md)

---
<!-- NAV:START -->
**前へ:** [Reader / 構文一覧](reader_syntax.ja.md)
**次へ:** [コレクション（list/vector/map/set）](collections.ja.md)
<!-- NAV:END -->

