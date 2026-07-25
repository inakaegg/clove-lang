# インタプリタとネイティブビルドの2経路

English version: [two-phase-implementation.md](two-phase-implementation.md)

- 更新日: 2026-07-25

## 決定

Clove は同じ言語に対して**実装を2つ**持っています。

| 経路 | 実装 | 用途 |
| --- | --- | --- |
| インタプリタ（phase1） | `clove-core` | REPL、スクリプト実行、LSP |
| ネイティブビルド（phase2） | `clove-build-core` / `clove-build-front` / `clove-build-backend-c` / `clove-build-runtime-c` | `clove build` による単体バイナリ生成 |

リーダーも型の扱いも別実装です。ネイティブ側は言語の**部分集合**を扱い、
対応していない構文や関数に出会ったら**エラーで止まります**。

## 理由

**性能とメモリが本来の動機です。** インタプリタは、データ構築と関数呼び出しが重く、
単純なアプリでもメモリが GB 単位に達していました。既存実装の改善では限界があると
判断し、型情報を前提に最適化できる経路を別に作りました。

**インタプリタを捨てなかったのは、用途が違うからです。** REPL・LSP・スクリプト実行には
動的な柔軟さが要ります。ネイティブビルドが要求する「ビルド時に全体が確定していること」と
両立しません。片方に寄せると、もう片方の体験が壊れます。

**黙ってフォールバックしません。** ネイティブビルドが未対応の機能に当たったとき、
インタプリタに退避して動かす選択肢もありましたが採りませんでした。それをすると
「ネイティブビルドしたはずなのに速くない」が原因不明のまま起きます。
未対応は未対応としてビルド時にエラーにします。

```bash
clove build app.clv
# lower error: set is not supported in typed IR yet
# unsupported call in phase2 C build: eval
# lower error: foreign block is not supported in typed IR yet
```

## ネイティブ側で禁止していること

ビルド時に全体が確定しているという前提を守るため、動的な機能は使えません。

- `eval` / `load-string` / `load-file` / `read-string`
- `set!` / `redef` / `with-redefs` / `with-dyn`
- 実行時の名前空間操作（`create-ns` / `resolve` など）
- `require` はビルド時に静的解決される

これは制限であると同時に、[マクロを持たない](no-macros.ja.md)決定と同じ方向です。
実行時にコードが変わらないなら、ビルド時に全部見られます。

## 現在の対応範囲

**ネイティブ経路はまだ言語の一部しか扱えません。** 2026-07-25 時点で確認した範囲では、
`clove build` が通るのはトップレベルのフォームと 1〜3 引数の関数定義までです。

| 例 | 結果 |
| --- | --- |
| `(println "hi")` | 通る |
| `(defn f [a] a)` | 通る |
| `(defn f [] 1)` / `(defn f [a b c d] a)` | `lambda currently supports one to three params` |
| `(defn -main [] ...)` | 同上。加えて生成バイナリは `-main` を呼ばない |
| `#{1 2}`（set リテラル） | `set is not supported in typed IR yet` |
| `$rb{...}`（foreign） | `foreign block is not supported in typed IR yet` |
| 自己再帰関数 | ビルドがスタックオーバーフローで異常終了（既知の不具合） |

対応状況は動いています。現時点の詳細は [docs/phase2/](../phase2/README.md) を見てください。

## 2つの実装を持つことの代償

**仕様がずれます。** 2026-07-25 に、ネイティブ側のリーダーが
[廃止済みの `/` 区切り](namespace-separator.ja.md)を受理していたことが判明しました。
決定から2か月半後に書かれたリーダーが、決定を反映しないまま実装されていたためです。

この種のずれは「片方だけ直して気づかない」形で入ります。対策として、2つのリーダーへ
同じコーパスを通し、**ネイティブ側がインタプリタ側より寛容になっていないこと**を
検査するテストを置いています。

```
crates/clove-lang/tests/reader_parity.rs
  phase2_is_never_more_permissive_than_phase1
```

ネイティブ側がインタプリタ側の部分集合であることは設計どおりなので、片方向だけを禁じます。
逆方向（ネイティブが受理してインタプリタが拒否する）は、言語にない構文を実装している
ことを意味するため、テストが落ちます。

## 関連

- [Phase2 ネイティブビルド経路](../phase2/README.md)
- [clove build](../tooling/build.md)
