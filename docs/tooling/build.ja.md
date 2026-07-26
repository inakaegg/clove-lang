# Build（`clove build`）

英語版: [build.md](build.md)

- 更新日: 2026-07-25

`clove build` は現在の C バックエンドで Clove ソースをコンパイルし、ネイティブバイナリを生成します。

## 1. 使い方

```bash
clove build path/to/app.clv
```

デフォルト出力先:

```text
target/clove/bin/<file-stem>
```

## 2. ビルドオプション

```bash
clove build path/to/app.clv --out ./bin/app
clove build path/to/app.clv --emit-c
```

- `--out PATH`
  - 生成するバイナリの出力先。
- `--emit-c`
  - バイナリは通常どおり生成した上で、表示をバイナリパスではなく生成 C ファイルパス（`<out>.c`）に切り替えます。
- `--main`
  - トップレベルのフォームを実行したあとに `(-main)` を呼びます。指定しない場合は
    トップレベルのみを実行し、`-main` を定義しているのに未指定ならビルド時に警告します。
    インタプリタの `clove --main` と対称です。
- `-o`, `--output`
  - `--out` の別名。

## 3. 旧オプション（未対応）

以下の旧 `clove build` オプションは現在は使用できません。

- `--opt`
- `--static`
- `--embed-ruby`
- `--embed-python`
- `--strict-types`
- `--emit-typed-ir`
- `--allow-native-plugins`
- `--plugin-dir`

実行時の設定は `clove` 側のランタイムオプションを使用し、最新の仕様は `clove --help` / `clove build --help` を参照してください。

## 4. 既知の制限と不具合

`clove build` は言語の**部分集合**しか扱えません。未対応の機能に当たると、インタプリタへ
フォールバックせずビルド時にエラーで止まります（[設計ノート](../design-notes/two-phase-implementation.ja.md)）。

以下は 2026-07-25 に実測した結果です。ここに挙がっている項目を踏んだ場合、
それは新しい不具合ではありません。

### 4.1 再帰

呼び出しは関数本体を呼び出し箇所へ展開してコンパイルするため、再帰関数は
コンパイルできません。ビルド時に明示されます。

```clojure
(defn f [n] (if (< n 2) 1 (* n (f (dec n)))))
; => recursive function 'f' is not supported by the C backend yet (calls are inlined)
```

相互再帰も同様に報告されます。`(loop ... (recur ...))` で書き直すか、
`clove app.clv` で実行してください。

2026-07-26に修正（以前はビルドプロセスがスタックオーバーフローで異常終了し、
引数0個・4個以上の関数と戻り値の型注釈はビルドできませんでした）。

いずれも実行時（`clove app.clv`）には問題ありません。ネイティブビルド経路のみの制限です。

### 4.2 未実装（設計として段階的に対応中）

| 機能 | 状態 |
| --- | --- |
| `#{...}` set リテラル | `set is not supported in typed IR yet` |
| `deftype` | `deftype is not supported in phase2 C backend yet` |
| `match` | `unsupported call in phase2 C build: match` |
| `mut` / `imut` | `unsupported call in phase2 C build: mut` |
| `atom` | `unsupported call in phase2 C build: atom` |
| `$rb{...}` / `$py{...}` foreign ブロック | `foreign block is not supported in typed IR yet` |
| 遅延シーケンス | 未対応。すべて eager |

### 4.3 動的機能（意図的な非対応）

ビルド時に全体が確定している必要があるため、以下は使えません。仕様です。

- `eval` / `load-string` / `load-file` / `read-string`
- `set!` / `redef` / `with-redefs` / `with-dyn`
- 実行時の名前空間操作（`create-ns` / `resolve` など）

### 4.4 動作を確認したもの

トップレベルのフォーム、任意個数の引数の関数（0個を含む）、`--main` による
`(-main)` 呼び出し、戻り値の型注釈、可変長引数、`loop` / `recur`、
vec / map リテラル、`(range n)` / `(range 0 n)`、`reduce`、`take`、`map`、`str`、
引数の後置型注釈（型不一致はビルドエラーになる）。

---
<!-- NAV:START -->
**前へ:** [Formatter（fmt / rubocop/syntax_tree）](formatter.ja.md)
**次へ:** [LSP（clove-lsp）](lsp.ja.md)
<!-- NAV:END -->

