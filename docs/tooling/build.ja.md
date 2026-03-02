# Build（`clove build`）

英語版: [build.md](build.md)

- 更新日: 2026-03-02

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

---
<!-- NAV:START -->
**前へ:** [Formatter（fmt / rubocop/syntax_tree）](formatter.ja.md)
**次へ:** [LSP（clove-lsp）](lsp.ja.md)
<!-- NAV:END -->

