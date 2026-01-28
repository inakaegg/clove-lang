# Getting Started

- 更新日: 2025-12-21

このページは「Clove を起動して、スクリプトを実行して、REPL で試す」までを最短で案内します。

## 1. ビルド

```bash
# repo ルート
cargo build -p clove-lang --release
```

生成物は通常 `target/release/clove` です。

## 2. REPL

```bash
./target/release/clove --repl
```

REPL で使える主なコマンド:

- `:help` … ヘルプ
- `:doc SYMBOL` … ドキュメント
- `:source PATH` … “この入力はどのファイル由来か” を設定（LSP/エラー表示用）
- `:load FILE` … ファイルを評価

詳しくは [REPL ガイド](tooling/repl.ja.md)。

## 3. スクリプト実行

### 3.1 そのまま評価

```bash
./target/release/clove path/to/app.clv
```

最後に評価された値が表示されます。

### 3.2 `-main` を呼ぶ（スクリプト用途）

```bash
./target/release/clove --main path/to/app.clv arg1 arg2
```

`--main` を付けると、ファイルを評価した後に **`-main` 関数** が定義されていれば呼び出します。
（引数 `arg...` は `-main` に渡されます。）

## 4. フォーマット

```bash
./target/release/clove fmt path/to/app.clv > /tmp/app.formatted.clv
```

`clove fmt` は **stdout に出力** します（現状 “上書き” はしません）。

詳しくは [Formatter](tooling/formatter.ja.md)。

## 5. ビルド（実行可能ファイル化）

```bash
./target/release/clove build path/to/app.clv
```

`build/` 以下に Rust プロジェクトを生成し、`cargo build --release` で実行可能ファイルを作れます。

詳しくは [Build](tooling/build.ja.md)。

---
<!-- NAV:START -->
**次へ:** [言語の基礎](language/basics.ja.md)
<!-- NAV:END -->

