# Phase2 の設計と現状

英語版: [README.md](README.md)

Phase2 は、Clove の実験的なネイティブビルド経路です。既存の実行系は REPL とスクリプト実行を引き続き担当し、`clove build` は対応済みの言語サブセットを C バックエンドへ渡します。

## 実行経路とビルド経路

| 経路 | 目的 | 主な構成要素 | 現在の入口 |
| --- | --- | --- | --- |
| Phase1 実行経路 | 動的機能を含む柔軟な REPL とスクリプト実行 | [`clove-core`](../../crates/clove-core/)、[`clove-lang`](../../crates/clove-lang/)、[`clove-lsp`](../../crates/clove-lsp/) | `clove --repl`、`clove file.clv` |
| Phase2 ネイティブビルド経路 | 静的に対応できる言語サブセットの事前コンパイル | [`clove-build-front`](../../crates/clove-build-front/)、[`clove-build-backend-c`](../../crates/clove-build-backend-c/)、[`clove-build-runtime-c`](../../crates/clove-build-runtime-c/) | `clove build file.clv` |

現在のネイティブビルドは次の順で処理します。

```text
Clove ソース
  -> clove-build-front
  -> clove-build-backend-c が C ソースを生成
  -> システムの C コンパイラ
  -> ネイティブ実行ファイル
```

[`clove-build-core`](../../crates/clove-build-core/) には、再設計の過程で作られた Phase2 のパーサ、型、コード生成、VM の基盤があります。現在の C 経路はその構文モデルを再利用していますが、Phase2 のすべての設計機能が `clove build` から利用できる段階ではありません。

## 設計方針

- **Native-first のビルド経路:** 未対応の動的処理を暗黙にインタプリタへフォールバックせず、ネイティブコンパイル時にエラーとします。
- **型情報を使う lowering:** build、実行時最適化、LSP 診断で同じ typed pipeline を共有することが長期方針です。現在も統合作業中です。
- **明示的な外部言語境界:** ネイティブビルドでは、外部言語呼び出しを通常の動的値に混ぜず、境界として見える設計を目指します。
- **動的値の限定:** `Any` を可能な限り避け、`Dyn` は明示的な境界に限定する方針です。

### `mut` と `imut`

Phase2 はコレクション更新を二つのモードに分けます。

- `imut` は、元の値を観測した結果が変わらない更新です。
- `mut` は、その場での更新を必須とし、値が共有されている場合はエラーにします。

この区別は Phase2 コンパイラの設計であり、`clove-build-core` に実装されています。統合済みの C バックエンドはまだ `--mut`、`--mode`、`--native` を公開していないため、これらを現在の `clove build` のコマンドライン保証としては扱いません。

## 現在の実装状況

- `clove build` は C バックエンドへ接続され、対応済みサブセットからネイティブ実行ファイルを生成します。
- `--out` / `--output` で出力先を指定し、`--emit-c` で生成した C ファイルのパスを表示できます。
- REPL と通常のスクリプト実行は、引き続き既存の実行系を使います。
- `clove-lsp` はネイティブビルド経路とは独立してエディタ向け言語機能を提供します。
- 現在の C バックエンドでは、`upper-case`、`lower-case`、`capitalize` は ASCII 文字列だけに対応します。非 ASCII 入力は不正な UTF-8 を生成せず、ネイティブ実行ファイルが明示的な実行時エラーを報告します。
- C バックエンドは実験段階です。インタプリタで動くすべての例をビルドできる状態ではありません。

何がビルドでき、何ができないか、および現時点で判明している不具合の実測一覧は
[既知の制限と不具合](../tooling/build.ja.md#4-既知の制限と不具合) にあります。

## ビルド例

clone 済みのリポジトリから CLI をインストールします。

```bash
cargo install --path crates/clove-lang
```

確認済みの例をビルドして実行します。

```bash
clove build examples/build/high_value_report.clv \
  --out target/clove/bin/high_value_report
./target/clove/bin/high_value_report
```

生成した C ファイルを確認する場合:

```bash
clove build examples/build/high_value_report.clv \
  --out target/clove/bin/high_value_report \
  --emit-c
```

動作するシステム C コンパイラが必要です。

## 過去のベンチ結果

[2026-01-29 ベンチ記録](bench/records/bench_release_20260129.md) は、同じ処理について、以前の `clove2` ネイティブビルド・VM 経路と Rust、Go、Ruby、Clojure を比較したものです。

| 実装 | hyperfine mean | max RSS |
| --- | ---: | ---: |
| `clove2` 生成実行ファイル | 0.3626 s | 5.3 MiB |
| `clove2 run(vm)` | 1.429 s | 280.0 MiB |
| Rust | 0.1299 s | 2.7 MiB |
| Go | 0.2000 s | 9.3 MiB |
| Ruby | 1.150 s | 57.4 MiB |
| Clojure | 1.289 s | 600.5 MiB |

この測定は統合済み C バックエンドより前の履歴上の基準値であり、現在の `clove build` の性能を示すものではありません。記録にはコマンド、実行回数、`hyperfine` の生出力を残しています。

## 関連文書

- [Phase2 の決定事項](DECISIONS.ja.md)
- [build コマンド](../tooling/build.ja.md)
- [ベンチのソースと測定方法](bench/README.ja.md)
- [リポジトリ概要](../../README.ja.md)
