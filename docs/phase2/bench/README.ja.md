# 公開用ベンチ（1本化）

英語版（公式）: `README.md`  
※ 日本語版は `README.ja.md` にまとめています。

このディレクトリは **公開リポジトリ向けの最小ベンチ**のみを保持します。

## ベンチコード

- `bench_release_clove2.clv`  
  各関数を **複数回反復**する構成で、1回だけの呼び出しに偏らないようにしています。
- 他言語の対応コード:
  - `bench_release_rust.rs`
  - `bench_release_go.go`
  - `bench_release_ruby.rb`
  - `bench_release_clj.clj`

## 記録

- `records/bench_release_20260129.md`

## 実行例

```bash
clove build docs/phase2/bench/bench_release_clove2.clv --out target/clove/bin/bench_release
/usr/bin/time -l target/clove/bin/bench_release
```

現在のCバックエンドはこのファイルをまだコンパイルできません。`map` に渡している
ラムダが `let` とコレクション操作を含み、バックエンドは算術ラムダのみを下ろすためです
（`map lambda must be simple arithmetic`）。記録済みの数値は以前の実装で取ったものです。
対応範囲は[ビルドの既知の制限](../../tooling/build.ja.md)を参照してください。
インタプリタで測る場合は `clove docs/phase2/bench/bench_release_clove2.clv` を実行します。
