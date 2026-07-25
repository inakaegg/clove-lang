# tools/

リポジトリのメンテナンスに使う開発者向けスクリプト。製品の一部ではないため、
`cargo test` からは呼ばれない。

| スクリプト | 用途 |
| --- | --- |
| `ns_alignment_audit.py` | 標準関数の名前空間が Clojure 由来の配置と揃っているかを監査する |
| `bench/run_micro.sh` | コレクション操作のマイクロベンチマークを実行する |

## ns_alignment_audit.py

`data/clove_docs/clove-docs.json`、`crates/clove-core/assets/clove_std.clv`、
`crates/clove-core/src/symbols.rs` を突き合わせ、Clojure で `clojure.string/reverse`
のように名前空間に属している関数が Clove でも `string::reverse` に置かれているかを
確認する。ズレはレポートに一覧される。

```bash
python3 tools/ns_alignment_audit.py
# => tmp/ns_alignment_report.md
```

レポートは現在のツリーのスナップショットなので `tmp/`（Git管理外）へ書き出す。
検出されたズレをそのまま直すべきとは限らない。Clove が意図的に配置を変えている
ものも含まれるため、`docs/language/namespaces.md` の方針と照合して判断する。

## bench/run_micro.sh

`crates/clove-core/src/bin/bench_collections.rs` を実行し、Vec / Map の構築と
参照のコストを測る。

```bash
tools/bench/run_micro.sh [SIZE] [ITERS] [GET_OPS]   # 既定: 100000 5 1000000
BENCH_FEATURES=... tools/bench/run_micro.sh          # cargo feature を有効にして測る
```

公開ベンチマーク（他言語との比較）は別物で、`docs/phase2/bench/` にある。
