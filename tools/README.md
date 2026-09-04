# tools/

リポジトリのメンテナンスに使う開発者向けスクリプト。製品の一部ではないため、
`cargo test` からは呼ばれない。

| スクリプト | 用途 |
| --- | --- |
| `ns_alignment_audit.py` | 標準関数の名前空間が Clojure 由来の配置と揃っているかを監査する |
| `bench/run_micro.sh` | コレクション操作のマイクロベンチマークを実行する |
| `phase2/vm_coverage.py` | ネイティブビルド経路の builtin が VM の高速パスに乗っているかを集計する |
| `phase2/vm_build_matrix.py` | 同じ builtin について、native codegen と VM 高速パスの対応状況を突き合わせる |
| `release/package.sh` | 配布用の `clove`（Ruby / Python 埋め込みなし）をビルドし、tar.gz へまとめる |
| `release/sign-macos.sh` | macOS の実行ファイルを Developer ID 証明書で署名し、公証を通す |

レポートを生成するスクリプトはいずれも `tmp/`（Git管理外）へ書き出します。
現在のツリーのスナップショットであり、バージョン管理する意味がないためです。

`release/` の 2 つだけは配布物を作るため `dist/`（同じく Git管理外）へ書き出します。

## ns_alignment_audit.py

`data/clove_docs/clove-docs.json`、`crates/clove-core/assets/clove_std.clv`、
`crates/clove-core/src/symbols.rs` の 3 つを突き合わせる。Clojure で
`clojure.string/reverse` のように名前空間に属している関数が、Clove でも
`string::reverse` に置かれているかを確認する。ズレはレポートに一覧される。

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

## phase2/vm_coverage.py, phase2/vm_build_matrix.py

`crates/clove-build-core/` の `builtins.rs` / `vm/mod.rs` / `codegen.rs` を読み、
ネイティブビルド経路の builtin がどの実行経路でカバーされているかを集計する。

```bash
python3 tools/phase2/vm_coverage.py      # => tmp/phase2_vm_coverage.md
python3 tools/phase2/vm_build_matrix.py  # => tmp/phase2_vm_build_matrix.md
```

`vm_coverage.py` は VM の高速パスと `Value` ベースの fallback を分けて数える。
`vm_build_matrix.py` はそこへ native codegen の対応状況を加えて突き合わせる。
[Phase2 の決定事項](../docs/phase2/DECISIONS.ja.md) が定める
「ビルドの hot path では `Value` を使用しない」の進捗を見るための道具。

## release/package.sh, release/sign-macos.sh

GitHub Releases へ添付する配布バイナリを作る。`package.sh` は Ruby / Python の埋め込みを
外した `clove` をビルドし、ライセンス 2 ファイルと合わせて tar.gz と `SHA256SUMS` にまとめる。
macOS では `sign-macos.sh` を呼び、署名用の環境変数が揃っていれば署名と公証まで行う。
揃っていなければ理由を表示して署名を飛ばすので、鍵を登録する前でも未署名のアーカイブは作れる。

```bash
tools/release/package.sh                    # => dist/clove-<version>-<target>.tar.gz
tools/release/package.sh --no-build --no-sign
```

`.github/workflows/release.yml` は `v*` タグの push と手動起動で同じスクリプトを走らせる。
埋め込みを外した理由と署名の設計は
[配布バイナリの設計ノート](../docs/design-notes/prebuilt-binaries.ja.md)にある。
