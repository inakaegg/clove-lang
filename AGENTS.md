# AGENTS.md — Clove リポジトリ固有のルール

一般的な作業規律（docs-first、test-first、1 commit 1目的、feature branch → PR、
機密情報を含めない など）は前提とし、ここには**このリポジトリでしか通用しない**
ことだけを書きます。

---

## 1. 検証コマンド

```bash
cargo test --workspace          # 基準値: 1096 passed / 0 failed @ 2026-07-25
cargo test -p clove-core        # インタプリタのみ
cargo test -p clove-build-core  # ネイティブビルド経路のみ
cargo fmt --check
cargo clippy --workspace
```

---

## 2. 実装が3箇所以上に散っているもの

Clove は「実装したのに補完に出ない」「doc が引けない」が起きやすい構造です。
以下は**同時に更新しないと壊れる**組み合わせです。

### 2.1 Special Form を追加・変更したとき

5箇所すべてを更新する。落とすと `__apply` に巻かれて未定義扱いになります。

1. `crates/clove-core/src/eval.rs` — special form dispatch
2. `crates/clove-core/src/compiler.rs` — `is_special_form`
3. `crates/clove-core/src/typing/infer.rs` — 型推論での特別扱い
4. `data/clove_docs/clove-docs.json` — ドキュメント
5. REPL 補完 / ヘルプ

### 2.2 関数・alias を追加・改名したとき

REPL と LSP の補完候補に出ること、`doc` と examples が引けることを確認する。

### 2.3 構文を追加・変更したとき

リーダーが**インタプリタ（`clove-core`）とネイティブビルド（`clove-build-core`）で
別実装**です。片方だけ直すと仕様がずれます。

ネイティブ側はインタプリタ側の**部分集合**であることが設計です。逆方向
（ネイティブが受理してインタプリタが拒否する）は言語にない構文を実装していることを
意味し、`crates/clove-lang/tests/reader_parity.rs` が検出します。
構文を変えたらこのテストのコーパスも更新してください。

背景: [2経路の設計ノート](docs/design-notes/two-phase-implementation.ja.md)

---

## 3. 設計判断を変えるとき

現在の仕様を選んだ理由は [docs/design-notes/](docs/design-notes/README.ja.md) にあります。
マクロ、名前空間区切り、型注釈の扱い、`mut`/`imut`、借用した記法のいずれかを変える提案は、
該当ノートの「採らなかった案」を読んでから出してください。前提が変わったなら、
ノート側も同じ変更で更新します。

---

## 4. Clove コードを書くとき

`.clv` ファイルは作成・修正時に `clove fmt` を通す。整形結果がおかしければ
フォーマッタ側の不具合として直す（フォーマッタを避けて書かない）。

---

## 5. ベンチマーク

- Rust で約1秒になるよう負荷を調整し、**hyperfine mean + max RSS** の両方を記録する
- ネイティブビルド経路は **run / build / バイナリ実行の3段階**を確認する
- ベンチを通すための特定パターン最適化・設定緩和は禁止。
  ベンチが型推論で落ちる場合はベンチ側ではなく型推論/実装側を直す
- ベンチ内容を変えたら**全言語のベンチに同じ変更を反映**する
- ベンチ生成物（`**/bin/**`）は Git に含めない

手順は [docs/phase2/bench/README.md](docs/phase2/bench/README.md) にあります。

---

## 6. 既知の制限

`clove build`（ネイティブビルド）は言語の部分集合しか扱えません。着手前に
[docs/tooling/build.md](docs/tooling/build.md) の「既知の制限と不具合」を確認してください。
そこに挙がっている項目を踏んだ場合、それは新しい不具合ではありません。
