# AGENTS.md — このリポジトリでの作業ルール

Clove の開発は AI コーディングエージェントを併用して進めています。
このファイルは、エージェント（および人間）がこのリポジトリで作業するときの
共通ルールです。

---

## 1. 進め方

### 1.1 仕様はリポジトリに残す

- 公開してよい仕様・設計判断は `docs/` に書く。チャットログに残さない
- 「なぜそうなっているか」は [docs/design-notes/](docs/design-notes/README.ja.md) に書く
- 未確定の内部メモ・作業途中の記録は `_ai/`（Git管理外）へ置く
- 実装前に、期待する挙動を先に文書へ固定する

### 1.2 テストでゴールを固定する

- バグ修正・仕様追加は原則 **テスト追加・更新 → 実装修正** の順で進める
- 変更前にテストが落ちることを確認してから実装する
- 自動テストを書けない場合は理由と代替の検証手順を示す

### 1.3 検証ゲート

変更をコミットする前に、変更種別に応じて実行する。

```bash
cargo test --workspace          # 全体（基準値: 1096 passed / 0 failed @ 2026-07-25）
cargo test -p clove-core        # インタプリタのみ
cargo test -p clove-build-core  # ネイティブビルド経路のみ
cargo fmt --check
cargo clippy --workspace
```

テストが落ちている状態でコミットしない。

### 1.4 局所最適化を避ける

- 1ケースだけを通す場当たりのヒューリスティクスを入れない
- 変更は一般化されたロジックか、文書化された例外に限る
- 性能改善を主張する変更には Before/After の計測を残す

---

## 2. Clove 固有の注意

### 2.1 関数・Special Form を追加するとき

Clove は REPL 補完・LSP・ドキュメントが実装と連動しています。片方だけ直すと
「実装したのに補完に出ない」「doc が引けない」が起きます。

**関数・alias を追加/改名したとき**は、REPL と LSP の補完候補に出ること、
`doc` と examples が引けることを確認する。

**Special Form を追加したとき**は次の5箇所を同時に更新する。
どれかを落とすと `__apply` に巻かれて未定義扱いになります。

1. `crates/clove-core/src/eval.rs` — special form dispatch
2. `crates/clove-core/src/compiler.rs` — `is_special_form`
3. `crates/clove-core/src/typing/infer.rs` — 型推論での特別扱い
4. `data/clove_docs/clove-docs.json` — ドキュメント
5. REPL 補完 / ヘルプ

### 2.2 2つのリーダーを揃える

Clove はインタプリタ（`clove-core`）とネイティブビルド（`clove-build-core`）で
**リーダーが別実装**です。片方だけ直すと仕様がずれます。

ネイティブ側はインタプリタ側の**部分集合**であることが設計です。逆方向
（ネイティブが受理してインタプリタが拒否する）は言語にない構文を実装している
ことを意味します。これは `crates/clove-lang/tests/reader_parity.rs` が検査します。

構文を追加・変更したら、このテストのコーパスも更新してください。
背景は [2経路の設計ノート](docs/design-notes/two-phase-implementation.ja.md) にあります。

### 2.3 コードは fmt する

Clove コードは作成・修正時に `clove fmt` を通す。整形結果がおかしければ
フォーマッタ側の不具合として直す。

### 2.4 ベンチマーク

- Rust で約1秒になるよう負荷を調整し、**hyperfine mean + max RSS** を記録する
- clove2 は **run / build / バイナリ実行の3段階**を確認する
- ベンチ合格のための特定パターン最適化は禁止
- ベンチ生成物（`**/bin/**`）は Git に含めない

---

## 3. Git 運用

- 1 commit 1目的。差分は小さく保つ
- コミットメッセージは `カテゴリ: 日本語 / English` の1行形式
  （`fix:` `feat:` `docs:` `test:` `refactor:` `perf:` `chore:`）
- 機能開発は `feature branch → PR → CI確認 → Squash merge`
- `main` へ直接 push しない
- 環境依存の絶対パス（ホームディレクトリ配下など）をコミットしない
- API キー・トークン・個人情報をコミットしない

---

## 4. ドキュメント

- 本文は日本語を正とし、英語版（`*.md`）と日本語版（`*.ja.md`）の両方を更新する
- `CHANGELOG.md` は英語
- 仕様と実装が食い違ったら、どちらが正しいかを決めてから直す。
  ドキュメントだけを実装に合わせて書き換えない
- 大量のログや全件一覧は、結論・方法・件数・代表例・再現手順へ要約する

書き方の詳細は [docs/contributing/docs_style.md](docs/contributing/docs_style.md) にあります。

---

## 5. 完了報告

変更作業の完了時は次を報告する。

- 何を直した・追加したか
- 仕様文書の変更点、または変更不要と判断した根拠
- 追加・更新したテスト
- 再現・確認コマンドと**その結果**
- 未確認範囲

「動作確認手順を示さずに確認済みと報告する」ことはしない。
