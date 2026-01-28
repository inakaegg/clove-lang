# Build（`clove build`）

- 更新日: 2025-12-21

`clove build` は Clove ファイルから **Rust プロジェクト** を生成します。

## 1. 使い方

```bash
clove build path/to/app.clv
```

生成されたディレクトリ（例: `build/`）で

```bash
cargo build --release
```

を実行すると実行可能ファイルができます。

## 2. `--opt=typed`

`--opt=typed` は typed IR を使って “より静的なコード生成” を行うモードです。

- 現段階では “実験的” として扱い、
  `hm` の互換性警告が出ることがあります。

## 3. 静的リンク / 埋め込み

- `--static` は Linux musl 前提の静的リンクを作るためのオプションです。
- `--embed-ruby` / `--embed-python` は foreign engine を同梱するためのオプションです。

> 制約:
> - 現状 `--embed-*` は `--static` を要求します。
> - foreign blocks があるスクリプトを “非 embed” の static build にするとエラーになります。

## 4. ネイティブプラグイン

生成物は `--allow-native-plugins` / `--plugin-dir` を受け取ります。
デフォルトは信頼ディレクトリのみ許可で、pkg 配下は lock の sha256 一致が必須です。
（同梱 plugins だけに固定するなど、運用設計が重要です。）

## 5. よくある落とし穴

- `--main` の有無で `-main` を呼ぶ/呼ばないが変わる
- foreign blocks を含むときは embed か実行環境の engine 設定が必要

---
<!-- NAV:START -->
**前へ:** [Formatter（fmt / rubocop/syntax_tree）](formatter.ja.md)
**次へ:** [LSP（clove-lsp）](lsp.ja.md)
<!-- NAV:END -->

