# ドキュメントの書き方

- 更新日: 2025-12-21

## 1. 基本

- 1 ページ 1 テーマ
- 先頭に “何が学べるか” を 2〜3 行で書く
- 実装済み/未実装を明確に分ける

## 2. 例の書き方

- REPL でコピペできる
- 余計な前提（別ファイル）に依存しない
- OOP examples は [data/clove_docs/clove-docs.json](/data/clove_docs/clove-docs.json) の examples から自動生成される

## 3. 記法

- コードブロックは `clojure`（ハイライト用）を使用
- 重要な注意は `> NOTE:` などの引用で強調

## 4. ナビゲーション

- `docs/index.md` に載るページは前/次ナビを必ず付ける
- 並び順は `docs/index.md`（日本語は `docs/index.ja.md`）に従う
- 同言語のページを辿れるようにする（英語は `.md`、日本語は `.ja.md`）

---
<!-- NAV:START -->
**前へ:** [テスト方針](testing.ja.md)
**次へ:** [FAQ](../faq.ja.md)
<!-- NAV:END -->

