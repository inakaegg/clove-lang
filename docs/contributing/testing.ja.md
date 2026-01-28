# テスト方針

- 更新日: 2025-12-21

## 1. Rust のテスト

```bash
cargo test
```

- Reader
- Eval
- 型推論（HM）
- fmt

## 2. 例（examples）を “動作テスト” にする案

- 例ファイルを走査して `eval_file` する
- 期待値を返す関数を用意する

この方針は repo の運用で決め、CI に組み込むと壊れにくくなります。

---
<!-- NAV:START -->
**前へ:** [リポジトリ構成](repo_layout.ja.md)
**次へ:** [ドキュメントの書き方](docs_style.ja.md)
<!-- NAV:END -->

