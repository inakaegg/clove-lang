# トラブルシューティング

English documentation: [troubleshooting.md](troubleshooting.md)

よくある問題と簡単な対処をまとめます。

## 1. REPL / 評価の問題

- **名前空間不一致の警告**
  - `ns` とファイルパスが一致しないと REPL/LSP で警告が出ます。
  - `myapp/core.clv` と `(ns myapp::core)` を揃えてください。

- **`:source` がずれる**
  - エラー位置がずれる場合は REPL で `:source` を設定するか、
    VS Code の「Send Selection to REPL」を使ってください。

## 2. `clove fmt` の問題

- **フォーマッタが失敗する**
  - ターミナルで `clove fmt --stdin` を試し、エラー内容を確認。
  - VS Code 設定のコマンドパスを確認してください。

- **設定が効かない**
  - `clovefmt.toml` の探索開始ディレクトリを確認。
  - `--print-config` で解決済み設定を確認できます。

## 3. LSP / エディタの問題

- **定義ジャンプが動かない**
  - 名前空間とファイル配置を合わせ、`aaa/bbb` 形式は避けます。
  - `::` 区切りに統一してください。

- **補完が出ない**
  - `clove-lsp` がビルド済みで PATH にあるか確認してください。

## 4. ネイティブプラグインの問題

- **プラグインロードが拒否される**
  - 既定では信頼ディレクトリのみ許可です。
  - 任意パスから読む場合は `--allow-native-plugins` を使います。

- **プラグインが見つかるが動かない**
  - OS に合った拡張子（`.dylib` / `.so` / `.dll`）を確認。
  - 実行時ライブラリ（例: SDL2）の導入を確認してください。

## 5. 配布バイナリの問題

- **`Runtime error: unknown foreign tag: rb`（または `py`）が出る**
  - リリースに添付しているバイナリは Ruby / Python の埋め込みなしでビルドしているため、
    `$rb{ ... }` と `$py{ ... }` は実行できません。埋め込みを外した理由は
    [設計ノート](../design-notes/prebuilt-binaries.ja.md)にあります。
  - 埋め込みが必要ならソースからインストールしてください。
    `cargo install --git https://github.com/inakaegg/clove-lang --locked clove-lang`

- **Apple Silicon でプラグインの dylib が署名不正として拒否される**
  - arm64 の macOS は署名のない dylib を読み込みません。`cargo` や `cc` はビルド時に
    ad-hoc 署名を付けるので、アーカイブから取り出した、あるいは署名を落としたなどで
    署名が失われた dylib が原因のことが多いです。`codesign -s - <dylib>` で署名し直してください。

---
<!-- NAV:START -->
**前へ:** [VS Code 拡張](vscode.ja.md)
**次へ:** [名前空間設計ノート](../advanced/namespaces_design.ja.md)
<!-- NAV:END -->

