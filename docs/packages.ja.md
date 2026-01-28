# パッケージ（Phase1）

Phase1 は **pure Clove ライブラリ**を対象に、Git リポジトリから取得して `require` できる状態にする段階です。
native/plugin/docs 外出し/fn_meta などは扱いません。

## パッケージ ID と名前空間

- **Package ID**: `owner/pkg`
- **Namespace root**: `owner::pkg::...`
- **配置ルール**（ns → path が自明になる形）

```
src/<owner>/<pkg>/core.clv
```

例: `example::flappy::core` → `src/example/flappy/core.clv`

## install / registry / require

- install 先は `CLOVE_HOME`（未設定なら `~/.clove`）配下

```
~/.clove/pkgs/<owner>/<pkg>/<commit>/
  src/...
  clove-pkg.json
```

- `registry.json` に install 済み情報を記録
- `require` は project lock があれば lock の commit を優先し、無ければ `installed_at` が最新の install を使う

## CLI

```
clove pkg install <git-url> [--pkg owner/pkg] [--rev <ref>] [--force] [--project]
clove pkg list
clove pkg update <owner/pkg> [--rev <ref>] [--force]
clove pkg uninstall <owner/pkg> [--commit <sha>] [--all]
clove pkg sync [--update] [--offline] [--force]
```

- `--rev` は git の参照（tag / commit SHA / branch）
- `--pkg` 未指定で GitHub URL の場合は `owner/repo` を仮決定
  - repo 名と pkg 名を変えたい場合は `--pkg` を明示する
- ローカルパスの場合は `clove-pkg.json` の `pkg` から推測する（無い場合は `--pkg` が必要）
- shorthand 例
  - `clove pkg install example/flappy --rev v0.1.0`
  - `clove pkg update example/flappy --rev v0.2.0`
- project 追加（deps/lock を更新）
  - `clove pkg install example/flappy --rev v0.1.0 --project`
- `pkg uninstall`
  - `--all` は installs を全削除し、registry から pkg を削除する
  - `--commit` は指定 commit だけ削除する
  - commit 指定なしの場合は `installed_at` が最大の commit を削除する

## Phase1 の制約

- `require` は自動ダウンロードしない
  - 先に `clove pkg install ...` を実行する
- 依存ファイル（deps.clv 等）は未対応
- project lock があれば lock が優先

## Phase2: project deps/lock

プロジェクトに `clove.deps.json` と `clove.lock.json` を置くことで、
**プロジェクトごとに固定した commit を優先**して `require` 解決します。

```
// clove.deps.json
{
  "deps": {
    "example/flappy": { "origin": "example/flappy", "rev": "v0.1.0" }
  }
}
```

```
// clove.lock.json
{
  "deps": {
    "example/flappy": { "origin_url": "https://github.com/example/flappy.git", "commit": "<sha>" }
  }
}
```

- `clove pkg sync` は deps を解決し、必要なら install を行って lock を生成する
  - `--update` は rev を再解決して lock を更新する
  - `--offline` はネットワークを使わず、未インストールならエラー
  - `--force` は origin 競合や既存 lock 差分を上書きする
- `require` の優先順位
  - lock に指定された commit
  - lock が無い場合は `installed_at` が最新の commit

## パッケージ側の deps（推移依存）

パッケージの `clove-pkg.json` に `deps` を書くと、**推移依存**を宣言できます。
`clove pkg sync` は project deps から依存閉包を解決し、**閉包全体**で lock を生成します。

```
// clove-pkg.json
{
  "pkg": "inakaegg/flappy",
  "ns_root": "inakaegg::flappy",
  "deps": {
    "somebody/util": { "origin": "somebody/util", "rev": "v0.1.0" }
  }
}
```

- `pkg install --project` は **追加した pkg とその依存閉包だけ** lock を更新する
- 同じ pkg が異なる commit を要求した場合はエラー（依存衝突）

## Phase3: native plugin 同梱パッケージ

パッケージにネイティブプラグインを同梱する場合は、以下のレイアウトを前提にします。

```
repo-root/
  src/<owner>/<pkg>/...
  plugins/<platform>/
    libclove_example.(dylib|so|dll)
```

- `<platform>` は `os-arch` 形式（例: `macos-aarch64`, `macos-x86_64`, `linux-x86_64`, `windows-x86_64`）
- 互換用として `plugins/` 直下も探索対象になる

`clove.lock.json` がある場合、**lock で参照されたパッケージの plugins ディレクトリが自動で探索パスに追加**されます。
ただし pkg 配下のネイティブプラグインは、lock に記録された `native_plugins` の sha256 一致が必須です。
`--allow-native-plugins` を付けると従来通り全許可になります。手動の `--plugin-dir` 指定も引き続き利用できます。

### meta.json（型情報）

ネイティブプラグインは `*.meta.json` を同梱し、**dylib/so/dll と同じディレクトリ**に配置します。
meta は `clove plugin write-meta` で自動生成でき、`clove pkg install` の過程でも
meta 未同梱時にフォールバック生成されます。

- 例: `plugins/<platform>/clove-sdl2.meta.json`
- LSP は `clove.lock.json` から plugin dirs を自動解決して meta を読みます
- 上書き指定は `pluginDirs` / `CLOVE_PLUGIN_DIR` で可能（従来の挙動を維持）
- プラグイン作者は `clove-plugin-sdk` だけを依存し、Rust 側の関数に `#[clove_fn]` を付けるだけでOK

---
<!-- NAV:START -->
**前へ:** [CLI（clove / clove fmt / clove build）](tooling/cli.ja.md)
**次へ:** [Formatter（fmt / rubocop/syntax_tree）](tooling/formatter.ja.md)
<!-- NAV:END -->

