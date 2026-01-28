# CLI ツール (`clove` / `clove fmt` / `clove build`)

ここでは Clove のコマンドラインツールについて説明します。

* `clove` … REPL / スクリプト実行 / ワンライナー評価
* `clove fmt` … フォーマッタ
* `clove build` … ネイティブバイナリビルド

> ⚠️ オプションの細かい仕様は `clove --help` / `clove build --help` に従ってください。
> ここでは概念とよく使うパターンだけを説明します。

---

## 1. `clove` – REPL / スクリプト / ワンライナー

### 1.1 REPL

```bash
clove --repl
# または単に
clove
```

* 対話的に式を評価できます。
* エラー位置や `:source` など、REPL 専用機能もいくつかあります。

代表的な REPL コマンド:

```clojure
:help           ; REPL ヘルプ
:doc map        ; シンボルの doc 表示
:doc ns::name    ; 名前空間付きシンボルの doc
:quit / :q      ; 終了
```

### 1.2 スクリプト実行

```bash
clove path/to/script.clv
```

* `ns` の宣言に従ってファイルを読み込み、最後まで評価します。
* `(-main)` 関数があれば、それを入口として呼び出すスタイルがおすすめです。

```clojure
(ns myapp::main)

(defn -main []
  (println "hello")
  (cli::exit 0))
```

### 1.3 ワンライナー評価 (`-e`)

```bash
clove -e '(+ 1 2 3)'
# => 6
```

* 与えた式を 1 つだけ評価して結果を表示します。
* 簡単な確認やシェルスクリプトからの利用に便利です。

---

### 1.4 ネイティブプラグイン (`require-native`)

ネイティブプラグインは **任意パスからのロードをデフォルト拒否** します。  
ただし以下の信頼ディレクトリは自動許可されます。

* `<project>/plugins/`
* `<project>/.clove/plugins/`
* `~/.clove/plugins/`
* `~/.clove/pkgs/**/plugins/**`（`clove.lock.json` に記録された sha256 一致が必須）

`--allow-native-plugins` を付けると従来通り **すべて許可** になります。

```bash
# プロジェクト配下の plugins を使う場合（フラグ不要）
clove --main examples/sdl2/01_events_input.clv

# 任意パスから読みたい場合（全許可）
clove --allow-native-plugins --plugin-dir ./plugins --main examples/sdl2/01_events_input.clv
```

* `--plugin-dir` は **相対パス** の場合に基準が変わります。  
  - どちらも起動時のカレントディレクトリを基準に解釈します。
* 探索先は **`--plugin-dir`（指定時）/ `CLOVE_PLUGIN_DIR` / 実行バイナリ隣の `plugins/` / `<project>` / `~/.clove`** が基本です。
  - `clove.lock.json` がある場合は、依存パッケージの `plugins/<platform>/` も自動で追加されます（pkg 配下は sha256 一致が必須）。
  - `CLOVE_PLUGIN_DIR` は OS のパス区切り（mac/Linux: `:`、Windows: `;`）で複数指定できます。
* `clove` を PATH から実行して `--plugin-dir` を省略すると、`~/.cargo/bin/plugins` が探索対象になります。
  - このパスは信頼ディレクトリではないため、必要なら `--allow-native-plugins` を付けてください。

SDL2 プラグインの例（外部パッケージ）:

```bash
clove pkg install --project <git-url> --pkg owner/clove-sdl2
clove --main examples/sdl2/01_events_input.clv
```

パッケージ側は `repo-root/plugins/<platform>/` に cdylib を同梱する前提です。

ファイル名の規約:

* macOS: `lib<name>.dylib` / `lib<name>_plugin.dylib`
* Linux: `lib<name>.so` / `lib<name>_plugin.so`
* Windows: `<name>.dll` / `<name>_plugin.dll`

Clove 側の呼び出しは以下のようになります。

```clojure
(require-native "clove-sdl2")
(require clove-sdl2)
```

SDL2 追加プラグインの例:

```clojure
(require-native "clove-sdl2-image")
(require clove-sdl2-image)

(require-native "clove-sdl2-ttf")
(require clove-sdl2-ttf)

(require-native "clove-sdl2-mixer")
(require clove-sdl2-mixer)
```

> 注意: SDL_mixer を使う場合は Core の queue-audio と併用しないでください。

> 補足: SDL2 は外部パッケージとして提供します。  
> `clove pkg install --project ...` で取得し、`clove.lock.json` に記録された sha256 が一致すればフラグなしで動きます。
> それ以外の場所から読みたい場合は `--allow-native-plugins` を付けてください。

### 1.5 SDL2 ゲームの配布（コード / バイナリ）

#### コード配布（.clv とプラグインを配る）

* ユーザーは `clove` をインストールしておき、配布物の `.clv` を実行します。
* 付属のプラグイン（`libclove_sdl2_plugin.*` など）を `<project>/plugins/` に同梱すればフラグ不要で実行できます。
  任意パスに置く場合は `--allow-native-plugins` を付けて実行します。

```bash
clove --plugin-dir ./plugins --main path/to/game.clv
```

#### バイナリ配布（`clove build` で生成）

* `clove build` で作ったバイナリは、`--allow-native-plugins` と `--plugin-dir` を受け取ります。
* バイナリと同じ階層の `plugins/` は探索対象ですが、信頼ディレクトリではないためデフォルトでは拒否されます。

```
mygame
plugins/
  libclove_sdl2_plugin.dylib
```

```bash
./mygame --allow-native-plugins --plugin-dir ./plugins
```

> 注意: SDL2 の実行時ライブラリが環境に必要です。配布先の環境で SDL2 が見えるようにしてください。

---

## 2. `clove fmt` – フォーマッタ

Clove コードを整形するためのサブコマンドです。

```bash
# ファイルを直接整形（上書き）
clove fmt src/main.clv

# stdin から読み込み、stdout に出力
clove fmt --stdin < src/main.clv

# インデント幅を指定
clove fmt --stdin --indent 4 < src/main.clv

# 行幅（1 行の最大文字数）を指定
clove fmt --stdin --width 100 < src/main.clv
```

インデント幅は `--indent`（デフォルト 2）、1 行の最大文字数は `--width`（デフォルト 120）で調整できます。

VS Code からは「Format Document」で `clove fmt` が呼ばれるように設定できます。
（詳細は [VS Code 拡張](vscode.ja.md) を参照）

---

## 3. `clove build` – ネイティブバイナリ生成

`clove build` は Clove コードからネイティブバイナリを生成するコマンドです。

```bash
clove build path/to/main.clv
```

大まかな流れ:

1. Clove ソースを読み込む
2. 中間表現 / LUT などを生成
3. Rust 側のランタイムとリンクしたバイナリを出力

**よくあるオプション例**（実装に応じて変わる可能性があります）:

* 出力ファイル名指定: `--output ./target/app`
* 静的リンク関連: `--static` など
* Ruby/Python 埋め込み: `--embed-ruby`, `--embed-python` など
* 最適化レベル: `--release` 的なフラグ（あれば）

正確なオプション一覧は:

```bash
clove build --help
```

で確認してください。

---

## 4. CLI と VS Code / REPL の連携

* VS Code 拡張は、内部的に `clove` / `clove fmt` を呼び出します。
* REPL とのやりとりも、統一して「ターミナル上の `clove --repl`」を前提にしています。

推奨ワークフロー:

1. まず VS Code で Clove 用のワークスペースを開く
2. `Clove: Send Selection to REPL` で対話的に遊ぶ
3. `clove fmt` でフォーマットを維持
4. 最後に `clove build` で配布用バイナリを作る

---

## 5. 今後の拡張候補

* `clove doc` … [docs/](/docs/) を元に HTML を生成するツール
* `clove new` … テンプレートからプロジェクトを生成
* `clove test` … テスト実行用コマンド

など、CLI 側のサポートコマンドも拡張余地があります。
まずは現状の `clove` / `clove fmt` / `clove build` を前提に設計しています。

---

## 重要: `clove fmt` は stdout 出力

現状の `clove fmt` は **ファイルを上書きしません**。

```bash
clove fmt foo.clv > foo.clv.new
mv foo.clv.new foo.clv
```

## 重要: ネイティブプラグイン

`clove build` の生成物や実行環境でネイティブプラグインを読み込む場合、
デフォルトは **信頼ディレクトリのみ許可** です。

- `<project>/plugins/` / `<project>/.clove/plugins/` / `~/.clove/plugins/` は自動許可
- `~/.clove/pkgs/**/plugins/**` は `clove.lock.json` の sha256 一致が必須
- それ以外のパスから読みたい場合は `--allow-native-plugins` を付ける

> どこからロードするか（同梱 plugins だけ等）は安全設計上とても重要です。

---
<!-- NAV:START -->
**前へ:** [標準ライブラリ `std` の使い方](../language/stdlib.ja.md)
**次へ:** [パッケージ管理（Phase1）](../packages.ja.md)
<!-- NAV:END -->

