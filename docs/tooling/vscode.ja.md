# VS Code 拡張

このドキュメントでは、[packages/vscode-clove](/packages/vscode-clove) に含まれる
VS Code 拡張の機能と使い方をまとめます。

* シンタックスハイライト
* 構造的な S 式選択（expand / shrink）
* REPL 連携
* `clove fmt` 統合
* 埋め込み Ruby / Python サポート
* Light Table 風テーマ

拡張そのものの README は [packages/vscode-clove/README.md](/packages/vscode-clove/README.md) も参照してください。

---

## 1. 言語登録

* 言語 ID: `clove`
* 対象拡張子:

  * `.clv`

`language-configuration.json` / `clove.tmLanguage.json` により

* コメント: `;`
* 括弧ペア: `()`, `[]`, `{}`
* 文字列: `"` … `"`
* シンタックスハイライト

などの設定が行われます。

---

## 2. 構造的 S 式選択

### 2.1 コマンド

* `Clove: Expand Selection` (`clove.expandSelection`)
* `Clove: Shrink Selection` (`clove.shrinkSelection`)

デフォルトキー（例）:

* Expand: `Shift+Alt+Right`
* Shrink: `Shift+Alt+Left`

> `when editorLangId == "clove"` のときだけ有効になるように設定します。

### 2.2 挙動

* シンボル → フォーム内 → リスト全体 → 外側のリスト … という順で拡大
* 末尾の括弧 `)` の直前にカーソルがある場合でも「いい感じ」に動くよう調整
* `$rb{...}`, `$py{...}`, `${...}`（Ruby の省略）などの「埋め込みフォーム」も認識し、

  * 可能であれば Tree-sitter Ruby / Python を使って中身を構造的に扱います
  * パーサがロードできない場合は S 式ベースのフォールバック挙動になります

デバッグ用に `Clove: Show Selection Debug Log` を用意しており、
内部のノード情報や選択レンジの変遷をログとして確認できます。

---

## 3. REPL 連携

### 3.1 コマンド

* `Clove: Send Selection to REPL` (`clove.sendSelection`)

### 3.2 挙動

1. アクティブな Clove ドキュメントから、選択中のテキストまたは
   カーソル位置のフォームを抽出。
2. 専用ターミナル（名前例: `Clove REPL`）を探す。なければ新規作成:

   * `shellPath`: `clove`
   * `shellArgs`: `["--repl"]`
3. 抽出したコードを「 bracketed paste 」で REPL に流し込み、評価。

ファイル保存済みの場合は、REPL 側に `:source` などの補助情報を送って
エラー位置をファイル名付きで表示できるようにしています。

---

## 4. フォーマッタ統合 (`clove fmt`)

VS Code のフォーマット機能と `clove fmt` を連携させています。

* 「Format Document」実行時:

  1. ドキュメントの中身を取得
  2. 設定されたコマンド（デフォルト: `clove`）に
     `fmt --stdin --indent <indentWidth>` を渡す
  3. 標準出力に返ってきたコードで置き換える

拡張設定例:

* `clove.format.command` … フォーマットに使うコマンド (`clove` / 絶対パスなど)
* `clove.format.indentWidth` … 幅のヒント（デフォルト 2）

---

## 5. 埋め込み Ruby / Python サポート

### 5.1 選択の扱い

以下のようなフォームを「埋め込みコード」と認識します。

* `$rb{ ... }` / `${ ... }` … Ruby として扱う（`${ ... }` は省略記法）
* `$py{ ... }` … Python として扱う

構造的選択では:

* まず Clove 側で埋め込みブロックの範囲を認識
* その内部については Tree-sitter Ruby / Python を使って
  できるだけ自然な単位（式 / 文）で expand / shrink を行う

### 5.2 Ruby 補完

Ruby 向けには、埋め込みブロック内で Ruby の補完を呼び出す機能を用意しています。

* 必要条件:

  * VS Code に Ruby 言語サーバ系の拡張が入っていること
* 仕組み:

  1. 埋め込み Ruby ブロックの内容を一時的に「仮想 Ruby ファイル」として扱う
  2. Ruby 言語サーバに補完をリクエスト
  3. 結果を Clove ドキュメント上の位置にマッピングして挿入

Python については現時点では「選択の構造化」のみ対応で、
言語サーバ連携は今後の拡張候補です。

---

## 6. Light Table 風テーマ

拡張には Light Table 風のダークテーマが含まれています。

* テーマ名例: `Clove Default (LightTable)`

特徴:

* 暗い背景
* 括弧のネストごとに色を変える
* シンボルと括弧のコントラストを高め、S 式の構造が追いやすい配色

### 6.1 Dark Modern を維持しつつ Clove 配色だけ反映する

**Default Dark Modern（ダークモダン）の UI のまま**、Clove だけ
Light Table 風の配色（括弧レインボー含む）を適用するには、
`settings.json` に以下を追加します。

```json
"editor.tokenColorCustomizations": {
  "[Default Dark Modern]": {
    "textMateRules": [
      {
        "scope": [
          "source.clove punctuation.section.list.begin.clove",
          "source.clove punctuation.section.list.end.clove",
          "source.clove meta.brace",
          "source.clove meta.bracket"
        ],
        "settings": { "foreground": "#c4c4c4" }
      },
      {
        "scope": [
          "source.clove variable.other.clove",
          "source.clove variable.parameter.clove",
          "source.clove meta.symbol.clove"
        ],
        "settings": { "foreground": "#e2e2e2" }
      },
      {
        "scope": [
          "source.clove string.quoted.double.clove",
          "source.clove string.quoted.other.clove"
        ],
        "settings": { "foreground": "#7ee8e8" }
      },
      {
        "scope": [
          "source.clove constant.numeric.clove"
        ],
        "settings": { "foreground": "#e0e0e0" }
      },
      {
        "scope": [
          "source.clove entity.name.function.clove",
          "source.clove meta.definition.clove"
        ],
        "settings": { "foreground": "#8fc1ff" }
      },
      {
        "scope": [
          "source.clove support.function.clove",
          "source.clove support.variable.clove",
          "source.clove keyword.control.clove",
          "source.clove storage.type.clove"
        ],
        "settings": { "foreground": "#7ff0c4" }
      },
      {
        "scope": [
          "source.clove entity.name.type.clove",
          "source.clove entity.name.type",
          "source.clove storage.type",
          "source.clove support.type.clove",
          "source.clove support.class.clove"
        ],
        "settings": { "foreground": "#4F92FF" }
      },
      {
        "scope": [
          "source.clove constant.language.clove"
        ],
        "settings": { "foreground": "#d6a2ff" }
      },
      {
        "scope": [
          "source.clove comment.line.semicolon.clove",
          "source.clove comment.block.clove"
        ],
        "settings": { "foreground": "#7fa9cc" }
      },
      {
        "scope": [
          "source.clove punctuation.section.embedded.begin.clove",
          "source.clove punctuation.section.embedded.end.clove"
        ],
        "settings": { "foreground": "#c4c4c4" }
      }
    ]
  }
},
"workbench.colorCustomizations": {
  "[Default Dark Modern]": {
    "editorBracketHighlight.foreground1": "#cfcfcf",
    "editorBracketHighlight.foreground2": "#00ff4d",
    "editorBracketHighlight.foreground3": "#00ffa0",
    "editorBracketHighlight.foreground4": "#00ffff",
    "editorBracketHighlight.foreground5": "#9c7fff",
    "editorBracketHighlight.foreground6": "#ff33ff",
    "editorBracketHighlight.unexpectedBracket.foreground": "#ff6666",
    "editorBracketMatch.border": "#40ffff",
    "editorBracketMatch.background": "#1b1b1b"
  }
}
```

補足:

* 括弧の色はエディタ全体に効くため、**全言語共通**で変わります。
  Clove だけに限定したい場合は `workbench.colorCustomizations` を外してください。
* 型色を変えたい場合は `#4F92FF` を差し替えてください。

---

## 7. トラブルシューティング

* 構造的選択がおかしい:

  * `Clove: Show Selection Debug Log` を実行し、ログを確認
  * 小さな再現コードと一緒に issue に貼る
* フォーマットが失敗する:

  * `clove fmt --stdin` を別途ターミナルで試し、エラー内容を確認
  * `clove.format.command` のパス設定を確認

---

## 8. 今後の拡張候補

* LSP サーバ（補完 / go-to-definition / hover など）の統合
* `clove build` / `clove test` などのタスクランナー連携
* 埋め込み Python への言語サーバ接続
* REPL との変数同期や「現在値のインライン表示」

など、エディタ連携はまだまだ伸びしろがあります。

---
<!-- NAV:START -->
**前へ:** [LSP（clove-lsp）](lsp.ja.md)
**次へ:** [トラブルシュート](troubleshooting.ja.md)
<!-- NAV:END -->

