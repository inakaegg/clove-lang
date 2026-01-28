# Phase2 作業引き継ぎ（入口）

英語版（公式）: `README.md`  
※ 日本語版は `README.ja.md` にまとめています。

このディレクトリは **Phase2 の仕様・進捗・意思決定**を一箇所に集約するための入口です。  
ターミナルの Codex にはここだけ見せれば迷わない、という前提で整理しています。

---

## 1. Clove とは？

- Clojure 系の S 式言語（Lisp 方言）
- REPL / スクリプト実行と、ネイティブビルド（高速実行）を両立したい言語
- 実装は Rust を前提にしており、最終的には Rust ネイティブ化を目標にしている

---

## 2. Phase1 とは？

- 既存実装の世代（現在の Clove）
- 仕様追加を積み重ねてきた結果、パフォーマンス・メモリ利用が厳しくなっている
- 動的機能を広く許容し、REPL とスクリプト用途に強い反面、最適化が難しい
- 主要コード: [crates/clove-core](/crates/clove-core), [crates/clove-lang](/crates/clove-lang), [crates/clove-lsp](/crates/clove-lsp)

---

## 3. Phase2 とは？

- **再設計・再実装**の世代（Native-first を最優先）
- 型情報を前提に Rust ネイティブ化を行う
- Dynamic 機能は「開発時のみ」に限定し、ビルド時は静的に解決する
- `mut` をデフォルト（build時）。`imut` は観測不変、`mut` は in-place 必須（共有があればエラー）
- 主要コード: [crates/clove2-core](/crates/clove2-core), [crates/clove2-lang](/crates/clove2-lang), [crates/clove2-lsp](/crates/clove2-lsp)

---

## 4. Phase1 との違い（要点）

- Native-first（Rust 化）を最優先し、Dynamic は縮退
- `Any` を極力排除し、`Dyn` は **境界に限定**して使う
- `mut/imut` の文脈で **同一関数が最適化の可否を変える**（mut は in-place 必須）
- Native ビルドでは **動的評価/ロードや再定義を禁止**する
- `def-foreign` を必須化して、外部言語との境界を明示
- strict / warn / allow によるビルドの厳格度を導入
- LSP の診断/補完/型表示を前提に設計

---

## 5. 現在の進捗（概要）

- `clove2` の `run`/`build`/`check` が動作
- `--native` / `--mode` / `--mut` を CLI から上書き可能
- `time` / `bench` を eval に追加（戻り型は関数の戻り値に追随）
- LSP: 診断 + 補完 + hover + 定義ジャンプ + シンボル一覧（トップレベル）

詳細な進捗管理は内部向け資料で管理（公開版では省略）。

---

## 6. 公開版の構成

- 公開用の情報は **README のみ**に集約
- ベンチは [docs/phase2/bench/](/docs/phase2/bench/) にまとめた 1 本のみを公開

---

## 7. 主要コマンド

```bash
# ビルド
cargo install --path crates/clove2-lang

# 実行
clove2 run path/to/a.clv

# ビルド（ネイティブ）
clove2 build path/to/a.clv --out target/clove2/bin/a

# 型チェック（strict/warn/allow 切替可）
clove2 check path/to/a.clv --native=strict

# LSP
cargo install --path crates/clove2-lsp
```

補足:

- clove2 の **デフォルトは strict**（CLI/LSP ともに同じ）
- `use native :warn` / `:allow` または `--native=warn` / `--native=allow` で緩められる

---

## 7.1 モード整理（run / REPL / build）

### コマンド別の前提（簡略運用）

ここでは **通常運用での前提**を先に固定する。  
`--mode` は各コマンドで暗黙に決まっているものとし、**指定しない**。

| コマンド | mode | native | 備考 |
| --- | --- | --- | --- |
| build | native | strict | 既定は strict のみ |
| run | native | strict / warn | warn は移行期の妥協用 |
| repl | dynamic | warn / allow | strict は使わない |

結論: **実運用で使うオプションは run の `--native=warn` だけで十分**（strict/ warn の切替のみ）。

### 例外的に詳細指定する場合

どうしても必要な場合のみ `--mode/--native/--mut` を個別指定する。  
ただし以下の制約を守ること。

- `mode=native` のとき `native=allow` は **不可**
- `mode=dynamic` のとき `native=strict` は **不可**

### `--mut`

- `soft` : 共有がある場合はエラー（mut の基本）
- `hard` : 共有があっても破壊的更新（現状は設計のみ、詳細は DECISIONS 参照）

### run と REPL の違い

- **run**: clove2 専用 VM で実行（typed opcode 優先 + dynamic fallback）
  - `--mode/--native/--mut` がそのまま効く
  - 速度重視（Ruby 以上を目標）
- **REPL**: dynamic を優先（融通重視）
  - typed opcode は「最適化できる部分のみ」適用
  - 動的機能は常に許容

### コマンド別の挙動例（同一コード）

サンプル:

```
(def m {:a 1})
(def n (get m :b)) ; nil が返る
(println (inc n))
```

| コマンド | 期待挙動 |
| --- | --- |
| build（strict） | **型エラー**（`inc` は Number のみ、`n` は Nil を含むため） |
| run（strict） | **型エラー**（同上） |
| run（warn） | **警告**（実行は可能だが、実行時エラーの可能性） |
| repl（allow） | **型エラーなし**で実行、`inc nil` により **実行時エラー** |

動的機能の例:

```
(eval "(+ 1 2)")
```

- `mode=native` : **エラー**（dynamic機能禁止）
- `mode=dynamic` : **実行可能**

---

## 8. 作業ルール（抜粋）

- 詳細ルールはリポジトリ直下の `AGENTS.md` を参照
