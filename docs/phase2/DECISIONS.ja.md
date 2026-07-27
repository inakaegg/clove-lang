# Phase2 の決定事項

English version: [DECISIONS.md](DECISIONS.md)

- 更新日: 2026-07-25

ネイティブビルド経路（Phase2）を設計・実装する過程で決めたことの記録です。
「なぜ2つの実装を持つのか」という上位の理由は
[設計ノート](../design-notes/two-phase-implementation.ja.md) にあります。ここには
そのうえで決めた**個別の仕様**を残します。

> **決定と実装は別です。** 以下には、決定済みだが `clove build` に未実装のものが
> 含まれます。現時点で実際に何がビルドできるかは
> [既知の制限と不具合](../tooling/build.ja.md#4-既知の制限と不具合) を参照してください。

---

## 評価: コレクションは eager（Vec のみ）

- `cons` / `vec` / `into` は現時点で **Vec のみ**対応する（Seq / Map / Str への拡張は後段）
- `repeat` / `repeatedly` は **有限のみ**（`count` 必須）で Vec を返す
- `take` / `take-while` は `range` / `repeat` / `repeatedly` / `iterate` の
  **ジェネレータフォーム**を eager に処理する
- `iterate` は2引数時に暫定で 1024 要素を eager 生成する（`count` 指定を推奨）

遅延シーケンスはネイティブ経路では未対応です。

### 将来: IO 系の LazySeq

Phase2 は全体を eager で進めますが、`io::line-seq` のように巨大入力に効く IO 系は
LazySeq を導入する方針です。Rust の Iterator を基礎にし、リソース解放と評価タイミングの
設計を別タスクとして詰めます。

---

## `mut` / `imut` と in-place

言語としての既定は永続データ構造です（[設計ノート](../design-notes/mutability.ja.md)）。
ネイティブビルドでは、この2つを**最適化の契約**として扱います。

- `imut` は観測可能な意味で不変
  - 共有がないと証明できる場合のみ、内部的に in-place 最適化してよい
  - 共有がある、または証明できない場合は必ず新しい値を返す
- `mut` は **in-place を必須**とする
  - 共有がある、または証明できない場合は**ビルドエラー**にする
  - 黙って新しい値を返すことはしない
- `mut` / `imut` の境界はネイティブビルドでは**実行時に切り替えられない**
- 共有を断つ明示的な手段を用意する
  - 新しいコンテナを確保し、要素は浅くコピーする
  - `vec` / `into` / `map` は新しい値を返すが、共有がないことは保証しない。両者を区別する

**未実装。** 現在の C バックエンドは `mut` 自体を受け付けません
（`unsupported call in phase2 C build: mut`）。

---

## ネイティブビルドの動的機能制限

ビルド時に全体が確定していることを前提にするため、以下を禁止します。

- 動的評価・動的ロード: `eval` / `load-string` / `load-file` / `read-string`
- 実行時の再定義・動的 Var: `set!` / `redef` / `with-redefs` / `with-dyn`
- 実行時の名前空間操作: `current-ns` / `create-ns` / `resolve` など
- `require` / `require-native` は**ビルド時に静的解決**する
- ネイティブ成果物はソース同梱や `eval` 実行を行わない

REPL とスクリプト実行（Dynamic）は従来どおり許可します。

### 文字列パースの代替

`read-string` はネイティブでは使いません。代替は既存の型変換関数
（`int` / `float` / `str` / `bool`。文字列からの変換を含む）で行います。

---

## 述語の意味

- `coll?` は `Vec` / `Map` / `Str` を対象とする
- `sequential?` は `Vec` のみを対象とする

---

## 型に関する暫定決定

### 高階関数ビルダーの戻り値

`constantly` / `partial` / `comp` / `juxt` の戻り値は **`Any` を含む関数型**として扱います。
strict モードでは「`Any` を含む」と報告されるため、ジェネリクス導入後に再設計します。

### `nil` を返し得る関数

関数仕様として `nil` があり得る場合は **`T?`** として扱います。
Map / Path 系（`get` / `get-in` / `update` / `update-in` / `assoc-in` など）が代表例です。
`T?` を非 optional な関数へ渡すと strict モードでは型エラーになります。

ネイティブレベルの既定は **strict**（CLI / LSP とも同一）です。

### `def-foreign`

宣言は許可しますが、呼び出し時は**未実装エラー**にします。
ネイティブ実行での外部言語連携は別タスクです。

---

## 表示規則

`println` はインタプリタと**同じ表示規則**に従います
（トップレベルの文字列は非クォート、コレクション内の文字列はクォート）。

---

## 性能の合格基準

- Clojure より**高速かつ省メモリ**であること
- 目標は Go / Rust に迫ること
- Clojure を下回る場合は**不合格として報告**する
- 実装ごとに [`docs/phase2/bench/`](bench/README.md) の各言語ベンチを実行して比較する
- **時間とメモリの両方**を比較する（max RSS を人間可読の MiB 表記で記録）

計測と最適化の作業ルールは [AGENTS.md](../../AGENTS.md) にあります。

---

## 実行経路（run）のバックエンド

ビルドせずに実行する `run` 経路を設ける計画があります。

- `run` はネイティブビルド経路専用の軽量 VM で実行する。インタプリタの VM は流用しない
- VM は **typed opcode 中心**とし、`Value` 依存を最小化する
- `clove build` は native codegen を継続し、VM は run 専用とする
- run は typed opcode を優先しつつ、dynamic fallback を許可する
- REPL は dynamic を優先し、typed opcode は最適化できる範囲のみ適用する
- ビルドの hot path では `Value` を使用しない

**CLI からは到達できません。** `crates/clove-build-core/src/vm/` を呼んでいるのは
`crates/clove-build-core/tests/vm.rs` だけです。CLI の `clove --vm` は
**インタプリタ側の VM**（`crates/clove-lang/src/vm/`）であり、別物です。

`Value` を使ってよいのは次に限ります。

- REPL / eval / 動的ロード
- plugin / foreign / dynamic fallback
- typed IR に乗らない動的機能

---

## Cバックエンドの呼び出し（2026-07-27）

Cバックエンドには C関数という概念がなく、呼び出しは呼び出し元へ本体を展開して
コンパイルします。再帰関数をビルドできないのはこのためです
（[既知の制限と不具合](../tooling/build.ja.md#4-既知の制限と不具合)）。

方針は2つ検討しました。**C関数として出力する**案は、再帰・相互再帰・関数を値として
渡すことをまとめて解決しますが、バックエンドの値表現とスコープを書き換えます。
**arity を一般化して再帰は明示エラーにする**案は小さく済みますが、再帰は未対応のままです。

**判断: 当面は明示エラーの案を続けます。** C関数の出力は後日のタスクとし、
実施を約束するものではありません。

理由は好みではなく順序です。バックエンドはいまも自前で `CType` を決めており、
これは関数型も型変数も表せないため、出力する C関数に型を付けられません。
**typed IR を受け取る**（上記で決定済み）のがその障害を取り除く段階であり、
どちらの案へ進んでも必要です。先にこれを済ませておけば、C関数の出力へ進まない場合でも
作業が無駄になりません。

再検討は、バックエンドが typed IR を受け取るようになってから。残りの作業量は
その時点で測り直します。

---

## 全体設計の見直し（2026-03-07）

- 根本課題は backend の実装言語ではなく、**frontend / 型 / lowering / runtime の分裂**にある
- source of truth は `clove-build-core` とし、reader / syntax rewrite / 型推論 /
  typed IR lowering をここへ集約する
- `clove-build-front` は**暫定 adapter** とし、最終的に削除する
- `clove-build-backend-c` は `FrontProgram` ではなく **typed IR** を受け取る形へ移行する
- `run` も最終的に **typed IR → bytecode** を正規経路とする
- C バックエンドへの切替判断は維持する（Rust バックエンドへ戻さない）
- REPL はインタプリタの runtime を当面維持してよいが、reader / syntax / 型情報は
  ネイティブビルドと共有する

**Clove は「1つの言語、1つの frontend、2つの backend/runtime」として実装します。**

### 優先順位

build の合格 > run(VM) > LSP / REPL。
build が不合格なら最優先で build を改善し、VM の改善は後回しにします。

---

## 関連

- [2経路の設計ノート](../design-notes/two-phase-implementation.ja.md) — なぜ2実装なのか
- [`mut` と `imut` の設計ノート](../design-notes/mutability.ja.md)
- [既知の制限と不具合](../tooling/build.ja.md#4-既知の制限と不具合) — 実測の現状
- [Phase2 の設計と現状](README.ja.md)
