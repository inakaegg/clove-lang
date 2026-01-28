# HM 型推論とメタデータの同期方針

Clove では `clove-core::fn_meta` に各関数のシグネチャ（`FnOverload` / `TypeKind`）を登録しつつ、  
HM ベースの型推論（`typing::infer`）用には別途 `prelude_env()` で型スキームを手書きしています。  
この二重管理は以下の問題を引き起こします。

- 片方だけ更新するともう片方に反映されず、REPL の `:doc` と HM 推論で型が食い違う。
- 新しいコア関数を追加する際に 2 箇所更新が必要になり、抜け漏れが発生しやすい。
- どちらがソース・オブ・トゥルースか不明瞭で、将来的な自動生成が難しい。

## 目標

1. **`FnMeta` を型情報の唯一の情報源**にする。  
   HM の前提環境（`typing::infer::prelude_env`）は `FnMeta` に登録された `TypeKind` から自動生成する。
2. `FnMeta` にまだ型が入っていないビルトインは、暫定的に手書きスキームで補完する。  
   これにより段階的に `TypeKind` への移行ができる。

## 実装ステップ

1. `fn_meta::all()` のような列挙 API を追加し、登録済みメタデータを取得できるようにする。✔（実装済み）
2. `TypeKind -> hm::Type` 変換を再利用し、`FnOverload` を `Scheme` に落とすヘルパーを `typing::infer` に導入する。
3. `prelude_env()` で
   - まず `FnMeta` 由来のスキームを名前（`name` / `ns::name`）ごとに挿入
   - 残りの手書きエントリ（まだメタデータが無い関数）を `entry` が存在しない場合だけ挿入
4. 以降、新しいビルトインを追加する際は `FnMeta` に `overloads` を登録し、手書きスキームとの差分を徐々に減らす。

## 具体的なタスク

1. **変換ヘルパーの整備**
   - `typing::infer` に `fn scheme_from_overload(overload: &FnOverload) -> Option<Scheme>` を追加。
   - `FnArity` と `rest` 引数（`TypeKind::Vector` や `TypeKind::Named`）への対応ルールを決めておく。`TypeKind::Named` は暫定的に `Type::Any` にフォールバックする。
2. **prelude_env の再構築**
   - 既存の手書き `Scheme` 作成ロジックを `manual_prelude_entries()` に切り出し、`FnMeta` から構築した HashMap に対して `entry.entry(name).or_insert(scheme)` する。
   - `core::*` 以外にも `string::*` / `io::*` など名前空間付き関数をカバーするため、`meta.fq_name()` と `meta.name` の両方をキーに登録する。
3. **型情報のギャップ洗い出し**
   - `fn_meta::all()` を使って「`overloads` が空」「TypeKind が `Any` しか無い」などの関数を一覧化し、優先度順に TypeKind を補完する ToDo を作成。
   - 補完不可の関数は `manual_prelude_entries()` 側に暫定スキームを残し、コメントで理由を明記する。
4. **テスト計画**
   - `typing::infer::tests` に `prelude_env_coverage` のようなチェックを追加し、FnMeta 由来スキームが一定数以上存在することを検証。
   - 代表的なビルトイン（`+`, `map`, `assoc` など）について、`fn_meta` の TypeKind を更新すると HM 推論の結果も変わることを確認する統合テストを追加。

この流れで進めれば、段階的に `FnMeta` をソース・オブ・トゥルース化しながら HM prelude を自動生成できる。***

この方針により「ドキュメント／LSP／REPL／型推論」がすべて同じメタデータを参照するようになり、  
仕様ブレやメンテナンスコストを大幅に下げられます。***

---

## 付録: 型ヒント（postfix / `:` annotation）

Clove の Reader は式の末尾に型ヒントを付けられます。

### 1) `<...>` 形式

`symbol<Type>` のように、シンボルに `<...>` を付ける形式です。

```clojure
(def x<Int> 10)
```

### 2) `: TYPE` 形式

`expr: TYPE` の形で、Reader が `:` の後ろを “型式” として読みます。

```clojure
(def v [1 2]: [Int Int])
```

> ただし `type` は **実行時値の型** を返す設計のため、
> 型ヒントがあっても `(type v)` の結果は変わらないことがあります。

### 型ヒントは “ヒント”

現状の多くは
- LSP 表示
- typed build

のための情報で、実行時チェックを強制するものではありません。

## 付録: `clove build --opt=typed`

typed build は typed IR から Rust コード生成を行う実験機能です。

- 互換性が薄い箇所は WARNING が出ます
- strict モードでは警告がエラーになります

ツール側の仕様は [Build](../tooling/build.ja.md) を参照。

---
<!-- NAV:START -->
**前へ:** [トラブルシュート](../tooling/troubleshooting.ja.md)
**次へ:** [名前空間設計ノート](namespaces_design.ja.md)
<!-- NAV:END -->

