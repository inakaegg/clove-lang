# 配布バイナリは Ruby / Python を埋め込まない

English documentation: [prebuilt-binaries.md](prebuilt-binaries.md)

- 更新日: 2026-09-04

## 決定

GitHub Releases に添付する `clove` は `--no-default-features --features repl_guard`
でビルドします。Ruby と Python の埋め込みは入っていません。それ以外
（インタプリタ、`clove fmt`、`clove build`、ネイティブプラグイン、REPL）は
ソースビルドと同じです。

```clojure
(println $rb{ RUBY_VERSION })
;; 配布バイナリ => Runtime error: unknown foreign tag: rb
```

起動は普通にできます。エラーが出るのは、プログラムが `$rb{...}` や `$py{...}`
に到達したときだけです。埋め込みが必要な人は
`cargo install --git https://github.com/inakaegg/clove-lang --locked clove-lang`
でソースからインストールすると、そのマシンにある Ruby / Python にリンクされます。

macOS 向けのバイナリは、CI に署名用の secrets を登録したあとの最初のリリースから
Developer ID 証明書で署名し、公証（notarization）します。それより前のリリースは
未署名です。署名には hardened runtime の
entitlement を1つだけ付けます（`com.apple.security.cs.disable-library-validation`）。
ネイティブプラグインを動かし続けるためです（「帰結」を参照）。

## 理由

**埋め込み Ruby / Python は `main` より前に解決される。** Ruby ブリッジは
`magnus` + `rb-sys`、Python ブリッジは `pyo3` を使い、どちらも `libruby` と
`libpython` をビルド時にリンクします。該当のライブラリがないマシンでは、動的ローダが
プログラムの開始前に失敗します。`clove` が警告を出して続行する余地はありません。

**バージョンと置き場所の両方が焼き込まれる。** minor バージョン（たとえば
Ruby 3.4、Python 3.12）は ABI としてコンパイル時に固定されます。加えて macOS では、
ライブラリの install name（絶対パス）もバイナリに書き込まれます。
`~/.rbenv/versions/3.4.1/lib/libruby.3.4.dylib` に対してビルドしたものは、
同じパスがある Mac でしか起動しません。Homebrew、rbenv、pyenv、python.org の
framework は同じバージョンを別々の場所に置くので、「Ruby 3.4 が必要」という表現では
要件を言い切れません。

**ライブラリをリンクしないバイナリには、この問題が一切ない。** Cargo の `ruby` /
`python` feature は optional 依存です。これを外すと、動的依存はシステムライブラリだけに
なります。Apple Silicon の Mac と、glibc 2.35 以上の x86_64 Linux なら、どこでも動きます。

## 採らなかった案

**Homebrew の `ruby@3.4` と `python@3.12` に固定した完全版を配る。** 成立はしますし、
README に要件を正直に書くこともできます。見送った理由は、「落として動かす」が
「先に Homebrew の formula を2つ、しかもその2つだけ入れる」に変わり、Homebrew が
formula を動かした時点で黙って動かなくなるからです。

**起動役（launcher）と2つのバイナリ。** 小さな起動役が設定ファイルを読み、指定の
`libruby` / `libpython` があるかを確かめて `DYLD_LIBRARY_PATH` を立てて完全版を
exec する。無ければ警告して埋め込みなし版を exec する。この形ならライブラリの
*パス* は設定で差し替えられますが、*バージョン* は差し替えられません。minor
バージョンごとに完全版が要ります。hardened runtime の下では
`allow-dyld-environment-variables` の entitlement も必要です。いまの利用者層に対して、
部品の多さが見合わないと判断しました。

**Ruby / Python ブリッジをプラグインに切り出し、`dlopen` で読む。** 本体は
ライブラリをリンクせず、最初に使うときにブリッジのプラグインを読み、読めなければ
穏やかに失敗する。長期的にはいちばん筋のよい形ですが、ブリッジとプラグイン API の
機能変更であって、配布形態の変更ではありません。配布バイナリの利用者から埋め込みの
要望が出たら再検討します。

**macOS バイナリに署名しない。** macOS 15 以降は、未署名で quarantine の付いた
実行ファイルは、Terminal から起動しても初回は止められます。システム設定で許可するまで
動きません。ブラウザで落とした人はその操作をするか、quarantine 属性を手で外すことに
なります。

## 帰結

**ネイティブプラグインのために hardened runtime を一部緩める。** 公証に必要な
hardened runtime は、既定で *library validation* を有効にします。プロセスは同じ
Team ID か Apple が署名した dylib しか読めません。`clove` は利用者がビルドした
プラグインを `dlopen` で読みます。`--allow-native-plugins` を付けたときだけではありません。
既定でも `<project>/plugins/`、`<project>/.clove/plugins/`、`$CLOVE_HOME/plugins/` から読みます。
lock で検証済みのパッケージのプラグインディレクトリも対象です
（`crates/clove-lang/src/native_policy.rs`）。さらに `clove plugin meta` と
`clove pkg install` は、メタデータを読むためにプラグインを開くときにこの policy を
通りません。library validation が有効だと、これらの経路はソースビルドでは動き、
配布バイナリでだけ失敗します。`disable-library-validation` の entitlement は、
その差をなくします。

**この緩和が意味すること。** 配布版の `clove` を信頼できないプロジェクトの
ディレクトリで実行すると、そこの `plugins/` に置かれた dylib が読み込まれます。
ソースビルドでもまったく同じ挙動で、署名によって広がるものではありません。
`native_policy.rs` の policy にも変更はありません。Apple Silicon では dylib 側にも
最低限の ad-hoc 署名が要りますが、`cargo` と `cc` が自動で付けます。

**チケットを staple できない。** 単体の Mach-O 実行ファイルには公証チケットを
staple できません。そのため quarantine の付いた複製を初めて実行するとき、Gatekeeper
は Apple へオンラインで照会します。コマンドラインの `tar` で展開すれば quarantine
属性は付きません。Finder で展開すると付きます。必要なら
`xattr -d com.apple.quarantine ./clove` で外せます。

**`clove build` には引き続き C コンパイラが要ります**（macOS なら Xcode Command
Line Tools）。ネイティブビルド経路が `cc` を起動するためです。

**埋め込みを外した状態ではテストが回りません。** `crates/clove-lang/tests/` の
統合テストのいくつかが `cfg` なしで `clove_ruby` と `clove_python` を参照しています。
release workflow はビルドだけを行い、テストは CI で既定 feature のまま回し続けます。

## 確認方法

```bash
# 配布バイナリは Ruby / Python にリンクしていない
otool -L ./clove | grep -E 'ruby|python'      # （出力なし）

# 既定 feature のソースビルドはリンクしている
otool -L ~/.cargo/bin/clove | grep -E 'ruby|python'

# foreign tag は起動時ではなく実行時に失敗する
./clove -e '(println (+ 1 2))'                # => 3
./clove -e '(println $rb{ 1 })'               # Runtime error: unknown foreign tag: rb

# 署名済み macOS バイナリは entitlement を持つ
codesign -d --entitlements - ./clove | grep disable-library-validation
```
