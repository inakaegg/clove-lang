# OOP メソッドチェインと Ruby デフォルトタグの扱い

- `oop-syntax` はデフォルトで有効。無効にしたい場合は `(use oop-syntax false)`（必要なら再度 `(use oop-syntax true)` でオン）。
- レシーバ位置は **関数メタ (`{:subject-pos n}` or `:last`) > `$arg` > 引数名推論（`coll`/`xs`/`seq`）** の順で決まる。FnMeta が存在するのに位置が決まらない場合はエラーにする（静かに1へ倒れない）。
- `:last`（または `-1`）を指定すると、レシーバは「引数リストの末尾」に入る。可変長でも末尾を選びたいときに使う。
- オブジェクト内メソッド / `method` / `&` self 糖衣は [docs/language/oop_methods.md](/docs/language/oop_methods.ja.md) を参照。
- 例（どれもデフォルト設定で動作）:
  - `"a".str("b")` → `"ab"`
  - `{:a 1}.keys` → `[:a]`
  - `(range 10).map(inc)` → `[1 2 3 4 5 6 7 8 9 10]`
  - `1.inc` → `2`
- より長いチェイン例:

```clojure
(use oop-syntax true)

(let [xs [1 2 3]
      ys [10 20 30]]
  ;; subject-pos = 2
  (println "mapped" xs.map(+ ys))          ; => [11 22 33]
  ;; subject-pos = :last（reduceは末尾）
  (println "sum" xs.reduce(+ 0))           ; => 6
  ;; dorun も末尾扱いなので arity を安定的にカバー
  (println "dorun" xs.dorun())
  (println "dorun with n" xs.dorun(2)))

;; 自作関数に :last を付ける例（doc の前後どちらでも attr-map を置ける）
(defn append-last {:subject-pos :last} "doc" [x coll] (conj coll x))
(println (str [1 2].append-last(3))) ; => [1 2 3]
```

## dot-indexer（OOP セグメントでのキー/インデックス参照）

`dot-indexer` を有効にすると、OOP チェイン内でキーやインデックス参照が書けます。

```clojure
(use oop-syntax true)
(use dot-indexer true)

(def config {:pipe {:gap-max 485}})
config:pipe:gap-max   ; => 485（keyword sugar）
config.:pipe.:gap-max ; => 485（明示セグメント）
```

### 明示セグメント（常にキー/インデックス参照）

- `x:kw` → `x.:kw` の糖衣（keyword セグメントのみ）
- `x:a:b` → `x.:a.:b`
- `x.:kw` → `get x :kw`
- `x."str"` → `get x "str"`
- `x.'sym` → `get x 'sym`
- `x.0` / `x.-1` → indexer と同じ扱い（vector/list/string はインデックス、map は key=0 参照）
- set にはインデックスが無いので `x.0` はエラー（`contains?` / `get` を使う）

### 省略セグメント（`x.ident`）の扱い

- `x.ident` は **常にメソッド呼び出し** として扱う（dot-indexer のキー探索はしない）
- キー参照は `x:kw` / `x.:kw` / `x."str"` / `x.'sym` を使う

メソッド呼び出しを明示したい場合は `x.(pipe ?)` または `(pipe x)` を使えます。

## チェイン内の束縛

- `.(as x)` はその時点の値を `x` に束縛し、以降のチェイン内だけで参照できます。
- `.(let x)` はその時点の値を `*x` に保存し、値はそのまま次に流します。
- `.repl` / `.(repl)` はその時点の値で REPL を開き、値を返してチェインを継続します。
- `.(repl x)` は `.(let x).repl` の短縮で、`*x` に保存してから REPL を開きます。

```clojure
(use oop-syntax true)

[1 2 3].(as xs).take(2).concat(xs).vec    ; => [1 2 1 2 3]
[1 2 3].(let a).take(2).vec               ; => [1 2]  (*a は [1 2 3])
[1 2 3].map(inc).repl.take(2).vec         ; => [2 3]
```

## Ruby 埋め込みの基本

- 他言語のオブジェクト参照には必ず `$` を付ける。Ruby なら `$rb:Foo.bar` を基本に使う。
  - 例: `$rb{require "nokogiri"}` のあとで `$rb:Nokogiri::HTML.parse(html).search("h1").text` と書く。
- Ruby の埋め込みブロックは `$rb{...}` が基本。`${...}` はデフォルト外部言語の省略（未指定なら Ruby）。
- タグ省略の `$Foo.bar` はデフォルトタグが設定されているときのみ有効（REPL の `:lang rb` / `(use default-interop :rb)` / ファイル名が `*.rb.clv` など）。

エラーが `foreign block requires explicit tag` と出た場合は、`$rb{...}` を付けるか、REPLでは`:lang rb`、ファイルなら拡張子 `*.rb.clv` もしくは `(use default-interop :rb)` でデフォルトタグをセットしてください。

---
<!-- NAV:START -->
**前へ:** [dot-chain: `x.(f ?)`, `x.(f *?)` など](dot_chain.ja.md)
**次へ:** [OOP メソッド（method / where / self）](oop_methods.ja.md)
<!-- NAV:END -->

