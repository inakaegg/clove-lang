# 正規表現 / Duration リテラル

- 更新日: 2025-12-21

このページは **Reader が理解する `regex` と `duration` のリテラル** を説明します。

## 1. 正規表現

### 1.1 `/.../`

```clojure
(def re /a+b*/)
(re-find re "aaab")
```

### 1.2 `#/.../`（曖昧回避用）

```clojure
(def re #/\d+/)
(re-find re "id=123")
```

> `/.../` は「直後が空白/区切りでない」かつ「同一行に終端の `/` がある」場合のみ regex として読まれます。曖昧な場合は `#/.../` を使います。  
> `#"..."` は廃止しました。
> OOP チェインの正規表現ステージは `.#/.../` を使います（`./` は `/` メソッドとして解釈されます）。

### 1.3 関数として使う

正規表現リテラルは **関数として呼べます**。  
`(/.../ text)` は `re-matches` と同等で、**全文マッチ時のみ値を返し**、それ以外は `nil` になります。  
そのため、高階関数にもそのまま渡せます。

```clojure
(/\\d+/ "123")    ; => "123"
(/\\d+/ "a2")     ; => nil

(map /\\d+/ ["1" "a2" "333"]) ; => ["1" nil "333"]
```

### 1.4 文字列補間

正規表現リテラルでも `#{...}` の補間が使えます。  
`#{` をリテラルとして出したい場合は `\#{` のようにエスケープしてください。

```clojure
(let [target "猫"]
  (/.*#{target}.*/ "黒猫")) ; => "黒猫"
```

## 2. Duration

Clove には `Duration` 値があり、タイムアウトなどに使います。

### 2.1 リテラル（**整数のみ**）

```clojure
10ms
2s
-5m
```

- `ns`, `us`, `ms`, `s`, `m`, `h`, `d`, `w`
- アンダースコア区切り: `1_000ms`

**注意:** 現状の Reader は `0.1s` のような “小数の duration リテラル” をサポートしません。
（`read-string` では `0.1s` は `Symbol` になります。）

### 2.2 関数で作る（小数対応）

小数を使いたい場合は `duration` 関数を使います。

```clojure
(duration 0.1 :s)
(duration 1500 :ms)
```

### 2.3 `timeout` との関係

`timeout` は “ミリ秒 int” か “Duration” を受け取ります。

```clojure
(timeout 100)     ; 100ms
(timeout 0.5s)    ; ← これは今は **書けない**
(timeout (duration 0.5 :s))
```

---
<!-- NAV:START -->
**前へ:** [マップ省略記法（JS 準拠）](map_shorthand.ja.md)
**次へ:** [並行/非同期（chan / future / go-loop / scope-loop / async-scope）](concurrency.ja.md)
<!-- NAV:END -->

