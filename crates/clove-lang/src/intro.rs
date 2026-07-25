//! REPL 起動時と `:intro` で表示する導入テキスト。
//!
//! 例はカテゴリごとに 1 件ずつランダムに抽選する。各行は
//! `code  ; => result  note` の 1 行で完結し、`; =>` 以降は Clove のコメントなので
//! **行をそのままコピペして評価できる**。この 1 行制約があるため、複数行に折り返す例は
//! プールに入れない（メソッドチェーンは `.filter(...)` の前で改行すると壊れる）。
//!
//! プールの全例は `tests/intro_examples.rs` で実際に評価され、`result` と突き合わされる。

use nu_ansi_term::Style;
use rand::Rng;

/// プールに登録できるコードの最大幅。
pub const MAX_CODE_WIDTH: usize = 45;
/// プールに登録できる結果の最大幅。
pub const MAX_RESULT_WIDTH: usize = 22;
/// プールに登録できる注記の最大幅。
pub const MAX_NOTE_WIDTH: usize = 14;

/// 注記を落とした最小レイアウトでも必要になる幅の上限。
/// これより狭い端末では折り返しを避けられない。
pub const MIN_RENDER_WIDTH: usize = INDENT + MAX_CODE_WIDTH + GAP + ARROW.len() + MAX_RESULT_WIDTH;

const INDENT: usize = 2;
const GAP: usize = 2;
const ARROW: &str = "; => ";

const FOOTER: &str = "Tab completes. :help for commands, :intro for more examples, :q to quit.";

/// 見出し。埋め込み言語は feature で有効なものだけを名乗る。
/// `--no-default-features` ビルドで `${...}` / `$py{...}` が使えないのに宣伝しないため。
fn header() -> String {
    let mut text = String::from("clove REPL — a small Lisp with method chains, indexers");
    match (cfg!(feature = "ruby"), cfg!(feature = "python")) {
        (true, true) => text.push_str(", and inline Ruby/Python."),
        (true, false) => text.push_str(", and inline Ruby."),
        (false, true) => text.push_str(", and inline Python."),
        (false, false) => text.push('.'),
    }
    text
}

/// バナーに載せる 1 例。
pub struct IntroExample {
    /// そのまま評価できる 1 行のコード。
    pub code: &'static str,
    /// `code` を評価した結果の `Display` 表現。
    pub result: &'static str,
    /// 何の機能を見ているかの短い注記。
    pub note: &'static str,
}

/// 抽選単位。起動ごとに各カテゴリから 1 件ずつ選ぶ。
pub struct IntroCategory {
    pub name: &'static str,
    pub examples: &'static [IntroExample],
}

const METHOD_CHAIN: &[IntroExample] = &[
    IntroExample {
        code: "(map inc (range 10)).filter(even?)",
        result: "[2 4 6 8 10]",
        note: "method chain",
    },
    IntroExample {
        code: "(range 10).filter(even?).reduce(+)",
        result: "20",
        note: "method chain",
    },
    IntroExample {
        code: "(range 10).map(#(* % %)).take(4)",
        result: "[0 1 4 9]",
        note: "short fn #()",
    },
    IntroExample {
        code: "[3 1 2].sort().map(inc)",
        result: "[2 3 4]",
        note: "method chain",
    },
    IntroExample {
        code: r#""hello world".split(" ")"#,
        result: r#"["hello" "world"]"#,
        note: "method chain",
    },
];

const DOT_CHAIN: &[IntroExample] = &[
    IntroExample {
        code: "(range 10).(map inc ?).(filter even? ?)",
        result: "[2 4 6 8 10]",
        note: "dot-chain",
    },
    IntroExample {
        code: "(range 10).(filter even? ?).(reduce + ?)",
        result: "20",
        note: "dot-chain",
    },
    IntroExample {
        code: "[inc (range 5)].(map *?)",
        result: "[1 2 3 4 5]",
        note: "*? spreads",
    },
    IntroExample {
        code: "[3 1 2].(sort ?).(map inc ?)",
        result: "[2 3 4]",
        note: "dot-chain",
    },
];

const PLACEHOLDER: &[IntroExample] = &[
    IntroExample {
        code: "(map (+ ? 10) (range 5))",
        result: "[10 11 12 13 14]",
        note: "? makes a fn",
    },
    IntroExample {
        code: "(filter (not= :skip ?) [:ok :skip :ok])",
        result: "[:ok :ok]",
        note: "? makes a fn",
    },
    IntroExample {
        code: r#"(map (str "id-" ?) [1 2 3])"#,
        result: r#"["id-1" "id-2" "id-3"]"#,
        note: "? makes a fn",
    },
    IntroExample {
        code: "(map (* ? ?) [1 2 3])",
        result: "[1 4 9]",
        note: "? makes a fn",
    },
];

const INDEXER: &[IntroExample] = &[
    IntroExample {
        code: r#"{name: "Taro" age: 30}[:name]"#,
        result: r#""Taro""#,
        note: "map + indexer",
    },
    IntroExample {
        code: "[10 11 12 13 14 15][1..3]",
        result: "[11 12 13]",
        note: "index range",
    },
    IntroExample {
        code: "[10 11 12 13 14 15][-1]",
        result: "15",
        note: "index from end",
    },
    IntroExample {
        code: "[10 11 12][99 || :none]",
        result: ":none",
        note: "index default",
    },
    IntroExample {
        code: r#"#json{"host":"localhost","port":8080}["host"]"#,
        result: r#""localhost""#,
        note: "json literal",
    },
];

#[cfg(feature = "ruby")]
const RUBY_UPCASE: IntroExample = IntroExample {
    code: r#"${ "hello".upcase }"#,
    result: r#""HELLO""#,
    note: "inline Ruby",
};

#[cfg(feature = "ruby")]
const RUBY_SUM: IntroExample = IntroExample {
    code: "${ (1..5).sum }",
    result: "15",
    note: "inline Ruby",
};

#[cfg(feature = "python")]
const PY_SQRT: IntroExample = IntroExample {
    code: "$py{ import math; math.sqrt(9) }",
    result: "3.0",
    note: "inline Python",
};

#[cfg(feature = "python")]
const PY_SORTED: IntroExample = IntroExample {
    code: r#"$py{ sorted("clove") }"#,
    result: r#"["c" "e" "l" "o" "v"]"#,
    note: "inline Python",
};

// ruby / python は既定 feature だが optional なので、無効ビルドで動かない例を宣伝しない。
#[cfg(all(feature = "ruby", feature = "python"))]
const FOREIGN: &[IntroExample] = &[RUBY_UPCASE, RUBY_SUM, PY_SQRT, PY_SORTED];
#[cfg(all(feature = "ruby", not(feature = "python")))]
const FOREIGN: &[IntroExample] = &[RUBY_UPCASE, RUBY_SUM];
#[cfg(all(not(feature = "ruby"), feature = "python"))]
const FOREIGN: &[IntroExample] = &[PY_SQRT, PY_SORTED];
#[cfg(all(not(feature = "ruby"), not(feature = "python")))]
const FOREIGN: &[IntroExample] = &[];

const CATEGORIES: &[IntroCategory] = &[
    IntroCategory {
        name: "method chain",
        examples: METHOD_CHAIN,
    },
    IntroCategory {
        name: "dot-chain",
        examples: DOT_CHAIN,
    },
    IntroCategory {
        name: "placeholder",
        examples: PLACEHOLDER,
    },
    IntroCategory {
        name: "indexer",
        examples: INDEXER,
    },
    IntroCategory {
        name: "foreign",
        examples: FOREIGN,
    },
];

/// 例を持つカテゴリだけを返す。feature 無効で空になったカテゴリは除外される。
pub fn categories() -> Vec<&'static IntroCategory> {
    CATEGORIES
        .iter()
        .filter(|category| !category.examples.is_empty())
        .collect()
}

/// カテゴリごとに 1 件ずつ抽選する。
pub fn pick() -> Vec<&'static IntroExample> {
    let mut rng = rand::thread_rng();
    categories()
        .into_iter()
        .map(|category| {
            let index = rng.gen_range(0..category.examples.len());
            &category.examples[index]
        })
        .collect()
}

/// 注記を落としたレイアウトに必要な幅。これ未満の端末では折り返しを避けられない。
pub fn min_width(picks: &[&IntroExample]) -> usize {
    let code = picks.iter().map(|p| width_of(p.code)).max().unwrap_or(0);
    let result = picks.iter().map(|p| width_of(p.result)).max().unwrap_or(0);
    INDENT + code + GAP + ARROW.len() + result
}

/// 抽選済みの例を桁揃えして描画する。
///
/// コード列と結果列は `picks` の中での最大長に揃える。注記まで含めた幅が `width` を超える場合は
/// 注記列を落とす（コード列と結果列の桁揃えは維持する）。`color` が真なら `; =>` 以降を dim 表示する。
pub fn render(picks: &[&IntroExample], width: usize, color: bool) -> String {
    let code_width = picks.iter().map(|p| width_of(p.code)).max().unwrap_or(0);
    let result_width = picks.iter().map(|p| width_of(p.result)).max().unwrap_or(0);
    let note_width = picks.iter().map(|p| width_of(p.note)).max().unwrap_or(0);

    let full_width = min_width(picks) + GAP + note_width;
    let with_notes = full_width <= width;

    let mut out = String::new();
    out.push_str(&header());
    out.push_str("\n\nTry:\n");
    for pick in picks {
        let head = format!("{}{}", " ".repeat(INDENT), pad(pick.code, code_width));
        let tail = if with_notes {
            format!(
                "{}{}{}{}{}",
                " ".repeat(GAP),
                ARROW,
                pad(pick.result, result_width),
                " ".repeat(GAP),
                pick.note
            )
        } else {
            format!("{}{}{}", " ".repeat(GAP), ARROW, pick.result)
        };
        let tail = tail.trim_end();
        let tail = if color {
            Style::new().dimmed().paint(tail).to_string()
        } else {
            tail.to_string()
        };
        out.push_str(&head);
        out.push_str(&tail);
        out.push('\n');
    }
    out.push('\n');
    out.push_str(FOOTER);
    out.push('\n');
    out
}

/// 起動時と `:intro` で表示する本文。端末幅に合わせて描画する。
pub fn intro_text() -> String {
    render(&pick(), terminal_width(), true)
}

fn terminal_width() -> usize {
    crossterm::terminal::size()
        .map(|(cols, _)| cols as usize)
        .unwrap_or(100)
}

fn width_of(text: &str) -> usize {
    text.chars().count()
}

fn pad(text: &str, width: usize) -> String {
    let len = width_of(text);
    if len >= width {
        text.to_string()
    } else {
        format!("{}{}", text, " ".repeat(width - len))
    }
}
