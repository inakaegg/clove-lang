//! REPL イントロの例が実際に動くことを保証する回帰テスト。
//!
//! バナーに載せる例は「コピペすればそのまま動く」ことが前提なので、
//! 言語仕様が変わって例が嘘になっていないかをここで検出する。

use std::process::Command;

use clove_lang::intro::{self, IntroExample, MAX_CODE_WIDTH, MAX_NOTE_WIDTH, MAX_RESULT_WIDTH};

/// 幅の保証はサンプル行だけに掛かる。見出しとフッタは固定文なので対象外。
fn example_lines<'a>(rendered: &'a str, picks: &[&IntroExample]) -> Vec<&'a str> {
    let lines: Vec<&str> = rendered
        .lines()
        .filter(|line| picks.iter().any(|pick| line.contains(pick.code)))
        .collect();
    assert_eq!(
        lines.len(),
        picks.len(),
        "every pick must render on its own line"
    );
    lines
}

fn all_examples() -> Vec<&'static IntroExample> {
    intro::categories()
        .into_iter()
        .flat_map(|cat| cat.examples.iter())
        .collect()
}

/// 本命のテスト: プールの全例を実際に評価し、記載された結果と一致することを確認する。
///
/// `clove -e` と同じ経路を通すため、ライブラリ内で評価せず実バイナリを起動する。
/// Ruby / Python の埋め込みは VM をプロセス内で一度しか初期化できず、
/// テストハーネスのスレッド上では起動できないため、この形が唯一忠実に検証できる。
#[test]
fn every_intro_example_evaluates_to_its_documented_result() {
    let clove = env!("CARGO_BIN_EXE_clove");

    let mut failures = Vec::new();
    for example in all_examples() {
        let output = Command::new(clove)
            .arg("-e")
            .arg(example.code)
            .output()
            .expect("failed to run the clove binary");

        if !output.status.success() {
            failures.push(format!(
                "{}\n  exited with {}\n  stderr: {}",
                example.code,
                output.status,
                String::from_utf8_lossy(&output.stderr).trim()
            ));
            continue;
        }

        let actual = String::from_utf8_lossy(&output.stdout)
            .trim_end()
            .to_string();
        if actual != example.result {
            failures.push(format!(
                "{}\n  expected: {}\n  actual:   {}",
                example.code, example.result, actual
            ));
        }
    }

    assert!(
        failures.is_empty(),
        "intro examples out of date:\n{}",
        failures.join("\n")
    );
}

#[test]
fn intro_examples_fit_the_width_budget() {
    for example in all_examples() {
        assert!(
            example.code.chars().count() <= MAX_CODE_WIDTH,
            "code too wide ({} > {}): {}",
            example.code.chars().count(),
            MAX_CODE_WIDTH,
            example.code
        );
        assert!(
            example.result.chars().count() <= MAX_RESULT_WIDTH,
            "result too wide ({} > {}): {}",
            example.result.chars().count(),
            MAX_RESULT_WIDTH,
            example.result
        );
        assert!(
            example.note.chars().count() <= MAX_NOTE_WIDTH,
            "note too wide ({} > {}): {}",
            example.note.chars().count(),
            MAX_NOTE_WIDTH,
            example.note
        );
    }
}

#[test]
fn intro_examples_are_single_line() {
    // メソッドチェーンは `.filter(...)` の前で改行すると壊れるため、
    // 表示もコピペも 1 行で完結していなければならない。
    for example in all_examples() {
        assert!(
            !example.code.contains('\n'),
            "example must stay on one line: {}",
            example.code
        );
    }
}

#[test]
fn pick_returns_one_example_per_category() {
    let categories = intro::categories();
    assert!(!categories.is_empty(), "at least one category must exist");

    for _ in 0..32 {
        let picks = intro::pick();
        assert_eq!(picks.len(), categories.len());
        for (category, pick) in categories.iter().zip(picks.iter()) {
            assert!(
                category.examples.iter().any(|e| std::ptr::eq(e, *pick)),
                "pick {} does not belong to category {}",
                pick.code,
                category.name
            );
        }
    }
}

#[test]
fn render_aligns_columns_and_stays_within_width() {
    let picks = intro::pick();
    let rendered = intro::render(&picks, 120, false);

    for pick in &picks {
        assert!(rendered.contains(pick.code), "missing code: {}", pick.code);
        assert!(
            rendered.contains(pick.result),
            "missing result: {}",
            pick.result
        );
        assert!(rendered.contains(pick.note), "missing note: {}", pick.note);
    }

    // `; =>` の桁が揃っていること
    let comment_columns: Vec<usize> = rendered
        .lines()
        .filter_map(|line| line.find("; => "))
        .collect();
    assert!(!comment_columns.is_empty());
    assert!(
        comment_columns.iter().all(|c| *c == comment_columns[0]),
        "`; =>` columns are not aligned: {:?}",
        comment_columns
    );
}

#[test]
fn render_drops_notes_when_the_terminal_is_narrow() {
    let picks = intro::pick();
    // 注記まで載せると溢れるが、コードと結果は必ず収まる幅
    let narrow = intro::min_width(&picks);
    let rendered = intro::render(&picks, narrow, false);

    for pick in &picks {
        assert!(rendered.contains(pick.code), "missing code: {}", pick.code);
        assert!(
            rendered.contains(&format!("; => {}", pick.result)),
            "missing result: {}",
            pick.result
        );
    }
    for line in example_lines(&rendered, &picks) {
        assert!(
            line.chars().count() <= narrow,
            "line exceeds terminal width {}: {:?}",
            narrow,
            line
        );
    }
}

#[test]
fn render_never_exceeds_the_given_width() {
    let picks = intro::pick();
    let min = intro::min_width(&picks);
    assert!(
        min <= intro::MIN_RENDER_WIDTH,
        "min_width {} must stay within the documented bound {}",
        min,
        intro::MIN_RENDER_WIDTH
    );

    for width in [min, min + 8, 80, 100, 120] {
        let rendered = intro::render(&picks, width, false);
        for line in example_lines(&rendered, &picks) {
            assert!(
                line.chars().count() <= width,
                "line exceeds width {}: {:?}",
                width,
                line
            );
        }
    }
}

/// 見出しは feature で実際に使える埋め込み言語だけを名乗らなければならない。
/// `--no-default-features` ビルドで `${...}` / `$py{...}` が動かないのに宣伝しないため。
#[test]
fn header_advertises_only_the_enabled_foreign_languages() {
    let rendered = intro::render(&intro::pick(), 120, false);
    let header = rendered.lines().next().expect("header line");

    assert_eq!(
        header.contains("Ruby"),
        cfg!(feature = "ruby"),
        "header must mention Ruby iff the ruby feature is on: {:?}",
        header
    );
    assert_eq!(
        header.contains("Python"),
        cfg!(feature = "python"),
        "header must mention Python iff the python feature is on: {:?}",
        header
    );

    let has_foreign_category = intro::categories().iter().any(|c| c.name == "foreign");
    assert_eq!(
        has_foreign_category,
        cfg!(feature = "ruby") || cfg!(feature = "python"),
        "the foreign category must exist iff at least one foreign feature is on"
    );
}

#[test]
fn intro_text_mentions_the_key_commands() {
    let text = intro::intro_text();
    for needle in [":help", ":intro", ":q", "Try:"] {
        assert!(text.contains(needle), "intro text must mention {}", needle);
    }
}
