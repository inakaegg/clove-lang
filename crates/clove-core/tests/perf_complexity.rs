use clove_core::options::EvalOptions;
use clove_core::RuntimeCtx;
use std::hint::black_box;
use std::sync::Arc;
use std::thread;
use std::time::{Duration, Instant};

fn runtime_ctx() -> Arc<RuntimeCtx> {
    RuntimeCtx::new(EvalOptions::default(), &[])
}

fn run_with_large_stack<F>(f: F)
where
    F: FnOnce() + Send + 'static,
{
    let handle = thread::Builder::new()
        .stack_size(16 * 1024 * 1024)
        .spawn(f)
        .expect("failed to allocate a larger stack");
    handle.join().expect("test thread panicked");
}

fn measure_eval(ctx: &Arc<RuntimeCtx>, src: &str) -> Duration {
    let mut best = Duration::MAX;
    for _ in 0..3 {
        let start = Instant::now();
        let value = ctx.eval_source(src).expect("eval");
        black_box(value);
        let elapsed = start.elapsed();
        if elapsed < best {
            best = elapsed;
        }
    }
    best
}

fn assert_linear(label: &str, small: Duration, large: Duration) {
    let small_ms = small.as_secs_f64() * 1000.0;
    let large_ms = large.as_secs_f64() * 1000.0;
    let ratio = large_ms / small_ms.max(0.000_001);
    assert!(
        ratio < 3.5,
        "{} complexity deviates from linear: {:.3}ms -> {:.3}ms (ratio {:.2})",
        label,
        small_ms,
        large_ms,
        ratio
    );
}

fn escape_string_literal(value: &str) -> String {
    value.replace('\\', "\\\\").replace('"', "\\\"")
}

fn json_array(n: usize) -> String {
    let mut out = String::with_capacity(n.saturating_mul(2) + 2);
    out.push('[');
    for idx in 0..n {
        if idx > 0 {
            out.push(',');
        }
        out.push('0');
    }
    out.push(']');
    out
}

#[test]
fn into_scales_linearly() {
    run_with_large_stack(|| {
        let ctx = runtime_ctx();
        let n = 2000;
        let small = format!("(count (into [] (range {})))", n);
        let large = format!("(count (into [] (range {})))", n * 2);
        let t_small = measure_eval(&ctx, &small);
        let t_large = measure_eval(&ctx, &large);
        assert_linear("into", t_small, t_large);
    });
}

#[test]
fn reduce_conj_scales_linearly() {
    run_with_large_stack(|| {
        let ctx = runtime_ctx();
        let n = 2000;
        let small = format!("(count (reduce conj [] (range {})))", n);
        let large = format!("(count (reduce conj [] (range {})))", n * 2);
        let t_small = measure_eval(&ctx, &small);
        let t_large = measure_eval(&ctx, &large);
        assert_linear("reduce+conj", t_small, t_large);
    });
}

#[test]
fn concat_scales_linearly() {
    run_with_large_stack(|| {
        let ctx = runtime_ctx();
        let n = 2000;
        let small = format!("(count (concat (range {}) (range {})))", n, n);
        let large = format!("(count (concat (range {}) (range {})))", n * 2, n * 2);
        let t_small = measure_eval(&ctx, &small);
        let t_large = measure_eval(&ctx, &large);
        assert_linear("concat", t_small, t_large);
    });
}

#[test]
fn flatten_scales_linearly() {
    run_with_large_stack(|| {
        let ctx = runtime_ctx();
        let n = 2000;
        let small = format!("(count (flatten (map (fn [x] [x]) (range {}))))", n);
        let large = format!("(count (flatten (map (fn [x] [x]) (range {}))))", n * 2);
        let t_small = measure_eval(&ctx, &small);
        let t_large = measure_eval(&ctx, &large);
        assert_linear("flatten", t_small, t_large);
    });
}

#[test]
fn map_scales_linearly() {
    run_with_large_stack(|| {
        let ctx = runtime_ctx();
        let n = 2000;
        let small = format!("(count (map inc (range {})))", n);
        let large = format!("(count (map inc (range {})))", n * 2);
        let t_small = measure_eval(&ctx, &small);
        let t_large = measure_eval(&ctx, &large);
        assert_linear("map", t_small, t_large);
    });
}

#[test]
fn filter_scales_linearly() {
    run_with_large_stack(|| {
        let ctx = runtime_ctx();
        let n = 2000;
        let small = format!("(count (filter (fn [x] (= 0 (mod x 2))) (range {})))", n);
        let large = format!(
            "(count (filter (fn [x] (= 0 (mod x 2))) (range {})))",
            n * 2
        );
        let t_small = measure_eval(&ctx, &small);
        let t_large = measure_eval(&ctx, &large);
        assert_linear("filter", t_small, t_large);
    });
}

#[test]
fn remove_scales_linearly() {
    run_with_large_stack(|| {
        let ctx = runtime_ctx();
        let n = 2000;
        let small = format!("(count (remove (fn [x] (= 0 (mod x 2))) (range {})))", n);
        let large = format!(
            "(count (remove (fn [x] (= 0 (mod x 2))) (range {})))",
            n * 2
        );
        let t_small = measure_eval(&ctx, &small);
        let t_large = measure_eval(&ctx, &large);
        assert_linear("remove", t_small, t_large);
    });
}

#[test]
fn keep_scales_linearly() {
    run_with_large_stack(|| {
        let ctx = runtime_ctx();
        let n = 2000;
        let small = format!(
            "(count (keep (fn [x] (if (= 0 (mod x 2)) x nil)) (range {})))",
            n
        );
        let large = format!(
            "(count (keep (fn [x] (if (= 0 (mod x 2)) x nil)) (range {})))",
            n * 2
        );
        let t_small = measure_eval(&ctx, &small);
        let t_large = measure_eval(&ctx, &large);
        assert_linear("keep", t_small, t_large);
    });
}

#[test]
fn take_scales_linearly() {
    run_with_large_stack(|| {
        let ctx = runtime_ctx();
        let n = 2000;
        let small = format!("(count (take {} (range {})))", n, n * 2);
        let large = format!("(count (take {} (range {})))", n * 2, n * 4);
        let t_small = measure_eval(&ctx, &small);
        let t_large = measure_eval(&ctx, &large);
        assert_linear("take", t_small, t_large);
    });
}

#[test]
fn drop_scales_linearly() {
    run_with_large_stack(|| {
        let ctx = runtime_ctx();
        let n = 2000;
        let small = format!("(count (drop {} (range {})))", n, n * 2);
        let large = format!("(count (drop {} (range {})))", n * 2, n * 4);
        let t_small = measure_eval(&ctx, &small);
        let t_large = measure_eval(&ctx, &large);
        assert_linear("drop", t_small, t_large);
    });
}

#[test]
fn nth_scales_linearly() {
    run_with_large_stack(|| {
        let ctx = runtime_ctx();
        let n = 2000;
        let small = format!(
            "(let [v (range {})] (reduce (fn [acc i] (+ acc (nth v i))) 0 (range {})))",
            n, n
        );
        let large = format!(
            "(let [v (range {})] (reduce (fn [acc i] (+ acc (nth v i))) 0 (range {})))",
            n * 2,
            n * 2
        );
        let t_small = measure_eval(&ctx, &small);
        let t_large = measure_eval(&ctx, &large);
        assert_linear("nth", t_small, t_large);
    });
}

#[test]
fn get_in_scales_linearly() {
    run_with_large_stack(|| {
        let ctx = runtime_ctx();
        let n = 2000;
        let small = format!(
            "(let [m {{:a {{:b {{:c 1}}}}}}] (reduce (fn [acc _] (get-in m [:a :b :c])) 0 (range {})))",
            n
        );
        let large = format!(
            "(let [m {{:a {{:b {{:c 1}}}}}}] (reduce (fn [acc _] (get-in m [:a :b :c])) 0 (range {})))",
            n * 2
        );
        let t_small = measure_eval(&ctx, &small);
        let t_large = measure_eval(&ctx, &large);
        assert_linear("get-in", t_small, t_large);
    });
}

#[test]
fn join_scales_linearly() {
    run_with_large_stack(|| {
        let ctx = runtime_ctx();
        let n = 2000;
        let small = format!("(count (join (repeat {} \"a\") \",\"))", n);
        let large = format!("(count (join (repeat {} \"a\") \",\"))", n * 2);
        let t_small = measure_eval(&ctx, &small);
        let t_large = measure_eval(&ctx, &large);
        assert_linear("join", t_small, t_large);
    });
}

#[test]
fn json_parse_scales_linearly() {
    run_with_large_stack(|| {
        let ctx = runtime_ctx();
        let n = 1000;
        let small_json = escape_string_literal(&json_array(n));
        let large_json = escape_string_literal(&json_array(n * 2));
        let small = format!("(count (json::parse \"{}\"))", small_json);
        let large = format!("(count (json::parse \"{}\"))", large_json);
        let t_small = measure_eval(&ctx, &small);
        let t_large = measure_eval(&ctx, &large);
        assert_linear("json::parse", t_small, t_large);
    });
}
