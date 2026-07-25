# bench_release 2026-01-29

## Summary (hyperfine mean + max RSS)

| Language | Output | mean(s) | max RSS (MiB) | Command |
| --- | --- | --- | --- | --- |
| clove2 run(vm) | 6850020200192 | 1.429 | 280.0 MiB | `target/release/clove2 run docs/phase2/bench/bench_release_clove2.clv` |
| clove2 | 6850020200192 | 0.3626 | 5.3 MiB | `docs/phase2/bench/bin/bench_release_clove2` |
| rust | 6850020200192 | 0.1299 | 2.7 MiB | `docs/phase2/bench/bin/bench_release_rust` |
| go | 6850020200192 | 0.2000 | 9.3 MiB | `docs/phase2/bench/bin/bench_release_go` |
| ruby | 6850020200192 | 1.150 | 57.4 MiB | `ruby docs/phase2/bench/bench_release_ruby.rb` |
| clj | 6850020200192 | 1.289 | 600.5 MiB | `clj -M docs/phase2/bench/bench_release_clj.clj` |

Note: max RSS is measured by `/usr/bin/time -l`.
Note: mean values are from `hyperfine`.

## Ratios (clove2 / others)

- vs Rust: **2.79x**
- vs Go: **1.81x**
- vs Ruby: **0.32x** (clove2 is faster)
- vs Clj: **0.28x** (clove2 is faster)

## Reference (clove2 run(vm) / others)

- vs Ruby: **1.24x** (slower)
- vs Clj: **1.11x** (slower)

## Source Files

- [docs/phase2/bench/bench_release_clove2.clv](../bench_release_clove2.clv)
- [docs/phase2/bench/bench_release_rust.rs](../bench_release_rust.rs)
- [docs/phase2/bench/bench_release_go.go](../bench_release_go.go)
- [docs/phase2/bench/bench_release_ruby.rb](../bench_release_ruby.rb)
- [docs/phase2/bench/bench_release_clj.clj](../bench_release_clj.clj)

## /usr/bin/time -l (max RSS)

| Language | Output | real(s) | max RSS (MiB) | Command |
| --- | --- | --- | --- | --- |
| clove2 run(vm) | 6850020200192 | 1.52 | 280.0 MiB | `target/release/clove2 run docs/phase2/bench/bench_release_clove2.clv` |
| clove2 | 6850020200192 | 0.36 | 5.3 MiB | `docs/phase2/bench/bin/bench_release_clove2` |
| rust | 6850020200192 | 0.14 | 2.7 MiB | `docs/phase2/bench/bin/bench_release_rust` |
| go | 6850020200192 | 0.21 | 9.3 MiB | `docs/phase2/bench/bin/bench_release_go` |
| ruby | 6850020200192 | 1.19 | 57.4 MiB | `ruby docs/phase2/bench/bench_release_ruby.rb` |
| clj | 6850020200192 | 1.71 | 600.5 MiB | `clj -M docs/phase2/bench/bench_release_clj.clj` |

### clove2 build (/usr/bin/time -l)

| Output | real(s) | max RSS (MiB) | Command |
| --- | --- | --- | --- |
| `docs/phase2/bench/bin/bench_release_clove2` | 14.14 | 746.4 MiB | `target/release/clove2 build docs/phase2/bench/bench_release_clove2.clv --out docs/phase2/bench/bin/bench_release_clove2 --emit-rust` |

## hyperfine

```
Benchmark 1: target/release/clove2 run docs/phase2/bench/bench_release_clove2.clv
  Time (mean ± σ):      1.429 s ±  0.038 s    [User: 1.328 s, System: 0.067 s]
  Range (min … max):    1.376 s …  1.493 s    10 runs
```

```
Benchmark 2: docs/phase2/bench/bin/bench_release_clove2
  Time (mean ± σ):     362.6 ms ±  32.4 ms    [User: 342.8 ms, System: 3.6 ms]
  Range (min … max):   345.4 ms … 450.1 ms    10 runs

Benchmark 3: docs/phase2/bench/bin/bench_release_rust
  Time (mean ± σ):     129.9 ms ±   5.7 ms    [User: 122.0 ms, System: 2.5 ms]
  Range (min … max):   121.7 ms … 145.1 ms    20 runs

Benchmark 4: docs/phase2/bench/bin/bench_release_go
  Time (mean ± σ):     200.0 ms ±  15.5 ms    [User: 190.4 ms, System: 9.0 ms]
  Range (min … max):   191.0 ms … 254.2 ms    15 runs

Benchmark 5: ruby docs/phase2/bench/bench_release_ruby.rb
  Time (mean ± σ):      1.150 s ±  0.031 s    [User: 1.046 s, System: 0.032 s]
  Range (min … max):    1.108 s …  1.216 s    10 runs

Benchmark 6: clj -M docs/phase2/bench/bench_release_clj.clj
  Time (mean ± σ):      1.289 s ±  0.061 s    [User: 2.644 s, System: 0.192 s]
  Range (min … max):    1.213 s …  1.387 s    10 runs

Summary
  docs/phase2/bench/bin/bench_release_rust ran
    1.54 ± 0.14 times faster than docs/phase2/bench/bin/bench_release_go
    2.79 ± 0.28 times faster than docs/phase2/bench/bin/bench_release_clove2
    8.85 ± 0.45 times faster than ruby docs/phase2/bench/bench_release_ruby.rb
    9.92 ± 0.64 times faster than clj -M docs/phase2/bench/bench_release_clj.clj
   10.99 ± 0.56 times faster than target/release/clove2 run docs/phase2/bench/bench_release_clove2.clv
```
