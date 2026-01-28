# bench_release 2026-01-27

## Summary (hyperfine mean + max RSS)

| Language | Output | mean(s) | max RSS (MiB) | Command |
| --- | --- | --- | --- | --- |
| clove2 run(vm) | 6850020200192 | 1.95 | 228.3 MiB | `target/release/clove2 run docs/phase2/bench/bench_release_clove2.clv` |
| clove2 | 6850020200192 | 0.4664 | 52.3 MiB | `docs/phase2/bench/bin/bench_release_clove2` |
| rust | 6850020200192 | 0.1035 | 2.0 MiB | `docs/phase2/bench/bin/bench_release_rust` |
| go | 6850020200192 | 0.1332 | 8.8 MiB | `docs/phase2/bench/bin/bench_release_go` |
| ruby | 6850020200192 | 0.9891 | 87.0 MiB | `ruby docs/phase2/bench/bench_release_ruby.rb` |
| clj | 6850020200192 | 1.230 | 602.3 MiB | `clj -M docs/phase2/bench/bench_release_clj.clj` |

Note: max RSS is measured by `/usr/bin/time -l`.
Note: mean values for clove2 run(vm) / Ruby / Clj are from `/usr/bin/time -l` (hyperfine not measured).

## Ratios (clove2 / others)

- vs Rust: **4.51x**
- vs Go: **3.50x**
- vs Ruby: **0.47x** (clove2 is faster)
- vs Clj: **0.38x** (clove2 is faster)

## Reference (clove2 run(vm) / others)

- vs Ruby: **1.84x** (slower)
- vs Clj: **1.26x** (slower)

## Source Files

- [docs/phase2/bench/bench_release_clove2.clv](/docs/phase2/bench/bench_release_clove2.clv)
- [docs/phase2/bench/bench_release_rust.rs](/docs/phase2/bench/bench_release_rust.rs)
- [docs/phase2/bench/bench_release_go.go](/docs/phase2/bench/bench_release_go.go)
- [docs/phase2/bench/bench_release_ruby.rb](/docs/phase2/bench/bench_release_ruby.rb)
- [docs/phase2/bench/bench_release_clj.clj](/docs/phase2/bench/bench_release_clj.clj)

## /usr/bin/time -l (max RSS)

| Language | Output | real(s) | max RSS (MiB) | Command |
| --- | --- | --- | --- | --- |
| clove2 run(vm) | 6850020200192 | 1.95 | 228.3 MiB | `target/release/clove2 run docs/phase2/bench/bench_release_clove2.clv` |
| clove2 | 6850020200192 | 0.45 | 52.3 MiB | `docs/phase2/bench/bin/bench_release_clove2` |
| rust | 6850020200192 | 0.51 | 2.0 MiB | `docs/phase2/bench/bin/bench_release_rust` |
| go | 6850020200192 | 0.84 | 8.8 MiB | `docs/phase2/bench/bin/bench_release_go` |
| ruby | 6850020200192 | 1.00 | 87.0 MiB | `ruby docs/phase2/bench/bench_release_ruby.rb` |
| clj | 6850020200192 | 1.55 | 602.3 MiB | `clj -M docs/phase2/bench/bench_release_clj.clj` |

### clove2 build (/usr/bin/time -l)

| Output | real(s) | max RSS (MiB) | Command |
| --- | --- | --- | --- |
| `docs/phase2/bench/bin/bench_release_clove2` | 0.01 | 5.1 MiB | `target/release/clove2 build docs/phase2/bench/bench_release_clove2.clv --out docs/phase2/bench/bin/bench_release_clove2 --emit-rust` |

## hyperfine

```
Benchmark 1: docs/phase2/bench/bin/bench_release_clove2
  Time (mean ± σ):     466.4 ms ±   5.9 ms    [User: 449.5 ms, System: 11.7 ms]
  Range (min … max):   461.0 ms … 475.7 ms    5 runs
 
Benchmark 2: docs/phase2/bench/bin/bench_release_rust
  Time (mean ± σ):     103.5 ms ±   2.9 ms    [User: 99.4 ms, System: 2.2 ms]
  Range (min … max):   100.4 ms … 106.7 ms    5 runs
 
Benchmark 3: docs/phase2/bench/bin/bench_release_go
  Time (mean ± σ):     133.2 ms ±   1.0 ms    [User: 129.5 ms, System: 7.4 ms]
  Range (min … max):   131.9 ms … 134.5 ms    5 runs
 
Benchmark 4: ruby docs/phase2/bench/bench_release_ruby.rb
  Time (mean ± σ):     989.1 ms ±  12.6 ms    [User: 899.6 ms, System: 36.9 ms]
  Range (min … max):   978.4 ms … 1009.0 ms    5 runs
 
Benchmark 5: clj -M docs/phase2/bench/bench_release_clj.clj
  Time (mean ± σ):      1.230 s ±  0.114 s    [User: 2.618 s, System: 0.177 s]
  Range (min … max):    1.121 s …  1.420 s    5 runs
 
Summary
  docs/phase2/bench/bin/bench_release_rust ran
    1.29 ± 0.04 times faster than docs/phase2/bench/bin/bench_release_go
    4.51 ± 0.14 times faster than docs/phase2/bench/bin/bench_release_clove2
    9.56 ± 0.29 times faster than ruby docs/phase2/bench/bench_release_ruby.rb
   11.89 ± 1.15 times faster than clj -M docs/phase2/bench/bench_release_clj.clj
```
