# Public Bench (Single Bundle)

Japanese version: [README.ja.md](README.ja.md)

This directory keeps **the minimal public benchmark set** for the public repo.

## Bench Code

- `bench_release_clove2.clv`
  - Each function is **repeated multiple times** to avoid single-call bias.
- Other language counterparts:
  - `bench_release_rust.rs`
  - `bench_release_go.go`
  - `bench_release_ruby.rb`
  - `bench_release_clj.clj`

## Records

- `records/bench_release_20260129.md`

## Example

```bash
clove build docs/phase2/bench/bench_release_clove2.clv --out target/clove/bin/bench_release
/usr/bin/time -l target/clove/bin/bench_release
```

The current C backend cannot compile this file yet: the `map` calls take lambdas with
`let` and collection operations in them, and the backend lowers only arithmetic lambdas
(`map lambda must be simple arithmetic`). The recorded numbers come from the earlier
implementation. See [Build known limitations](../../tooling/build.md) for the subset the
backend handles today, and run the file with `clove docs/phase2/bench/bench_release_clove2.clv`
to measure the interpreter.
