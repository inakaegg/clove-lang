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

- `records/bench_release_20260127.md`

## Example

```bash
clove2 build docs/phase2/bench/bench_release_clove2.clv --out target/clove2/bin/bench_release
/usr/bin/time -l target/clove2/bin/bench_release
```
