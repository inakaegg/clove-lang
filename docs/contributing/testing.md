# Testing policy

Japanese version: [testing.ja.md](testing.ja.md)

- Updated: 2025-12-21

## 1. Rust tests

```bash
cargo test
```

- Reader
- Eval
- Type inference (HM)
- fmt

## 2. Use examples as “behavior tests” (idea)

- Scan example files and `eval_file`
- Provide functions that return expected values

Decide this in repo policy and integrate into CI to reduce breakage.

---
<!-- NAV:START -->
**Previous:** [Repository layout](repo_layout.md)
**Next:** [How to write docs](docs_style.md)
<!-- NAV:END -->

