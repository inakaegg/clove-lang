# Glossary

Japanese version: [glossary.ja.md](glossary.ja.md)

- Updated: 2026-01-14

- **Form**: syntax tree node read by the reader (with source location)
- **Value**: runtime value handled by the evaluator
- **Reader**: layer that parses strings/files into Forms (expands sugar here)
- **special form**: syntax that is not treated as a “normal function call” by the evaluator
- **dot-chain**: chained stages with a “? placeholder” such as `x.(f ?)`
- **indexer**: access sugar like `x[0]`, `x[0 1]`, `x[0||d]`
- **foreign blocks**: embedding external languages like `$rb{...}`, `$py{...}`, `${...}` (Ruby shorthand)
- **promise/future/task/agent**: async/concurrency handles in Clove

---
<!-- NAV:START -->
**Previous:** [FAQ](faq.md)
<!-- NAV:END -->

