# Writing docs

Japanese version: [docs_style.ja.md](docs_style.ja.md)

- Updated: 2025-12-21

## 1. Basics

- One page, one topic
- At the top, write 2–3 lines on “what you will learn”
- Clearly separate implemented vs not-yet-implemented

## 2. Example style

- Copy/paste runnable in REPL
- Avoid extra dependencies (other files)
- OOP examples are generated from examples in [data/clove_docs/clove-docs.json](/data/clove_docs/clove-docs.json)

## 3. Notation

- Use `clojure` in code blocks (for highlighting)
- Emphasize important notes with blockquotes like `> NOTE:`

## 4. Navigation

- Pages listed in `docs/index.md` must include Prev/Next navigation
- Order follows `docs/index.md` (or `docs/index.ja.md` for Japanese pages)
- Keep readers in the same language (English pages link to `.md`, Japanese pages link to `.ja.md`)

---
<!-- NAV:START -->
**Previous:** [Testing policy](testing.md)
**Next:** [FAQ](../faq.md)
<!-- NAV:END -->

