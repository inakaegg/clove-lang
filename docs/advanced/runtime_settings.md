# Runtime settings (use / use-syntax)

Japanese version: [runtime_settings.ja.md](runtime_settings.ja.md)

- Updated: 2025-12-21

Clove allows some features to be toggled via settings.

## 1. `use-syntax` / `use`

Controls reader-level extensions.

```clojure
(use-syntax dot-chain true)
(use-syntax indexer true)
(use-syntax dot-indexer true)
(use-syntax foreign-block true)
(use-syntax oop-syntax true)
(use-syntax map-refs true)
```

- `map-refs` can also be enabled via `(use map-refs true)` (`use` is an alias of `use-syntax`)
- `true` enables
- `false` disables
- `:scope` is deprecated and not accepted

Defaults may vary by environment, so if a team decides “always ON for this project”,
it is safer to set it explicitly in the entrypoint.

## 2. `repl-on-error`

Opens a REPL **at the error site**.

```clojure
(use repl-on-error true)
```

- Useful during debugging
- Recommended OFF for CI or non-interactive environments

In REPL, use `:continue` to resume.

## 3. Package config files (user.toml / user.clv)

Settings are loaded **per package**.
The package root is the first parent directory that contains `user.toml` or `user.clv`.
If none is found, it is treated as `<main>`.

Load order and priority:

1. `user.toml` (optional)
2. `user.clv` (optional; overrides)

### user.toml

```toml
[syntax]
dot-chain = true
dot-indexer = true
indexer = true
oop-syntax = true
map-refs = true
foreign = false
repl-on-error = true
```

- keys under `syntax` are feature names
- values are `true` / `false` only

### user.clv

Only **`(use <feature> true|false)`** is allowed.
Other forms are rejected.

```clojure
(use dot-chain false)
(use repl-on-error true)
```

- `user.clv` is **config-only** and is not evaluated; no `require` or arbitrary code
- Switching `std` (to another library) is not supported in `user.clv`; use explicit `require` in each `ns` if needed

---
<!-- NAV:START -->
**Previous:** [Memoization and persistent cache (memo / memoize)](memoization.md)
**Next:** [Dev environment setup](../contributing/dev_setup.md)
<!-- NAV:END -->

