# Map shorthand (JS-style)

Japanese version: [map_shorthand.ja.md](map_shorthand.ja.md)

## Goal

Following JS object literals, Clove adds a shorthand where values with the same name
as keys can be omitted, e.g. `{:a, :b}`. `clove fmt` / `pretty-print` keep the shorthand.

## Literal shorthand

- Basic form: `{:name, :age}` -> `{:name name :age age}`
- Label-like keys: `{name:, age:}` also expand the same way
- Comma is treated as whitespace, so JS-like comma separation works anywhere
- Special keys like `:keys` / `:as` are not eligible for shorthand (must provide values)

## Destructuring shorthand

- `{name, age}` binds like `{:keys [name age]}`
- Can be combined with regular syntax like `{:keys [x y], :as whole}` (commas allowed)
- Special keys `:keys` / `:as` themselves cannot be omitted
- Destructuring cannot rename them either — a binding name must be a symbol,
  a vector pattern, or a map pattern. To read those keys as data, use plain access:
  `(let [keys-val m[:keys] as-val m[:as]] ...)` (or `get` / calling the map)

## Formatter behavior

- Shorthand entries keep commas like `:a,`
- `:keys` / `:as` are never omitted (always emit values)

## Compatibility notes

- Since comma is treated as a separator (not part of symbol/keyword names),
  names containing `,` are not allowed.
- `{:keys, :as}` does **not** error and does **not** expand: because the comma is
  whitespace and these keys are excluded from the shorthand, it is read as the
  one-entry map `{:keys :as}`. Write the values explicitly when you mean a map
  with both keys.

---
<!-- NAV:START -->
**Previous:** [OOP methods quick reference (new syntax)](oop_methods_quickref.md)
**Next:** [Regex / Duration literals](regex_duration.md)
<!-- NAV:END -->

