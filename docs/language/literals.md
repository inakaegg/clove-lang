# Literals

Japanese version: [literals.ja.md](literals.ja.md)

- Updated: 2025-12-21

## 1. Numbers

### 1.1 int

- `0`, `42`, `-10`
- Separator: `1_000_000`

### 1.2 float

- `0.5`, `-3.14`

> The `Duration` literal currently supports **integers only**.
> For fractional seconds, use a function like `(duration 0.1 :s)`.

## 2. Strings

- `"..."`
- String interpolation: `"hello #{name}"`

Interpolation is expanded by the reader into `str` concatenation.

## 3. Keywords

- `:foo`
- Forms like `:ns/foo` can be read by the reader, but in Clove
  we recommend `::` as the namespace separator.

## 4. Symbols

- `foo`, `core::map`, `my.ns::f`

`::` is the namespace separator in Clove.

## 5. nil / bool

- `nil`
- `true`, `false`

## 6. regex / duration

See: [Regex / Duration](regex_duration.md)

---
<!-- NAV:START -->
**Previous:** [Reader / Syntax Overview](reader_syntax.md)
**Next:** [Collections (list/vector/map/set)](collections.md)
<!-- NAV:END -->

