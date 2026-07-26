# Types stay optional annotations

Japanese version: [gradual-typing.ja.md](gradual-typing.ja.md)

- Updated: 2026-07-25

## Decision

Type annotations are **optional**. Code runs with or without them, and they are
**not used for runtime type checking**.

```clojure
(defn add [x y] (+ x y))                ; no annotations
(defn add :int [x<Int> y<Int>] (+ x y)) ; return and argument annotations
```

Annotations do not create a checked boundary.

```bash
clove -e '(defn add :int [x<Int> y<Int>] (+ x y)) (println (add "a" "b"))'
# Type mismatch: expected number (arg 1 to +), got str ("a")
```

The error comes from inside `+`, not from `add` receiving its arguments. An
annotation is a **declaration** that a value should be an `Int`, not a gate.

## Reasoning

Type information exists for three purposes.

**The LSP.** Narrowing completion candidates, showing signatures on hover, inlay
hints. More annotations mean better precision.

**[Native build optimization](two-phase-implementation.md).** The build path
runs HM type inference and lowers resolved types into specialized code. This is
the main performance mechanism behind `clove build`.

**Communication.** Stating a function's intent to the next reader.

All three reward writing annotations without requiring them. Runtime checking
would turn annotations into a **cost**: writing code dynamically first and
annotating once it settles fits how this language is used.

## Alternatives not taken

**Check annotations at runtime.** "You declared it, so it should be enforced."
Rejected. It imposes a permanent cost, and it means adding an annotation can
break working code. Annotations should be safe to add after the fact, so adding
one must not change behavior.

**Make types mandatory.** Better whole-program optimization and safety, but the
REPL stops being casual. The REPL and script execution are primary use cases.

**Have no types at all.** That gives up both LSP precision and native
optimization.

## Notation

Annotations go in two positions.

```clojure
(def x<Int> 10)                ; postfix on a symbol
(def v [1 2]: [Int Int])       ; postfix on an expression
(def m: Map<Str, Int> {"a" 1}) ; generics use commas
```

The postfix `<...>` form was chosen because it does not disturb S-expression
structure. Prefix or other positions would add special cases in both the reader
and the formatter. Full notation and the list of supported types are in
[type hints](../language/type_hints.md).

## What makes it gradual

Treating types as hints means all three of these hold for the same code:

1. Write it without annotations and run it in the REPL
2. Annotate the functions that have settled. Behavior does not change
3. Run `clove build` and let the annotations drive optimization. Only here are
   types treated strictly

At step 3, mismatched types fail the build. Strict checking is deliberately
pushed to the native build: loose in the interpreter, strict in the build.

```bash
clove build app.clv    # source has (defn add [x<Int> y<Int>] (+ x y)) and (add "a" "b")
# expected Int expression
```

The same failure occurs without annotations wherever inference reaches. An
annotation assists inference; it is not a switch that turns checking on.

> The native build currently **does not accept the return-type annotation**
> (`(defn add :int [...])`) and reports `syntax error: params must be a vector`.
> Postfix argument annotations work. See
> [two implementations](two-phase-implementation.md) for the native path's
> coverage.

## See also

- [Type hints](../language/type_hints.md) — the current notation
- [Types, enums, and match](../language/types_enum_match.md)
