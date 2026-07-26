# `mut` and `imut`

Japanese version: [mutability.ja.md](mutability.ja.md)

- Updated: 2026-07-25

## Decision

Collections are **persistent** by default. Updating returns a new value and
leaves the original untouched.

```bash
clove -e '(def w [1 2 3]) (println (assoc w 0 99) w)'
# [99 2 3] [1 2 3]
```

Destructive updates require an explicit `mut`. A `mut` value displays
differently and is updated in place by `assoc!` and friends.

```bash
clove -e '(def v (mut [1 2 3])) (println v) (assoc! v 0 99) (println v)'
# #<mut [1 2 3]>
# #<mut [99 2 3]>
```

## Reasoning

**The default should be the safe side.** Persistent data structures
structurally prevent a shared value from changing underneath its other holders.
They are also what readers expect from a Lisp.

**But some places need speed.** In a game loop updating every frame, allocating
a fresh collection per update dominates. Performance is exactly why the
[native build path](two-phase-implementation.md) exists, so a way to opt into
speed is required.

**Being explicit makes it an optimization contract.** `mut` is not merely "the
fast version" — it is a declaration that **the author takes responsibility for
the value not being shared**. Given that declaration, a compiler can update in
place safely.

So `mut` means three different things depending on the layer.

| Layer | What `mut` means |
| --- | --- |
| Interpreter | Perform a destructive update |
| Reader of the code | A statement that this value is not shared |
| Native build | A contract demanding in-place update (decided, see below) |

## Alternatives not taken

**Make everything mutable.** Straightforward performance, but the default
becomes the dangerous side. Losing track of where a shared value gets modified
costs more.

**Make everything immutable.** Simpler to implement, but removes the escape
hatch for performance — which contradicts the reason the native path exists.

**Distinguish only by a trailing `!`.** Having both `assoc` and `assoc!` is
Clojure-like, but on its own it does not put "this value is not shared" into the
type. Marking the value itself with `mut` is what lets the native build decide
statically.

**Introduce an `Imut<T>` type.** Naming the immutable side was considered and
dropped. Since immutability is the default, it needs no name; only the mutable
side is marked, as `Mut<T>` ([type hints](../language/type_hints.md)).

## Native build handling (not implemented)

The design that turns `mut` / `imut` into an optimization contract for the
native build is decided, but **`clove build` does not implement it yet** (as of
2026-07-25).

```bash
clove build app.clv    # source contains (mut [1 2 3])
# unsupported call in phase2 C build: mut
```

What has been decided, recorded here as guidance for whoever implements it:

- In a `mut` context, in-place update is **required**. If sharing cannot be
  proven absent, the build fails rather than quietly returning a new value
- In an `imut` context, values are immutable as observed. In-place optimization
  is permitted only where sharing can be proven absent
- Provide an explicit way to break sharing: an operation that allocates a fresh
  container and shallow-copies the elements, distinct from `vec` / `into` /
  `map`, which return new values without guaranteeing the absence of sharing
- The `mut` / `imut` boundary **cannot be switched at runtime** in a native build

The interpreter currently has no dedicated operation for breaking sharing
(`copy` is a different function and does not duplicate collections).

For the native path's overall coverage, see
[two implementations](two-phase-implementation.md) and
[docs/phase2/](../phase2/README.md).

## See also

- [Collections](../language/collections.md)
- [Type hints](../language/type_hints.md) — `Mut<T>`
