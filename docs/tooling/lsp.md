# LSP (clove-lsp)

Japanese version: [lsp.ja.md](lsp.ja.md)

- Updated: 2025-12-21

`clove-lsp` is a minimal LSP server for Clove.

## 1. Features (current)

- Diagnostics (syntax errors / undefined, etc.)
- Hover (doc / signature)
- Go to Definition

> Implementation is in progress. Complex namespace mixes (e.g., `/` separators)
> can reduce accuracy.

## 2. Usage (VS Code)

The VS Code extension launches `clove-lsp` and analyzes `.clv` files.

- [VS Code Extension](vscode.md)

## 3. Troubleshooting

- `ns` and file layout mismatch -> warnings / resolution failure
- Legacy `aaa/bbb` style -> definition tracking may break

See: [Namespaces](../language/namespaces.md)

---
<!-- NAV:START -->
**Previous:** [Build (--opt=typed / --static / embed)](build.md)
**Next:** [VS Code Extension](vscode.md)
<!-- NAV:END -->

