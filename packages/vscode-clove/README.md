# Clove for VS Code

VS Code extension for the Clove language.

- Syntax highlighting for `.clv`
- Structural S-expression selection
- REPL send (`clove --repl`)
- Formatting via `clove fmt`
- Embedded Ruby/Python block selection + Ruby completion
- Light Table-style theme

> This README is for the VS Code extension. For the language itself, see the root `README.md`, `README.ja.md`, and [docs/](/docs/).

---

## Features

### 1. Language support

- Language ID: `clove`
- File extension:
  - `.clv`
- Uses `syntaxes/clove.tmLanguage.json` and `language-configuration.json`:
  - Syntax highlighting (`source.clove`)
  - Bracket pairs / auto-closing
  - Line comments with `;`

### 2. Structural S-expression selection

Provides Paredit-style structural selection using a small S-expression parser and Tree-sitter.

**Commands**

- `Clove: Expand Selection` (`clove.expandSelection`)
- `Clove: Shrink Selection` (`clove.shrinkSelection`)

**Default keybindings**

- Expand: `Shift+Alt+Right`
- Shrink: `Shift+Alt+Left`
- Jump: `Ctrl+Shift+\` (mac: `Cmd+Shift+\`)

**Notes**

- Selection expands/shrinks by **syntax units**.
- Cursor on whitespace snaps to the nearest token.
- On `)` / `]` / `}`, expansion aligns with the previous element.
- Recognizes embedded blocks: `$rb{...}` / `$py{...}` / `${...}`.
  - `${...}` is the Ruby shorthand.
  - Uses Tree-sitter Ruby/Python parsers when available.
  - Falls back to S-expression selection if parser loading fails.

If selection looks off, check the debug log described below.

### 3. REPL integration

**Command**

- `Clove: Send Selection to REPL` (`clove.sendSelection`)

**Behavior**

- If REPL is not running: starts a terminal (does not send on first run).
- If there is a selection: sends it as one block.
- If there is no selection: sends the **current form** at the cursor.
- Starts a dedicated terminal named **`Clove REPL`** as needed:
  - `shellPath: "clove"`
  - `shellArgs: ["--repl"]`
- When a file is open, also sends "source" info (file path, etc.) for error mapping.

**Requirements**

- `clove` is on `PATH`.
- `clove --repl` can be launched.

### 4. Document formatting (`clove fmt`)

Supports VS Code **Format Document**.

1. If there is no selection, format the whole document.
2. If there are selections, format each selection (falls back to whole document if they overlap).
3. Runs `clove fmt --stdin` (uses `clovefmt.toml` if present).

Default command:

```bash
clove fmt --stdin
```

**Formatter Preview**

- `Clove: Formatter Preview` shows before/after diff.
- The same preview URI is reused.

If the formatter fails, `clove fmt failed: ...` is shown and the document is not modified.

### 5. Embedded Ruby / Python support

Clove supports embedded blocks like `$rb{...}` / `$py{...}` / `${...}`.
`${...}` is the Ruby shorthand.
The extension recognizes these blocks and assists with selection and completion.

#### 5.1 Selection and navigation

- `$rb{...}` / `${...}` are treated as **Ruby** (`${...}` is shorthand).
- `$py{...}` is treated as **Python**.

When Tree-sitter is available, selection inside embedded blocks is kept as natural as possible.
If parser loading fails, it falls back to S-expression selection.

#### 5.2 Embedded Ruby completion

**How to use**

- Regular completion (`Ctrl+Space` / `Ctrl+Space`)
- Target blocks:
  - `$rb{ ... }`
  - `${ ... }`

**Behavior**

1. Extracts the embedded Ruby block.
2. Builds a virtual Ruby document.
3. Queries VS Code's Ruby completion provider.
4. Maps results back into the original Clove document.

**Limitations**

- Ruby completion only (Python completion is not supported).
- No completion if a Ruby extension is not installed.

### 6. LSP (hover / completion / refactor)

With the experimental **`clove-lsp`**, the following are enabled.

**Main features**

- Built-in + workspace function completion (signature + docs/examples)
- Hover renders the same Markdown
- Go-to-definition (including built-in stubs)
- Signature help on `(` and spaces
- Access syntax refactors

**Requirements**

- `clove-lsp` binary is available:
  - `cargo install --path crates/clove-lsp`
  - or specify `target/debug/clove-lsp` after `cargo build -p clove-lsp`
- VS Code can spawn the binary.
- LSP works only on `file` documents.
  - Save unsaved buffers first.

**Settings**

- `clove.lsp.serverPath` (string, default: `"clove-lsp"`)
  - Execution path or command name
  - Supports `~` expansion
- `clove.suggest.autoShowDocumentation` (boolean, default: `true`)
  - Auto-opens the completion docs pane
- `clove.refactor.access.preferKeywordChain` (boolean, default: `true`)
  - Prefer `m:kw` / `m:a:b` when refactoring access syntax without a default

Startup logs appear in the **"Clove LSP"** output channel.

**Refactor commands**

- `Clove: Refactor - Rewrite to Shortest Access` (`clove.refactor.shortestAccess`)
- `Clove: Refactor - Rewrite to Canonical Access` (`clove.refactor.canonicalAccess`)
- `Clove: Refactor - Toggle S-exp / OOP (Safe)` (`clove.refactor.toggleOop`)
- `Clove: Refactor - Toggle Access Syntax` (`clove.refactor.toggleAccess`)

> Because it hooks `editor.action.triggerParameterHints`, hints pop right after a function head is typed.
> If you do not want that, close them with `Esc`.

### 7. Selection debug log

**Command**

- `Clove: Show Selection Debug Log` (`clove.showSelectionDebugLog`)

Emits internal logs such as:

- Document version
- Selection changes
- Embedded language detection state

If selection behaves unexpectedly, include this log and a minimal repro snippet.

### 8. Light Table-style theme

The extension ships an optional dark theme.

- `Clove Default (LightTable)`

### 8.1 Keep Dark Modern UI and only change colors

If you want to keep VS Code's **Default Dark Modern** UI and only tint colors toward Clove,
add the following to `settings.json`.

```json
"editor.tokenColorCustomizations": {
  "[Default Dark Modern]": {
    "textMateRules": [
      {
        "scope": [
          "source.clove punctuation.section.list.begin.clove",
          "source.clove punctuation.section.list.end.clove",
          "source.clove meta.brace",
          "source.clove meta.bracket"
        ],
        "settings": { "foreground": "#c4c4c4" }
      },
      {
        "scope": [
          "source.clove variable.other.clove",
          "source.clove variable.parameter.clove",
          "source.clove meta.symbol.clove"
        ],
        "settings": { "foreground": "#e2e2e2" }
      },
      {
        "scope": [
          "source.clove string.quoted.double.clove",
          "source.clove string.quoted.other.clove"
        ],
        "settings": { "foreground": "#7ee8e8" }
      },
      {
        "scope": [
          "source.clove constant.numeric.clove"
        ],
        "settings": { "foreground": "#e0e0e0" }
      },
      {
        "scope": [
          "source.clove entity.name.function.clove",
          "source.clove meta.definition.clove"
        ],
        "settings": { "foreground": "#8fc1ff" }
      },
      {
        "scope": [
          "source.clove support.function.clove",
          "source.clove support.variable.clove",
          "source.clove keyword.control.clove",
          "source.clove storage.type.clove"
        ],
        "settings": { "foreground": "#7ff0c4" }
      },
      {
        "scope": [
          "source.clove entity.name.type.clove",
          "source.clove entity.name.type",
          "source.clove storage.type",
          "source.clove support.type.clove",
          "source.clove support.class.clove"
        ],
        "settings": { "foreground": "#4F92FF" }
      },
      {
        "scope": [
          "source.clove constant.language.clove"
        ],
        "settings": { "foreground": "#d6a2ff" }
      },
      {
        "scope": [
          "source.clove comment.line.semicolon.clove",
          "source.clove comment.block.clove"
        ],
        "settings": { "foreground": "#7fa9cc" }
      },
      {
        "scope": [
          "source.clove punctuation.section.embedded.begin.clove",
          "source.clove punctuation.section.embedded.end.clove"
        ],
        "settings": { "foreground": "#c4c4c4" }
      }
    ]
  }
},
"workbench.colorCustomizations": {
  "[Default Dark Modern]": {
    "editorBracketHighlight.foreground1": "#cfcfcf",
    "editorBracketHighlight.foreground2": "#00ff4d",
    "editorBracketHighlight.foreground3": "#00ffa0",
    "editorBracketHighlight.foreground4": "#00ffff",
    "editorBracketHighlight.foreground5": "#9c7fff",
    "editorBracketHighlight.foreground6": "#ff33ff",
    "editorBracketHighlight.unexpectedBracket.foreground": "#ff6666",
    "editorBracketMatch.border": "#40ffff",
    "editorBracketMatch.background": "#1b1b1b"
  }
}
```

**Notes**

- `workbench.colorCustomizations` affects all languages.
  Omit this block if you want it to apply only to Clove.
- Adjust the type color `#4F92FF` to your preference.

---

## Command list

| Command ID                       | Title                                      | Default key         |
| ------------------------------ | ----------------------------------------- | ------------------- |
| `clove.expandSelection`        | Clove: Expand Selection                   | `Shift+Alt+Right`   |
| `clove.shrinkSelection`        | Clove: Shrink Selection                   | `Shift+Alt+Left`    |
| `clove.jumpToMatchingBracket`  | Clove: Jump to Matching Bracket           | `Ctrl+Shift+\`      |
| `clove.sendSelection`          | Clove: Send Selection to REPL             | *(none)*            |
| `clove.showSelectionDebugLog`  | Clove: Show Selection Debug Log           | *(none)*            |
| `clove.formatterPreview`       | Clove: Formatter Preview                  | *(none)*            |
| `clove.refactor.shortestAccess`| Clove: Refactor - Rewrite to Shortest Access | *(none)*        |
| `clove.refactor.canonicalAccess`| Clove: Refactor - Rewrite to Canonical Access | *(none)*      |
| `clove.refactor.toggleOop`     | Clove: Refactor - Toggle S-exp / OOP (Safe) | *(none)*        |
| `clove.refactor.toggleAccess`  | Clove: Refactor - Toggle Access Syntax    | *(none)*            |

You can change bindings by searching `clove.` in `File -> Preferences -> Keyboard Shortcuts`.

---

## Requirements

- **Clove CLI**
  - `clove` is on `PATH`
  - `clove --repl` can be launched
  - `clove fmt --stdin` accepts stdin
- **Ruby (for completion)**
  - An extension that provides `language: "ruby"` completion is required

The extension bundles Tree-sitter and required JS/TS assets; no extra install is needed.

---

## Getting started

1. Install the extension
2. Put `clove` on `PATH`
3. Open a `.clv` file
4. Try:

- `Shift+Alt+Right` / `Shift+Alt+Left` for S-expression selection
- `Clove: Send Selection to REPL` to send to REPL
- `Format Document` (`Shift+Alt+F`) to format
- Trigger completion inside `$rb{ ... }`

If you run into issues, open `Clove: Show Selection Debug Log` and report it with a minimal repro.

---

## Development

```bash
npm install
npm run compile
```

Press `F5` to launch the Extension Development Host.
Release builds are generated into `dist/` via `npm run vscode:prepublish`.
