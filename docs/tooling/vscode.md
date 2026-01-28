# VS Code extension

Japanese version: [vscode.ja.md](vscode.ja.md)

This document summarizes features and usage of the VS Code extension in
[packages/vscode-clove](/packages/vscode-clove).

* Syntax highlighting
* Structural S-expression selection (expand / shrink)
* REPL integration
* `clove fmt` integration
* Embedded Ruby / Python support
* Light Table–style theme

See also [packages/vscode-clove/README.md](/packages/vscode-clove/README.md) for the extension README.

---

## 1. Language registration

* Language ID: `clove`
* File extensions:

  * `.clv`

`language-configuration.json` / `clove.tmLanguage.json` define:

* comment: `;`
* bracket pairs: `()`, `[]`, `{}`
* strings: `"` ... `"`
* syntax highlighting

---

## 2. Structural S-expression selection

### 2.1 Commands

* `Clove: Expand Selection` (`clove.expandSelection`)
* `Clove: Shrink Selection` (`clove.shrinkSelection`)

Default keys (example):

* Expand: `Shift+Alt+Right`
* Shrink: `Shift+Alt+Left`

> Enabled only when `editorLangId == "clove"`.

### 2.2 Behavior

* Expands in order: symbol -> within form -> whole list -> outer list ...
* Works even if cursor is just before the closing `)`
* Recognizes embedded forms like `$rb{...}`, `$py{...}`, `${...}` (Ruby shorthand), and

  * uses Tree-sitter Ruby / Python if available to structure inner content
  * falls back to S-expression behavior if parser cannot be loaded

For debugging, use `Clove: Show Selection Debug Log` to view node info and range changes.

---

## 3. REPL integration

### 3.1 Command

* `Clove: Send Selection to REPL` (`clove.sendSelection`)

### 3.2 Behavior

1. Extract selected text or the form at cursor from the active Clove document.
2. Find a terminal named like `Clove REPL`, or create one with:

   * `shellPath`: `clove`
   * `shellArgs`: `["--repl"]`
3. Send the extracted code to REPL via “bracketed paste” and evaluate.

If the file is saved, it sends `:source` info to REPL so error locations show file name.

---

## 4. Formatter integration (`clove fmt`)

Integrates VS Code format with `clove fmt`.

* On “Format Document”:

  1. Read document content
  2. Invoke configured command (default: `clove`) with `fmt --stdin --indent <indentWidth>`
  3. Replace document with stdout

Extension settings:

* `clove.format.command` ... command to run (`clove` / absolute path)
* `clove.format.indentWidth` ... indent hint (default 2)

---

## 5. Embedded Ruby / Python support

### 5.1 Selection behavior

These forms are treated as “embedded code”:

* `$rb{ ... }` / `${ ... }` ... Ruby (`${ ... }` is shorthand)
* `$py{ ... }` ... Python

For structural selection:

* Clove first recognizes the embedded block range
* Inside it, Tree-sitter Ruby / Python is used to expand/shrink by natural units

### 5.2 Ruby completion

Ruby embedding supports invoking Ruby completion inside the block.

* Requirements:

  * A Ruby language server extension must be installed in VS Code
* Mechanism:

  1. Treat embedded Ruby block content as a temporary “virtual Ruby file”
  2. Request completion from Ruby language server
  3. Map results back into the Clove document position

Python currently supports only “structured selection”; language server integration
is a future extension.

---

## 6. Light Table–style theme

The extension includes a Light Table–style dark theme.

* Theme name example: `Clove Default (LightTable)`

Characteristics:

* Dark background
* Bracket colors vary by nesting
* Higher contrast between symbols and brackets to help S-expression structure

### 6.1 Keep Dark Modern UI but apply Clove colors only

To keep **Default Dark Modern** UI while applying only Clove colors
(including rainbow brackets), add the following to `settings.json`.

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

Notes:

* Bracket colors affect the whole editor (all languages).
  If you want only Clove, remove `workbench.colorCustomizations`.
* To change type color, replace `#4F92FF`.

---

## 7. Troubleshooting

* Structural selection is wrong:

  * Run `Clove: Show Selection Debug Log` and check logs
  * File an issue with a small repro snippet
* Formatting fails:

  * Try `clove fmt --stdin` in a terminal to see the error
  * Check `clove.format.command` path

---

## 8. Future extensions

* LSP server integration (completion / go-to-definition / hover, etc.)
* Task runner integration for `clove build` / `clove test`
* Language server for embedded Python
* REPL variable sync and inline value display

There is still plenty of room to grow editor integration.

---
<!-- NAV:START -->
**Previous:** [LSP (clove-lsp)](lsp.md)
**Next:** [Troubleshooting](troubleshooting.md)
<!-- NAV:END -->

