import * as vscode from 'vscode';
import * as cp from 'child_process';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
} from 'vscode-languageclient/node';
import * as os from 'os';
import * as path from 'path';
import * as fs from 'fs';
import { OffsetRange, SelectionOptions, SexpNavigator } from './sexpSelection';
import { getEmbeddedParser, initEmbedParsers } from './embedParsers';
import { logSelectionDebug, showSelectionDebugOutput } from './logger';

type SelectionTransform = (navigator: SexpNavigator, range: OffsetRange) => OffsetRange | undefined;

interface HistoryEntry {
  version: number;
  stacks: OffsetRange[][];
}

const historyStore = new WeakMap<vscode.TextDocument, HistoryEntry>();
const navigatorCache = new WeakMap<vscode.TextDocument, { version: number; navigator: SexpNavigator }>();
const SUGGEST_DETAILS_STATE_KEY = 'clove.suggestDetailsInitialized';
const SIGNATURE_HINT_TRIGGERS = new Set(['(', ' ']);
const FORMATTER_PREVIEW_SCHEME = 'clove-fmt-preview';
const formatterPreviewContent = new Map<string, string>();
const formatterPreviewEmitter = new vscode.EventEmitter<vscode.Uri>();
const EMBEDDED_RUBY_SCHEME = 'clove-embedded-ruby';
const embeddedRubyContent = new Map<string, string>();
const embeddedRubyEmitter = new vscode.EventEmitter<vscode.Uri>();

interface RubyEmbedContext {
  head: string;
  start: number;
  end: number;
}

let lspClient: LanguageClient | null = null;
let lspOutput: vscode.OutputChannel | null = null;

export function activate(context: vscode.ExtensionContext) {
  let replTerminal: vscode.Terminal | undefined;

  logSelectionDebug('activate: extension initialization started');
  lspOutput = vscode.window.createOutputChannel('Clove LSP');
  lspOutput.appendLine(`activate: cwd=${process.cwd()}`);

  // Share initialization state for embedded-language parsers.
  const embedParsersReady = initEmbedParsers(context);
  let embedParserWarningShown = false;

  // Start LSP client.
  lspClient = startLanguageClient(context);
  void ensureSuggestDetailsVisible(context);

  // Log language info when a document is opened.
  context.subscriptions.push(
    vscode.workspace.onDidOpenTextDocument((doc) => {
      if (lspOutput) {
        lspOutput.appendLine(
          `onDidOpen: ${doc.uri.toString()} lang=${doc.languageId} scheme=${doc.uri.scheme}`
        );
      }
    })
  );

  context.subscriptions.push(registerSignatureHelpAutoTrigger());
  context.subscriptions.push(
    vscode.workspace.onDidChangeConfiguration((event) => {
      if (event.affectsConfiguration('clove.suggest.autoShowDocumentation')) {
        const config = vscode.workspace.getConfiguration('clove');
        const enabled = config.get<boolean>('suggest.autoShowDocumentation', true);
        if (enabled) {
          void ensureSuggestDetailsVisible(context, true);
        } else {
          void context.globalState.update(SUGGEST_DETAILS_STATE_KEY, false);
        }
      }
    })
  );

  const ensureEmbedParsersReady = async () => {
    logSelectionDebug('ensureEmbedParsersReady: awaiting parser init');
    await embedParsersReady;
    const rubyReady = !!getEmbeddedParser('ruby');
    const pythonReady = !!getEmbeddedParser('python');
    logSelectionDebug(
      `ensureEmbedParsersReady: ruby parser ready=${rubyReady} python parser ready=${pythonReady}`
    );
    if (!embedParserWarningShown && (!rubyReady || !pythonReady)) {
      embedParserWarningShown = true;
      logSelectionDebug('ensureEmbedParsersReady: missing embedded parsers detected');
      const missing: string[] = [];
      if (!rubyReady) missing.push('Ruby');
      if (!pythonReady) missing.push('Python');
      void vscode.window.showWarningMessage(
        `Clove: embedded selection for ${missing.join(', ')} cannot use tree-sitter; falling back.`
      );
    }
  };

  context.subscriptions.push(
    vscode.commands.registerTextEditorCommand('clove.expandSelection', async (editor) => {
      logSelectionDebug('command: expandSelection invoked');
      await ensureEmbedParsersReady();
      applyTransform(editor, (nav, range) => nav.expand(range), 'expand');
    })
  );

  context.subscriptions.push(
    vscode.commands.registerTextEditorCommand('clove.shrinkSelection', async (editor) => {
      logSelectionDebug('command: shrinkSelection invoked');
      await ensureEmbedParsersReady();
      applyTransform(editor, (nav, range) => nav.shrink(range), 'shrink');
    })
  );

  context.subscriptions.push(
    vscode.commands.registerTextEditorCommand('clove.jumpToMatchingBracket', (editor) => {
      const document = editor.document;
      const navigator = getNavigator(document);
      const updated: vscode.Selection[] = [];
      let moved = false;

      for (const selection of editor.selections) {
        const offset = document.offsetAt(selection.active);
        const target = navigator.findMatchingBracketOffset(offset);
        if (target === undefined) {
          updated.push(selection);
          continue;
        }
        const position = document.positionAt(target);
        updated.push(new vscode.Selection(position, position));
        moved = true;
      }

      if (moved) {
        editor.selections = updated;
        return;
      }
      void vscode.commands.executeCommand('editor.action.jumpToBracket');
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand('clove.showSelectionDebugLog', () => {
      logSelectionDebug('command: showSelectionDebugLog invoked');
      showSelectionDebugOutput();
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand('clove.refactor.shortestAccess', async () => {
      await runRefactorAction('refactor.rewrite.clove.shortestAccess');
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand('clove.refactor.canonicalAccess', async () => {
      await runRefactorAction('refactor.rewrite.clove.canonicalAccess');
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand('clove.refactor.toggleOop', async () => {
      await runRefactorAction('refactor.rewrite.clove.toggleOop');
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand('clove.refactor.toggleAccess', async () => {
      await runRefactorAction('refactor.rewrite.clove.toggleAccess');
    })
  );

  context.subscriptions.push(
    vscode.commands.registerTextEditorCommand('clove.sendSelection', async (editor) => {
      const document = editor.document;
      const selectionOptions = getSelectionOptions();
      const navigator = new SexpNavigator(document.getText(), selectionOptions);
      const selection = editor.selection;
      const range = selection.isEmpty
        ? climbToForm(navigator, selectionToRange(document, selection))
        : selectionToRange(document, selection);
      const raw = document.getText(rangeToRange(document, range));
      if (!raw.trim()) {
        vscode.window.showInformationMessage('No code to send.');
        return;
      }
      const selectionStart = document.positionAt(range.start);
      const code = ensureTrailingNewline(raw);
      const isNewRepl = !replTerminal || replTerminal.exitStatus;
      const handle = ensureReplTerminal(replTerminal);
      replTerminal = handle.terminal;
      replTerminal.show();
      if (isNewRepl) {
        return;
      }
      await handle.ready;
      if (document.uri.scheme === 'file') {
        const line = selectionStart.line + 1;
        const col = selectionStart.character + 1;
        replTerminal.sendText(`:source ${document.fileName} ${line} ${col}`, true);
      }
      replTerminal.sendText(code, false);
    })
  );

  context.subscriptions.push(
    vscode.languages.registerDocumentFormattingEditProvider({ language: 'clove' }, {
      provideDocumentFormattingEdits(document) {
        return formatDocument(document);
      }
    })
  );

  context.subscriptions.push(formatterPreviewEmitter);
  const previewProvider: vscode.TextDocumentContentProvider = {
    onDidChange: formatterPreviewEmitter.event,
    provideTextDocumentContent(uri) {
      return formatterPreviewContent.get(uri.toString()) ?? '';
    }
  };
  context.subscriptions.push(
    vscode.workspace.registerTextDocumentContentProvider(FORMATTER_PREVIEW_SCHEME, previewProvider)
  );

  context.subscriptions.push(embeddedRubyEmitter);
  const embeddedRubyProvider: vscode.TextDocumentContentProvider = {
    onDidChange: embeddedRubyEmitter.event,
    provideTextDocumentContent(uri) {
      return embeddedRubyContent.get(uri.toString()) ?? '';
    }
  };
  context.subscriptions.push(
    vscode.workspace.registerTextDocumentContentProvider(EMBEDDED_RUBY_SCHEME, embeddedRubyProvider)
  );
  context.subscriptions.push(
    vscode.workspace.onDidCloseTextDocument((doc) => {
      if (doc.uri.scheme !== EMBEDDED_RUBY_SCHEME) {
        return;
      }
      embeddedRubyContent.delete(doc.uri.toString());
    })
  );

  context.subscriptions.push(
    vscode.commands.registerCommand('clove.formatterPreview', async () => {
      const editor = vscode.window.activeTextEditor;
      if (!editor) {
        vscode.window.showInformationMessage('No editor found to preview.');
        return;
      }
      try {
        const previewUri = await updateFormatterPreview(editor.document);
        await vscode.commands.executeCommand(
          'vscode.diff',
          editor.document.uri,
          previewUri,
          'Clove Formatter Preview'
        );
      } catch (error) {
        const message = error instanceof Error ? error.message : String(error);
        vscode.window.showErrorMessage(`clove fmt preview failed: ${message}`);
      }
    })
  );

  context.subscriptions.push(
    vscode.languages.registerCompletionItemProvider(
      { language: 'clove' },
      new EmbeddedRubyCompletionProvider(),
      '.',
      ':'
    )
  );
}

export async function deactivate() {
  if (lspClient) {
    await lspClient.stop();
    lspClient = null;
  }
}

function applyTransform(
  editor: vscode.TextEditor,
  transform: SelectionTransform,
  direction: 'expand' | 'shrink'
) {
  const document = editor.document;
  const selectionOptions = getSelectionOptions();
  const navigator = new SexpNavigator(document.getText(), selectionOptions);
  const history = ensureHistory(document, editor.selections.length);
  const updated: vscode.Selection[] = [];
  logSelectionDebug(
    `applyTransform: direction=${direction} selectionCount=${editor.selections.length} version=${document.version}`
  );

  editor.selections.forEach((selection, index) => {
    const currentRange = selectionToRange(document, selection);
    logSelectionDebug(
      `applyTransform: index=${index} before=[${currentRange.start}, ${currentRange.end}] isEmpty=${selection.isEmpty}`
    );

    if (direction === 'shrink') {
      const previous = history.stacks[index]?.pop();
      if (previous) {
        logSelectionDebug(
          `applyTransform: shrink returning history range=[${previous.start}, ${previous.end}]`
        );
        updated.push(rangeToSelection(document, previous));
        return;
      }
    }

    const next = transform(navigator, currentRange);
    if (!next || sameRange(next, currentRange)) {
      logSelectionDebug('applyTransform: no change in range');
      updated.push(selection);
      return;
    }

    if (direction === 'expand') {
      history.stacks[index].push(currentRange);
    }

    logSelectionDebug(`applyTransform: result=[${next.start}, ${next.end}]`);
    updated.push(rangeToSelection(document, next));
  });

  editor.selections = updated;
}

function ensureHistory(document: vscode.TextDocument, selectionCount: number): HistoryEntry {
  const currentVersion = document.version;
  let entry = historyStore.get(document);
  if (!entry || entry.version !== currentVersion) {
    entry = {
      version: currentVersion,
      stacks: createStacks(selectionCount)
    };
    historyStore.set(document, entry);
    return entry;
  }
  if (entry.stacks.length !== selectionCount) {
    entry.stacks = createStacks(selectionCount);
  }
  return entry;
}

function createStacks(count: number): OffsetRange[][] {
  return Array.from({ length: count }, () => []);
}

function selectionToRange(document: vscode.TextDocument, selection: vscode.Selection): OffsetRange {
  return {
    start: document.offsetAt(selection.start),
    end: document.offsetAt(selection.end)
  };
}

function rangeToRange(document: vscode.TextDocument, range: OffsetRange): vscode.Range {
  return new vscode.Range(document.positionAt(range.start), document.positionAt(range.end));
}

function rangeToSelection(document: vscode.TextDocument, range: OffsetRange): vscode.Selection {
  const start = document.positionAt(range.start);
  const end = document.positionAt(range.end);
  return new vscode.Selection(start, end);
}

function sameRange(a: OffsetRange, b: OffsetRange): boolean {
  return a.start === b.start && a.end === b.end;
}

function getSelectionOptions(): SelectionOptions {
  const config = vscode.workspace.getConfiguration('clove');
  const hyphenExpansion = config.get<string>('selection.hyphenExpansion', 'segment');
  return {
    hyphenExpansion: hyphenExpansion === 'full' ? 'full' : 'segment'
  };
}

async function formatDocument(document: vscode.TextDocument): Promise<vscode.TextEdit[]> {
  const command = 'clove';
  const args = ['fmt', '--stdin'];
  const ranges = getFormattingRanges(document);

  try {
    if (ranges.length === 1 && isFullDocumentRange(document, ranges[0])) {
      const formatted = await runFormatter(command, args, document.getText(), document);
      const normalized = ensureTrailingNewline(formatted);
      return [vscode.TextEdit.replace(ranges[0], normalized)];
    }
    const edits: vscode.TextEdit[] = [];
    for (const range of ranges) {
      const raw = document.getText(range);
      if (!raw.trim()) {
        continue;
      }
      const formatted = await runFormatter(command, args, raw, document);
      const normalized = normalizeSelectionOutput(formatted, raw);
      const adjusted = applySelectionIndent(document, range, normalized);
      edits.push(vscode.TextEdit.replace(range, adjusted));
    }
    return edits;
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    vscode.window.showErrorMessage(`clove fmt failed: ${message}`);
    return [];
  }
}

async function updateFormatterPreview(document: vscode.TextDocument): Promise<vscode.Uri> {
  const command = 'clove';
  const args = ['fmt', '--stdin'];
  const formatted = await runFormatter(command, args, document.getText(), document);
  const normalized = ensureTrailingNewline(formatted);
  const previewUri = buildFormatterPreviewUri(document);
  formatterPreviewContent.set(previewUri.toString(), normalized);
  formatterPreviewEmitter.fire(previewUri);
  return previewUri;
}

function buildFormatterPreviewUri(document: vscode.TextDocument): vscode.Uri {
  const key = encodeURIComponent(document.uri.toString());
  return vscode.Uri.parse(`${FORMATTER_PREVIEW_SCHEME}:${key}`);
}

function buildEmbeddedRubyUri(document: vscode.TextDocument): vscode.Uri {
  const key = encodeURIComponent(document.uri.toString());
  return vscode.Uri.from({ scheme: EMBEDDED_RUBY_SCHEME, path: `/${key}.rb` });
}

async function getEmbeddedRubyDocument(
  document: vscode.TextDocument,
  content: string
): Promise<vscode.TextDocument> {
  const uri = buildEmbeddedRubyUri(document);
  const uriKey = uri.toString();
  if (embeddedRubyContent.get(uriKey) !== content) {
    embeddedRubyContent.set(uriKey, content);
    embeddedRubyEmitter.fire(uri);
  }
  let rubyDocument = await vscode.workspace.openTextDocument(uri);
  if (rubyDocument.languageId !== 'ruby') {
    rubyDocument = await vscode.languages.setTextDocumentLanguage(rubyDocument, 'ruby');
  }
  return rubyDocument;
}

function runFormatter(
  command: string,
  args: string[],
  input: string,
  document: vscode.TextDocument
): Promise<string> {
  return new Promise((resolve, reject) => {
    const workspaceFolder = vscode.workspace.getWorkspaceFolder(document.uri);
    const child = cp.spawn(command, args, {
      cwd: workspaceFolder?.uri.fsPath,
      stdio: 'pipe'
    });
    let stdout = '';
    let stderr = '';
    let stdoutSize = 0;
    let stderrSize = 0;
    let finished = false;
    const maxOutputBytes = 32 * 1024 * 1024;
    const timeoutMs = 15000;
    const abort = (message: string) => {
      if (finished) return;
      finished = true;
      clearTimeout(timeoutId);
      child.kill();
      reject(new Error(message));
    };
    const timeoutId = setTimeout(() => {
      abort(`formatter timed out after ${timeoutMs}ms`);
    }, timeoutMs);
    child.stdout.on('data', (chunk) => {
      if (finished) return;
      stdoutSize += chunk.length;
      if (stdoutSize + stderrSize > maxOutputBytes) {
        abort(`formatter output exceeded ${maxOutputBytes} bytes`);
        return;
      }
      stdout += chunk.toString();
    });
    child.stderr.on('data', (chunk) => {
      if (finished) return;
      stderrSize += chunk.length;
      if (stdoutSize + stderrSize > maxOutputBytes) {
        abort(`formatter output exceeded ${maxOutputBytes} bytes`);
        return;
      }
      stderr += chunk.toString();
    });
    child.on('error', (err) => {
      if (finished) return;
      finished = true;
      clearTimeout(timeoutId);
      reject(err);
    });
    child.on('close', (code) => {
      if (finished) return;
      finished = true;
      clearTimeout(timeoutId);
      if (code === 0) {
        resolve(stdout);
      } else {
        reject(new Error(stderr.trim() || `formatter exited with code ${code}`));
      }
    });
    child.stdin.write(input);
    child.stdin.end();
  });
}

function fullDocumentRange(document: vscode.TextDocument): vscode.Range {
  const start = document.positionAt(0);
  const end = document.positionAt(document.getText().length);
  return new vscode.Range(start, end);
}

function isFullDocumentRange(document: vscode.TextDocument, range: vscode.Range): boolean {
  return range.start.line === 0
    && range.start.character === 0
    && range.end.line === document.lineCount - 1
    && range.end.character === document.lineAt(document.lineCount - 1).text.length;
}

function getFormattingRanges(document: vscode.TextDocument): vscode.Range[] {
  const editor = vscode.window.activeTextEditor;
  if (!editor || editor.document !== document) {
    return [fullDocumentRange(document)];
  }
  const selections = editor.selections.filter((selection) => !selection.isEmpty);
  if (selections.length === 0) {
    return [fullDocumentRange(document)];
  }
  if (selectionsOverlap(document, selections)) {
    return [fullDocumentRange(document)];
  }
  return selections.map((selection) => new vscode.Range(selection.start, selection.end));
}

function selectionsOverlap(document: vscode.TextDocument, selections: vscode.Selection[]): boolean {
  const ranges = selections
    .map((selection) => {
      return {
        start: document.offsetAt(selection.start),
        end: document.offsetAt(selection.end)
      };
    })
    .sort((a, b) => a.start - b.start);
  for (let index = 1; index < ranges.length; index += 1) {
    if (ranges[index].start < ranges[index - 1].end) {
      return true;
    }
  }
  return false;
}

function ensureTrailingNewline(text: string): string {
  if (text.length === 0) {
    return '\n';
  }
  return text.endsWith('\n') ? text : `${text}\n`;
}

function normalizeSelectionOutput(formatted: string, original: string): string {
  let normalized = ensureTrailingNewline(formatted);
  if (!original.endsWith('\n') && normalized.endsWith('\n')) {
    normalized = normalized.slice(0, -1);
  }
  return normalized;
}

function applySelectionIndent(
  document: vscode.TextDocument,
  range: vscode.Range,
  formatted: string
): string {
  const lineText = document.lineAt(range.start.line).text;
  const prefix = lineText.slice(0, range.start.character);
  if (prefix.trim().length !== 0 || prefix.length === 0) {
    return formatted;
  }
  const lines = formatted.split('\n');
  const indented = lines.map((line, idx) => {
    if (line.length === 0) {
      return line;
    }
    if (idx === 0 && range.start.character > 0) {
      return line;
    }
    return `${prefix}${line}`;
  });
  return indented.join('\n');
}

function climbToForm(navigator: SexpNavigator, range: OffsetRange): OffsetRange {
  let current = range;
  for (;;) {
    const next = navigator.expand(current);
    if (!next || sameRange(next, current)) {
      return current;
    }
    current = next;
  }
}

interface ReplTerminalHandle {
  terminal: vscode.Terminal;
  ready: Promise<void>;
}

function ensureReplTerminal(existing: vscode.Terminal | undefined): ReplTerminalHandle {
  if (existing && !existing.exitStatus) {
    return {
      terminal: existing,
      ready: Promise.resolve()
    };
  }
  const term = vscode.window.createTerminal({
    name: 'Clove REPL',
    shellPath: 'clove',
    shellArgs: ['--repl']
  });
  return {
    terminal: term,
    ready: Promise.resolve()
  };
}

function startLanguageClient(context: vscode.ExtensionContext): LanguageClient | null {
  try {
    const config = vscode.workspace.getConfiguration('clove');
    const configured = config.get<string>('lsp.serverPath') || 'clove-lsp';
    const serverCommand = resolveServerCommand(configured);
    if (!serverCommand) {
      lspOutput?.appendLine(`serverPath '${configured}' not found`);
      void vscode.window.showWarningMessage(
        `Clove LSP: serverPath '${configured}' was not found. Please check your settings.`
      );
      return null;
    }
    lspOutput?.appendLine(`starting clove-lsp: ${serverCommand}`);
    const serverOptions: ServerOptions = {
      run: { command: serverCommand },
      debug: { command: serverCommand }
    };
    const clientOptions: LanguageClientOptions = {
      documentSelector: [{ scheme: 'file', language: 'clove' }],
      synchronize: {
        configurationSection: 'clove'
      },
      outputChannel: lspOutput ?? vscode.window.createOutputChannel('Clove LSP'),
      traceOutputChannel: lspOutput ?? vscode.window.createOutputChannel('Clove LSP')
    };
    const client = new LanguageClient('cloveLsp', 'Clove LSP', serverOptions, clientOptions);
    void client.start();
    context.subscriptions.push({
      dispose: () => {
        void client.stop();
      }
    });
    return client;
  } catch (error) {
    console.error('Clove: failed to start LSP client', error);
    void vscode.window.showWarningMessage(`Failed to start Clove LSP: ${error}`);
    return null;
  }
}

function resolveServerCommand(raw: string): string | null {
  let cmd = raw.trim();
  if (cmd.startsWith('~')) {
    cmd = path.join(os.homedir(), cmd.slice(1));
  }
  // Leave PATH lookup to LanguageClient; only verify when an absolute path is set.
  if (cmd.startsWith('/') || cmd.match(/^[A-Za-z]:\\/)) {
    if (!fs.existsSync(cmd)) {
      return null;
    }
    return cmd;
  }
  return cmd;
}

async function runRefactorAction(kind: string): Promise<void> {
  const editor = vscode.window.activeTextEditor;
  if (!editor) {
    return;
  }
  const selection = editor.selection;
  const range = selection.isEmpty
    ? new vscode.Range(selection.active, selection.active)
    : new vscode.Range(selection.start, selection.end);
  const actions = await vscode.commands.executeCommand<
    Array<vscode.CodeAction | vscode.Command> | undefined
  >('vscode.executeCodeActionProvider', editor.document.uri, range, kind);
  if (!actions || actions.length === 0) {
    void vscode.window.showInformationMessage('Clove: no refactor target found at this position.');
    return;
  }
  const action = actions.find(isCodeAction) ?? actions[0];
  if (isCodeAction(action)) {
    if (action.edit) {
      await vscode.workspace.applyEdit(action.edit);
    }
    if (action.command) {
      await vscode.commands.executeCommand(
        action.command.command,
        ...(action.command.arguments ?? [])
      );
    }
    return;
  }
  await vscode.commands.executeCommand(action.command, ...(action.arguments ?? []));
}

function isCodeAction(
  item: vscode.CodeAction | vscode.Command
): item is vscode.CodeAction {
  return (
    (item as vscode.CodeAction).edit !== undefined
    || (item as vscode.CodeAction).kind !== undefined
  );
}

async function ensureSuggestDetailsVisible(
  context: vscode.ExtensionContext,
  force = false
): Promise<void> {
  const config = vscode.workspace.getConfiguration('clove');
  if (!config.get<boolean>('suggest.autoShowDocumentation', true)) {
    return;
  }
  const alreadyToggled = context.globalState.get<boolean>(SUGGEST_DETAILS_STATE_KEY);
  if (alreadyToggled && !force) {
    return;
  }
  try {
    await vscode.commands.executeCommand('editor.action.toggleSuggestionDetails');
    await context.globalState.update(SUGGEST_DETAILS_STATE_KEY, true);
  } catch (error) {
    console.error('Clove: failed to toggle suggestion details', error);
  }
}

function registerSignatureHelpAutoTrigger(): vscode.Disposable {
  return vscode.workspace.onDidChangeTextDocument((event) => {
    if (event.document.languageId !== 'clove') {
      return;
    }
    const editor = vscode.window.activeTextEditor;
    if (!editor || editor.document !== event.document) {
      return;
    }
    for (const change of event.contentChanges) {
      if (!change.range.isEmpty || change.text.length !== 1) {
        continue;
      }
      const ch = change.text;
      if (!SIGNATURE_HINT_TRIGGERS.has(ch)) {
        continue;
      }
      if (ch === ' ') {
        const position = change.range.start.translate(0, 1);
        if (position.character === 0) {
          continue;
        }
        const prevRange = new vscode.Range(position.translate(0, -1), position);
        const prev = event.document.getText(prevRange);
        if (!prev.trim()) {
          continue;
        }
      }
      void vscode.commands.executeCommand('editor.action.triggerParameterHints');
      break;
    }
  });
}

class EmbeddedRubyCompletionProvider implements vscode.CompletionItemProvider {
  async provideCompletionItems(
    document: vscode.TextDocument,
    position: vscode.Position,
    token: vscode.CancellationToken,
    context: vscode.CompletionContext
  ): Promise<vscode.CompletionList | vscode.CompletionItem[] | undefined> {
    try {
      const navigator = getNavigator(document);
      const offset = document.offsetAt(position);
      const embed = findRubyEmbedContext(document, navigator, offset);
      if (!embed || offset < embed.start || offset > embed.end) {
        return undefined;
      }
      const blockRange = rangeToRange(document, { start: embed.start, end: embed.end });
      const rubySource = document.getText(blockRange);
      const blockLength = rubySource.length;
      const localOffset = clampLocalOffset(offset - embed.start, blockLength);
      const rubyDocument = await getEmbeddedRubyDocument(document, rubySource);
      if (token.isCancellationRequested) {
        return undefined;
      }
      const raw = await vscode.commands.executeCommand<
        vscode.CompletionList | vscode.CompletionItem[] | undefined
      >(
        'vscode.executeCompletionItemProvider',
        rubyDocument.uri,
        rubyDocument.positionAt(localOffset),
        context.triggerCharacter
      );
      if (!raw || token.isCancellationRequested) {
        return undefined;
      }
      const items = Array.isArray(raw) ? raw : raw.items;
      if (!items.length) {
        return undefined;
      }
      const mapped = items
        .map((item) =>
          mapEmbeddedCompletionItem(item, document, rubyDocument, embed.start, blockLength)
        )
        .filter((item): item is vscode.CompletionItem => !!item);
      if (!mapped.length) {
        return undefined;
      }
      if (Array.isArray(raw)) {
        return mapped;
      }
      return new vscode.CompletionList(mapped, raw.isIncomplete);
    } catch (error) {
      console.error('Clove: embedded Ruby completion failed', error);
      return undefined;
    }
  }
}

function mapEmbeddedCompletionItem(
  item: vscode.CompletionItem,
  document: vscode.TextDocument,
  rubyDocument: vscode.TextDocument,
  blockStart: number,
  blockLength: number
): vscode.CompletionItem | undefined {
  const mapped = new vscode.CompletionItem(item.label, item.kind);
  mapped.detail = item.detail;
  mapped.documentation = item.documentation;
  mapped.sortText = item.sortText;
  mapped.filterText = item.filterText;
  mapped.insertText = cloneInsertText(item.insertText);
  mapped.keepWhitespace = item.keepWhitespace;
  mapped.preselect = item.preselect;
  mapped.commitCharacters = item.commitCharacters;
  mapped.command = item.command;
  mapped.kind = item.kind;
  if (item.additionalTextEdits) {
    const edits = mapAdditionalTextEdits(
      item.additionalTextEdits,
      document,
      rubyDocument,
      blockStart,
      blockLength
    );
    if (!edits) {
      return undefined;
    }
    mapped.additionalTextEdits = edits.length > 0 ? edits : undefined;
  }
  if (item.textEdit) {
    const mappedEdit = mapCompletionTextEdit(
      item.textEdit,
      document,
      rubyDocument,
      blockStart,
      blockLength
    );
    if (!mappedEdit) {
      return undefined;
    }
    mapped.textEdit = mappedEdit;
  }
  if (item.range) {
    const mappedRange = mapCompletionRange(
      item.range,
      document,
      rubyDocument,
      blockStart,
      blockLength
    );
    if (!mappedRange) {
      return undefined;
    }
    mapped.range = mappedRange;
  }
  return mapped;
}

function cloneInsertText(
  insertText: string | vscode.SnippetString | undefined
): string | vscode.SnippetString | undefined {
  if (insertText instanceof vscode.SnippetString) {
    return new vscode.SnippetString(insertText.value);
  }
  return insertText;
}

function mapAdditionalTextEdits(
  edits: vscode.TextEdit[],
  document: vscode.TextDocument,
  rubyDocument: vscode.TextDocument,
  blockStart: number,
  blockLength: number
): vscode.TextEdit[] | undefined {
  const mapped: vscode.TextEdit[] = [];
  for (const edit of edits) {
    const next = mapSimpleTextEdit(edit, document, rubyDocument, blockStart, blockLength);
    if (!next) {
      return undefined;
    }
    mapped.push(next);
  }
  return mapped;
}

function mapSimpleTextEdit(
  edit: vscode.TextEdit,
  document: vscode.TextDocument,
  rubyDocument: vscode.TextDocument,
  blockStart: number,
  blockLength: number
): vscode.TextEdit | undefined {
  const range = mapRangeFromRuby(edit.range, document, rubyDocument, blockStart, blockLength);
  if (!range) {
    return undefined;
  }
  return new vscode.TextEdit(range, edit.newText);
}

type CompletionRange = vscode.Range | { inserting: vscode.Range; replacing: vscode.Range };

function mapCompletionTextEdit(
  edit: vscode.TextEdit,
  document: vscode.TextDocument,
  rubyDocument: vscode.TextDocument,
  blockStart: number,
  blockLength: number
): vscode.TextEdit | undefined {
  const range = mapRangeFromRuby(edit.range, document, rubyDocument, blockStart, blockLength);
  if (!range) {
    return undefined;
  }
  return new vscode.TextEdit(range, edit.newText);
}

function mapCompletionRange(
  range: CompletionRange,
  document: vscode.TextDocument,
  rubyDocument: vscode.TextDocument,
  blockStart: number,
  blockLength: number
): CompletionRange | undefined {
  if (range instanceof vscode.Range) {
    return mapRangeFromRuby(range, document, rubyDocument, blockStart, blockLength);
  }
  if ('inserting' in range && 'replacing' in range) {
    const inserting = mapRangeFromRuby(range.inserting, document, rubyDocument, blockStart, blockLength);
    const replacing = mapRangeFromRuby(range.replacing, document, rubyDocument, blockStart, blockLength);
    if (!inserting || !replacing) {
      return undefined;
    }
    return { inserting, replacing };
  }
  return undefined;
}

function mapRangeFromRuby(
  range: vscode.Range,
  document: vscode.TextDocument,
  rubyDocument: vscode.TextDocument,
  blockStart: number,
  blockLength: number
): vscode.Range | undefined {
  const localStart = rubyDocument.offsetAt(range.start);
  const localEnd = rubyDocument.offsetAt(range.end);
  const startOffset = mapLocalOffsetToDocument(blockStart, blockLength, localStart);
  const endOffset = mapLocalOffsetToDocument(blockStart, blockLength, localEnd);
  if (startOffset === undefined || endOffset === undefined) {
    return undefined;
  }
  const start = document.positionAt(startOffset);
  const end = document.positionAt(endOffset);
  return new vscode.Range(start, end);
}

function mapLocalOffsetToDocument(
  blockStart: number,
  blockLength: number,
  localOffset: number
): number | undefined {
  if (!Number.isFinite(localOffset)) {
    return undefined;
  }
  const clampedLocal = clampLocalOffset(localOffset, blockLength);
  return blockStart + clampedLocal;
}

function clampLocalOffset(offset: number, length: number): number {
  if (length <= 0) {
    return 0;
  }
  if (offset < 0) {
    return 0;
  }
  if (offset > length) {
    return length;
  }
  return offset;
}

function findRubyEmbedContext(
  document: vscode.TextDocument,
  navigator: SexpNavigator,
  offset: number
): RubyEmbedContext | undefined {
  const context = navigator.findEmbeddedLanguageContext(offset);
  if (!context) {
    return undefined;
  }
  if (context.head === '$rb') {
    return {
      head: context.head,
      start: context.innerRange.start,
      end: context.innerRange.end
    };
  }
  if (context.head === '$') {
    return {
      head: context.head,
      start: context.innerRange.start,
      end: context.innerRange.end
    };
  }
  return undefined;
}

function getNavigator(document: vscode.TextDocument): SexpNavigator {
  const cached = navigatorCache.get(document);
  if (cached && cached.version === document.version) {
    return cached.navigator;
  }
  const navigator = new SexpNavigator(document.getText());
  navigatorCache.set(document, { version: document.version, navigator });
  return navigator;
}
