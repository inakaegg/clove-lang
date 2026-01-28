import * as path from 'path';
import * as vscode from 'vscode';
import { Buffer } from 'buffer';
import { logSelectionDebug } from './logger';

type EmbeddedLanguage = 'ruby' | 'python';

interface EmbeddedParserState {
  parser: any | null;
  initPromise: Promise<void> | null;
  wasm: string;
  logLabel: string;
}

const embeddedStates: Record<EmbeddedLanguage, EmbeddedParserState> = {
  ruby: {
    parser: null,
    initPromise: null,
    wasm: 'tree-sitter-ruby.wasm',
    logLabel: 'ruby'
  },
  python: {
    parser: null,
    initPromise: null,
    wasm: 'tree-sitter-python.wasm',
    logLabel: 'python'
  }
};

const embeddedLanguages: EmbeddedLanguage[] = ['ruby', 'python'];

// Use the locally bundled tree-sitter JS bridge.
declare const require: any;
let Parser: any | null = null;
try {
  // eslint-disable-next-line @typescript-eslint/no-var-requires
  const treeSitter = require(path.join(__dirname, '..', 'tree-sitter', 'tree-sitter.cjs'));
  Parser = treeSitter?.Parser ?? treeSitter;
  if (treeSitter?.Language && Parser && !Parser.Language) {
    Parser.Language = treeSitter.Language;
  }
  logSelectionDebug('embedParsers: tree-sitter bridge loaded successfully');
} catch (error) {
  console.error('Clove: failed to load tree-sitter bridge', error);
  logSelectionDebug(`embedParsers: failed to load tree-sitter bridge: ${String(error)}`);
  Parser = null;
}

type SyntaxNode = any;

let parserCoreInitPromise: Promise<void> | null = null;

function ensureParserCoreInitialized(context: vscode.ExtensionContext): Promise<void> {
  if (parserCoreInitPromise) {
    return parserCoreInitPromise;
  }
  parserCoreInitPromise = (async () => {
    logSelectionDebug('embedParsers: initializing web-tree-sitter');
    await Parser.init({
      locateFile(scriptName: string) {
        return context.asAbsolutePath(path.join('tree-sitter', scriptName));
      }
    });
    logSelectionDebug('embedParsers: tree-sitter core initialized');
  })();
  return parserCoreInitPromise;
}

async function ensureLanguageParser(
  language: EmbeddedLanguage,
  context: vscode.ExtensionContext
): Promise<void> {
  const state = embeddedStates[language];
  if (!state) return;
  if (state.initPromise) {
    await state.initPromise;
    return;
  }

  state.initPromise = (async () => {
    await ensureParserCoreInitialized(context);
    const wasmPath = context.asAbsolutePath(path.join('tree-sitter', state.wasm));
    const languageInstance = await Parser.Language.load(wasmPath);
    logSelectionDebug(`embedParsers: ${state.logLabel} language wasm loaded`);
    const ParserClass = typeof Parser.Parser === 'function' ? Parser.Parser : Parser;
    const parserInstance = new ParserClass();
    parserInstance.setLanguage(languageInstance);
    state.parser = parserInstance;
    logSelectionDebug(`embedParsers: ${state.logLabel} parser instance ready`);
  })();

  try {
    await state.initPromise;
  } catch (error) {
    console.error(`Clove: failed to initialize ${language} parser`, error);
    logSelectionDebug(
      `embedParsers: failed to initialize ${state.logLabel} parser: ${String(error)}`
    );
    state.parser = null;
    state.initPromise = null;
    throw error;
  }
}

export async function initEmbedParsers(context: vscode.ExtensionContext): Promise<void> {
  logSelectionDebug('embedParsers: initEmbedParsers called');
  if (!Parser) {
    logSelectionDebug('embedParsers: Parser is null, skipping init');
    return;
  }

  for (const language of embeddedLanguages) {
    try {
      await ensureLanguageParser(language, context);
    } catch {
      // Already logged
    }
  }
}

export function getEmbeddedParser(language: EmbeddedLanguage): any | null {
  return embeddedStates[language]?.parser ?? null;
}

export function getRubyParser(): any | null {
  return getEmbeddedParser('ruby');
}

export function getPythonParser(): any | null {
  return getEmbeddedParser('python');
}

export interface ByteRange {
  start: number;
  end: number;
}

type RangeCollector = (start: number, end: number) => void;

export function computeRubyExpandedRange(
  text: string,
  startOffset: number,
  endOffset: number
): ByteRange | undefined {
  logSelectionDebug(
    `computeRubyExpandedRange: called length=${text.length} start=${startOffset} end=${endOffset}`
  );
  return computeEmbeddedExpandedRange('ruby', text, startOffset, endOffset, 'computeRubyExpandedRange');
}

export function computePythonExpandedRange(
  text: string,
  startOffset: number,
  endOffset: number
): ByteRange | undefined {
  logSelectionDebug(
    `computePythonExpandedRange: called length=${text.length} start=${startOffset} end=${endOffset}`
  );
  return computeEmbeddedExpandedRange(
    'python',
    text,
    startOffset,
    endOffset,
    'computePythonExpandedRange'
  );
}

function computeEmbeddedExpandedRange(
  language: EmbeddedLanguage,
  text: string,
  startOffset: number,
  endOffset: number,
  logLabel: string
): ByteRange | undefined {
  const parser = getEmbeddedParser(language);
  if (!Parser || !parser) return undefined;
  if (startOffset < 0 || endOffset < startOffset || endOffset > text.length) {
    logSelectionDebug(`${logLabel}: invalid offsets, aborting`);
    return undefined;
  }

  const byteStart = charIndexToByteIndex(text, startOffset);
  const byteEnd = charIndexToByteIndex(text, endOffset);
  logSelectionDebug(`${logLabel}: byteStart=${byteStart} byteEnd=${byteEnd}`);

  const tree = parser.parse(text);
  const root = tree.rootNode;

  const ranges = collectSelectionRanges(root, byteStart, byteEnd);
  const current: ByteRange = { start: byteStart, end: byteEnd };
  const next = findExpandedRange(ranges, current);

  tree.delete();

  if (!next) {
    logSelectionDebug(`${logLabel}: no expansion candidate`);
    return undefined;
  }

  return {
    start: byteIndexToCharIndex(text, next.start),
    end: byteIndexToCharIndex(text, next.end)
  };
}

function collectSelectionRanges(
  root: SyntaxNode,
  startOffset: number,
  endOffset: number
): ByteRange[] {
  const coverageEnd = endOffset > startOffset ? endOffset - 1 : endOffset;

  const node = root.descendantForIndex(startOffset, coverageEnd);
  if (!node) {
    return [];
  }

  const path: SyntaxNode[] = [];
  const seenNodes = new Set<SyntaxNode>();
  const pushPath = (nodeToAdd: SyntaxNode | null) => {
    let current: SyntaxNode | null = nodeToAdd;
    while (current) {
      if (seenNodes.has(current)) {
        current = current.parent;
        continue;
      }
      path.push(current);
      seenNodes.add(current);
      current = current.parent;
    }
  };

  pushPath(node);
  pushPath(root.descendantForIndex(startOffset, startOffset));
  if (startOffset === endOffset && startOffset > 0) {
    let candidate = root.descendantForIndex(startOffset - 1, startOffset - 1);
    while (candidate && !candidate.isNamed) {
      candidate = candidate.parent;
    }
    if (candidate && candidate.startIndex < startOffset && candidate.endIndex === startOffset) {
      pushPath(candidate);
    }
  }
  if (endOffset > startOffset) {
    pushPath(root.descendantForIndex(coverageEnd, coverageEnd));
  }

  const rangeMap = new Map<string, ByteRange>();
  const addRange = (start: number, end: number) => {
    if (start < 0 || end < start) {
      return;
    }
    const key = `${start}:${end}`;
    if (!rangeMap.has(key)) {
      rangeMap.set(key, { start, end });
    }
  };

  const addNodeRange = (nodeToAdd: SyntaxNode) => {
    if (nodeToAdd.startIndex === nodeToAdd.endIndex) {
      return;
    }
    if (nodeToAdd.type === 'ERROR') {
      return;
    }
    addRange(nodeToAdd.startIndex, nodeToAdd.endIndex);
  };

  // The original implementation sets both anchor/active to zero-length,
  // here we use only startOffset as a symmetric cursor.
  addRange(startOffset, startOffset);
  if (endOffset !== startOffset) {
    addRange(endOffset, endOffset);
  }

  for (const nodeInPath of path) {
    addNodeRange(nodeInPath);
    addSyntheticRanges(nodeInPath, addRange);
  }

  if (endOffset > startOffset) {
    const stack: SyntaxNode[] = [node];
    const visited = new Set<SyntaxNode>();
    while (stack.length > 0) {
      const current = stack.pop();
      if (!current || visited.has(current)) {
        continue;
      }
      visited.add(current);
      for (let i = 0; i < current.childCount; i++) {
        const child = current.child(i);
        if (child) {
          stack.push(child);
        }
      }
      if (current.startIndex < startOffset || current.endIndex > endOffset) {
        continue;
      }
      addNodeRange(current);
      addSyntheticRanges(current, addRange);
    }
  }

  if (endOffset > startOffset) {
    addRange(startOffset, endOffset);
  }

  const ranges = Array.from(rangeMap.values());
  ranges.sort((a, b) => {
    const lengthA = a.end - a.start;
    const lengthB = b.end - b.start;
    if (lengthA !== lengthB) {
      return lengthA - lengthB;
    }
    return a.start - b.start;
  });

  return ranges;
}

function addSyntheticRanges(node: SyntaxNode, collect: RangeCollector): void {
  if (node.type === 'string') {
    const interior = stringInteriorRange(node);
    if (interior) {
      collect(interior.start, interior.end);
    }
    const content = findChildOfType(node, 'string_content');
    if (content) {
      collect(content.startIndex, content.endIndex);
    }
  }

  if (node.type === 'call') {
    const receiver = node.childForFieldName('receiver');
    const method = node.childForFieldName('method');
    const args = node.childForFieldName('arguments');
    if (receiver && method) {
      const accessor = findAccessorToken(node, receiver, method);
      if (accessor) {
        collect(receiver.startIndex, accessor.endIndex);
      }
      collect(receiver.startIndex, method.endIndex);
    }
    if (args) {
      collect(args.startIndex, args.endIndex);
    }
  }

  if (node.type === 'argument_list' || node.type === 'parenthesized_statements') {
    collectDelimitedRanges(node, ['('], [')'], collect);
  }

  if (node.type === 'array') {
    collectDelimitedRanges(node, ['['], [']'], collect);
  }

  if (node.type === 'hash') {
    collectDelimitedRanges(node, ['{'], ['}'], collect);
  }

  if (node.type === 'element_reference') {
    collectDelimitedRanges(node, ['['], [']'], collect);
  }

  if (node.type === 'block' || node.type === 'do_block') {
    const parameters = node.childForFieldName('parameters');
    const body = node.childForFieldName('body');
    if (parameters && body) {
      collect(parameters.startIndex, body.endIndex);
    }
  }
}

function collectDelimitedRanges(
  node: SyntaxNode,
  openings: string[],
  closings: string[],
  collect: RangeCollector
): void {
  const inner = delimitedInnerRange(node, openings, closings);
  if (inner) {
    collect(inner.start, inner.end);
  }

  const outer = delimitedOuterRange(node, openings, closings);
  if (outer) {
    collect(outer.start, outer.end);
  }
}

function findChildOfType(node: SyntaxNode, type: string): SyntaxNode | null {
  for (let i = 0; i < node.childCount; i++) {
    const child = node.child(i);
    if (child?.type === type) {
      return child;
    }
  }
  return null;
}

function findAccessorToken(node: SyntaxNode, receiver: SyntaxNode, method: SyntaxNode): SyntaxNode | null {
  for (let i = 0; i < node.childCount; i++) {
    const child = node.child(i);
    if (!child || child.isNamed) {
      continue;
    }
    if (child.startIndex >= receiver.endIndex && child.endIndex <= method.startIndex) {
      return child;
    }
  }
  return null;
}

function stringInteriorRange(node: SyntaxNode): ByteRange | null {
  if (node.childCount === 0) {
    return null;
  }
  const first = node.child(0);
  const last = node.child(node.childCount - 1);
  let start = node.startIndex;
  let end = node.endIndex;
  if (first && !first.isNamed) {
    start = first.endIndex;
  }
  if (last && !last.isNamed) {
    end = last.startIndex;
  }
  if (end <= start) {
    return null;
  }
  return { start, end };
}

function delimitedInnerRange(node: SyntaxNode, openings: string[], closings: string[]): ByteRange | null {
  const open = findDelimiter(node, openings, true);
  const close = findDelimiter(node, closings, false);
  if (!open || !close) {
    return null;
  }
  const start = open.endIndex;
  const end = close.startIndex;
  if (end <= start) {
    return null;
  }
  return { start, end };
}

function delimitedOuterRange(node: SyntaxNode, openings: string[], closings: string[]): ByteRange | null {
  const open = findDelimiter(node, openings, true);
  const close = findDelimiter(node, closings, false);
  if (!open || !close) {
    return null;
  }
  const start = open.startIndex;
  const end = close.endIndex;
  if (end <= start) {
    return null;
  }
  return { start, end };
}

function findDelimiter(node: SyntaxNode, types: string[], fromStart: boolean): SyntaxNode | null {
  if (fromStart) {
    for (let i = 0; i < node.childCount; i++) {
      const child = node.child(i);
      if (child && !child.isNamed && types.includes(child.type)) {
        return child;
      }
    }
    return null;
  }

  for (let i = node.childCount - 1; i >= 0; i--) {
    const child = node.child(i);
    if (child && !child.isNamed && types.includes(child.type)) {
      return child;
    }
  }
  return null;
}

function findExpandedRange(ranges: ByteRange[], current: ByteRange): ByteRange | null {
  const index = indexOfRange(ranges, current);
  if (index !== -1) {
    for (let i = index + 1; i < ranges.length; i++) {
      const candidate = ranges[i];
      if (rangeStrictlyContains(candidate, current)) {
        return candidate;
      }
    }
  }

  let best: ByteRange | null = null;
  for (const candidate of ranges) {
    if (!rangeStrictlyContains(candidate, current)) {
      continue;
    }
    if (!best || rangeSize(candidate) < rangeSize(best)) {
      best = candidate;
    }
  }
  return best;
}

function indexOfRange(ranges: ByteRange[], target: ByteRange): number {
  return ranges.findIndex((range) => range.start === target.start && range.end === target.end);
}

function rangeStrictlyContains(outer: ByteRange, inner: ByteRange): boolean {
  return outer.start <= inner.start && outer.end >= inner.end && rangeSize(outer) > rangeSize(inner);
}

function rangeSize(range: ByteRange): number {
  return range.end - range.start;
}

function charIndexToByteIndex(text: string, charIndex: number): number {
  if (charIndex <= 0) {
    return 0;
  }
  if (charIndex >= text.length) {
    return Buffer.byteLength(text);
  }
  return Buffer.byteLength(text.slice(0, charIndex));
}

function byteIndexToCharIndex(text: string, byteIndex: number): number {
  if (byteIndex <= 0) {
    return 0;
  }
  const totalBytes = Buffer.byteLength(text);
  if (byteIndex >= totalBytes) {
    return text.length;
  }

  let low = 0;
  let high = text.length;
  while (low < high) {
    const mid = Math.floor((low + high) / 2);
    const midBytes = Buffer.byteLength(text.slice(0, mid));
    if (midBytes < byteIndex) {
      low = mid + 1;
    } else {
      high = mid;
    }
  }
  return low;
}
