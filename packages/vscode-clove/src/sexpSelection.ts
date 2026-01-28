import { computePythonExpandedRange, computeRubyExpandedRange } from './embedParsers';

export interface OffsetRange {
  start: number;
  end: number;
}

export interface EmbeddedLanguageContext {
  head: string;
  language?: 'ruby' | 'python';
  innerRange: OffsetRange;
  listRange: OffsetRange;
  headRange: OffsetRange;
}

export interface SelectionOptions {
  hyphenExpansion: 'segment' | 'full';
}

type NodeKind = 'list' | 'vector' | 'map' | 'set' | 'string' | 'comment' | 'atom' | 'root';

interface Node {
  kind: NodeKind;
  start: number;
  end: number;
  parent?: Node;
  children: Node[];
}

interface InterpolationRange {
  exprRange: OffsetRange;
  braceRange: OffsetRange;
  fullRange: OffsetRange;
}

export class SexpNavigator {
  private readonly text: string;
  private readonly root: Node;
  private readonly selectionOptions: SelectionOptions;

  constructor(text: string, selectionOptions?: Partial<SelectionOptions>) {
    this.text = text;
    this.root = buildAst(text);
    this.selectionOptions = normalizeSelectionOptions(selectionOptions);
  }

  findEmbeddedLanguageContext(offset: number): EmbeddedLanguageContext | undefined {
    const node = this.smallestContaining({ start: offset, end: offset });
    if (!node) return undefined;
    const range: OffsetRange = { start: offset, end: offset };
    const embed = findEnclosingEmbed(this.text, node, range, false);
    if (!embed) return undefined;
    const language = embed.headName === '$' || embed.headName === '$rb'
      ? 'ruby'
      : embed.headName === '$py'
      ? 'python'
      : undefined;
    return {
      head: embed.headName,
      language,
      innerRange: embed.innerRange,
      listRange: { start: embed.listNode.start, end: embed.listNode.end },
      headRange: { start: embed.headNode.start, end: embed.headNode.end }
    };
  }

  expand(range: OffsetRange): OffsetRange | undefined {
    const node = this.smallestContaining(range);
    if (!node) return undefined;

    // Fine-grained selection inside comments (word -> comment body -> whole comment node)
    const commentExpanded = this.expandInComment(node, range);
    if (commentExpanded) {
      return commentExpanded;
    }

    // Fine-grained selection inside $py{...} blocks (word -> line -> block body -> whole S-expression)
    const pyExpanded = this.expandInPyBlock(node, range);
    if (pyExpanded) {
      return pyExpanded;
    }

    const regexExpanded = this.expandInRegex(node, range);
    if (regexExpanded) {
      return regexExpanded;
    }

    // Handle "receiver + method name" like $WordTools.decorate in finer steps
    const qualifiedExpanded = this.expandInQualifiedSymbol(node, range);
    if (qualifiedExpanded) {
      return qualifiedExpanded;
    }

    const stringExpanded = this.expandInString(node, range);
    if (stringExpanded) {
      return stringExpanded;
    }

    // If it's just a cursor, snap to the smallest node at that position
    if (range.start === range.end) {
      const nodeRange = { start: node.start, end: node.end };
      if (!sameRange(range, nodeRange)) {
        return nodeRange;
      }
    }

    // Expand as a sibling group (x -> + x 1 -> [x] (+ x 1) -> ...)
    const siblingExpanded = this.expandSiblingGroup(range);
    if (siblingExpanded && !sameRange(range, siblingExpanded)) {
      return siblingExpanded;
    }

    // Expand from node body -> full node -> parent node, in that order
    const structuralExpanded = this.expandStructurally(node, range);
    if (structuralExpanded && !sameRange(range, structuralExpanded)) {
      return structuralExpanded;
    }

    return range;
  }

  shrink(_range: OffsetRange): OffsetRange | undefined {
    // Actual shrinking is handled by the history stack; no special shrink here
    return undefined;
  }

  findMatchingBracketOffset(offset: number): number | undefined {
    if (offset < 0 || offset > this.text.length) return undefined;
    const ch = offset < this.text.length ? this.text[offset] : undefined;
    const prevOffset = offset - 1;
    const prevCh = prevOffset >= 0 ? this.text[prevOffset] : undefined;

    // Prefer the outer bracket first
    if (isCloseBracket(prevCh)) {
      const node = findSmallestAtOffset(this.root, prevOffset);
      if (node && isCollectionNode(node) && node.end === offset) {
        return node.start;
      }
    }

    if (isOpenBracket(ch)) {
      const node = findSmallestAtOffset(this.root, offset);
      if (node && isCollectionNode(node) && node.start === offset) {
        if (!isCloseBracket(this.text[node.end - 1])) {
          return undefined;
        }
        return node.end;
      }
    }

    if (isOpenBracket(prevCh)) {
      const node = findSmallestAtOffset(this.root, prevOffset);
      if (node && isCollectionNode(node) && node.start === prevOffset) {
        if (!isCloseBracket(this.text[node.end - 1])) {
          return undefined;
        }
        return node.end - 1;
      }
    }

    if (isCloseBracket(ch)) {
      const node = findSmallestAtOffset(this.root, offset);
      if (node && isCollectionNode(node) && node.end === offset + 1) {
        return node.start + 1;
      }
    }

    return undefined;
  }

  private smallestContaining(range: OffsetRange): Node | undefined {
    if (range.start === range.end) {
      const offset = range.start;
      const ch =
        offset >= 0 && offset < this.text.length
          ? this.text[offset]
          : undefined;
      const prevOffset = offset - 1;
      const prevCh =
        prevOffset >= 0 && prevOffset < this.text.length
          ? this.text[prevOffset]
          : undefined;

      // If the cursor is on whitespace, snap to the nearest non-whitespace node
      if (isWhitespace(ch)) {
        const neighbor = findNearestNonWhitespaceOffset(this.text, offset);
        if (neighbor !== undefined) {
          const snapped = findSmallestAtOffset(this.root, neighbor);
          if (snapped) return snapped;
        }
      }

      // If the cursor is on a closing bracket, snap to the previous element
      if (ch === ')' || ch === ']' || ch === '}') {
        if (prevCh && !isWhitespace(prevCh)) {
          const snapped = findSmallestAtOffset(this.root, prevOffset);
          if (snapped) return snapped;
        }
      }

      // Otherwise, use the smallest node that includes the position
      return findSmallestAtOffset(this.root, offset);
    }
    return findSmallest(this.root, range);
  }

  private expandSiblingGroup(range: OffsetRange): OffsetRange | undefined {
    const group = findSiblingGroup(this.root, range);
    if (!group) return undefined;

    const { parent, startIndex, endIndex } = group;
    const children = parent.children;

    const isCollection =
      parent.kind === 'list' ||
      parent.kind === 'vector' ||
      parent.kind === 'map' ||
      parent.kind === 'set';

    // For let binding vectors / map literals, treat "key + value" as a pair
    const isLetBindingsVector = parent.kind === 'vector' && isLetBindingVector(parent, this.text);
    const isMapLiteral = parent.kind === 'map';

    if (isLetBindingsVector || isMapLiteral) {
      const pair = findKeyValuePair(children, startIndex, endIndex);
      if (pair) {
        const pairRange: OffsetRange = {
          start: children[pair.startIndex].start,
          end: children[pair.endIndex].end
        };
        if (!sameRange(range, pairRange)) {
          return pairRange;
        }
      }
    }

    // For (fn [args] body ...): expand [args] and following body as one group first
    if (
      parent.kind === 'list' &&
      children.length >= 3 &&
      children[0].kind === 'atom'
    ) {
      const headText = this.text.slice(children[0].start, children[0].end);
      if (headText === 'fn') {
        const argsAndBodyRange: OffsetRange = {
          start: children[1].start,
          end: children[children.length - 1].end
        };
        if (
          rangeWithin(range, argsAndBodyRange) &&
          !sameRange(range, argsAndBodyRange)
        ) {
          return argsAndBodyRange;
        }
      }
    }

    // If a single element inside a nested S-exp is selected,
    // expand directly to the S-exp body (all elements without brackets).
    if (
      isCollection &&
      parent.parent &&
      parent.parent.kind !== 'root' &&
      startIndex === endIndex &&
      children.length > 1
    ) {
      const contentRange = contentRangeFromChildren(children);
      if (contentRange && !sameRange(range, contentRange)) {
        return contentRange;
      }
    }

    // If there is a left sibling, expand one step left (skip comments)
    const leftIndex = findPreviousNonCommentIndex(children, startIndex - 1);
    if (
      leftIndex !== undefined &&
      (parent.kind !== 'root' ||
        !hasLineBreakBetween(this.text, children[leftIndex].end, children[startIndex].start))
    ) {
      const newStart = children[leftIndex].start;
      const newEnd = children[endIndex].end;
      return { start: newStart, end: newEnd };
    }

    // If no left sibling but a right sibling exists, expand to the right (skip comments)
    const rightIndex = findNextNonCommentIndex(children, endIndex + 1);
    if (
      rightIndex !== undefined &&
      (parent.kind !== 'root' ||
        !hasLineBreakBetween(this.text, children[endIndex].end, children[rightIndex].start))
    ) {
      const newStart = children[startIndex].start;
      const newEnd = children[rightIndex].end;
      return { start: newStart, end: newEnd };
    }

    // Can't expand further within the same parent
    return undefined;
  }

  private expandStructurally(node: Node, range: OffsetRange): OffsetRange | undefined {
    let current: Node | undefined = node;

    while (current) {
      if (current.kind === 'root' && !hasLineBreakBetween(this.text, range.start, range.end)) {
        return undefined;
      }
      const nodeRange = { start: current.start, end: current.end };
      const children = current.children;

      const contentRange = contentRangeFromChildren(children);
      if (contentRange) {
        const contentStart = contentRange.start;
        const contentEnd = contentRange.end;

        // If fully inside node content, expand to the full content first
        if (
          range.start >= contentStart &&
          range.end <= contentEnd &&
          !sameRange(range, contentRange)
        ) {
          return contentRange;
        }

        // If the full content is already selected, expand to the full node (with brackets)
        if (sameRange(range, contentRange) && !sameRange(range, nodeRange)) {
          return nodeRange;
        }
      }

      // If exactly the whole node is selected, expand to the parent
      if (sameRange(range, nodeRange)) {
        if (current.parent) {
          return { start: current.parent.start, end: current.parent.end };
        }
        return range;
      }

      // If inside this node, expand to the full node
      if (range.start >= current.start && range.end <= current.end) {
        return nodeRange;
      }

      current = current.parent;
    }

    return undefined;
  }

  private expandInComment(node: Node, range: OffsetRange): OffsetRange | undefined {
    if (node.kind !== 'comment') return undefined;

    const commentRange: OffsetRange = { start: node.start, end: node.end };
    const contentRange = commentContentRange(this.text, node);

    // If outside the comment, do not handle here
    if (!rangeWithin(range, commentRange)) return undefined;

    // If cursor only: word -> comment body -> whole comment
    if (range.start === range.end) {
      const pos = range.start;
      const word = findWordRange(this.text, pos, contentRange.start, contentRange.end);
      if (word && !sameRange(range, word)) {
        return word;
      }
      if (!sameRange(range, contentRange)) {
        return contentRange;
      }
      if (!sameRange(range, commentRange)) {
        return commentRange;
      }
      return range;
    }

    // If within comment body, expand to full body first
    if (rangeWithin(range, contentRange) && !sameRange(range, contentRange)) {
      return contentRange;
    }

    // If full body is selected, expand to full comment node
    if (sameRange(range, contentRange) && !sameRange(range, commentRange)) {
      return commentRange;
    }

    // If the whole comment node is selected, let the parent handle it
    if (sameRange(range, commentRange)) {
      if (node.parent) {
        return { start: node.parent.start, end: node.parent.end };
      }
      return range;
    }

    return undefined;
  }

  private expandInPyBlock(node: Node, range: OffsetRange): OffsetRange | undefined {
    const embed = findEnclosingEmbed(this.text, node, range, true);
    if (!embed) return undefined;

    const innerRange = embed.innerRange;
    const listRange: OffsetRange = { start: embed.listNode.start, end: embed.listNode.end };
    const headRange: OffsetRange = { start: embed.headNode.start, end: embed.headNode.end };
    const fullRange: OffsetRange = { start: headRange.start, end: listRange.end };
    const insideInner = rangeWithin(range, innerRange);
    const insideFull = rangeWithin(range, fullRange);

    if (!insideFull) {
      return undefined;
    }

    const listChildren = embed.listNode.children.filter(child => child.kind !== 'comment');
    const innerContentRange =
      listChildren.length > 0
        ? { start: listChildren[0].start, end: listChildren[listChildren.length - 1].end }
        : innerRange;

    if (sameRange(range, fullRange) || rangeCovers(range, fullRange)) {
      const parent = embed.listNode.parent;
      if (!parent) {
        return fullRange;
      }

      if (parent.kind === 'root') {
        const rootRange: OffsetRange = { start: parent.start, end: parent.end };
        if (!sameRange(range, rootRange)) {
          return rootRange;
        }
        return undefined;
      }

      if (parent.children.length > 0) {
        const contentRange: OffsetRange = {
          start: parent.children[0].start,
          end: parent.children[parent.children.length - 1].end
        };
        if (!sameRange(range, contentRange)) {
          return contentRange;
        }
      }

      const parentRange: OffsetRange = { start: parent.start, end: parent.end };
      if (!sameRange(range, parentRange)) {
        return parentRange;
      }
      return undefined;
    }

    if (!insideInner) {
      if (!sameRange(range, fullRange)) {
        return fullRange;
      }
      return undefined;
    }

    const pos = range.start;
    const lineRange = findLineRange(this.text, pos, innerRange.start, innerRange.end);
    const wordRange = findWordRange(this.text, pos, innerRange.start, innerRange.end);

    const snappedRange = snapRangeForEmbeddedLanguage(this.text, innerRange, range);
    const isRubyEmbed = isRubyEmbedHead(this.text, embed.listNode);
    if (isRubyEmbed && snappedRange) {
      const localStart = snappedRange.start - innerRange.start;
      const localEnd = snappedRange.end - innerRange.start;
      const rubyNext = computeRubyExpandedRange(
        this.text.slice(innerRange.start, innerRange.end),
        localStart,
        localEnd
      );
      if (rubyNext) {
        const expanded: OffsetRange = {
          start: innerRange.start + rubyNext.start,
          end: innerRange.start + rubyNext.end
        };
        if (!sameRange(expanded, range)) {
          return expanded;
        }
      }
    }

    const isPythonEmbed = isPythonEmbedHead(this.text, embed.listNode);
    if (isPythonEmbed && snappedRange) {
      const localStart = snappedRange.start - innerRange.start;
      const localEnd = snappedRange.end - innerRange.start;
      const pythonNext = computePythonExpandedRange(
        this.text.slice(innerRange.start, innerRange.end),
        localStart,
        localEnd
      );
      if (pythonNext) {
        const expanded: OffsetRange = {
          start: innerRange.start + pythonNext.start,
          end: innerRange.start + pythonNext.end
        };
        if (!sameRange(expanded, range)) {
          return expanded;
        }
      }
    }

    if (range.start === range.end) {
      if (wordRange && !sameRange(range, wordRange)) {
        return wordRange;
      }
      if (!sameRange(range, lineRange)) {
        return lineRange;
      }
      if (!sameRange(range, innerRange)) {
        return innerRange;
      }
      return fullRange;
    }

    if (wordRange && sameRange(range, wordRange) && !sameRange(range, lineRange)) {
      return lineRange;
    }

    if (sameRange(range, lineRange) && !sameRange(range, innerRange)) {
      return innerRange;
    }

    if (
      rangeCovers(range, innerContentRange) ||
      rangeCovers(range, innerRange) ||
      sameRange(range, innerRange)
    ) {
      if (!sameRange(range, fullRange)) {
        return fullRange;
      }
    }

    if (rangeWithin(range, headRange) && !sameRange(range, fullRange)) {
      return fullRange;
    }

    if (rangeWithin(range, innerRange) && !sameRange(range, innerRange)) {
      return innerRange;
    }

    return undefined;
  }

  private expandInQualifiedSymbol(node: Node, range: OffsetRange): OffsetRange | undefined {
    if (node.kind !== 'atom') return undefined;

    const fullRange: OffsetRange = { start: node.start, end: node.end };
    const text = this.text.slice(node.start, node.end);
    const prefixedFullCandidates = buildPrefixedSegmentCandidates(
      text,
      node.start,
      [{ start: 0, end: text.length }]
    );

    // Only handle symbols that start with `$` and contain exactly one `.`
    if (text.startsWith('$')) {
      const dotIndex = text.indexOf('.');
      if (dotIndex <= 1 || dotIndex === text.length - 1) {
        return undefined;
      }

      const classRange: OffsetRange = {
        start: node.start,
        end: node.start + dotIndex
      };
      const methodRange: OffsetRange = {
        start: node.start + dotIndex + 1,
        end: node.end
      };

      // For cursor-only, expand to method/class name first
      if (range.start === range.end) {
        const pos = range.start;
        if (pos >= methodRange.start && pos <= methodRange.end && !sameRange(range, methodRange)) {
          return methodRange;
        }
        if (pos >= classRange.start && pos <= classRange.end && !sameRange(range, classRange)) {
          return classRange;
        }
        if (!sameRange(range, fullRange)) {
          return fullRange;
        }
        return undefined;
      }

      // If only method/class name is selected, next expand to the full symbol
      if (
        (sameRange(range, classRange) || sameRange(range, methodRange)) &&
        !sameRange(range, fullRange)
      ) {
        return fullRange;
      }

      return undefined;
    }

    // Expand namespaced symbols like `sdl2::create-canvas`
    const namespaceSegments = splitQualifiedSymbol(text);
    if (namespaceSegments) {
      const candidates = buildSegmentExpansionCandidates(text, node.start, namespaceSegments, 2);
      const next = pickQualifiedSymbolExpansion(range, fullRange, candidates);
      if (next && !sameRange(range, next)) {
        return next;
      }
    }

    const dotSegments = splitSymbolOnDot(text);
    if (dotSegments) {
      const candidates = buildSegmentExpansionCandidates(text, node.start, dotSegments, 1);
      const nestedColonCandidates: OffsetRange[] = [];
      for (const segment of dotSegments) {
        const segmentText = text.slice(segment.start, segment.end);
        const colonSegments = splitSymbolOnColon(segmentText);
        if (colonSegments) {
          nestedColonCandidates.push(
            ...buildSegmentExpansionCandidates(
              segmentText,
              node.start + segment.start,
              colonSegments,
              1
            )
          );
        }
      }
      const mergedCandidates = uniqueRanges(candidates.concat(nestedColonCandidates));
      const next = pickQualifiedSymbolExpansion(range, fullRange, mergedCandidates);
      if (next && !sameRange(range, next)) {
        return next;
      }
    }

    const colonSegments = splitSymbolOnColon(text);
    if (colonSegments) {
      const candidates = buildSegmentExpansionCandidates(text, node.start, colonSegments, 1);
      const next = pickQualifiedSymbolExpansion(range, fullRange, candidates);
      if (next && !sameRange(range, next)) {
        return next;
      }
    }

    if (this.selectionOptions.hyphenExpansion === 'segment') {
      const hyphenSegments = splitSymbolOnHyphen(text);
      if (hyphenSegments) {
        const candidates = uniqueRanges([
          ...buildSegmentExpansionCandidates(text, node.start, hyphenSegments, 1, false),
          ...prefixedFullCandidates
        ]);
        const next = pickQualifiedSymbolExpansion(range, fullRange, candidates);
        if (next && !sameRange(range, next)) {
          return next;
        }
      }
    }

    if (prefixedFullCandidates.length > 0) {
      const candidates = uniqueRanges([...prefixedFullCandidates, fullRange]);
      const next = pickQualifiedSymbolExpansion(range, fullRange, candidates);
      if (next && !sameRange(range, next)) {
        return next;
      }
    }

    return undefined;
  }

  private expandInterpolationExpr(
    exprRange: OffsetRange,
    range: OffsetRange
  ): OffsetRange | undefined {
    if (!rangeWithin(range, exprRange)) return undefined;

    const exprText = this.text.slice(exprRange.start, exprRange.end);
    const localRange: OffsetRange = {
      start: range.start - exprRange.start,
      end: range.end - exprRange.start
    };
    const navigator = new SexpNavigator(exprText, this.selectionOptions);
    const next = navigator.expand(localRange);
    if (!next) return undefined;
    const mapped: OffsetRange = {
      start: exprRange.start + next.start,
      end: exprRange.start + next.end
    };
    if (sameRange(mapped, range)) {
      return undefined;
    }
    return mapped;
  }

  private expandInInterpolation(
    contentRange: OffsetRange,
    range: OffsetRange
  ): OffsetRange | undefined {
    const interpolation = findInterpolationRange(this.text, contentRange, range);
    if (!interpolation) return undefined;

    const { exprRange, braceRange, fullRange } = interpolation;

    if (rangeWithin(range, exprRange)) {
      const expanded = this.expandInterpolationExpr(exprRange, range);
      if (expanded) {
        return expanded;
      }
      if (!sameRange(range, exprRange)) {
        return exprRange;
      }
    } else if (rangeWithin(range, braceRange)) {
      if (range.start === range.end) {
        return exprRange;
      }
    }

    if (sameRange(range, exprRange)) {
      return braceRange;
    }

    if (sameRange(range, braceRange)) {
      return fullRange;
    }

    if (rangeWithin(range, fullRange) && !sameRange(range, fullRange)) {
      return fullRange;
    }

    if (sameRange(range, fullRange) && !sameRange(range, contentRange)) {
      return contentRange;
    }

    return undefined;
  }

  private expandInRegex(node: Node, range: OffsetRange): OffsetRange | undefined {
    if (node.kind !== 'atom') return undefined;

    const literal = parseRegexLiteralRanges(this.text, node);
    if (!literal) return undefined;

    const { fullRange, contentRange } = literal;
    if (!rangeWithin(range, fullRange)) return undefined;

    const interpolationExpanded = this.expandInInterpolation(contentRange, range);
    if (interpolationExpanded) {
      return interpolationExpanded;
    }

    if (sameRange(range, fullRange)) {
      return undefined;
    }

    if (sameRange(range, contentRange)) {
      return fullRange;
    }

    if (range.start === range.end || rangeWithin(range, contentRange)) {
      return contentRange;
    }

    return fullRange;
  }

  private expandInString(node: Node, range: OffsetRange): OffsetRange | undefined {
    if (node.kind !== 'string') return undefined;

    const stringRange: OffsetRange = { start: node.start, end: node.end };
    if (!rangeWithin(range, stringRange)) return undefined;

    const contentStart = node.start + 1;
    const contentEnd = Math.max(contentStart, node.end - 1);
    const contentRange: OffsetRange = { start: contentStart, end: contentEnd };

    const interpolationExpanded = this.expandInInterpolation(contentRange, range);
    if (interpolationExpanded) {
      return interpolationExpanded;
    }

    // If the whole string is selected, let the parent handle it
    if (sameRange(range, stringRange)) {
      return undefined;
    }

    // If only string content is selected, next expand to full string
    if (sameRange(range, contentRange)) {
      return stringRange;
    }

    // If cursor only or partial content is selected, expand to full content first
    if (range.start === range.end || rangeWithin(range, contentRange)) {
      return contentRange;
    }

    // Otherwise (e.g., spans quotes), expand to the full string
    if (!sameRange(range, stringRange)) {
      return stringRange;
    }

    return undefined;
  }
}

const defaultSelectionOptions: SelectionOptions = {
  hyphenExpansion: 'segment'
};

function normalizeSelectionOptions(options?: Partial<SelectionOptions>): SelectionOptions {
  return {
    hyphenExpansion: options?.hyphenExpansion ?? defaultSelectionOptions.hyphenExpansion
  };
}

function buildAst(text: string): Node {
  const root: Node = { kind: 'root', start: 0, end: text.length, children: [] };
  const stack: Node[] = [root];
  let i = 0;

  const ensureStack = () => {
    if (stack.length === 0) {
      stack.push(root);
    }
  };

  const pushNode = (node: Node) => {
    ensureStack();
    const parent = stack[stack.length - 1];
    node.parent = parent;
    parent.children.push(node);
  };

  while (i < text.length) {
    const ch = text[i];

    // Comment
    if (ch === ';') {
      const start = i;
      while (i < text.length && text[i] !== '\n') {
        i++;
      }
      pushNode({ kind: 'comment', start, end: i, children: [] });
      continue;
    }

    // String (Clove string literals are double quotes only)
    if (ch === '"') {
      const start = i;
      i++;
      let escaped = false;
      while (i < text.length) {
        const c = text[i];
        if (escaped) {
          escaped = false;
          i++;
          continue;
        }
        if (c === '\\') {
          escaped = true;
          i++;
          continue;
        }
        if (c === '"') {
          i++;
          break;
        }
        i++;
      }
      pushNode({ kind: 'string', start, end: i, children: [] });
      continue;
    }

    // Collection start
    if (ch === '(' || ch === '[' || ch === '{') {
      const kind: NodeKind =
        ch === '(' ? 'list' :
        ch === '[' ? 'vector' : 'map';
      const node: Node = { kind, start: i, end: text.length, children: [] };
      pushNode(node);
      stack.push(node);
      i++;
      continue;
    }

    // Collection end
    if (ch === ')' || ch === ']' || ch === '}') {
      const node = stack.pop();
      if (node && node !== root) {
        node.end = i + 1;
      }
       ensureStack();
      i++;
      continue;
    }

    // Atom
    if (!isWhitespace(ch)) {
      const start = i;
      while (i < text.length && !isTerminator(text[i])) {
        i++;
      }
      pushNode({ kind: 'atom', start, end: i, children: [] });
      continue;
    }

    i++;
  }

  // Unclosed collections extend to end of file
  while (stack.length > 1) {
    const node = stack.pop();
    if (node) {
      node.end = text.length;
    }
  }

  return root;
}

interface SiblingGroup {
  parent: Node;
  startIndex: number;
  endIndex: number;
}

function findNearestNonWhitespaceOffset(text: string, offset: number): number | undefined {
  const length = text.length;
  if (length === 0) return undefined;

  let distance = 1;
  while (offset - distance >= 0 || offset + distance < length) {
    const right = offset + distance;
    if (right < length) {
      const ch = text[right];
      if (!isWhitespace(ch)) {
        return right;
      }
    }
    const left = offset - distance;
    if (left >= 0) {
      const ch = text[left];
      if (!isWhitespace(ch)) {
        return left;
      }
    }
    distance++;
  }

  return undefined;
}

function findSmallestAtOffset(node: Node, offset: number): Node | undefined {
  if (!(node.start <= offset && offset < node.end)) return undefined;
  for (const child of node.children) {
    const found = findSmallestAtOffset(child, offset);
    if (found) return found;
  }
  return node;
}

function findSiblingGroup(root: Node, range: OffsetRange): SiblingGroup | undefined {
  const node = findSmallest(root, range);
  let current: Node | undefined = node;

  while (current) {
    const children = current.children;
    const parent = current;
    let startIndex = -1;
    let endIndex = -1;

    for (let i = 0; i < children.length; i++) {
      const child = children[i];
      if (child.start === range.start) {
        startIndex = i;
      }
      if (child.end === range.end) {
        endIndex = i;
      }
    }

    if (
      startIndex !== -1 &&
      endIndex !== -1 &&
      startIndex <= endIndex &&
      children[startIndex].start === range.start &&
      children[endIndex].end === range.end
    ) {
      return { parent, startIndex, endIndex };
    }

    current = current.parent;
  }

  return undefined;
}

function findSmallest(node: Node, range: OffsetRange): Node | undefined {
  if (!(node.start <= range.start && range.end <= node.end)) return undefined;
  for (const child of node.children) {
    const found = findSmallest(child, range);
    if (found) return found;
  }
  return node;
}

function contentRangeFromChildren(children: Node[]): OffsetRange | undefined {
  const nonComment = children.filter((child) => child.kind !== 'comment');
  if (nonComment.length === 0) return undefined;
  return { start: nonComment[0].start, end: nonComment[nonComment.length - 1].end };
}

function hasLineBreakBetween(text: string, start: number, end: number): boolean {
  if (end <= start) return false;
  const slice = text.slice(start, end);
  return slice.includes('\n') || slice.includes('\r');
}

function findPreviousNonCommentIndex(children: Node[], startIndex: number): number | undefined {
  for (let i = startIndex; i >= 0; i--) {
    if (children[i].kind !== 'comment') {
      return i;
    }
  }
  return undefined;
}

function findNextNonCommentIndex(children: Node[], startIndex: number): number | undefined {
  for (let i = startIndex; i < children.length; i++) {
    if (children[i].kind !== 'comment') {
      return i;
    }
  }
  return undefined;
}

function isTerminator(ch: string | undefined): boolean {
  if (!ch) return true;
  if (ch === ';') return true;
  if (ch === '(' || ch === ')' || ch === '[' || ch === ']' || ch === '{' || ch === '}') return true;
  return isWhitespace(ch);
}

function isOpenBracket(ch: string | undefined): boolean {
  return ch === '(' || ch === '[' || ch === '{';
}

function isCloseBracket(ch: string | undefined): boolean {
  return ch === ')' || ch === ']' || ch === '}';
}

function isCollectionNode(node: Node): boolean {
  return node.kind === 'list' || node.kind === 'vector' || node.kind === 'map' || node.kind === 'set';
}

function isWhitespace(ch: string | undefined): boolean {
  return ch !== undefined && /\s/.test(ch);
}

function sameRange(a: OffsetRange, b: OffsetRange): boolean {
  return a.start === b.start && a.end === b.end;
}

function rangeWithin(inner: OffsetRange, outer: OffsetRange): boolean {
  return outer.start <= inner.start && inner.end <= outer.end;
}

function rangeCovers(outer: OffsetRange, inner: OffsetRange): boolean {
  return outer.start <= inner.start && outer.end >= inner.end;
}

function isCommentNode(node: Node): boolean {
  return node.kind === 'comment';
}

function isLetBindingVector(node: Node, text: string): boolean {
  if (node.kind !== 'vector' || !node.parent) return false;
  const parent = node.parent;
  if (parent.kind !== 'list' || parent.children.length === 0) return false;
  const head = parent.children[0];
  if (head.kind !== 'atom') return false;
  const name = text.slice(head.start, head.end);
  return name === 'let';
}

function findKeyValuePair(
  children: Node[],
  startIndex: number,
  endIndex: number
): { startIndex: number; endIndex: number } | undefined {
  const pairs: { startIndex: number; endIndex: number }[] = [];

  let i = 0;
  while (i < children.length) {
    if (isCommentNode(children[i])) {
      i++;
      continue;
    }

    const keyIndex = i;
    let j = i + 1;
    while (j < children.length && isCommentNode(children[j])) {
      j++;
    }

    if (j >= children.length) break;

    const valueIndex = j;
    pairs.push({ startIndex: keyIndex, endIndex: valueIndex });
    i = valueIndex + 1;
  }

  for (const pair of pairs) {
    if (pair.startIndex <= startIndex && endIndex <= pair.endIndex) {
      return pair;
    }
  }

  return undefined;
}

function commentContentRange(text: string, node: Node): OffsetRange {
  let start = node.start;
  const end = node.end > node.start && text[node.end - 1] === '\n' ? node.end - 1 : node.end;

  // Skip `;` and following whitespace to find the start of comment content
  while (start < end) {
    const ch = text[start];
    if (ch === ';' || ch === ' ' || ch === '\t') {
      start++;
      continue;
    }
    break;
  }

  if (start >= end) {
    return { start: node.start, end: node.end };
  }

  return { start, end };
}

function findWordRange(
  text: string,
  position: number,
  min: number,
  max: number
): OffsetRange | undefined {
  if (position < min || position > max) return undefined;

  let pos = position;
  if (pos === max) {
    pos = max - 1;
  }

  const ch = text[pos];
  if (!ch || isWhitespace(ch)) {
    return undefined;
  }

  let start = pos;
  while (start > min) {
    const c = text[start - 1];
    if (!c || isWhitespace(c)) break;
    start--;
  }

  let end = pos + 1;
  while (end < max) {
    const c = text[end];
    if (!c || isWhitespace(c)) break;
    end++;
  }

  return { start, end };
}

function findLineRange(
  text: string,
  position: number,
  min: number,
  max: number
): OffsetRange {
  let pos = position;
  if (pos < min) pos = min;
  if (pos > max) pos = max;

  let start = pos;
  while (start > min) {
    const ch = text[start - 1];
    if (ch === '\n') break;
    start--;
  }

  let end = pos;
  while (end < max) {
    const ch = text[end];
    if (ch === '\n') break;
    end++;
  }

  return { start, end };
}

function parseRegexLiteralRanges(
  text: string,
  node: Node
): { fullRange: OffsetRange; contentRange: OffsetRange } | undefined {
  if (node.kind !== 'atom') return undefined;
  if (node.end <= node.start) return undefined;

  const first = text[node.start];
  let delimStart = node.start;
  if (first === '#') {
    if (node.start + 1 >= node.end || text[node.start + 1] !== '/') {
      return undefined;
    }
    delimStart = node.start + 1;
  } else if (first !== '/') {
    return undefined;
  }

  const regexEnd = scanRegexLiteral(text, delimStart, node.end, '/');
  if (regexEnd === undefined || regexEnd !== node.end) {
    return undefined;
  }

  const contentStart = delimStart + 1;
  const contentEnd = Math.max(contentStart, node.end - 1);
  return {
    fullRange: { start: node.start, end: node.end },
    contentRange: { start: contentStart, end: contentEnd }
  };
}

function findInterpolationRange(
  text: string,
  contentRange: OffsetRange,
  range: OffsetRange
): InterpolationRange | undefined {
  const interpolations = collectInterpolationsInString(text, contentRange);
  for (const interpolation of interpolations) {
    if (rangeWithin(range, interpolation.fullRange)) {
      return interpolation;
    }
  }
  return undefined;
}

function collectInterpolationsInString(
  text: string,
  contentRange: OffsetRange
): InterpolationRange[] {
  const ranges: InterpolationRange[] = [];
  let i = contentRange.start;
  let escaped = false;
  while (i < contentRange.end) {
    const ch = text[i];
    if (escaped) {
      escaped = false;
      i++;
      continue;
    }
    if (ch === '\\') {
      escaped = true;
      i++;
      continue;
    }
    if (ch === '#' && i + 1 < contentRange.end && text[i + 1] === '{') {
      const found = scanInterpolationRange(text, i, contentRange.end);
      if (found) {
        ranges.push(found);
        i = found.fullRange.end;
        continue;
      }
    }
    i++;
  }
  return ranges;
}

function scanInterpolationRange(
  text: string,
  hashStart: number,
  max: number
): InterpolationRange | undefined {
  const braceStart = hashStart + 1;
  if (braceStart >= max || text[braceStart] !== '{') {
    return undefined;
  }
  const exprStart = skipWsAndComments(text, braceStart + 1, max);
  const exprEnd = scanFormEnd(text, exprStart, max);
  if (exprEnd === undefined) {
    return undefined;
  }
  const closeStart = skipWsAndComments(text, exprEnd, max);
  if (closeStart >= max || text[closeStart] !== '}') {
    return undefined;
  }
  const braceEnd = closeStart + 1;
  return {
    exprRange: { start: exprStart, end: exprEnd },
    braceRange: { start: braceStart, end: braceEnd },
    fullRange: { start: hashStart, end: braceEnd }
  };
}

function scanFormEnd(text: string, start: number, max: number): number | undefined {
  let i = skipWsAndComments(text, start, max);
  if (i >= max) return undefined;

  const ch = text[i];
  if (ch === '"') {
    return scanPostfixAccessors(text, scanStringLiteral(text, i, max), max);
  }
  if (ch === '(') {
    return scanPostfixAccessors(text, scanDelimited(text, i, max, '(', ')'), max);
  }
  if (ch === '[') {
    return scanPostfixAccessors(text, scanDelimited(text, i, max, '[', ']'), max);
  }
  if (ch === '{') {
    return scanPostfixAccessors(text, scanDelimited(text, i, max, '{', '}'), max);
  }
  if (ch === '#') {
    const end = scanDispatchFormEnd(text, i, max);
    return scanPostfixAccessors(text, end, max);
  }
  if (ch === '$') {
    const end = scanDollarFormEnd(text, i, max);
    return scanPostfixAccessors(text, end, max);
  }
  if (ch === '@' || ch === '\'') {
    const targetStart = skipWsAndComments(text, i + 1, max);
    const targetEnd = scanFormEnd(text, targetStart, max);
    return scanPostfixAccessors(text, targetEnd, max);
  }
  if (ch === '/' && shouldReadRegexLiteral(text, i, max)) {
    return scanPostfixAccessors(text, scanRegexLiteral(text, i, max, '/'), max);
  }

  let end = i;
  while (end < max && !isTerminatorOrComma(text[end])) {
    end++;
  }
  return scanPostfixAccessors(text, end, max);
}

function scanDispatchFormEnd(text: string, start: number, max: number): number | undefined {
  const next = start + 1;
  if (next >= max) return undefined;
  const ch = text[next];
  if (ch === '{') {
    return scanDelimited(text, next, max, '{', '}');
  }
  if (ch === '(') {
    return scanDelimited(text, next, max, '(', ')');
  }
  if (ch === '/') {
    return scanRegexLiteral(text, next, max, '/');
  }
  if (ch === '_') {
    const discardedStart = skipWsAndComments(text, next + 1, max);
    const discardedEnd = scanFormEnd(text, discardedStart, max);
    if (discardedEnd === undefined) return undefined;
    const nextStart = skipWsAndComments(text, discardedEnd, max);
    return scanFormEnd(text, nextStart, max);
  }
  if (isDigitChar(ch) || ch === '[') {
    return scanSharpAccessor(text, start, max);
  }

  let tagEnd = next;
  while (tagEnd < max) {
    const c = text[tagEnd];
    if (isTerminatorOrComma(c) || c === '"' || c === '[' || c === ']' || c === '{' || c === '}') {
      break;
    }
    tagEnd++;
  }
  if (tagEnd === next) {
    return undefined;
  }
  const formStart = skipWsAndComments(text, tagEnd, max);
  return scanFormEnd(text, formStart, max);
}

function scanDollarFormEnd(text: string, start: number, max: number): number | undefined {
  const next = start + 1;
  if (next >= max) return undefined;
  const ch = text[next];
  if (ch === '{') {
    return scanDelimited(text, next, max, '{', '}');
  }
  if (ch === '(') {
    return scanDelimited(text, next, max, '(', ')');
  }
  let end = next;
  while (end < max && !isTerminatorOrComma(text[end])) {
    if (text[end] === '(' || text[end] === '{' || text[end] === '"' || text[end] === '[') {
      break;
    }
    end++;
  }
  if (end < max && text[end] === '(') {
    return scanDelimited(text, end, max, '(', ')');
  }
  if (end < max && text[end] === '{') {
    return scanDelimited(text, end, max, '{', '}');
  }
  return end;
}

function scanSharpAccessor(text: string, start: number, max: number): number | undefined {
  let i = start + 1;
  let allowsLeadingNumber = true;
  while (i < max) {
    const ch = text[i];
    if (ch === '[') {
      const end = scanBracketGroup(text, i, max);
      if (end === undefined) return undefined;
      i = end;
      allowsLeadingNumber = false;
      continue;
    }
    if (ch === '.') {
      i++;
      const numStart = i;
      while (i < max && isDigitChar(text[i])) {
        i++;
      }
      if (i === numStart) return undefined;
      allowsLeadingNumber = false;
      continue;
    }
    if (allowsLeadingNumber && isDigitChar(ch)) {
      while (i < max && isDigitChar(text[i])) {
        i++;
      }
      allowsLeadingNumber = false;
      continue;
    }
    break;
  }
  return i;
}

function scanBracketGroup(text: string, start: number, max: number): number | undefined {
  let i = start + 1;
  while (i < max) {
    const ch = text[i];
    if (ch === ']') {
      return i + 1;
    }
    i++;
  }
  return undefined;
}

function scanDelimited(
  text: string,
  start: number,
  max: number,
  open: string,
  close: string
): number | undefined {
  if (text[start] !== open) return undefined;
  let i = start + 1;
  while (i < max) {
    i = skipWsAndComments(text, i, max);
    if (i >= max) return undefined;
    if (text[i] === close) {
      return i + 1;
    }
    const next = scanFormEnd(text, i, max);
    if (next === undefined) return undefined;
    i = next;
  }
  return undefined;
}

function scanStringLiteral(text: string, start: number, max: number): number | undefined {
  if (text[start] !== '"') return undefined;
  let i = start + 1;
  let escaped = false;
  while (i < max) {
    const ch = text[i];
    if (escaped) {
      escaped = false;
      i++;
      continue;
    }
    if (ch === '\\') {
      escaped = true;
      i++;
      continue;
    }
    if (ch === '"') {
      return i + 1;
    }
    i++;
  }
  return undefined;
}

function scanRegexLiteral(
  text: string,
  start: number,
  max: number,
  delim: string
): number | undefined {
  if (text[start] !== delim) return undefined;
  let i = start + 1;
  let escaped = false;
  while (i < max) {
    const ch = text[i];
    if (escaped) {
      escaped = false;
      i++;
      continue;
    }
    if (ch === '\\') {
      escaped = true;
      i++;
      continue;
    }
    if (ch === '#' && i + 1 < max && text[i + 1] === '{') {
      const interpolation = scanInterpolationRange(text, i, max);
      if (!interpolation) return undefined;
      i = interpolation.fullRange.end;
      continue;
    }
    if (ch === delim) {
      return i + 1;
    }
    if (ch === '\n' && delim === '/') {
      return undefined;
    }
    i++;
  }
  return undefined;
}

function scanPostfixAccessors(
  text: string,
  end: number | undefined,
  max: number
): number | undefined {
  if (end === undefined) return undefined;
  let i = end;
  while (i < max && text[i] === '[') {
    const next = scanDelimited(text, i, max, '[', ']');
    if (next === undefined) return i;
    i = next;
  }
  return i;
}

function shouldReadRegexLiteral(text: string, start: number, max: number): boolean {
  const next = start + 1 < max ? text[start + 1] : undefined;
  if (!next) return false;
  if (isWhitespace(next) || next === ',' || next === ')' || next === ']' || next === '}' || next === ';') {
    return false;
  }
  return hasRegexEnd(text, start, max, '/');
}

function hasRegexEnd(text: string, start: number, max: number, delim: string): boolean {
  let escaped = false;
  let i = start + 1;
  while (i < max) {
    const ch = text[i];
    if (escaped) {
      escaped = false;
    } else if (ch === '\\') {
      escaped = true;
    } else if (ch === delim) {
      return true;
    } else if (ch === '\n' && delim === '/') {
      return false;
    }
    i++;
  }
  return false;
}

function skipWsAndComments(text: string, start: number, max: number): number {
  let i = start;
  while (i < max) {
    const ch = text[i];
    if (isWhitespace(ch) || ch === ',') {
      i++;
      continue;
    }
    if (ch === ';') {
      i++;
      while (i < max && text[i] !== '\n') {
        i++;
      }
      continue;
    }
    break;
  }
  return i;
}

function isDigitChar(ch: string | undefined): boolean {
  return !!ch && ch >= '0' && ch <= '9';
}

function isTerminatorOrComma(ch: string | undefined): boolean {
  return isTerminator(ch) || ch === ',';
}

function splitQualifiedSymbol(text: string): OffsetRange[] | undefined {
  const segments: OffsetRange[] = [];
  let segStart = 0;
  let i = 0;
  let sawSeparator = false;

  while (i < text.length) {
    if (text[i] === ':' && text[i + 1] === ':') {
      sawSeparator = true;
      if (i === segStart) {
        return undefined;
      }
      segments.push({ start: segStart, end: i });
      i += 2;
      segStart = i;
      continue;
    }
    i++;
  }

  if (!sawSeparator || segStart >= text.length) {
    return undefined;
  }

  segments.push({ start: segStart, end: text.length });
  return segments.length >= 2 ? segments : undefined;
}

function splitSymbolOnSingleSeparator(
  text: string,
  separator: string,
  shouldSplit: (index: number, value: string) => boolean
): OffsetRange[] | undefined {
  const segments: OffsetRange[] = [];
  let segStart = 0;
  let i = 0;
  let sawSeparator = false;

  while (i < text.length) {
    if (text[i] === separator && shouldSplit(i, text)) {
      if (i === segStart) {
        return undefined;
      }
      segments.push({ start: segStart, end: i });
      i += 1;
      segStart = i;
      sawSeparator = true;
      continue;
    }
    i++;
  }

  if (!sawSeparator || segStart >= text.length) {
    return undefined;
  }

  segments.push({ start: segStart, end: text.length });
  return segments.length >= 2 ? segments : undefined;
}

function splitSymbolOnDot(text: string): OffsetRange[] | undefined {
  if (/^[+-]?[0-9.]+$/.test(text)) {
    return undefined;
  }
  return splitSymbolOnSingleSeparator(text, '.', () => true);
}

function splitSymbolOnColon(text: string): OffsetRange[] | undefined {
  if (text.startsWith(':')) {
    return undefined;
  }
  return splitSymbolOnSingleSeparator(text, ':', (index, value) => {
    const prev = index > 0 ? value[index - 1] : undefined;
    const next = index + 1 < value.length ? value[index + 1] : undefined;
    if (prev === ':' || next === ':') {
      return false;
    }
    return true;
  });
}

function splitSymbolOnHyphen(text: string): OffsetRange[] | undefined {
  return splitSymbolOnSingleSeparator(text, '-', (index, value) => {
    if (index === 0 || index === value.length - 1) {
      return false;
    }
    const prev = value[index - 1];
    const next = value[index + 1];
    return isAsciiWordChar(prev) && isAsciiWordChar(next);
  });
}

function buildSegmentCandidates(
  nodeStart: number,
  segments: OffsetRange[],
  separatorLength: number,
  includeSeparatorEdges = true
): OffsetRange[] {
  const candidates: OffsetRange[] = [];
  const lastIndex = segments.length - 1;

  for (let i = 0; i <= lastIndex; i++) {
    for (let j = i; j <= lastIndex; j++) {
      const baseStart = segments[i].start;
      const baseEnd = segments[j].end;
      candidates.push({ start: nodeStart + baseStart, end: nodeStart + baseEnd });
      if (includeSeparatorEdges && i > 0) {
        const leadStart = segments[i].start - separatorLength;
        candidates.push({ start: nodeStart + leadStart, end: nodeStart + baseEnd });
      }
      if (includeSeparatorEdges && j < lastIndex) {
        const trailEnd = segments[j].end + separatorLength;
        candidates.push({ start: nodeStart + baseStart, end: nodeStart + trailEnd });
      }
    }
  }

  return uniqueRanges(candidates);
}

function buildSegmentExpansionCandidates(
  text: string,
  nodeStart: number,
  segments: OffsetRange[],
  separatorLength: number,
  includeSeparatorEdges = true
): OffsetRange[] {
  const baseCandidates = buildSegmentCandidates(
    nodeStart,
    segments,
    separatorLength,
    includeSeparatorEdges
  );
  const prefixedCandidates = buildPrefixedSegmentCandidates(text, nodeStart, segments);
  return uniqueRanges(baseCandidates.concat(prefixedCandidates));
}

function buildPrefixedSegmentCandidates(
  text: string,
  nodeStart: number,
  segments: OffsetRange[]
): OffsetRange[] {
  const candidates: OffsetRange[] = [];
  for (const segment of segments) {
    if (segment.end <= segment.start) {
      continue;
    }
    const segmentText = text.slice(segment.start, segment.end);
    const absStart = nodeStart + segment.start;
    const absEnd = nodeStart + segment.end;
    const inner = extractPrefixedSegmentRange(segmentText, absStart, absEnd);
    if (inner) {
      candidates.push(inner);
    }
  }
  return candidates;
}

function extractPrefixedSegmentRange(
  segmentText: string,
  absStart: number,
  absEnd: number
): OffsetRange | undefined {
  if (segmentText.length <= 1) {
    return undefined;
  }

  const first = segmentText[0];
  const last = segmentText[segmentText.length - 1];
  if (first === '"' && last === '"' && segmentText.length > 2) {
    return { start: absStart + 1, end: absEnd - 1 };
  }

  if (first === ':' || first === "'") {
    return { start: absStart + 1, end: absEnd };
  }

  return undefined;
}

function isAsciiWordChar(ch: string | undefined): boolean {
  return !!ch && /[A-Za-z0-9]/.test(ch);
}

function buildQualifiedSymbolCandidates(
  nodeStart: number,
  segments: OffsetRange[]
): OffsetRange[] {
  return buildSegmentCandidates(nodeStart, segments, 2);
}

function pickQualifiedSymbolExpansion(
  range: OffsetRange,
  fullRange: OffsetRange,
  candidates: OffsetRange[]
): OffsetRange | undefined {
  if (!rangeWithin(range, fullRange)) return undefined;

  const preferStart = (range.start - fullRange.start) <= (fullRange.end - range.end);
  const containing = candidates.filter(
    candidate => rangeWithin(range, candidate) && !sameRange(range, candidate)
  );

  if (containing.length === 0) return undefined;

  containing.sort((a, b) => {
    const lenA = a.end - a.start;
    const lenB = b.end - b.start;
    if (lenA !== lenB) return lenA - lenB;

    const aKeepStart = a.start === range.start;
    const bKeepStart = b.start === range.start;
    const aKeepEnd = a.end === range.end;
    const bKeepEnd = b.end === range.end;
    const aKeepsBoundary = aKeepStart || aKeepEnd;
    const bKeepsBoundary = bKeepStart || bKeepEnd;

    if (aKeepsBoundary !== bKeepsBoundary) {
      return aKeepsBoundary ? -1 : 1;
    }

    if (preferStart) {
      if (aKeepStart !== bKeepStart) return aKeepStart ? -1 : 1;
      if (aKeepEnd !== bKeepEnd) return aKeepEnd ? -1 : 1;
    } else {
      if (aKeepEnd !== bKeepEnd) return aKeepEnd ? -1 : 1;
      if (aKeepStart !== bKeepStart) return aKeepStart ? -1 : 1;
    }

    if (a.start !== b.start) return a.start - b.start;
    return a.end - b.end;
  });

  return containing[0];
}

function uniqueRanges(ranges: OffsetRange[]): OffsetRange[] {
  const seen = new Set<string>();
  const unique: OffsetRange[] = [];
  for (const range of ranges) {
    const key = `${range.start}:${range.end}`;
    if (!seen.has(key)) {
      seen.add(key);
      unique.push(range);
    }
  }
  return unique;
}

function snapRangeForEmbeddedLanguage(
  text: string,
  innerRange: OffsetRange,
  selection: OffsetRange
): OffsetRange | undefined {
  if (innerRange.start >= innerRange.end) {
    return undefined;
  }

  const clamp = (value: number) => clampOffset(value, innerRange.start, innerRange.end);
  let start = clamp(selection.start);
  let end = clamp(selection.end);

  if (start > end) {
    const tmp = start;
    start = end;
    end = tmp;
  }

  if (start === end) {
    const snapped = findNearestNonWhitespaceWithin(text, start, innerRange.start, innerRange.end);
    if (snapped === undefined) {
      return undefined;
    }
    return { start: snapped, end: snapped };
  }

  while (start < end && start < innerRange.end && isWhitespace(text[start])) {
    start++;
  }
  while (end > start && end > innerRange.start && isWhitespace(text[end - 1])) {
    end--;
  }

  if (start === end) {
    const snapped = findNearestNonWhitespaceWithin(text, start, innerRange.start, innerRange.end);
    if (snapped === undefined) {
      return undefined;
    }
    return { start: snapped, end: snapped };
  }

  return { start, end };
}

function clampOffset(value: number, min: number, max: number): number {
  if (value < min) return min;
  if (value > max) return max;
  return value;
}

function findNearestNonWhitespaceWithin(
  text: string,
  offset: number,
  min: number,
  max: number
): number | undefined {
  let left = offset;
  let right = offset + 1;
  while (left >= min || right < max) {
    if (left >= min) {
      const ch = text[left];
      if (ch && !isWhitespace(ch)) {
        return left;
      }
      left--;
    }
    if (right < max) {
      const ch = text[right];
      if (ch && !isWhitespace(ch)) {
        return right;
      }
      right++;
    }
  }
  return undefined;
}

interface EmbedMatch {
  listNode: Node;
  headNode: Node;
  headName: string;
  innerRange: OffsetRange;
}

function findEnclosingEmbed(
  text: string,
  node: Node,
  range: OffsetRange,
  allowHeadRange: boolean
): EmbedMatch | undefined {
  let current: Node | undefined = node;

  while (current) {
    if (current.parent) {
      if (current.kind === 'list' || current.kind === 'map') {
        const embed = buildEmbedFromList(text, current.parent, current);
        if (embed) {
          const innerRange = embed.innerRange;
          const combinedRange: OffsetRange = {
            start: embed.headNode.start,
            end: embed.listNode.end
          };
          if (
            rangeWithin(range, innerRange) ||
            (allowHeadRange && rangeWithin(range, combinedRange))
          ) {
            return embed;
          }
        }
      } else if (allowHeadRange && current.kind === 'atom') {
        const embed = buildEmbedFromHead(text, current);
        if (embed) {
          const combinedRange: OffsetRange = {
            start: embed.headNode.start,
            end: embed.listNode.end
          };
          if (rangeWithin(range, combinedRange)) {
            return embed;
          }
        }
      }
    }
    current = current.parent;
  }

  return undefined;
}

function buildEmbedFromList(text: string, parent: Node, listNode: Node): EmbedMatch | undefined {
  const children = parent.children;
  const index = children.indexOf(listNode);
  if (index <= 0) return undefined;
  const headNode = findEmbedHeadNode(children, index);
  if (!headNode || headNode.kind !== 'atom') return undefined;
  const headName = text.slice(headNode.start, headNode.end);
  if (!isEmbedHeadToken(headName)) return undefined;
  if (listNode.kind === 'list') return undefined;
  const innerStart = listNode.start + 1;
  const innerEnd = Math.max(innerStart, listNode.end - 1);
  return {
    listNode,
    headNode,
    headName,
    innerRange: { start: innerStart, end: innerEnd }
  };
}

function buildEmbedFromHead(text: string, headNode: Node): EmbedMatch | undefined {
  const parent = headNode.parent;
  if (!parent) return undefined;
  const children = parent.children;
  const index = children.indexOf(headNode);
  if (index < 0 || index + 1 >= children.length) return undefined;
  const next = children[index + 1];
  if (next.kind !== 'list') return undefined;
  return buildEmbedFromList(text, parent, next);
}

function isRubyEmbedHead(text: string, listNode: Node): boolean {
  const head = resolveEmbedHead(text, listNode);
  if (!head) return false;
  return head.name === '$' || head.name === '$rb';
}

function isPythonEmbedHead(text: string, listNode: Node): boolean {
  const head = resolveEmbedHead(text, listNode);
  if (!head) return false;
  return head.name === '$py';
}

function resolveEmbedHead(text: string, listNode: Node): { node: Node; name: string } | undefined {
  const parent = listNode.parent;
  if (!parent) return undefined;
  const children = parent.children;
  const index = children.indexOf(listNode);
  if (index <= 0) return undefined;
  const headNode = findEmbedHeadNode(children, index);
  if (!headNode || headNode.kind !== 'atom') return undefined;
  return { node: headNode, name: text.slice(headNode.start, headNode.end) };
}

function isEmbedHeadToken(name: string): boolean {
  return name === '$' || name === '$py' || name === '$rb';
}

function findEmbedHeadNode(siblings: Node[], index: number): Node | undefined {
  for (let i = index - 1; i >= 0; i--) {
    const candidate = siblings[i];
    if (candidate.kind === 'comment') {
      continue;
    }
    if (candidate.kind !== 'atom') {
      return undefined;
    }
    return candidate;
  }
  return undefined;
}
