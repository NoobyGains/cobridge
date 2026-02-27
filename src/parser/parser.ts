/**
 * COBridge — COBOL Copybook Parser
 *
 * Parses a token stream from the lexer into a typed AST (Copybook).
 * Builds the hierarchical field structure from COBOL level numbers,
 * calculates byte lengths from PIC clauses and USAGE types,
 * and computes field offsets within the record.
 */

import { tokenize, Token, TokenType } from "./lexer.js";
import {
  Copybook,
  CopybookField,
  PicInfo,
  UsageType,
  OccursClause,
} from "./types.js";

// ---------------------------------------------------------------------------
// PIC string parsing
// ---------------------------------------------------------------------------

/**
 * Expand PIC shorthand: 9(5) -> 99999, X(3) -> XXX, etc.
 * Also handles patterns like 9(5)V9(2) -> 99999V99
 */
function expandPic(raw: string): string {
  // Replace patterns like X(5) with XXXXX
  return raw.replace(/([A-Za-z9])(\((\d+)\))/g, (_match, char, _paren, count) => {
    return char.repeat(parseInt(count, 10));
  });
}

/**
 * Parse a PIC string into structured PicInfo.
 */
export function parsePicString(raw: string): PicInfo {
  const upper = raw.toUpperCase();
  const expanded = expandPic(upper);

  let signed = false;
  let type: PicInfo["type"] = "alphanumeric";
  let intDigits = 0;
  let decDigits = 0;
  let totalLength = 0;
  let pastDecimal = false;

  for (let i = 0; i < expanded.length; i++) {
    const ch = expanded[i];
    switch (ch) {
      case "S":
        signed = true;
        // S does not occupy a byte position in the PIC string count
        break;
      case "9":
        if (pastDecimal) {
          decDigits++;
        } else {
          intDigits++;
        }
        totalLength++;
        break;
      case "X":
        type = "alphanumeric";
        totalLength++;
        break;
      case "A":
        type = "alpha";
        totalLength++;
        break;
      case "V":
        // Implied decimal — no storage
        pastDecimal = true;
        break;
      case "P":
        // Scaling position — counts as a digit position but no storage in DISPLAY
        if (pastDecimal) {
          decDigits++;
        } else {
          intDigits++;
        }
        break;
      case "Z":
      case "*":
        // Numeric edited — each position is one byte
        type = "numeric";
        totalLength++;
        if (pastDecimal) decDigits++;
        else intDigits++;
        break;
      case "+":
      case "-":
        totalLength++;
        if (pastDecimal) decDigits++;
        else intDigits++;
        break;
      case ".":
        // Actual decimal point — takes one byte
        totalLength++;
        pastDecimal = true;
        break;
      case ",":
      case "B":
      case "0":
      case "/":
        // Insertion characters — each takes one byte
        totalLength++;
        break;
      default:
        // CR, DB are two-character editing symbols
        if (ch === "C" && i + 1 < expanded.length && expanded[i + 1] === "R") {
          totalLength += 2;
          i++;
        } else if (ch === "D" && i + 1 < expanded.length && expanded[i + 1] === "B") {
          totalLength += 2;
          i++;
        }
        break;
    }
  }

  // Determine type based on content
  if (intDigits > 0 || decDigits > 0) {
    type = "numeric";
  }

  return {
    type,
    length: totalLength,
    decimals: decDigits,
    signed,
    raw,
  };
}

// ---------------------------------------------------------------------------
// Byte length calculation
// ---------------------------------------------------------------------------

/**
 * Calculate the byte length of a field based on its PIC clause and USAGE.
 */
export function calculateByteLength(pic: PicInfo, usage: UsageType): number {
  switch (usage) {
    case UsageType.DISPLAY:
      return pic.length;

    case UsageType.COMP_1:
      // Single-precision float: always 4 bytes
      return 4;

    case UsageType.COMP_2:
      // Double-precision float: always 8 bytes
      return 8;

    case UsageType.COMP_3:
    case UsageType.PACKED_DECIMAL: {
      // Packed decimal: ceil((totalDigits + 1) / 2)
      // totalDigits = integer digits + decimal digits
      const totalDigits = pic.length;
      return Math.ceil((totalDigits + 1) / 2);
    }

    case UsageType.COMP:
    case UsageType.BINARY: {
      // Binary: depends on total digit count
      const totalDigits = pic.length;
      if (totalDigits <= 4) return 2;
      if (totalDigits <= 9) return 4;
      return 8;
    }

    default:
      return pic.length;
  }
}

// ---------------------------------------------------------------------------
// Token stream consumer
// ---------------------------------------------------------------------------

class TokenReader {
  private tokens: Token[];
  private pos: number = 0;

  constructor(tokens: Token[]) {
    this.tokens = tokens;
  }

  peek(): Token {
    return this.tokens[this.pos];
  }

  next(): Token {
    const t = this.tokens[this.pos];
    if (t.type !== TokenType.EOF) {
      this.pos++;
    }
    return t;
  }

  expect(type: TokenType): Token {
    const t = this.next();
    if (t.type !== type) {
      throw new Error(
        `Expected ${type} but got ${t.type} ("${t.value}") at line ${t.line}`
      );
    }
    return t;
  }

  match(type: TokenType): Token | null {
    if (this.peek().type === type) {
      return this.next();
    }
    return null;
  }

  isAtEnd(): boolean {
    return this.peek().type === TokenType.EOF;
  }
}

// ---------------------------------------------------------------------------
// Field parsing
// ---------------------------------------------------------------------------

interface RawField {
  levelNumber: number;
  name: string;
  picture?: PicInfo;
  usage: UsageType;
  occurs?: OccursClause;
  redefines?: string;
  line: number;
}

/**
 * Parse a single field declaration from the token stream.
 * A field declaration starts with a level number and ends with a period.
 */
function parseFieldDeclaration(reader: TokenReader): RawField | null {
  if (reader.isAtEnd()) return null;

  // Expect level number
  const levelToken = reader.match(TokenType.LEVEL_NUMBER);
  if (!levelToken) return null;

  const levelNumber = parseInt(levelToken.value, 10);

  // Parse data name or FILLER
  let name = "FILLER";
  if (reader.peek().type === TokenType.DATA_NAME) {
    name = reader.next().value;
  } else if (reader.peek().type === TokenType.FILLER) {
    reader.next();
    name = "FILLER";
  }

  // Parse clauses until period
  let picture: PicInfo | undefined;
  let usage = UsageType.DISPLAY;
  let occurs: OccursClause | undefined;
  let redefines: string | undefined;

  while (reader.peek().type !== TokenType.PERIOD && !reader.isAtEnd()) {
    const token = reader.peek();

    switch (token.type) {
      case TokenType.PIC: {
        reader.next();
        // Optional IS
        reader.match(TokenType.IS);
        const picToken = reader.expect(TokenType.PIC_STRING);
        picture = parsePicString(picToken.value);
        break;
      }

      case TokenType.USAGE: {
        reader.next();
        // Optional IS
        reader.match(TokenType.IS);
        const usageToken = reader.expect(TokenType.USAGE_TYPE);
        usage = usageToken.value as UsageType;
        break;
      }

      case TokenType.USAGE_TYPE: {
        // USAGE type can appear without the USAGE keyword
        const usageToken = reader.next();
        usage = usageToken.value as UsageType;
        break;
      }

      case TokenType.OCCURS: {
        reader.next();
        const countToken = reader.expect(TokenType.NUMBER);
        const count = parseInt(countToken.value, 10);

        // Check for TO (variable-length OCCURS)
        if (reader.match(TokenType.TO)) {
          const maxToken = reader.expect(TokenType.NUMBER);
          const maxCount = parseInt(maxToken.value, 10);
          // TIMES is optional here
          reader.match(TokenType.TIMES);
          // DEPENDING ON
          reader.expect(TokenType.DEPENDING);
          reader.expect(TokenType.ON);
          const depField = reader.expect(TokenType.DATA_NAME);
          occurs = {
            count: maxCount,
            min: count,
            dependingOn: depField.value,
          };
        } else {
          // Optional TIMES
          reader.match(TokenType.TIMES);
          occurs = { count };
        }
        break;
      }

      case TokenType.REDEFINES: {
        reader.next();
        const refField = reader.expect(TokenType.DATA_NAME);
        redefines = refField.value;
        break;
      }

      case TokenType.VALUE: {
        // Skip VALUE clause entirely — we tokenize but don't use it
        reader.next();
        reader.match(TokenType.IS);
        // Consume the value literal if present
        reader.match(TokenType.VALUE_LITERAL);
        break;
      }

      default:
        // Skip unrecognized tokens (e.g., IS by itself, extra keywords)
        reader.next();
        break;
    }
  }

  // Consume the period
  reader.match(TokenType.PERIOD);

  return {
    levelNumber,
    name,
    picture,
    usage,
    occurs,
    redefines,
    line: levelToken.line,
  };
}

// ---------------------------------------------------------------------------
// AST construction — build tree from flat list using level numbers
// ---------------------------------------------------------------------------

/**
 * Build a tree of CopybookField nodes from a flat list of parsed fields.
 * COBOL's level numbering creates implicit nesting: a field with level 05
 * under a level 01 is a child of that 01 group.
 */
function buildTree(rawFields: RawField[]): CopybookField[] {
  if (rawFields.length === 0) return [];

  const root: CopybookField[] = [];
  const stack: { field: CopybookField; level: number }[] = [];

  for (const raw of rawFields) {
    // Level 88 fields are condition names — skip for layout purposes
    if (raw.levelNumber === 88) continue;

    const field: CopybookField = {
      levelNumber: raw.levelNumber,
      name: raw.name,
      picture: raw.picture,
      usage: raw.usage,
      occurs: raw.occurs,
      redefines: raw.redefines,
      children: [],
      isGroup: false, // will be determined later
      byteLength: 0,
      startOffset: 0,
    };

    // Find the correct parent by popping the stack until we find a level
    // that is lower (higher in hierarchy) than the current one
    while (stack.length > 0 && stack[stack.length - 1].level >= raw.levelNumber) {
      stack.pop();
    }

    if (stack.length === 0) {
      root.push(field);
    } else {
      stack[stack.length - 1].field.children.push(field);
    }

    stack.push({ field, level: raw.levelNumber });
  }

  // Mark group items (fields that have children or no PIC clause and level < 77)
  markGroups(root);

  return root;
}

function markGroups(fields: CopybookField[]): void {
  for (const field of fields) {
    if (field.children.length > 0) {
      field.isGroup = true;
    }
    if (field.children.length > 0) {
      markGroups(field.children);
    }
  }
}

// ---------------------------------------------------------------------------
// Byte length and offset calculation
// ---------------------------------------------------------------------------

/**
 * Propagate USAGE from group items to children that have DISPLAY (default).
 * In COBOL, a group USAGE clause applies to all elementary children.
 */
function propagateUsage(fields: CopybookField[], parentUsage?: UsageType): void {
  for (const field of fields) {
    if (parentUsage && field.usage === UsageType.DISPLAY && !field.picture) {
      // Group item inheriting parent usage — pass down to children
      field.usage = parentUsage;
    } else if (parentUsage && field.usage === UsageType.DISPLAY && field.picture) {
      // Elementary item with default DISPLAY — inherit parent if parent specifies
      field.usage = parentUsage;
    }

    if (field.children.length > 0) {
      propagateUsage(field.children, field.usage !== UsageType.DISPLAY ? field.usage : parentUsage);
    }
  }
}

/**
 * Calculate byte lengths for all fields bottom-up.
 * Elementary items get their length from PIC + USAGE.
 * Group items sum their children's lengths.
 */
function calculateLengths(fields: CopybookField[]): void {
  for (const field of fields) {
    if (field.isGroup) {
      // Recurse into children first
      calculateLengths(field.children);

      // Group length = sum of children lengths (excluding REDEFINES)
      let groupLen = 0;
      for (const child of field.children) {
        if (child.redefines) {
          // REDEFINES does not add to the group length
          continue;
        }
        const childLen = child.occurs
          ? child.byteLength * child.occurs.count
          : child.byteLength;
        groupLen += childLen;
      }
      field.byteLength = groupLen;
    } else if (field.picture) {
      field.byteLength = calculateByteLength(field.picture, field.usage);
    } else if (field.usage === UsageType.COMP_1) {
      field.byteLength = 4;
    } else if (field.usage === UsageType.COMP_2) {
      field.byteLength = 8;
    }
    // else byteLength stays 0 (e.g., level 88 or empty filler)
  }
}

/**
 * Calculate start offsets for each field within the record.
 * Offsets are relative to the start of the parent group (or the record for top-level).
 * REDEFINES fields share the offset of the field they redefine.
 */
function calculateOffsets(fields: CopybookField[], baseOffset: number = 0): void {
  let currentOffset = baseOffset;

  // Build a map of field names to their offsets for REDEFINES lookups
  const offsetMap = new Map<string, number>();

  for (const field of fields) {
    if (field.redefines) {
      // REDEFINES: use the offset of the field being redefined
      const redefOffset = offsetMap.get(field.redefines);
      if (redefOffset !== undefined) {
        field.startOffset = redefOffset;
      } else {
        // Fallback: use current offset (shouldn't happen in valid COBOL)
        field.startOffset = currentOffset;
      }
    } else {
      field.startOffset = currentOffset;
    }

    offsetMap.set(field.name, field.startOffset);

    // Calculate offsets for children
    if (field.isGroup) {
      calculateOffsets(field.children, field.startOffset);
    }

    // Advance current offset (unless this is a REDEFINES)
    if (!field.redefines) {
      const fieldLen = field.occurs
        ? field.byteLength * field.occurs.count
        : field.byteLength;
      currentOffset += fieldLen;
    }
  }
}

// ---------------------------------------------------------------------------
// Main parse function
// ---------------------------------------------------------------------------

/**
 * Parse a COBOL copybook source string into a typed AST.
 *
 * @param source - The raw COBOL copybook text
 * @returns A Copybook AST with calculated lengths and offsets
 */
export function parseCopybook(source: string): Copybook {
  const tokens = tokenize(source);
  const reader = new TokenReader(tokens);

  // Parse all field declarations
  const rawFields: RawField[] = [];
  while (!reader.isAtEnd()) {
    const field = parseFieldDeclaration(reader);
    if (field) {
      rawFields.push(field);
    } else {
      // Skip any stray tokens
      if (!reader.isAtEnd()) reader.next();
    }
  }

  // Build hierarchical tree
  const fields = buildTree(rawFields);

  // Propagate USAGE from groups to children
  propagateUsage(fields);

  // Calculate lengths bottom-up
  calculateLengths(fields);

  // Calculate offsets top-down
  calculateOffsets(fields, 0);

  // Derive copybook name from the first 01-level field
  let name = "UNKNOWN";
  if (fields.length > 0) {
    name = fields[0].name;
  }

  // Calculate total record length (sum of top-level non-REDEFINES fields)
  let totalLength = 0;
  for (const field of fields) {
    if (!field.redefines) {
      const len = field.occurs ? field.byteLength * field.occurs.count : field.byteLength;
      totalLength += len;
    }
  }

  return { name, fields, totalLength };
}
