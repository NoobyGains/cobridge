/**
 * COBridge — COBOL Copybook Lexer
 *
 * Tokenizes COBOL copybook source text into a stream of typed tokens.
 * Handles both fixed-format (columns 7-72) and free-format source,
 * COBOL comment lines, and continuation lines.
 */

export enum TokenType {
  LEVEL_NUMBER = "LEVEL_NUMBER",
  DATA_NAME = "DATA_NAME",
  PIC = "PIC",
  PIC_STRING = "PIC_STRING",
  USAGE = "USAGE",
  USAGE_TYPE = "USAGE_TYPE",
  OCCURS = "OCCURS",
  TIMES = "TIMES",
  TO = "TO",
  DEPENDING = "DEPENDING",
  ON = "ON",
  REDEFINES = "REDEFINES",
  VALUE = "VALUE",
  VALUE_LITERAL = "VALUE_LITERAL",
  IS = "IS",
  PERIOD = "PERIOD",
  FILLER = "FILLER",
  NUMBER = "NUMBER",
  EOF = "EOF",
}

export interface Token {
  type: TokenType;
  value: string;
  line: number;
  col: number;
}

/** Reserved keywords mapped to their token types */
const KEYWORDS: Record<string, TokenType> = {
  PIC: TokenType.PIC,
  PICTURE: TokenType.PIC,
  USAGE: TokenType.USAGE,
  DISPLAY: TokenType.USAGE_TYPE,
  COMP: TokenType.USAGE_TYPE,
  "COMP-1": TokenType.USAGE_TYPE,
  "COMP-2": TokenType.USAGE_TYPE,
  "COMP-3": TokenType.USAGE_TYPE,
  BINARY: TokenType.USAGE_TYPE,
  "PACKED-DECIMAL": TokenType.USAGE_TYPE,
  COMPUTATIONAL: TokenType.USAGE_TYPE,
  "COMPUTATIONAL-1": TokenType.USAGE_TYPE,
  "COMPUTATIONAL-2": TokenType.USAGE_TYPE,
  "COMPUTATIONAL-3": TokenType.USAGE_TYPE,
  OCCURS: TokenType.OCCURS,
  TIMES: TokenType.TIMES,
  TO: TokenType.TO,
  DEPENDING: TokenType.DEPENDING,
  ON: TokenType.ON,
  REDEFINES: TokenType.REDEFINES,
  VALUE: TokenType.VALUE,
  VALUES: TokenType.VALUE,
  IS: TokenType.IS,
  FILLER: TokenType.FILLER,
};

/** Normalize COMPUTATIONAL variants to COMP forms */
function normalizeUsage(value: string): string {
  const map: Record<string, string> = {
    COMPUTATIONAL: "COMP",
    "COMPUTATIONAL-1": "COMP-1",
    "COMPUTATIONAL-2": "COMP-2",
    "COMPUTATIONAL-3": "COMP-3",
  };
  return map[value] || value;
}

/**
 * Detect whether the source uses fixed-format (columns matter)
 * or free-format. Heuristic: if most non-empty lines are >= 7 chars
 * and column 7 often has space or *, treat as fixed.
 */
function isFixedFormat(source: string): boolean {
  const lines = source.split(/\r?\n/);
  let fixedIndicators = 0;
  let totalNonEmpty = 0;

  for (const line of lines) {
    if (line.trim().length === 0) continue;
    totalNonEmpty++;
    // Fixed-format: columns 1-6 are sequence, col 7 is indicator, 8-72 are code
    if (line.length >= 7) {
      const col7 = line[6];
      // Column 7 is typically space, *, /, or - (continuation)
      if (col7 === " " || col7 === "*" || col7 === "/" || col7 === "-") {
        fixedIndicators++;
      }
    }
  }

  if (totalNonEmpty === 0) return false;
  return fixedIndicators / totalNonEmpty > 0.5;
}

/**
 * Pre-process source lines: strip sequence numbers, handle comments
 * and continuation, return clean content lines.
 */
function preprocessLines(source: string): { text: string; originalLine: number }[] {
  const rawLines = source.split(/\r?\n/);
  const fixed = isFixedFormat(source);
  const result: { text: string; originalLine: number }[] = [];

  for (let i = 0; i < rawLines.length; i++) {
    const raw = rawLines[i];

    if (fixed) {
      // Fixed format: need at least 7 columns
      if (raw.length < 7) {
        // Short line — treat as blank
        continue;
      }
      const indicator = raw[6];

      // Comment lines: * or / in column 7
      if (indicator === "*" || indicator === "/") {
        continue;
      }

      // Continuation line: - in column 7
      if (indicator === "-") {
        // Append to previous line (strip leading spaces from area A/B)
        const content = raw.substring(7, Math.min(raw.length, 72)).trimStart();
        if (result.length > 0) {
          result[result.length - 1].text += content;
        }
        continue;
      }

      // Normal line: extract columns 8-72
      const content = raw.substring(7, Math.min(raw.length, 72));
      result.push({ text: content, originalLine: i + 1 });
    } else {
      // Free format
      const trimmed = raw.trim();

      // Skip empty lines
      if (trimmed.length === 0) continue;

      // Skip comment lines (starting with *> in free format, or * for compatibility)
      if (trimmed.startsWith("*>") || trimmed.startsWith("*")) {
        continue;
      }

      result.push({ text: trimmed, originalLine: i + 1 });
    }
  }

  return result;
}

/**
 * Tokenize a COBOL copybook source string.
 */
export function tokenize(source: string): Token[] {
  const lines = preprocessLines(source);
  const tokens: Token[] = [];

  // Join all content into a single stream for easier multi-line parsing,
  // but track line numbers for error reporting.
  for (const { text, originalLine } of lines) {
    let pos = 0;
    const lineText = text;

    while (pos < lineText.length) {
      // Skip whitespace
      if (/\s/.test(lineText[pos])) {
        pos++;
        continue;
      }

      // Period terminator
      if (lineText[pos] === ".") {
        tokens.push({ type: TokenType.PERIOD, value: ".", line: originalLine, col: pos });
        pos++;
        continue;
      }

      // Check if we're right after a PIC/PICTURE keyword — next non-space word is the PIC string
      const lastToken = tokens.length > 0 ? tokens[tokens.length - 1] : null;
      const isPicContext =
        lastToken &&
        (lastToken.type === TokenType.PIC ||
          (lastToken.type === TokenType.IS &&
            tokens.length >= 2 &&
            tokens[tokens.length - 2].type === TokenType.PIC));

      if (isPicContext) {
        // Read the PIC string — can contain 9, X, A, V, S, P, Z, B, 0, /, +, -, ., *, CR, DB and (n)
        const picStart = pos;
        while (pos < lineText.length && !/[\s.]/.test(lineText[pos])) {
          pos++;
        }
        const picStr = lineText.substring(picStart, pos);
        tokens.push({
          type: TokenType.PIC_STRING,
          value: picStr,
          line: originalLine,
          col: picStart,
        });
        continue;
      }

      // Check if we're right after VALUE/VALUES — consume the literal
      const isValueContext = lastToken && lastToken.type === TokenType.VALUE;
      const isValueIsContext =
        lastToken &&
        lastToken.type === TokenType.IS &&
        tokens.length >= 2 &&
        tokens[tokens.length - 2].type === TokenType.VALUE;

      if (isValueContext || isValueIsContext) {
        const valStart = pos;
        if (lineText[pos] === "'" || lineText[pos] === '"') {
          // Quoted string literal
          const quote = lineText[pos];
          pos++;
          while (pos < lineText.length && lineText[pos] !== quote) {
            pos++;
          }
          if (pos < lineText.length) pos++; // skip closing quote
        } else {
          // Unquoted numeric or figurative constant (SPACES, ZEROS, etc.)
          while (pos < lineText.length && !/[\s.]/.test(lineText[pos])) {
            pos++;
          }
        }
        tokens.push({
          type: TokenType.VALUE_LITERAL,
          value: lineText.substring(valStart, pos),
          line: originalLine,
          col: valStart,
        });
        continue;
      }

      // Number (level number or OCCURS count)
      if (/[0-9]/.test(lineText[pos])) {
        const numStart = pos;
        while (pos < lineText.length && /[0-9]/.test(lineText[pos])) {
          pos++;
        }
        const numStr = lineText.substring(numStart, pos);

        // Check if the next char is not a letter/hyphen (otherwise it's a data name starting with a digit — rare but possible)
        if (pos < lineText.length && /[A-Za-z-]/.test(lineText[pos])) {
          // It's actually a data name starting with digits — continue reading
          while (pos < lineText.length && /[A-Za-z0-9-]/.test(lineText[pos])) {
            pos++;
          }
          const name = lineText.substring(numStart, pos);
          tokens.push({
            type: TokenType.DATA_NAME,
            value: name.toUpperCase(),
            line: originalLine,
            col: numStart,
          });
          continue;
        }

        // Determine if it's a level number based on position and value
        const num = parseInt(numStr, 10);
        const isLevelNumber =
          (tokens.length === 0 || tokens[tokens.length - 1].type === TokenType.PERIOD) &&
          ((num >= 1 && num <= 49) || num === 66 || num === 77 || num === 88);

        tokens.push({
          type: isLevelNumber ? TokenType.LEVEL_NUMBER : TokenType.NUMBER,
          value: numStr,
          line: originalLine,
          col: numStart,
        });
        continue;
      }

      // Word (keyword or data name)
      if (/[A-Za-z]/.test(lineText[pos])) {
        const wordStart = pos;
        while (pos < lineText.length && /[A-Za-z0-9-]/.test(lineText[pos])) {
          pos++;
        }
        let word = lineText.substring(wordStart, pos).toUpperCase();

        // Check for keyword
        const kwType = KEYWORDS[word];
        if (kwType) {
          const token: Token = {
            type: kwType,
            value: kwType === TokenType.USAGE_TYPE ? normalizeUsage(word) : word,
            line: originalLine,
            col: wordStart,
          };
          tokens.push(token);
        } else {
          tokens.push({
            type: TokenType.DATA_NAME,
            value: word,
            line: originalLine,
            col: wordStart,
          });
        }
        continue;
      }

      // Skip any other character (shouldn't normally happen)
      pos++;
    }
  }

  tokens.push({ type: TokenType.EOF, value: "", line: 0, col: 0 });
  return tokens;
}
