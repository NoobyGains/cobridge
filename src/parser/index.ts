/**
 * COBridge — COBOL Copybook Parser
 *
 * Main entry point for parsing COBOL copybook definitions into
 * a typed AST with calculated byte lengths and offsets.
 *
 * Usage:
 *   import { parseCopybook } from "./parser";
 *   const ast = parseCopybook(copybookSource);
 */

export { parseCopybook, parsePicString, calculateByteLength } from "./parser.js";
export { tokenize } from "./lexer.js";
export type { Token } from "./lexer.js";
export { TokenType } from "./lexer.js";
export {
  UsageType,
  type PicInfo,
  type OccursClause,
  type CopybookField,
  type Copybook,
} from "./types.js";
