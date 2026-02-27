/**
 * COBridge — COBOL Copybook AST Types
 *
 * These types represent the parsed structure of a COBOL copybook.
 * A copybook defines record layouts used for data interchange between
 * COBOL programs and modern systems.
 */

/** COBOL USAGE clause values that determine storage format */
export enum UsageType {
  DISPLAY = "DISPLAY",
  COMP = "COMP",
  COMP_1 = "COMP-1",
  COMP_2 = "COMP-2",
  COMP_3 = "COMP-3",
  BINARY = "BINARY",
  PACKED_DECIMAL = "PACKED-DECIMAL",
}

/** Parsed PIC (PICTURE) clause information */
export interface PicInfo {
  /** Category: alphanumeric, numeric, or alphabetic */
  type: "alphanumeric" | "numeric" | "alpha";
  /** Total digit/character positions (including decimals) */
  length: number;
  /** Number of decimal positions (after V) */
  decimals: number;
  /** Whether the field has an S (sign) indicator */
  signed: boolean;
  /** The original PIC string as written in the copybook */
  raw: string;
}

/** OCCURS clause — fixed or variable-length arrays */
export interface OccursClause {
  /** Fixed count, or maximum count when DEPENDING ON is used */
  count: number;
  /** Minimum count for variable-length (OCCURS n TO m DEPENDING ON) */
  min?: number;
  /** Field name referenced by DEPENDING ON */
  dependingOn?: string;
}

/** A single field (elementary or group item) in the copybook */
export interface CopybookField {
  /** COBOL level number: 01-49, 66, 77, 88 */
  levelNumber: number;
  /** Field name (COBOL data-name), e.g. WS-ACCOUNT-NUMBER */
  name: string;
  /** Parsed PIC clause info — undefined for group items */
  picture?: PicInfo;
  /** Storage format — defaults to DISPLAY */
  usage: UsageType;
  /** OCCURS clause for repeating fields */
  occurs?: OccursClause;
  /** Name of the field this one REDEFINES (overlapping storage) */
  redefines?: string;
  /** Child fields — present for group items */
  children: CopybookField[];
  /** True if this is a group item (has children, no PIC clause) */
  isGroup: boolean;
  /** Computed byte length of this field (including children/occurs) */
  byteLength: number;
  /** Computed byte offset from start of the record */
  startOffset: number;
}

/** Root node representing an entire parsed copybook */
export interface Copybook {
  /** Copybook name — derived from the 01-level name or file */
  name: string;
  /** Top-level fields (typically one 01-level with nested children) */
  fields: CopybookField[];
  /** Total record length in bytes */
  totalLength: number;
}
