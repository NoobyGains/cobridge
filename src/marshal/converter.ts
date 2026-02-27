// High-level COBOL record marshaller.
//
// Converts between JSON objects and COBOL-format binary buffers using
// a copybook field definition. Handles group items, OCCURS,
// PIC X (alphanumeric), PIC 9 (numeric), packed decimal (COMP-3),
// and binary (COMP/BINARY) fields.
//
// Accepts both the rich parser AST (CopybookField from parser/types)
// and a simpler inline format (MarshalField) for convenience.

import { asciiToEbcdic, ebcdicToAscii, CodePage } from './ebcdic';
import { packDecimal, unpackDecimal } from './packed-decimal';
import { encodeBinary, decodeBinary, encodeComp1, decodeComp1, encodeComp2, decodeComp2, binarySize } from './binary';

/**
 * Simplified field definition for direct use with the marshaller.
 * This is the canonical format the marshaller works with internally.
 * The parser's CopybookField can be converted to this via fromParserField().
 */
export interface MarshalField {
  name: string;
  level: number;
  pic?: string;          // PIC clause, e.g., "X(10)", "9(5)V9(2)", "S9(7)"
  usage?: string;        // COMP, COMP-1, COMP-2, COMP-3, BINARY, DISPLAY
  occurs?: number;       // OCCURS count for arrays
  children?: MarshalField[];
  size?: number;         // Pre-calculated byte size (optional)
  signed?: boolean;      // Whether the field is signed (S prefix in PIC)
  redefines?: string;    // Name of the field this one REDEFINES (overlapping storage)
}

// Re-export MarshalField as CopybookField for backward compat with tests
export type { MarshalField as CopybookField };

export interface MarshalOptions {
  codePage?: CodePage;   // EBCDIC code page (default: '037')
  useEbcdic?: boolean;   // Whether to encode text as EBCDIC (default: false, uses ASCII)
}

/**
 * Convert a parser CopybookField (rich AST) to a MarshalField.
 * This bridges the parser output to the marshaller input.
 */
export function fromParserField(field: any): MarshalField {
  const result: MarshalField = {
    name: field.name,
    level: field.levelNumber ?? field.level ?? 1,
  };

  // Convert picture (parser PicInfo) to pic string
  if (field.picture) {
    result.pic = field.picture.raw;
    result.signed = field.picture.signed;
  } else if (field.pic) {
    result.pic = field.pic;
  }

  // Convert usage enum to string
  if (field.usage) {
    result.usage = typeof field.usage === 'string' ? field.usage : String(field.usage);
  }

  // Convert occurs (OccursClause) to count
  if (field.occurs) {
    result.occurs = typeof field.occurs === 'number' ? field.occurs : field.occurs.count;
  }

  // Convert byteLength to size
  if (field.byteLength !== undefined) {
    result.size = field.byteLength;
  } else if (field.size !== undefined) {
    result.size = field.size;
  }

  // Convert redefines
  if (field.redefines) {
    result.redefines = field.redefines;
  }

  // Convert children recursively
  if (field.children && field.children.length > 0) {
    result.children = field.children.map(fromParserField);
  }

  return result;
}

/**
 * Convert an array of parser fields to MarshalFields.
 */
export function fromParserFields(fields: any[]): MarshalField[] {
  return fields.map(fromParserField);
}

// --- Internal PIC parsing ---

interface ParsedPic {
  type: 'alpha' | 'numeric';
  totalDigits: number;
  decimalDigits: number;
  signed: boolean;
  displaySize: number;
}

function parsePic(pic: string): ParsedPic {
  const signed = pic.includes('S') || pic.includes('s');
  const upper = pic.toUpperCase().replace(/^S/, '');

  if (upper.includes('X') || upper.includes('A')) {
    const totalDigits = expandPicCount(upper);
    return { type: 'alpha', totalDigits, decimalDigits: 0, signed: false, displaySize: totalDigits };
  }

  const parts = upper.split('V');
  const intDigits = expandPicCount(parts[0] || '');
  const decDigits = parts[1] ? expandPicCount(parts[1]) : 0;
  const totalDigits = intDigits + decDigits;
  const displaySize = totalDigits + (signed ? 1 : 0);

  return { type: 'numeric', totalDigits, decimalDigits: decDigits, signed, displaySize };
}

function expandPicCount(segment: string): number {
  let count = 0;
  const parenPattern = /[9XA]\((\d+)\)/gi;
  let match;
  let lastIndex = 0;

  while ((match = parenPattern.exec(segment)) !== null) {
    const before = segment.substring(lastIndex, match.index);
    count += (before.match(/[9XA]/gi) || []).length;
    count += parseInt(match[1], 10);
    lastIndex = parenPattern.lastIndex;
  }

  const remaining = segment.substring(lastIndex);
  count += (remaining.match(/[9XA]/gi) || []).length;
  return count;
}

function fieldByteSize(field: MarshalField): number {
  if (field.size !== undefined) return field.size;

  if (field.children && field.children.length > 0) {
    let total = 0;
    for (const child of field.children) {
      // REDEFINES fields share storage with the field they redefine,
      // so they do not add to the group's total size.
      if (child.redefines) continue;
      total += fieldByteSize(child) * (child.occurs || 1);
    }
    return total;
  }

  if (!field.pic) return 0;

  const pic = parsePic(field.pic);
  const usage = (field.usage || 'DISPLAY').toUpperCase();

  switch (usage) {
    case 'COMP-3':
    case 'PACKED-DECIMAL':
      return Math.floor(pic.totalDigits / 2) + 1;
    case 'COMP':
    case 'COMP-4':
    case 'BINARY':
      return binarySize(pic.totalDigits);
    case 'COMP-1':
      return 4;
    case 'COMP-2':
      return 8;
    case 'DISPLAY':
    default:
      return pic.displaySize;
  }
}

/**
 * Marshal a JSON object to a COBOL-format buffer using a copybook definition.
 *
 * @param json - The JSON object with field values
 * @param copybook - The copybook field tree (top-level fields)
 * @param options - Marshal options (code page, EBCDIC mode)
 * @returns Buffer containing the COBOL record
 */
export function marshalToCobol(
  json: Record<string, any>,
  copybook: MarshalField[],
  options: MarshalOptions = {}
): Buffer {
  let totalSize = 0;
  for (const field of copybook) {
    totalSize += fieldByteSize(field) * (field.occurs || 1);
  }

  const buf = Buffer.alloc(totalSize);
  let offset = 0;
  const topLevelOffsets = new Map<string, number>();

  for (const field of copybook) {
    offset = marshalField(buf, offset, json, field, options, topLevelOffsets);
  }

  return buf;
}

function marshalField(
  buf: Buffer,
  offset: number,
  data: Record<string, any>,
  field: MarshalField,
  options: MarshalOptions,
  siblingOffsets?: Map<string, number>
): number {
  const key = fieldKey(field.name);
  const occurs = field.occurs || 1;

  // Handle REDEFINES: write at the same offset as the redefined field
  // and do NOT advance the caller's offset.
  if (field.redefines && siblingOffsets) {
    const redefOffset = siblingOffsets.get(field.redefines);
    if (redefOffset !== undefined) {
      // Marshal the REDEFINES view at the redefined field's offset
      const tempOffset = redefOffset;
      marshalFieldInner(buf, tempOffset, data, field, options);
      // Return the original offset unchanged -- REDEFINES doesn't advance
      return offset;
    }
  }

  // Track this field's offset for potential REDEFINES lookups by later siblings
  if (siblingOffsets) {
    siblingOffsets.set(field.name, offset);
  }

  for (let i = 0; i < occurs; i++) {
    let value: any;
    if (occurs > 1) {
      const arr = data[key];
      value = Array.isArray(arr) ? arr[i] : undefined;
    } else {
      value = data[key];
    }

    if (field.children && field.children.length > 0) {
      const groupData = (typeof value === 'object' && value !== null) ? value : {};
      const childOffsets = new Map<string, number>();
      for (const child of field.children) {
        offset = marshalField(buf, offset, groupData, child, options, childOffsets);
      }
    } else {
      const size = fieldByteSize(field);
      writeFieldValue(buf, offset, value, field, size, options);
      offset += size;
    }
  }

  return offset;
}

/**
 * Inner marshal for REDEFINES: writes data at a fixed offset without advancing.
 */
function marshalFieldInner(
  buf: Buffer,
  offset: number,
  data: Record<string, any>,
  field: MarshalField,
  options: MarshalOptions
): number {
  const key = fieldKey(field.name);
  const occurs = field.occurs || 1;

  for (let i = 0; i < occurs; i++) {
    let value: any;
    if (occurs > 1) {
      const arr = data[key];
      value = Array.isArray(arr) ? arr[i] : undefined;
    } else {
      value = data[key];
    }

    if (field.children && field.children.length > 0) {
      const groupData = (typeof value === 'object' && value !== null) ? value : {};
      for (const child of field.children) {
        offset = marshalFieldInner(buf, offset, groupData, child, options);
      }
    } else {
      const size = fieldByteSize(field);
      if (offset + size <= buf.length) {
        writeFieldValue(buf, offset, value, field, size, options);
      }
      offset += size;
    }
  }

  return offset;
}

function writeFieldValue(
  buf: Buffer,
  offset: number,
  value: any,
  field: MarshalField,
  size: number,
  options: MarshalOptions
): void {
  const pic = field.pic ? parsePic(field.pic) : null;
  const usage = (field.usage || 'DISPLAY').toUpperCase();

  if (!pic) return;

  switch (usage) {
    case 'COMP-3':
    case 'PACKED-DECIMAL': {
      const numVal = value ?? 0;
      const packed = packDecimal(numVal, pic.totalDigits, pic.decimalDigits);
      packed.copy(buf, offset);
      break;
    }

    case 'COMP':
    case 'COMP-4':
    case 'BINARY': {
      const numVal = Number(value ?? 0);
      const scaled = pic.decimalDigits > 0
        ? Math.round(numVal * Math.pow(10, pic.decimalDigits))
        : numVal;
      const encoded = encodeBinary(scaled, size, pic.signed);
      encoded.copy(buf, offset);
      break;
    }

    case 'COMP-1': {
      encodeComp1(Number(value ?? 0)).copy(buf, offset);
      break;
    }

    case 'COMP-2': {
      encodeComp2(Number(value ?? 0)).copy(buf, offset);
      break;
    }

    case 'DISPLAY':
    default: {
      if (pic.type === 'alpha') {
        const strVal = String(value ?? '').substring(0, size);
        const padded = strVal.padEnd(size, ' ');
        if (options.useEbcdic) {
          asciiToEbcdic(padded, options.codePage).copy(buf, offset);
        } else {
          buf.write(padded, offset, size, 'ascii');
        }
      } else {
        const numVal = value ?? 0;
        let numStr = Math.abs(Number(numVal)).toString();

        if (pic.decimalDigits > 0) {
          const parts = numStr.split('.');
          const intPart = parts[0];
          const fracPart = (parts[1] || '').padEnd(pic.decimalDigits, '0')
            .substring(0, pic.decimalDigits);
          numStr = intPart + fracPart;
        }

        numStr = numStr.padStart(pic.totalDigits, '0');
        if (numStr.length > pic.totalDigits) {
          numStr = numStr.substring(numStr.length - pic.totalDigits);
        }

        if (pic.signed) {
          const signChar = Number(numVal) < 0 ? '-' : '+';
          const display = signChar + numStr;
          if (options.useEbcdic) {
            asciiToEbcdic(display, options.codePage).copy(buf, offset);
          } else {
            buf.write(display, offset, size, 'ascii');
          }
        } else {
          if (options.useEbcdic) {
            asciiToEbcdic(numStr, options.codePage).copy(buf, offset);
          } else {
            buf.write(numStr, offset, size, 'ascii');
          }
        }
      }
      break;
    }
  }
}

/**
 * Marshal a COBOL-format buffer to a JSON object using a copybook definition.
 *
 * @param buffer - Buffer containing the COBOL record
 * @param copybook - The copybook field tree
 * @param options - Marshal options
 * @returns JSON object with field values
 */
export function marshalFromCobol(
  buffer: Buffer,
  copybook: MarshalField[],
  options: MarshalOptions = {}
): Record<string, any> {
  const result: Record<string, any> = {};
  let offset = 0;
  const topLevelOffsets = new Map<string, number>();

  for (const field of copybook) {
    const { value, newOffset } = unmarshalField(buffer, offset, field, options, topLevelOffsets);
    result[fieldKey(field.name)] = value;
    offset = newOffset;
  }

  return result;
}

function unmarshalField(
  buf: Buffer,
  offset: number,
  field: MarshalField,
  options: MarshalOptions,
  siblingOffsets?: Map<string, number>
): { value: any; newOffset: number } {
  const occurs = field.occurs || 1;

  if (occurs > 1) {
    const arr: any[] = [];
    for (let i = 0; i < occurs; i++) {
      const { value, newOffset } = unmarshalSingleField(buf, offset, field, options, siblingOffsets);
      arr.push(value);
      offset = newOffset;
    }
    return { value: arr, newOffset: offset };
  }

  return unmarshalSingleField(buf, offset, field, options, siblingOffsets);
}

function unmarshalSingleField(
  buf: Buffer,
  offset: number,
  field: MarshalField,
  options: MarshalOptions,
  siblingOffsets?: Map<string, number>
): { value: any; newOffset: number } {
  // Handle REDEFINES: read from the redefined field's offset,
  // but don't advance the caller's offset.
  if (field.redefines && siblingOffsets) {
    const redefOffset = siblingOffsets.get(field.redefines);
    if (redefOffset !== undefined) {
      const { value } = unmarshalSingleFieldInner(buf, redefOffset, field, options);
      return { value, newOffset: offset };
    }
  }

  // Track this field's offset for REDEFINES lookups
  if (siblingOffsets) {
    siblingOffsets.set(field.name, offset);
  }

  if (field.children && field.children.length > 0) {
    const group: Record<string, any> = {};
    let groupOffset = offset;
    const childOffsets = new Map<string, number>();
    for (const child of field.children) {
      const { value, newOffset } = unmarshalField(buf, groupOffset, child, options, childOffsets);
      group[fieldKey(child.name)] = value;
      groupOffset = newOffset;
    }
    return { value: group, newOffset: groupOffset };
  }

  const size = fieldByteSize(field);
  const value = readFieldValue(buf, offset, field, size, options);
  return { value, newOffset: offset + size };
}

function unmarshalSingleFieldInner(
  buf: Buffer,
  offset: number,
  field: MarshalField,
  options: MarshalOptions
): { value: any; newOffset: number } {
  if (field.children && field.children.length > 0) {
    const group: Record<string, any> = {};
    let groupOffset = offset;
    for (const child of field.children) {
      const { value, newOffset } = unmarshalField(buf, groupOffset, child, options);
      group[fieldKey(child.name)] = value;
      groupOffset = newOffset;
    }
    return { value: group, newOffset: groupOffset };
  }

  const size = fieldByteSize(field);
  const value = readFieldValue(buf, offset, field, size, options);
  return { value, newOffset: offset + size };
}

function readFieldValue(
  buf: Buffer,
  offset: number,
  field: MarshalField,
  size: number,
  options: MarshalOptions
): any {
  const pic = field.pic ? parsePic(field.pic) : null;
  const usage = (field.usage || 'DISPLAY').toUpperCase();

  if (!pic) return null;

  const fieldBuf = buf.subarray(offset, offset + size);

  switch (usage) {
    case 'COMP-3':
    case 'PACKED-DECIMAL':
      return unpackDecimal(fieldBuf, pic.decimalDigits);

    case 'COMP':
    case 'COMP-4':
    case 'BINARY': {
      const raw = decodeBinary(fieldBuf, pic.signed);
      if (pic.decimalDigits > 0) {
        return raw / Math.pow(10, pic.decimalDigits);
      }
      return raw;
    }

    case 'COMP-1':
      return decodeComp1(fieldBuf);

    case 'COMP-2':
      return decodeComp2(fieldBuf);

    case 'DISPLAY':
    default: {
      if (pic.type === 'alpha') {
        let str: string;
        if (options.useEbcdic) {
          str = ebcdicToAscii(fieldBuf, options.codePage);
        } else {
          str = fieldBuf.toString('ascii');
        }
        return str.replace(/\s+$/, '');
      } else {
        let str: string;
        if (options.useEbcdic) {
          str = ebcdicToAscii(fieldBuf, options.codePage);
        } else {
          str = fieldBuf.toString('ascii');
        }

        let numStr = str.trim();
        const isNegative = numStr.startsWith('-');
        numStr = numStr.replace(/^[+-]/, '');

        let result = parseFloat(numStr) || 0;
        if (pic.decimalDigits > 0) {
          result = result / Math.pow(10, pic.decimalDigits);
        }
        return isNegative ? -result : result;
      }
    }
  }
}

/**
 * Convert a COBOL field name to a JSON-friendly key.
 * Converts hyphens to camelCase.
 */
function fieldKey(name: string): string {
  return name
    .toLowerCase()
    .replace(/-([a-z0-9])/g, (_, char) => char.toUpperCase());
}
