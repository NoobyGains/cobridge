// COMP/BINARY encoding and decoding for COBOL data types.
//
// COBOL binary types:
//   COMP / BINARY: Big-endian integer (2, 4, or 8 bytes)
//   COMP-1: Single-precision IEEE 754 float (4 bytes)
//   COMP-2: Double-precision IEEE 754 float (8 bytes)
//
// All multi-byte values use big-endian (network) byte order,
// which is the mainframe standard.

/**
 * Encode a numeric value as a big-endian binary integer.
 *
 * @param value - The integer value to encode
 * @param bytes - Number of bytes: 2 (halfword), 4 (fullword), or 8 (doubleword)
 * @param signed - Whether to use signed (two's complement) encoding
 * @returns Buffer containing the encoded value
 */
export function encodeBinary(value: number, bytes: number, signed: boolean = true): Buffer {
  const buf = Buffer.alloc(bytes);

  switch (bytes) {
    case 2:
      if (signed) {
        buf.writeInt16BE(value, 0);
      } else {
        buf.writeUInt16BE(value, 0);
      }
      break;
    case 4:
      if (signed) {
        buf.writeInt32BE(value, 0);
      } else {
        buf.writeUInt32BE(value, 0);
      }
      break;
    case 8:
      // Use BigInt for 8-byte values
      if (signed) {
        buf.writeBigInt64BE(BigInt(value), 0);
      } else {
        buf.writeBigUInt64BE(BigInt(value), 0);
      }
      break;
    default:
      throw new Error(`Unsupported byte size: ${bytes}. Use 2, 4, or 8.`);
  }

  return buf;
}

/**
 * Decode a big-endian binary buffer to a number.
 *
 * @param buffer - Buffer containing the binary data
 * @param signed - Whether to interpret as signed (two's complement)
 * @returns The decoded numeric value
 */
export function decodeBinary(buffer: Buffer, signed: boolean = true): number {
  switch (buffer.length) {
    case 2:
      return signed ? buffer.readInt16BE(0) : buffer.readUInt16BE(0);
    case 4:
      return signed ? buffer.readInt32BE(0) : buffer.readUInt32BE(0);
    case 8: {
      const bigVal = signed ? buffer.readBigInt64BE(0) : buffer.readBigUInt64BE(0);
      return Number(bigVal);
    }
    default:
      throw new Error(`Unsupported buffer size: ${buffer.length}. Expected 2, 4, or 8.`);
  }
}

/**
 * Encode a number as COMP-1 (single-precision IEEE 754 float, 4 bytes, big-endian).
 *
 * @param value - The float value to encode
 * @returns 4-byte Buffer
 */
export function encodeComp1(value: number): Buffer {
  const buf = Buffer.alloc(4);
  buf.writeFloatBE(value, 0);
  return buf;
}

/**
 * Decode a COMP-1 (single-precision float) buffer.
 *
 * @param buffer - 4-byte buffer
 * @returns The decoded float value
 */
export function decodeComp1(buffer: Buffer): number {
  if (buffer.length !== 4) {
    throw new Error(`COMP-1 requires exactly 4 bytes, got ${buffer.length}`);
  }
  return buffer.readFloatBE(0);
}

/**
 * Encode a number as COMP-2 (double-precision IEEE 754 float, 8 bytes, big-endian).
 *
 * @param value - The double value to encode
 * @returns 8-byte Buffer
 */
export function encodeComp2(value: number): Buffer {
  const buf = Buffer.alloc(8);
  buf.writeDoubleBE(value, 0);
  return buf;
}

/**
 * Decode a COMP-2 (double-precision float) buffer.
 *
 * @param buffer - 8-byte buffer
 * @returns The decoded double value
 */
export function decodeComp2(buffer: Buffer): number {
  if (buffer.length !== 8) {
    throw new Error(`COMP-2 requires exactly 8 bytes, got ${buffer.length}`);
  }
  return buffer.readDoubleBE(0);
}

/**
 * Determine the number of bytes needed for a COMP/BINARY field
 * based on the number of digits in the PIC clause.
 *
 * COBOL rules:
 *   1-4 digits  -> 2 bytes (halfword)
 *   5-9 digits  -> 4 bytes (fullword)
 *   10-18 digits -> 8 bytes (doubleword)
 */
export function binarySize(digits: number): number {
  if (digits <= 4) return 2;
  if (digits <= 9) return 4;
  if (digits <= 18) return 8;
  throw new Error(`COMP/BINARY supports up to 18 digits, got ${digits}`);
}
