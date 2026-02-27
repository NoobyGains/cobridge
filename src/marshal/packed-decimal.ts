// COMP-3 Packed Decimal encoding/decoding for COBOL data.
//
// Packed decimal format:
//   - Each byte holds two digits (one per nibble), except the last byte
//   - The last byte's low nibble holds the sign: C=positive, D=negative, F=unsigned
//   - Number of bytes = floor((totalDigits + 1) / 2) + (if even totalDigits, +1 for leading nibble)
//   - Actually: bytes = floor((totalDigits / 2)) + 1
//
// Example: +12345 with PIC S9(5) COMP-3
//   Digits: 1 2 3 4 5, sign C
//   Packed: 0x12 0x34 0x5C  (3 bytes)

const SIGN_POSITIVE = 0x0C;
const SIGN_NEGATIVE = 0x0D;
const SIGN_UNSIGNED = 0x0F;

/**
 * Calculate the number of bytes needed for a packed decimal.
 */
function packedSize(totalDigits: number): number {
  return Math.floor(totalDigits / 2) + 1;
}

/**
 * Pack a numeric value into COMP-3 packed decimal format.
 *
 * @param value - The numeric value to pack (number or string for precision)
 * @param totalDigits - Total number of digits in the PIC clause (e.g., 9(5)V9(2) = 7)
 * @param decimalDigits - Number of digits after the implied decimal point
 * @returns Buffer containing the packed decimal
 */
export function packDecimal(
  value: number | string,
  totalDigits: number,
  decimalDigits: number = 0
): Buffer {
  if (totalDigits < 1 || totalDigits > 18) {
    throw new Error(`totalDigits must be between 1 and 18, got ${totalDigits}`);
  }

  // Convert to string, handling the decimal scaling
  let strValue: string;
  if (typeof value === 'string') {
    strValue = value;
  } else {
    strValue = value.toString();
  }

  // Determine sign
  const isNegative = strValue.startsWith('-');
  strValue = strValue.replace(/^[+-]/, '');

  // Split on decimal point
  const parts = strValue.split('.');
  let intPart = parts[0] || '0';
  let fracPart = parts[1] || '';

  // Scale the fractional part: pad or truncate to decimalDigits
  if (fracPart.length < decimalDigits) {
    fracPart = fracPart.padEnd(decimalDigits, '0');
  } else if (fracPart.length > decimalDigits) {
    fracPart = fracPart.substring(0, decimalDigits);
  }

  // Combine into a single digit string (implied decimal)
  let digits = intPart + fracPart;

  // Pad with leading zeros to match totalDigits
  const intDigitsNeeded = totalDigits - decimalDigits;
  if (intPart.length > intDigitsNeeded) {
    throw new Error(
      `Value ${value} exceeds capacity: integer part has ${intPart.length} digits but only ${intDigitsNeeded} allowed`
    );
  }
  digits = digits.padStart(totalDigits, '0');

  if (digits.length > totalDigits) {
    throw new Error(`Value ${value} exceeds total digit capacity of ${totalDigits}`);
  }

  // Determine sign nibble
  const signNibble = isNegative ? SIGN_NEGATIVE : SIGN_POSITIVE;

  // Build packed bytes
  const numBytes = packedSize(totalDigits);
  const buf = Buffer.alloc(numBytes);

  // If totalDigits is even, the first nibble is a leading zero
  // Pack digits into nibbles, with sign as last nibble
  const nibbles: number[] = [];
  for (const ch of digits) {
    nibbles.push(parseInt(ch, 10));
  }
  nibbles.push(signNibble);

  // If we have an even number of nibbles total, pad with a leading zero nibble
  if (nibbles.length % 2 !== 0) {
    nibbles.unshift(0);
  }

  for (let i = 0; i < nibbles.length; i += 2) {
    buf[i / 2] = (nibbles[i] << 4) | nibbles[i + 1];
  }

  return buf;
}

/**
 * Unpack a COMP-3 packed decimal buffer to a number.
 *
 * @param buffer - Buffer containing packed decimal data
 * @param decimalDigits - Number of implied decimal digits
 * @returns The unpacked numeric value
 */
export function unpackDecimal(buffer: Buffer, decimalDigits: number = 0): number {
  if (buffer.length === 0) {
    throw new Error('Empty buffer');
  }

  // Extract all nibbles
  const nibbles: number[] = [];
  for (let i = 0; i < buffer.length; i++) {
    nibbles.push((buffer[i] >> 4) & 0x0F);
    nibbles.push(buffer[i] & 0x0F);
  }

  // Last nibble is the sign
  const signNibble = nibbles.pop()!;
  const isNegative = signNibble === SIGN_NEGATIVE;

  // Remaining nibbles are digits
  // Skip any leading zero nibbles that are just padding (for even digit counts)
  let digitStr = nibbles.map(n => n.toString()).join('');

  // Insert decimal point if needed
  if (decimalDigits > 0 && digitStr.length > decimalDigits) {
    const intPart = digitStr.substring(0, digitStr.length - decimalDigits);
    const fracPart = digitStr.substring(digitStr.length - decimalDigits);
    digitStr = intPart + '.' + fracPart;
  } else if (decimalDigits > 0) {
    // All digits are fractional
    digitStr = '0.' + digitStr.padStart(decimalDigits, '0');
  }

  let result = parseFloat(digitStr);
  if (isNegative) {
    result = -result;
  }

  return result;
}
