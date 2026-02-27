# Data Marshalling

Complete guide to converting data between JSON and COBOL binary record formats.

## Overview

COBOL programs store data in fixed-length binary records. Alphanumeric fields are encoded
in EBCDIC, numeric fields may use packed decimal (COMP-3) or big-endian binary (COMP),
and every field occupies a precisely defined number of bytes determined by the copybook.

**Marshalling** is the process of converting between these binary record layouts and the
JSON objects used by modern applications. COBridge's marshalling layer handles the full
round trip:

- **JSON to COBOL** (`marshalToCobol`) -- serialise a JSON object into a fixed-length
  `Buffer` that a COBOL program can read as its LINKAGE SECTION.
- **COBOL to JSON** (`marshalFromCobol`) -- deserialise a COBOL record buffer back into a
  plain JavaScript object.

The marshaller walks a copybook field tree and applies the correct encoding for each field
based on its PIC clause and USAGE.

---

## EBCDIC Conversion

### What Is EBCDIC?

EBCDIC (Extended Binary Coded Decimal Interchange Code) is the character encoding used by
IBM mainframes and many COBOL systems. Unlike ASCII, which arranges letters in a
contiguous range, EBCDIC scatters them across several non-contiguous zones. Any system
that exchanges text data with a mainframe must convert between the two encodings.

### Supported Code Pages

COBridge supports three commonly used EBCDIC code pages:

| Code Page | Name                 | Typical Use                        |
|-----------|----------------------|------------------------------------|
| `037`     | US/Canada            | North American mainframes (default)|
| `500`     | International        | European and multi-national sites  |
| `1047`    | Latin 1/Open Systems | Unix System Services on z/OS       |

Each code page is a 256-entry lookup table mapping EBCDIC byte values to their ASCII
equivalents. COBridge pre-computes forward (EBCDIC-to-ASCII) and reverse (ASCII-to-EBCDIC)
tables for every supported code page at module load time.

### API

#### `ebcdicToAscii(buffer, codePage?)`

Convert an EBCDIC-encoded `Buffer` to an ASCII string.

```ts
import { ebcdicToAscii } from 'cobridge/marshal/ebcdic';

const ebcdicBuf = Buffer.from([0xC1, 0xC2, 0xC3]); // 'ABC' in CP037
const text = ebcdicToAscii(ebcdicBuf);
console.log(text); // 'ABC'
```

**Parameters:**

| Name       | Type     | Default | Description                          |
|------------|----------|---------|--------------------------------------|
| `buffer`   | `Buffer` | --      | Buffer containing EBCDIC-encoded data|
| `codePage` | `CodePage` | `'037'` | EBCDIC code page to use            |

**Returns:** `string` -- the decoded ASCII text.

Untranslatable bytes (those mapping to `0x1A` SUB) are replaced with `?`.

#### `asciiToEbcdic(str, codePage?)`

Convert an ASCII string to an EBCDIC-encoded `Buffer`.

```ts
import { asciiToEbcdic } from 'cobridge/marshal/ebcdic';

const ebcdicBuf = asciiToEbcdic('Hello', '037');
console.log(ebcdicBuf); // <Buffer c8 85 93 93 96>
```

**Parameters:**

| Name       | Type       | Default | Description                       |
|------------|------------|---------|-----------------------------------|
| `str`      | `string`   | --      | ASCII string to convert           |
| `codePage` | `CodePage` | `'037'` | EBCDIC code page to use           |

**Returns:** `Buffer` -- the EBCDIC-encoded bytes.

Characters with no mapping in the target code page are encoded as EBCDIC `?` (`0x3F`).

### Character Mapping Notes

- Code page `500` differs from `037` in the positions of punctuation characters such as
  the cent sign, exclamation mark, dollar sign, and brackets.
- Code page `1047` maps EBCDIC `0x15` (Newline) and `0x25` (Line Feed) to ASCII `0x0A`
  (LF), which is the convention on Unix-based z/OS environments.
- All three code pages share the same digit (`0`--`9`) and letter (`A`--`Z`, `a`--`z`)
  mappings derived from the base CP037 table.

---

## Packed Decimal (COMP-3)

### What Is COMP-3?

COMP-3 (also known as packed decimal) is a compact numeric encoding where each byte stores
two decimal digits, one per nibble (half-byte). The final nibble holds a sign indicator:

| Nibble | Meaning    |
|--------|------------|
| `C`    | Positive   |
| `D`    | Negative   |
| `F`    | Unsigned   |

This encoding is far more space-efficient than DISPLAY format and is the most common
storage type for monetary and high-precision values in COBOL.

### How It Works

Consider `+12345` stored with `PIC S9(5) COMP-3`:

1. The digits are `1 2 3 4 5` and the sign is positive (`C`).
2. Pair digits into nibbles: `12 34 5C`.
3. Result: three bytes `0x12 0x34 0x5C`.

The byte length for a packed decimal is always `floor(totalDigits / 2) + 1`.

For a negative value such as `-123.45` stored with `PIC S9(3)V9(2) COMP-3`:

1. Total digits = 5, decimal digits = 2. The implied-decimal value is `12345`.
2. Sign is negative (`D`).
3. Nibbles: `01 23 45 D` (padded to even nibble count) -- four bytes `0x01 0x23 0x45 0x0D`.

> Wait -- let us be more precise. With 5 total digits the byte count is
> `floor(5/2) + 1 = 3`. The nibbles are `1 2 3 4 5 D` which is 6 nibbles = 3 bytes:
> `0x12 0x34 0x5D`.

### API

#### `packDecimal(value, totalDigits, decimalDigits?)`

Pack a numeric value into COMP-3 format.

```ts
import { packDecimal } from 'cobridge/marshal/packed-decimal';

// PIC S9(5) COMP-3: pack +12345
const buf1 = packDecimal(12345, 5);
console.log(buf1); // <Buffer 12 34 5c>

// PIC S9(5)V9(2) COMP-3: pack -123.45
const buf2 = packDecimal(-123.45, 7, 2);
console.log(buf2); // <Buffer 01 23 45 0d>
```

**Parameters:**

| Name            | Type               | Default | Description                              |
|-----------------|--------------------|---------|------------------------------------------|
| `value`         | `number \| string` | --      | The numeric value to pack                |
| `totalDigits`   | `number`           | --      | Total digits in the PIC clause (1--18)   |
| `decimalDigits` | `number`           | `0`     | Digits after the implied decimal point   |

**Returns:** `Buffer` -- the packed decimal bytes.

String values are accepted for precision-sensitive use cases where floating-point
rounding must be avoided.

**Throws:** `Error` if the value exceeds the digit capacity or `totalDigits` is outside
the range 1--18.

#### `unpackDecimal(buffer, decimalDigits?)`

Unpack a COMP-3 buffer to a JavaScript number.

```ts
import { unpackDecimal } from 'cobridge/marshal/packed-decimal';

const buf = Buffer.from([0x12, 0x34, 0x5C]);
const value = unpackDecimal(buf);
console.log(value); // 12345

const bufDec = Buffer.from([0x01, 0x23, 0x45, 0x0D]);
const valueDec = unpackDecimal(bufDec, 2);
console.log(valueDec); // -123.45
```

**Parameters:**

| Name            | Type     | Default | Description                           |
|-----------------|----------|---------|---------------------------------------|
| `buffer`        | `Buffer` | --      | Buffer containing packed decimal data |
| `decimalDigits` | `number` | `0`     | Number of implied decimal digits      |

**Returns:** `number` -- the unpacked numeric value.

### Precision

Packed decimal supports up to 18 digits, matching the COBOL `PIC 9(18)` maximum. For
values requiring more than 15 significant digits, pass the value as a string to
`packDecimal` to avoid JavaScript floating-point precision loss.

---

## Binary Encoding (COMP/BINARY)

COBOL offers several binary numeric storage types, all stored in big-endian (network)
byte order.

### COMP / BINARY

Fixed-size big-endian integers. The number of bytes is determined by the digit count in the
PIC clause:

| PIC Digits | Bytes | Type          | Signed Range                              |
|------------|-------|---------------|-------------------------------------------|
| 1--4       | 2     | Halfword      | -32,768 to 32,767                         |
| 5--9       | 4     | Fullword      | -2,147,483,648 to 2,147,483,647           |
| 10--18     | 8     | Doubleword    | -9,223,372,036,854,775,808 to 9.22 x 10^18 |

Signed values use two's complement representation. Unsigned values (`PIC 9(n)` without
the `S` prefix) use the full positive range of the same byte width.

### COMP-1

IEEE 754 single-precision floating point. Always 4 bytes, big-endian. Provides
approximately 7 significant decimal digits.

### COMP-2

IEEE 754 double-precision floating point. Always 8 bytes, big-endian. Provides
approximately 15 significant decimal digits.

### API

#### `encodeBinary(value, bytes, signed?)`

Encode an integer as a big-endian binary value.

```ts
import { encodeBinary } from 'cobridge/marshal/binary';

const buf = encodeBinary(1000, 2, true);
console.log(buf); // <Buffer 03 e8>
```

**Parameters:**

| Name     | Type      | Default | Description                             |
|----------|-----------|---------|-----------------------------------------|
| `value`  | `number`  | --      | The integer value to encode             |
| `bytes`  | `number`  | --      | Number of bytes: 2, 4, or 8            |
| `signed` | `boolean` | `true`  | Use signed two's complement encoding    |

**Returns:** `Buffer`

#### `decodeBinary(buffer, signed?)`

Decode a big-endian binary buffer to a number.

```ts
import { decodeBinary } from 'cobridge/marshal/binary';

const value = decodeBinary(Buffer.from([0x03, 0xE8]), true);
console.log(value); // 1000
```

**Parameters:**

| Name     | Type      | Default | Description                              |
|----------|-----------|---------|------------------------------------------|
| `buffer` | `Buffer`  | --      | Buffer containing binary data (2, 4, or 8 bytes) |
| `signed` | `boolean` | `true`  | Interpret as signed two's complement     |

**Returns:** `number`

#### `encodeComp1(value)` / `decodeComp1(buffer)`

Encode or decode a COMP-1 (single-precision float, 4 bytes).

```ts
import { encodeComp1, decodeComp1 } from 'cobridge/marshal/binary';

const buf = encodeComp1(3.14);
const val = decodeComp1(buf);
console.log(val); // 3.140000104904175 (single-precision rounding)
```

#### `encodeComp2(value)` / `decodeComp2(buffer)`

Encode or decode a COMP-2 (double-precision float, 8 bytes).

```ts
import { encodeComp2, decodeComp2 } from 'cobridge/marshal/binary';

const buf = encodeComp2(3.14159265358979);
const val = decodeComp2(buf);
console.log(val); // 3.14159265358979
```

#### `binarySize(digits)`

Calculate the number of bytes for a COMP/BINARY field given the PIC digit count.

```ts
import { binarySize } from 'cobridge/marshal/binary';

console.log(binarySize(3));  // 2
console.log(binarySize(7));  // 4
console.log(binarySize(15)); // 8
```

---

## Full Record Marshalling

The high-level marshaller in `src/marshal/converter.ts` ties all of the above together. It
walks a copybook field tree and applies the correct encoding to each leaf field.

### Core API

#### `marshalToCobol(json, copybook, options?)`

Serialise a JSON object into a fixed-length COBOL record buffer.

```ts
import { marshalToCobol, MarshalField } from 'cobridge/marshal/converter';

const copybook: MarshalField[] = [
  { name: 'CUST-NAME',    level: 5, pic: 'X(20)' },
  { name: 'CUST-BALANCE', level: 5, pic: 'S9(7)V99', usage: 'COMP-3' },
];

const buf = marshalToCobol(
  { custName: 'Jane Smith', custBalance: 15234.50 },
  copybook
);
console.log(buf.length); // 25 (20 bytes + 5 bytes packed)
```

**Parameters:**

| Name       | Type                  | Description                            |
|------------|-----------------------|----------------------------------------|
| `json`     | `Record<string, any>` | JSON object with field values          |
| `copybook` | `MarshalField[]`      | Copybook field tree (top-level fields) |
| `options`  | `MarshalOptions`      | Optional: code page, EBCDIC mode       |

**Returns:** `Buffer` -- the COBOL record.

#### `marshalFromCobol(buffer, copybook, options?)`

Deserialise a COBOL record buffer back into a JSON object.

```ts
import { marshalFromCobol, MarshalField } from 'cobridge/marshal/converter';

const copybook: MarshalField[] = [
  { name: 'CUST-NAME',    level: 5, pic: 'X(20)' },
  { name: 'CUST-BALANCE', level: 5, pic: 'S9(7)V99', usage: 'COMP-3' },
];

const json = marshalFromCobol(buf, copybook);
console.log(json);
// { custName: 'Jane Smith', custBalance: 15234.5 }
```

**Parameters:**

| Name       | Type                  | Description                              |
|------------|-----------------------|------------------------------------------|
| `buffer`   | `Buffer`              | Buffer containing the COBOL record       |
| `copybook` | `MarshalField[]`      | Copybook field tree                      |
| `options`  | `MarshalOptions`      | Optional: code page, EBCDIC mode         |

**Returns:** `Record<string, any>` -- the JSON object.

### MarshalOptions

```ts
interface MarshalOptions {
  codePage?: '037' | '500' | '1047';  // EBCDIC code page (default: '037')
  useEbcdic?: boolean;                // Encode text as EBCDIC (default: false)
}
```

When `useEbcdic` is `false` (the default), alphanumeric fields are written and read as
ASCII. Set it to `true` when exchanging buffers directly with a mainframe that expects
EBCDIC text.

### MarshalField

The `MarshalField` interface describes a single copybook field:

```ts
interface MarshalField {
  name: string;            // COBOL field name (e.g. 'CUST-NAME')
  level: number;           // Level number (01, 05, 10, etc.)
  pic?: string;            // PIC clause (e.g. 'X(10)', 'S9(7)V99')
  usage?: string;          // COMP, COMP-1, COMP-2, COMP-3, BINARY, DISPLAY
  occurs?: number;         // OCCURS count for arrays
  children?: MarshalField[];  // Child fields for group items
  size?: number;           // Pre-calculated byte size (optional override)
  signed?: boolean;        // Whether the field is signed
  redefines?: string;      // Name of the field this one REDEFINES
}
```

### How It Works

The marshaller processes fields recursively:

1. **Group items** (fields with `children`): recurse into each child, accumulating the
   offset. The group itself has no PIC clause; its byte size is the sum of its children.

2. **OCCURS** (arrays): when a field has `occurs: N`, the JSON value is expected to be an
   array of length N. The marshaller serialises each element consecutively.

3. **PIC X (alphanumeric)**: the string value is right-padded with spaces to fill the
   field width. On read, trailing spaces are stripped.

4. **PIC 9 (numeric DISPLAY)**: the numeric value is converted to a digit string and
   left-padded with zeros. Signed fields are prefixed with `+` or `-`.

5. **COMP-3**: delegated to `packDecimal` / `unpackDecimal`.

6. **COMP/BINARY**: delegated to `encodeBinary` / `decodeBinary` with scaling for implied
   decimal digits.

7. **COMP-1/COMP-2**: delegated to `encodeComp1` / `encodeComp2`.

8. **REDEFINES**: a REDEFINES field overlays the same storage as the field it redefines.
   The marshaller writes to (or reads from) the original field's byte offset without
   advancing the current position. This allows multiple interpretations of the same bytes.

### Field Name Conversion

COBOL field names use hyphens (e.g. `CUST-NAME`). The marshaller converts them to
camelCase JSON keys:

| COBOL Name        | JSON Key         |
|-------------------|------------------|
| `CUST-NAME`       | `custName`       |
| `CUST-BALANCE`    | `custBalance`    |
| `LS-RETURN-CODE`  | `lsReturnCode`   |

### Converting from Parser Output

If you are using the COBridge copybook parser, convert its AST nodes to `MarshalField`
objects using the provided bridge functions:

```ts
import { parseCopybook } from 'cobridge/parser/parser';
import { fromParserFields, marshalToCobol } from 'cobridge/marshal/converter';

const copybook = parseCopybook(copybookSource);
const fields = fromParserFields(copybook.fields);

const buf = marshalToCobol({ custName: 'Alice', custBalance: 100 }, fields);
```

### Complete Round-Trip Example

Below is a full example that defines a copybook inline, marshals JSON to a COBOL buffer,
and unmarshals it back.

```ts
import {
  marshalToCobol,
  marshalFromCobol,
  MarshalField,
} from 'cobridge/marshal/converter';

// Define a copybook for a customer record:
//   01 CUSTOMER-RECORD.
//     05 CUST-ID        PIC 9(10).
//     05 CUST-NAME      PIC X(30).
//     05 CUST-BALANCE   PIC S9(13)V99 COMP-3.
//     05 ORDER-COUNT    PIC 9(5) COMP.
//     05 ADDRESSES OCCURS 3.
//       10 ADDR-LINE    PIC X(40).
const copybook: MarshalField[] = [
  {
    name: 'CUSTOMER-RECORD',
    level: 1,
    children: [
      { name: 'CUST-ID', level: 5, pic: '9(10)' },
      { name: 'CUST-NAME', level: 5, pic: 'X(30)' },
      { name: 'CUST-BALANCE', level: 5, pic: 'S9(13)V99', usage: 'COMP-3' },
      { name: 'ORDER-COUNT', level: 5, pic: '9(5)', usage: 'COMP' },
      {
        name: 'ADDRESSES',
        level: 5,
        occurs: 3,
        children: [
          { name: 'ADDR-LINE', level: 10, pic: 'X(40)' },
        ],
      },
    ],
  },
];

// Marshal JSON -> COBOL buffer
const input = {
  customerRecord: {
    custId: 1234567890,
    custName: 'Jane Smith',
    custBalance: 15234.50,
    orderCount: 42,
    addresses: [
      { addrLine: '123 Main Street' },
      { addrLine: 'Suite 400' },
      { addrLine: 'Springfield, IL 62704' },
    ],
  },
};

const cobolBuffer = marshalToCobol(input, copybook);
console.log('Buffer length:', cobolBuffer.length);
// 10 (CUST-ID) + 30 (CUST-NAME) + 8 (COMP-3) + 4 (COMP) + 3*40 (ADDRESSES) = 172

// Unmarshal COBOL buffer -> JSON
const output = marshalFromCobol(cobolBuffer, copybook);
console.log(JSON.stringify(output, null, 2));
// {
//   "customerRecord": {
//     "custId": 1234567890,
//     "custName": "Jane Smith",
//     "custBalance": 15234.5,
//     "orderCount": 42,
//     "addresses": [
//       { "addrLine": "123 Main Street" },
//       { "addrLine": "Suite 400" },
//       { "addrLine": "Springfield, IL 62704" }
//     ]
//   }
// }
```

---

## Programmatic Usage

### Marshalling with EBCDIC

```ts
import { marshalToCobol, marshalFromCobol, MarshalField } from 'cobridge/marshal/converter';

const fields: MarshalField[] = [
  { name: 'GREETING', level: 5, pic: 'X(10)' },
];

// Write EBCDIC-encoded text (e.g. for direct mainframe communication)
const buf = marshalToCobol(
  { greeting: 'HELLO' },
  fields,
  { useEbcdic: true, codePage: '037' }
);

// Read it back
const json = marshalFromCobol(buf, fields, { useEbcdic: true, codePage: '037' });
console.log(json.greeting); // 'HELLO'
```

### Mixed Field Types

```ts
import { marshalToCobol, marshalFromCobol, MarshalField } from 'cobridge/marshal/converter';

const fields: MarshalField[] = [
  { name: 'LABEL',      level: 5, pic: 'X(8)' },
  { name: 'COUNT',      level: 5, pic: '9(5)',     usage: 'COMP' },
  { name: 'AMOUNT',     level: 5, pic: 'S9(7)V99', usage: 'COMP-3' },
  { name: 'RATE',       level: 5, pic: '',          usage: 'COMP-2' },
];

const input = { label: 'ORDER', count: 150, amount: -2345.67, rate: 0.0875 };
const buf = marshalToCobol(input, fields);
const output = marshalFromCobol(buf, fields);

console.log(output);
// { label: 'ORDER', count: 150, amount: -2345.67, rate: 0.0875 }
```

### Using REDEFINES

```ts
import { marshalToCobol, marshalFromCobol, MarshalField } from 'cobridge/marshal/converter';

// REDEFINES lets two field definitions share the same bytes.
// The original field and the REDEFINES field overlay the same storage.
const fields: MarshalField[] = [
  { name: 'DATE-NUMERIC',  level: 5, pic: '9(8)' },
  { name: 'DATE-TEXT',     level: 5, pic: 'X(8)', redefines: 'DATE-NUMERIC' },
];

// Write as numeric
const buf = marshalToCobol({ dateNumeric: 20250115 }, fields);

// Read both views
const output = marshalFromCobol(buf, fields);
console.log(output.dateNumeric); // 20250115
console.log(output.dateText);    // '20250115' (same bytes, read as text)
```
