# COBOL Copybook Parser

Complete guide to the COBridge parser — transforming COBOL copybook source text into a typed abstract syntax tree (AST).

## Overview

The COBridge parser reads raw COBOL copybook source text and produces a structured `Copybook` object. The pipeline has three stages:

1. **Lexer** (`tokenize`) — splits the source into a stream of typed tokens, handling fixed-format and free-format source, comments, and continuation lines.
2. **Parser** (`parseCopybook`) — consumes the token stream, builds a flat list of field declarations, then assembles them into a hierarchical tree using COBOL level numbers.
3. **Post-processing** — propagates USAGE clauses from groups to children, calculates byte lengths bottom-up, and computes byte offsets top-down.

**Input:** COBOL copybook source text (a string).
**Output:** A `Copybook` AST with calculated byte lengths and offsets for every field.

```typescript
import { parseCopybook } from 'cobridge';

const ast = parseCopybook(copybookSource);
// ast: Copybook { name, fields, totalLength }
```

---

## AST Types

The parser produces these core types:

### `Copybook`

The root node representing an entire parsed copybook.

```typescript
interface Copybook {
  name: string;           // Derived from the first 01-level field name
  fields: CopybookField[];// Top-level fields (typically one 01-level with children)
  totalLength: number;    // Total record length in bytes
}
```

### `CopybookField`

A single field — either a group item (has children) or an elementary item (has a PIC clause).

```typescript
interface CopybookField {
  levelNumber: number;     // 01-49, 66, 77, or 88
  name: string;            // COBOL data-name, e.g. "CUST-FIRST-NAME"
  picture?: PicInfo;       // Parsed PIC clause — undefined for group items
  usage: UsageType;        // Storage format — defaults to DISPLAY
  occurs?: OccursClause;   // OCCURS clause for repeating fields
  redefines?: string;      // Name of the field this one REDEFINES
  children: CopybookField[];// Child fields (present for group items)
  isGroup: boolean;        // True if this is a group item
  byteLength: number;      // Computed byte length
  startOffset: number;     // Computed byte offset from record start
}
```

### `PicInfo`

Parsed information from a PIC (PICTURE) clause.

```typescript
interface PicInfo {
  type: "alphanumeric" | "numeric" | "alpha";
  length: number;    // Total character/digit positions (excluding S and V)
  decimals: number;  // Decimal positions (after V)
  signed: boolean;   // Whether the field has an S indicator
  raw: string;       // Original PIC string as written
}
```

### `OccursClause`

```typescript
interface OccursClause {
  count: number;          // Fixed count, or max count for variable-length
  min?: number;           // Minimum count (OCCURS n TO m)
  dependingOn?: string;   // Field name for DEPENDING ON
}
```

### `UsageType` (enum)

```typescript
enum UsageType {
  DISPLAY        = "DISPLAY",
  COMP           = "COMP",
  COMP_1         = "COMP-1",
  COMP_2         = "COMP-2",
  COMP_3         = "COMP-3",
  BINARY         = "BINARY",
  PACKED_DECIMAL = "PACKED-DECIMAL",
}
```

---

## Supported COBOL Syntax

### Level Numbers

COBOL uses level numbers to define a hierarchy of data items. The parser supports:

| Level | Meaning | Notes |
|-------|---------|-------|
| 01 | Record-level group item | Top of the hierarchy |
| 02-49 | Group or elementary items | Hierarchy determined by relative numbering |
| 66 | RENAMES clause | Planned — not yet implemented |
| 77 | Standalone elementary item | Independent item, not part of a group |
| 88 | Condition name | Boolean conditions on the parent field |

#### How Hierarchy Is Built

The parser builds a tree from the flat list of fields using a stack-based algorithm. A field with a higher level number (e.g. 10) is nested under the most recent field with a lower level number (e.g. 05). For example:

```cobol
       01  CUSTOMER-RECORD.
           05  CUST-ID             PIC 9(10).
           05  CUST-PERSONAL-INFO.
               10  CUST-FIRST-NAME  PIC X(30).
               10  CUST-LAST-NAME   PIC X(30).
```

Produces this tree:

```
CUSTOMER-RECORD (01, group)
  +-- CUST-ID (05, elementary, PIC 9(10))
  +-- CUST-PERSONAL-INFO (05, group)
       +-- CUST-FIRST-NAME (10, elementary, PIC X(30))
       +-- CUST-LAST-NAME (10, elementary, PIC X(30))
```

Level 88 condition names are parsed but excluded from the layout tree (they do not occupy storage). Level 77 items stand alone at the top level.

---

### PIC Clauses

The PIC (PICTURE) clause defines the format and size of an elementary data item.

#### Alphanumeric — `PIC X(n)`

Each `X` represents one alphanumeric character position (1 byte in DISPLAY usage).

```cobol
       05  CUST-NAME  PIC X(30).
```

Result:

```json
{ "type": "alphanumeric", "length": 30, "decimals": 0, "signed": false, "raw": "X(30)" }
```

#### Alphabetic — `PIC A(n)`

Each `A` represents one alphabetic character position.

```cobol
       05  COUNTRY-CODE  PIC A(3).
```

Result:

```json
{ "type": "alpha", "length": 3, "decimals": 0, "signed": false, "raw": "A(3)" }
```

#### Numeric Display — `PIC 9(n)`

Each `9` represents one numeric digit position.

```cobol
       05  CUST-ID  PIC 9(10).
```

Result:

```json
{ "type": "numeric", "length": 10, "decimals": 0, "signed": false, "raw": "9(10)" }
```

#### Signed Numeric — `PIC S9(n)`

The `S` indicates a signed value. It does not consume a storage position in the PIC length count itself (the sign is embedded in the trailing digit in DISPLAY usage, or handled by the binary/packed format).

```cobol
       05  TXN-AMOUNT  PIC S9(11)V99 COMP-3.
```

Result:

```json
{ "type": "numeric", "length": 13, "decimals": 2, "signed": true, "raw": "S9(11)V99" }
```

#### Implied Decimal — `PIC 9(n)V9(n)`

The `V` marks an implied (virtual) decimal point. It does not occupy storage — the decimal position is known from the PIC clause, not from an actual character in the data.

```cobol
       05  INTEREST-RATE  PIC 9V9(4).
```

Result:

```json
{ "type": "numeric", "length": 5, "decimals": 4, "signed": false, "raw": "9V9(4)" }
```

The total `length` is the sum of integer digits and decimal digits (1 + 4 = 5). The `V` itself does not add to the length.

#### Edited Fields

The parser recognises numeric editing characters:

| Character | Meaning | Bytes |
|-----------|---------|-------|
| `Z` | Zero-suppress with spaces | 1 each |
| `*` | Zero-suppress with asterisks | 1 each |
| `+` | Floating plus sign | 1 each |
| `-` | Floating minus sign | 1 each |
| `.` | Actual decimal point | 1 |
| `,` | Comma insertion | 1 |
| `B` | Space insertion | 1 |
| `0` | Zero insertion | 1 |
| `/` | Slash insertion | 1 |
| `CR` | Credit symbol | 2 |
| `DB` | Debit symbol | 2 |

#### Shorthand Notation

COBOL allows both shorthand and expanded PIC notation. The parser expands shorthand internally before counting positions.

```cobol
      * These are equivalent:
       05  FIELD-A  PIC XXXXX.
       05  FIELD-B  PIC X(5).

      * These are equivalent:
       05  FIELD-C  PIC 999V99.
       05  FIELD-D  PIC 9(3)V9(2).
```

The expansion function converts `X(5)` to `XXXXX`, `9(3)V9(2)` to `999V99`, etc., before parsing the individual characters.

---

### USAGE Types

The USAGE clause determines how a field is stored in memory. It affects the byte length calculation.

#### DISPLAY (default)

One byte per character/digit position. This is the default when no USAGE is specified.

```cobol
       05  CUST-NAME  PIC X(30).
```

Byte length: 30 (one byte per `X` position).

#### COMP / BINARY

Binary integer storage. The byte length depends on the total digit count:

| Digits | Bytes | Range |
|--------|-------|-------|
| 1-4 | 2 | -9,999 to 9,999 |
| 5-9 | 4 | -999,999,999 to 999,999,999 |
| 10-18 | 8 | Up to 18 digits |

```cobol
       05  ORDER-COUNT  PIC 9(4) COMP.
```

Byte length: 2 (4 digits fits in a halfword).

#### COMP-1

Single-precision IEEE 754 floating-point. Always 4 bytes. No PIC clause is required.

```cobol
       05  TEMPERATURE  COMP-1.
```

Byte length: 4.

#### COMP-2

Double-precision IEEE 754 floating-point. Always 8 bytes. No PIC clause is required.

```cobol
       05  PRECISE-RATE  COMP-2.
```

Byte length: 8.

#### COMP-3 / PACKED-DECIMAL

Packed binary-coded decimal (BCD). Each byte holds two decimal digits, with the last half-byte used for the sign. The formula is:

```
byteLength = ceil((totalDigits + 1) / 2)
```

```cobol
       05  BALANCE  PIC S9(13)V99 COMP-3.
```

Total digits = 13 + 2 = 15. Byte length = ceil((15 + 1) / 2) = **8 bytes**.

The parser also recognises `PACKED-DECIMAL` as a synonym for `COMP-3`.

#### USAGE Keyword Variants

The parser accepts both short and long forms:

| Short | Long | Normalised to |
|-------|------|---------------|
| `COMP` | `COMPUTATIONAL` | `COMP` |
| `COMP-1` | `COMPUTATIONAL-1` | `COMP-1` |
| `COMP-2` | `COMPUTATIONAL-2` | `COMP-2` |
| `COMP-3` | `COMPUTATIONAL-3` | `COMP-3` |

The USAGE keyword itself is optional — these are equivalent:

```cobol
       05  FIELD-A  PIC 9(5) USAGE COMP.
       05  FIELD-B  PIC 9(5) USAGE IS COMP.
       05  FIELD-C  PIC 9(5) COMP.
```

---

### OCCURS Clause

The OCCURS clause defines repeating (array) fields.

#### Fixed-Length OCCURS

```cobol
       10  ACCT-SIGNER  OCCURS 5 TIMES.
           15  SIGNER-ID    PIC 9(10).
           15  SIGNER-NAME  PIC X(30).
           15  SIGNER-ROLE  PIC X(1).
```

AST result:

```json
{
  "name": "ACCT-SIGNER",
  "occurs": { "count": 5 },
  "isGroup": true,
  "byteLength": 41,
  "children": [...]
}
```

The total space consumed by this group is `byteLength * occurs.count` = 41 * 5 = **205 bytes**. The `TIMES` keyword is optional.

#### Variable-Length OCCURS

```cobol
       10  TXN-LINE  OCCURS 1 TO 99 DEPENDING ON TXN-LINE-COUNT.
           15  LINE-DESC  PIC X(40).
           15  LINE-AMT   PIC S9(9)V99 COMP-3.
```

AST result:

```json
{
  "name": "TXN-LINE",
  "occurs": { "count": 99, "min": 1, "dependingOn": "TXN-LINE-COUNT" },
  "isGroup": true
}
```

The `count` field holds the maximum (99), `min` holds the minimum (1), and `dependingOn` names the controlling field. For byte length calculation, the maximum count is used.

#### Nested OCCURS

COBOL supports arrays within arrays (up to seven levels per the COBOL standard). The parser handles this naturally through the recursive tree structure:

```cobol
       05  MONTHLY-DATA OCCURS 12 TIMES.
           10  DAILY-TOTAL OCCURS 31 TIMES.
               15  DAY-AMOUNT  PIC S9(9)V99 COMP-3.
```

The resulting AST nests the inner OCCURS under the outer one:

```
MONTHLY-DATA (occurs 12)
  +-- DAILY-TOTAL (occurs 31)
       +-- DAY-AMOUNT (PIC S9(9)V99 COMP-3, 7 bytes)
```

Total bytes: 7 * 31 * 12 = **2,604 bytes**.

---

### REDEFINES

The REDEFINES clause allows two or more field definitions to occupy the same physical storage location. This is COBOL's equivalent of a union type.

```cobol
       05  ACCT-ID-NUMERIC  PIC 9(10).
       05  ACCT-ID-ALPHA    REDEFINES ACCT-ID-NUMERIC  PIC X(10).
```

AST result for the redefining field:

```json
{
  "name": "ACCT-ID-ALPHA",
  "redefines": "ACCT-ID-NUMERIC",
  "picture": { "type": "alphanumeric", "length": 10, ... },
  "startOffset": 0,
  "byteLength": 10
}
```

Key points:

- The REDEFINES field has the **same startOffset** as the field it redefines.
- REDEFINES fields **do not add** to the parent group's total byte length.
- Multiple fields can redefine the same original field (chained REDEFINES).

#### Chained REDEFINES Example

```cobol
       05  DATE-FIELD         PIC 9(8).
       05  DATE-PARTS         REDEFINES DATE-FIELD.
           10  DATE-YEAR      PIC 9(4).
           10  DATE-MONTH     PIC 9(2).
           10  DATE-DAY       PIC 9(2).
       05  DATE-TEXT           REDEFINES DATE-FIELD  PIC X(8).
```

All three fields (DATE-FIELD, DATE-PARTS, DATE-TEXT) share the same 8 bytes of storage. Only the first (DATE-FIELD) contributes to the parent group's length.

---

### Group Items

A group item is a field that has children but no PIC clause. It acts as a container.

```cobol
       05  CUST-CONTACT-INFO.
           10  CUST-EMAIL   PIC X(50).
           10  CUST-PHONE   PIC X(15).
```

The parser sets `isGroup = true` for any field that has child fields. The group's `byteLength` is the sum of its children's lengths (accounting for OCCURS and REDEFINES).

#### USAGE Propagation

When a group item specifies a USAGE clause, that usage propagates to all elementary children that have the default DISPLAY usage:

```cobol
       05  PACKED-GROUP  COMP-3.
           10  FIELD-A  PIC 9(5).
           10  FIELD-B  PIC 9(7).
```

Both FIELD-A and FIELD-B inherit `COMP-3` from their parent group. FIELD-A gets `ceil((5+1)/2) = 3` bytes, and FIELD-B gets `ceil((7+1)/2) = 4` bytes.

---

### Other Features

#### FILLER Fields

FILLER is an unnamed placeholder that occupies space without being individually addressable:

```cobol
       05  FILLER  PIC X(10).
```

The parser assigns the name `"FILLER"` to these fields. If no name or FILLER keyword appears after the level number, the name defaults to `"FILLER"`.

#### VALUE Clauses

VALUE clauses are parsed and tokenised but not used for layout calculations. They are primarily relevant for level 88 condition names:

```cobol
       05  CUST-STATUS  PIC X(1).
           88  CUST-ACTIVE    VALUE 'A'.
           88  CUST-INACTIVE  VALUE 'I'.
```

The VALUE literals are consumed by the lexer to avoid confusing the parser, but they do not appear in the AST output.

#### BLANK WHEN ZERO, JUSTIFIED

These clauses are recognised and skipped during parsing. They do not affect the AST or byte length calculations.

#### Comments

In **fixed-format** source, an asterisk (`*`) or slash (`/`) in column 7 marks a comment line:

```cobol
      *================================================================*
      * This is a comment line
      *================================================================*
       01  MY-RECORD.
```

In **free-format** source, lines beginning with `*>` or `*` are treated as comments.

#### Fixed-Format vs Free-Format Detection

The lexer automatically detects whether the source is fixed-format or free-format using a heuristic: if more than 50% of non-empty lines have a valid column-7 indicator character (space, `*`, `/`, or `-`), the source is treated as fixed-format.

**Fixed-format rules:**
- Columns 1-6: sequence numbers (ignored)
- Column 7: indicator (`*`/`/` = comment, `-` = continuation, space = normal)
- Columns 8-72: source code
- Columns 73+: identification area (ignored)

**Free-format rules:**
- No column restrictions
- Comments start with `*>` or `*`

#### Continuation Lines

In fixed-format source, a hyphen (`-`) in column 7 indicates a continuation of the previous line. The lexer strips the indicator and appends the content to the previous line before tokenisation.

---

## Byte Length Calculation Reference

Complete reference for how PIC clause and USAGE combine to determine byte length.

### DISPLAY (default)

| PIC Clause | Length | Bytes | Notes |
|------------|--------|-------|-------|
| `X(10)` | 10 | 10 | 1 byte per character |
| `A(5)` | 5 | 5 | 1 byte per character |
| `9(8)` | 8 | 8 | 1 byte per digit |
| `S9(5)` | 5 | 5 | Sign embedded, no extra byte |
| `9(5)V99` | 7 | 7 | V is implied, no storage |
| `Z(6)` | 6 | 6 | Edited, 1 byte per position |
| `-(4).99` | 7 | 7 | Sign + digits + actual decimal |

### COMP / BINARY

| PIC Clause | Digits | Bytes |
|------------|--------|-------|
| `9(1)` | 1 | 2 |
| `9(4)` | 4 | 2 |
| `9(5)` | 5 | 4 |
| `9(9)` | 9 | 4 |
| `9(10)` | 10 | 8 |
| `9(18)` | 18 | 8 |
| `S9(4)` | 4 | 2 |
| `S9(9)V99` | 11 | 8 |

### COMP-3 / PACKED-DECIMAL

Formula: `ceil((totalDigits + 1) / 2)`

| PIC Clause | Digits | Bytes | Calculation |
|------------|--------|-------|-------------|
| `9(3)` | 3 | 2 | ceil(4/2) |
| `9(5)` | 5 | 3 | ceil(6/2) |
| `S9(7)V99` | 9 | 5 | ceil(10/2) |
| `S9(11)V99` | 13 | 7 | ceil(14/2) |
| `S9(13)V99` | 15 | 8 | ceil(16/2) |
| `9V9(4)` | 5 | 3 | ceil(6/2) |

### COMP-1 and COMP-2

| Usage | Bytes | Notes |
|-------|-------|-------|
| COMP-1 | 4 | Single-precision float, no PIC required |
| COMP-2 | 8 | Double-precision float, no PIC required |

### Worked Example

Given the COBOL field:

```cobol
       05  CUST-TOTAL-BALANCE  PIC S9(13)V99 COMP-3.
```

1. Expand PIC: `S9(13)V99` → `S` + 13 nines + `V` + 2 nines
2. `S` sets `signed = true`, does not count as a digit
3. Integer digits: 13, decimal digits: 2, total digits: 15
4. `V` is implied decimal — no storage
5. USAGE is COMP-3: `ceil((15 + 1) / 2) = ceil(8) = 8`

**Result:** 8 bytes, `PicInfo { type: "numeric", length: 15, decimals: 2, signed: true }`.

---

## Programmatic Usage

### Basic Parsing

```typescript
import { parseCopybook } from 'cobridge';

const source = `
       01  MY-RECORD.
           05  MY-FIELD    PIC X(10).
           05  MY-NUMBER   PIC 9(5) COMP-3.
`;

const copybook = parseCopybook(source);

console.log(copybook.name);        // "MY-RECORD"
console.log(copybook.totalLength);  // 13 (10 + 3)

const field = copybook.fields[0].children[0];
console.log(field.name);            // "MY-FIELD"
console.log(field.byteLength);      // 10
console.log(field.startOffset);     // 0
```

### Accessing Nested Fields

```typescript
import { parseCopybook } from 'cobridge';

const source = `
       01  CUSTOMER-RECORD.
           05  CUST-ID                    PIC 9(10).
           05  CUST-PERSONAL-INFO.
               10  CUST-FIRST-NAME       PIC X(30).
               10  CUST-LAST-NAME        PIC X(30).
`;

const copybook = parseCopybook(source);
const root = copybook.fields[0]; // CUSTOMER-RECORD

// Navigate the tree
const personalInfo = root.children[1]; // CUST-PERSONAL-INFO
console.log(personalInfo.isGroup);     // true
console.log(personalInfo.byteLength);  // 60

const firstName = personalInfo.children[0];
console.log(firstName.name);          // "CUST-FIRST-NAME"
console.log(firstName.startOffset);   // 10 (after CUST-ID)
console.log(firstName.picture?.type); // "alphanumeric"
```

### Using the Tokeniser Directly

```typescript
import { tokenize, TokenType } from 'cobridge';

const source = `       05  MY-FIELD  PIC X(10).`;
const tokens = tokenize(source);

for (const token of tokens) {
  if (token.type === TokenType.EOF) break;
  console.log(`${token.type}: ${token.value} (line ${token.line})`);
}
// LEVEL_NUMBER: 05 (line 1)
// DATA_NAME: MY-FIELD (line 1)
// PIC: PIC (line 1)
// PIC_STRING: X(10) (line 1)
// PERIOD: . (line 1)
```

### Lower-Level Utilities

```typescript
import { parsePicString, calculateByteLength, UsageType } from 'cobridge';

// Parse a PIC string independently
const pic = parsePicString('S9(7)V99');
console.log(pic);
// { type: "numeric", length: 9, decimals: 2, signed: true, raw: "S9(7)V99" }

// Calculate byte length for a given PIC + USAGE combination
const bytes = calculateByteLength(pic, UsageType.COMP_3);
console.log(bytes); // 5  — ceil((9+1)/2)
```

---

## Error Handling

The parser throws standard JavaScript `Error` objects when it encounters structural problems in the token stream.

### Common Errors

| Error | Cause | Fix |
|-------|-------|-----|
| `Expected PIC_STRING but got ...` | Missing PIC string after PIC/PICTURE keyword | Ensure every PIC keyword is followed by a valid picture string |
| `Expected NUMBER but got ...` | Missing count after OCCURS | Add the occurrence count, e.g. `OCCURS 5 TIMES` |
| `Expected DATA_NAME but got ...` | Missing field name after REDEFINES or DEPENDING ON | Provide the referenced field name |

### Graceful Handling

The parser is designed to be reasonably tolerant:

- **Unrecognised tokens** within a field declaration (between the level number and the terminating period) are silently skipped. This allows the parser to handle clauses it does not yet support (e.g. `BLANK WHEN ZERO`, `JUSTIFIED RIGHT`).
- **Missing periods** at the end of a declaration will cause the parser to consume tokens until the next level number or end-of-file, which may produce unexpected groupings. Always terminate each declaration with a period.
- **Level 88 fields** are parsed but excluded from the tree structure since they do not occupy storage. Their VALUE clauses are consumed but not stored in the AST.

### Tips for Debugging Parse Issues

1. Use `tokenize()` directly to inspect the token stream and verify the lexer is producing the expected tokens.
2. Check that your source uses consistent formatting — mixed fixed/free format can confuse the auto-detection heuristic.
3. Ensure every field declaration ends with a period (`.`).
4. Verify that level numbers create a valid hierarchy (e.g. a level 10 field must appear after a level 05 or lower, not directly after a level 15).
