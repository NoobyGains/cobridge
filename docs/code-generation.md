# Code Generation

Complete guide to generating OpenAPI specifications, TypeScript interfaces, and JSON Schemas from parsed COBOL copybooks.

## Overview

COBridge provides three code generators, all operating on the same parsed `Copybook` AST:

| Generator | Output | Use Case |
|-----------|--------|----------|
| `generateOpenApiSpec` | OpenAPI 3.0.3 specification | REST API definitions, Swagger UI, API gateways |
| `generateTypeScriptTypes` | TypeScript interface declarations | Type-safe client/server code |
| `generateJsonSchema` | JSON Schema draft-07 | Validation, documentation, form generation |

All three generators share the same name conversion utilities and COBOL-to-JSON type mapping logic.

---

## Name Conversion Rules

COBOL data names use UPPER-CASE with hyphens. COBridge converts these to idiomatic JavaScript/TypeScript conventions.

### camelCase (for field/property names)

The function `cobolNameToCamelCase` converts a COBOL data-name to camelCase:

```
CUST-FIRST-NAME   -> custFirstName
ACCT-NUMBER       -> acctNumber
TXN-BALANCE-AFTER -> txnBalanceAfter
CUST-ID           -> custId
```

**Rules:**
1. Convert the entire name to lower case.
2. Split on hyphens.
3. Capitalise the first letter of every part except the first.
4. Join without separators.

### PascalCase (for type/interface names)

The function `cobolNameToPascalCase` converts a COBOL data-name to PascalCase:

```
CUSTOMER-RECORD     -> CustomerRecord
ACCOUNT-RECORD      -> AccountRecord
TRANSACTION-RECORD  -> TransactionRecord
CUST-PERSONAL-INFO  -> CustPersonalInfo
```

**Rules:**
1. Convert the entire name to lower case.
2. Split on hyphens.
3. Capitalise the first letter of every part (including the first).
4. Join without separators.

### kebab-case (for URL slugs)

Used internally by the OpenAPI generator for REST paths:

```
CUSTOMER-RECORD -> customer-record
ACCOUNT-RECORD  -> account-record
```

---

## Type Mapping

All three generators use the same fundamental mapping from COBOL types to JSON/TypeScript types.

### COBOL to JSON Schema Type Mapping

| COBOL PIC/USAGE | JSON Schema Type | Format | Notes |
|-----------------|-----------------|--------|-------|
| `PIC X(n)` | `string` | — | `maxLength: n` |
| `PIC A(n)` | `string` | — | `maxLength: n` |
| `PIC 9(n)` DISPLAY | `string` | — | Pattern-constrained numeric string |
| `PIC S9(n)` DISPLAY | `string` | — | Pattern allows leading minus |
| `PIC 9(n)V9(m)` DISPLAY | `string` | — | Pattern includes decimal point |
| `PIC 9(n)` COMP/BINARY | `integer` | — | With `minimum`/`maximum` |
| `PIC S9(n)` COMP/BINARY | `integer` | — | Signed range |
| `PIC 9(n)V9(m)` COMP-3 | `number` | — | Decimal precision |
| `PIC 9(n)` COMP-3 | `integer` | — | No decimal portion |
| COMP-1 | `number` | `float` | Single-precision |
| COMP-2 | `number` | `double` | Double-precision |
| Group item | `object` | — | Properties from children |
| OCCURS field | `array` | — | Items from field schema |
| Level 88 | `boolean` | — | Condition name |

### COBOL to TypeScript Type Mapping

| COBOL PIC/USAGE | TypeScript Type | Notes |
|-----------------|----------------|-------|
| `PIC X(n)` | `string` | Alphanumeric |
| `PIC A(n)` | `string` | Alphabetic |
| `PIC 9(n)` (any USAGE) | `number` | All numerics map to `number` |
| `PIC S9(n)V9(m)` (any USAGE) | `number` | Signed decimal |
| COMP-1 | `number` | Float |
| COMP-2 | `number` | Float |
| Group item | Interface name (PascalCase) | Nested interface |
| OCCURS field | `Type[]` | Array of the element type |

---

## OpenAPI Generation

### Basic Usage

```typescript
import { parseCopybook, generateOpenApiSpec } from 'cobridge';

const source = `
       01  CUSTOMER-RECORD.
           05  CUST-ID             PIC 9(10).
           05  CUST-NAME           PIC X(30).
           05  CUST-BALANCE        PIC S9(13)V99 COMP-3.
`;

const copybook = parseCopybook(source);
const spec = generateOpenApiSpec(copybook);
```

### Options

```typescript
generateOpenApiSpec(copybook, {
  basePath: '/api/v2',    // Default: '/api'
  title: 'My Custom API', // Default: 'COBridge — {copybook.name}'
});
```

### Output Structure

The generated OpenAPI spec includes:

- **info** — title, version, and description
- **paths** — a POST endpoint at `{basePath}/{slug}` (e.g. `/api/customer-record`)
- **components/schemas** — a named schema derived from the copybook

The POST endpoint uses the schema for both the request body and the 200 response, reflecting the COBOL pattern where the same record layout is used for input and output. Error responses (400, 500) are also defined.

### How Fields Map

#### Single 01-Level Group (Common Case)

When the copybook has a single 01-level group item, the generator unwraps it so the component schema directly contains the group's child properties. This avoids an unnecessary wrapper level.

#### Group Items Become Objects

Each group item is mapped to a JSON Schema `object` with `properties` and `required` arrays:

```cobol
       05  CUST-PERSONAL-INFO.
           10  CUST-FIRST-NAME  PIC X(30).
           10  CUST-LAST-NAME   PIC X(30).
```

Generates:

```json
{
  "custPersonalInfo": {
    "type": "object",
    "properties": {
      "custFirstName": { "type": "string", "maxLength": 30 },
      "custLastName": { "type": "string", "maxLength": 30 }
    },
    "required": ["custFirstName", "custLastName"],
    "description": "COBOL group: CUST-PERSONAL-INFO (level 05, 60 bytes)"
  }
}
```

#### OCCURS Become Arrays

Fields with OCCURS clauses are wrapped in an array schema:

```cobol
       10  ACCT-SIGNER  OCCURS 5 TIMES.
           15  SIGNER-ID    PIC 9(10).
           15  SIGNER-NAME  PIC X(30).
           15  SIGNER-ROLE  PIC X(1).
```

Generates:

```json
{
  "acctSigner": {
    "type": "array",
    "maxItems": 5,
    "items": {
      "type": "object",
      "properties": {
        "signerId": { "type": "string", "pattern": "^-?\\d{1,10}$", "maxLength": 10 },
        "signerName": { "type": "string", "maxLength": 30 },
        "signerRole": { "type": "string", "maxLength": 1 }
      },
      "required": ["signerId", "signerName", "signerRole"]
    }
  }
}
```

Variable-length OCCURS include `minItems` as well:

```json
{
  "type": "array",
  "maxItems": 99,
  "minItems": 1,
  "items": { ... }
}
```

#### Level 88 Conditions

Level 88 condition names are mapped as boolean properties with a descriptive label:

```json
{ "type": "boolean", "description": "Condition: CUST-ACTIVE" }
```

#### Numeric Display Fields

Numeric fields with DISPLAY usage are represented as pattern-constrained strings, since the COBOL on-the-wire format is a text representation:

```json
{
  "type": "string",
  "pattern": "^-?\\d{1,13}\\.\\d{2}$",
  "maxLength": 17,
  "description": "COBOL: CUST-TOTAL-BALANCE PIC S9(13)V99 (DISPLAY, offset 0, 15 bytes)"
}
```

#### Computational Numeric Fields

COMP, COMP-3, BINARY, and PACKED-DECIMAL fields with no decimal portion map to `integer`; those with decimals map to `number`. COMP-1 uses format `float`, COMP-2 uses format `double`.

### Complete Example

Given this copybook:

```cobol
       01  CUSTOMER-RECORD.
           05  CUST-ID                    PIC 9(10).
           05  CUST-PERSONAL-INFO.
               10  CUST-FIRST-NAME       PIC X(30).
               10  CUST-LAST-NAME        PIC X(30).
           05  CUST-BALANCE              PIC S9(13)V99 COMP-3.
           05  CUST-STATUS               PIC X(1).
               88  CUST-ACTIVE           VALUE 'A'.
               88  CUST-INACTIVE         VALUE 'I'.
```

The generated OpenAPI spec (as YAML) would be:

```yaml
openapi: "3.0.3"
info:
  title: "COBridge — CUSTOMER-RECORD"
  version: "1.0.0"
  description: "Auto-generated API from COBOL copybook: CUSTOMER-RECORD"
paths:
  /api/customer-record:
    post:
      summary: "Call COBOL program CUSTOMER-RECORD"
      operationId: "callCustomerRecord"
      requestBody:
        required: true
        content:
          application/json:
            schema:
              $ref: "#/components/schemas/CustomerRecord"
      responses:
        "200":
          description: "Successful COBOL program execution"
          content:
            application/json:
              schema:
                $ref: "#/components/schemas/CustomerRecord"
        "400":
          description: "Invalid request body"
        "500":
          description: "COBOL program execution error"
components:
  schemas:
    CustomerRecord:
      type: object
      description: "Generated from COBOL copybook CUSTOMER-RECORD (49 bytes)"
      required:
        - custId
        - custPersonalInfo
        - custBalance
        - custStatus
      properties:
        custId:
          type: string
          pattern: "^-?\\d{1,10}$"
          maxLength: 10
          description: "COBOL: CUST-ID PIC 9(10) (DISPLAY, offset 0, 10 bytes)"
        custPersonalInfo:
          type: object
          required:
            - custFirstName
            - custLastName
          properties:
            custFirstName:
              type: string
              maxLength: 30
            custLastName:
              type: string
              maxLength: 30
          description: "COBOL group: CUST-PERSONAL-INFO (level 05, 60 bytes)"
        custBalance:
          type: number
          description: "COBOL: CUST-BALANCE PIC S9(13)V99 (COMP-3, offset 70, 8 bytes)"
        custStatus:
          type: string
          maxLength: 1
          description: "COBOL: CUST-STATUS PIC X(1) (DISPLAY, offset 78, 1 bytes)"
```

---

## TypeScript Generation

### Basic Usage

```typescript
import { parseCopybook, generateTypeScriptTypes } from 'cobridge';

const source = `
       01  CUSTOMER-RECORD.
           05  CUST-ID             PIC 9(10).
           05  CUST-NAME           PIC X(30).
           05  CUST-BALANCE        PIC S9(13)V99 COMP-3.
`;

const copybook = parseCopybook(source);
const tsCode = generateTypeScriptTypes(copybook);
console.log(tsCode);
```

### Output Features

The TypeScript generator produces:

- **File-level JSDoc** with the copybook name and total record length.
- **Nested interfaces** for group items — child group interfaces are generated before their parent so all types are defined before use.
- **JSDoc comments** on each field with the original PIC clause, USAGE type, byte offset, and byte length.
- **Array types** for OCCURS fields (e.g. `AcctSigner[]`).
- **Type aliases** for top-level elementary fields (rare, but handled).

### Interface Naming

Group items produce `export interface` declarations with PascalCase names:

| COBOL Name | TypeScript Interface |
|------------|---------------------|
| `CUSTOMER-RECORD` | `CustomerRecord` |
| `CUST-PERSONAL-INFO` | `CustPersonalInfo` |
| `ACCT-BALANCES` | `AcctBalances` |

### Field Naming

Fields within interfaces use camelCase names:

| COBOL Name | TypeScript Property |
|------------|---------------------|
| `CUST-FIRST-NAME` | `custFirstName` |
| `ACCT-CURRENT-BAL` | `acctCurrentBal` |
| `TXN-AMOUNT` | `txnAmount` |

### Type Rules

| COBOL Type | TypeScript Type |
|------------|----------------|
| `PIC X(n)` / `PIC A(n)` | `string` |
| `PIC 9(n)` (any USAGE) | `number` |
| `PIC S9(n)V9(m)` (any USAGE) | `number` |
| COMP-1 / COMP-2 (no PIC) | `number` |
| Group item | PascalCase interface name |
| Field with OCCURS | `Type[]` |

### Complete Example

Given this copybook:

```cobol
       01  ACCOUNT-RECORD.
           05  ACCT-NUMBER            PIC X(12).
           05  ACCT-TYPE              PIC X(2).
               88  ACCT-CHECKING     VALUE 'CK'.
               88  ACCT-SAVINGS      VALUE 'SV'.
           05  ACCT-OWNER-ID         PIC 9(10).
           05  ACCT-BALANCES.
               10  ACCT-CURRENT-BAL  PIC S9(13)V99 COMP-3.
               10  ACCT-AVAILABLE-BAL PIC S9(13)V99 COMP-3.
           05  ACCT-SIGNATORIES.
               10  ACCT-NUM-SIGNERS  PIC 9(1).
               10  ACCT-SIGNER       OCCURS 5 TIMES.
                   15  SIGNER-ID     PIC 9(10).
                   15  SIGNER-NAME   PIC X(30).
                   15  SIGNER-ROLE   PIC X(1).
```

The generated TypeScript output:

```typescript
/**
 * Auto-generated TypeScript types for COBOL copybook: ACCOUNT-RECORD
 * Total record length: 245 bytes
 *
 * Generated by COBridge
 */

/** COBOL group: ACCT-BALANCES (level 05, 16 bytes) */
export interface AcctBalances {
  /** PIC S9(13)V99 COMP-3 (offset 24, 8 bytes) */
  acctCurrentBal: number;
  /** PIC S9(13)V99 COMP-3 (offset 32, 8 bytes) */
  acctAvailableBal: number;
}

/** COBOL group: ACCT-SIGNER (level 10, 41 bytes) */
export interface AcctSigner {
  /** PIC 9(10) DISPLAY (offset 41, 10 bytes) */
  signerId: number;
  /** PIC X(30) DISPLAY (offset 51, 30 bytes) */
  signerName: string;
  /** PIC X(1) DISPLAY (offset 81, 1 bytes) */
  signerRole: string;
}

/** COBOL group: ACCT-SIGNATORIES (level 05, 206 bytes) */
export interface AcctSignatories {
  /** PIC 9(1) DISPLAY (offset 40, 1 bytes) */
  acctNumSigners: number;
  /** Group item: ACCT-SIGNER (41 bytes) */
  acctSigner: AcctSigner[];
}

/** COBOL group: ACCOUNT-RECORD (level 01, 245 bytes) */
export interface AccountRecord {
  /** PIC X(12) DISPLAY (offset 0, 12 bytes) */
  acctNumber: string;
  /** PIC X(2) DISPLAY (offset 12, 2 bytes) */
  acctType: string;
  /** PIC 9(10) DISPLAY (offset 14, 10 bytes) */
  acctOwnerId: number;
  /** Group item: ACCT-BALANCES (16 bytes) */
  acctBalances: AcctBalances;
  /** Group item: ACCT-SIGNATORIES (206 bytes) */
  acctSignatories: AcctSignatories;
}
```

Key points to note:

- Level 88 condition names (e.g. `ACCT-CHECKING`) are **excluded** from the interface — they are metadata about valid values, not separate fields.
- The `ACCT-SIGNER` interface is defined before `AcctSignatories` which references it.
- The `acctSigner` field in `AcctSignatories` has type `AcctSigner[]` because of the `OCCURS 5 TIMES` clause.

---

## JSON Schema Generation

### Basic Usage

```typescript
import { parseCopybook, generateJsonSchema } from 'cobridge';

const source = `
       01  TRANSACTION-RECORD.
           05  TXN-ID        PIC X(20).
           05  TXN-AMOUNT    PIC S9(11)V99 COMP-3.
           05  TXN-STATUS    PIC X(1).
`;

const copybook = parseCopybook(source);
const schema = generateJsonSchema(copybook);
console.log(JSON.stringify(schema, null, 2));
```

### Output Features

The JSON Schema generator produces draft-07 schemas with:

- **`$schema`** set to `http://json-schema.org/draft-07/schema#`
- **`title`** set to the copybook name
- **`description`** including the total record length
- **Constraints** derived from PIC clauses:
  - `maxLength` for string fields
  - `minimum` and `maximum` for computational numeric fields
  - `pattern` for DISPLAY numeric fields
  - `maxItems` and `minItems` for OCCURS arrays
  - `format` (`float`/`double`) for COMP-1/COMP-2

### Constraint Details

#### String Fields (PIC X, PIC A)

```json
{
  "type": "string",
  "maxLength": 30,
  "description": "COBOL: CUST-FIRST-NAME PIC X(30) DISPLAY"
}
```

#### Computational Integers (COMP/BINARY/COMP-3 without decimals)

The generator calculates exact bounds based on the digit count:

```cobol
       05  COUNTER  PIC 9(5) COMP-3.
```

```json
{
  "type": "integer",
  "maximum": 99999,
  "minimum": 0,
  "description": "COBOL: COUNTER PIC 9(5) COMP-3"
}
```

For signed fields:

```cobol
       05  ADJUSTMENT  PIC S9(7) COMP.
```

```json
{
  "type": "integer",
  "maximum": 9999999,
  "minimum": -9999999,
  "description": "COBOL: ADJUSTMENT PIC S9(7) COMP"
}
```

#### Computational Decimals (COMP-3 with V)

```cobol
       05  RATE  PIC 9V9(4) COMP-3.
```

```json
{
  "type": "number",
  "maximum": 9.9999,
  "minimum": 0,
  "description": "COBOL: RATE PIC 9V9(4) COMP-3"
}
```

#### Display Numeric Fields

Display numerics are string-typed with a regex pattern constraint:

```cobol
       05  ACCOUNT-ID  PIC 9(10).
```

```json
{
  "type": "string",
  "pattern": "^-?\\d{1,10}$",
  "maxLength": 10,
  "description": "COBOL: ACCOUNT-ID PIC 9(10) DISPLAY"
}
```

With decimals:

```cobol
       05  RATE  PIC S9(3)V99.
```

```json
{
  "type": "string",
  "pattern": "^-?\\d{1,3}\\.\\d{2}$",
  "maxLength": 7,
  "description": "COBOL: RATE PIC S9(3)V99 DISPLAY"
}
```

The `maxLength` accounts for the optional sign character and the actual decimal point character.

### Complete Example

Given this copybook:

```cobol
       01  TRANSACTION-RECORD.
           05  TXN-HEADER.
               10  TXN-ID              PIC X(20).
               10  TXN-DATE            PIC 9(8).
               10  TXN-TYPE            PIC X(3).
                   88  TXN-DEPOSIT     VALUE 'DEP'.
                   88  TXN-WITHDRAWAL  VALUE 'WDR'.
           05  TXN-AMOUNTS.
               10  TXN-AMOUNT          PIC S9(11)V99 COMP-3.
               10  TXN-FEE             PIC S9(7)V99 COMP-3.
           05  TXN-STATUS              PIC X(1).
               88  TXN-COMPLETED       VALUE 'C'.
               88  TXN-PENDING         VALUE 'P'.
```

The generated JSON Schema:

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "TRANSACTION-RECORD",
  "description": "JSON Schema for COBOL copybook: TRANSACTION-RECORD (44 bytes)",
  "type": "object",
  "required": ["txnHeader", "txnAmounts", "txnStatus"],
  "properties": {
    "txnHeader": {
      "type": "object",
      "description": "COBOL group: TXN-HEADER (level 05)",
      "required": ["txnId", "txnDate", "txnType"],
      "properties": {
        "txnId": {
          "type": "string",
          "maxLength": 20,
          "description": "COBOL: TXN-ID PIC X(20) DISPLAY"
        },
        "txnDate": {
          "type": "string",
          "pattern": "^-?\\d{1,8}$",
          "maxLength": 8,
          "description": "COBOL: TXN-DATE PIC 9(8) DISPLAY"
        },
        "txnType": {
          "type": "string",
          "maxLength": 3,
          "description": "COBOL: TXN-TYPE PIC X(3) DISPLAY"
        }
      }
    },
    "txnAmounts": {
      "type": "object",
      "description": "COBOL group: TXN-AMOUNTS (level 05)",
      "required": ["txnAmount", "txnFee"],
      "properties": {
        "txnAmount": {
          "type": "number",
          "maximum": 99999999999.99,
          "minimum": -99999999999.99,
          "description": "COBOL: TXN-AMOUNT PIC S9(11)V99 COMP-3"
        },
        "txnFee": {
          "type": "number",
          "maximum": 9999999.99,
          "minimum": -9999999.99,
          "description": "COBOL: TXN-FEE PIC S9(7)V99 COMP-3"
        }
      }
    },
    "txnStatus": {
      "type": "string",
      "maxLength": 1,
      "description": "COBOL: TXN-STATUS PIC X(1) DISPLAY"
    }
  }
}
```

---

## Programmatic Usage

### All Three Generators Together

```typescript
import {
  parseCopybook,
  generateOpenApiSpec,
  generateTypeScriptTypes,
  generateJsonSchema,
} from 'cobridge';
import fs from 'node:fs';

// Read and parse the copybook
const source = fs.readFileSync('copybooks/customer.cpy', 'utf-8');
const copybook = parseCopybook(source);

// Generate OpenAPI spec (returns a plain object — serialise to YAML or JSON)
const openApiSpec = generateOpenApiSpec(copybook, {
  basePath: '/api/v1',
  title: 'Customer API',
});
fs.writeFileSync('output/customer-api.json', JSON.stringify(openApiSpec, null, 2));

// Generate TypeScript interfaces (returns a string)
const tsTypes = generateTypeScriptTypes(copybook);
fs.writeFileSync('output/customer.types.ts', tsTypes);

// Generate JSON Schema (returns a plain object)
const jsonSchema = generateJsonSchema(copybook);
fs.writeFileSync('output/customer.schema.json', JSON.stringify(jsonSchema, null, 2));
```

### Using Name Conversion Utilities

The name conversion functions are exported and can be used independently:

```typescript
import { cobolNameToCamelCase, cobolNameToPascalCase } from 'cobridge';

cobolNameToCamelCase('CUST-FIRST-NAME');  // "custFirstName"
cobolNameToPascalCase('CUSTOMER-RECORD'); // "CustomerRecord"
```

### Processing Multiple Copybooks

```typescript
import { parseCopybook, generateOpenApiSpec } from 'cobridge';
import fs from 'node:fs';
import path from 'node:path';

const copybookDir = './copybooks';
const files = fs.readdirSync(copybookDir).filter(f => f.endsWith('.cpy'));

const allSchemas: Record<string, unknown> = {};

for (const file of files) {
  const source = fs.readFileSync(path.join(copybookDir, file), 'utf-8');
  const copybook = parseCopybook(source);
  const spec = generateOpenApiSpec(copybook);

  // Merge schemas from each copybook
  const schemas = (spec.components as any).schemas;
  Object.assign(allSchemas, schemas);
}

console.log(`Generated schemas for ${Object.keys(allSchemas).length} copybooks`);
```

---

## Generator Comparison

A quick reference for choosing the right generator:

| Feature | OpenAPI | TypeScript | JSON Schema |
|---------|---------|------------|-------------|
| Output format | Object (serialise to JSON/YAML) | String (`.ts` source) | Object (serialise to JSON) |
| REST paths | Yes | No | No |
| Type constraints | maxLength, pattern | No (types only) | maxLength, min/max, pattern |
| Nested objects | Yes | Yes (interfaces) | Yes |
| Array support | maxItems, minItems | `Type[]` | maxItems, minItems |
| COBOL metadata | In `description` | In JSDoc comments | In `description` |
| Numeric handling | string/integer/number | `number` | string/integer/number |
