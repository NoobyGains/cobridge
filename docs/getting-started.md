# Getting Started with COBridge

COBridge auto-generates REST APIs from COBOL copybooks. This guide walks you through installation, parsing your first copybook, generating API schemas, and starting a live server -- all in under five minutes.

## Prerequisites

### Node.js 18+

COBridge requires Node.js 18 or later. Check your installed version:

```bash
node --version
# v18.0.0 or higher
```

If you need to install or upgrade Node.js, visit [nodejs.org](https://nodejs.org) or use a version manager such as [nvm](https://github.com/nvm-sh/nvm):

```bash
nvm install 18
nvm use 18
```

### GnuCOBOL (optional)

GnuCOBOL is only required if you plan to use the bridge feature to call live COBOL programs through the REST API. It is **not** needed for parsing copybooks or generating schemas.

Check whether GnuCOBOL is installed:

```bash
cobc --version
```

Install GnuCOBOL on your platform:

- **Ubuntu / Debian:** `sudo apt install gnucobol`
- **macOS (Homebrew):** `brew install gnucobol`
- **Windows:** Use [MSYS2](https://www.msys2.org/) or [WSL](https://learn.microsoft.com/en-us/windows/wsl/)

## Installation

### Global CLI

Install COBridge globally to use the `cobridge` command anywhere:

```bash
npm install -g cobridge
```

Verify the installation:

```bash
cobridge --version
# 0.1.0
```

### As a library

Add COBridge to an existing project for programmatic use:

```bash
npm install cobridge
```

## Your First Copybook Parse (2 minutes)

### 1. Create a copybook file

Create a file called `account.cpy` with the following content:

```cobol
      *================================================================*
      * ACCOUNT RECORD - My first COBridge copybook
      *================================================================*
       01  ACCOUNT-RECORD.
           05  ACCT-ID                PIC 9(10).
           05  ACCT-HOLDER-NAME       PIC X(40).
           05  ACCT-BALANCE           PIC S9(13)V99 COMP-3.
           05  ACCT-TYPE              PIC X(1).
           05  ACCT-OPEN-DATE         PIC 9(8).
```

This defines a 5-field COBOL record layout: a numeric account ID, an alphanumeric holder name, a signed packed-decimal balance, a single-character account type, and a date stored as eight digits.

### 2. Parse the copybook

Run the `parse` command to see the Abstract Syntax Tree (AST):

```bash
cobridge parse ./account.cpy
```

You will see JSON output similar to:

```json
{
  "name": "ACCOUNT-RECORD",
  "fields": [
    {
      "levelNumber": 1,
      "name": "ACCOUNT-RECORD",
      "usage": "DISPLAY",
      "children": [
        {
          "levelNumber": 5,
          "name": "ACCT-ID",
          "picture": {
            "type": "numeric",
            "length": 10,
            "decimals": 0,
            "signed": false,
            "raw": "9(10)"
          },
          "usage": "DISPLAY",
          "children": [],
          "isGroup": false,
          "byteLength": 10,
          "startOffset": 0
        },
        {
          "levelNumber": 5,
          "name": "ACCT-HOLDER-NAME",
          "picture": {
            "type": "alphanumeric",
            "length": 40,
            "decimals": 0,
            "signed": false,
            "raw": "X(40)"
          },
          "usage": "DISPLAY",
          "children": [],
          "isGroup": false,
          "byteLength": 40,
          "startOffset": 10
        },
        {
          "levelNumber": 5,
          "name": "ACCT-BALANCE",
          "picture": {
            "type": "numeric",
            "length": 15,
            "decimals": 2,
            "signed": true,
            "raw": "S9(13)V99"
          },
          "usage": "COMP-3",
          "children": [],
          "isGroup": false,
          "byteLength": 8,
          "startOffset": 50
        },
        {
          "levelNumber": 5,
          "name": "ACCT-TYPE",
          "picture": {
            "type": "alphanumeric",
            "length": 1,
            "decimals": 0,
            "signed": false,
            "raw": "X(1)"
          },
          "usage": "DISPLAY",
          "children": [],
          "isGroup": false,
          "byteLength": 1,
          "startOffset": 58
        },
        {
          "levelNumber": 5,
          "name": "ACCT-OPEN-DATE",
          "picture": {
            "type": "numeric",
            "length": 8,
            "decimals": 0,
            "signed": false,
            "raw": "9(8)"
          },
          "usage": "DISPLAY",
          "children": [],
          "isGroup": false,
          "byteLength": 8,
          "startOffset": 59
        }
      ],
      "isGroup": true,
      "byteLength": 67,
      "startOffset": 0
    }
  ],
  "totalLength": 67
}
```

### 3. Understanding the output

Each field in the AST contains these key properties:

| Property | Description |
|---|---|
| `levelNumber` | The COBOL level number (01, 05, 10, etc.) defining the hierarchy. |
| `name` | The COBOL data-name exactly as written in the copybook. |
| `picture` | Parsed PIC clause details -- data type, length, decimal places, and sign. |
| `usage` | Storage format: `DISPLAY` (character), `COMP-3` (packed decimal), `BINARY`, etc. |
| `isGroup` | `true` for group items (containers), `false` for elementary items (leaf fields). |
| `byteLength` | The number of bytes this field occupies in the COBOL record buffer. |
| `startOffset` | The byte position where this field starts, relative to the beginning of the record. |
| `children` | Nested fields for group items. Empty array for elementary items. |

Notice how `ACCT-BALANCE` with `PIC S9(13)V99 COMP-3` occupies only 8 bytes -- packed decimal stores two digits per byte plus a sign nibble -- while the `DISPLAY` field `ACCT-HOLDER-NAME` with `PIC X(40)` takes exactly 40 bytes (one byte per character).

## Generate API Schemas (1 minute)

COBridge can generate OpenAPI specifications and TypeScript type definitions from any copybook.

### Generate both formats

```bash
cobridge generate ./account.cpy --format both
```

This creates two files in the `./generated` directory:

```
generated/
  openapi.json
  types.ts
```

### Specify a custom output directory

```bash
cobridge generate ./account.cpy --format both --output ./src/api
```

### OpenAPI output

The generated `openapi.json` is a complete OpenAPI 3.0.3 specification. It maps COBOL fields to JSON Schema types and creates a POST endpoint for calling the COBOL programme:

```json
{
  "openapi": "3.0.3",
  "info": {
    "title": "COBridge -- ACCOUNT-RECORD",
    "version": "1.0.0",
    "description": "Auto-generated API from COBOL copybook: ACCOUNT-RECORD"
  },
  "paths": {
    "/api/account-record": {
      "post": {
        "summary": "Call COBOL program ACCOUNT-RECORD",
        "operationId": "callAccountRecord",
        "requestBody": {
          "required": true,
          "content": {
            "application/json": {
              "schema": { "$ref": "#/components/schemas/AccountRecord" }
            }
          }
        },
        "responses": {
          "200": { "description": "Successful COBOL program execution" },
          "400": { "description": "Invalid request body" },
          "500": { "description": "COBOL program execution error" }
        }
      }
    }
  },
  "components": {
    "schemas": {
      "AccountRecord": {
        "type": "object",
        "properties": {
          "acctId": { "type": "string", "pattern": "^-?\\d{1,10}$", "maxLength": 10 },
          "acctHolderName": { "type": "string", "maxLength": 40 },
          "acctBalance": { "type": "number" },
          "acctType": { "type": "string", "maxLength": 1 },
          "acctOpenDate": { "type": "string", "pattern": "^-?\\d{1,8}$", "maxLength": 8 }
        },
        "required": ["acctId", "acctHolderName", "acctBalance", "acctType", "acctOpenDate"]
      }
    }
  }
}
```

Key mappings:

- **COBOL data-names** become camelCase JSON property names (`ACCT-HOLDER-NAME` becomes `acctHolderName`).
- **Alphanumeric fields** (`PIC X`) map to `string` with a `maxLength` constraint.
- **DISPLAY numeric fields** (`PIC 9`) map to `string` with a regex `pattern` to preserve leading zeroes.
- **Computational numeric fields** (`COMP-3`, `BINARY`) map to `number` or `integer`.

### TypeScript output

The generated `types.ts` file provides strongly-typed interfaces:

```typescript
/**
 * Auto-generated TypeScript types for COBOL copybook: ACCOUNT-RECORD
 * Total record length: 67 bytes
 *
 * Generated by COBridge
 */

/** COBOL group: ACCOUNT-RECORD (level 1, 67 bytes) */
export interface AccountRecord {
  /** PIC 9(10) DISPLAY (offset 0, 10 bytes) */
  acctId: number;
  /** PIC X(40) DISPLAY (offset 10, 40 bytes) */
  acctHolderName: string;
  /** PIC S9(13)V99 COMP-3 (offset 50, 8 bytes) */
  acctBalance: number;
  /** PIC X(1) DISPLAY (offset 58, 1 bytes) */
  acctType: string;
  /** PIC 9(8) DISPLAY (offset 59, 8 bytes) */
  acctOpenDate: number;
}
```

You can import these types directly into your TypeScript application for type-safe COBOL data interchange.

### Generate only one format

```bash
# OpenAPI only
cobridge generate ./account.cpy --format openapi

# TypeScript only
cobridge generate ./account.cpy --format typescript
```

## Start the REST API Server (1 minute)

The `serve` command starts an Express HTTP server that auto-generates REST endpoints from every `.cpy` file in a directory.

### 1. Set up a project directory

```bash
mkdir my-cobol-api
mkdir my-cobol-api/copybooks
```

Copy your copybook files into the `copybooks` directory. For this example, copy the `account.cpy` file you created earlier:

```bash
cp account.cpy my-cobol-api/copybooks/
```

### 2. Start the server

```bash
cobridge serve ./my-cobol-api/copybooks --port 3000
```

You will see output like:

```
  COBridge server running
  Local:   http://localhost:3000
  Docs:    http://localhost:3000/docs
  API:     http://localhost:3000/api

  Copybooks: ./my-cobol-api/copybooks
```

### 3. Explore the API

**List all available endpoints:**

```bash
curl http://localhost:3000/api
```

```json
{
  "copybooks": [
    {
      "name": "ACCOUNT-RECORD",
      "slug": "account",
      "endpoints": {
        "call": "POST /api/account",
        "schema": "GET /api/account/schema",
        "types": "GET /api/account/types"
      }
    }
  ]
}
```

**Call a COBOL programme endpoint:**

```bash
curl -X POST http://localhost:3000/api/account \
  -H "Content-Type: application/json" \
  -d '{
    "acctId": "0000001234",
    "acctHolderName": "Jane Smith",
    "acctBalance": 15042.50,
    "acctType": "C",
    "acctOpenDate": "20230115"
  }'
```

**Retrieve the OpenAPI schema for a copybook:**

```bash
curl http://localhost:3000/api/account/schema
```

**Retrieve TypeScript types for a copybook:**

```bash
curl http://localhost:3000/api/account/types
```

**Browse the Swagger UI documentation:**

Open [http://localhost:3000/docs](http://localhost:3000/docs) in your browser to interact with the auto-generated API documentation.

### Auto-routing

The server automatically creates endpoints for every `.cpy` file in the directory. Each copybook file `<name>.cpy` generates three routes:

| Route | Method | Description |
|---|---|---|
| `/api/<name>` | POST | Call the COBOL programme with a JSON request body |
| `/api/<name>/schema` | GET | Return the OpenAPI specification for this copybook |
| `/api/<name>/types` | GET | Return the generated TypeScript type definitions |

Add or remove `.cpy` files and restart the server to update the available endpoints.

## Using COBridge as a Library

COBridge can be imported directly into your TypeScript or JavaScript application for programmatic use.

### Parse a copybook from a string

```typescript
import { parseCopybook } from 'cobridge';
import { readFileSync } from 'node:fs';

// Parse from a file
const source = readFileSync('./copybooks/account.cpy', 'utf-8');
const copybook = parseCopybook(source);

console.log(`Copybook: ${copybook.name}`);
console.log(`Total length: ${copybook.totalLength} bytes`);
console.log(`Fields: ${copybook.fields[0].children.length}`);
```

### Generate schemas programmatically

```typescript
import { parseCopybook } from 'cobridge';
import { generateOpenApiSpec, generateTypeScriptTypes } from 'cobridge';
import { readFileSync, writeFileSync } from 'node:fs';

const source = readFileSync('./copybooks/customer.cpy', 'utf-8');
const copybook = parseCopybook(source);

// Generate an OpenAPI 3.0.3 specification
const spec = generateOpenApiSpec(copybook);
writeFileSync('./openapi.json', JSON.stringify(spec, null, 2));

// Generate TypeScript interfaces
const types = generateTypeScriptTypes(copybook);
writeFileSync('./types.ts', types);

console.log('Schema generation complete.');
```

### Generate JSON Schema

```typescript
import { parseCopybook } from 'cobridge';
import { generateJsonSchema } from 'cobridge';

const source = `
       01  ORDER-RECORD.
           05  ORDER-ID       PIC 9(8).
           05  CUSTOMER-ID    PIC 9(10).
           05  ORDER-TOTAL    PIC S9(9)V99 COMP-3.
           05  ORDER-DATE     PIC 9(8).
`;

const copybook = parseCopybook(source);
const schema = generateJsonSchema(copybook);

console.log(JSON.stringify(schema, null, 2));
```

### Create a server programmatically

```typescript
import { createApp } from 'cobridge';

const app = createApp({
  copybookDir: './copybooks',
  port: 4000,
});

app.listen(4000, () => {
  console.log('COBridge API running on http://localhost:4000');
});
```

### Work with data marshalling

```typescript
import { parseCopybook } from 'cobridge';
import { marshalToCobol, marshalFromCobol, fromParserFields } from 'cobridge';

const source = readFileSync('./copybooks/account.cpy', 'utf-8');
const copybook = parseCopybook(source);

// Convert parser fields to marshal fields
const fields = fromParserFields(copybook.fields);

// Marshal a JSON object to a COBOL byte buffer
const jsonData = {
  acctId: '0000001234',
  acctHolderName: 'Jane Smith',
  acctBalance: 15042.50,
  acctType: 'C',
  acctOpenDate: '20230115',
};
const buffer = marshalToCobol(jsonData, fields);

// Marshal a COBOL byte buffer back to JSON
const result = marshalFromCobol(buffer, fields);
console.log(result);
```

## Next Steps

Now that you have COBridge running, explore the rest of the documentation:

- **[CLI Reference](./cli-reference.md)** -- Complete reference for all CLI commands and options.
- **[Parser Guide](./parser-guide.md)** -- Deep dive into how COBOL copybooks are parsed, with details on supported PIC clauses and USAGE types.
- **[Codegen Guide](./codegen-guide.md)** -- Customising OpenAPI, TypeScript, and JSON Schema generation.
- **[Marshalling Guide](./marshalling-guide.md)** -- Converting between JSON and COBOL binary record buffers, including EBCDIC, packed decimal, and binary encoding.
- **[Bridge Guide](./bridge-guide.md)** -- Connecting to live GnuCOBOL programmes through the REST API.
- **[Server Guide](./server-guide.md)** -- Configuring the HTTP server, middleware, CORS, and custom routes.
