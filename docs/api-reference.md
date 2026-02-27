# API Reference

Complete programmatic API reference for using COBridge as a Node.js library.

---

## Installation

```bash
npm install cobridge
```

COBridge requires Node.js 18 or later.

```ts
import {
  parseCopybook,
  generateOpenApiSpec,
  generateTypeScriptTypes,
  generateJsonSchema,
  marshalToCobol,
  marshalFromCobol,
  detectGnuCOBOL,
  compileCobol,
  callCobolProgram,
  CobridgeProcess,
  startServer,
  createApp,
} from "cobridge";
```

---

## Parser API

### `parseCopybook(source: string): Copybook`

Parse a COBOL copybook source string into a typed AST. This is the primary entry point for the parser.

**Parameters**

| Name     | Type     | Description                                    |
|----------|----------|------------------------------------------------|
| `source` | `string` | The raw COBOL copybook source text.            |

**Returns** a `Copybook` object (see [Types](#copybook) below).

**Throws** if the copybook contains syntax errors that cannot be recovered from.

**Example**

```ts
import { parseCopybook } from "cobridge";

const source = `
       01  CUSTOMER-RECORD.
           05  CUST-ID         PIC 9(8).
           05  CUST-NAME       PIC X(30).
           05  CUST-BALANCE    PIC S9(7)V99 COMP-3.
`;

const copybook = parseCopybook(source);

console.log(copybook.name);         // "CUSTOMER-RECORD"
console.log(copybook.totalLength);   // total record length in bytes
console.log(copybook.fields.length); // 1 (the 01-level group)
```

---

### `parsePicString(raw: string): PicInfo`

Parse a COBOL PIC (PICTURE) string into its component parts.

**Parameters**

| Name  | Type     | Description                                     |
|-------|----------|-------------------------------------------------|
| `raw` | `string` | The PIC clause string, e.g. `"S9(7)V99"`.      |

**Returns** a `PicInfo` object.

**Example**

```ts
import { parsePicString } from "cobridge";

const pic = parsePicString("S9(7)V99");

console.log(pic.type);     // "numeric"
console.log(pic.length);   // 9 (7 integer + 2 decimal digits)
console.log(pic.decimals); // 2
console.log(pic.signed);   // true
console.log(pic.raw);      // "S9(7)V99"
```

---

### `calculateByteLength(pic: PicInfo, usage: UsageType): number`

Calculate the byte length of a field given its PIC info and USAGE type.

**Parameters**

| Name    | Type        | Description                              |
|---------|-------------|------------------------------------------|
| `pic`   | `PicInfo`   | The parsed PIC information.              |
| `usage` | `UsageType` | The COBOL USAGE clause value.            |

**Returns** the number of bytes the field occupies.

**Example**

```ts
import { parsePicString, calculateByteLength, UsageType } from "cobridge";

const pic = parsePicString("S9(7)V99");

console.log(calculateByteLength(pic, UsageType.DISPLAY));  // 9 bytes
console.log(calculateByteLength(pic, UsageType.COMP_3));   // 5 bytes
console.log(calculateByteLength(pic, UsageType.BINARY));   // 4 bytes
```

---

### `tokenize(source: string): Token[]`

Low-level tokeniser that converts raw copybook source text into a stream of typed tokens. Most users will use `parseCopybook()` instead; `tokenize()` is useful for building custom tooling.

**Parameters**

| Name     | Type     | Description                        |
|----------|----------|------------------------------------|
| `source` | `string` | The raw COBOL copybook source text.|

**Returns** an array of `Token` objects.

**Example**

```ts
import { tokenize } from "cobridge";

const tokens = tokenize("01 CUSTOMER-RECORD.");

for (const token of tokens) {
  console.log(`${token.type}: ${token.value} (line ${token.line}, col ${token.col})`);
}
```

---

### Types

#### `Copybook`

Root node representing an entire parsed copybook.

```ts
interface Copybook {
  /** Copybook name -- derived from the 01-level name or file */
  name: string;
  /** Top-level fields (typically one 01-level with nested children) */
  fields: CopybookField[];
  /** Total record length in bytes */
  totalLength: number;
}
```

#### `CopybookField`

A single field (elementary or group item) in the copybook.

```ts
interface CopybookField {
  /** COBOL level number: 01-49, 66, 77, 88 */
  levelNumber: number;
  /** Field name (COBOL data-name), e.g. WS-ACCOUNT-NUMBER */
  name: string;
  /** Parsed PIC clause info -- undefined for group items */
  picture?: PicInfo;
  /** Storage format -- defaults to DISPLAY */
  usage: UsageType;
  /** OCCURS clause for repeating fields */
  occurs?: OccursClause;
  /** Name of the field this one REDEFINES (overlapping storage) */
  redefines?: string;
  /** Child fields -- present for group items */
  children: CopybookField[];
  /** True if this is a group item (has children, no PIC clause) */
  isGroup: boolean;
  /** Computed byte length of this field (including children/occurs) */
  byteLength: number;
  /** Computed byte offset from start of the record */
  startOffset: number;
}
```

#### `PicInfo`

Parsed PIC (PICTURE) clause information.

```ts
interface PicInfo {
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
```

#### `UsageType`

COBOL USAGE clause values that determine storage format.

```ts
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

#### `OccursClause`

OCCURS clause for fixed or variable-length arrays.

```ts
interface OccursClause {
  /** Fixed count, or maximum count when DEPENDING ON is used */
  count: number;
  /** Minimum count for variable-length (OCCURS n TO m DEPENDING ON) */
  min?: number;
  /** Field name referenced by DEPENDING ON */
  dependingOn?: string;
}
```

#### `Token`

A single lexer token.

```ts
interface Token {
  type: TokenType;
  value: string;
  line: number;
  col: number;
}
```

#### `TokenType`

Enumeration of all token types produced by the lexer.

```ts
enum TokenType {
  LEVEL_NUMBER  = "LEVEL_NUMBER",
  DATA_NAME     = "DATA_NAME",
  PIC           = "PIC",
  PIC_STRING    = "PIC_STRING",
  USAGE         = "USAGE",
  USAGE_TYPE    = "USAGE_TYPE",
  OCCURS        = "OCCURS",
  TIMES         = "TIMES",
  TO            = "TO",
  DEPENDING     = "DEPENDING",
  ON            = "ON",
  REDEFINES     = "REDEFINES",
  VALUE         = "VALUE",
  VALUE_LITERAL = "VALUE_LITERAL",
  IS            = "IS",
  PERIOD        = "PERIOD",
  FILLER        = "FILLER",
  NUMBER        = "NUMBER",
  EOF           = "EOF",
}
```

---

## Code Generation API

### `generateOpenApiSpec(copybook: Copybook, options?: OpenApiOptions): object`

Generate a complete OpenAPI 3.0.3 specification from a parsed copybook. The specification includes component schemas derived from the copybook fields and REST API paths for interacting with COBOL programs.

**Parameters**

| Name       | Type              | Description                              |
|------------|-------------------|------------------------------------------|
| `copybook` | `Copybook`        | The parsed copybook AST.                 |
| `options`  | `OpenApiOptions`  | Optional generation settings.            |

**`OpenApiOptions`**

| Property   | Type     | Default               | Description                              |
|------------|----------|-----------------------|------------------------------------------|
| `basePath` | `string` | `"/api"`              | Base path prefix for generated endpoints.|
| `title`    | `string` | `"COBridge -- {name}"` | Title for the OpenAPI info block.        |

**Returns** an OpenAPI 3.0.3 specification object with `info`, `paths`, and `components.schemas`.

**Example**

```ts
import { parseCopybook, generateOpenApiSpec } from "cobridge";

const copybook = parseCopybook(source);
const spec = generateOpenApiSpec(copybook, { basePath: "/v2/api" });

// Write to file
import * as fs from "fs";
fs.writeFileSync("openapi.json", JSON.stringify(spec, null, 2));
```

---

### `generateTypeScriptTypes(copybook: Copybook): string`

Generate TypeScript interface declarations from a parsed copybook. Group items become `export interface` declarations; elementary items are mapped to TypeScript primitive types. COBOL names are converted to PascalCase for interfaces and camelCase for properties.

**Parameters**

| Name       | Type       | Description                    |
|------------|------------|--------------------------------|
| `copybook` | `Copybook` | The parsed copybook AST.      |

**Returns** a string containing TypeScript source code with interface declarations.

**Example**

```ts
import { parseCopybook, generateTypeScriptTypes } from "cobridge";

const copybook = parseCopybook(source);
const types = generateTypeScriptTypes(copybook);

console.log(types);
// /**
//  * Auto-generated TypeScript types for COBOL copybook: CUSTOMER-RECORD
//  * Total record length: 42 bytes
//  *
//  * Generated by COBridge
//  */
//
// /** COBOL group: CUSTOMER-RECORD (level 01, 42 bytes) */
// export interface CustomerRecord {
//   /** PIC 9(8) DISPLAY (offset 0, 8 bytes) */
//   custId: number;
//   /** PIC X(30) DISPLAY (offset 8, 30 bytes) */
//   custName: string;
//   /** PIC S9(7)V99 COMP-3 (offset 38, 5 bytes) */
//   custBalance: number;
// }
```

---

### `generateJsonSchema(copybook: Copybook): object`

Generate a JSON Schema (draft-07) from a parsed copybook. Includes field constraints derived from PIC clauses such as `maxLength`, `minimum`, `maximum`, and `pattern`.

**Parameters**

| Name       | Type       | Description                    |
|------------|------------|--------------------------------|
| `copybook` | `Copybook` | The parsed copybook AST.      |

**Returns** a JSON Schema (draft-07) object.

**Example**

```ts
import { parseCopybook, generateJsonSchema } from "cobridge";

const copybook = parseCopybook(source);
const schema = generateJsonSchema(copybook);

console.log(schema.$schema); // "http://json-schema.org/draft-07/schema#"
console.log(schema.title);   // "CUSTOMER-RECORD"
```

---

### Utility Functions

#### `cobolNameToCamelCase(name: string): string`

Convert a COBOL data-name to camelCase.

```ts
cobolNameToCamelCase("CUST-FIRST-NAME"); // "custFirstName"
```

#### `cobolNameToPascalCase(name: string): string`

Convert a COBOL data-name to PascalCase.

```ts
cobolNameToPascalCase("CUSTOMER-RECORD"); // "CustomerRecord"
```

---

## Marshalling API

### `marshalToCobol(json: object, fields: MarshalField[], options?: MarshalOptions): Buffer`

Convert a JSON object to a COBOL-format binary buffer using a copybook field definition. Handles group items, OCCURS arrays, alphanumeric (PIC X), numeric (PIC 9), packed decimal (COMP-3), and binary (COMP/BINARY) fields.

**Parameters**

| Name      | Type              | Description                                             |
|-----------|-------------------|---------------------------------------------------------|
| `json`    | `object`          | The JSON object with field values.                      |
| `fields`  | `MarshalField[]`  | The copybook field tree (top-level fields).             |
| `options` | `MarshalOptions`  | Optional marshalling settings (code page, EBCDIC mode). |

**Returns** a `Buffer` containing the COBOL record.

**Example**

```ts
import { parseCopybook, marshalToCobol, fromParserFields } from "cobridge";

const copybook = parseCopybook(source);
const fields = fromParserFields(copybook.fields);

const buffer = marshalToCobol(
  { custId: 12345678, custName: "SMITH", custBalance: 1234.56 },
  fields
);

console.log(buffer.length); // total record length in bytes
```

---

### `marshalFromCobol(buffer: Buffer, fields: MarshalField[], options?: MarshalOptions): object`

Convert a COBOL-format binary buffer to a JSON object using a copybook field definition.

**Parameters**

| Name      | Type              | Description                                             |
|-----------|-------------------|---------------------------------------------------------|
| `buffer`  | `Buffer`          | Buffer containing the COBOL record.                     |
| `fields`  | `MarshalField[]`  | The copybook field tree.                                |
| `options` | `MarshalOptions`  | Optional marshalling settings.                          |

**Returns** a plain object with field values extracted from the buffer.

**Example**

```ts
import { parseCopybook, marshalFromCobol, fromParserFields } from "cobridge";

const copybook = parseCopybook(source);
const fields = fromParserFields(copybook.fields);

const json = marshalFromCobol(buffer, fields);

console.log(json.custId);      // 12345678
console.log(json.custName);    // "SMITH"
console.log(json.custBalance); // 1234.56
```

---

### `fromParserField(field: CopybookField): MarshalField`

Convert a parser `CopybookField` (rich AST) to a `MarshalField` suitable for the marshaller. This bridges the parser output to the marshaller input.

**Parameters**

| Name    | Type             | Description                     |
|---------|------------------|---------------------------------|
| `field` | `CopybookField`  | A field from the parser AST.    |

**Returns** a `MarshalField`.

---

### `fromParserFields(fields: CopybookField[]): MarshalField[]`

Convert an array of parser fields to `MarshalField[]`.

**Parameters**

| Name     | Type               | Description                            |
|----------|--------------------|----------------------------------------|
| `fields` | `CopybookField[]`  | Array of fields from the parser AST.   |

**Returns** an array of `MarshalField` objects.

---

### `ebcdicToAscii(buffer: Buffer, codePage?: CodePage): string`

Convert an EBCDIC-encoded buffer to an ASCII string.

**Parameters**

| Name       | Type       | Default  | Description                                      |
|------------|------------|----------|--------------------------------------------------|
| `buffer`   | `Buffer`   | --       | Buffer containing EBCDIC-encoded data.           |
| `codePage` | `CodePage` | `"037"`  | EBCDIC code page to use for translation.         |

**Returns** the decoded ASCII string.

**Throws** `Error` if the code page is not supported.

**Example**

```ts
import { ebcdicToAscii, asciiToEbcdic } from "cobridge";

const ebcdicBuffer = asciiToEbcdic("HELLO WORLD");
const text = ebcdicToAscii(ebcdicBuffer);

console.log(text); // "HELLO WORLD"
```

---

### `asciiToEbcdic(str: string, codePage?: CodePage): Buffer`

Convert an ASCII string to an EBCDIC-encoded buffer.

**Parameters**

| Name       | Type       | Default  | Description                                      |
|------------|------------|----------|--------------------------------------------------|
| `str`      | `string`   | --       | ASCII string to convert.                         |
| `codePage` | `CodePage` | `"037"`  | EBCDIC code page to use for translation.         |

**Returns** a `Buffer` containing EBCDIC-encoded data.

**Throws** `Error` if the code page is not supported.

**Supported Code Pages**

| Code Page | Description                  |
|-----------|------------------------------|
| `"037"`   | US/Canada (default)          |
| `"500"`   | International                |
| `"1047"`  | Latin 1 / Open Systems       |

---

### `packDecimal(value: number | string, totalDigits: number, decimalDigits?: number): Buffer`

Pack a numeric value into COMP-3 packed decimal format.

**Parameters**

| Name            | Type               | Default | Description                                     |
|-----------------|--------------------|---------|-------------------------------------------------|
| `value`         | `number \| string` | --      | The numeric value to pack.                      |
| `totalDigits`   | `number`           | --      | Total number of digits (1--18).                 |
| `decimalDigits` | `number`           | `0`     | Number of digits after the implied decimal.     |

**Returns** a `Buffer` containing the packed decimal.

**Throws** `Error` if `totalDigits` is outside 1--18 or the value exceeds capacity.

**Example**

```ts
import { packDecimal, unpackDecimal } from "cobridge";

const packed = packDecimal(12345.67, 7, 2);
console.log(packed); // <Buffer 01 23 45 67 0c>

const value = unpackDecimal(packed, 2);
console.log(value); // 12345.67
```

---

### `unpackDecimal(buffer: Buffer, decimalDigits?: number): number`

Unpack a COMP-3 packed decimal buffer to a number.

**Parameters**

| Name            | Type     | Default | Description                                     |
|-----------------|----------|---------|-------------------------------------------------|
| `buffer`        | `Buffer` | --      | Buffer containing packed decimal data.          |
| `decimalDigits` | `number` | `0`     | Number of implied decimal digits.               |

**Returns** the unpacked numeric value.

**Throws** `Error` if the buffer is empty.

---

### `encodeBinary(value: number, bytes: number, signed?: boolean): Buffer`

Encode a numeric value as a big-endian binary integer (COMP/BINARY).

**Parameters**

| Name     | Type      | Default | Description                                              |
|----------|-----------|---------|----------------------------------------------------------|
| `value`  | `number`  | --      | The integer value to encode.                             |
| `bytes`  | `number`  | --      | Number of bytes: 2 (halfword), 4 (fullword), or 8 (doubleword). |
| `signed` | `boolean` | `true`  | Whether to use signed (two's complement) encoding.       |

**Returns** a `Buffer` containing the encoded value.

**Throws** `Error` if `bytes` is not 2, 4, or 8.

---

### `decodeBinary(buffer: Buffer, signed?: boolean): number`

Decode a big-endian binary buffer to a number.

**Parameters**

| Name     | Type      | Default | Description                                              |
|----------|-----------|---------|----------------------------------------------------------|
| `buffer` | `Buffer`  | --      | Buffer containing the binary data.                       |
| `signed` | `boolean` | `true`  | Whether to interpret as signed (two's complement).       |

**Returns** the decoded numeric value.

**Throws** `Error` if the buffer length is not 2, 4, or 8.

---

### `encodeComp1(value: number): Buffer`

Encode a number as COMP-1 (single-precision IEEE 754 float, 4 bytes, big-endian).

**Parameters**

| Name    | Type     | Description              |
|---------|----------|--------------------------|
| `value` | `number` | The float value to encode.|

**Returns** a 4-byte `Buffer`.

---

### `decodeComp1(buffer: Buffer): number`

Decode a COMP-1 (single-precision float) buffer.

**Parameters**

| Name     | Type     | Description          |
|----------|----------|----------------------|
| `buffer` | `Buffer` | 4-byte buffer.       |

**Returns** the decoded float value.

**Throws** `Error` if the buffer is not exactly 4 bytes.

---

### `encodeComp2(value: number): Buffer`

Encode a number as COMP-2 (double-precision IEEE 754 float, 8 bytes, big-endian).

**Parameters**

| Name    | Type     | Description               |
|---------|----------|---------------------------|
| `value` | `number` | The double value to encode.|

**Returns** an 8-byte `Buffer`.

---

### `decodeComp2(buffer: Buffer): number`

Decode a COMP-2 (double-precision float) buffer.

**Parameters**

| Name     | Type     | Description          |
|----------|----------|----------------------|
| `buffer` | `Buffer` | 8-byte buffer.       |

**Returns** the decoded double value.

**Throws** `Error` if the buffer is not exactly 8 bytes.

---

### `binarySize(digits: number): number`

Determine the number of bytes needed for a COMP/BINARY field based on the digit count in the PIC clause.

| Digits   | Bytes | COBOL Term  |
|----------|-------|-------------|
| 1--4     | 2     | Halfword    |
| 5--9     | 4     | Fullword    |
| 10--18   | 8     | Doubleword  |

**Throws** `Error` if digits exceeds 18.

---

### Types

#### `MarshalField`

Simplified field definition for direct use with the marshaller.

```ts
interface MarshalField {
  name: string;
  level: number;
  pic?: string;          // PIC clause, e.g. "X(10)", "9(5)V9(2)", "S9(7)"
  usage?: string;        // COMP, COMP-1, COMP-2, COMP-3, BINARY, DISPLAY
  occurs?: number;       // OCCURS count for arrays
  children?: MarshalField[];
  size?: number;         // Pre-calculated byte size (optional)
  signed?: boolean;      // Whether the field is signed (S prefix in PIC)
  redefines?: string;    // Name of the field this one REDEFINES
}
```

#### `MarshalOptions`

Options for marshalling operations.

```ts
interface MarshalOptions {
  codePage?: CodePage;   // EBCDIC code page (default: "037")
  useEbcdic?: boolean;   // Encode text as EBCDIC (default: false, uses ASCII)
}
```

#### `CodePage`

Supported EBCDIC code pages.

```ts
type CodePage = "037" | "500" | "1047";
```

---

## Bridge API

### `detectGnuCOBOL(): Promise<GnuCOBOLInfo>`

Detect whether GnuCOBOL is installed on the system and return version information.

**Returns** a `GnuCOBOLInfo` object.

**Example**

```ts
import { detectGnuCOBOL } from "cobridge";

const info = await detectGnuCOBOL();

if (info.installed) {
  console.log(`GnuCOBOL ${info.version} found at ${info.compilerPath}`);
} else {
  console.log("GnuCOBOL is not installed");
}
```

---

### `compileCobol(sourcePath: string, outputPath?: string, options?: CompileOptions): Promise<string>`

Compile a COBOL source file to a shared library or executable using GnuCOBOL's `cobc` compiler. By default, compiles as a module (`-m`) producing a `.so` (Linux/macOS) or `.dll` (Windows) file.

**Parameters**

| Name         | Type             | Description                                    |
|--------------|------------------|------------------------------------------------|
| `sourcePath` | `string`         | Path to the `.cbl`/`.cob` source file.         |
| `outputPath` | `string`         | Optional output path for the compiled module.  |
| `options`    | `CompileOptions` | Compilation options.                           |

**Returns** the path to the compiled shared library.

**Throws** `Error` if the source file is not found or compilation fails.

**Example**

```ts
import { compileCobol } from "cobridge";

const libPath = await compileCobol("./programs/CUSTINQ.cbl");
console.log(`Compiled to: ${libPath}`); // "./programs/CUSTINQ.so"

// Compile as an executable instead
const exePath = await compileCobol("./programs/CUSTINQ.cbl", undefined, {
  asModule: false,
});
```

---

### `callCobolProgram(libraryPath: string, programName: string, linkageData: Buffer, options?: CallOptions): Promise<Buffer>`

Call a compiled COBOL program using `cobcrun`. The linkage data buffer is passed via stdin and the program's output (modified linkage data) is read from stdout.

**Parameters**

| Name          | Type          | Description                                        |
|---------------|---------------|----------------------------------------------------|
| `libraryPath` | `string`      | Path to the compiled shared library / module.      |
| `programName` | `string`      | Name of the COBOL program to call.                 |
| `linkageData` | `Buffer`      | Buffer containing the linkage section data.        |
| `options`     | `CallOptions` | Call options (timeout, env, cwd).                  |

**Returns** a `Buffer` containing the modified linkage section data.

**Throws** `Error` if the library is not found or the program fails.

**Example**

```ts
import { compileCobol, callCobolProgram, marshalToCobol, marshalFromCobol, parseCopybook, fromParserFields } from "cobridge";

// Compile the program
const libPath = await compileCobol("./programs/CUSTINQ.cbl");

// Prepare input data
const copybook = parseCopybook(copybookSource);
const fields = fromParserFields(copybook.fields);
const inputBuffer = marshalToCobol({ custId: 12345678 }, fields);

// Call the COBOL program
const outputBuffer = await callCobolProgram(libPath, "CUSTINQ", inputBuffer);

// Parse the response
const result = marshalFromCobol(outputBuffer, fields);
console.log(result.custName); // "SMITH"
```

---

### `CobridgeProcess`

A persistent bridge process manager for calling COBOL programs. Avoids the overhead of spawning a new process for each call. Supports request queuing, health checking, automatic restart, and concurrent request management.

#### Constructor

```ts
new CobridgeProcess(config: CobridgeProcessConfig)
```

**`CobridgeProcessConfig`**

| Property              | Type                     | Default   | Description                                      |
|-----------------------|--------------------------|-----------|--------------------------------------------------|
| `libraryPath`         | `string`                 | --        | Directory containing compiled COBOL modules.     |
| `maxConcurrent`       | `number`                 | `1`       | Maximum concurrent requests.                     |
| `healthCheckInterval` | `number`                 | `30000`   | Health check interval in milliseconds.           |
| `requestTimeout`      | `number`                 | `30000`   | Per-request timeout in milliseconds.             |
| `autoRestart`         | `boolean`                | `true`    | Automatically restart on crash.                  |
| `maxRestarts`         | `number`                 | `3`       | Maximum restart attempts before giving up.       |
| `env`                 | `Record<string, string>` | `{}`      | Additional environment variables.                |

#### Methods

##### `start(): Promise<void>`

Start the bridge process. If the process is already running, this is a no-op.

##### `stop(): Promise<void>`

Stop the bridge process gracefully. Sends `SIGTERM` first, then `SIGKILL` after 5 seconds if the process has not exited. Rejects all pending requests.

##### `restart(): Promise<void>`

Stop and then start the bridge process.

##### `call(programName: string, data: Buffer): Promise<Buffer>`

Queue a COBOL program call. Returns a promise that resolves with the output buffer.

**Throws** `Error` if the process is not running or the request times out.

##### `isHealthy(): boolean`

Returns `true` if the bridge process is running and responsive.

##### `getState(): ProcessState`

Returns the current process state.

##### `getStats(): object`

Returns queue statistics:

```ts
{ state: ProcessState; queueLength: number; activeRequests: number; restartCount: number }
```

#### Events

| Event        | Payload                     | Description                          |
|--------------|-----------------------------|--------------------------------------|
| `starting`   | --                          | Process is starting.                 |
| `started`    | --                          | Process is running.                  |
| `stopping`   | --                          | Process is shutting down.            |
| `stopped`    | --                          | Process has stopped.                 |
| `error`      | `Error`                     | An error occurred.                   |
| `exit`       | `(code, signal)`            | Process exited.                      |
| `restarting` | `number` (restart count)    | Auto-restarting after crash.         |
| `healthy`    | --                          | Health check passed.                 |
| `unhealthy`  | --                          | Health check failed.                 |
| `stderr`     | `string`                    | Data from the process's stderr.      |

#### `ProcessState`

```ts
type ProcessState = "stopped" | "starting" | "running" | "stopping" | "error";
```

**Example**

```ts
import { CobridgeProcess } from "cobridge";

const bridge = new CobridgeProcess({
  libraryPath: "./lib",
  requestTimeout: 10000,
  autoRestart: true,
});

bridge.on("started", () => console.log("Bridge is ready"));
bridge.on("error", (err) => console.error("Bridge error:", err));

await bridge.start();

const result = await bridge.call("CUSTINQ", inputBuffer);

await bridge.stop();
```

---

### Types

#### `GnuCOBOLInfo`

Result from detecting a GnuCOBOL installation.

```ts
interface GnuCOBOLInfo {
  installed: boolean;
  version?: string;
  compilerPath?: string;
}
```

#### `CompileOptions`

Options for compiling COBOL programs.

```ts
interface CompileOptions {
  /** Additional flags to pass to cobc */
  flags?: string[];
  /** Working directory for compilation */
  cwd?: string;
  /** Compile as a module/shared library (default: true) */
  asModule?: boolean;
}
```

#### `CallOptions`

Options for calling COBOL programs.

```ts
interface CallOptions {
  /** Working directory */
  cwd?: string;
  /** Timeout in milliseconds (default: 30000) */
  timeout?: number;
  /** Environment variables */
  env?: Record<string, string>;
}
```

---

## Server API

### `startServer(options: ServerOptions): Promise<void>`

Start the COBridge HTTP server. Scans a directory for `.cpy` copybook files, auto-generates REST API endpoints for each, and serves a Swagger UI documentation page.

**Parameters**

| Name      | Type            | Description                |
|-----------|-----------------|----------------------------|
| `options` | `ServerOptions` | Server configuration.      |

**Returns** a promise that resolves once the server is listening.

**Example**

```ts
import { startServer } from "cobridge";

await startServer({
  port: 3000,
  copybookDir: "./copybooks",
  programDir: "./programs",
});

// Server is now running:
//   API:  http://localhost:3000/api
//   Docs: http://localhost:3000/docs
```

---

### `createApp(options: ServerOptions): express.Application`

Create the Express application with all middleware and routes, without starting the HTTP listener. Useful for testing or embedding COBridge within a larger Express application.

**Parameters**

| Name      | Type            | Description                |
|-----------|-----------------|----------------------------|
| `options` | `ServerOptions` | Server configuration.      |

**Returns** an Express `Application` instance.

**Example**

```ts
import { createApp } from "cobridge";
import express from "express";

const mainApp = express();

// Mount COBridge under a sub-path
const cobridgeApp = createApp({ copybookDir: "./copybooks" });
mainApp.use("/cobridge", cobridgeApp);

mainApp.listen(8080);
```

---

### `createRouter(config: RouterConfig): express.Router`

Create an Express router that auto-generates REST endpoints from COBOL copybook files. Each `.cpy` file produces three endpoints:

- `POST /api/{slug}` -- call the COBOL program
- `GET /api/{slug}/schema` -- returns the OpenAPI spec
- `GET /api/{slug}/types` -- returns TypeScript type declarations

Additionally, `GET /api` lists all available endpoints.

**Parameters**

| Name     | Type           | Description                              |
|----------|----------------|------------------------------------------|
| `config` | `RouterConfig` | Router configuration.                    |

**`RouterConfig`**

| Property      | Type     | Description                                      |
|---------------|----------|--------------------------------------------------|
| `copybookDir` | `string` | Directory containing `.cpy` copybook files.      |
| `programDir`  | `string` | Optional directory containing compiled programs. |

**Returns** an Express `Router`.

---

### Types

#### `ServerOptions`

```ts
interface ServerOptions {
  /** Port to listen on (default: 3000) */
  port?: number;
  /** Directory containing .cpy copybook files */
  copybookDir: string;
  /** Optional directory containing compiled COBOL programs */
  programDir?: string;
}
```

---

### Generated Endpoints

When the server is running, these endpoints are available for each copybook:

| Method | Path                    | Description                              |
|--------|-------------------------|------------------------------------------|
| `POST` | `/api/{slug}`           | Call the COBOL program with JSON input.  |
| `GET`  | `/api/{slug}/schema`    | Retrieve the OpenAPI specification.      |
| `GET`  | `/api/{slug}/types`     | Retrieve TypeScript type declarations.   |
| `GET`  | `/api`                  | List all available copybook endpoints.   |
| `GET`  | `/docs`                 | Swagger UI documentation page.           |
| `GET`  | `/docs/openapi.json`    | Combined OpenAPI specification.          |
