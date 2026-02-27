# CLI Reference

Complete reference for the `cobridge` command-line interface. COBridge provides commands for parsing COBOL copybooks, generating API schemas, running a REST server, and scaffolding new projects.

## Synopsis

```
cobridge <command> [arguments] [options]
```

## Global Options

| Option | Description |
|---|---|
| `--version` | Display the current COBridge version and exit. |
| `--help` | Display help information for COBridge or a specific command. |

```bash
cobridge --version
# 0.1.0

cobridge --help
cobridge parse --help
```

---

## cobridge parse

Parse a COBOL copybook file and display the AST as formatted JSON.

### Usage

```
cobridge parse <file>
```

### Arguments

| Argument | Required | Description |
|---|---|---|
| `file` | Yes | Path to a `.cpy` copybook file. |

### Description

The `parse` command reads a COBOL copybook file, tokenises it, builds a hierarchical AST based on level numbers, and outputs the result as formatted JSON. The AST includes computed byte lengths and start offsets for every field.

This is useful for inspecting copybook structure, verifying that COBridge correctly interprets your COBOL record layouts, and debugging schema generation issues.

### Output

The output is a JSON object with the following top-level properties:

| Property | Type | Description |
|---|---|---|
| `name` | string | The copybook name, derived from the first 01-level field. |
| `fields` | array | The hierarchical field tree. |
| `totalLength` | number | Total record length in bytes. |

Each field in the tree contains:

| Property | Type | Description |
|---|---|---|
| `levelNumber` | number | COBOL level number (01-49, 66, 77, 88). |
| `name` | string | The COBOL data-name. |
| `picture` | object or undefined | Parsed PIC clause info (type, length, decimals, signed, raw). |
| `usage` | string | Storage format: `DISPLAY`, `COMP`, `COMP-1`, `COMP-2`, `COMP-3`, `BINARY`, `PACKED-DECIMAL`. |
| `occurs` | object or undefined | OCCURS clause info (count, min, dependingOn). |
| `redefines` | string or undefined | Name of the field being redefined. |
| `children` | array | Child fields (non-empty for group items). |
| `isGroup` | boolean | `true` if this is a group item (has children). |
| `byteLength` | number | Computed byte length of this field. |
| `startOffset` | number | Byte offset from the start of the record. |

### Examples

**Parse a simple copybook:**

```bash
cobridge parse ./copybooks/account.cpy
```

**Parse the included customer example:**

```bash
cobridge parse ./examples/copybooks/customer.cpy
```

**Pipe the output to a file:**

```bash
cobridge parse ./copybooks/account.cpy > account-ast.json
```

**Pipe through jq for specific fields:**

```bash
cobridge parse ./copybooks/account.cpy 2>/dev/null | jq '.fields[0].children[] | {name, byteLength, startOffset}'
```

> **Note:** The banner and status messages are written to stderr, so piping stdout captures only the JSON AST.

### Errors

- If the file does not exist, COBridge prints an error message and exits with code 1.
- If the copybook contains syntax that COBridge cannot parse, a descriptive error is printed with the line number where the problem was detected.

---

## cobridge generate

Generate OpenAPI specifications and/or TypeScript type definitions from a COBOL copybook.

### Usage

```
cobridge generate <file> [options]
```

### Arguments

| Argument | Required | Description |
|---|---|---|
| `file` | Yes | Path to a `.cpy` copybook file. |

### Options

| Option | Default | Description |
|---|---|---|
| `-o, --output <dir>` | `./generated` | Directory where output files are written. Created automatically if it does not exist. |
| `-f, --format <format>` | `both` | Output format. One of: `openapi`, `typescript`, or `both`. |

### Description

The `generate` command parses a copybook and produces schema files that describe the COBOL record layout in modern formats. These files can be used for API documentation, client code generation, request validation, and type-safe development.

### Output files

Depending on the `--format` option, the following files are created in the output directory:

| Format | File | Description |
|---|---|---|
| `openapi` | `openapi.json` | A complete OpenAPI 3.0.3 specification with paths, schemas, and request/response definitions. |
| `typescript` | `types.ts` | TypeScript interface declarations with JSDoc comments documenting the COBOL field metadata. |
| `both` | `openapi.json` and `types.ts` | Both files are generated. |

### Type mapping

COBridge maps COBOL data types to JSON Schema and TypeScript types as follows:

| COBOL PIC / USAGE | JSON Schema type | TypeScript type |
|---|---|---|
| `PIC X(n)` (alphanumeric) | `string` (maxLength: n) | `string` |
| `PIC A(n)` (alphabetic) | `string` (maxLength: n) | `string` |
| `PIC 9(n)` DISPLAY | `string` (with regex pattern) | `number` |
| `PIC 9(n)` COMP / BINARY | `integer` | `number` |
| `PIC 9(n)V9(m)` COMP-3 | `number` | `number` |
| `PIC S9(n)V9(m)` COMP-3 | `number` | `number` |
| `COMP-1` | `number` (format: float) | `number` |
| `COMP-2` | `number` (format: double) | `number` |
| Group items | `object` | `interface` |
| `OCCURS n TIMES` | `array` (maxItems: n) | `Type[]` |

### Naming conventions

- **COBOL data-names** are converted to **camelCase** for JSON properties (e.g. `CUST-FIRST-NAME` becomes `custFirstName`).
- **Interface names** use **PascalCase** (e.g. `CUSTOMER-RECORD` becomes `CustomerRecord`).
- **API slugs** use **kebab-case** (e.g. `CUSTOMER-RECORD` becomes `customer-record`).

### Examples

**Generate both formats with default output directory:**

```bash
cobridge generate ./copybooks/customer.cpy
# Creates: ./generated/openapi.json
# Creates: ./generated/types.ts
```

**Generate OpenAPI spec only to a custom directory:**

```bash
cobridge generate ./copybooks/customer.cpy --format openapi --output ./api/schemas
# Creates: ./api/schemas/openapi.json
```

**Generate TypeScript types only:**

```bash
cobridge generate ./copybooks/customer.cpy --format typescript --output ./src/types
# Creates: ./src/types/types.ts
```

**Generate schemas for multiple copybooks:**

```bash
for cpy in ./copybooks/*.cpy; do
  name=$(basename "$cpy" .cpy)
  cobridge generate "$cpy" --output "./generated/$name"
done
```

---

## cobridge serve

Start an HTTP server that auto-generates REST API endpoints from COBOL copybook files.

### Usage

```
cobridge serve [directory] [options]
```

### Arguments

| Argument | Required | Default | Description |
|---|---|---|---|
| `directory` | No | `.` (current directory) | Path to a directory containing `.cpy` copybook files. |

### Options

| Option | Default | Description |
|---|---|---|
| `-p, --port <number>` | `3000` | Port number for the HTTP server. |

### Description

The `serve` command scans the specified directory for `.cpy` files, parses each one, and creates a set of REST API endpoints automatically. It starts an Express HTTP server with CORS support, request logging, JSON body parsing, request validation, and a built-in Swagger UI documentation page.

### Auto-generated endpoints

For each copybook file `<name>.cpy` found in the directory, the server creates:

| Route | Method | Description |
|---|---|---|
| `/api/<slug>` | POST | Call the COBOL programme with a JSON request body. The request is validated against the generated JSON Schema. |
| `/api/<slug>/schema` | GET | Returns the OpenAPI 3.0.3 specification for this copybook. |
| `/api/<slug>/types` | GET | Returns the generated TypeScript type definitions as plain text. |

The slug is derived from the copybook filename: `customer.cpy` becomes `/api/customer`, `account_master.cpy` becomes `/api/account-master`.

### Global endpoints

| Route | Method | Description |
|---|---|---|
| `/api` | GET | Lists all available copybook endpoints with their names, slugs, and route details. |
| `/docs` | GET | Serves a Swagger UI page for interactive API exploration. |
| `/docs/openapi.json` | GET | Returns a combined OpenAPI specification merging all copybooks. |

### Middleware

The server includes the following middleware by default:

- **CORS** -- Cross-origin requests are allowed from any origin.
- **Request logging** -- Each request is logged to the console.
- **JSON body parsing** -- Request bodies are parsed as JSON automatically.
- **Request validation** -- POST requests are validated against the generated JSON Schema before reaching the handler.
- **Error handling** -- Unhandled errors return structured JSON error responses.

### Examples

**Start with default settings:**

```bash
cobridge serve ./copybooks
# Server runs on http://localhost:3000
```

**Start on a custom port:**

```bash
cobridge serve ./copybooks --port 8080
# Server runs on http://localhost:8080
```

**Serve the current directory:**

```bash
cd my-project/copybooks
cobridge serve
# Scans the current directory for .cpy files
```

**Test the API with curl:**

```bash
# List all endpoints
curl http://localhost:3000/api

# Call a copybook endpoint
curl -X POST http://localhost:3000/api/customer \
  -H "Content-Type: application/json" \
  -d '{"custId": "0000000001", "custStatus": "A"}'

# Get the OpenAPI spec for a single copybook
curl http://localhost:3000/api/customer/schema

# Get TypeScript types
curl http://localhost:3000/api/customer/types

# Get the combined OpenAPI spec
curl http://localhost:3000/docs/openapi.json
```

**Browse the documentation:**

Open [http://localhost:3000/docs](http://localhost:3000/docs) in your browser to access the Swagger UI, where you can explore all endpoints and make test requests interactively.

---

## cobridge init

Scaffold a new COBridge project with a standard directory structure and example files.

### Usage

```
cobridge init [directory]
```

### Arguments

| Argument | Required | Default | Description |
|---|---|---|---|
| `directory` | No | `.` (current directory) | Path where the project should be created. |

### Description

The `init` command creates a conventional COBridge project structure with directories for copybooks, COBOL programmes, and generated output. It also creates an example copybook and a `cobridge.json` configuration file.

### Files and directories created

```
<directory>/
  copybooks/            # Place your .cpy copybook files here
    example.cpy         # An example copybook to get you started
  programs/             # Place your COBOL source files here
  generated/            # Output directory for generated schemas and types
  cobridge.json         # Project configuration file
```

If any of these files already exist, they are **not** overwritten.

### The example copybook

The generated `example.cpy` contains a simple five-field record layout:

```cobol
       01  EXAMPLE-RECORD.
           05  RECORD-ID            PIC 9(10).
           05  RECORD-NAME          PIC X(30).
           05  RECORD-AMOUNT        PIC S9(9)V99 COMP-3.
           05  RECORD-DATE          PIC 9(8).
           05  RECORD-STATUS        PIC X(1).
               88  REC-ACTIVE       VALUE 'A'.
               88  REC-INACTIVE     VALUE 'I'.
```

### The configuration file

The generated `cobridge.json` contains:

```json
{
  "name": "my-cobridge-project",
  "copybooks": "./copybooks",
  "programs": "./programs",
  "output": "./generated",
  "server": {
    "port": 3000
  }
}
```

### Examples

**Scaffold in the current directory:**

```bash
mkdir my-project && cd my-project
cobridge init
```

**Scaffold in a new directory:**

```bash
cobridge init ./my-cobol-api
```

### Recommended workflow after init

```bash
# 1. Scaffold the project
cobridge init my-project
cd my-project

# 2. Parse the example copybook
cobridge parse copybooks/example.cpy

# 3. Generate API schemas
cobridge generate copybooks/example.cpy

# 4. Start the development server
cobridge serve copybooks

# 5. Open the Swagger UI
#    http://localhost:3000/docs
```

---

## Exit Codes

| Code | Meaning |
|---|---|
| `0` | The command completed successfully. |
| `1` | An error occurred. Details are printed to stderr. Common causes include: file not found, parse errors in the copybook, port already in use, or invalid command-line arguments. |

---

## Environment Variables

COBridge does not currently require any environment variables. The following standard Node.js variables may be useful:

| Variable | Description |
|---|---|
| `NODE_ENV` | Set to `production` for production deployments of `cobridge serve`. |
| `PORT` | While not read directly by COBridge (use `--port` instead), you can pass it through: `cobridge serve --port $PORT`. |

---

## Configuration File

The `cobridge.json` file (created by `cobridge init`) defines project-level defaults:

```json
{
  "name": "my-cobridge-project",
  "copybooks": "./copybooks",
  "programs": "./programs",
  "output": "./generated",
  "server": {
    "port": 3000
  }
}
```

| Key | Type | Description |
|---|---|---|
| `name` | string | Project name. |
| `copybooks` | string | Path to the directory containing `.cpy` files. |
| `programs` | string | Path to the directory containing COBOL source files. |
| `output` | string | Default output directory for `cobridge generate`. |
| `server.port` | number | Default port for `cobridge serve`. |

---

## Examples

### Full workflow: from copybook to live API

```bash
# Initialise a new project
cobridge init banking-api
cd banking-api

# Add your copybook
cp /path/to/ACCOUNT-MASTER.cpy copybooks/

# Inspect the parsed structure
cobridge parse copybooks/ACCOUNT-MASTER.cpy

# Generate OpenAPI and TypeScript
cobridge generate copybooks/ACCOUNT-MASTER.cpy --output generated/account

# Start the server
cobridge serve copybooks --port 8080

# Test the endpoint
curl -X POST http://localhost:8080/api/account-master \
  -H "Content-Type: application/json" \
  -d '{"acctId": "1234567890"}'
```

### Batch-generate schemas for all copybooks

```bash
for cpy in copybooks/*.cpy; do
  name=$(basename "$cpy" .cpy)
  echo "Generating schemas for $name..."
  cobridge generate "$cpy" --output "generated/$name"
done
```

### Use in a CI/CD pipeline

```bash
# Parse all copybooks to check for errors
for cpy in copybooks/*.cpy; do
  cobridge parse "$cpy" > /dev/null || exit 1
done

# Generate schemas
cobridge generate copybooks/main.cpy --format both --output dist/schemas
```
