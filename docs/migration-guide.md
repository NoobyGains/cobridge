# Migration Guide

This guide helps you migrate from commercial COBOL integration products to COBridge. Whether you are using IBM z/OS Connect, OpenLegacy, or another tool, COBridge can replace or complement your existing setup.

---

## Migrating from IBM z/OS Connect

### Conceptual Differences

| Concept               | z/OS Connect                        | COBridge                                      |
|-----------------------|-------------------------------------|-----------------------------------------------|
| Runtime               | Runs on z/OS (mainframe)            | Runs on Node.js (any platform)                |
| Copybook handling     | Integrated into z/OS tooling        | Standalone parser, works with any copybook     |
| API generation        | z/OS Connect EE Service Archive     | OpenAPI 3.0 spec + Express server             |
| Data transformation   | Built-in z/OS Connect transformers  | Marshalling module (JSON <-> COBOL binary)    |
| COBOL invocation      | Direct CICS/IMS/Batch calls         | GnuCOBOL bridge (local) or custom integration |
| Deployment            | z/OS Liberty server                 | Docker, bare metal, cloud -- anywhere Node.js runs |
| Licence               | IBM commercial licence              | MIT (free and open source)                    |

### Mapping z/OS Connect Concepts to COBridge

- **Service Archive (.sar)** -> COBridge does not use archive files. Instead, place your `.cpy` copybook files in a directory and point COBridge at it.
- **Service interface** -> COBridge auto-generates OpenAPI specs from copybooks (`generateOpenApiSpec()`).
- **Data transformation rules** -> COBridge's marshaller handles transformation automatically based on the copybook structure.
- **API requester** -> Use the generated REST endpoints or call the marshalling API directly.

### Step-by-Step Migration

#### 1. Export Your Copybooks

Extract the COBOL copybook files used by your z/OS Connect services. These are typically found in your PDS (Partitioned Data Set) libraries on z/OS. Transfer them to your local file system:

```bash
# If using FTP from z/OS
ftp mainframe.example.com
> get 'USER.COPYLIB(CUSTOMER)' ./copybooks/CUSTOMER.cpy
> get 'USER.COPYLIB(ACCOUNT)' ./copybooks/ACCOUNT.cpy
```

Ensure the files are converted from EBCDIC to ASCII during transfer (most FTP clients handle this in text mode).

#### 2. Parse with COBridge

Verify each copybook parses correctly:

```bash
npx cobridge parse ./copybooks/CUSTOMER.cpy
```

Or programmatically:

```ts
import { parseCopybook } from "cobridge";
import * as fs from "fs";

const source = fs.readFileSync("./copybooks/CUSTOMER.cpy", "utf-8");
const copybook = parseCopybook(source);

console.log(`Parsed: ${copybook.name}, ${copybook.totalLength} bytes, ${copybook.fields.length} top-level fields`);
```

#### 3. Compare Generated OpenAPI Specs

Generate an OpenAPI spec and compare it with your z/OS Connect service interface:

```bash
npx cobridge codegen --copybook ./copybooks/CUSTOMER.cpy --openapi --output ./generated/
```

Review the generated `openapi.json` and compare the schema definitions with your existing z/OS Connect service interface. The field names will be converted to camelCase (e.g. `CUST-FIRST-NAME` becomes `custFirstName`).

#### 4. Set Up COBridge Server

Start the COBridge server to serve your auto-generated API:

```bash
npx cobridge serve --copybooks ./copybooks --port 3000
```

This gives you:
- REST endpoints at `http://localhost:3000/api/{copybook-name}`
- Swagger UI documentation at `http://localhost:3000/docs`
- OpenAPI spec at `http://localhost:3000/docs/openapi.json`

#### 5. Update Client Applications

Update your API consumers to point to the new COBridge endpoints. The request and response formats will be JSON, matching the copybook field structure.

**Before (z/OS Connect):**
```
POST https://mainframe:9443/zosConnect/services/customerService
```

**After (COBridge):**
```
POST http://cobridge-host:3000/api/customer-record
```

The JSON payload structure should be very similar, though field names may differ (COBridge uses camelCase by default).

---

## Migrating from OpenLegacy

### Conceptual Differences

| Concept               | OpenLegacy                          | COBridge                                      |
|-----------------------|-------------------------------------|-----------------------------------------------|
| Architecture          | Java-based middleware               | Node.js library and CLI                       |
| Copybook handling     | Java SDK with annotations           | TypeScript parser producing a typed AST       |
| API generation        | Spring Boot REST controllers        | Express.js server with auto-generated routes  |
| Data transformation   | Java object mapping                 | Buffer-level marshalling (JSON <-> COBOL)     |
| Connectors            | Mainframe, AS/400, CICS, MQ, etc.  | GnuCOBOL (local), custom integration (remote) |
| Deployment            | Java application server             | Node.js process, Docker, serverless           |
| Licence               | Commercial licence                  | MIT (free and open source)                    |

### Mapping OpenLegacy Concepts to COBridge

- **Entity classes** (annotated Java POJOs) -> COBridge `Copybook` AST + generated TypeScript interfaces
- **SDK connectors** (CICS, MQ, etc.) -> COBridge bridge module (GnuCOBOL) or custom integration layer
- **Trail** (API generation from connector) -> `cobridge serve` or `generateOpenApiSpec()`
- **Hub** (hosted API gateway) -> Self-hosted Express server with `createApp()`

### Step-by-Step Migration

#### 1. Identify Your Copybooks

In OpenLegacy projects, the copybook definitions are typically embedded within the entity class annotations or stored alongside the project. Extract the original `.cpy` files:

```bash
# Check your OpenLegacy project's resources directory
ls ./src/main/resources/copybooks/
```

#### 2. Parse and Validate

```bash
npx cobridge parse ./copybooks/CUSTOMER.cpy --json
```

Compare the parsed AST with your OpenLegacy entity class to ensure all fields are captured correctly.

#### 3. Generate API Artefacts

```bash
# Generate everything at once
npx cobridge codegen --copybook ./copybooks/CUSTOMER.cpy \
  --openapi --typescript --json-schema \
  --output ./generated/
```

#### 4. Replace the Server

Instead of a Spring Boot application, use COBridge's Express server:

```ts
import { startServer } from "cobridge";

await startServer({
  port: 3000,
  copybookDir: "./copybooks",
  programDir: "./programs",
});
```

#### 5. Migrate Clients

Update API consumers. The endpoint structure changes from OpenLegacy's Spring Boot routes to COBridge's auto-generated routes:

**Before (OpenLegacy):**
```
POST /api/customer
```

**After (COBridge):**
```
POST /api/customer-record
```

---

## Key Differences

### Feature Comparison

| Feature                          | z/OS Connect     | OpenLegacy       | COBridge          |
|----------------------------------|------------------|------------------|-------------------|
| **Licence**                      | Commercial       | Commercial       | MIT (free)        |
| **Language**                     | Java / z/OS      | Java             | TypeScript / Node |
| **Copybook parsing**            | Yes              | Yes              | Yes               |
| **OpenAPI generation**           | Yes              | Yes              | Yes               |
| **TypeScript types**             | No               | No               | Yes               |
| **JSON Schema generation**       | No               | Limited          | Yes               |
| **EBCDIC conversion**            | Yes              | Yes              | Yes               |
| **Packed decimal (COMP-3)**      | Yes              | Yes              | Yes               |
| **Binary (COMP/BINARY)**         | Yes              | Yes              | Yes               |
| **REDEFINES support**            | Yes              | Yes              | Yes               |
| **OCCURS DEPENDING ON**          | Yes              | Yes              | Yes               |
| **Direct mainframe connectivity**| Yes (native)     | Yes (connectors) | No (bridge only)  |
| **CICS integration**             | Yes (native)     | Yes (connector)  | No                |
| **MQ integration**               | Yes              | Yes (connector)  | No                |
| **Docker support**               | Limited          | Yes              | Yes               |
| **Serverless support**           | No               | Limited          | Yes               |
| **Self-hosted**                  | z/OS only        | Yes              | Yes               |
| **Cloud-native**                 | No               | Yes              | Yes               |

### What COBridge Does That They Don't

- **TypeScript-first**: Native TypeScript types generated directly from copybooks, with full type safety.
- **Zero vendor lock-in**: MIT licence, no proprietary runtime, no licence keys.
- **Lightweight**: A single npm package with minimal dependencies, versus a full Java application server.
- **Node.js ecosystem**: Use with any Node.js framework, serverless platform, or build tool.
- **Low-level marshalling API**: Direct access to EBCDIC, packed decimal, and binary encoding/decoding functions for custom integration.

### What They Do That COBridge Doesn't (Yet)

- **Direct mainframe connectivity**: z/OS Connect runs on the mainframe; OpenLegacy has connectors for CICS, IMS, MQ, and more. COBridge requires a separate integration layer for mainframe communication.
- **Visual design tools**: Both z/OS Connect and OpenLegacy offer graphical tools for mapping and configuring APIs. COBridge is CLI and code-driven.
- **Enterprise support**: Commercial products include vendor support, SLAs, and professional services. COBridge is community-supported.
- **Non-COBOL legacy systems**: OpenLegacy supports AS/400 (RPG), Natural/ADABAS, and other legacy platforms. COBridge focuses exclusively on COBOL.

---

## Hybrid Approach

You do not need to migrate everything at once. COBridge works well alongside existing tools.

### Using COBridge Alongside Existing Tools

**Scenario 1: Schema Generation Only**

Keep your existing z/OS Connect or OpenLegacy setup for runtime COBOL invocation, but use COBridge to generate TypeScript types and JSON schemas for your front-end team:

```bash
npx cobridge codegen --copybook ./copybooks/CUSTOMER.cpy --typescript --output ./frontend/types/
```

**Scenario 2: Local Development**

Use COBridge with GnuCOBOL for local development and testing, whilst your production environment continues to use z/OS Connect:

```bash
# Local development
cobridge serve --copybooks ./copybooks --port 3000

# Production: z/OS Connect on the mainframe (unchanged)
```

**Scenario 3: Complementary Marshalling**

Use your existing middleware for mainframe communication, but use COBridge's marshalling functions to handle the data conversion in your Node.js application:

```ts
import { marshalToCobol, marshalFromCobol, fromParserFields, parseCopybook } from "cobridge";

// Parse the copybook once
const copybook = parseCopybook(copybookSource);
const fields = fromParserFields(copybook.fields);

// Marshal data before sending to mainframe via your existing middleware
const cobolBuffer = marshalToCobol(jsonData, fields, { useEbcdic: true, codePage: "037" });

// Send cobolBuffer via MQ / TCP / your existing transport
// ...

// Unmarshal the response
const responseJson = marshalFromCobol(responseBuffer, fields, { useEbcdic: true, codePage: "037" });
```

### Gradual Migration Strategy

1. **Phase 1 -- Schema generation**: Use COBridge to generate OpenAPI specs, TypeScript types, and JSON schemas alongside your existing tools. Validate that COBridge produces equivalent schemas.

2. **Phase 2 -- Local development**: Set up COBridge with GnuCOBOL for local development and testing. Developers can work without mainframe access.

3. **Phase 3 -- Non-critical services**: Migrate a small, non-critical service to COBridge in production. Monitor and validate.

4. **Phase 4 -- Broader rollout**: Once confident, migrate additional services. Keep your mainframe running -- COBridge bridges to it, it doesn't replace it.

5. **Phase 5 -- Full migration**: When all services are migrated, decommission the commercial middleware. Redirect savings to your development team.
