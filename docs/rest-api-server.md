# REST API Server

Complete guide to the COBridge HTTP server that auto-generates REST endpoints from COBOL
copybooks.

## Overview

COBridge includes an Express-based HTTP server that:

- **Scans** a directory for `.cpy` copybook files.
- **Parses** each copybook and generates a JSON Schema, OpenAPI spec, and TypeScript types.
- **Creates** REST API endpoints for each copybook with automatic request validation.
- **Serves** a Swagger UI documentation page.

This allows any COBOL program with a defined copybook to be exposed as a modern REST API
with no manual route configuration.

---

## Starting the Server

### CLI

```bash
cobridge serve ./my-project --port 3000
```

This starts the server using:
- `./my-project/copybooks/` as the copybook directory
- `./my-project/programs/` as the compiled COBOL program directory
- Port 3000 (or the default if omitted)

### Programmatic

```ts
import { startServer } from 'cobridge/server';

await startServer({
  port: 3000,
  copybookDir: './copybooks',
  programDir: './programs',
});
```

**`ServerOptions`:**

| Option        | Type     | Default | Description                              |
|---------------|----------|---------|------------------------------------------|
| `port`        | `number` | `3000`  | HTTP port to listen on                   |
| `copybookDir` | `string` | --      | Directory containing `.cpy` copybook files |
| `programDir`  | `string` | --      | Directory containing compiled COBOL programs |

On start, the server prints:

```
  COBridge server running
  Local:   http://localhost:3000
  Docs:    http://localhost:3000/docs
  API:     http://localhost:3000/api

  Copybooks: ./copybooks
  Programs:  ./programs
```

### `createApp(options)` for Testing

For unit or integration tests, use `createApp` to get an Express application instance
without binding to a port:

```ts
import { createApp } from 'cobridge/server';
import request from 'supertest';

const app = createApp({
  copybookDir: './test/fixtures/copybooks',
  programDir: './test/fixtures/programs',
});

const res = await request(app).get('/api');
expect(res.status).toBe(200);
```

---

## Auto-Generated Endpoints

For each `.cpy` file found in the copybook directory, the server generates a set of
endpoints. The copybook filename is converted to a URL slug (lowercased, underscores
replaced with hyphens). For example, `CUSTOMER_RECORD.cpy` becomes `customer-record`.

### `POST /api/{slug}`

Submit data matching the copybook schema. The request body is validated against the
generated JSON Schema before processing.

```bash
curl -X POST http://localhost:3000/api/customer-record \
  -H "Content-Type: application/json" \
  -d '{
    "custId": 1234567890,
    "custName": "Jane Smith",
    "custBalance": 15234.50
  }'
```

**Success response (200):**

```json
{
  "copybook": "CUSTOMER-RECORD",
  "message": "COBOL program call stub -- bridge not connected",
  "input": {
    "custId": 1234567890,
    "custName": "Jane Smith",
    "custBalance": 15234.50
  }
}
```

> Note: In the current implementation, the POST handler returns a stub response. When the
> bridge is fully connected, it will marshal the request, call the COBOL program, and
> return the unmarshalled result.

### `GET /api/{slug}/schema`

Returns the OpenAPI specification for this copybook.

```bash
curl http://localhost:3000/api/customer-record/schema
```

**Response (200):** OpenAPI 3.0.3 JSON object describing the copybook's data structure,
field types, and constraints.

### `GET /api/{slug}/types`

Returns generated TypeScript type definitions for the copybook.

```bash
curl http://localhost:3000/api/customer-record/types
```

**Response (200, text/plain):** TypeScript interface definitions that match the copybook
structure. These can be saved to a `.d.ts` file for type-safe client development.

### `GET /api`

List all available copybooks and their endpoints.

```bash
curl http://localhost:3000/api
```

**Response (200):**

```json
{
  "copybooks": [
    {
      "name": "CUSTOMER-RECORD",
      "slug": "customer-record",
      "endpoints": {
        "call": "POST /api/customer-record",
        "schema": "GET /api/customer-record/schema",
        "types": "GET /api/customer-record/types"
      }
    },
    {
      "name": "BALANCE-CHECK",
      "slug": "balance-check",
      "endpoints": {
        "call": "POST /api/balance-check",
        "schema": "GET /api/balance-check/schema",
        "types": "GET /api/balance-check/types"
      }
    }
  ]
}
```

### `GET /docs`

Serves a Swagger UI page that loads the combined OpenAPI specification. Open this in a
browser to interactively explore and test all endpoints.

### `GET /docs/openapi.json`

Returns the combined OpenAPI 3.0.3 specification covering all copybooks.

```bash
curl http://localhost:3000/docs/openapi.json
```

**Response (200):**

```json
{
  "openapi": "3.0.3",
  "info": {
    "title": "COBridge API",
    "version": "1.0.0",
    "description": "Auto-generated REST API from COBOL copybooks"
  },
  "paths": { ... },
  "components": { "schemas": { ... } }
}
```

---

## Request Validation

Every `POST /api/{slug}` endpoint validates the request body against the JSON Schema
generated from the copybook. Validation is performed by the `validateRequest` middleware
before the request reaches the handler.

### What Is Validated

The validator checks:

- **Required fields** -- all fields marked as required in the schema must be present.
- **Type checking** -- values must match their expected types (`string`, `number`,
  `integer`, `object`, `array`).
- **String constraints** -- `maxLength` is enforced for string fields.
- **Numeric constraints** -- `minimum` and `maximum` bounds are enforced. Integer fields
  reject floating-point values.
- **Array constraints** -- `minItems` and `maxItems` are enforced, and each element is
  validated against the `items` schema.
- **Nested objects** -- `properties` and `required` are validated recursively.

### Error Response Format

When validation fails, the response has status 400 and a structured JSON body:

```json
{
  "error": "Request validation failed",
  "code": "VALIDATION_ERROR",
  "details": [
    "body.custName: required field missing",
    "body.custBalance: expected number, got string",
    "body.orders: array length 15 exceeds maxItems 10"
  ]
}
```

The `details` array contains one entry per validation error, with the JSON path and a
human-readable description.

### Custom Validation Rules

Validation rules are derived directly from the copybook structure:

- **PIC X(n)** fields produce `{ "type": "string", "maxLength": n }`.
- **PIC 9(n)** fields produce `{ "type": "integer" }` or `{ "type": "number" }` for
  fields with decimal places.
- **OCCURS n** fields produce `{ "type": "array", "maxItems": n, "minItems": n }`.
- **Group items** produce `{ "type": "object", "properties": { ... } }`.

---

## Middleware

COBridge provides four middleware functions that are automatically applied to the server.
They can also be used independently in custom Express applications.

### `requestLogger()`

Logs every incoming request with method, URL, status code, and response time.

```ts
import { requestLogger } from 'cobridge/server/middleware';

app.use(requestLogger());
```

Log output format:

```
[COBridge] GET /api 200 12ms
[COBridge] POST /api/customer-record 400 3ms
```

### `errorHandler()`

Catches errors thrown by route handlers and returns structured JSON responses. Must be
registered **after** all routes.

```ts
import { errorHandler } from 'cobridge/server/middleware';

app.use(errorHandler());
```

**Response format:**

```json
{
  "error": "Something went wrong",
  "code": "SOME_CODE",
  "details": ["detail 1", "detail 2"]
}
```

In development mode (`NODE_ENV=development`), the error stack trace is included in the
response for debugging.

The `error.status` property controls the HTTP status code (defaults to 500). The optional
`error.code` and `error.details` properties are included in the response when present.

### `validateRequest(schema)`

Validates the request body against a JSON Schema object. Used internally by the
auto-generated routes, but available for custom endpoints.

```ts
import { validateRequest } from 'cobridge/server/middleware';

const schema = {
  type: 'object',
  required: ['name'],
  properties: {
    name: { type: 'string', maxLength: 50 },
    age: { type: 'integer', minimum: 0, maximum: 150 },
  },
};

app.post('/custom', validateRequest(schema), (req, res) => {
  res.json({ received: req.body });
});
```

### `cors()`

Sets permissive CORS headers for development use. Handles preflight `OPTIONS` requests
automatically.

```ts
import { cors } from 'cobridge/server/middleware';

app.use(cors());
```

**Headers set:**

| Header                           | Value                                |
|----------------------------------|--------------------------------------|
| `Access-Control-Allow-Origin`    | `*`                                  |
| `Access-Control-Allow-Methods`   | `GET, POST, PUT, DELETE, OPTIONS`    |
| `Access-Control-Allow-Headers`   | `Content-Type, Authorization`        |
| `Access-Control-Max-Age`         | `86400` (24 hours)                   |

For production deployments, replace this with a more restrictive CORS configuration that
specifies allowed origins.

---

## Configuration

### Port

Set the port via the `port` option or the `PORT` environment variable:

```bash
PORT=8080 cobridge serve ./my-project
```

```ts
await startServer({ port: 8080, copybookDir: './copybooks' });
```

### Copybook Directory

The `copybookDir` option specifies where the server looks for `.cpy` files. Both `.cpy`
and `.CPY` extensions are recognised. The directory is scanned once at startup; adding new
copybooks requires a restart.

### Program Directory

The `programDir` option specifies where compiled COBOL modules (`.so`, `.dll`, `.dylib`)
are located. This is used by the bridge when connecting COBOL program calls to the REST
endpoints.

### CORS Settings

The built-in `cors()` middleware uses permissive defaults. For production, configure CORS
at the reverse proxy level or replace the middleware:

```ts
import corsLib from 'cors';

const app = createApp({ copybookDir: './copybooks' });
// Remove default CORS and add restrictive policy
app.use(corsLib({
  origin: 'https://myapp.example.com',
  methods: ['GET', 'POST'],
}));
```

---

## Production Deployment

### Running Behind a Reverse Proxy

In production, run COBridge behind nginx or a similar reverse proxy:

```nginx
upstream cobridge {
    server 127.0.0.1:3000;
}

server {
    listen 443 ssl;
    server_name api.example.com;

    location / {
        proxy_pass http://cobridge;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
}
```

### Environment Variables

| Variable    | Description                         | Default |
|-------------|-------------------------------------|---------|
| `PORT`      | HTTP port                           | `3000`  |
| `NODE_ENV`  | Set to `production` to hide stack traces in errors | `development` |

### Health Check Endpoint

Use `GET /api` as a lightweight health check. It returns 200 with the list of loaded
copybooks and does not invoke any COBOL programs.

```bash
curl -f http://localhost:3000/api || echo "Server is down"
```

### Logging

The `requestLogger` middleware logs to stdout in a simple format suitable for log
aggregation:

```
[COBridge] POST /api/customer-record 200 45ms
```

For structured logging, replace `requestLogger()` with a logging library such as
`pino-http` or `morgan`:

```ts
import pinoHttp from 'pino-http';

const app = express();
app.use(pinoHttp());
```

---

## Complete Example

A full walk-through from project setup to testing with curl.

### Step 1: Create the Project Structure

```bash
mkdir my-cobol-api
cd my-cobol-api
mkdir copybooks programs
```

### Step 2: Add a Copybook

Create `copybooks/customer-record.cpy`:

```cobol
       01  CUSTOMER-RECORD.
           05  CUST-ID        PIC 9(10).
           05  CUST-NAME      PIC X(30).
           05  CUST-BALANCE   PIC S9(13)V99 COMP-3.
           05  CUST-STATUS    PIC X(1).
```

### Step 3: Add a COBOL Program

Create `programs/customer-record.cbl`:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTREC.

       DATA DIVISION.
       LINKAGE SECTION.
       01  LS-CUST-ID        PIC 9(10).
       01  LS-CUST-NAME      PIC X(30).
       01  LS-CUST-BALANCE   PIC S9(13)V99 COMP-3.
       01  LS-CUST-STATUS    PIC X(1).

       PROCEDURE DIVISION USING LS-CUST-ID
                                LS-CUST-NAME
                                LS-CUST-BALANCE
                                LS-CUST-STATUS.

       MAIN-LOGIC.
           MOVE "Alice Johnson" TO LS-CUST-NAME
           MOVE 42150.75 TO LS-CUST-BALANCE
           MOVE "A" TO LS-CUST-STATUS
           GOBACK.
```

### Step 4: Start the Server

```bash
npx cobridge serve . --port 3000
```

Or programmatically:

```ts
import { startServer } from 'cobridge/server';

await startServer({
  port: 3000,
  copybookDir: './copybooks',
  programDir: './programs',
});
```

### Step 5: Test the Endpoints

**List available endpoints:**

```bash
curl http://localhost:3000/api
```

```json
{
  "copybooks": [
    {
      "name": "CUSTOMER-RECORD",
      "slug": "customer-record",
      "endpoints": {
        "call": "POST /api/customer-record",
        "schema": "GET /api/customer-record/schema",
        "types": "GET /api/customer-record/types"
      }
    }
  ]
}
```

**Submit a request:**

```bash
curl -X POST http://localhost:3000/api/customer-record \
  -H "Content-Type: application/json" \
  -d '{
    "custId": 1234567890,
    "custName": "Jane Smith",
    "custBalance": 15234.50,
    "custStatus": "A"
  }'
```

**Get the OpenAPI schema:**

```bash
curl http://localhost:3000/api/customer-record/schema
```

**Get TypeScript types:**

```bash
curl http://localhost:3000/api/customer-record/types
```

**Get the combined OpenAPI spec:**

```bash
curl http://localhost:3000/docs/openapi.json
```

**Open Swagger UI:**

Open `http://localhost:3000/docs` in a browser to explore the API interactively.

**Test validation errors:**

```bash
curl -X POST http://localhost:3000/api/customer-record \
  -H "Content-Type: application/json" \
  -d '{
    "custName": 12345
  }'
```

```json
{
  "error": "Request validation failed",
  "code": "VALIDATION_ERROR",
  "details": [
    "body.custName: expected string, got number"
  ]
}
```
