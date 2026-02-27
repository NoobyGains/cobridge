# GnuCOBOL Bridge

Complete guide to compiling and calling COBOL programs from Node.js using GnuCOBOL.

## Overview

[GnuCOBOL](https://gnucobol.sourceforge.io/) is a free, open-source COBOL compiler that
translates COBOL source code to C, then compiles it to native machine code. It produces
shared libraries (`.so`, `.dll`, `.dylib`) that can be loaded and executed on any platform.

The COBridge GnuCOBOL bridge provides:

- **Detection** -- check whether GnuCOBOL is installed and query its version.
- **Compilation** -- compile `.cbl` source files to shared library modules via `cobc`.
- **Invocation** -- call compiled COBOL programs from Node.js, passing data through
  stdin/stdout.
- **Process management** -- run a persistent bridge process for high-throughput workloads
  with request queuing, health checking, and auto-restart.

---

## Prerequisites

### Installing GnuCOBOL

#### Ubuntu / Debian

```bash
sudo apt update
sudo apt install gnucobol
```

#### macOS

```bash
brew install gnucobol
```

#### Windows

GnuCOBOL is not natively supported on Windows. Use one of these approaches:

- **WSL (recommended):** Install Windows Subsystem for Linux, then follow the Ubuntu
  instructions above.
- **MSYS2:** Install MSYS2 from https://www.msys2.org/, then run:

  ```bash
  pacman -S mingw-w64-x86_64-gnucobol
  ```

#### Docker (planned)

A `cobridge/gnucobol` Docker image is planned for future releases. This will provide a
pre-configured environment with GnuCOBOL and all COBridge dependencies.

### Verifying Installation

After installation, verify that `cobc` is available:

```bash
cobc --version
```

Expected output:

```
cobc (GnuCOBOL) 3.2.0
...
```

---

## Detecting GnuCOBOL

### `detectGnuCOBOL()`

Probe the system for a working GnuCOBOL installation.

```ts
import { detectGnuCOBOL } from 'cobridge/bridge/gnucobol';

const info = await detectGnuCOBOL();
console.log(info);
// {
//   installed: true,
//   version: '3.2.0',
//   compilerPath: '/usr/bin/cobc'
// }
```

**Returns:** `Promise<GnuCOBOLInfo>`

```ts
interface GnuCOBOLInfo {
  installed: boolean;      // true if cobc was found and executed successfully
  version?: string;        // Version string (e.g. '3.2.0')
  compilerPath?: string;   // Absolute path to the cobc binary
}
```

The function runs `cobc --version` with a 5-second timeout and parses the version from the
output. It then runs `which cobc` (or `where cobc` on Windows) to determine the compiler
path. If `cobc` is not found or fails, `{ installed: false }` is returned with no error
thrown.

---

## Compiling COBOL Programs

### `compileCobol(sourcePath, outputPath?, options?)`

Compile a COBOL source file to a shared library module.

```ts
import { compileCobol } from 'cobridge/bridge/gnucobol';

const libraryPath = await compileCobol('./programs/customer-lookup.cbl');
console.log(libraryPath); // './programs/customer-lookup.so'
```

**Parameters:**

| Name         | Type             | Default                   | Description                          |
|--------------|------------------|---------------------------|--------------------------------------|
| `sourcePath` | `string`         | --                        | Path to the `.cbl` / `.cob` source file |
| `outputPath` | `string`         | Same name, platform extension | Output path for the compiled module |
| `options`    | `CompileOptions` | `{}`                      | Additional compilation options       |

**Returns:** `Promise<string>` -- path to the compiled shared library.

**Throws:** `Error` if the source file does not exist or compilation fails.

### CompileOptions

```ts
interface CompileOptions {
  flags?: string[];    // Additional flags to pass to cobc
  cwd?: string;        // Working directory for compilation
  asModule?: boolean;  // Compile as module (true, default) or executable (false)
}
```

### How It Works

By default, `compileCobol` runs:

```bash
cobc -m -o <output> <source>
```

The `-m` flag tells GnuCOBOL to produce a dynamically loadable module rather than a
standalone executable. The output file extension is platform-dependent:

| Platform | Extension |
|----------|-----------|
| Linux    | `.so`     |
| Windows  | `.dll`    |
| macOS    | `.dylib`  |

If `asModule` is set to `false`, the `-x` flag is used instead, producing a standalone
executable.

The compilation timeout is set to 60 seconds. Additional compiler flags (such as
`-free` for free-format source or `-std=cobol2014` for a specific standard) can be passed
via the `flags` option.

### Error Handling

When compilation fails, the error message includes the stderr output from `cobc`, which
typically contains line numbers and descriptions of syntax errors:

```ts
try {
  await compileCobol('./programs/broken.cbl');
} catch (err) {
  console.error(err.message);
  // COBOL compilation failed: broken.cbl:15: error: syntax error, unexpected MOVE
}
```

---

## Calling COBOL Programs

### `callCobolProgram(libraryPath, programName, linkageData, options?)`

Execute a compiled COBOL program, passing data via stdin and reading results from stdout.

```ts
import { callCobolProgram } from 'cobridge/bridge/gnucobol';

const input = Buffer.alloc(80); // Linkage section data
const result = await callCobolProgram(
  './programs/customer-lookup.so',
  'CUSTLKUP',
  input
);
console.log(result); // Buffer containing modified linkage data
```

**Parameters:**

| Name          | Type          | Default | Description                                 |
|---------------|---------------|---------|---------------------------------------------|
| `libraryPath` | `string`      | --      | Path to the compiled shared library         |
| `programName` | `string`      | --      | COBOL PROGRAM-ID to invoke                  |
| `linkageData` | `Buffer`      | --      | Input buffer (LINKAGE SECTION data)         |
| `options`     | `CallOptions` | `{}`    | Call options (timeout, cwd, env)            |

**Returns:** `Promise<Buffer>` -- the modified linkage section data from stdout.

**Throws:** `Error` if the library does not exist, the program fails, or the call times
out.

### CallOptions

```ts
interface CallOptions {
  cwd?: string;                  // Working directory
  timeout?: number;              // Timeout in milliseconds (default: 30000)
  env?: Record<string, string>;  // Additional environment variables
}
```

### How Data Is Passed

COBridge uses a stdin/stdout protocol to communicate with COBOL programs:

1. The bridge spawns `cobcrun <programName>` as a child process.
2. The `COB_LIBRARY_PATH` environment variable is set to the directory containing the
   compiled module, so `cobcrun` can locate it.
3. The `linkageData` buffer is written to the child process's stdin.
4. The COBOL program reads from stdin, processes the data, and writes its output to stdout.
5. The bridge reads stdout into a buffer and returns it.

The maximum buffer size for a single call is 10 MB.

### Timeout Configuration

The default timeout is 30 seconds. For long-running COBOL programs, increase it:

```ts
const result = await callCobolProgram(
  './programs/batch-report.so',
  'BATCHRPT',
  inputBuffer,
  { timeout: 120000 } // 2 minutes
);
```

---

## Process Manager

For high-throughput workloads where spawning a new process per request is too expensive,
COBridge provides the `CobridgeProcess` class. It maintains a persistent bridge process
and multiplexes requests through it.

### `CobridgeProcess`

```ts
import { CobridgeProcess } from 'cobridge/bridge/process';

const bridge = new CobridgeProcess({
  libraryPath: './compiled-programs',
  requestTimeout: 10000,
  autoRestart: true,
  maxRestarts: 5,
});

await bridge.start();

const result = await bridge.call('CUSTLKUP', inputBuffer);

await bridge.stop();
```

### CobridgeProcessConfig

```ts
interface CobridgeProcessConfig {
  libraryPath: string;             // Directory containing compiled COBOL modules
  maxConcurrent?: number;          // Max concurrent requests (default: 1)
  healthCheckInterval?: number;    // Health check interval in ms (default: 30000)
  requestTimeout?: number;         // Per-request timeout in ms (default: 30000)
  autoRestart?: boolean;           // Auto-restart on crash (default: true)
  maxRestarts?: number;            // Maximum restart attempts (default: 3)
  env?: Record<string, string>;    // Environment variables for the process
}
```

### Start / Stop / Restart

```ts
await bridge.start();    // Spawn the bridge process
await bridge.stop();     // Graceful shutdown (SIGTERM, then SIGKILL after 5s)
await bridge.restart();  // Stop then start
```

### Health Checking

The process manager runs a periodic health check (default: every 30 seconds). If the
bridge process is found to be unhealthy (crashed or killed), it is automatically restarted
up to `maxRestarts` times.

```ts
console.log(bridge.isHealthy()); // true or false
```

### Request Queuing

Because COBOL is inherently single-threaded, the process manager queues requests and
dispatches them one at a time by default. The `maxConcurrent` option controls how many
requests can be in flight simultaneously (useful if the bridge process supports
multiplexing).

Each request has its own timeout. If a request exceeds its timeout, it is rejected with an
error and removed from the queue.

### Wire Protocol

The process manager uses a length-prefixed binary protocol over stdin/stdout:

**Request (stdin):**
```
[4 bytes: payload length (big-endian uint32)]
[N bytes: program name (null-terminated ASCII)]
[M bytes: linkage data]
```

**Response (stdout):**
```
[4 bytes: payload length (big-endian uint32)]
[N bytes: response data]
```

### Events

`CobridgeProcess` extends `EventEmitter` and emits these events:

| Event        | Payload                  | Description                          |
|--------------|--------------------------|--------------------------------------|
| `starting`   | --                       | Process is about to start            |
| `started`    | --                       | Process is running                   |
| `stopping`   | --                       | Graceful shutdown initiated          |
| `stopped`    | --                       | Process has exited                   |
| `exit`       | `(code, signal)`         | Process exited (with exit details)   |
| `error`      | `Error`                  | An error occurred                    |
| `restarting` | `restartCount`           | Auto-restart triggered               |
| `healthy`    | --                       | Health check passed                  |
| `unhealthy`  | --                       | Health check failed                  |
| `stderr`     | `string`                 | Output from the process's stderr     |

```ts
bridge.on('restarting', (count) => {
  console.log(`Bridge restarting (attempt ${count})`);
});

bridge.on('error', (err) => {
  console.error('Bridge error:', err.message);
});
```

### Statistics

```ts
const stats = bridge.getStats();
console.log(stats);
// {
//   state: 'running',
//   queueLength: 0,
//   activeRequests: 1,
//   restartCount: 0
// }
```

### Process States

The bridge process transitions through these states:

```
stopped --> starting --> running --> stopping --> stopped
                |                       ^
                v                       |
              error  ---- (auto) -------+
```

| State      | Description                              |
|------------|------------------------------------------|
| `stopped`  | No process running                       |
| `starting` | Process is being spawned                 |
| `running`  | Process is active and accepting requests |
| `stopping` | Graceful shutdown in progress            |
| `error`    | Process encountered a fatal error        |

---

## Writing COBOL Programs for COBridge

COBOL programs called by COBridge must follow a specific structure to receive input and
return output through the LINKAGE SECTION.

### Structure

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MYPROG.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-TEMP          PIC X(10).

       LINKAGE SECTION.
       01  LS-INPUT-FIELD    PIC X(20).
       01  LS-OUTPUT-FIELD   PIC X(50).
       01  LS-RETURN-CODE    PIC 9(2).

       PROCEDURE DIVISION USING LS-INPUT-FIELD
                                LS-OUTPUT-FIELD
                                LS-RETURN-CODE.

       MAIN-LOGIC.
           PERFORM PROCESS-DATA
           GOBACK.

       PROCESS-DATA.
           MOVE "Result" TO LS-OUTPUT-FIELD
           MOVE 0 TO LS-RETURN-CODE.
```

### Key Points

1. **LINKAGE SECTION** -- defines the data interface between COBridge and your program.
   These fields are populated from the input buffer and read back as the output buffer.

2. **PROCEDURE DIVISION USING** -- lists the linkage fields that the program expects. This
   is how GnuCOBOL maps the passed data to COBOL variables.

3. **GOBACK** -- returns control to the caller. Always use `GOBACK` rather than
   `STOP RUN`, which would terminate the entire process.

4. **Return codes** -- use a numeric field (e.g. `PIC 9(2)`) to signal success (`00`) or
   error conditions to the caller.

### Example: customer-lookup.cbl Explained

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTLKUP.
```

The `PROGRAM-ID` is the name you pass to `callCobolProgram` or `bridge.call()`.

```cobol
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-STATUS          PIC X(2).
```

`WORKING-STORAGE` holds private variables that exist only during execution.

```cobol
       LINKAGE SECTION.
       01  LS-CUST-ID         PIC 9(10).
       01  LS-CUST-NAME       PIC X(60).
       01  LS-CUST-BALANCE    PIC S9(13)V99 COMP-3.
       01  LS-RETURN-CODE     PIC 9(2).
```

The `LINKAGE SECTION` defines the data contract. In a real application, the program would
use `LS-CUST-ID` to look up a customer record in a database and populate `LS-CUST-NAME`
and `LS-CUST-BALANCE` with the results.

- `LS-CUST-ID` -- 10-digit customer identifier (10 bytes, DISPLAY).
- `LS-CUST-NAME` -- 60-character customer name (60 bytes).
- `LS-CUST-BALANCE` -- signed 15-digit packed decimal with 2 decimal places (8 bytes).
- `LS-RETURN-CODE` -- 2-digit return code (2 bytes).

```cobol
       PROCEDURE DIVISION USING LS-CUST-ID
                                LS-CUST-NAME
                                LS-CUST-BALANCE
                                LS-RETURN-CODE.

       MAIN-LOGIC.
           MOVE "John Doe" TO LS-CUST-NAME
           MOVE 15234.50 TO LS-CUST-BALANCE
           MOVE 0 TO LS-RETURN-CODE
           GOBACK.
```

The `PROCEDURE DIVISION USING` clause binds the linkage fields in order. The program sets
the output values and returns via `GOBACK`.

---

## Complete Example

The following walk-through shows how to write a COBOL program, compile it with COBridge,
call it from Node.js, and parse the results as JSON.

### Step 1: Write the COBOL Program

Create `programs/balance-check.cbl`:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BALCHECK.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CURRENT-DATE    PIC 9(8).

       LINKAGE SECTION.
       01  LS-ACCOUNT-NUM     PIC X(12).
       01  LS-BALANCE         PIC S9(13)V99 COMP-3.
       01  LS-AVAILABLE       PIC S9(13)V99 COMP-3.
       01  LS-RETURN-CODE     PIC 9(2).

       PROCEDURE DIVISION USING LS-ACCOUNT-NUM
                                LS-BALANCE
                                LS-AVAILABLE
                                LS-RETURN-CODE.

       MAIN-LOGIC.
           MOVE 52750.00 TO LS-BALANCE
           MOVE 48500.00 TO LS-AVAILABLE
           MOVE 0 TO LS-RETURN-CODE
           GOBACK.
```

### Step 2: Compile

```ts
import { compileCobol, detectGnuCOBOL } from 'cobridge/bridge/gnucobol';

// Verify GnuCOBOL is installed
const info = await detectGnuCOBOL();
if (!info.installed) {
  throw new Error('GnuCOBOL is not installed. Run: apt install gnucobol');
}
console.log(`Using GnuCOBOL ${info.version}`);

// Compile the program
const libraryPath = await compileCobol('./programs/balance-check.cbl');
console.log(`Compiled to: ${libraryPath}`);
```

### Step 3: Define the Linkage Layout

```ts
import { MarshalField, marshalToCobol, marshalFromCobol } from 'cobridge/marshal/converter';

const linkageFields: MarshalField[] = [
  { name: 'LS-ACCOUNT-NUM',  level: 1, pic: 'X(12)' },
  { name: 'LS-BALANCE',      level: 1, pic: 'S9(13)V99', usage: 'COMP-3' },
  { name: 'LS-AVAILABLE',    level: 1, pic: 'S9(13)V99', usage: 'COMP-3' },
  { name: 'LS-RETURN-CODE',  level: 1, pic: '9(2)' },
];
```

### Step 4: Call the Program

```ts
import { callCobolProgram } from 'cobridge/bridge/gnucobol';

// Marshal input JSON to a COBOL buffer
const inputBuffer = marshalToCobol(
  {
    lsAccountNum: 'ACC-00012345',
    lsBalance: 0,
    lsAvailable: 0,
    lsReturnCode: 0,
  },
  linkageFields
);

// Call the COBOL program
const outputBuffer = await callCobolProgram(libraryPath, 'BALCHECK', inputBuffer);

// Unmarshal the output buffer to JSON
const result = marshalFromCobol(outputBuffer, linkageFields);
console.log(result);
// {
//   lsAccountNum: 'ACC-00012345',
//   lsBalance: 52750,
//   lsAvailable: 48500,
//   lsReturnCode: 0
// }
```

### Step 5: Handle Errors

```ts
if (result.lsReturnCode !== 0) {
  console.error(`COBOL program returned error code: ${result.lsReturnCode}`);
} else {
  console.log(`Account: ${result.lsAccountNum}`);
  console.log(`Balance: $${result.lsBalance.toFixed(2)}`);
  console.log(`Available: $${result.lsAvailable.toFixed(2)}`);
}
```

### Using the Process Manager for High Throughput

For applications that need to call COBOL programs repeatedly (e.g. an API server), use
`CobridgeProcess` to avoid the overhead of spawning a new process per request:

```ts
import { CobridgeProcess } from 'cobridge/bridge/process';
import { marshalToCobol, marshalFromCobol, MarshalField } from 'cobridge/marshal/converter';

const bridge = new CobridgeProcess({
  libraryPath: './compiled-programs',
  requestTimeout: 10000,
  autoRestart: true,
});

await bridge.start();

// Handle multiple requests concurrently (queued internally)
const results = await Promise.all([
  bridge.call('BALCHECK', marshalToCobol({ lsAccountNum: 'ACC-001' }, linkageFields)),
  bridge.call('BALCHECK', marshalToCobol({ lsAccountNum: 'ACC-002' }, linkageFields)),
  bridge.call('BALCHECK', marshalToCobol({ lsAccountNum: 'ACC-003' }, linkageFields)),
]);

for (const buf of results) {
  const json = marshalFromCobol(buf, linkageFields);
  console.log(`${json.lsAccountNum}: $${json.lsBalance}`);
}

await bridge.stop();
```
