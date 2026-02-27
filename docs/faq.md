# FAQ & Troubleshooting

Frequently asked questions about COBridge, along with solutions to common issues.

---

## General

### What is COBridge?

COBridge is an open-source Node.js library and CLI tool that reads COBOL copybook definitions and auto-generates everything modern applications need to communicate with COBOL programs: REST APIs, OpenAPI specifications, TypeScript types, JSON schemas, and binary data marshalling.

### Is COBridge free?

Yes. COBridge is completely free and open source.

### What licence is COBridge released under?

COBridge is released under the [MIT Licence](../LICENSE). You may use it in commercial and non-commercial projects without restriction, provided you include the copyright notice.

### Can I use COBridge in production?

COBridge is under active development (v0.1.x). The parser, code generation, and marshalling modules are well-tested (177 tests passing) and suitable for production use. The GnuCOBOL bridge and HTTP server are functional but should be evaluated against your organisation's requirements before deployment.

### Does COBridge replace my COBOL programs?

No. COBridge's entire philosophy is **"Don't rewrite your COBOL. Bridge it."** Your COBOL programs continue running exactly as they are. COBridge generates the glue code that lets modern applications talk to them.

---

## Parser

### What COBOL dialects are supported?

COBridge's parser handles standard COBOL copybook syntax as defined by the ANSI/ISO COBOL standard, which covers the vast majority of IBM mainframe, GnuCOBOL, and Micro Focus copybooks. It supports:

- Level numbers 01--49, 66, 77, 88
- PIC/PICTURE clauses with expansion shorthand (e.g. `9(5)`, `X(10)`)
- USAGE clauses: DISPLAY, COMP, COMP-1, COMP-2, COMP-3, BINARY, PACKED-DECIMAL
- OCCURS (fixed and variable with DEPENDING ON)
- REDEFINES
- VALUE clauses
- FILLER items
- Both fixed-format (columns 7--72) and free-format source

### Can it parse full COBOL programs or just copybooks?

COBridge is designed to parse **copybook definitions** (Data Division record layouts). It does not parse Procedure Division logic, PERFORM statements, or other procedural COBOL constructs. If you pass a full COBOL program, the parser will attempt to extract the data definitions but may produce unexpected results.

### What about COPY/REPLACE statements?

The parser does not resolve COPY or REPLACE directives. You should pre-process your copybooks to expand any COPY statements before passing them to COBridge. Most COBOL preprocessors (including GnuCOBOL's `cobc -E`) can do this.

### My copybook won't parse -- how do I debug it?

Try these steps:

1. **Check for syntax errors**: Ensure the copybook compiles cleanly with your COBOL compiler.
2. **Use the tokeniser**: Call `tokenize(source)` directly to see how COBridge breaks down the source. Look for unexpected token types.
3. **Simplify**: Remove fields one at a time until parsing succeeds, to isolate the problematic construct.
4. **Check formatting**: If using fixed-format, ensure code is in columns 7--72. Sequence numbers in columns 1--6 and anything past column 72 are stripped.
5. **File an issue**: If you believe the copybook is valid and COBridge should handle it, open a GitHub issue with a minimal reproducing example.

### Does it handle EBCDIC-encoded copybook files?

The parser expects copybook source as a UTF-8 or ASCII string. If your copybook file is EBCDIC-encoded, convert it to ASCII first. You can use the `ebcdicToAscii()` function from the marshalling module, or a tool like `iconv`:

```bash
iconv -f EBCDIC-US -t UTF-8 MY-COPYBOOK.cpy > MY-COPYBOOK-utf8.cpy
```

---

## Performance

### How fast is parsing?

Parsing is very fast. A typical copybook with 50--100 fields parses in under 1 millisecond on modern hardware. The parser operates in a single pass with O(n) complexity.

### How fast is marshalling?

Marshalling (JSON to COBOL buffer and back) is highly optimised and operates at buffer level. A typical record with 50 fields marshals in microseconds. Throughput of tens of thousands of records per second is achievable.

### Can it handle large copybooks (1000+ fields)?

Yes. The parser and marshaller are designed to handle copybooks of any practical size. Copybooks with over 1000 fields parse and marshal without issue. Memory usage scales linearly with the number of fields.

### Is there a size limit?

There is no hard-coded size limit. The practical limit is your system's available memory. A copybook would need tens of thousands of fields before memory became a concern.

---

## GnuCOBOL

### Do I need GnuCOBOL installed?

**It depends on what you need.** GnuCOBOL is only required if you want to:

- Compile COBOL source files (`compileCobol()`)
- Call COBOL programs at runtime (`callCobolProgram()`, `CobridgeProcess`)
- Use the `cobridge serve` command with live COBOL execution

The parser, code generation, and marshalling modules work **entirely without GnuCOBOL**.

### Which version of GnuCOBOL is required?

COBridge supports GnuCOBOL 3.x and later. Version 3.2 or later is recommended. You can check your installation with:

```bash
cobc --version
```

Or programmatically:

```ts
const info = await detectGnuCOBOL();
console.log(info.version);
```

### Can I use Micro Focus COBOL instead?

The bridge module is currently built around GnuCOBOL's `cobc` and `cobcrun` commands. Support for other COBOL runtimes (Micro Focus, IBM Enterprise COBOL) is planned but not yet available. The parser, code generation, and marshalling modules are entirely runtime-agnostic.

### How do I install GnuCOBOL on Windows?

The recommended approach is to use MSYS2:

```bash
# Install MSYS2 from https://www.msys2.org/
# Then in the MSYS2 terminal:
pacman -S mingw-w64-x86_64-gnucobol
```

Alternatively, prebuilt Windows binaries are available from the [GnuCOBOL project](https://gnucobol.sourceforge.io/).

### How do I install GnuCOBOL on macOS?

Use Homebrew:

```bash
brew install gnucobol
```

### How do I install GnuCOBOL on Linux?

Use your distribution's package manager:

```bash
# Debian / Ubuntu
sudo apt install gnucobol

# Fedora / RHEL
sudo dnf install gnucobol

# Arch Linux
sudo pacman -S gnucobol
```

---

## Integration

### Can I use COBridge with mainframe COBOL (z/OS)?

Yes, partially. The parser, code generation, and marshalling modules work with any COBOL copybook regardless of the target platform. You can:

- Parse z/OS copybooks and generate OpenAPI specs, TypeScript types, and JSON schemas
- Marshal data to and from mainframe-format buffers (including EBCDIC encoding)
- Use the generated schemas to validate API requests

However, the GnuCOBOL bridge module cannot directly call programs running on a z/OS mainframe. For that, you would need a middleware layer (e.g. CICS Transaction Gateway, MQ, or a custom TCP socket bridge).

### Can I use it with Docker?

Yes. COBridge runs anywhere Node.js runs. A minimal Dockerfile:

```dockerfile
FROM node:20-slim
RUN apt-get update && apt-get install -y gnucobol && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY package*.json ./
RUN npm ci --omit=dev
COPY . .

EXPOSE 3000
CMD ["npx", "cobridge", "serve", "--copybooks", "./copybooks"]
```

### Can I use it in a CI/CD pipeline?

Absolutely. Common CI/CD uses:

- **Parse copybooks and generate schemas** as part of your build process
- **Validate copybook changes** by running `cobridge parse` and checking the output
- **Generate TypeScript types** and commit them to your repository
- **Run the test suite** to verify your copybooks parse correctly

```yaml
# Example GitHub Actions step
- name: Generate API specs
  run: |
    npx cobridge codegen --copybook ./copybooks/CUSTOMER.cpy --openapi --output ./generated/
```

### How do I connect to an existing CICS system?

COBridge does not directly connect to CICS. However, you can use it in combination with a CICS integration tool:

1. Use COBridge to parse copybooks and generate schemas
2. Use a CICS connector (e.g. IBM CICS Transaction Gateway, or a CTG client library) to send and receive data
3. Use COBridge's marshalling functions to convert between JSON and COBOL binary format

---

## Troubleshooting

### "GnuCOBOL not found" error

This means COBridge could not locate the `cobc` compiler on your system PATH.

**Solutions:**

1. Verify GnuCOBOL is installed: `cobc --version`
2. If installed but not on PATH, add it:
   ```bash
   export PATH="/usr/local/bin:$PATH"  # adjust to your installation path
   ```
3. On Windows with MSYS2, ensure you are using the correct terminal or have added the MSYS2 `bin` directory to your system PATH.
4. Use `detectGnuCOBOL()` to diagnose programmatically:
   ```ts
   const info = await detectGnuCOBOL();
   console.log(info);
   ```

### "Failed to parse copybook" -- common causes

| Symptom                                | Likely cause                                                     |
|----------------------------------------|------------------------------------------------------------------|
| Unexpected token at line 1             | File has a BOM (byte order mark). Remove it or re-save as UTF-8 without BOM. |
| Level number errors                    | Mixing fixed-format and free-format conventions.                 |
| Missing PIC clause                     | Group item incorrectly treated as elementary. Check level numbers.|
| Unrecognised USAGE type                | Using a vendor-specific extension (e.g. COMP-5, COMP-X).        |
| Truncated fields                       | Fixed-format source extending past column 72.                    |

### "Buffer overflow during marshalling" -- REDEFINES issue

This typically happens when a REDEFINES field is larger than the field it redefines, or when the total record size calculation does not account for REDEFINES correctly.

**Solutions:**

1. Verify your copybook has correct REDEFINES relationships (the redefining field should not be larger than the redefined field).
2. Use `fromParserFields()` to convert parser output to marshaller input, which handles byte size calculations automatically.
3. If you are constructing `MarshalField` objects manually, ensure the `size` property is set correctly for REDEFINES fields.

### Server won't start -- port already in use

If you see `EADDRINUSE`, another process is already using the port.

**Solutions:**

1. Use a different port:
   ```bash
   cobridge serve --port 3001 --copybooks ./copybooks
   ```
2. Find and stop the process using the port:
   ```bash
   # Linux/macOS
   lsof -i :3000
   kill <PID>

   # Windows
   netstat -ano | findstr :3000
   taskkill /PID <PID> /F
   ```
3. Programmatically, pass a different port to `startServer()`:
   ```ts
   await startServer({ port: 3001, copybookDir: "./copybooks" });
   ```

### Packed decimal values are wrong

If `unpackDecimal()` returns unexpected values:

1. Check that `decimalDigits` matches the number of digits after `V` in the PIC clause (e.g. `PIC S9(5)V99` has `decimalDigits: 2`).
2. Ensure the buffer contains valid packed decimal data (each nibble 0--9, last nibble is sign: C=positive, D=negative, F=unsigned).
3. Verify the buffer length matches `floor(totalDigits / 2) + 1`.

### EBCDIC conversion produces garbled output

1. Check you are using the correct code page. The default is `"037"` (US/Canada). Common alternatives:
   - `"500"` -- International
   - `"1047"` -- Latin 1 / Open Systems
2. Ensure the input buffer is actually EBCDIC-encoded and not already ASCII or UTF-8.
3. If working with mainframe data, confirm which code page your mainframe uses with your systems team.
