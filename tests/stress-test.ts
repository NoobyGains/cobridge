/**
 * COBridge -- Stress Test for COBOL Copybook Parser
 *
 * Reads every .cpy file from tests/stress-copybooks/,
 * runs each through the parser, codegen, and marshaller,
 * and reports detailed results including timing and errors.
 *
 * Run with: npx tsx tests/stress-test.ts
 */

import * as fs from "fs";
import * as path from "path";
import { parseCopybook } from "../src/parser/index.js";
import { generateOpenApiSpec, generateTypeScriptTypes } from "../src/codegen/index.js";
import { generateJsonSchema } from "../src/codegen/json-schema.js";
import {
  marshalToCobol,
  marshalFromCobol,
  fromParserFields,
  type MarshalField,
} from "../src/marshal/converter.js";
import type { Copybook, CopybookField } from "../src/parser/types.js";

// ---------------------------------------------------------------------------
// Result tracking
// ---------------------------------------------------------------------------

interface TestResult {
  file: string;
  parseTime: number;
  codegenTime: number;
  marshalTime: number;
  parsed: boolean;
  codegenOk: boolean;
  marshalOk: boolean;
  totalLength: number;
  fieldCount: number;
  errors: string[];
  warnings: string[];
}

const results: TestResult[] = [];

// ---------------------------------------------------------------------------
// Validation helpers
// ---------------------------------------------------------------------------

function countFields(fields: CopybookField[]): number {
  let count = 0;
  for (const f of fields) {
    count++;
    if (f.children.length > 0) {
      count += countFields(f.children);
    }
  }
  return count;
}

function validateField(
  field: CopybookField,
  parentPath: string,
  errors: string[],
  warnings: string[],
  totalLength: number
): void {
  const fieldPath = `${parentPath}.${field.name}`;

  // Every field must have a name
  if (!field.name || field.name.trim() === "") {
    errors.push(`${fieldPath}: field has no name`);
  }

  // Every elementary field must have byteLength > 0
  if (!field.isGroup && field.byteLength <= 0 && field.levelNumber !== 88) {
    // Exception: COMP-1 and COMP-2 fields without PIC should still have size
    if (field.usage === "COMP-1" || field.usage === "COMP-2") {
      // These are OK if they have byteLength == 4 or 8
      if (field.byteLength <= 0) {
        errors.push(
          `${fieldPath}: COMP-1/COMP-2 field has byteLength=${field.byteLength}`
        );
      }
    } else if (field.picture) {
      errors.push(
        `${fieldPath}: elementary field has byteLength=${field.byteLength} (PIC ${field.picture.raw})`
      );
    } else {
      warnings.push(
        `${fieldPath}: elementary field has no PIC and byteLength=${field.byteLength}`
      );
    }
  }

  // Group items should have children (unless they're level 88, 66, or 77)
  if (field.isGroup && field.children.length === 0) {
    errors.push(`${fieldPath}: group item has isGroup=true but no children`);
  }

  // startOffset should be non-negative
  if (field.startOffset < 0) {
    errors.push(
      `${fieldPath}: startOffset is negative (${field.startOffset})`
    );
  }

  // No field should have startOffset > totalLength (with exception for REDEFINES)
  if (!field.redefines && field.startOffset > totalLength && totalLength > 0) {
    errors.push(
      `${fieldPath}: startOffset (${field.startOffset}) > totalLength (${totalLength})`
    );
  }

  // PIC types should be valid
  if (field.picture) {
    const validTypes = ["alphanumeric", "numeric", "alpha"];
    if (!validTypes.includes(field.picture.type)) {
      errors.push(
        `${fieldPath}: invalid PIC type "${field.picture.type}" (PIC ${field.picture.raw})`
      );
    }
  }

  // Validate siblings have monotonically increasing offsets (for non-REDEFINES)
  if (field.isGroup && field.children.length > 1) {
    let lastOffset = -1;
    let lastNonRedefName = "";
    for (const child of field.children) {
      if (!child.redefines) {
        if (child.startOffset < lastOffset) {
          warnings.push(
            `${fieldPath}: sibling ${child.name} offset (${child.startOffset}) < previous sibling ${lastNonRedefName} offset (${lastOffset})`
          );
        }
        lastOffset = child.startOffset;
        lastNonRedefName = child.name;
      }
    }
  }

  // Recurse into children
  for (const child of field.children) {
    validateField(child, fieldPath, errors, warnings, totalLength);
  }
}

// ---------------------------------------------------------------------------
// Dummy JSON object generator for marshalling tests
// ---------------------------------------------------------------------------

function generateDummyJson(fields: CopybookField[]): Record<string, any> {
  const result: Record<string, any> = {};

  for (const field of fields) {
    const key = field.name
      .toLowerCase()
      .replace(/-([a-z0-9])/g, (_, c: string) => c.toUpperCase());

    if (field.levelNumber === 88) continue;

    if (field.isGroup) {
      if (field.occurs) {
        const arr: any[] = [];
        for (let i = 0; i < Math.min(field.occurs.count, 2); i++) {
          arr.push(generateDummyJson(field.children));
        }
        result[key] = arr;
      } else {
        result[key] = generateDummyJson(field.children);
      }
    } else {
      if (field.occurs) {
        const arr: any[] = [];
        for (let i = 0; i < Math.min(field.occurs.count, 2); i++) {
          arr.push(getDummyValue(field));
        }
        result[key] = arr;
      } else {
        result[key] = getDummyValue(field);
      }
    }
  }

  return result;
}

function getDummyValue(field: CopybookField): any {
  if (!field.picture) {
    if (field.usage === "COMP-1" || field.usage === "COMP-2") {
      return 1.5;
    }
    return "";
  }

  const pic = field.picture;
  if (pic.type === "alphanumeric" || pic.type === "alpha") {
    return "A".repeat(Math.min(pic.length, 5));
  }

  // Numeric -- generate a value that fits within the PIC clause capacity
  const intDigits = pic.length - pic.decimals;
  if (pic.decimals > 0) {
    // Generate value that fits: e.g., for PIC 9V9(4) (1 int digit, 4 dec),
    // use something like 1.2345 not 12.34
    const intPart = Math.min(1, Math.pow(10, intDigits) - 1);
    const decPart = 5 / Math.pow(10, pic.decimals);
    return intPart + decPart;
  }
  // Integer field -- fit within capacity
  return Math.min(100, Math.pow(10, intDigits) - 1);
}

// ---------------------------------------------------------------------------
// Main stress test
// ---------------------------------------------------------------------------

function runStressTest(): void {
  const cpyDir = path.join(__dirname, "stress-copybooks");

  if (!fs.existsSync(cpyDir)) {
    console.error(`ERROR: Directory not found: ${cpyDir}`);
    process.exit(1);
  }

  const files = fs
    .readdirSync(cpyDir)
    .filter((f) => f.endsWith(".cpy"))
    .sort();

  if (files.length === 0) {
    console.error("ERROR: No .cpy files found in stress-copybooks/");
    process.exit(1);
  }

  console.log("=".repeat(72));
  console.log("  COBridge STRESS TEST - COBOL Copybook Parser");
  console.log("=".repeat(72));
  console.log(`  Found ${files.length} copybook files to test\n`);

  for (const file of files) {
    const filePath = path.join(cpyDir, file);
    const source = fs.readFileSync(filePath, "utf-8");

    const result: TestResult = {
      file,
      parseTime: 0,
      codegenTime: 0,
      marshalTime: 0,
      parsed: false,
      codegenOk: false,
      marshalOk: false,
      totalLength: 0,
      fieldCount: 0,
      errors: [],
      warnings: [],
    };

    // ---- Step 1: Parse ----
    let copybook: Copybook | null = null;
    try {
      const start = performance.now();
      copybook = parseCopybook(source);
      result.parseTime = performance.now() - start;
      result.parsed = true;
      result.totalLength = copybook.totalLength;
      result.fieldCount = countFields(copybook.fields);

      // Validate
      if (copybook.totalLength <= 0) {
        result.errors.push(`totalLength is ${copybook.totalLength} (expected > 0)`);
      }

      for (const field of copybook.fields) {
        validateField(
          field,
          copybook.name,
          result.errors,
          result.warnings,
          copybook.totalLength
        );
      }
    } catch (e: any) {
      result.errors.push(`PARSE ERROR: ${e.message}`);
    }

    // ---- Step 2: Codegen ----
    if (copybook) {
      try {
        const start = performance.now();

        const openapi = generateOpenApiSpec(copybook);
        if (!openapi || !openapi.openapi) {
          result.errors.push("OpenAPI spec missing 'openapi' field");
        }
        if (!openapi.components) {
          result.errors.push("OpenAPI spec missing 'components'");
        }

        const tsTypes = generateTypeScriptTypes(copybook);
        if (!tsTypes || tsTypes.length === 0) {
          result.errors.push("TypeScript types output is empty");
        }

        const jsonSchema = generateJsonSchema(copybook);
        if (!jsonSchema || !jsonSchema.$schema) {
          result.errors.push("JSON Schema missing '$schema' field");
        }

        result.codegenTime = performance.now() - start;
        result.codegenOk = true;
      } catch (e: any) {
        result.errors.push(`CODEGEN ERROR: ${e.message}`);
      }
    }

    // ---- Step 3: Marshal round-trip ----
    if (copybook && copybook.totalLength > 0) {
      try {
        const start = performance.now();

        // Convert parser fields to marshal fields
        const marshalFields = fromParserFields(copybook.fields);

        // Generate dummy data
        const dummyJson = generateDummyJson(copybook.fields);

        // Marshal to COBOL buffer
        const buffer = marshalToCobol(dummyJson, marshalFields);

        if (buffer.length === 0) {
          result.errors.push("Marshal produced empty buffer");
        }

        // Marshal back from COBOL buffer
        const roundTripped = marshalFromCobol(buffer, marshalFields);

        if (!roundTripped || typeof roundTripped !== "object") {
          result.errors.push("Unmarshal produced invalid result");
        }

        result.marshalTime = performance.now() - start;
        result.marshalOk = true;
      } catch (e: any) {
        result.errors.push(`MARSHAL ERROR: ${e.message}`);
      }
    }

    results.push(result);
  }

  // ---------------------------------------------------------------------------
  // Print report
  // ---------------------------------------------------------------------------

  console.log("-".repeat(72));
  console.log("  DETAILED RESULTS");
  console.log("-".repeat(72));

  let totalParsed = 0;
  let totalCodegen = 0;
  let totalMarshal = 0;
  let totalErrors = 0;
  let totalWarnings = 0;

  for (const r of results) {
    const status = r.parsed ? (r.errors.length === 0 ? "PASS" : "WARN") : "FAIL";
    const icon = status === "PASS" ? "[OK]" : status === "WARN" ? "[!!]" : "[XX]";

    console.log(`\n  ${icon} ${r.file}`);
    console.log(
      `      Parse: ${r.parsed ? "OK" : "FAIL"} (${r.parseTime.toFixed(2)}ms) | ` +
      `Fields: ${r.fieldCount} | Length: ${r.totalLength} bytes`
    );
    console.log(
      `      Codegen: ${r.codegenOk ? "OK" : "FAIL"} (${r.codegenTime.toFixed(2)}ms) | ` +
      `Marshal: ${r.marshalOk ? "OK" : "FAIL"} (${r.marshalTime.toFixed(2)}ms)`
    );

    if (r.errors.length > 0) {
      console.log(`      ERRORS (${r.errors.length}):`);
      for (const err of r.errors) {
        console.log(`        - ${err}`);
      }
    }

    if (r.warnings.length > 0) {
      console.log(`      WARNINGS (${r.warnings.length}):`);
      for (const warn of r.warnings) {
        console.log(`        - ${warn}`);
      }
    }

    if (r.parsed) totalParsed++;
    if (r.codegenOk) totalCodegen++;
    if (r.marshalOk) totalMarshal++;
    totalErrors += r.errors.length;
    totalWarnings += r.warnings.length;
  }

  // ---------------------------------------------------------------------------
  // Summary
  // ---------------------------------------------------------------------------

  console.log("\n" + "=".repeat(72));
  console.log("  SUMMARY");
  console.log("=".repeat(72));
  console.log(`  Total copybooks:  ${results.length}`);
  console.log(`  Parsed OK:        ${totalParsed}/${results.length}`);
  console.log(`  Codegen OK:       ${totalCodegen}/${results.length}`);
  console.log(`  Marshal OK:       ${totalMarshal}/${results.length}`);
  console.log(`  Total errors:     ${totalErrors}`);
  console.log(`  Total warnings:   ${totalWarnings}`);

  const totalParseTime = results.reduce((s, r) => s + r.parseTime, 0);
  const totalCodegenTime = results.reduce((s, r) => s + r.codegenTime, 0);
  const totalMarshalTime = results.reduce((s, r) => s + r.marshalTime, 0);
  console.log(
    `  Total parse time:   ${totalParseTime.toFixed(2)}ms (avg ${(totalParseTime / results.length).toFixed(2)}ms)`
  );
  console.log(
    `  Total codegen time: ${totalCodegenTime.toFixed(2)}ms (avg ${(totalCodegenTime / results.length).toFixed(2)}ms)`
  );
  console.log(
    `  Total marshal time: ${totalMarshalTime.toFixed(2)}ms (avg ${(totalMarshalTime / results.length).toFixed(2)}ms)`
  );
  console.log("=".repeat(72));

  // Detailed bug report
  const failures = results.filter((r) => r.errors.length > 0);
  if (failures.length > 0) {
    console.log("\n" + "!".repeat(72));
    console.log("  BUGS FOUND - DETAILED FAILURE ANALYSIS");
    console.log("!".repeat(72));
    for (const r of failures) {
      console.log(`\n  FILE: ${r.file}`);
      for (const err of r.errors) {
        console.log(`    BUG: ${err}`);
      }
    }
    console.log("\n" + "!".repeat(72));
  }

  if (totalErrors > 0) {
    process.exit(1);
  } else {
    console.log("\n  ALL TESTS PASSED!\n");
  }
}

runStressTest();
