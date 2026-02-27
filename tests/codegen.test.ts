/**
 * COBridge -- Code Generation Tests
 *
 * Tests OpenAPI generation, TypeScript type generation, JSON Schema
 * generation, and helper utilities from parsed COBOL copybooks.
 *
 * Run with: npx tsx tests/codegen.test.ts
 */

import { parseCopybook } from "../src/parser/parser.js";
import { generateOpenApiSpec, cobolNameToCamelCase, cobolNameToPascalCase } from "../src/codegen/openapi.js";
import { generateTypeScriptTypes } from "../src/codegen/typescript.js";
import { generateJsonSchema } from "../src/codegen/json-schema.js";

let passed = 0;
let failed = 0;

function assert(condition: boolean, message: string): void {
  if (condition) {
    passed++;
    console.log(`  PASS: ${message}`);
  } else {
    failed++;
    console.error(`  FAIL: ${message}`);
  }
}

function assertEqual(actual: unknown, expected: unknown, message: string): void {
  if (actual === expected) {
    passed++;
    console.log(`  PASS: ${message}`);
  } else {
    failed++;
    console.error(`  FAIL: ${message}`);
    console.error(`    expected: ${JSON.stringify(expected)}`);
    console.error(`    actual:   ${JSON.stringify(actual)}`);
  }
}

// ---------------------------------------------------------------------------
// Sample COBOL copybooks for testing
// ---------------------------------------------------------------------------

const SIMPLE_COPYBOOK = `
       01  CUSTOMER-RECORD.
           05  CUST-ID             PIC 9(8) COMP-3.
           05  CUST-FIRST-NAME     PIC X(25).
           05  CUST-LAST-NAME      PIC X(30).
           05  CUST-BALANCE        PIC S9(7)V99 COMP-3.
           05  CUST-TYPE           PIC X(1).
`;

const NESTED_COPYBOOK = `
       01  ORDER-RECORD.
           05  ORDER-ID            PIC 9(10).
           05  ORDER-DATE.
               10  ORDER-YEAR     PIC 9(4).
               10  ORDER-MONTH    PIC 9(2).
               10  ORDER-DAY      PIC 9(2).
           05  ORDER-TOTAL         PIC S9(7)V99 COMP-3.
`;

const OCCURS_COPYBOOK = `
       01  INVOICE-RECORD.
           05  INVOICE-ID          PIC 9(8).
           05  LINE-ITEM-COUNT     PIC 9(3).
           05  LINE-ITEMS OCCURS 10 TIMES.
               10  ITEM-CODE      PIC X(10).
               10  ITEM-QTY       PIC 9(5) COMP.
               10  ITEM-PRICE     PIC S9(5)V99 COMP-3.
`;

const NUMERIC_TYPES_COPYBOOK = `
       01  NUMERIC-TEST.
           05  DISPLAY-NUM         PIC 9(5).
           05  COMP-NUM            PIC 9(9) COMP.
           05  COMP3-NUM           PIC S9(7)V99 COMP-3.
           05  ALPHA-FIELD         PIC A(20).
           05  ALPHANUM-FIELD      PIC X(15).
`;

// ---------------------------------------------------------------------------
// COBOL name conversion tests
// ---------------------------------------------------------------------------

console.log("\n--- COBOL Name Conversion ---");

assertEqual(cobolNameToCamelCase("CUST-FIRST-NAME"), "custFirstName", "camelCase: CUST-FIRST-NAME -> custFirstName");
assertEqual(cobolNameToCamelCase("ORDER-ID"), "orderId", "camelCase: ORDER-ID -> orderId");
assertEqual(cobolNameToCamelCase("WS-ACCOUNT-NUMBER"), "wsAccountNumber", "camelCase: WS-ACCOUNT-NUMBER -> wsAccountNumber");
assertEqual(cobolNameToCamelCase("SIMPLE"), "simple", "camelCase: SIMPLE -> simple");

assertEqual(cobolNameToPascalCase("CUSTOMER-RECORD"), "CustomerRecord", "PascalCase: CUSTOMER-RECORD -> CustomerRecord");
assertEqual(cobolNameToPascalCase("ORDER-ID"), "OrderId", "PascalCase: ORDER-ID -> OrderId");
assertEqual(cobolNameToPascalCase("SIMPLE"), "Simple", "PascalCase: SIMPLE -> Simple");

// ---------------------------------------------------------------------------
// OpenAPI generation tests
// ---------------------------------------------------------------------------

console.log("\n--- OpenAPI Generation ---");

const simpleCopybook = parseCopybook(SIMPLE_COPYBOOK);
const spec = generateOpenApiSpec(simpleCopybook) as any;

assertEqual(spec.openapi, "3.0.3", "OpenAPI version is 3.0.3");
assert(spec.info !== undefined, "Spec has info section");
assert(spec.paths !== undefined, "Spec has paths section");
assert(spec.components !== undefined, "Spec has components section");

// Check that paths contain the expected endpoint
const paths = spec.paths;
const pathKeys = Object.keys(paths);
assert(pathKeys.length === 1, "Spec has one path");
assert(pathKeys[0].includes("customer-record"), "Path slug is customer-record");

// Check POST method exists
const postOp = paths[pathKeys[0]].post;
assert(postOp !== undefined, "POST operation exists");
assert(postOp.requestBody !== undefined, "POST has requestBody");
assert(postOp.responses["200"] !== undefined, "POST has 200 response");

// Check component schema — single 01-level group is unwrapped
const schemas = spec.components.schemas;
assert(schemas["CustomerRecord"] !== undefined, "Schema CustomerRecord exists");
const custSchema = schemas["CustomerRecord"];
assert(custSchema.properties !== undefined, "Schema has properties");
assert(custSchema.properties.custId !== undefined, "Schema has custId property");
assert(custSchema.properties.custFirstName !== undefined, "Schema has custFirstName property");

// Verify type mappings
assertEqual(custSchema.properties.custFirstName.type, "string", "PIC X -> string type");
assertEqual(custSchema.properties.custFirstName.maxLength, 25, "PIC X(25) maxLength is 25");
assertEqual(custSchema.properties.custId.type, "integer", "PIC 9 COMP-3 -> integer type");

// Check custom options
const specWithOptions = generateOpenApiSpec(simpleCopybook, { basePath: "/v2", title: "My API" }) as any;
assert(specWithOptions.info.title === "My API", "Custom title applied");
const v2PathKeys = Object.keys(specWithOptions.paths);
assert(v2PathKeys[0].startsWith("/v2/"), "Custom basePath applied");

// ---------------------------------------------------------------------------
// Nested group -> nested object mapping
// ---------------------------------------------------------------------------

console.log("\n--- Nested Group Mapping ---");

const nestedCopybook = parseCopybook(NESTED_COPYBOOK);
const nestedSpec = generateOpenApiSpec(nestedCopybook) as any;
const orderSchema = nestedSpec.components.schemas["OrderRecord"];

assert(orderSchema !== undefined, "OrderRecord schema exists");
assert(orderSchema.properties.orderDate !== undefined, "orderDate group property exists");
assertEqual(orderSchema.properties.orderDate.type, "object", "Group item maps to object type");
assert(orderSchema.properties.orderDate.properties !== undefined, "Nested group has properties");
assert(orderSchema.properties.orderDate.properties.orderYear !== undefined, "Nested field orderYear exists");
assert(orderSchema.properties.orderDate.properties.orderMonth !== undefined, "Nested field orderMonth exists");
assert(orderSchema.properties.orderDate.properties.orderDay !== undefined, "Nested field orderDay exists");

// ---------------------------------------------------------------------------
// OCCURS -> array mapping
// ---------------------------------------------------------------------------

console.log("\n--- OCCURS Array Mapping ---");

const occursCopybook = parseCopybook(OCCURS_COPYBOOK);
const occursSpec = generateOpenApiSpec(occursCopybook) as any;
const invoiceSchema = occursSpec.components.schemas["InvoiceRecord"];

assert(invoiceSchema !== undefined, "InvoiceRecord schema exists");
assert(invoiceSchema.properties.lineItems !== undefined, "lineItems property exists");
assertEqual(invoiceSchema.properties.lineItems.type, "array", "OCCURS maps to array type");
assertEqual(invoiceSchema.properties.lineItems.maxItems, 10, "OCCURS 10 TIMES -> maxItems 10");
assert(invoiceSchema.properties.lineItems.items !== undefined, "Array has items schema");
assertEqual(invoiceSchema.properties.lineItems.items.type, "object", "OCCURS group items -> object");
assert(invoiceSchema.properties.lineItems.items.properties.itemCode !== undefined, "Array item has itemCode");

// ---------------------------------------------------------------------------
// Numeric type mapping
// ---------------------------------------------------------------------------

console.log("\n--- Numeric Type Mapping ---");

const numCopybook = parseCopybook(NUMERIC_TYPES_COPYBOOK);
const numSpec = generateOpenApiSpec(numCopybook) as any;
const numSchema = numSpec.components.schemas["NumericTest"];

assert(numSchema !== undefined, "NumericTest schema exists");
// DISPLAY numeric -> string with pattern
assertEqual(numSchema.properties.displayNum.type, "string", "PIC 9 DISPLAY -> string");
assert(numSchema.properties.displayNum.pattern !== undefined, "DISPLAY numeric has pattern");
// COMP -> integer
assertEqual(numSchema.properties.compNum.type, "integer", "PIC 9 COMP -> integer");
// COMP-3 with decimals -> number
assertEqual(numSchema.properties.comp3Num.type, "number", "PIC S9V99 COMP-3 -> number");
// PIC A -> string
assertEqual(numSchema.properties.alphaField.type, "string", "PIC A -> string");
assertEqual(numSchema.properties.alphaField.maxLength, 20, "PIC A(20) maxLength is 20");
// PIC X -> string
assertEqual(numSchema.properties.alphanumField.type, "string", "PIC X -> string");

// ---------------------------------------------------------------------------
// TypeScript type generation
// ---------------------------------------------------------------------------

console.log("\n--- TypeScript Type Generation ---");

const tsOutput = generateTypeScriptTypes(simpleCopybook);

assert(tsOutput.includes("export interface CustomerRecord"), "Generates exported interface");
assert(tsOutput.includes("custId: number"), "COMP-3 field -> number type");
assert(tsOutput.includes("custFirstName: string"), "PIC X field -> string type");
assert(tsOutput.includes("custLastName: string"), "PIC X field -> string type");
assert(tsOutput.includes("custBalance: number"), "COMP-3 with decimals -> number");
assert(tsOutput.includes("PIC 9(8)"), "JSDoc includes PIC clause");
assert(tsOutput.includes("COBridge"), "Header mentions COBridge");

// Nested types
const nestedTs = generateTypeScriptTypes(nestedCopybook);
assert(nestedTs.includes("export interface OrderRecord"), "Nested: parent interface exists");
assert(nestedTs.includes("export interface OrderDate"), "Nested: child interface exists");
assert(nestedTs.includes("orderDate: OrderDate"), "Nested: parent references child type");

// OCCURS -> array
const occursTs = generateTypeScriptTypes(occursCopybook);
assert(occursTs.includes("lineItems: LineItems[]"), "OCCURS generates array type");

// ---------------------------------------------------------------------------
// JSON Schema generation
// ---------------------------------------------------------------------------

console.log("\n--- JSON Schema Generation ---");

const jsonSchema = generateJsonSchema(simpleCopybook) as any;

assertEqual(jsonSchema.$schema, "http://json-schema.org/draft-07/schema#", "Uses draft-07 schema");
assertEqual(jsonSchema.type, "object", "Root type is object");
assert(jsonSchema.properties !== undefined, "Has properties");

// Single 01-level group is unwrapped — custId is directly on the root
assert(jsonSchema.properties.custId !== undefined, "Has custId property (unwrapped)");
assert(jsonSchema.required !== undefined, "Has required array");

// Constraints on custId
const custIdSchema = jsonSchema.properties.custId;
assert(custIdSchema.maximum !== undefined, "COMP-3 integer has maximum constraint");
assert(custIdSchema.minimum !== undefined, "COMP-3 integer has minimum constraint");
assertEqual(custIdSchema.minimum, 0, "Unsigned COMP-3 has minimum 0");

const balanceSchema = jsonSchema.properties.custBalance;
assertEqual(balanceSchema.type, "number", "Decimal COMP-3 -> number");
assert(balanceSchema.minimum !== undefined, "Signed numeric has minimum");
assert((balanceSchema.minimum as number) < 0, "Signed numeric minimum is negative");

// Nested schema
const nestedJsonSchema = generateJsonSchema(nestedCopybook) as any;
assert(nestedJsonSchema.properties.orderDate !== undefined, "Nested group exists in JSON Schema");
assertEqual(nestedJsonSchema.properties.orderDate.type, "object", "Nested group -> object in JSON Schema");
assert(nestedJsonSchema.properties.orderDate.properties.orderYear !== undefined, "Nested field exists in JSON Schema");

// OCCURS schema
const occursJsonSchema = generateJsonSchema(occursCopybook) as any;
const lineItemsSchema = occursJsonSchema.properties.lineItems;
assertEqual(lineItemsSchema.type, "array", "OCCURS -> array in JSON Schema");
assertEqual(lineItemsSchema.maxItems, 10, "maxItems from OCCURS count");

// ---------------------------------------------------------------------------
// Summary
// ---------------------------------------------------------------------------

console.log(`\n--- Results: ${passed} passed, ${failed} failed ---\n`);
if (failed > 0) {
  process.exit(1);
}
