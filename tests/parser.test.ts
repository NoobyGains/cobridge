/**
 * COBridge — COBOL Copybook Parser Tests
 *
 * Comprehensive tests using basic assert/console.log.
 * Run with: npx tsx tests/parser.test.ts
 */

import * as assert from "assert";
import { parseCopybook, parsePicString, calculateByteLength, tokenize, UsageType } from "../src/parser/index.js";

let passed = 0;
let failed = 0;

function test(name: string, fn: () => void): void {
  try {
    fn();
    passed++;
    console.log(`  PASS: ${name}`);
  } catch (e: any) {
    failed++;
    console.log(`  FAIL: ${name}`);
    console.log(`        ${e.message}`);
  }
}

// =========================================================================
// PIC string parsing
// =========================================================================
console.log("\n--- PIC String Parsing ---");

test("PIC X(10) -> alphanumeric, length 10", () => {
  const pic = parsePicString("X(10)");
  assert.strictEqual(pic.type, "alphanumeric");
  assert.strictEqual(pic.length, 10);
  assert.strictEqual(pic.decimals, 0);
  assert.strictEqual(pic.signed, false);
});

test("PIC 9(5) -> numeric, length 5", () => {
  const pic = parsePicString("9(5)");
  assert.strictEqual(pic.type, "numeric");
  assert.strictEqual(pic.length, 5);
  assert.strictEqual(pic.decimals, 0);
});

test("PIC S9(7)V99 -> signed numeric, length 9, 2 decimals", () => {
  const pic = parsePicString("S9(7)V99");
  assert.strictEqual(pic.type, "numeric");
  assert.strictEqual(pic.length, 9);
  assert.strictEqual(pic.decimals, 2);
  assert.strictEqual(pic.signed, true);
});

test("PIC S9(7)V9(2) -> signed numeric, length 9, 2 decimals", () => {
  const pic = parsePicString("S9(7)V9(2)");
  assert.strictEqual(pic.type, "numeric");
  assert.strictEqual(pic.length, 9);
  assert.strictEqual(pic.decimals, 2);
  assert.strictEqual(pic.signed, true);
});

test("PIC A(20) -> alpha, length 20", () => {
  const pic = parsePicString("A(20)");
  assert.strictEqual(pic.type, "alpha");
  assert.strictEqual(pic.length, 20);
});

test("PIC Z(5)9.99 -> numeric edited, length 9", () => {
  const pic = parsePicString("Z(5)9.99");
  assert.strictEqual(pic.type, "numeric");
  // Z(5) = 5, 9 = 1, . = 1, 99 = 2 => total 9
  assert.strictEqual(pic.length, 9);
  assert.strictEqual(pic.decimals, 2);
});

test("PIC 99/99/9999 -> numeric edited with slashes", () => {
  const pic = parsePicString("99/99/9999");
  assert.strictEqual(pic.type, "numeric");
  // 2 + 1 + 2 + 1 + 4 = 10
  assert.strictEqual(pic.length, 10);
});

// =========================================================================
// Byte length calculation
// =========================================================================
console.log("\n--- Byte Length Calculation ---");

test("DISPLAY: PIC X(10) = 10 bytes", () => {
  const pic = parsePicString("X(10)");
  assert.strictEqual(calculateByteLength(pic, UsageType.DISPLAY), 10);
});

test("DISPLAY: PIC 9(5) = 5 bytes", () => {
  const pic = parsePicString("9(5)");
  assert.strictEqual(calculateByteLength(pic, UsageType.DISPLAY), 5);
});

test("COMP-3: PIC 9(5) = ceil((5+1)/2) = 3 bytes", () => {
  const pic = parsePicString("9(5)");
  assert.strictEqual(calculateByteLength(pic, UsageType.COMP_3), 3);
});

test("COMP-3: PIC S9(7)V99 = ceil((9+1)/2) = 5 bytes", () => {
  const pic = parsePicString("S9(7)V99");
  assert.strictEqual(calculateByteLength(pic, UsageType.COMP_3), 5);
});

test("COMP-3: PIC S9(7)V9(2) = ceil((9+1)/2) = 5 bytes", () => {
  const pic = parsePicString("S9(7)V9(2)");
  assert.strictEqual(calculateByteLength(pic, UsageType.COMP_3), 5);
});

test("COMP: PIC 9(4) = 2 bytes (1-4 digits)", () => {
  const pic = parsePicString("9(4)");
  assert.strictEqual(calculateByteLength(pic, UsageType.COMP), 2);
});

test("COMP: PIC 9(5) = 4 bytes (5-9 digits)", () => {
  const pic = parsePicString("9(5)");
  assert.strictEqual(calculateByteLength(pic, UsageType.COMP), 4);
});

test("COMP: PIC 9(10) = 8 bytes (10-18 digits)", () => {
  const pic = parsePicString("9(10)");
  assert.strictEqual(calculateByteLength(pic, UsageType.COMP), 8);
});

test("BINARY: PIC 9(9) = 4 bytes (5-9 digits)", () => {
  const pic = parsePicString("9(9)");
  assert.strictEqual(calculateByteLength(pic, UsageType.BINARY), 4);
});

test("COMP-1: always 4 bytes (single float)", () => {
  const pic = parsePicString("9(1)");
  assert.strictEqual(calculateByteLength(pic, UsageType.COMP_1), 4);
});

test("COMP-2: always 8 bytes (double float)", () => {
  const pic = parsePicString("9(1)");
  assert.strictEqual(calculateByteLength(pic, UsageType.COMP_2), 8);
});

test("PACKED-DECIMAL: PIC 9(3) = ceil((3+1)/2) = 2 bytes", () => {
  const pic = parsePicString("9(3)");
  assert.strictEqual(calculateByteLength(pic, UsageType.PACKED_DECIMAL), 2);
});

// =========================================================================
// Tokenizer
// =========================================================================
console.log("\n--- Tokenizer ---");

test("Tokenizes simple field declaration", () => {
  const tokens = tokenize("05 WS-NAME PIC X(20).");
  // Should have: LEVEL_NUMBER, DATA_NAME, PIC, PIC_STRING, PERIOD, EOF
  assert.strictEqual(tokens[0].type, "LEVEL_NUMBER");
  assert.strictEqual(tokens[0].value, "05");
  assert.strictEqual(tokens[1].type, "DATA_NAME");
  assert.strictEqual(tokens[1].value, "WS-NAME");
  assert.strictEqual(tokens[2].type, "PIC");
  assert.strictEqual(tokens[3].type, "PIC_STRING");
  assert.strictEqual(tokens[3].value, "X(20)");
  assert.strictEqual(tokens[4].type, "PERIOD");
});

test("Tokenizes COMP-3 usage", () => {
  const tokens = tokenize("05 WS-AMOUNT PIC S9(7)V99 COMP-3.");
  const usageToken = tokens.find((t) => t.type === "USAGE_TYPE");
  assert.ok(usageToken);
  assert.strictEqual(usageToken!.value, "COMP-3");
});

test("Tokenizes OCCURS clause", () => {
  const tokens = tokenize("05 WS-ITEM OCCURS 10 TIMES PIC X(5).");
  const occursToken = tokens.find((t) => t.type === "OCCURS");
  assert.ok(occursToken);
  const numToken = tokens.find((t) => t.type === "NUMBER");
  assert.ok(numToken);
  assert.strictEqual(numToken!.value, "10");
});

test("Tokenizes REDEFINES clause", () => {
  const tokens = tokenize("05 WS-ALT REDEFINES WS-ORIGINAL PIC X(10).");
  const redefToken = tokens.find((t) => t.type === "REDEFINES");
  assert.ok(redefToken);
});

test("Skips comment lines in fixed format", () => {
  const source = [
    "      * This is a comment",
    "       05 WS-FIELD PIC X(5).",
  ].join("\n");
  const tokens = tokenize(source);
  // Should not contain any token from the comment line
  const fieldTokens = tokens.filter((t) => t.type !== "EOF");
  assert.ok(fieldTokens.length > 0);
  assert.strictEqual(fieldTokens[0].type, "LEVEL_NUMBER");
});

// =========================================================================
// Simple flat record
// =========================================================================
console.log("\n--- Simple Flat Record ---");

test("Parses a simple flat record", () => {
  const source = `
       01 CUSTOMER-RECORD.
          05 CUST-ID          PIC 9(8).
          05 CUST-NAME        PIC X(30).
          05 CUST-BALANCE     PIC S9(7)V99.
  `;
  const result = parseCopybook(source);

  assert.strictEqual(result.name, "CUSTOMER-RECORD");
  assert.strictEqual(result.fields.length, 1);

  const record = result.fields[0];
  assert.strictEqual(record.isGroup, true);
  assert.strictEqual(record.children.length, 3);

  const id = record.children[0];
  assert.strictEqual(id.name, "CUST-ID");
  assert.strictEqual(id.byteLength, 8);
  assert.strictEqual(id.startOffset, 0);

  const name = record.children[1];
  assert.strictEqual(name.name, "CUST-NAME");
  assert.strictEqual(name.byteLength, 30);
  assert.strictEqual(name.startOffset, 8);

  const bal = record.children[2];
  assert.strictEqual(bal.name, "CUST-BALANCE");
  assert.strictEqual(bal.byteLength, 9);
  assert.strictEqual(bal.startOffset, 38);

  // Total: 8 + 30 + 9 = 47
  assert.strictEqual(record.byteLength, 47);
  assert.strictEqual(result.totalLength, 47);
});

// =========================================================================
// Nested group items
// =========================================================================
console.log("\n--- Nested Group Items ---");

test("Parses nested group items", () => {
  const source = `
       01 ACCOUNT-RECORD.
          05 ACCT-HEADER.
             10 ACCT-NUMBER     PIC 9(10).
             10 ACCT-TYPE       PIC X(2).
          05 ACCT-DETAILS.
             10 ACCT-OPEN-DATE.
                15 ACCT-YEAR    PIC 9(4).
                15 ACCT-MONTH   PIC 9(2).
                15 ACCT-DAY     PIC 9(2).
             10 ACCT-STATUS     PIC X(1).
  `;
  const result = parseCopybook(source);

  const record = result.fields[0];
  assert.strictEqual(record.children.length, 2);

  const header = record.children[0];
  assert.strictEqual(header.name, "ACCT-HEADER");
  assert.strictEqual(header.isGroup, true);
  assert.strictEqual(header.children.length, 2);
  // 10 + 2 = 12
  assert.strictEqual(header.byteLength, 12);

  const details = record.children[1];
  assert.strictEqual(details.name, "ACCT-DETAILS");
  assert.strictEqual(details.isGroup, true);
  assert.strictEqual(details.children.length, 2);

  const openDate = details.children[0];
  assert.strictEqual(openDate.name, "ACCT-OPEN-DATE");
  assert.strictEqual(openDate.isGroup, true);
  assert.strictEqual(openDate.children.length, 3);
  // 4 + 2 + 2 = 8
  assert.strictEqual(openDate.byteLength, 8);

  const status = details.children[1];
  assert.strictEqual(status.name, "ACCT-STATUS");
  assert.strictEqual(status.byteLength, 1);

  // details = 8 + 1 = 9
  assert.strictEqual(details.byteLength, 9);

  // total = 12 + 9 = 21
  assert.strictEqual(result.totalLength, 21);
});

// =========================================================================
// COMP-3 packed decimal fields (banking scenario)
// =========================================================================
console.log("\n--- COMP-3 Packed Decimal ---");

test("Parses COMP-3 packed decimal fields", () => {
  const source = `
       01 TRANSACTION-RECORD.
          05 TXN-ID            PIC 9(12).
          05 TXN-AMOUNT        PIC S9(7)V99 COMP-3.
          05 TXN-FEE           PIC S9(5)V99 COMP-3.
          05 TXN-BALANCE       PIC S9(11)V99 COMP-3.
  `;
  const result = parseCopybook(source);
  const record = result.fields[0];

  const txnId = record.children[0];
  assert.strictEqual(txnId.byteLength, 12);
  assert.strictEqual(txnId.usage, "DISPLAY");

  const amount = record.children[1];
  // S9(7)V99 = 9 digit positions, COMP-3 = ceil((9+1)/2) = 5
  assert.strictEqual(amount.byteLength, 5);
  assert.strictEqual(amount.usage, "COMP-3");
  assert.strictEqual(amount.picture!.signed, true);
  assert.strictEqual(amount.picture!.decimals, 2);

  const fee = record.children[2];
  // S9(5)V99 = 7 digit positions, COMP-3 = ceil((7+1)/2) = 4
  assert.strictEqual(fee.byteLength, 4);

  const balance = record.children[3];
  // S9(11)V99 = 13 digit positions, COMP-3 = ceil((13+1)/2) = 7
  assert.strictEqual(balance.byteLength, 7);

  // total = 12 + 5 + 4 + 7 = 28
  assert.strictEqual(result.totalLength, 28);
});

// =========================================================================
// OCCURS arrays
// =========================================================================
console.log("\n--- OCCURS Arrays ---");

test("Parses OCCURS with simple repeat", () => {
  const source = `
       01 MONTHLY-REPORT.
          05 REPORT-HEADER     PIC X(20).
          05 MONTHLY-DATA OCCURS 12 TIMES.
             10 MONTH-NAME     PIC X(10).
             10 MONTH-TOTAL    PIC S9(9)V99 COMP-3.
  `;
  const result = parseCopybook(source);
  const record = result.fields[0];

  const header = record.children[0];
  assert.strictEqual(header.byteLength, 20);

  const monthlyData = record.children[1];
  assert.ok(monthlyData.occurs);
  assert.strictEqual(monthlyData.occurs!.count, 12);
  assert.strictEqual(monthlyData.isGroup, true);

  const monthName = monthlyData.children[0];
  assert.strictEqual(monthName.byteLength, 10);

  const monthTotal = monthlyData.children[1];
  // S9(9)V99 = 11 positions, COMP-3 = ceil((11+1)/2) = 6
  assert.strictEqual(monthTotal.byteLength, 6);

  // Group element size = 10 + 6 = 16
  assert.strictEqual(monthlyData.byteLength, 16);

  // Total = 20 + (16 * 12) = 20 + 192 = 212
  assert.strictEqual(result.totalLength, 212);
});

test("Parses OCCURS DEPENDING ON (variable length)", () => {
  const source = `
       01 ORDER-RECORD.
          05 ORDER-ID          PIC 9(8).
          05 LINE-COUNT        PIC 9(3).
          05 ORDER-LINE OCCURS 1 TO 50 DEPENDING ON LINE-COUNT.
             10 ITEM-CODE      PIC X(10).
             10 ITEM-QTY       PIC 9(5).
  `;
  const result = parseCopybook(source);
  const record = result.fields[0];

  const lineCount = record.children[1];
  assert.strictEqual(lineCount.byteLength, 3);

  const orderLine = record.children[2];
  assert.ok(orderLine.occurs);
  assert.strictEqual(orderLine.occurs!.count, 50); // max
  assert.strictEqual(orderLine.occurs!.min, 1);
  assert.strictEqual(orderLine.occurs!.dependingOn, "LINE-COUNT");

  // Each line = 10 + 5 = 15
  assert.strictEqual(orderLine.byteLength, 15);

  // Max total = 8 + 3 + (15 * 50) = 761
  assert.strictEqual(result.totalLength, 761);
});

// =========================================================================
// REDEFINES
// =========================================================================
console.log("\n--- REDEFINES ---");

test("Parses REDEFINES with overlapping storage", () => {
  const source = `
       01 DATE-RECORD.
          05 DATE-FORMATTED    PIC X(10).
          05 DATE-PARTS REDEFINES DATE-FORMATTED.
             10 DATE-YEAR      PIC 9(4).
             10 DATE-SEP1      PIC X(1).
             10 DATE-MONTH     PIC 9(2).
             10 DATE-SEP2      PIC X(1).
             10 DATE-DAY       PIC 9(2).
  `;
  const result = parseCopybook(source);
  const record = result.fields[0];

  const formatted = record.children[0];
  assert.strictEqual(formatted.byteLength, 10);
  assert.strictEqual(formatted.startOffset, 0);

  const parts = record.children[1];
  assert.strictEqual(parts.redefines, "DATE-FORMATTED");
  assert.strictEqual(parts.isGroup, true);
  // REDEFINES should share offset with the field it redefines
  assert.strictEqual(parts.startOffset, 0);
  // Group length = 4 + 1 + 2 + 1 + 2 = 10
  assert.strictEqual(parts.byteLength, 10);

  // Total should be 10 (REDEFINES does not add to total)
  assert.strictEqual(result.totalLength, 10);
});

// =========================================================================
// Complex banking copybook
// =========================================================================
console.log("\n--- Complex Banking Copybook ---");

test("Parses a realistic banking copybook", () => {
  const source = `
      * ============================================================
      * BANK ACCOUNT MASTER FILE RECORD LAYOUT
      * ============================================================
       01 BANK-ACCOUNT-RECORD.
          05 BA-HEADER.
             10 BA-RECORD-TYPE       PIC X(2).
             10 BA-ACCOUNT-NUMBER    PIC 9(10).
             10 BA-BRANCH-CODE       PIC 9(4).
          05 BA-CUSTOMER-INFO.
             10 BA-CUST-NAME.
                15 BA-FIRST-NAME     PIC X(20).
                15 BA-LAST-NAME      PIC X(30).
             10 BA-CUST-SSN          PIC 9(9).
             10 BA-CUST-DOB          PIC 9(8).
          05 BA-ACCOUNT-DATA.
             10 BA-CURRENT-BALANCE   PIC S9(11)V99 COMP-3.
             10 BA-AVAILABLE-BAL     PIC S9(11)V99 COMP-3.
             10 BA-HOLD-AMOUNT       PIC S9(9)V99  COMP-3.
             10 BA-INTEREST-RATE     PIC 9(2)V9(4) COMP-3.
             10 BA-LAST-TXN-DATE     PIC 9(8).
             10 BA-LAST-TXN-AMOUNT   PIC S9(9)V99  COMP-3.
          05 BA-HISTORY OCCURS 6 TIMES.
             10 BA-HIST-MONTH        PIC 9(6).
             10 BA-HIST-DEBITS       PIC S9(11)V99 COMP-3.
             10 BA-HIST-CREDITS      PIC S9(11)V99 COMP-3.
             10 BA-HIST-FEES         PIC S9(7)V99  COMP-3.
             10 BA-HIST-INTEREST     PIC S9(7)V99  COMP-3.
  `;
  const result = parseCopybook(source);
  assert.strictEqual(result.name, "BANK-ACCOUNT-RECORD");

  const record = result.fields[0];
  assert.strictEqual(record.children.length, 4);

  // BA-HEADER: 2 + 10 + 4 = 16
  const header = record.children[0];
  assert.strictEqual(header.name, "BA-HEADER");
  assert.strictEqual(header.byteLength, 16);

  // BA-CUSTOMER-INFO: (20 + 30) + 9 + 8 = 67
  const custInfo = record.children[1];
  assert.strictEqual(custInfo.name, "BA-CUSTOMER-INFO");
  assert.strictEqual(custInfo.byteLength, 67);
  assert.strictEqual(custInfo.startOffset, 16);

  // BA-ACCOUNT-DATA:
  //   S9(11)V99 COMP-3 = ceil((13+1)/2) = 7
  //   S9(11)V99 COMP-3 = 7
  //   S9(9)V99 COMP-3  = ceil((11+1)/2) = 6
  //   9(2)V9(4) COMP-3 = ceil((6+1)/2) = 4 (note: 2+4=6 digit positions)
  //   9(8) DISPLAY = 8
  //   S9(9)V99 COMP-3  = 6
  //   Total = 7 + 7 + 6 + 4 + 8 + 6 = 38
  const acctData = record.children[2];
  assert.strictEqual(acctData.name, "BA-ACCOUNT-DATA");
  assert.strictEqual(acctData.byteLength, 38);
  assert.strictEqual(acctData.startOffset, 83); // 16 + 67

  // BA-HISTORY (OCCURS 6):
  //   9(6) DISPLAY = 6
  //   S9(11)V99 COMP-3 = 7
  //   S9(11)V99 COMP-3 = 7
  //   S9(7)V99 COMP-3  = ceil((9+1)/2) = 5
  //   S9(7)V99 COMP-3  = 5
  //   Each = 6 + 7 + 7 + 5 + 5 = 30
  const history = record.children[3];
  assert.strictEqual(history.name, "BA-HISTORY");
  assert.ok(history.occurs);
  assert.strictEqual(history.occurs!.count, 6);
  assert.strictEqual(history.byteLength, 30);
  assert.strictEqual(history.startOffset, 121); // 16 + 67 + 38

  // Total = 16 + 67 + 38 + (30 * 6) = 121 + 180 = 301
  assert.strictEqual(result.totalLength, 301);
});

// =========================================================================
// USAGE IS syntax
// =========================================================================
console.log("\n--- USAGE IS Syntax ---");

test("Parses USAGE IS COMP-3 syntax", () => {
  const source = `
       01 TEST-RECORD.
          05 FIELD-A PIC S9(5)V99 USAGE IS COMP-3.
          05 FIELD-B PIC 9(4) USAGE IS BINARY.
  `;
  const result = parseCopybook(source);
  const record = result.fields[0];

  assert.strictEqual(record.children[0].usage, "COMP-3");
  assert.strictEqual(record.children[1].usage, "BINARY");
});

// =========================================================================
// FILLER fields
// =========================================================================
console.log("\n--- FILLER Fields ---");

test("Parses FILLER fields", () => {
  const source = `
       01 PADDED-RECORD.
          05 REC-DATA          PIC X(10).
          05 FILLER            PIC X(5).
          05 REC-CODE          PIC 9(3).
  `;
  const result = parseCopybook(source);
  const record = result.fields[0];

  assert.strictEqual(record.children.length, 3);
  assert.strictEqual(record.children[1].name, "FILLER");
  assert.strictEqual(record.children[1].byteLength, 5);
  assert.strictEqual(record.children[1].startOffset, 10);
  assert.strictEqual(record.children[2].startOffset, 15);

  // Total = 10 + 5 + 3 = 18
  assert.strictEqual(result.totalLength, 18);
});

// =========================================================================
// Level 77 (independent items)
// =========================================================================
console.log("\n--- Level 77 ---");

test("Parses level 77 independent items", () => {
  const source = `
       77 WS-COUNTER          PIC 9(5).
       77 WS-FLAG             PIC X(1).
  `;
  const result = parseCopybook(source);

  assert.strictEqual(result.fields.length, 2);
  assert.strictEqual(result.fields[0].name, "WS-COUNTER");
  assert.strictEqual(result.fields[0].levelNumber, 77);
  assert.strictEqual(result.fields[0].byteLength, 5);
  assert.strictEqual(result.fields[1].name, "WS-FLAG");
  assert.strictEqual(result.fields[1].byteLength, 1);
  assert.strictEqual(result.totalLength, 6);
});

// =========================================================================
// VALUE clauses (should be parsed but not affect layout)
// =========================================================================
console.log("\n--- VALUE Clauses ---");

test("Ignores VALUE clauses in layout calculation", () => {
  const source = `
       01 INIT-RECORD.
          05 STATUS-CODE       PIC X(2) VALUE 'OK'.
          05 ERROR-COUNT       PIC 9(4) VALUE 0.
  `;
  const result = parseCopybook(source);
  const record = result.fields[0];

  assert.strictEqual(record.children[0].byteLength, 2);
  assert.strictEqual(record.children[1].byteLength, 4);
  assert.strictEqual(result.totalLength, 6);
});

// =========================================================================
// Offset correctness
// =========================================================================
console.log("\n--- Offset Correctness ---");

test("Calculates correct offsets for sequential fields", () => {
  const source = `
       01 OFFSET-TEST.
          05 FIELD-A PIC X(5).
          05 FIELD-B PIC X(10).
          05 FIELD-C PIC X(3).
          05 FIELD-D PIC 9(8).
  `;
  const result = parseCopybook(source);
  const record = result.fields[0];

  assert.strictEqual(record.children[0].startOffset, 0);
  assert.strictEqual(record.children[1].startOffset, 5);
  assert.strictEqual(record.children[2].startOffset, 15);
  assert.strictEqual(record.children[3].startOffset, 18);
  assert.strictEqual(result.totalLength, 26);
});

test("Calculates correct offsets with OCCURS", () => {
  const source = `
       01 ARRAY-TEST.
          05 PREFIX PIC X(4).
          05 ITEMS OCCURS 5 TIMES PIC X(10).
          05 SUFFIX PIC X(2).
  `;
  const result = parseCopybook(source);
  const record = result.fields[0];

  assert.strictEqual(record.children[0].startOffset, 0);  // PREFIX
  assert.strictEqual(record.children[1].startOffset, 4);  // ITEMS
  // After ITEMS: 4 + (10 * 5) = 54
  assert.strictEqual(record.children[2].startOffset, 54); // SUFFIX
  assert.strictEqual(result.totalLength, 56);
});

// =========================================================================
// COMP/BINARY fields
// =========================================================================
console.log("\n--- COMP/BINARY Fields ---");

test("Parses COMP and BINARY fields with correct sizes", () => {
  const source = `
       01 BINARY-RECORD.
          05 SMALL-INT  PIC 9(4) COMP.
          05 MED-INT    PIC 9(9) COMP.
          05 LARGE-INT  PIC 9(18) BINARY.
  `;
  const result = parseCopybook(source);
  const record = result.fields[0];

  assert.strictEqual(record.children[0].byteLength, 2);  // 1-4 digits
  assert.strictEqual(record.children[1].byteLength, 4);  // 5-9 digits
  assert.strictEqual(record.children[2].byteLength, 8);  // 10-18 digits

  // Total = 2 + 4 + 8 = 14
  assert.strictEqual(result.totalLength, 14);
});

// =========================================================================
// Mixed COMP types
// =========================================================================
console.log("\n--- Mixed COMP Types ---");

test("Parses record with mixed COMP types", () => {
  const source = `
       01 MIXED-COMP-RECORD.
          05 MC-COUNTER    PIC 9(4) COMP.
          05 MC-FLOAT-S    COMP-1.
          05 MC-FLOAT-D    COMP-2.
          05 MC-PACKED     PIC S9(5)V99 COMP-3.
          05 MC-DISPLAY    PIC X(15).
  `;
  const result = parseCopybook(source);
  const record = result.fields[0];

  assert.strictEqual(record.children[0].byteLength, 2);   // COMP 9(4)
  assert.strictEqual(record.children[1].byteLength, 4);   // COMP-1 (float)
  assert.strictEqual(record.children[2].byteLength, 8);   // COMP-2 (double)
  assert.strictEqual(record.children[3].byteLength, 4);   // COMP-3 ceil((7+1)/2)
  assert.strictEqual(record.children[4].byteLength, 15);  // DISPLAY

  // Total = 2 + 4 + 8 + 4 + 15 = 33
  assert.strictEqual(result.totalLength, 33);
});

// =========================================================================
// Multiple 01-levels
// =========================================================================
console.log("\n--- Multiple 01-levels ---");

test("Parses multiple 01-level records", () => {
  const source = `
       01 RECORD-A.
          05 RA-FIELD PIC X(10).
       01 RECORD-B.
          05 RB-FIELD PIC X(20).
  `;
  const result = parseCopybook(source);

  assert.strictEqual(result.fields.length, 2);
  assert.strictEqual(result.fields[0].name, "RECORD-A");
  assert.strictEqual(result.fields[0].byteLength, 10);
  assert.strictEqual(result.fields[1].name, "RECORD-B");
  assert.strictEqual(result.fields[1].byteLength, 20);
  assert.strictEqual(result.totalLength, 30);
});

// =========================================================================
// Free-format source
// =========================================================================
console.log("\n--- Free Format ---");

test("Parses free-format copybook source", () => {
  const source = `01 FREE-FORMAT-REC.
  05 FF-NAME PIC X(25).
  05 FF-CODE PIC 9(3).
  05 FF-AMT PIC S9(7)V99 COMP-3.
`;
  const result = parseCopybook(source);

  assert.strictEqual(result.name, "FREE-FORMAT-REC");
  const record = result.fields[0];
  assert.strictEqual(record.children.length, 3);
  assert.strictEqual(record.children[0].byteLength, 25);
  assert.strictEqual(record.children[1].byteLength, 3);
  assert.strictEqual(record.children[2].byteLength, 5); // COMP-3 ceil((9+1)/2)
  assert.strictEqual(result.totalLength, 33);
});

// =========================================================================
// Summary
// =========================================================================
console.log("\n===================================");
console.log(`Results: ${passed} passed, ${failed} failed`);
console.log("===================================\n");

if (failed > 0) {
  process.exit(1);
}
