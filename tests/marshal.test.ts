// Comprehensive tests for the marshal layer.
// Uses basic assert/console.log — no test framework required.

import * as assert from 'assert';
import { ebcdicToAscii, asciiToEbcdic } from '../src/marshal/ebcdic';
import { packDecimal, unpackDecimal } from '../src/marshal/packed-decimal';
import { encodeBinary, decodeBinary, encodeComp1, decodeComp1, encodeComp2, decodeComp2, binarySize } from '../src/marshal/binary';
import { marshalToCobol, marshalFromCobol, CopybookField } from '../src/marshal/converter';

let passed = 0;
let failed = 0;

function test(name: string, fn: () => void): void {
  try {
    fn();
    console.log(`  PASS: ${name}`);
    passed++;
  } catch (err: any) {
    console.log(`  FAIL: ${name}`);
    console.log(`        ${err.message}`);
    failed++;
  }
}

// ============================================================
// EBCDIC Conversion Tests
// ============================================================
console.log('\n=== EBCDIC Conversion ===');

test('EBCDIC round-trip: ASCII letters', () => {
  const original = 'HELLO WORLD';
  const ebcdic = asciiToEbcdic(original);
  const backToAscii = ebcdicToAscii(ebcdic);
  assert.strictEqual(backToAscii, original);
});

test('EBCDIC round-trip: digits', () => {
  const original = '0123456789';
  const ebcdic = asciiToEbcdic(original);
  const backToAscii = ebcdicToAscii(ebcdic);
  assert.strictEqual(backToAscii, original);
});

test('EBCDIC round-trip: mixed case and symbols', () => {
  const original = 'Hello, World! #123';
  const ebcdic = asciiToEbcdic(original);
  const backToAscii = ebcdicToAscii(ebcdic);
  assert.strictEqual(backToAscii, original);
});

test('EBCDIC: known CP037 mappings', () => {
  // In CP037, space (0x20 ASCII) -> 0x40 EBCDIC
  const spaceEbcdic = asciiToEbcdic(' ');
  assert.strictEqual(spaceEbcdic[0], 0x40);

  // 'A' (0x41 ASCII) -> 0xC1 EBCDIC in CP037
  const aEbcdic = asciiToEbcdic('A');
  assert.strictEqual(aEbcdic[0], 0xC1);

  // '0' (0x30 ASCII) -> 0xF0 EBCDIC in CP037
  const zeroEbcdic = asciiToEbcdic('0');
  assert.strictEqual(zeroEbcdic[0], 0xF0);
});

test('EBCDIC: code page 1047 round-trip', () => {
  const original = 'TEST DATA';
  const ebcdic = asciiToEbcdic(original, '1047');
  const backToAscii = ebcdicToAscii(ebcdic, '1047');
  assert.strictEqual(backToAscii, original);
});

test('EBCDIC: code page 500 round-trip', () => {
  const original = 'COBOL PROGRAM';
  const ebcdic = asciiToEbcdic(original, '500');
  const backToAscii = ebcdicToAscii(ebcdic, '500');
  assert.strictEqual(backToAscii, original);
});

test('EBCDIC: empty string', () => {
  const ebcdic = asciiToEbcdic('');
  assert.strictEqual(ebcdic.length, 0);
  assert.strictEqual(ebcdicToAscii(ebcdic), '');
});

// ============================================================
// Packed Decimal (COMP-3) Tests
// ============================================================
console.log('\n=== Packed Decimal (COMP-3) ===');

test('Pack/unpack positive integer: 12345', () => {
  const packed = packDecimal(12345, 5);
  const unpacked = unpackDecimal(packed);
  assert.strictEqual(unpacked, 12345);
});

test('Pack 12345: correct bytes', () => {
  // 12345 positive -> 0x12 0x34 0x5C
  const packed = packDecimal(12345, 5);
  assert.strictEqual(packed.length, 3);
  assert.strictEqual(packed[0], 0x12);
  assert.strictEqual(packed[1], 0x34);
  assert.strictEqual(packed[2], 0x5C);
});

test('Pack/unpack with decimal: 123.45', () => {
  const packed = packDecimal(123.45, 5, 2);
  const unpacked = unpackDecimal(packed, 2);
  assert.strictEqual(unpacked, 123.45);
});

test('Pack/unpack negative: -12345', () => {
  const packed = packDecimal(-12345, 5);
  const unpacked = unpackDecimal(packed);
  assert.strictEqual(unpacked, -12345);
});

test('Pack negative: correct sign nibble', () => {
  // -12345 -> 0x12 0x34 0x5D
  const packed = packDecimal(-12345, 5);
  assert.strictEqual(packed[2] & 0x0F, 0x0D); // D = negative
});

test('Pack/unpack zero', () => {
  const packed = packDecimal(0, 5);
  const unpacked = unpackDecimal(packed);
  assert.strictEqual(unpacked, 0);
});

test('Pack/unpack negative zero as zero', () => {
  const packed = packDecimal('-0', 5);
  const unpacked = unpackDecimal(packed);
  // -0 packs with D sign but unpacks as -0, which equals 0
  assert.strictEqual(Object.is(unpacked, 0) || unpacked === 0, true);
});

test('Pack/unpack single digit', () => {
  const packed = packDecimal(7, 1);
  const unpacked = unpackDecimal(packed);
  assert.strictEqual(unpacked, 7);
});

test('Pack/unpack large value: 999999999', () => {
  const packed = packDecimal(999999999, 9);
  const unpacked = unpackDecimal(packed);
  assert.strictEqual(unpacked, 999999999);
});

test('Pack/unpack negative decimal: -999.99', () => {
  const packed = packDecimal(-999.99, 5, 2);
  const unpacked = unpackDecimal(packed, 2);
  assert.strictEqual(unpacked, -999.99);
});

test('Pack with string value for precision', () => {
  const packed = packDecimal('123.45', 5, 2);
  const unpacked = unpackDecimal(packed, 2);
  assert.strictEqual(unpacked, 123.45);
});

// ============================================================
// Binary Encoding (COMP/BINARY) Tests
// ============================================================
console.log('\n=== Binary Encoding (COMP/BINARY) ===');

test('Binary 2-byte signed positive', () => {
  const encoded = encodeBinary(1234, 2, true);
  const decoded = decodeBinary(encoded, true);
  assert.strictEqual(decoded, 1234);
  assert.strictEqual(encoded.length, 2);
});

test('Binary 2-byte signed negative', () => {
  const encoded = encodeBinary(-1234, 2, true);
  const decoded = decodeBinary(encoded, true);
  assert.strictEqual(decoded, -1234);
});

test('Binary 2-byte unsigned', () => {
  const encoded = encodeBinary(50000, 2, false);
  const decoded = decodeBinary(encoded, false);
  assert.strictEqual(decoded, 50000);
});

test('Binary 4-byte signed', () => {
  const encoded = encodeBinary(123456789, 4, true);
  const decoded = decodeBinary(encoded, true);
  assert.strictEqual(decoded, 123456789);
});

test('Binary 4-byte signed negative', () => {
  const encoded = encodeBinary(-123456789, 4, true);
  const decoded = decodeBinary(encoded, true);
  assert.strictEqual(decoded, -123456789);
});

test('Binary 8-byte signed', () => {
  const encoded = encodeBinary(1234567890123, 8, true);
  const decoded = decodeBinary(encoded, true);
  assert.strictEqual(decoded, 1234567890123);
});

test('Binary is big-endian', () => {
  const encoded = encodeBinary(0x0102, 2, false);
  assert.strictEqual(encoded[0], 0x01);
  assert.strictEqual(encoded[1], 0x02);
});

test('Binary zero', () => {
  const encoded = encodeBinary(0, 4, true);
  const decoded = decodeBinary(encoded, true);
  assert.strictEqual(decoded, 0);
});

test('COMP-1 float round-trip', () => {
  const encoded = encodeComp1(3.14);
  const decoded = decodeComp1(encoded);
  assert.ok(Math.abs(decoded - 3.14) < 0.001, `Expected ~3.14, got ${decoded}`);
});

test('COMP-2 double round-trip', () => {
  const encoded = encodeComp2(3.141592653589793);
  const decoded = decodeComp2(encoded);
  assert.strictEqual(decoded, 3.141592653589793);
});

test('binarySize: 1-4 digits -> 2 bytes', () => {
  assert.strictEqual(binarySize(1), 2);
  assert.strictEqual(binarySize(4), 2);
});

test('binarySize: 5-9 digits -> 4 bytes', () => {
  assert.strictEqual(binarySize(5), 4);
  assert.strictEqual(binarySize(9), 4);
});

test('binarySize: 10-18 digits -> 8 bytes', () => {
  assert.strictEqual(binarySize(10), 8);
  assert.strictEqual(binarySize(18), 8);
});

// ============================================================
// Full Record Marshalling Tests
// ============================================================
console.log('\n=== Full Record Marshalling ===');

test('Marshal/unmarshal simple alphanumeric record', () => {
  const copybook: CopybookField[] = [
    { name: 'FIRST-NAME', level: 5, pic: 'X(10)' },
    { name: 'LAST-NAME', level: 5, pic: 'X(15)' },
  ];

  const input = {
    firstName: 'John',
    lastName: 'Doe',
  };

  const buffer = marshalToCobol(input, copybook);
  assert.strictEqual(buffer.length, 25); // 10 + 15

  const output = marshalFromCobol(buffer, copybook);
  assert.strictEqual(output.firstName, 'John');
  assert.strictEqual(output.lastName, 'Doe');
});

test('Marshal/unmarshal numeric display field', () => {
  const copybook: CopybookField[] = [
    { name: 'AMOUNT', level: 5, pic: '9(5)V9(2)' },
  ];

  const input = { amount: 123.45 };
  const buffer = marshalToCobol(input, copybook);
  assert.strictEqual(buffer.length, 7); // 5 + 2 digits

  const output = marshalFromCobol(buffer, copybook);
  assert.strictEqual(output.amount, 123.45);
});

test('Marshal/unmarshal COMP-3 field', () => {
  const copybook: CopybookField[] = [
    { name: 'BALANCE', level: 5, pic: 'S9(7)V9(2)', usage: 'COMP-3' },
  ];

  const input = { balance: 12345.67 };
  const buffer = marshalToCobol(input, copybook);
  // 9 total digits packed: ceil((9+1)/2) = 5 bytes
  assert.strictEqual(buffer.length, 5);

  const output = marshalFromCobol(buffer, copybook);
  assert.strictEqual(output.balance, 12345.67);
});

test('Marshal/unmarshal BINARY/COMP field', () => {
  const copybook: CopybookField[] = [
    { name: 'COUNT', level: 5, pic: 'S9(4)', usage: 'COMP' },
  ];

  const input = { count: 9999 };
  const buffer = marshalToCobol(input, copybook);
  assert.strictEqual(buffer.length, 2); // 4 digits -> halfword

  const output = marshalFromCobol(buffer, copybook);
  assert.strictEqual(output.count, 9999);
});

test('Marshal/unmarshal group item', () => {
  const copybook: CopybookField[] = [
    {
      name: 'EMPLOYEE',
      level: 1,
      children: [
        { name: 'EMP-NAME', level: 5, pic: 'X(20)' },
        { name: 'EMP-ID', level: 5, pic: '9(5)', usage: 'COMP-3' },
        { name: 'EMP-SALARY', level: 5, pic: 'S9(7)V9(2)', usage: 'COMP-3' },
      ],
    },
  ];

  const input = {
    employee: {
      empName: 'Jane Smith',
      empId: 12345,
      empSalary: 75000.50,
    },
  };

  const buffer = marshalToCobol(input, copybook);
  // 20 (name) + 3 (emp-id packed) + 5 (salary packed) = 28 bytes
  assert.strictEqual(buffer.length, 28);

  const output = marshalFromCobol(buffer, copybook);
  assert.strictEqual(output.employee.empName, 'Jane Smith');
  assert.strictEqual(output.employee.empId, 12345);
  assert.strictEqual(output.employee.empSalary, 75000.50);
});

test('Marshal/unmarshal OCCURS (arrays)', () => {
  const copybook: CopybookField[] = [
    { name: 'ITEM-COUNT', level: 5, pic: '9(2)' },
    { name: 'ITEM-NAME', level: 5, pic: 'X(10)', occurs: 3 },
  ];

  const input = {
    itemCount: 3,
    itemName: ['Widget', 'Gadget', 'Gizmo'],
  };

  const buffer = marshalToCobol(input, copybook);
  // 2 (count) + 3*10 (items) = 32 bytes
  assert.strictEqual(buffer.length, 32);

  const output = marshalFromCobol(buffer, copybook);
  assert.strictEqual(output.itemCount, 3);
  assert.strictEqual(output.itemName[0], 'Widget');
  assert.strictEqual(output.itemName[1], 'Gadget');
  assert.strictEqual(output.itemName[2], 'Gizmo');
});

test('Marshal/unmarshal complex mixed record', () => {
  // Simulates a typical COBOL record:
  //   01 CUSTOMER-RECORD.
  //      05 CUST-ID        PIC 9(8)   COMP.
  //      05 CUST-NAME      PIC X(30).
  //      05 CUST-BALANCE   PIC S9(9)V99 COMP-3.
  //      05 CUST-TYPE      PIC X(1).
  const copybook: CopybookField[] = [
    {
      name: 'CUSTOMER-RECORD',
      level: 1,
      children: [
        { name: 'CUST-ID', level: 5, pic: '9(8)', usage: 'COMP' },
        { name: 'CUST-NAME', level: 5, pic: 'X(30)' },
        { name: 'CUST-BALANCE', level: 5, pic: 'S9(9)V9(2)', usage: 'COMP-3' },
        { name: 'CUST-TYPE', level: 5, pic: 'X(1)' },
      ],
    },
  ];

  const input = {
    customerRecord: {
      custId: 12345678,
      custName: 'Acme Corporation',
      custBalance: -50000.99,
      custType: 'B',
    },
  };

  const buffer = marshalToCobol(input, copybook);
  // 4 (COMP 8 digits) + 30 (name) + 6 (COMP-3 11 digits) + 1 (type) = 41 bytes
  assert.strictEqual(buffer.length, 41);

  const output = marshalFromCobol(buffer, copybook);
  assert.strictEqual(output.customerRecord.custId, 12345678);
  assert.strictEqual(output.customerRecord.custName, 'Acme Corporation');
  assert.strictEqual(output.customerRecord.custBalance, -50000.99);
  assert.strictEqual(output.customerRecord.custType, 'B');
});

test('Marshal: space-padding for PIC X', () => {
  const copybook: CopybookField[] = [
    { name: 'FIELD', level: 5, pic: 'X(10)' },
  ];

  const buffer = marshalToCobol({ field: 'Hi' }, copybook);
  // Should be "Hi        " (8 trailing spaces)
  assert.strictEqual(buffer.toString('ascii'), 'Hi        ');
});

test('Marshal: zero-padding for PIC 9', () => {
  const copybook: CopybookField[] = [
    { name: 'NUM', level: 5, pic: '9(5)' },
  ];

  const buffer = marshalToCobol({ num: 42 }, copybook);
  // Should be "00042"
  assert.strictEqual(buffer.toString('ascii'), '00042');
});

// ============================================================
// Summary
// ============================================================
console.log('\n=== Results ===');
console.log(`  Passed: ${passed}`);
console.log(`  Failed: ${failed}`);
console.log(`  Total:  ${passed + failed}`);

if (failed > 0) {
  process.exit(1);
}
