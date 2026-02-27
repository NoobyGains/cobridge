/**
 * COBridge -- JSON Schema Generator
 *
 * Generates a JSON Schema (draft-07) from a parsed COBOL copybook AST.
 * Includes field constraints derived from PIC clauses such as
 * maxLength, minimum, maximum, and pattern.
 */

import { Copybook, CopybookField, UsageType } from "../parser/types.js";
import { cobolNameToCamelCase } from "./openapi.js";

/**
 * Compute the maximum integer value representable by a given number of digits.
 */
function maxForDigits(digits: number): number {
  return Math.pow(10, digits) - 1;
}

/**
 * Map a single CopybookField to a JSON Schema property.
 */
function fieldToSchema(field: CopybookField): Record<string, unknown> {
  if (field.levelNumber === 88) {
    return { type: "boolean", description: `Condition: ${field.name}` };
  }

  // Group item -> object
  if (field.isGroup) {
    const properties: Record<string, unknown> = {};
    const required: string[] = [];

    for (const child of field.children) {
      if (child.levelNumber === 88) continue;
      const propName = cobolNameToCamelCase(child.name);
      let schema = fieldToSchema(child);

      if (child.occurs) {
        schema = {
          type: "array",
          items: schema,
          maxItems: child.occurs.count,
          ...(child.occurs.min !== undefined ? { minItems: child.occurs.min } : {}),
        };
      }

      properties[propName] = schema;
      required.push(propName);
    }

    const schema: Record<string, unknown> = {
      type: "object",
      properties,
      description: `COBOL group: ${field.name} (level ${field.levelNumber})`,
    };
    if (required.length > 0) {
      schema.required = required;
    }
    return schema;
  }

  // Elementary item
  if (!field.picture) {
    if (field.usage === UsageType.COMP_1) {
      return { type: "number", format: "float", description: `COBOL: ${field.name} COMP-1` };
    }
    if (field.usage === UsageType.COMP_2) {
      return { type: "number", format: "double", description: `COBOL: ${field.name} COMP-2` };
    }
    return { type: "string", description: `COBOL: ${field.name}` };
  }

  const pic = field.picture;
  const description = `COBOL: ${field.name} PIC ${pic.raw} ${field.usage}`;

  // Alphanumeric / alphabetic -> string with maxLength
  if (pic.type === "alphanumeric" || pic.type === "alpha") {
    return { type: "string", maxLength: pic.length, description };
  }

  // Numeric types
  switch (field.usage) {
    case UsageType.COMP_1:
      return { type: "number", format: "float", description };

    case UsageType.COMP_2:
      return { type: "number", format: "double", description };

    case UsageType.COMP:
    case UsageType.COMP_3:
    case UsageType.BINARY:
    case UsageType.PACKED_DECIMAL: {
      const intDigits = pic.length - pic.decimals;
      const schema: Record<string, unknown> = { description };

      if (pic.decimals > 0) {
        schema.type = "number";
        schema.maximum = maxForDigits(intDigits) + (maxForDigits(pic.decimals) / Math.pow(10, pic.decimals));
        if (pic.signed) {
          schema.minimum = -(schema.maximum as number);
        } else {
          schema.minimum = 0;
        }
      } else {
        schema.type = "integer";
        schema.maximum = maxForDigits(pic.length);
        if (pic.signed) {
          schema.minimum = -(schema.maximum as number);
        } else {
          schema.minimum = 0;
        }
      }
      return schema;
    }

    case UsageType.DISPLAY:
    default: {
      // DISPLAY numeric — string representation with constraints
      const intDigits = pic.length - pic.decimals;
      const schema: Record<string, unknown> = {
        type: "string",
        description,
        maxLength: pic.length + (pic.signed ? 1 : 0) + (pic.decimals > 0 ? 1 : 0),
      };
      if (pic.decimals > 0) {
        schema.pattern = `^-?\\d{1,${intDigits}}\\.\\d{${pic.decimals}}$`;
      } else {
        schema.pattern = `^-?\\d{1,${pic.length}}$`;
      }
      return schema;
    }
  }
}

/**
 * Generate a JSON Schema (draft-07) from a parsed Copybook.
 *
 * @param copybook - The parsed copybook AST
 * @returns A JSON Schema object
 */
export function generateJsonSchema(copybook: Copybook): Record<string, unknown> {
  // When there is a single 01-level group (the common case), unwrap it
  // so the root schema directly contains the group's children.
  let root: Record<string, unknown>;

  if (
    copybook.fields.length === 1 &&
    copybook.fields[0].isGroup &&
    !copybook.fields[0].occurs
  ) {
    root = fieldToSchema(copybook.fields[0]);
  } else {
    const properties: Record<string, unknown> = {};
    const required: string[] = [];

    for (const field of copybook.fields) {
      const propName = cobolNameToCamelCase(field.name);
      let schema = fieldToSchema(field);

      if (field.occurs) {
        schema = {
          type: "array",
          items: schema,
          maxItems: field.occurs.count,
          ...(field.occurs.min !== undefined ? { minItems: field.occurs.min } : {}),
        };
      }

      properties[propName] = schema;
      required.push(propName);
    }

    root = {
      type: "object",
      properties,
    };
    if (required.length > 0) {
      root.required = required;
    }
  }

  root.$schema = "http://json-schema.org/draft-07/schema#";
  root.title = copybook.name;
  root.description = `JSON Schema for COBOL copybook: ${copybook.name} (${copybook.totalLength} bytes)`;
  return root;
}
