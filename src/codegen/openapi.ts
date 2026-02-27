/**
 * COBridge -- OpenAPI 3.0 Specification Generator
 *
 * Generates an OpenAPI 3.0.x specification from a parsed COBOL copybook AST.
 * Maps COBOL data types to JSON Schema types and creates REST API paths
 * for interacting with COBOL programs.
 */

import { Copybook, CopybookField, UsageType } from "../parser/types.js";

/** Convert a COBOL data-name to camelCase (e.g. CUST-FIRST-NAME -> custFirstName) */
export function cobolNameToCamelCase(name: string): string {
  return name
    .toLowerCase()
    .split("-")
    .map((part, i) => (i === 0 ? part : part.charAt(0).toUpperCase() + part.slice(1)))
    .join("");
}

/** Convert a COBOL data-name to PascalCase (e.g. CUSTOMER-RECORD -> CustomerRecord) */
export function cobolNameToPascalCase(name: string): string {
  return name
    .toLowerCase()
    .split("-")
    .map((part) => part.charAt(0).toUpperCase() + part.slice(1))
    .join("");
}

/** Convert a COBOL name to a URL-friendly kebab-case slug */
function cobolNameToSlug(name: string): string {
  return name.toLowerCase().replace(/_/g, "-");
}

interface OpenApiOptions {
  basePath?: string;
  title?: string;
}

/**
 * Map a single CopybookField to a JSON Schema property definition.
 */
function fieldToJsonSchema(field: CopybookField): Record<string, unknown> {
  // Level 88 — condition names become enum markers on the parent
  // (handled at the parent level, not individually)
  if (field.levelNumber === 88) {
    return { type: "boolean", description: `Condition: ${field.name}` };
  }

  // Group item -> object
  if (field.isGroup) {
    const properties: Record<string, unknown> = {};
    const required: string[] = [];

    // Check for level-88 children on non-group children (enum values)
    for (const child of field.children) {
      if (child.levelNumber === 88) continue;
      const propName = cobolNameToCamelCase(child.name);
      let schema = fieldToJsonSchema(child);

      // Wrap in array if OCCURS
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

    // Collect level-88 conditions from children as enum constraints
    const enumChildren = field.children.filter((c) => c.levelNumber === 88);
    if (enumChildren.length > 0 && !field.isGroup) {
      return {
        type: "string",
        enum: enumChildren.map((c) => cobolNameToCamelCase(c.name)),
        description: `COBOL: ${field.name} (level ${field.levelNumber})`,
      };
    }

    const schema: Record<string, unknown> = {
      type: "object",
      properties,
      description: `COBOL group: ${field.name} (level ${field.levelNumber}, ${field.byteLength} bytes)`,
    };
    if (required.length > 0) {
      schema.required = required;
    }
    return schema;
  }

  // Elementary item — map PIC + USAGE to JSON Schema type
  if (!field.picture) {
    // No PIC, no children — treat as string
    return { type: "string", description: `COBOL: ${field.name}` };
  }

  const pic = field.picture;
  const description = `COBOL: ${field.name} PIC ${pic.raw} (${field.usage}, offset ${field.startOffset}, ${field.byteLength} bytes)`;

  // Alphanumeric / Alphabetic -> string
  if (pic.type === "alphanumeric" || pic.type === "alpha") {
    return {
      type: "string",
      maxLength: pic.length,
      description,
    };
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
    case UsageType.PACKED_DECIMAL:
      if (pic.decimals > 0) {
        return { type: "number", description };
      }
      return { type: "integer", description };

    case UsageType.DISPLAY:
    default:
      // DISPLAY numeric — represented as a formatted string
      return {
        type: "string",
        pattern: pic.decimals > 0
          ? `^-?\\d{1,${pic.length - pic.decimals}}\\.\\d{${pic.decimals}}$`
          : `^-?\\d{1,${pic.length}}$`,
        maxLength: pic.length + (pic.signed ? 1 : 0) + (pic.decimals > 0 ? 1 : 0),
        description,
      };
  }
}

/**
 * Generate a complete OpenAPI 3.0.x specification from a parsed Copybook.
 */
export function generateOpenApiSpec(
  copybook: Copybook,
  options?: OpenApiOptions
): Record<string, unknown> {
  const basePath = options?.basePath ?? "/api";
  const title = options?.title ?? `COBridge — ${copybook.name}`;
  const slug = cobolNameToSlug(copybook.name);
  const schemaName = cobolNameToPascalCase(copybook.name);

  // Build the component schema from the top-level fields.
  // When there is a single 01-level group item (the common case), unwrap it
  // so the component schema directly contains the group's children.
  let componentSchema: Record<string, unknown>;

  if (
    copybook.fields.length === 1 &&
    copybook.fields[0].isGroup &&
    !copybook.fields[0].occurs
  ) {
    // Unwrap the single 01-level group
    componentSchema = fieldToJsonSchema(copybook.fields[0]);
    componentSchema.description = `Generated from COBOL copybook ${copybook.name} (${copybook.totalLength} bytes)`;
  } else {
    // Multiple top-level fields — wrap them in an object
    const properties: Record<string, unknown> = {};
    const required: string[] = [];

    for (const field of copybook.fields) {
      const propName = cobolNameToCamelCase(field.name);
      let schema = fieldToJsonSchema(field);

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

    componentSchema = {
      type: "object",
      properties,
      description: `Generated from COBOL copybook ${copybook.name} (${copybook.totalLength} bytes)`,
    };
    if (required.length > 0) {
      componentSchema.required = required;
    }
  }

  return {
    openapi: "3.0.3",
    info: {
      title,
      version: "1.0.0",
      description: `Auto-generated API from COBOL copybook: ${copybook.name}`,
    },
    paths: {
      [`${basePath}/${slug}`]: {
        post: {
          summary: `Call COBOL program ${copybook.name}`,
          operationId: `call${schemaName}`,
          requestBody: {
            required: true,
            content: {
              "application/json": {
                schema: { $ref: `#/components/schemas/${schemaName}` },
              },
            },
          },
          responses: {
            "200": {
              description: "Successful COBOL program execution",
              content: {
                "application/json": {
                  schema: { $ref: `#/components/schemas/${schemaName}` },
                },
              },
            },
            "400": {
              description: "Invalid request body",
              content: {
                "application/json": {
                  schema: {
                    type: "object",
                    properties: {
                      error: { type: "string" },
                      details: { type: "array", items: { type: "string" } },
                    },
                  },
                },
              },
            },
            "500": {
              description: "COBOL program execution error",
              content: {
                "application/json": {
                  schema: {
                    type: "object",
                    properties: {
                      error: { type: "string" },
                      code: { type: "string" },
                    },
                  },
                },
              },
            },
          },
        },
      },
    },
    components: {
      schemas: {
        [schemaName]: componentSchema,
      },
    },
  };
}
