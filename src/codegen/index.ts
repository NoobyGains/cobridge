/**
 * COBridge -- Code Generation Module
 *
 * Generates OpenAPI specs, TypeScript types, and JSON Schemas
 * from parsed COBOL copybook ASTs.
 */

export {
  generateOpenApiSpec,
  cobolNameToCamelCase,
  cobolNameToPascalCase,
} from "./openapi.js";

export { generateTypeScriptTypes } from "./typescript.js";

export { generateJsonSchema } from "./json-schema.js";
