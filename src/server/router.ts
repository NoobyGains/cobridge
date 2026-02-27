/**
 * COBridge -- Express Router
 *
 * Auto-creates REST API routes from COBOL copybook files.
 * Each .cpy file in the configured directory becomes a POST endpoint
 * with JSON Schema validation, plus metadata endpoints for schema
 * and TypeScript type introspection.
 */

import { Router } from "express";
import * as fs from "node:fs";
import * as path from "node:path";
import { parseCopybook } from "../parser/parser.js";
import { generateOpenApiSpec } from "../codegen/openapi.js";
import { generateTypeScriptTypes } from "../codegen/typescript.js";
import { generateJsonSchema } from "../codegen/json-schema.js";
import { validateRequest } from "./middleware.js";

interface RouterConfig {
  copybookDir: string;
  programDir?: string;
}

interface CopybookEntry {
  name: string;
  slug: string;
  schema: Record<string, unknown>;
  openapi: Record<string, unknown>;
  types: string;
}

/**
 * Convert a copybook filename to a URL-friendly slug.
 */
function fileToSlug(filename: string): string {
  return path
    .basename(filename, path.extname(filename))
    .toLowerCase()
    .replace(/_/g, "-");
}

/**
 * Scan a directory for .cpy files and return their paths.
 */
function scanCopybooks(dir: string): string[] {
  if (!fs.existsSync(dir)) {
    return [];
  }
  return fs
    .readdirSync(dir)
    .filter((f) => f.endsWith(".cpy") || f.endsWith(".CPY"))
    .map((f) => path.join(dir, f));
}

/**
 * Create an Express router that auto-generates REST endpoints
 * from COBOL copybook files.
 *
 * Routes created for each copybook:
 * - POST   /api/{slug}         — call the COBOL program (stub)
 * - GET    /api/{slug}/schema   — returns OpenAPI spec
 * - GET    /api/{slug}/types    — returns TypeScript types
 *
 * @param config - Router configuration
 * @returns An Express Router with auto-generated routes
 */
export function createRouter(config: RouterConfig): Router {
  const router = Router();
  const entries: CopybookEntry[] = [];

  // Scan and parse all copybooks
  const cpyFiles = scanCopybooks(config.copybookDir);

  for (const cpyFile of cpyFiles) {
    const source = fs.readFileSync(cpyFile, "utf-8");
    const copybook = parseCopybook(source);
    const slug = fileToSlug(cpyFile);

    const schema = generateJsonSchema(copybook);
    const openapi = generateOpenApiSpec(copybook);
    const types = generateTypeScriptTypes(copybook);

    const entry: CopybookEntry = { name: copybook.name, slug, schema, openapi, types };
    entries.push(entry);

    // POST /api/{slug} — call COBOL program (stub handler)
    router.post(
      `/api/${slug}`,
      validateRequest(schema),
      (req, res) => {
        // In a full implementation this would marshal data and call the COBOL program.
        // For now, echo the request body back as a stub response.
        res.json({
          copybook: copybook.name,
          message: "COBOL program call stub — bridge not connected",
          input: req.body,
        });
      }
    );

    // GET /api/{slug}/schema — return OpenAPI spec
    router.get(`/api/${slug}/schema`, (_req, res) => {
      res.json(openapi);
    });

    // GET /api/{slug}/types — return TypeScript types
    router.get(`/api/${slug}/types`, (_req, res) => {
      res.type("text/plain").send(types);
    });
  }

  // GET /api — list all available endpoints
  router.get("/api", (_req, res) => {
    res.json({
      copybooks: entries.map((e) => ({
        name: e.name,
        slug: e.slug,
        endpoints: {
          call: `POST /api/${e.slug}`,
          schema: `GET /api/${e.slug}/schema`,
          types: `GET /api/${e.slug}/types`,
        },
      })),
    });
  });

  return router;
}
