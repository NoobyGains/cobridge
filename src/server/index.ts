/**
 * COBridge -- HTTP Server
 *
 * Main entry point for the COBridge REST API server.
 * Mounts the auto-generated router, applies middleware,
 * and serves Swagger UI documentation.
 */

import express, { type Express } from "express";
import * as fs from "node:fs";
import * as path from "node:path";
import { createRouter } from "./router.js";
import { requestLogger, errorHandler, cors } from "./middleware.js";
import { parseCopybook } from "../parser/parser.js";
import { generateOpenApiSpec } from "../codegen/openapi.js";

interface ServerOptions {
  port?: number;
  copybookDir: string;
  programDir?: string;
}

/**
 * Create the Express application with all middleware and routes.
 * Exported separately for testing.
 */
export function createApp(options: ServerOptions): Express {
  const app = express();

  // Middleware
  app.use(cors());
  app.use(requestLogger());
  app.use(express.json());

  // Mount auto-generated API routes
  const router = createRouter({
    copybookDir: options.copybookDir,
    programDir: options.programDir,
  });
  app.use(router);

  // Serve a combined OpenAPI spec at /docs/openapi.json
  app.get("/docs/openapi.json", (_req, res) => {
    const combined = buildCombinedSpec(options);
    res.json(combined);
  });

  // Serve a minimal Swagger UI page at /docs
  app.get("/docs", (_req, res) => {
    res.type("html").send(swaggerUiHtml());
  });

  // Error handler (must be last)
  app.use(errorHandler());

  return app;
}

/**
 * Build a combined OpenAPI spec from all copybooks in the directory.
 */
function buildCombinedSpec(options: ServerOptions): Record<string, unknown> {
  const dir = options.copybookDir;
  const allPaths: Record<string, unknown> = {};
  const allSchemas: Record<string, unknown> = {};

  if (fs.existsSync(dir)) {
    const files = fs.readdirSync(dir).filter((f) => f.endsWith(".cpy") || f.endsWith(".CPY"));

    for (const file of files) {
      const source = fs.readFileSync(path.join(dir, file), "utf-8");
      const copybook = parseCopybook(source);
      const spec = generateOpenApiSpec(copybook) as {
        paths: Record<string, unknown>;
        components: { schemas: Record<string, unknown> };
      };
      Object.assign(allPaths, spec.paths);
      Object.assign(allSchemas, spec.components.schemas);
    }
  }

  return {
    openapi: "3.0.3",
    info: {
      title: "COBridge API",
      version: "1.0.0",
      description: "Auto-generated REST API from COBOL copybooks",
    },
    paths: allPaths,
    components: { schemas: allSchemas },
  };
}

/**
 * Minimal Swagger UI HTML page that loads from CDN.
 */
function swaggerUiHtml(): string {
  return `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>COBridge API Docs</title>
  <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/swagger-ui-dist@5/swagger-ui.css">
</head>
<body>
  <div id="swagger-ui"></div>
  <script src="https://cdn.jsdelivr.net/npm/swagger-ui-dist@5/swagger-ui-bundle.js"></script>
  <script>
    SwaggerUIBundle({
      url: '/docs/openapi.json',
      dom_id: '#swagger-ui',
      presets: [SwaggerUIBundle.presets.apis, SwaggerUIBundle.SwaggerUIStandalonePreset],
      layout: 'BaseLayout',
    });
  </script>
</body>
</html>`;
}

/**
 * Start the COBridge HTTP server.
 *
 * @param options - Server configuration
 */
export async function startServer(options: ServerOptions): Promise<void> {
  const port = options.port ?? 3000;
  const app = createApp(options);

  return new Promise((resolve) => {
    app.listen(port, () => {
      console.log("");
      console.log("  COBridge server running");
      console.log(`  Local:   http://localhost:${port}`);
      console.log(`  Docs:    http://localhost:${port}/docs`);
      console.log(`  API:     http://localhost:${port}/api`);
      console.log("");
      console.log(`  Copybooks: ${options.copybookDir}`);
      if (options.programDir) {
        console.log(`  Programs:  ${options.programDir}`);
      }
      console.log("");
      resolve();
    });
  });
}
