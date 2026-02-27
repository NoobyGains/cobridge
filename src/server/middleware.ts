/**
 * COBridge -- Express Middleware
 *
 * Provides request logging, error handling, request validation,
 * and CORS support for the COBridge HTTP server.
 */

import { Request, Response, NextFunction } from "express";

/**
 * Request logger middleware.
 * Logs incoming requests with method, URL, status code, and response time.
 */
export function requestLogger() {
  return (req: Request, res: Response, next: NextFunction): void => {
    const start = Date.now();
    const { method, url } = req;

    res.on("finish", () => {
      const duration = Date.now() - start;
      const status = res.statusCode;
      console.log(`[COBridge] ${method} ${url} ${status} ${duration}ms`);
    });

    next();
  };
}

/**
 * Structured JSON error response.
 */
interface ErrorResponse {
  error: string;
  code?: string;
  details?: string[];
  stack?: string;
}

/**
 * Error handler middleware.
 * Catches errors and returns structured JSON error responses.
 */
export function errorHandler() {
  return (err: Error & { status?: number; code?: string; details?: string[] }, _req: Request, res: Response, _next: NextFunction): void => {
    const status = err.status ?? 500;
    const body: ErrorResponse = {
      error: err.message || "Internal Server Error",
    };

    if (err.code) {
      body.code = err.code;
    }
    if (err.details) {
      body.details = err.details;
    }
    if (process.env.NODE_ENV === "development" && err.stack) {
      body.stack = err.stack;
    }

    res.status(status).json(body);
  };
}

/**
 * Validation error with status code and details.
 */
class ValidationError extends Error {
  status = 400;
  code = "VALIDATION_ERROR";
  details: string[];

  constructor(details: string[]) {
    super("Request validation failed");
    this.details = details;
  }
}

/**
 * Request body validation middleware.
 * Validates the request body against a JSON Schema object.
 *
 * This is a lightweight validator that checks required fields, types,
 * maxLength, minimum, maximum, and array constraints.
 */
export function validateRequest(schema: Record<string, unknown>) {
  return (req: Request, _res: Response, next: NextFunction): void => {
    const errors = validateValue(req.body, schema, "body");
    if (errors.length > 0) {
      next(new ValidationError(errors));
    } else {
      next();
    }
  };
}

/**
 * Simple recursive JSON Schema validator.
 */
function validateValue(
  value: unknown,
  schema: Record<string, unknown>,
  path: string
): string[] {
  const errors: string[] = [];
  const type = schema.type as string | undefined;

  if (value === undefined || value === null) {
    errors.push(`${path}: value is required`);
    return errors;
  }

  if (type === "object" && typeof value === "object" && !Array.isArray(value)) {
    const obj = value as Record<string, unknown>;
    const properties = schema.properties as Record<string, Record<string, unknown>> | undefined;
    const required = schema.required as string[] | undefined;

    if (required) {
      for (const key of required) {
        if (obj[key] === undefined) {
          errors.push(`${path}.${key}: required field missing`);
        }
      }
    }

    if (properties) {
      for (const [key, propSchema] of Object.entries(properties)) {
        if (obj[key] !== undefined) {
          errors.push(...validateValue(obj[key], propSchema, `${path}.${key}`));
        }
      }
    }
  } else if (type === "array" && Array.isArray(value)) {
    const maxItems = schema.maxItems as number | undefined;
    const minItems = schema.minItems as number | undefined;
    if (maxItems !== undefined && value.length > maxItems) {
      errors.push(`${path}: array length ${value.length} exceeds maxItems ${maxItems}`);
    }
    if (minItems !== undefined && value.length < minItems) {
      errors.push(`${path}: array length ${value.length} below minItems ${minItems}`);
    }
    const items = schema.items as Record<string, unknown> | undefined;
    if (items) {
      for (let i = 0; i < value.length; i++) {
        errors.push(...validateValue(value[i], items, `${path}[${i}]`));
      }
    }
  } else if (type === "string") {
    if (typeof value !== "string") {
      errors.push(`${path}: expected string, got ${typeof value}`);
    } else {
      const maxLength = schema.maxLength as number | undefined;
      if (maxLength !== undefined && value.length > maxLength) {
        errors.push(`${path}: string length ${value.length} exceeds maxLength ${maxLength}`);
      }
    }
  } else if (type === "number" || type === "integer") {
    if (typeof value !== "number") {
      errors.push(`${path}: expected number, got ${typeof value}`);
    } else {
      if (type === "integer" && !Number.isInteger(value)) {
        errors.push(`${path}: expected integer, got float`);
      }
      const minimum = schema.minimum as number | undefined;
      const maximum = schema.maximum as number | undefined;
      if (minimum !== undefined && value < minimum) {
        errors.push(`${path}: value ${value} below minimum ${minimum}`);
      }
      if (maximum !== undefined && value > maximum) {
        errors.push(`${path}: value ${value} exceeds maximum ${maximum}`);
      }
    }
  }

  return errors;
}

/**
 * CORS middleware.
 * Sets permissive CORS headers for development use.
 */
export function cors() {
  return (req: Request, res: Response, next: NextFunction): void => {
    res.setHeader("Access-Control-Allow-Origin", "*");
    res.setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS");
    res.setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization");
    res.setHeader("Access-Control-Max-Age", "86400");

    if (req.method === "OPTIONS") {
      res.status(204).end();
      return;
    }

    next();
  };
}
