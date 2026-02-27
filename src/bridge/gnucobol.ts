// GnuCOBOL FFI Bridge
//
// Provides functions to compile COBOL source files to shared libraries
// and call compiled COBOL programs, using GnuCOBOL's `cobc` compiler
// and `cobcrun` runtime.

import { execFile } from 'child_process';
import { promisify } from 'util';
import * as path from 'path';
import * as fs from 'fs';

const execFileAsync = promisify(execFile);

/**
 * Result from detecting GnuCOBOL installation.
 */
export interface GnuCOBOLInfo {
  installed: boolean;
  version?: string;
  compilerPath?: string;
}

/**
 * Options for compiling COBOL programs.
 */
export interface CompileOptions {
  /** Additional flags to pass to cobc */
  flags?: string[];
  /** Working directory for compilation */
  cwd?: string;
  /** Compile as a module/shared library (default: true) */
  asModule?: boolean;
}

/**
 * Options for calling COBOL programs.
 */
export interface CallOptions {
  /** Working directory */
  cwd?: string;
  /** Timeout in milliseconds (default: 30000) */
  timeout?: number;
  /** Environment variables */
  env?: Record<string, string>;
}

/**
 * Detect GnuCOBOL installation and return version info.
 *
 * @returns Information about the GnuCOBOL installation
 */
export async function detectGnuCOBOL(): Promise<GnuCOBOLInfo> {
  try {
    const { stdout } = await execFileAsync('cobc', ['--version'], {
      timeout: 5000,
    });

    // Parse version from output like "cobc (GnuCOBOL) 3.2.0"
    const versionMatch = stdout.match(/cobc\s+\(GnuCOBOL\)\s+([\d.]+)/);
    const version = versionMatch ? versionMatch[1] : stdout.trim().split('\n')[0];

    // Find the full path to cobc
    const whichCmd = process.platform === 'win32' ? 'where' : 'which';
    let compilerPath: string | undefined;
    try {
      const { stdout: pathOutput } = await execFileAsync(whichCmd, ['cobc'], {
        timeout: 5000,
      });
      compilerPath = pathOutput.trim().split('\n')[0];
    } catch {
      // Could not determine path, but compiler works
    }

    return { installed: true, version, compilerPath };
  } catch {
    return { installed: false };
  }
}

/**
 * Compile a COBOL source file to a shared library / module.
 *
 * Uses `cobc -m` (module) by default, which produces a .so/.dll
 * that can be loaded by cobcrun or dynamically.
 *
 * @param sourcePath - Path to the .cbl/.cob source file
 * @param outputPath - Optional output path for the compiled module
 * @param options - Compilation options
 * @returns Path to the compiled shared library
 */
export async function compileCobol(
  sourcePath: string,
  outputPath?: string,
  options: CompileOptions = {}
): Promise<string> {
  // Verify source file exists
  if (!fs.existsSync(sourcePath)) {
    throw new Error(`COBOL source file not found: ${sourcePath}`);
  }

  const args: string[] = [];

  // Module (-m) or shared library (-shared)
  if (options.asModule === false) {
    args.push('-x'); // Executable
  } else {
    args.push('-m'); // Module (default)
  }

  // Output path
  if (outputPath) {
    args.push('-o', outputPath);
  }

  // Additional flags
  if (options.flags) {
    args.push(...options.flags);
  }

  // Source file
  args.push(sourcePath);

  try {
    await execFileAsync('cobc', args, {
      cwd: options.cwd || path.dirname(sourcePath),
      timeout: 60000, // 1 minute timeout for compilation
    });
  } catch (err: any) {
    const message = err.stderr || err.message || 'Unknown compilation error';
    throw new Error(`COBOL compilation failed: ${message}`);
  }

  // Determine the output file path
  if (outputPath) {
    return outputPath;
  }

  // Default output: same name as source with platform-specific extension
  const baseName = path.basename(sourcePath, path.extname(sourcePath));
  const ext = process.platform === 'win32' ? '.dll' : '.so';
  return path.join(options.cwd || path.dirname(sourcePath), baseName + ext);
}

/**
 * Call a compiled COBOL program using cobcrun.
 *
 * The linkage data buffer is passed via stdin and the program's
 * output (modified linkage data) is read from stdout.
 *
 * @param libraryPath - Path to the compiled shared library / module
 * @param programName - Name of the COBOL program to call
 * @param linkageData - Buffer containing the linkage section data
 * @param options - Call options
 * @returns Buffer containing the modified linkage section data
 */
export async function callCobolProgram(
  libraryPath: string,
  programName: string,
  linkageData: Buffer,
  options: CallOptions = {}
): Promise<Buffer> {
  // Verify library exists
  if (!fs.existsSync(libraryPath)) {
    throw new Error(`COBOL library not found: ${libraryPath}`);
  }

  const timeout = options.timeout ?? 30000;
  const libDir = path.dirname(path.resolve(libraryPath));

  // Set up environment for cobcrun to find the module
  const env = {
    ...process.env,
    ...options.env,
    COB_LIBRARY_PATH: libDir,
  };

  return new Promise<Buffer>((resolve, reject) => {
    const child = execFile(
      'cobcrun',
      [programName],
      {
        cwd: options.cwd || libDir,
        timeout,
        encoding: 'buffer' as any,
        env,
        maxBuffer: 10 * 1024 * 1024, // 10MB
      },
      (error, stdout, stderr) => {
        if (error) {
          const stderrStr = stderr ? Buffer.from(stderr).toString('utf-8') : '';
          reject(new Error(
            `COBOL program '${programName}' failed: ${stderrStr || error.message}`
          ));
          return;
        }
        resolve(Buffer.from(stdout as any));
      }
    );

    // Write linkage data to stdin
    if (child.stdin) {
      child.stdin.write(linkageData);
      child.stdin.end();
    }
  });
}
