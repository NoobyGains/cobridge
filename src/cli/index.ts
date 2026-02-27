#!/usr/bin/env node

import { Command } from 'commander';
import chalk from 'chalk';
import { readFileSync, mkdirSync, writeFileSync, existsSync } from 'node:fs';
import { resolve, join } from 'node:path';

const BANNER = `
   ${chalk.cyan('_____ ____  ____       _     _            ')}
  ${chalk.cyan('/ ____/ __ \\|  _ \\     (_)   | |           ')}
 ${chalk.cyan('| |   | |  | | |_) |_ __ _  __| | __ _  ___ ')}
 ${chalk.cyan('| |   | |  | |  _ <| \'__| |/ _` |/ _` |/ _ \\')}
 ${chalk.cyan('| |___| |__| | |_) | |  | | (_| | (_| |  __/')}
  ${chalk.cyan('\\_____\\____/|____/|_|  |_|\\__,_|\\__, |\\___|')}
  ${chalk.cyan('                                  __/ |     ')}
  ${chalk.cyan('                                 |___/      ')}

  ${chalk.gray('Don\'t rewrite your COBOL. Bridge it.')}
`;

const program = new Command();

program
  .name('cobridge')
  .description('Auto-generate REST APIs from COBOL copybooks')
  .version('0.1.0')
  .hook('preAction', () => {
    console.log(BANNER);
  });

program
  .command('parse <copybook>')
  .description('Parse a copybook file and display the AST as formatted JSON')
  .action(async (copybook: string) => {
    const filePath = resolve(copybook);
    if (!existsSync(filePath)) {
      console.error(chalk.red(`Error: File not found: ${filePath}`));
      process.exit(1);
    }

    try {
      const { parseCopybook } = await import('../parser/index.js');
      const source = readFileSync(filePath, 'utf-8');
      const ast = parseCopybook(source);
      console.log(chalk.green('Parsed copybook AST:\n'));
      console.log(JSON.stringify(ast, null, 2));
    } catch (err) {
      console.error(chalk.red(`Error parsing copybook: ${(err as Error).message}`));
      process.exit(1);
    }
  });

program
  .command('generate <copybook>')
  .description('Generate OpenAPI spec and/or TypeScript types from a copybook')
  .option('-o, --output <dir>', 'Output directory', './generated')
  .option('-f, --format <format>', 'Output format: openapi, typescript, or both', 'both')
  .action(async (copybook: string, options: { output: string; format: string }) => {
    const filePath = resolve(copybook);
    if (!existsSync(filePath)) {
      console.error(chalk.red(`Error: File not found: ${filePath}`));
      process.exit(1);
    }

    const outputDir = resolve(options.output);
    mkdirSync(outputDir, { recursive: true });

    try {
      const { parseCopybook } = await import('../parser/index.js');
      const { generateOpenApiSpec, generateTypeScriptTypes } = await import('../codegen/index.js');
      const source = readFileSync(filePath, 'utf-8');
      const ast = parseCopybook(source);

      if (options.format === 'openapi' || options.format === 'both') {
        const spec = generateOpenApiSpec(ast);
        const outPath = join(outputDir, 'openapi.json');
        writeFileSync(outPath, JSON.stringify(spec, null, 2), 'utf-8');
        console.log(chalk.green(`OpenAPI spec written to ${outPath}`));
      }

      if (options.format === 'typescript' || options.format === 'both') {
        const types = generateTypeScriptTypes(ast);
        const outPath = join(outputDir, 'types.ts');
        writeFileSync(outPath, types, 'utf-8');
        console.log(chalk.green(`TypeScript types written to ${outPath}`));
      }

      console.log(chalk.green('\nGeneration complete!'));
    } catch (err) {
      console.error(chalk.red(`Error generating output: ${(err as Error).message}`));
      process.exit(1);
    }
  });

program
  .command('serve [directory]')
  .description('Start an HTTP server exposing COBOL programs as REST APIs')
  .option('-p, --port <port>', 'Port to listen on', '3000')
  .action(async (directory: string | undefined, options: { port: string }) => {
    const dir = resolve(directory ?? '.');
    const port = parseInt(options.port, 10);

    try {
      const { startServer } = await import('../server/index.js');
      console.log(chalk.blue(`Starting COBridge server on port ${port}...`));
      console.log(chalk.gray(`Serving COBOL programs from: ${dir}\n`));
      await startServer({ copybookDir: dir, port });
    } catch (err) {
      console.error(chalk.red(`Error starting server: ${(err as Error).message}`));
      process.exit(1);
    }
  });

program
  .command('init [directory]')
  .description('Scaffold a new COBridge project with example files')
  .action((directory: string | undefined) => {
    const dir = resolve(directory ?? '.');

    console.log(chalk.blue(`Initializing COBridge project in ${dir}...\n`));

    const dirs = [
      join(dir, 'copybooks'),
      join(dir, 'programs'),
      join(dir, 'generated'),
    ];

    for (const d of dirs) {
      mkdirSync(d, { recursive: true });
      console.log(chalk.gray(`  Created ${d}`));
    }

    const exampleCopybook = `      *================================================================*
      * EXAMPLE RECORD - Created by COBridge
      *================================================================*
       01  EXAMPLE-RECORD.
           05  RECORD-ID            PIC 9(10).
           05  RECORD-NAME          PIC X(30).
           05  RECORD-AMOUNT        PIC S9(9)V99 COMP-3.
           05  RECORD-DATE          PIC 9(8).
           05  RECORD-STATUS        PIC X(1).
               88  REC-ACTIVE       VALUE 'A'.
               88  REC-INACTIVE     VALUE 'I'.
`;

    const copybookPath = join(dir, 'copybooks', 'example.cpy');
    if (!existsSync(copybookPath)) {
      writeFileSync(copybookPath, exampleCopybook, 'utf-8');
      console.log(chalk.gray(`  Created ${copybookPath}`));
    }

    const configContent = JSON.stringify({
      name: 'my-cobridge-project',
      copybooks: './copybooks',
      programs: './programs',
      output: './generated',
      server: {
        port: 3000,
      },
    }, null, 2);

    const configPath = join(dir, 'cobridge.json');
    if (!existsSync(configPath)) {
      writeFileSync(configPath, configContent, 'utf-8');
      console.log(chalk.gray(`  Created ${configPath}`));
    }

    console.log(chalk.green('\nProject initialized! Next steps:'));
    console.log(chalk.white('  1. Add your COBOL copybooks to the copybooks/ directory'));
    console.log(chalk.white('  2. Run: cobridge parse copybooks/example.cpy'));
    console.log(chalk.white('  3. Run: cobridge generate copybooks/example.cpy'));
    console.log(chalk.white('  4. Run: cobridge serve'));
  });

program.parse();
