// Process manager for long-running COBOL bridge processes.
//
// Manages a persistent bridge process that can handle multiple COBOL
// program invocations without the overhead of spawning a new process
// each time. Supports request queuing, health checking, and restart.

import { ChildProcess, spawn } from 'child_process';
import { EventEmitter } from 'events';
import * as path from 'path';

/**
 * States the bridge process can be in.
 */
export type ProcessState = 'stopped' | 'starting' | 'running' | 'stopping' | 'error';

/**
 * A queued request waiting to be sent to the bridge process.
 */
interface QueuedRequest {
  id: string;
  programName: string;
  data: Buffer;
  resolve: (result: Buffer) => void;
  reject: (error: Error) => void;
  timeout: ReturnType<typeof setTimeout> | null;
}

/**
 * Configuration for the CobridgeProcess.
 */
export interface CobridgeProcessConfig {
  /** Directory containing compiled COBOL modules */
  libraryPath: string;
  /** Maximum concurrent requests (default: 1, COBOL is single-threaded) */
  maxConcurrent?: number;
  /** Health check interval in ms (default: 30000) */
  healthCheckInterval?: number;
  /** Request timeout in ms (default: 30000) */
  requestTimeout?: number;
  /** Auto-restart on crash (default: true) */
  autoRestart?: boolean;
  /** Maximum restart attempts (default: 3) */
  maxRestarts?: number;
  /** Environment variables for the process */
  env?: Record<string, string>;
}

/**
 * Manages a persistent bridge process for calling COBOL programs.
 *
 * Usage:
 *   const bridge = new CobridgeProcess({ libraryPath: './lib' });
 *   await bridge.start();
 *   const result = await bridge.call('MYPROG', inputBuffer);
 *   await bridge.stop();
 */
export class CobridgeProcess extends EventEmitter {
  private config: Required<CobridgeProcessConfig>;
  private process: ChildProcess | null = null;
  private state: ProcessState = 'stopped';
  private queue: QueuedRequest[] = [];
  private activeRequests: Map<string, QueuedRequest> = new Map();
  private healthTimer: ReturnType<typeof setInterval> | null = null;
  private restartCount = 0;
  private requestCounter = 0;
  private responseBuffer = Buffer.alloc(0);

  constructor(config: CobridgeProcessConfig) {
    super();
    this.config = {
      libraryPath: config.libraryPath,
      maxConcurrent: config.maxConcurrent ?? 1,
      healthCheckInterval: config.healthCheckInterval ?? 30000,
      requestTimeout: config.requestTimeout ?? 30000,
      autoRestart: config.autoRestart ?? true,
      maxRestarts: config.maxRestarts ?? 3,
      env: config.env ?? {},
    };
  }

  /**
   * Get the current process state.
   */
  getState(): ProcessState {
    return this.state;
  }

  /**
   * Start the bridge process.
   */
  async start(): Promise<void> {
    if (this.state === 'running') {
      return;
    }

    this.state = 'starting';
    this.emit('starting');

    try {
      const env = {
        ...process.env,
        ...this.config.env,
        COB_LIBRARY_PATH: path.resolve(this.config.libraryPath),
      };

      // Spawn cobcrun as a persistent process
      // In practice, you'd use a custom bridge program that reads requests
      // from stdin and writes responses to stdout in a loop
      this.process = spawn('cobcrun', [], {
        cwd: this.config.libraryPath,
        env,
        stdio: ['pipe', 'pipe', 'pipe'],
      });

      this.process.on('exit', (code, signal) => {
        this.handleProcessExit(code, signal);
      });

      this.process.on('error', (err) => {
        this.state = 'error';
        this.emit('error', err);
        this.rejectAllPending(new Error(`Bridge process error: ${err.message}`));
      });

      if (this.process.stdout) {
        this.process.stdout.on('data', (data: Buffer) => {
          this.handleStdout(data);
        });
      }

      if (this.process.stderr) {
        this.process.stderr.on('data', (data: Buffer) => {
          this.emit('stderr', data.toString('utf-8'));
        });
      }

      this.state = 'running';
      this.restartCount = 0;
      this.emit('started');

      // Start health check timer
      this.startHealthCheck();
    } catch (err: any) {
      this.state = 'error';
      throw new Error(`Failed to start bridge process: ${err.message}`);
    }
  }

  /**
   * Stop the bridge process gracefully.
   */
  async stop(): Promise<void> {
    if (this.state === 'stopped') {
      return;
    }

    this.state = 'stopping';
    this.emit('stopping');
    this.stopHealthCheck();

    // Reject pending requests
    this.rejectAllPending(new Error('Bridge process is shutting down'));

    if (this.process) {
      return new Promise<void>((resolve) => {
        const forceKillTimer = setTimeout(() => {
          if (this.process) {
            this.process.kill('SIGKILL');
          }
        }, 5000);

        this.process!.on('exit', () => {
          clearTimeout(forceKillTimer);
          this.process = null;
          this.state = 'stopped';
          this.emit('stopped');
          resolve();
        });

        // Close stdin to signal the process to exit
        if (this.process!.stdin) {
          this.process!.stdin.end();
        }
        this.process!.kill('SIGTERM');
      });
    }

    this.state = 'stopped';
    this.emit('stopped');
  }

  /**
   * Restart the bridge process.
   */
  async restart(): Promise<void> {
    await this.stop();
    await this.start();
  }

  /**
   * Queue a COBOL program call.
   *
   * @param programName - Name of the COBOL program to call
   * @param data - Input buffer (linkage section data)
   * @returns Promise resolving to the output buffer
   */
  async call(programName: string, data: Buffer): Promise<Buffer> {
    if (this.state !== 'running') {
      throw new Error(`Bridge process is not running (state: ${this.state})`);
    }

    return new Promise<Buffer>((resolve, reject) => {
      const id = `req-${++this.requestCounter}`;

      const request: QueuedRequest = {
        id,
        programName,
        data,
        resolve,
        reject,
        timeout: null,
      };

      // Set request timeout
      request.timeout = setTimeout(() => {
        this.activeRequests.delete(id);
        const idx = this.queue.indexOf(request);
        if (idx >= 0) this.queue.splice(idx, 1);
        reject(new Error(`Request ${id} timed out after ${this.config.requestTimeout}ms`));
      }, this.config.requestTimeout);

      this.queue.push(request);
      this.processQueue();
    });
  }

  /**
   * Check if the bridge process is healthy.
   */
  isHealthy(): boolean {
    return this.state === 'running' && this.process !== null && !this.process.killed;
  }

  /**
   * Get queue statistics.
   */
  getStats(): { state: ProcessState; queueLength: number; activeRequests: number; restartCount: number } {
    return {
      state: this.state,
      queueLength: this.queue.length,
      activeRequests: this.activeRequests.size,
      restartCount: this.restartCount,
    };
  }

  /**
   * Process the request queue, sending requests to the bridge process.
   */
  private processQueue(): void {
    while (
      this.queue.length > 0 &&
      this.activeRequests.size < this.config.maxConcurrent &&
      this.state === 'running'
    ) {
      const request = this.queue.shift()!;
      this.activeRequests.set(request.id, request);
      this.sendRequest(request);
    }
  }

  /**
   * Send a request to the bridge process via stdin.
   * Protocol: 4-byte length prefix (big-endian) + program name (null-terminated) + data
   */
  private sendRequest(request: QueuedRequest): void {
    if (!this.process?.stdin || !this.process.stdin.writable) {
      request.reject(new Error('Bridge process stdin is not writable'));
      this.activeRequests.delete(request.id);
      return;
    }

    const programNameBuf = Buffer.from(request.programName + '\0', 'ascii');
    const header = Buffer.alloc(4);
    header.writeUInt32BE(programNameBuf.length + request.data.length, 0);

    this.process.stdin.write(Buffer.concat([header, programNameBuf, request.data]));
  }

  /**
   * Handle data received from the bridge process stdout.
   * Protocol: 4-byte length prefix (big-endian) + response data
   */
  private handleStdout(data: Buffer): void {
    this.responseBuffer = Buffer.concat([this.responseBuffer, data]);

    // Try to read complete messages
    while (this.responseBuffer.length >= 4) {
      const msgLen = this.responseBuffer.readUInt32BE(0);
      if (this.responseBuffer.length < 4 + msgLen) {
        break; // Incomplete message, wait for more data
      }

      const responseData = this.responseBuffer.subarray(4, 4 + msgLen);
      this.responseBuffer = this.responseBuffer.subarray(4 + msgLen);

      // Resolve the oldest active request (FIFO order)
      const firstKey = this.activeRequests.keys().next().value;
      if (firstKey !== undefined) {
        const request = this.activeRequests.get(firstKey)!;
        this.activeRequests.delete(firstKey);
        if (request.timeout) clearTimeout(request.timeout);
        request.resolve(Buffer.from(responseData));
      }

      // Process more queued requests
      this.processQueue();
    }
  }

  /**
   * Handle bridge process exit.
   */
  private handleProcessExit(code: number | null, signal: string | null): void {
    this.process = null;
    this.stopHealthCheck();

    const wasRunning = this.state === 'running';
    this.state = 'stopped';

    this.rejectAllPending(
      new Error(`Bridge process exited (code: ${code}, signal: ${signal})`)
    );

    this.emit('exit', code, signal);

    // Auto-restart if configured and the exit was unexpected
    if (wasRunning && this.config.autoRestart && this.restartCount < this.config.maxRestarts) {
      this.restartCount++;
      this.emit('restarting', this.restartCount);
      this.start().catch((err) => {
        this.emit('error', new Error(`Auto-restart failed: ${err.message}`));
      });
    }
  }

  /**
   * Reject all pending and active requests.
   */
  private rejectAllPending(error: Error): void {
    for (const request of this.activeRequests.values()) {
      if (request.timeout) clearTimeout(request.timeout);
      request.reject(error);
    }
    this.activeRequests.clear();

    for (const request of this.queue) {
      if (request.timeout) clearTimeout(request.timeout);
      request.reject(error);
    }
    this.queue = [];
  }

  /**
   * Start periodic health checking.
   */
  private startHealthCheck(): void {
    this.stopHealthCheck();
    this.healthTimer = setInterval(() => {
      if (!this.isHealthy()) {
        this.emit('unhealthy');
        if (this.config.autoRestart) {
          this.restart().catch((err) => {
            this.emit('error', new Error(`Health check restart failed: ${err.message}`));
          });
        }
      } else {
        this.emit('healthy');
      }
    }, this.config.healthCheckInterval);
  }

  /**
   * Stop health checking.
   */
  private stopHealthCheck(): void {
    if (this.healthTimer) {
      clearInterval(this.healthTimer);
      this.healthTimer = null;
    }
  }
}
