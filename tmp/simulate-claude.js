#!/usr/bin/env node

/**
 * This script simulates Claude's communication with the gendl-mcp-wrapper
 * It spawns the wrapper process and sends requests to it in the same way Claude would
 */

const { spawn } = require('child_process');
const fs = require('fs');
const path = require('path');

// Configuration
const WRAPPER_SCRIPT = '/home/dcooper8/gornskew/xfer/gendl-mcp/debug-wrapper.js';
const LOG_FILE = '/tmp/simulate-claude.log';

// Initialize logging
const logStream = fs.createWriteStream(LOG_FILE, { flags: 'a' });
const log = (message) => {
  const timestamp = new Date().toISOString();
  const logMessage = `[${timestamp}] ${message}`;
  console.log(logMessage);
  logStream.write(logMessage + '\n');
};

log('Starting Claude MCP simulation');

// Environment variables for the wrapper process
const env = {
  ...process.env,
  NODE_ENV: 'production',
  DEBUG: '*'
};

log(`Spawning wrapper process: node ${WRAPPER_SCRIPT}`);

// Spawn the wrapper process
const wrapperProcess = spawn('node', [WRAPPER_SCRIPT], {
  env: env,
  stdio: ['pipe', 'pipe', 'pipe']
});

// Set up event handlers for the wrapper process
wrapperProcess.stdout.on('data', (data) => {
  const response = data.toString().trim();
  log(`Received from wrapper: ${response}`);
  
  try {
    // Parse response as JSON
    const jsonResponse = JSON.parse(response);
    
    // Check if we got a successful response to the initialization
    if (jsonResponse.result && jsonResponse.result.serverInfo) {
      log('Initialization successful, sending tools/list request');
      
      // Send a tools/list request
      setTimeout(() => {
        const toolsListRequest = {
          jsonrpc: '2.0',
          id: 2,
          method: 'tools/list',
          params: {}
        };
        sendRequest(toolsListRequest);
      }, 1000);
    }
    
    // Check if we got a successful response to tools/list
    if (jsonResponse.id === 2 && jsonResponse.result) {
      log('Tools list successful, sending tool call');
      
      // Send a tool call request (ping_gendl)
      setTimeout(() => {
        const toolCallRequest = {
          jsonrpc: '2.0',
          id: 3,
          method: 'tools/call',
          params: {
            name: 'ping_gendl',
            arguments: {}
          }
        };
        sendRequest(toolCallRequest);
      }, 1000);
    }
  } catch (error) {
    log(`Error parsing wrapper response: ${error.message}`);
  }
});

wrapperProcess.stderr.on('data', (data) => {
  log(`Wrapper stderr: ${data.toString().trim()}`);
});

wrapperProcess.on('close', (code) => {
  log(`Wrapper process exited with code ${code}`);
  logStream.end();
  process.exit(0);
});

wrapperProcess.on('error', (error) => {
  log(`Error spawning wrapper: ${error.message}`);
  logStream.end();
  process.exit(1);
});

// Function to send a request to the wrapper
function sendRequest(request) {
  const requestStr = JSON.stringify(request);
  log(`Sending to wrapper: ${requestStr}`);
  wrapperProcess.stdin.write(requestStr + '\n');
}

// Send initialization request
const initRequest = {
  jsonrpc: '2.0',
  id: 1,
  method: 'initialize',
  params: {
    protocolVersion: '0.1.0'
  }
};

// Wait a moment before sending the first request
setTimeout(() => {
  sendRequest(initRequest);
}, 1000);

// Set up cleanup
process.on('SIGINT', () => {
  log('Received SIGINT, closing');
  wrapperProcess.kill();
  logStream.end();
  process.exit(0);
});

log('Simulation started, waiting for wrapper responses...');
