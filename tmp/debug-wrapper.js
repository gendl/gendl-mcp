#!/usr/bin/env node

/**
 * Enhanced debug version of the Gendl MCP wrapper
 * This has additional error handling and logs more details about what's happening
 */

const http = require('http');
const readline = require('readline');
const fs = require('fs');
const path = require('path');

// Configure the Gendl HTTP server details
const GENDL_HOST = '127.0.0.1';
const GENDL_PORT = 9081;
const GENDL_BASE_PATH = '/mcp';

// Set up debug logging
const DEBUG_LOG_FILE = '/tmp/gendl-mcp-debug.log';
let debugLogStream;

try {
  debugLogStream = fs.createWriteStream(DEBUG_LOG_FILE, { flags: 'a' });
  debugLogStream.write(`\n\n---- STARTING DEBUG LOG AT ${new Date().toISOString()} ----\n\n`);
} catch (error) {
  console.error(`Failed to create debug log file: ${error.message}`);
  // Continue anyway, we'll log to stderr
}

// Enhanced logging function
const log = (message, level = 'INFO') => {
  const timestamp = new Date().toISOString();
  const logMessage = `[${timestamp}] [${level}] ${message}`;
  
  // Log to stderr
  console.error(logMessage);
  
  // Log to file if available
  if (debugLogStream) {
    debugLogStream.write(logMessage + '\n');
  }
};

// Log environment variables
log('Environment variables:', 'DEBUG');
Object.keys(process.env).forEach(key => {
  log(`${key}=${process.env[key]}`, 'DEBUG');
});

log('Starting enhanced debug Gendl MCP wrapper');

// Create readline interface for stdin/stdout communication
let rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: false
});

// Handle MCP requests
rl.on('line', (line) => {
  log(`Received input line: ${line}`);
  
  if (!line || line.trim() === '') {
    log('Received empty line, ignoring', 'WARN');
    return;
  }
  
  try {
    const request = JSON.parse(line);
    log(`Parsed request: ${JSON.stringify(request)}`);
    
    if (request.method === 'initialize') {
      // Handle MCP initialization
      handleInitialize(request);
    } else if (request.method === 'tools/call') {
      // Handle tool calls
      handleToolCall(request);
    } else if (request.method === 'tools/list') {
      // Handle tools list request
      handleToolsList(request);
    } else {
      // Generic method handling for all other methods
      log(`Unhandled method: ${request.method}`, 'WARN');
      sendErrorResponse(request, -32601, `Method not supported: ${request.method}`);
    }
  } catch (error) {
    log(`Error processing request: ${error.message}`, 'ERROR');
    log(`Error stack: ${error.stack}`, 'ERROR');
    
    if (line && line.includes('"id"')) {
      try {
        const id = JSON.parse(line).id;
        sendErrorResponse({ id }, -32603, `Internal error: ${error.message}`);
      } catch (e) {
        log(`Could not extract request ID: ${e.message}`, 'ERROR');
      }
    }
  }
});

// Handle MCP initialization
function handleInitialize(request) {
  log('Handling initialize request');
  
  // Test connection to Gendl server
  testGendlConnection()
    .then((connectionResponse) => {
      log(`Gendl connection test successful: ${connectionResponse}`);
      
      // Send successful initialization response
      const response = {
        jsonrpc: '2.0',
        id: request.id,
        result: {
          protocolVersion: request.params.protocolVersion,
          capabilities: {
            experimental: {},
            prompts: {
              listChanged: false
            },
            resources: {
              subscribe: false,
              listChanged: false
            },
            tools: {
              listChanged: false
            }
          },
          serverInfo: {
            name: 'gendl-mcp-debug',
            version: '1.0.0'
          }
        }
      };
      
      sendResponse(response);
      log('Initialization complete');
    })
    .catch(error => {
      log(`Failed to connect to Gendl server: ${error.message}`, 'ERROR');
      sendErrorResponse(request, -32603, `Could not connect to Gendl server: ${error.message}`);
    });
}

// Handle tool list requests
function handleToolsList(request) {
  log('Handling tools/list request');
  
  // Provide a minimal tools list for debugging
  const response = {
    jsonrpc: '2.0',
    id: request.id,
    result: {
      tools: [
        {
          name: "ping_gendl",
          description: "Check if the Gendl server is available",
          inputSchema: {
            properties: {},
            required: [],
            type: "object"
          }
        }
      ]
    }
  };
  
  sendResponse(response);
}

// Handle tool calls
function handleToolCall(request) {
  log(`Handling tool call: ${request.params.name}`);
  
  const toolName = request.params.name;
  
  if (toolName === 'ping_gendl') {
    makeGendlRequest(`${GENDL_BASE_PATH}/claude/ping`)
      .then(data => {
        const response = {
          jsonrpc: '2.0',
          id: request.id,
          result: { content: [{ type: 'text', text: data }] }
        };
        sendResponse(response);
      })
      .catch(error => {
        log(`Error calling tool ${toolName}: ${error.message}`, 'ERROR');
        sendErrorResponse(request, -32603, `Error calling tool: ${error.message}`);
      });
  } else {
    log(`Unknown tool: ${toolName}`, 'WARN');
    sendErrorResponse(request, -32601, `Unknown tool: ${toolName}`);
  }
}

// Helper to make HTTP requests to Gendl server
function makeGendlRequest(path, method = 'GET', body = null) {
  return new Promise((resolve, reject) => {
    const options = {
      hostname: GENDL_HOST,
      port: GENDL_PORT,
      path: path,
      method: method.toUpperCase(),
      headers: {
        'Accept': 'application/json, text/plain, */*'
      }
    };
    
    if (body) {
      options.headers['Content-Type'] = 'application/json';
      options.headers['Content-Length'] = Buffer.byteLength(body);
    }
    
    log(`Making ${method} request to http://${GENDL_HOST}:${GENDL_PORT}${path}`);
    
    const req = http.request(options, (res) => {
      let responseData = '';
      
      res.on('data', (chunk) => {
        responseData += chunk;
      });
      
      res.on('end', () => {
        log(`Response status: ${res.statusCode}`);
        log(`Response headers: ${JSON.stringify(res.headers)}`);
        log(`Response data: ${responseData}`);
        
        if (res.statusCode >= 200 && res.statusCode < 300) {
          resolve(responseData);
        } else {
          reject(new Error(`HTTP ${res.statusCode}: ${responseData}`));
        }
      });
    });
    
    req.on('error', (error) => {
      log(`HTTP request error: ${error.message}`, 'ERROR');
      reject(error);
    });
    
    if (body) {
      req.write(body);
    }
    
    req.on('timeout', () => {
      log('HTTP request timed out', 'ERROR');
      req.abort();
      reject(new Error('Request timeout'));
    });
    
    req.end();
  });
}

// Test connection to Gendl server
function testGendlConnection() {
  return makeGendlRequest(`${GENDL_BASE_PATH}/claude/ping`);
}

// Helper to send MCP responses
function sendResponse(response) {
  const jsonResponse = JSON.stringify(response);
  log(`Sending response: ${jsonResponse}`);
  console.log(jsonResponse);
}

// Helper to send error responses
function sendErrorResponse(request, code, message) {
  const response = {
    jsonrpc: '2.0',
    id: request.id,
    error: {
      code: code,
      message: message
    }
  };
  sendResponse(response);
}

// Handle process events
process.on('exit', () => {
  log('Wrapper script exiting');
  if (debugLogStream) {
    debugLogStream.end();
  }
  if (rl) rl.close();
});

// Keep the wrapper alive
process.stdin.resume();

// Handle errors
process.on('uncaughtException', (error) => {
  log(`Uncaught exception: ${error.message}`, 'ERROR');
  log(error.stack, 'ERROR');
  // Continue running
});

process.on('unhandledRejection', (reason, promise) => {
  log(`Unhandled rejection at ${promise}: ${reason}`, 'ERROR');
  // Continue running
});

// Add event handlers to prevent unexpected termination
process.on('SIGINT', () => {
  log('Received SIGINT signal - keeping wrapper alive');
  // Don't exit
});

process.on('SIGTERM', () => {
  log('Received SIGTERM signal - keeping wrapper alive');
  // Don't exit
});

log('Debug Gendl MCP wrapper initialized - ready to handle requests');
