#!/usr/bin/env node

/*
 * mcp-format-wrapper.js
 * 
 * Wrapper script for Gendl MCP integration with improved dynamic tool handling
 */

const http = require('http');
const readline = require('readline');
const fs = require('fs');

// Configure the Gendl HTTP server details
const GENDL_HOST = '127.0.0.1';
const GENDL_PORT = 9081;
const GENDL_BASE_PATH = '/mcp';

// Set up logging to file for debugging
const LOG_FILE = '/tmp/gendl-mcp-wrapper.log';
let logStream;

try {
  logStream = fs.createWriteStream(LOG_FILE, { flags: 'a' });
  logStream.write(`\n\n---- STARTING MCP WRAPPER LOG AT ${new Date().toISOString()} ----\n\n`);
} catch (error) {
  console.error(`Failed to create log file: ${error.message}`);
}

// Enhanced logging function
const log = (message) => {
  const timestamp = new Date().toISOString();
  const logMessage = `[${timestamp}] ${message}`;
  
  // Log to stderr
  console.error(`[GENDL-MCP-WRAPPER] ${logMessage}`);
  
  // Log to file if available
  if (logStream) {
    logStream.write(logMessage + '\n');
  }
};

log('Starting Gendl MCP wrapper with dynamic tool handling');

// Cache for tools definition to avoid repeated requests
let toolsCache = null;

// Create readline interface for stdin/stdout communication
let rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: false
});

// Handle MCP requests
rl.on('line', (line) => {
  log(`Received: ${line}`);
  
  try {
    const request = JSON.parse(line);
    
    switch (request.method) {
      case 'initialize':
        handleInitialize(request);
        break;
      case 'tools/call':
        handleToolCall(request);
        break;
      case 'tools/list':
        handleToolsList(request);
        break;
      case 'resources/list':
        sendStandardResponse(request, { resources: [] });
        break;
      case 'prompts/list':
        sendStandardResponse(request, { prompts: [] });
        break;
      case 'notifications/initialized':
        // Just acknowledge notification, no response needed
        log('Received initialization notification');
        break;
      default:
        // Send method not supported error for any other method
        log(`Unsupported method: ${request.method}`);
        sendErrorResponse(request, -32601, `Method not supported: ${request.method}`);
    }
  } catch (error) {
    log(`Error processing request: ${error.message}`);
    if (line && line.includes('"id"')) {
      try {
        const id = JSON.parse(line).id;
        sendErrorResponse({ id }, -32603, `Internal error: ${error.message}`);
      } catch (e) {
        log(`Could not extract request ID: ${e.message}`);
      }
    }
  }
});

// Handle MCP initialization
function handleInitialize(request) {
  log('Handling initialize request');
  
  // Test connection to Gendl server and fetch tools list to cache
  Promise.all([
    testGendlConnection(),
    fetchAndCacheTools()
  ])
    .then(([connectionResponse, _]) => {
      log(`Gendl connection test successful: ${connectionResponse}`);
      
      // Send successful initialization response with strict adherence to MCP format
      const response = {
        jsonrpc: '2.0',
        id: request.id,
        result: {
          protocolVersion: request.params.protocolVersion || '0.1.0',
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
            name: 'gendl-mcp',
            version: '1.0.0'
          }
        }
      };
      
      sendResponse(response);
      log('Initialization complete');
    })
    .catch(error => {
      log(`Failed to connect to Gendl server: ${error.message}`);
      sendErrorResponse(request, -32603, `Could not connect to Gendl server: ${error.message}`);
    });
}

// Fetch and cache the tools definition
async function fetchAndCacheTools() {
  if (toolsCache) {
    return toolsCache;
  }
  
  try {
    const data = await makeGendlRequest(`${GENDL_BASE_PATH}/tools/list`);
    toolsCache = JSON.parse(data);
    log(`Successfully cached ${toolsCache.tools ? toolsCache.tools.length : 0} tools`);
    return toolsCache;
  } catch (error) {
    log(`Error fetching tools list: ${error.message}`);
    throw error;
  }
}

// Handle tool list requests
function handleToolsList(request) {
  log('Handling tools/list request');
  
  fetchAndCacheTools()
    .then(toolsData => {
      sendStandardResponse(request, toolsData);
    })
    .catch(error => {
      log(`Error fetching tools list: ${error.message}`);
      sendErrorResponse(request, -32603, `Error fetching tools list: ${error.message}`);
    });
}

// Handle tool calls with dynamic tool resolution
async function handleToolCall(request) {
  const toolName = request.params?.name;
  const args = request.params?.arguments || {};
  
  log(`Handling tool call: ${toolName}`);
  
  if (!toolName) {
    sendErrorResponse(request, -32602, "Invalid params: missing tool name");
    return;
  }
  
  try {
    // Ensure we have the tools definition
    const toolsDefinition = await fetchAndCacheTools();
    
    // Find the tool definition to see if it exists
    const tool = toolsDefinition.tools.find(t => t.name === toolName);
    
    if (!tool) {
      log(`Unknown tool: ${toolName}`);
      sendErrorResponse(request, -32601, `Unknown tool: ${toolName}`);
      return;
    }
    
    // Validate required parameters
    const requiredParams = tool.inputSchema?.required || [];
    const missingParams = requiredParams.filter(param => !(param in args));
    
    if (missingParams.length > 0) {
      sendErrorResponse(
        request, 
        -32602, 
        `Missing required parameters: ${missingParams.join(', ')}`
      );
      return;
    }
    
    // Convert tool name to endpoint path (convert underscores to hyphens)
    const endpointPath = `${GENDL_BASE_PATH}/${toolName.replace(/_/g, '-')}`;
    
    // Choose HTTP method based on whether we have args or not
    const method = Object.keys(args).length > 0 ? 'POST' : 'GET';
    const body = Object.keys(args).length > 0 ? JSON.stringify(args) : null;
    
    log(`Calling Gendl endpoint: ${endpointPath} with method ${method}`);
    
    const responseData = await makeGendlRequest(endpointPath, method, body);
    
    // Process the response based on the tool type
    handleToolResponse(request, toolName, responseData);
  } catch (error) {
    log(`Error in tool call: ${error.message}`);
    sendErrorResponse(request, -32603, `Error calling tool: ${error.message}`);
  }
}

// Process tool responses based on tool type
function handleToolResponse(request, toolName, responseData) {
  try {
    // Try to parse as JSON first
    const jsonData = JSON.parse(responseData);
    
    // For lisp_eval and similar tools that return success/error status
    if ('success' in jsonData) {
      if (jsonData.success) {
        // Success response
        const responseText = `Result: ${jsonData.result}`;
        sendTextResponse(request, responseText);
      } else {
        // Error from the tool
        sendErrorResponse(
          request, 
          -32603, 
          `Error executing ${toolName}: ${jsonData.error || "Unknown error"}`
        );
      }
    } else {
      // For other JSON responses, return as formatted text
      sendTextResponse(request, `${JSON.stringify(jsonData, null, 2)}`);
    }
  } catch (e) {
    // Not JSON, treat as plain text
    log(`Response is not JSON, treating as text: ${e.message}`);
    sendTextResponse(request, responseData.toString());
  }
}

// Standard text response format
function sendTextResponse(request, text) {
  const response = {
    jsonrpc: '2.0',
    id: request.id,
    result: {
      content: [
        {
          type: 'text',
          text: text
        }
      ]
    }
  };
  sendResponse(response);
}

// Standard response for simple data
function sendStandardResponse(request, data) {
  const response = {
    jsonrpc: '2.0',
    id: request.id,
    result: data
  };
  sendResponse(response);
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
        log(`Response data: ${responseData.substring(0, 500)}${responseData.length > 500 ? '...' : ''}`);
        if (res.statusCode >= 200 && res.statusCode < 300) {
          resolve(responseData);
        } else {
          reject(new Error(`HTTP ${res.statusCode}: ${responseData}`));
        }
      });
    });
    
    req.on('error', (error) => {
      log(`HTTP request error: ${error.message}`);
      reject(error);
    });
    
    req.on('timeout', () => {
      log('HTTP request timed out');
      req.abort();
      reject(new Error('Request timeout'));
    });
    
    if (body) {
      req.write(body);
    }
    
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
  if (logStream) {
    logStream.end();
  }
  if (rl) rl.close();
});

// Keep the wrapper alive
process.stdin.resume();

// Handle errors
process.on('uncaughtException', (error) => {
  log(`Uncaught exception: ${error.message}`);
  log(error.stack);
  // Continue running
});

process.on('unhandledRejection', (reason, promise) => {
  log(`Unhandled rejection at ${promise}: ${reason}`);
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

log('Gendl MCP wrapper initialized - ready to handle requests');
