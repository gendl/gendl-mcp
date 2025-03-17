#!/usr/bin/env node

/*
 * mcp-format-wrapper-fixed.js
 * 
 * Modified version with improved handling for Gendl API responses
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

log('Starting Gendl MCP wrapper with improved handling');

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
    
    if (request.method === 'initialize') {
      // Handle MCP initialization
      handleInitialize(request);
    } else if (request.method === 'tools/call') {
      // Handle tool calls
      handleToolCall(request);
    } else if (request.method === 'tools/list') {
      // Handle tools list request
      handleToolsList(request);
    } else if (request.method === 'resources/list') {
      // Handle resources list request
      handleResourcesList(request);
    } else if (request.method === 'prompts/list') {
      // Handle prompts list request
      handlePromptsList(request);
    } else if (request.method === 'notifications/initialized') {
      // Just acknowledge notification, no response needed
      log('Received initialization notification');
    } else {
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
  
  // Test connection to Gendl server
  testGendlConnection()
    .then((connectionResponse) => {
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

// Handle tool list requests
function handleToolsList(request) {
  log('Handling tools/list request');
  
  // Fetch tools list from Gendl server endpoint
  makeGendlRequest(`${GENDL_BASE_PATH}/tools/list`)
    .then(data => {
      try {
        // Parse the response from Gendl
        const toolsData = JSON.parse(data);
        log(`Successfully retrieved tools from server`);
        
        // Create response with the tools list from Gendl
        const response = {
          jsonrpc: '2.0',
          id: request.id,
          result: toolsData
        };
        
        sendResponse(response);
      } catch (error) {
        log(`Error parsing tools list: ${error.message}`);
        log(`Raw response from server: ${data}`);
        sendErrorResponse(request, -32603, `Error parsing tools list: ${error.message}`);
      }
    })
    .catch(error => {
      log(`Error fetching tools list: ${error.message}`);
      sendErrorResponse(request, -32603, `Error fetching tools list: ${error.message}`);
    });
}

// Handle resources list requests
function handleResourcesList(request) {
  log('Handling resources/list request');
  
  // Strict format for resources list
  const response = {
    jsonrpc: '2.0',
    id: request.id,
    result: {
      resources: []
    }
  };
  
  sendResponse(response);
}

// Handle prompts list requests
function handlePromptsList(request) {
  log('Handling prompts/list request');
  
  // Strict format for prompts list
  const response = {
    jsonrpc: '2.0',
    id: request.id,
    result: {
      prompts: []
    }
  };
  
  sendResponse(response);
}

// Handle tool calls
function handleToolCall(request) {
  const toolName = request.params?.name;
  const args = request.params?.arguments || {};
  
  log(`Handling tool call: ${toolName}`);
  
  if (!toolName) {
    sendErrorResponse(request, -32602, "Invalid params: missing tool name");
    return;
  }
  
  // Map tool names to Gendl HTTP endpoints
  switch (toolName) {
    case 'ping_gendl':
      makeGendlRequest(`${GENDL_BASE_PATH}/claude/ping`)
        .then(data => {
          const response = {
            jsonrpc: '2.0',
            id: request.id,
            result: {
              content: [
                {
                  type: 'text',
                  text: "Gendl MCP server is ready for Claude interaction!"
                }
              ]
            }
          };
          sendResponse(response);
        })
        .catch(error => {
          log(`Error calling ping_gendl: ${error.message}`);
          sendErrorResponse(request, -32603, `Error calling tool: ${error.message}`);
        });
      break;
      
    case 'lisp_eval':
      if (!args.code) {
        sendErrorResponse(request, -32602, "Invalid params: missing code parameter");
        return;
      }
      
      makeGendlRequest(`${GENDL_BASE_PATH}/lisp-eval`, 'POST', JSON.stringify({ code: args.code }))
        .then(data => {
          try {
            // Parse the response from Gendl
            const jsonData = JSON.parse(data);
            
            // Check if we have a success or error
            if (jsonData.success) {
              // Success response - result field contains the result from Lisp
              const resultText = `Result: ${jsonData.result}`;
              
              const response = {
                jsonrpc: '2.0',
                id: request.id,
                result: {
                  content: [
                    {
                      type: 'text',
                      text: resultText
                    }
                  ]
                }
              };
              sendResponse(response);
            } else {
              // Error response - error field contains the error message
              sendErrorResponse(
                request, 
                -32603, 
                `Error executing code: ${jsonData.error || "Unknown Lisp error"}`
              );
            }
          } catch (e) {
            // If we can't parse the JSON, return the raw response
            log(`Error parsing Lisp eval response: ${e.message}`);
            const response = {
              jsonrpc: '2.0',
              id: request.id,
              result: {
                content: [
                  {
                    type: 'text',
                    text: "Raw response: " + data.toString()
                  }
                ]
              }
            };
            sendResponse(response);
          }
        })
        .catch(error => {
          log(`Error calling lisp_eval: ${error.message}`);
          sendErrorResponse(request, -32603, `Error evaluating Lisp code: ${error.message}`);
        });
      break;
      
    default:
      log(`Unknown tool: ${toolName}`);
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
        log(`Response data: ${responseData}`);
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
