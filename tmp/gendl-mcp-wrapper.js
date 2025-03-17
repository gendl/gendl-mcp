#!/usr/bin/env node

/*
 * gendl-mcp-wrapper.js
 * 
 * This script serves as an adapter between the Model Context Protocol (MCP)
 * and the Gendl HTTP server. It translates MCP requests into HTTP requests
 * to the Gendl server and sends responses back in MCP format.
 */

const http = require('http');
const readline = require('readline');

// Configure the Gendl HTTP server details
const GENDL_HOST = '127.0.0.1';
const GENDL_PORT = 9081;
const GENDL_BASE_PATH = '/mcp';

// Set up logging to stderr for debugging
const log = (message) => {
  console.error(`[GENDL-MCP-WRAPPER] ${message}`);
};

log('Starting Gendl MCP wrapper');

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
      // Generic method handling for all other methods by converting to HTTP path
      handleGenericMethod(request);
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
  
  makeGendlRequest(`${GENDL_BASE_PATH}/tools/list`)
    .then(data => {
      try {
        const toolsData = JSON.parse(data);
        const response = {
          jsonrpc: '2.0',
          id: request.id,
          result: toolsData
        };
        sendResponse(response);
      } catch (error) {
        log(`Error parsing tools list data: ${error.message}`);
        sendErrorResponse(request, -32603, `Error parsing tools list: ${error.message}`);
      }
    })
    .catch(error => {
      log(`Error getting tools list: ${error.message}`);
      // Provide a default empty tools list as fallback
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
            },
            {
              name: "list_objects",
              description: "List all available Gendl objects",
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
    });
}

// Handle resources list requests
function handleResourcesList(request) {
  log('Handling resources/list request');
  
  makeGendlRequest(`${GENDL_BASE_PATH}/resources/list`)
    .then(data => {
      try {
        const resourcesData = JSON.parse(data);
        const response = {
          jsonrpc: '2.0',
          id: request.id,
          result: resourcesData
        };
        sendResponse(response);
      } catch (error) {
        log(`Error parsing resources list data: ${error.message}`);
        sendErrorResponse(request, -32603, `Error parsing resources list: ${error.message}`);
      }
    })
    .catch(error => {
      log(`Error getting resources list: ${error.message}`);
      // Provide an empty resources list as fallback
      const response = {
        jsonrpc: '2.0',
        id: request.id,
        result: {
          resources: []
        }
      };
      sendResponse(response);
    });
}

// Handle prompts list requests
function handlePromptsList(request) {
  log('Handling prompts/list request');
  
  makeGendlRequest(`${GENDL_BASE_PATH}/prompts/list`)
    .then(data => {
      try {
        const promptsData = JSON.parse(data);
        const response = {
          jsonrpc: '2.0',
          id: request.id,
          result: promptsData
        };
        sendResponse(response);
      } catch (error) {
        log(`Error parsing prompts list data: ${error.message}`);
        sendErrorResponse(request, -32603, `Error parsing prompts list: ${error.message}`);
      }
    })
    .catch(error => {
      log(`Error getting prompts list: ${error.message}`);
      // Provide an empty prompts list as fallback
      const response = {
        jsonrpc: '2.0',
        id: request.id,
        result: {
          prompts: []
        }
      };
      sendResponse(response);
    });
}

// Generic method handler that converts MCP methods to HTTP paths
function handleGenericMethod(request) {
  const method = request.method;
  const params = request.params || {};
  
  // Convert method to HTTP path (e.g., "some/method" -> "/mcp/some/method")
  const path = `${GENDL_BASE_PATH}/${method}`;
  
  log(`Handling generic method: ${method} -> ${path}`);
  
  // Send GET request for simple requests, POST for those with params
  const httpMethod = Object.keys(params).length > 0 ? 'POST' : 'GET';
  const body = Object.keys(params).length > 0 ? JSON.stringify(params) : null;
  
  makeGendlRequest(path, httpMethod, body)
    .then(data => {
      try {
        // Try to parse response as JSON
        const jsonData = JSON.parse(data);
        const response = {
          jsonrpc: '2.0',
          id: request.id,
          result: jsonData
        };
        sendResponse(response);
      } catch (e) {
        // If not JSON, return as text
        const response = {
          jsonrpc: '2.0',
          id: request.id,
          result: { 
            content: [{ type: 'text', text: data }] 
          }
        };
        sendResponse(response);
      }
    })
    .catch(error => {
      log(`Error for generic method ${method}: ${error.message}`);
      sendErrorResponse(request, -32601, `Method not supported: ${method}`);
    });
}

// Handle tool calls
function handleToolCall(request) {
  log(`Handling tool call: ${request.params.name}`);
  
  const toolName = request.params.name;
  const args = request.params.arguments || {};
  
  // Map tool names to Gendl HTTP endpoints
  let path;
  let method = 'GET';
  let body = null;
  
  switch (toolName) {
    case 'ping_gendl':
      path = `${GENDL_BASE_PATH}/claude/ping`;
      break;
    case 'list_objects':
      path = `${GENDL_BASE_PATH}/objects`;
      break;
    case 'make_object':
      path = `${GENDL_BASE_PATH}/make-object`;
      method = 'POST';
      body = JSON.stringify(args);
      break;
    case 'send_message':
      path = `${GENDL_BASE_PATH}/theo?object=${args.objectId}&message=${args.message}`;
      if (args.args) {
        body = JSON.stringify(args.args);
        method = 'POST';
      }
      break;
    default:
      // Try to map unknown tool calls to a generic endpoint
      path = `${GENDL_BASE_PATH}/tools/call/${toolName}`;
      method = 'POST';
      body = JSON.stringify(args);
  }
  
  // Make request to Gendl server
  makeGendlRequest(path, method, body)
    .then(data => {
      try {
        // Try to parse as JSON
        const jsonData = JSON.parse(data);
        const response = {
          jsonrpc: '2.0',
          id: request.id,
          result: { content: [{ type: 'json', json: jsonData }] }
        };
        sendResponse(response);
      } catch (e) {
        // If not JSON, return as text
        const response = {
          jsonrpc: '2.0',
          id: request.id,
          result: { content: [{ type: 'text', text: data }] }
        };
        sendResponse(response);
      }
    })
    .catch(error => {
      log(`Error calling tool ${toolName}: ${error.message}`);
      sendErrorResponse(request, -32603, `Error calling tool: ${error.message}`);
    });
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
        if (res.statusCode >= 200 && res.statusCode < 300) {
          resolve(responseData);
        } else {
          reject(new Error(`HTTP ${res.statusCode}: ${responseData}`));
        }
      });
    });
    
    req.on('error', (error) => {
      reject(error);
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
  if (rl) rl.close();
});

// Keep the wrapper alive
process.stdin.resume();

// Add a heartbeat to keep the process alive and show it's still running
setInterval(() => {
  log('Heartbeat - wrapper is alive');
}, 60000);

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
