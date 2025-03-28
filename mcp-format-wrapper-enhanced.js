#!/usr/bin/env node

/*
 * mcp-format-wrapper-enhanced.js
 * 
 * Wrapper script for Gendl MCP integration with HTTP request support
 */

const http = require('http');
const readline = require('readline');
const fs = require('fs');
const { exec } = require('child_process');

// Configure the Gendl HTTP server details
const GENDL_HOST = '127.0.0.1';
const GENDL_PORT = 9081;
const GENDL_BASE_PATH = '/mcp';

// Path to knowledge base query script
const GENDL_KB_SCRIPT = '/projects/xfer/gendl-mcp/gendl_kb.py';

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

log('Starting Gendl MCP wrapper with HTTP request support');

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
    
    // Add the knowledge base query tool if it's not already there
    if (!toolsCache.tools.some(tool => tool.name === 'query_gendl_kb')) {
      toolsCache.tools.push({
        name: 'query_gendl_kb',
        description: 'Search the Gendl documentation knowledge base for information about Gendl/GDL, a General-purpose Declarative Language',
        inputSchema: {
          type: 'object',
          properties: {
            query: {
              type: 'string',
              description: 'The search query about Gendl/GDL'
            }
          },
          required: ['query']
        }
      });
    }
    
    // Add the HTTP request tool if it's not already there
    if (!toolsCache.tools.some(tool => tool.name === 'http_request')) {
      toolsCache.tools.push({
        name: 'http_request',
        description: 'Make an HTTP request to any endpoint on the Gendl server',
        inputSchema: {
          type: 'object',
          properties: {
            method: {
              type: 'string',
              description: 'HTTP method (GET, POST, PUT, DELETE, etc.)',
              default: 'GET'
            },
            path: {
              type: 'string',
              description: 'The path part of the URL (e.g., /tasty/box)'
            },
            query: {
              type: 'object',
              description: 'Query parameters as key-value pairs',
              additionalProperties: {
                type: 'string'
              }
            },
            headers: {
              type: 'object',
              description: 'HTTP headers as key-value pairs',
              additionalProperties: {
                type: 'string'
              }
            },
            body: {
              type: 'string',
              description: 'Request body for POST, PUT, etc.'
            },
            rawResponse: {
              type: 'boolean',
              description: 'If true, return the raw response instead of parsing it',
              default: false
            }
          },
          required: ['path']
        }
      });
    }
    
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
    // Special handling for query_gendl_kb tool
    if (toolName === 'query_gendl_kb') {
      return handleKnowledgeBaseQuery(request, args);
    }
    
    // Special handling for http_request tool
    if (toolName === 'http_request') {
      return handleHttpRequest(request, args);
    }
    
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
    // Handle special cases for tool names
    let endpointPath;
    if (toolName === 'ping_gendl') {
      endpointPath = `${GENDL_BASE_PATH}/ping-gendl`;
    } else if (toolName === 'lisp_eval') {
      // Use HTTP request handler for lisp_eval to reduce duplication
      return handleLispEval(request, args);
    } else {
      endpointPath = `${GENDL_BASE_PATH}/${toolName.replace(/_/g, '-')}`;
    }
    
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

// Handle lisp_eval tool calls using the HTTP request functionality
async function handleLispEval(request, args) {
  log(`Handling lisp_eval with code: ${args.code.substring(0, 100)}${args.code.length > 100 ? '...' : ''}`);
  
  try {
    // Check for required parameter
    if (!args.code) {
      sendErrorResponse(request, -32602, "Missing required parameter: code");
      return;
    }
    
    // Create HTTP request args
    const httpArgs = {
      method: 'POST',
      path: '/mcp/lisp-eval',
      body: JSON.stringify({ code: args.code }),
      headers: { 'Content-Type': 'application/json' }
    };
    
    // Use the generic HTTP request handler
    const response = await makeHttpRequest(httpArgs);
    
    // Process the response
    if (response.statusCode >= 200 && response.statusCode < 300) {
      try {
        // Try to parse as JSON
        const jsonData = JSON.parse(response.body);
        
        if ('success' in jsonData) {
          if (jsonData.success) {
            // Success response
            const responseText = `Result: ${jsonData.result}`;
            sendTextResponse(request, responseText);
          } else {
            // Error from lisp_eval
            sendErrorResponse(
              request, 
              -32603, 
              `Error executing Lisp code: ${jsonData.error || "Unknown error"}`
            );
          }
        } else {
          // Non-standard JSON response
          sendTextResponse(request, `${JSON.stringify(jsonData, null, 2)}`);
        }
      } catch (e) {
        // Not JSON, treat as plain text
        sendTextResponse(request, response.body);
      }
    } else {
      // HTTP error
      sendErrorResponse(
        request,
        -32603,
        `HTTP ${response.statusCode}: ${response.body}`
      );
    }
  } catch (error) {
    log(`Error in lisp_eval: ${error.message}`);
    sendErrorResponse(request, -32603, `Error evaluating Lisp code: ${error.message}`);
  }
}

// Handle HTTP request tool calls
async function handleHttpRequest(request, args) {
  log(`Handling http_request: ${JSON.stringify(args)}`);
  
  try {
    // Check for required path parameter
    if (!args.path) {
      sendErrorResponse(request, -32602, "Missing required parameter: path");
      return;
    }
    
    // Make the HTTP request
    const response = await makeHttpRequest(args);
    
    // Prepare the response object
    const responseObj = {
      statusCode: response.statusCode,
      headers: response.headers,
      body: response.body
    };
    
    // If there's a redirect, include the location
    if (response.headers.location) {
      responseObj.redirectUrl = response.headers.location;
    }
    
    // Format the response
    if (args.rawResponse) {
      // Return raw response as text
      sendTextResponse(request, response.body);
    } else {
      // Try to parse JSON responses
      try {
        if (response.headers['content-type'] && 
            response.headers['content-type'].includes('application/json')) {
          responseObj.body = JSON.parse(response.body);
        }
      } catch (e) {
        // If parsing fails, keep as string
        log(`Failed to parse JSON response: ${e.message}`);
      }
      
      // Send the full response object
      sendStandardResponse(request, responseObj);
    }
  } catch (error) {
    log(`Error in http_request: ${error.message}`);
    sendErrorResponse(request, -32603, `Error making HTTP request: ${error.message}`);
  }
}

// Helper function to make HTTP requests
async function makeHttpRequest(args) {
  const method = args.method || 'GET';
  let path = args.path;
  
  // Add query parameters if provided
  if (args.query && Object.keys(args.query).length > 0) {
    const queryString = Object.entries(args.query)
      .map(([key, value]) => `${encodeURIComponent(key)}=${encodeURIComponent(value)}`)
      .join('&');
    
    path += path.includes('?') ? `&${queryString}` : `?${queryString}`;
  }
  
  // Set up request options
  const options = {
    hostname: GENDL_HOST,
    port: GENDL_PORT,
    path: path,
    method: method,
    headers: {
      'Accept': 'application/json, text/plain, */*'
    }
  };
  
  // Add custom headers if provided
  if (args.headers && Object.keys(args.headers).length > 0) {
    options.headers = { ...options.headers, ...args.headers };
  }
  
  // Add content-type for requests with body
  if (args.body) {
    if (!options.headers['Content-Type']) {
      options.headers['Content-Type'] = 'application/json';
    }
    options.headers['Content-Length'] = Buffer.byteLength(args.body);
  }
  
  log(`Making ${method} request to http://${GENDL_HOST}:${GENDL_PORT}${path}`);
  log(`Request headers: ${JSON.stringify(options.headers)}`);
  if (args.body) {
    log(`Request body: ${args.body.substring(0, 500)}${args.body.length > 500 ? '...' : ''}`);
  }
  
  return new Promise((resolve, reject) => {
    const req = http.request(options, (res) => {
      let responseData = '';
      
      res.on('data', (chunk) => {
        responseData += chunk;
      });
      
      res.on('end', () => {
        log(`Response status: ${res.statusCode}`);
        log(`Response headers: ${JSON.stringify(res.headers)}`);
        log(`Response data: ${responseData.substring(0, 500)}${responseData.length > 500 ? '...' : ''}`);
        
        // Return full response object
        resolve({
          statusCode: res.statusCode,
          headers: res.headers,
          body: responseData
        });
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
    
    if (args.body) {
      req.write(args.body);
    }
    
    req.end();
  });
}

// Handle queries to the Gendl knowledge base
function handleKnowledgeBaseQuery(request, args) {
  const query = args.query;
  
  if (!query) {
    sendErrorResponse(request, -32602, "Missing required parameter: query");
    return;
  }
  
  log(`Querying Gendl knowledge base with: ${query}`);
  
  // Verify the Python script exists before executing
  fs.access(GENDL_KB_SCRIPT, fs.constants.F_OK | fs.constants.R_OK, (err) => {
    if (err) {
      log(`Python script access error: ${err.message}`);
      sendErrorResponse(request, -32603, `Error accessing knowledge base script: ${err.message}`);
      return;
    }
    
    // Check knowledge base directory
    const kbPath = '/projects/xfer/gendl-mcp/gendl_knowledge_base';
    fs.access(kbPath, fs.constants.F_OK | fs.constants.R_OK, (err) => {
      if (err) {
        log(`Knowledge base directory access error: ${err.message}`);
        sendErrorResponse(request, -32603, `Error accessing knowledge base directory: ${err.message}`);
        return;
      }
      
      log(`Script and KB directory verified, executing Python script`);
      
      // Execute the Python script as a child process with full path
      const command = `python3 ${GENDL_KB_SCRIPT} "${query.replace(/"/g, '\"')}"`;  
      log(`Executing command: ${command}`);
      
      exec(command, (error, stdout, stderr) => {
        if (error) {
          log(`Knowledge base query error: ${error.message}`);
          log(`stderr: ${stderr}`);
          sendErrorResponse(request, -32603, `Error querying knowledge base: ${error.message}`);
          return;
        }
        
        if (stderr) {
          log(`Knowledge base query stderr: ${stderr}`);
        }
        
        log(`Query successful, response length: ${stdout.length} characters`);
        
        // Send successful response with the query results
        sendTextResponse(request, stdout);
      });
    });  
  });
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

// Helper to make HTTP requests to Gendl server (legacy method)
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
  return makeGendlRequest(`${GENDL_BASE_PATH}/ping-gendl`);
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

log('Gendl MCP wrapper initialized with HTTP request support - ready to handle requests');
