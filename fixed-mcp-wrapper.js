#!/usr/bin/env node

/**
 * fixed-mcp-wrapper.js
 * 
 * Improved wrapper script for Gendl MCP integration with fixes for lisp_eval
 * and general HTTP request handling.
 */

const readline = require('readline');
const fs = require('fs');
const http = require('http');
const { exec } = require('child_process');

// Configure the Gendl server details
const GENDL_HOST = '127.0.0.1';
const GENDL_PORT = 9081;
const GENDL_BASE_PATH = '/mcp';

// Path to knowledge base query script
const GENDL_KB_SCRIPT = '/projects/xfer/gendl-mcp/gendl_kb.py';

// Set up logging to file for debugging
const LOG_FILE = '/tmp/fixed-mcp-wrapper.log';
let logStream;

try {
  logStream = fs.createWriteStream(LOG_FILE, { flags: 'a' });
  logStream.write(`\n\n---- STARTING FIXED MCP WRAPPER LOG AT ${new Date().toISOString()} ----\n\n`);
} catch (error) {
  console.error(`Failed to create log file: ${error.message}`);
}

// Logger implementation
const logger = {
  error: (message) => log('ERROR', message),
  warn: (message) => log('WARN', message),
  info: (message) => log('INFO', message),
  debug: (message) => log('DEBUG', message)
};

// Enhanced logging function
const log = (level, message) => {
  const timestamp = new Date().toISOString();
  const logMessage = `[${timestamp}] [${level}] ${message}`;
  
  // Log to stderr
  console.error(`[FIXED-MCP-WRAPPER] ${logMessage}`);
  
  // Log to file if available
  if (logStream) {
    logStream.write(logMessage + '\n');
  }
};

logger.info('Starting Fixed MCP wrapper with improved lisp_eval support');

// Create readline interface for stdin/stdout communication
let rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: false
});

// Handle MCP requests
rl.on('line', (line) => {
  logger.info(`Received: ${line}`);
  
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
        logger.info('Received initialization notification');
        break;
      default:
        // Send method not supported error for any other method
        logger.warn(`Unsupported method: ${request.method}`);
        sendErrorResponse(request, -32601, `Method not supported: ${request.method}`);
    }
  } catch (error) {
    logger.error(`Error processing request: ${error.message}`);
    if (line && line.includes('"id"')) {
      try {
        const id = JSON.parse(line).id;
        sendErrorResponse({ id }, -32603, `Internal error: ${error.message}`);
      } catch (e) {
        logger.error(`Could not extract request ID: ${e.message}`);
      }
    }
  }
});

// Handle MCP initialization
function handleInitialize(request) {
  logger.info('Handling initialize request');
  
  // Send successful initialization response
  const response = {
    jsonrpc: '2.0',
    id: request.id,
    result: {
      protocolVersion: request.params.protocolVersion || '0.1.0',
      capabilities: {
        experimental: {},
        prompts: { listChanged: false },
        resources: { subscribe: false, listChanged: false },
        tools: { listChanged: false }
      },
      serverInfo: {
        name: 'fixed-gendl-mcp',
        version: '1.0.0'
      }
    }
  };
  
  sendResponse(response);
  logger.info('Initialization complete');
}

// Handle tool list requests
function handleToolsList(request) {
  logger.info('Handling tools/list request');
  
  // Define our tools
  const toolsData = {
    tools: [
      {
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
              description: 'The path part of the URL (e.g., /color-map)'
            },
            body: {
              type: 'string',
              description: 'Request body for POST, PUT, etc.'
            },
            headers: {
              type: 'object',
              description: 'HTTP headers as key-value pairs',
              additionalProperties: {
                type: 'string'
              }
            },
            rawResponse: {
              type: 'boolean',
              description: 'If true, return the raw response instead of parsing it',
              default: false
            }
          },
          required: ['path']
        }
      },
      {
        name: 'ping_gendl',
        description: 'Check if the Gendl server is available',
        inputSchema: {
          type: 'object',
          properties: {},
          required: []
        }
      },
      {
        name: 'lisp_eval',
        description: 'Evaluate Lisp code on the Gendl server',
        inputSchema: {
          type: 'object',
          properties: {
            code: {
              type: 'string',
              description: 'The Lisp code to evaluate'
            }
          },
          required: ['code']
        }
      },
      {
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
      }
    ]
  };
  
  sendStandardResponse(request, toolsData);
}

// Handle tool calls
async function handleToolCall(request) {
  const toolName = request.params?.name;
  const args = request.params?.arguments || {};
  
  logger.info(`Handling tool call: ${toolName}`);
  
  try {
    // Special handling for each tool
    if (toolName === 'http_request') {
      return handleHttpRequest(request, args);
    } else if (toolName === 'ping_gendl') {
      return handlePingGendl(request);
    } else if (toolName === 'lisp_eval') {
      return handleLispEval(request, args);
    } else if (toolName === 'query_gendl_kb') {
      return handleKnowledgeBaseQuery(request, args);
    } else {
      // Unknown tool
      sendErrorResponse(request, -32601, `Unknown tool: ${toolName}`);
    }
  } catch (error) {
    logger.error(`Error in tool call: ${error.message}`);
    sendErrorResponse(request, -32603, `Error calling tool: ${error.message}`);
  }
}

// Handle HTTP request with support for all methods and bodies
async function handleHttpRequest(request, args) {
  logger.info(`Handling http_request: ${JSON.stringify(args)}`);
  
  try {
    // Check for required path parameter
    if (!args.path) {
      sendErrorResponse(request, -32602, "Missing required parameter: path");
      return;
    }
    
    // Prepare the request options
    const options = {
      hostname: GENDL_HOST,
      port: GENDL_PORT,
      path: args.path,
      method: args.method || 'GET',
      headers: {
        'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8'
      }
    };
    
    // Add custom headers if provided
    if (args.headers && typeof args.headers === 'object') {
      options.headers = { ...options.headers, ...args.headers };
    }
    
    // If Content-Type is not specified for POST/PUT and we have a body, default to application/json
    if (['POST', 'PUT'].includes(options.method) && args.body && 
        !options.headers['Content-Type'] && !options.headers['content-type']) {
      options.headers['Content-Type'] = 'application/json';
    }
    
    makeHttpRequest(options, args.body, (error, response) => {
      if (error) {
        logger.error(`HTTP request error: ${error.message}`);
        sendErrorResponse(request, -32603, `Error making HTTP request: ${error.message}`);
        return;
      }
      
      // Return the response data
      sendTextResponse(request, response);
    });
  } catch (error) {
    logger.error(`Error in http_request: ${error.message}`);
    sendErrorResponse(request, -32603, `Error making HTTP request: ${error.message}`);
  }
}

// Helper function to make HTTP requests
function makeHttpRequest(options, body, callback) {
  logger.debug(`Making ${options.method} request to http://${options.hostname}:${options.port}${options.path}`);
  logger.debug(`Request headers: ${JSON.stringify(options.headers)}`);
  if (body) {
    logger.debug(`Request body: ${body.substring(0, 500)}${body.length > 500 ? '...' : ''}`);
  }
  
  const req = http.request(options, (res) => {
    let data = '';
    
    res.on('data', (chunk) => {
      data += chunk;
    });
    
    res.on('end', () => {
      logger.debug(`Response status: ${res.statusCode}, Content-Type: ${res.headers['content-type']}`);
      logger.debug(`Response data: ${data.substring(0, 500)}${data.length > 500 ? '...' : ''}`);
      
      // Follow redirects
      if ([301, 302, 303, 307, 308].includes(res.statusCode)) {
        const location = res.headers.location;
        
        if (location) {
          logger.info(`Following redirect to: ${location}`);
          
          // Create new options for the redirect
          const redirectOptions = {
            ...options,
            path: location
          };
          
          // For 303 redirects, use GET
          if (res.statusCode === 303) {
            redirectOptions.method = 'GET';
          }
          
          // Follow the redirect
          return makeHttpRequest(redirectOptions, null, callback);
        }
      }
      
      callback(null, data);
    });
  });
  
  req.on('error', (error) => {
    logger.error(`HTTP request error: ${error.message}`);
    callback(error);
  });
  
  // Send the request body if present
  if (body && ['POST', 'PUT', 'PATCH'].includes(options.method)) {
    req.write(body);
  }
  
  req.end();
}

// Handle simple ping_gendl tool
function handlePingGendl(request) {
  logger.info('Handling ping_gendl request');
  
  const options = {
    hostname: GENDL_HOST,
    port: GENDL_PORT,
    path: `${GENDL_BASE_PATH}/ping-gendl`,
    method: 'GET'
  };
  
  makeHttpRequest(options, null, (error, response) => {
    if (error) {
      logger.error(`Ping Gendl error: ${error.message}`);
      sendErrorResponse(request, -32603, `Error pinging Gendl server: ${error.message}`);
      return;
    }
    
    sendTextResponse(request, response);
  });
}

// Handle lisp_eval tool with direct HTTP request
function handleLispEval(request, args) {
  logger.info(`Handling lisp_eval with code: ${args.code.substring(0, 100)}${args.code.length > 100 ? '...' : ''}`);
  
  try {
    // Check for required parameter
    if (!args.code) {
      sendErrorResponse(request, -32602, "Missing required parameter: code");
      return;
    }
    
    // Create JSON payload for the request
    const payload = JSON.stringify({ code: args.code });
    
    // Direct HTTP POST to lisp-eval endpoint with proper content type
    const options = {
      hostname: GENDL_HOST,
      port: GENDL_PORT,
      path: `${GENDL_BASE_PATH}/lisp-eval`,
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'Content-Length': Buffer.byteLength(payload)
      }
    };
    
    logger.debug(`Sending lisp_eval request with payload: ${payload}`);
    
    makeHttpRequest(options, payload, (error, response) => {
      if (error) {
        logger.error(`lisp_eval error: ${error.message}`);
        sendErrorResponse(request, -32603, `Error evaluating Lisp code: ${error.message}`);
        return;
      }
      
      logger.debug(`Raw lisp_eval response: ${response}`);
      
      // Process the response
      try {
        // First try to parse as JSON
        const jsonData = JSON.parse(response);
        
        // Handle success/error based on the JSON structure
        if ('success' in jsonData) {
          if (jsonData.success) {
            sendTextResponse(request, `Result: ${jsonData.result}`);
          } else {
            sendErrorResponse(request, -32603, `Error evaluating Lisp code: ${jsonData.error || "Unknown error"}`);
          }
        } else {
          // Not a standard success/error format, return as-is
          sendTextResponse(request, JSON.stringify(jsonData, null, 2));
        }
      } catch (e) {
        // Not JSON, treat as text
        logger.debug(`Response is not JSON: ${e.message}`);
        sendTextResponse(request, response);
      }
    });
  } catch (error) {
    logger.error(`Error in lisp_eval: ${error.message}`);
    sendErrorResponse(request, -32603, `Error evaluating Lisp code: ${error.message}`);
  }
}

// Handle queries to the Gendl knowledge base
function handleKnowledgeBaseQuery(request, args) {
  const query = args.query;
  
  if (!query) {
    sendErrorResponse(request, -32602, "Missing required parameter: query");
    return;
  }
  
  logger.info(`Querying Gendl knowledge base with: ${query}`);
  
  // Verify the Python script exists before executing
  fs.access(GENDL_KB_SCRIPT, fs.constants.F_OK | fs.constants.R_OK, (err) => {
    if (err) {
      logger.error(`Python script access error: ${err.message}`);
      sendErrorResponse(request, -32603, `Error accessing knowledge base script: ${err.message}`);
      return;
    }
    
    // Check knowledge base directory
    const kbPath = '/projects/xfer/gendl-mcp/gendl_knowledge_base';
    fs.access(kbPath, fs.constants.F_OK | fs.constants.R_OK, (err) => {
      if (err) {
        logger.error(`Knowledge base directory access error: ${err.message}`);
        sendErrorResponse(request, -32603, `Error accessing knowledge base directory: ${err.message}`);
        return;
      }
      
      logger.info(`Script and KB directory verified, executing Python script`);
      
      // Execute the Python script as a child process with full path
      const command = `python3 ${GENDL_KB_SCRIPT} "${query.replace(/"/g, '\"')}"`;  
      logger.info(`Executing command: ${command}`);
      
      exec(command, (error, stdout, stderr) => {
        if (error) {
          logger.error(`Knowledge base query error: ${error.message}`);
          logger.error(`stderr: ${stderr}`);
          sendErrorResponse(request, -32603, `Error querying knowledge base: ${error.message}`);
          return;
        }
        
        if (stderr) {
          logger.warn(`Knowledge base query stderr: ${stderr}`);
        }
        
        logger.info(`Query successful, response length: ${stdout.length} characters`);
        
        // Send successful response with the query results
        sendTextResponse(request, stdout);
      });
    });  
  });
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
          text: String(text || '')
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

// Helper to send MCP responses
function sendResponse(response) {
  const jsonResponse = JSON.stringify(response);
  logger.info(`Sending response: ${jsonResponse.substring(0, 500)}${jsonResponse.length > 500 ? '...' : ''}`);
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
  logger.info('Wrapper script exiting');
  if (logStream) {
    logStream.end();
  }
  if (rl) rl.close();
});

// Keep the wrapper alive
process.stdin.resume();

// Handle errors
process.on('uncaughtException', (error) => {
  logger.error(`Uncaught exception: ${error.message}`);
  logger.error(error.stack);
});

process.on('unhandledRejection', (reason, promise) => {
  logger.error(`Unhandled rejection at ${promise}: ${reason}`);
});

// Add event handlers to prevent unexpected termination
process.on('SIGINT', () => {
  logger.info('Received SIGINT signal - keeping wrapper alive');
  // Don't exit
});

process.on('SIGTERM', () => {
  logger.info('Received SIGTERM signal - keeping wrapper alive');
  // Don't exit
});

logger.info('Fixed MCP wrapper initialized with improved lisp_eval support - ready to handle requests');
