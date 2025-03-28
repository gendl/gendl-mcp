#!/usr/bin/env node

/**
 * mcp-gendl-wrapper.js
 * 
 * Main wrapper script for Gendl MCP integration that handles communication
 * between Claude and the Gendl server. This script imports the little-http-proxy
 * module to handle HTTP requests with redirect support.
 */

const readline = require('readline');
const fs = require('fs');
const { exec } = require('child_process');
const { httpProxy } = require('./little-http-proxy');

// Configure the Gendl server details
const GENDL_HOST = '127.0.0.1';
const GENDL_PORT = 9081; // Internal port within the container
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

// Logger implementation
const logger = {
  error: (message) => log('ERROR', message),
  warn: (message) => log('WARN', message),
  info: (message) => log('INFO', message)
};

// Enhanced logging function
const log = (level, message) => {
  const timestamp = new Date().toISOString();
  const logMessage = `[${timestamp}] [${level}] ${message}`;
  
  // Log to stderr
  console.error(`[GENDL-MCP-WRAPPER] ${logMessage}`);
  
  // Log to file if available
  if (logStream) {
    logStream.write(logMessage + '\n');
  }
};

logger.info('Starting Gendl MCP wrapper with HTTP redirect support');

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
  
  // Test connection to Gendl server and fetch tools list to cache
  Promise.all([
    testGendlConnection(),
    fetchAndCacheTools()
  ])
    .then(([connectionResponse, _]) => {
      logger.info(`Gendl connection test successful: ${connectionResponse}`);
      
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
      logger.info('Initialization complete');
    })
    .catch(error => {
      logger.error(`Failed to connect to Gendl server: ${error.message}`);
      sendErrorResponse(request, -32603, `Could not connect to Gendl server: ${error.message}`);
    });
}

// Fetch and cache the tools definition
async function fetchAndCacheTools() {
  if (toolsCache) {
    return toolsCache;
  }
  
  try {
    const response = await httpProxy({
      hostname: GENDL_HOST,
      port: GENDL_PORT,
      path: `${GENDL_BASE_PATH}/tools/list`,
      logger: logger
    });
    
    let toolsData;
    if (typeof response.content === 'string') {
      toolsData = JSON.parse(response.content);
    } else {
      toolsData = response.content;
    }
    
    toolsCache = toolsData;
    
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
    
    logger.info(`Successfully cached ${toolsCache.tools ? toolsCache.tools.length : 0} tools`);
    return toolsCache;
  } catch (error) {
    logger.error(`Error fetching tools list: ${error.message}`);
    throw error;
  }
}

// Handle tool list requests
function handleToolsList(request) {
  logger.info('Handling tools/list request');
  
  fetchAndCacheTools()
    .then(toolsData => {
      sendStandardResponse(request, toolsData);
    })
    .catch(error => {
      logger.error(`Error fetching tools list: ${error.message}`);
      sendErrorResponse(request, -32603, `Error fetching tools list: ${error.message}`);
    });
}

// Handle tool calls with dynamic tool resolution
async function handleToolCall(request) {
  const toolName = request.params?.name;
  const args = request.params?.arguments || {};
  
  logger.info(`Handling tool call: ${toolName}`);
  
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
      logger.warn(`Unknown tool: ${toolName}`);
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
      // Use dedicated handler for lisp_eval
      return handleLispEval(request, args);
    } else {
      endpointPath = `${GENDL_BASE_PATH}/${toolName.replace(/_/g, '-')}`;
    }
    
    // Choose HTTP method based on whether we have args or not
    const method = Object.keys(args).length > 0 ? 'POST' : 'GET';
    const body = Object.keys(args).length > 0 ? JSON.stringify(args) : null;
    
    logger.info(`Calling Gendl endpoint: ${endpointPath} with method ${method}`);
    
    const response = await httpProxy({
      hostname: GENDL_HOST,
      port: GENDL_PORT,
      path: endpointPath,
      method: method,
      body: body,
      headers: body ? { 'Content-Type': 'application/json' } : {},
      logger: logger
    });
    
    // Process the response based on the tool type
    handleToolResponse(request, toolName, response);
  } catch (error) {
    logger.error(`Error in tool call: ${error.message}`);
    sendErrorResponse(request, -32603, `Error calling tool: ${error.message}`);
  }
}

// Handle lisp_eval tool calls
async function handleLispEval(request, args) {
  logger.info(`Handling lisp_eval with code: ${args.code.substring(0, 100)}${args.code.length > 100 ? '...' : ''}`);
  
  try {
    // Check for required parameter
    if (!args.code) {
      sendErrorResponse(request, -32602, "Missing required parameter: code");
      return;
    }
    
    // Make request to the Gendl server
    const response = await httpProxy({
      hostname: GENDL_HOST,
      port: GENDL_PORT,
      path: `${GENDL_BASE_PATH}/lisp-eval`,
      method: 'POST',
      body: JSON.stringify({ code: args.code }),
      headers: { 'Content-Type': 'application/json' },
      logger: logger
    });
    
    // Process the response
    if (response.statusCode >= 200 && response.statusCode < 300) {
      try {
        // Try to parse as JSON if content is a string
        const jsonData = typeof response.content === 'string' 
          ? JSON.parse(response.content) 
          : response.content;
        
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
        sendTextResponse(request, response.content);
      }
    } else {
      // HTTP error
      sendErrorResponse(
        request,
        -32603,
        `HTTP ${response.statusCode}: ${response.content}`
      );
    }
  } catch (error) {
    logger.error(`Error in lisp_eval: ${error.message}`);
    sendErrorResponse(request, -32603, `Error evaluating Lisp code: ${error.message}`);
  }
}

// Handle HTTP request tool calls
async function handleHttpRequest(request, args) {
  logger.info(`Handling http_request: ${JSON.stringify(args)}`);
  
  try {
    // Check for required path parameter
    if (!args.path) {
      sendErrorResponse(request, -32602, "Missing required parameter: path");
      return;
    }
    
    // Make the HTTP request
    const response = await httpProxy({
      hostname: GENDL_HOST,
      port: GENDL_PORT,
      path: args.path,
      method: args.method || 'GET',
      query: args.query,
      headers: args.headers,
      body: args.body,
      rawResponse: args.rawResponse,
      logger: logger
    });
    
    // Format the response for raw responses
    if (args.rawResponse) {
      // Return raw response as text
      sendTextResponse(request, response.content);
    } else {
      // CRITICAL FIX: Skip the response object entirely and just use text response for HTML
      try {
        // Log detailed information about the response for debugging
        logger.info(`Response content type: ${typeof response.content}`);
        logger.info(`Response statusCode: ${response.statusCode}`);
        logger.info(`Response has redirectUrl: ${response.redirectUrl ? 'yes' : 'no'}`);
        
        // For HTML content or any string content, simply return it as a text response
        // instead of trying to use the response object structure
        if (typeof response.content === 'string') {
          // Include basic details about the request in the text response
          const textResponse = `HTTP ${response.statusCode} response from ${args.path}:\n\n${response.content}`;
          sendTextResponse(request, textResponse);
          return;
        }
        
        // For non-string content, create a safer response
        let safeContent;
        if (response.content === null || response.content === undefined) {
          safeContent = '';
        } else if (typeof response.content === 'object') {
          try {
            safeContent = JSON.stringify(response.content);
          } catch (e) {
            safeContent = `[Object representation failed]`;
          }
        } else {
          safeContent = String(response.content);
        }
        
        // Return this as a text response too
        sendTextResponse(request, safeContent);
      } catch (processingError) {
        // If there's an error processing the response, log it and return a safer response
        logger.error(`Error processing HTTP response: ${processingError.message}`);
        sendTextResponse(request, `Error processing HTTP response: ${processingError.message}`);
      }
    }
  } catch (error) {
    logger.error(`Error in http_request: ${error.message}`);
    sendErrorResponse(request, -32603, `Error making HTTP request: ${error.message}`);
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

// Process tool responses based on tool type
function handleToolResponse(request, toolName, response) {
  try {
    // Extract content from response
    const content = response.content;
    
    // For tools that return status and result directly
    if (typeof content === 'object' && content !== null && 'success' in content) {
      if (content.success) {
        // Success response
        const responseText = `Result: ${content.result}`;
        sendTextResponse(request, responseText);
      } else {
        // Error from the tool
        sendErrorResponse(
          request, 
          -32603, 
          `Error executing ${toolName}: ${content.error || "Unknown error"}`
        );
      }
    } else {
      // For other responses, return as formatted text
      try {
        const responseText = typeof content === 'object' && content !== null
          ? JSON.stringify(content, null, 2) 
          : String(content || '');
        
        sendTextResponse(request, responseText);
      } catch (stringifyError) {
        // If JSON.stringify fails, send a safe fallback
        sendTextResponse(request, `[Content could not be stringified: ${stringifyError.message}]`);
      }
    }
  } catch (e) {
    // Handle unexpected response formats
    logger.error(`Error processing response: ${e.message}`);
    sendTextResponse(request, `[Error processing response: ${e.message}]`);
  }
}

// Test connection to Gendl server
async function testGendlConnection() {
  try {
    const response = await httpProxy({
      hostname: GENDL_HOST,
      port: GENDL_PORT,
      path: `${GENDL_BASE_PATH}/ping-gendl`,
      logger: logger
    });
    
    return response.content;
  } catch (error) {
    logger.error(`Gendl server connection test failed: ${error.message}`);
    throw error;
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
  // Continue running
});

process.on('unhandledRejection', (reason, promise) => {
  logger.error(`Unhandled rejection at ${promise}: ${reason}`);
  // Continue running
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

logger.info('Gendl MCP wrapper initialized with HTTP redirect support - ready to handle requests');
