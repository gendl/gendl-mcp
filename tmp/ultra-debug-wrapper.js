#!/usr/bin/env node

/*
 * ultra-debug-wrapper.js
 * 
 * Ultra-verbose debugging wrapper for Gendl MCP integration
 */

const http = require('http');
const readline = require('readline');
const fs = require('fs');

// Configure the Gendl HTTP server details
const GENDL_HOST = '127.0.0.1';
const GENDL_PORT = 9081;
const GENDL_BASE_PATH = '/mcp';

// Set up extremely verbose logging
const DEBUG_LOG_FILE = '/tmp/gendl-mcp-ultra-debug.log';
let debugLogStream;

try {
  debugLogStream = fs.createWriteStream(DEBUG_LOG_FILE, { flags: 'a' });
  const startMessage = `\n\n==== STARTING ULTRA-DEBUG LOG AT ${new Date().toISOString()} ====\n\n`;
  console.error(startMessage);
  debugLogStream.write(startMessage);
} catch (error) {
  console.error(`Failed to create debug log file: ${error.message}`);
}

// Enhanced logging function with line numbers and function name
const log = (message, level = 'INFO', functionName = '') => {
  const timestamp = new Date().toISOString();
  const caller = functionName || (new Error().stack.split('\n')[2] || '').trim();
  const logMessage = `[${timestamp}] [${level}] [${caller}] ${message}`;
  
  // Log to stderr
  console.error(logMessage);
  
  // Log to file if available
  if (debugLogStream) {
    debugLogStream.write(logMessage + '\n');
  }
};

// Special function to log objects with full detail
const logObject = (label, obj, level = 'INFO', functionName = '') => {
  try {
    const objStr = JSON.stringify(obj, null, 2);
    log(`${label}: ${objStr}`, level, functionName);
  } catch (e) {
    log(`${label}: [Error serializing object: ${e.message}]`, 'ERROR', functionName);
    log(`Object type: ${typeof obj}`, 'ERROR', functionName);
  }
};

log('Starting ultra-verbose debug wrapper for Gendl MCP');
log('Node.js version: ' + process.version);
log('Current directory: ' + process.cwd());

// Log environment variables
log('Environment variables:');
Object.keys(process.env).forEach(key => {
  log(`  ${key}=${process.env[key]}`);
});

// Create readline interface for stdin/stdout communication
let rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  terminal: false
});

// Handle MCP requests
rl.on('line', (line) => {
  log(`RECEIVED RAW LINE: ${line}`, 'DEBUG', 'rl.on("line")');
  
  if (!line || line.trim() === '') {
    log('Received empty line, ignoring', 'WARN', 'rl.on("line")');
    return;
  }
  
  try {
    const request = JSON.parse(line);
    logObject('Parsed request', request, 'DEBUG', 'rl.on("line")');
    
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
      log('Received initialization notification', 'INFO', 'rl.on("line")');
    } else {
      // Send method not supported error for any other method
      log(`Unsupported method: ${request.method}`, 'WARN', 'rl.on("line")');
      sendErrorResponse(request, -32601, `Method not supported: ${request.method}`);
    }
  } catch (error) {
    log(`Error processing request: ${error.message}`, 'ERROR', 'rl.on("line")');
    log(`Error stack: ${error.stack}`, 'ERROR', 'rl.on("line")');
    log(`Raw line causing error: ${line}`, 'ERROR', 'rl.on("line")');
    
    if (line && line.includes('"id"')) {
      try {
        const id = JSON.parse(line).id;
        sendErrorResponse({ id }, -32603, `Internal error: ${error.message}`);
      } catch (e) {
        log(`Could not extract request ID: ${e.message}`, 'ERROR', 'rl.on("line")');
      }
    }
  }
});

// Handle MCP initialization
function handleInitialize(request) {
  log('Handling initialize request', 'INFO', 'handleInitialize');
  logObject('Initialize request', request, 'DEBUG', 'handleInitialize');
  
  // Test connection to Gendl server
  testGendlConnection()
    .then((connectionResponse) => {
      log(`Gendl connection test successful`, 'INFO', 'handleInitialize');
      log(`Raw connection response: ${connectionResponse}`, 'DEBUG', 'handleInitialize');
      
      // Send successful initialization response
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
            name: 'gendl-mcp-ultra-debug',
            version: '1.0.0'
          }
        }
      };
      
      sendResponse(response);
      log('Initialization complete', 'INFO', 'handleInitialize');
    })
    .catch(error => {
      log(`Failed to connect to Gendl server: ${error.message}`, 'ERROR', 'handleInitialize');
      log(`Error stack: ${error.stack}`, 'ERROR', 'handleInitialize');
      sendErrorResponse(request, -32603, `Could not connect to Gendl server: ${error.message}`);
    });
}

// Handle tool list requests
function handleToolsList(request) {
  log('Handling tools/list request', 'INFO', 'handleToolsList');
  logObject('Tools list request', request, 'DEBUG', 'handleToolsList');
  
  // Provide tools list
  const toolsList = {
    tools: [
      {
        name: "ping_gendl",
        description: "Check if the Gendl server is available",
        inputSchema: {
          type: "object",
          properties: {},
          required: []
        }
      },
      {
        name: "list_objects",
        description: "List all available Gendl objects",
        inputSchema: {
          type: "object",
          properties: {},
          required: []
        }
      },
      {
        name: "lisp_eval",
        description: "Evaluate Lisp code in the Gendl environment",
        inputSchema: {
          type: "object",
          properties: {
            code: {
              type: "string",
              description: "Lisp code to evaluate"
            }
          },
          required: ["code"]
        }
      },
      {
        name: "make_object",
        description: "Create a new Gendl object",
        inputSchema: {
          type: "object",
          properties: {
            type: {
              type: "string",
              description: "The type of Gendl object to create"
            },
            parameters: {
              type: "object",
              description: "Parameters for the object creation"
            }
          },
          required: ["type"]
        }
      },
      {
        name: "send_message",
        description: "Send a message to a Gendl object",
        inputSchema: {
          type: "object",
          properties: {
            objectId: {
              type: "string",
              description: "ID of the object to send the message to"
            },
            message: {
              type: "string",
              description: "The message to send to the object"
            },
            args: {
              type: "object",
              description: "Optional arguments for the message"
            }
          },
          required: ["objectId", "message"]
        }
      }
    ]
  };
  
  const response = {
    jsonrpc: '2.0',
    id: request.id,
    result: toolsList
  };
  
  sendResponse(response);
  log('Sent tools list response', 'INFO', 'handleToolsList');
}

// Handle resources list requests
function handleResourcesList(request) {
  log('Handling resources/list request', 'INFO', 'handleResourcesList');
  logObject('Resources list request', request, 'DEBUG', 'handleResourcesList');
  
  // Empty resources list
  const response = {
    jsonrpc: '2.0',
    id: request.id,
    result: {
      resources: []
    }
  };
  
  sendResponse(response);
  log('Sent empty resources list response', 'INFO', 'handleResourcesList');
}

// Handle prompts list requests
function handlePromptsList(request) {
  log('Handling prompts/list request', 'INFO', 'handlePromptsList');
  logObject('Prompts list request', request, 'DEBUG', 'handlePromptsList');
  
  // Empty prompts list
  const response = {
    jsonrpc: '2.0',
    id: request.id,
    result: {
      prompts: []
    }
  };
  
  sendResponse(response);
  log('Sent empty prompts list response', 'INFO', 'handlePromptsList');
}

// Handle tool calls
function handleToolCall(request) {
  const toolName = request.params?.name;
  const args = request.params?.arguments || {};
  
  log(`Handling tool call: ${toolName}`, 'INFO', 'handleToolCall');
  logObject('Tool call request', request, 'DEBUG', 'handleToolCall');
  logObject('Tool arguments', args, 'DEBUG', 'handleToolCall');
  
  if (!toolName) {
    log('Missing tool name in request', 'ERROR', 'handleToolCall');
    sendErrorResponse(request, -32602, "Invalid params: missing tool name");
    return;
  }
  
  // Map tool names to Gendl HTTP endpoints
  switch (toolName) {
    case 'ping_gendl':
      log('Processing ping_gendl tool call', 'INFO', 'handleToolCall');
      makeGendlRequest(`${GENDL_BASE_PATH}/claude/ping`)
        .then(data => {
          log(`Ping successful, raw response: ${data}`, 'DEBUG', 'handleToolCall');
          
          // Format response
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
          log('Sent ping_gendl response', 'INFO', 'handleToolCall');
        })
        .catch(error => {
          log(`Error calling ping_gendl: ${error.message}`, 'ERROR', 'handleToolCall');
          log(`Error stack: ${error.stack}`, 'ERROR', 'handleToolCall');
          sendErrorResponse(request, -32603, `Error calling tool: ${error.message}`);
        });
      break;
      
    case 'list_objects':
      log('Processing list_objects tool call', 'INFO', 'handleToolCall');
      makeGendlRequest(`${GENDL_BASE_PATH}/objects`)
        .then(data => {
          log(`list_objects successful, raw response: ${data}`, 'DEBUG', 'handleToolCall');
          
          try {
            // Format the output as text
            const jsonData = JSON.parse(data);
            logObject('Parsed objects list', jsonData, 'DEBUG', 'handleToolCall');
            
            let objectsList = "Available Gendl objects:\n";
            
            if (jsonData && Array.isArray(jsonData)) {
              if (jsonData.length === 0) {
                objectsList += "No objects found.";
              } else {
                jsonData.forEach((obj, index) => {
                  objectsList += `${index + 1}. ${obj.id} (${obj.type})\n`;
                });
              }
            } else {
              objectsList += "Unexpected response format: " + JSON.stringify(jsonData);
            }
            
            const response = {
              jsonrpc: '2.0',
              id: request.id,
              result: {
                content: [
                  {
                    type: 'text',
                    text: objectsList
                  }
                ]
              }
            };
            
            sendResponse(response);
            log('Sent list_objects response', 'INFO', 'handleToolCall');
          } catch (e) {
            log(`Error parsing objects list: ${e.message}`, 'ERROR', 'handleToolCall');
            log(`Error stack: ${e.stack}`, 'ERROR', 'handleToolCall');
            log(`Raw data causing error: ${data}`, 'ERROR', 'handleToolCall');
            
            const response = {
              jsonrpc: '2.0',
              id: request.id,
              result: {
                content: [
                  {
                    type: 'text',
                    text: "Error parsing objects list. Raw response: " + data
                  }
                ]
              }
            };
            
            sendResponse(response);
          }
        })
        .catch(error => {
          log(`Error calling list_objects: ${error.message}`, 'ERROR', 'handleToolCall');
          log(`Error stack: ${error.stack}`, 'ERROR', 'handleToolCall');
          sendErrorResponse(request, -32603, `Error calling tool: ${error.message}`);
        });
      break;
      
    case 'lisp_eval':
      if (!args.code) {
        log('Missing code parameter in lisp_eval call', 'ERROR', 'handleToolCall');
        sendErrorResponse(request, -32602, "Invalid params: missing code parameter");
        return;
      }
      
      log(`Processing lisp_eval tool call with code: ${args.code}`, 'INFO', 'handleToolCall');
      
      makeGendlRequest(`${GENDL_BASE_PATH}/lisp-eval`, 'POST', JSON.stringify({ code: args.code }))
        .then(data => {
          log(`lisp_eval HTTP request successful`, 'INFO', 'handleToolCall');
          log(`Raw lisp_eval response: "${data}"`, 'DEBUG', 'handleToolCall');
          
          // If data is empty, provide a default response
          if (!data || data.trim() === '') {
            log('Empty response from lisp_eval, providing default response', 'WARN', 'handleToolCall');
            const response = {
              jsonrpc: '2.0',
              id: request.id,
              result: {
                content: [
                  {
                    type: 'text',
                    text: `Lisp evaluation completed. No output returned.`
                  }
                ]
              }
            };
            sendResponse(response);
            return;
          }
          
          try {
            // Try to parse the response as JSON
            const jsonData = JSON.parse(data);
            logObject('Parsed lisp_eval response', jsonData, 'DEBUG', 'handleToolCall');
            
            // Check if we have a success or error
            if (jsonData.success) {
              // Success response - result field contains the result from Lisp
              const resultText = jsonData.result 
                ? `Result: ${jsonData.result}` 
                : "Evaluation successful (no return value)";
              
              log(`Successful lisp_eval result: ${resultText}`, 'INFO', 'handleToolCall');
              
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
              log(`Lisp evaluation error: ${jsonData.error || "Unknown Lisp error"}`, 'ERROR', 'handleToolCall');
              sendErrorResponse(
                request, 
                -32603, 
                `Error executing code: ${jsonData.error || "Unknown Lisp error"}`
              );
            }
          } catch (e) {
            // If we can't parse the JSON, return the raw response
            log(`Error parsing Lisp eval response as JSON: ${e.message}`, 'ERROR', 'handleToolCall');
            log(`Error stack: ${e.stack}`, 'ERROR', 'handleToolCall');
            log(`Raw data causing JSON parse error: "${data}"`, 'ERROR', 'handleToolCall');
            
            const response = {
              jsonrpc: '2.0',
              id: request.id,
              result: {
                content: [
                  {
                    type: 'text',
                    text: `Raw response from Lisp evaluation: ${data}`
                  }
                ]
              }
            };
            sendResponse(response);
          }
        })
        .catch(error => {
          log(`Error in HTTP request for lisp_eval: ${error.message}`, 'ERROR', 'handleToolCall');
          log(`Error stack: ${error.stack}`, 'ERROR', 'handleToolCall');
          sendErrorResponse(request, -32603, `Error evaluating Lisp code: ${error.message}`);
        });
      break;
      
    case 'make_object':
      if (!args.type) {
        log('Missing type parameter in make_object call', 'ERROR', 'handleToolCall');
        sendErrorResponse(request, -32602, "Invalid params: missing type parameter");
        return;
      }
      
      log(`Processing make_object tool call for type: ${args.type}`, 'INFO', 'handleToolCall');
      logObject('make_object parameters', args.parameters || {}, 'DEBUG', 'handleToolCall');
      
      makeGendlRequest(`${GENDL_BASE_PATH}/make-object`, 'POST', JSON.stringify(args))
        .then(data => {
          log(`make_object HTTP request successful`, 'INFO', 'handleToolCall');
          log(`Raw make_object response: "${data}"`, 'DEBUG', 'handleToolCall');
          
          try {
            // Try to parse the response as JSON
            const jsonData = JSON.parse(data);
            logObject('Parsed make_object response', jsonData, 'DEBUG', 'handleToolCall');
            
            let resultText = "";
            
            if (jsonData.success) {
              log(`Successfully created object with ID: ${jsonData.id}`, 'INFO', 'handleToolCall');
              resultText = `Successfully created ${args.type} object with ID: ${jsonData.id}`;
            } else {
              log(`Failed to create object: ${jsonData.error || "Unknown error"}`, 'ERROR', 'handleToolCall');
              resultText = `Failed to create ${args.type} object: ${jsonData.error || "Unknown error"}`;
            }
            
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
          } catch (e) {
            log(`Error parsing make_object response as JSON: ${e.message}`, 'ERROR', 'handleToolCall');
            log(`Error stack: ${e.stack}`, 'ERROR', 'handleToolCall');
            log(`Raw data causing JSON parse error: "${data}"`, 'ERROR', 'handleToolCall');
            
            const response = {
              jsonrpc: '2.0',
              id: request.id,
              result: {
                content: [
                  {
                    type: 'text',
                    text: `Raw response from make_object: ${data}`
                  }
                ]
              }
            };
            sendResponse(response);
          }
        })
        .catch(error => {
          log(`Error in HTTP request for make_object: ${error.message}`, 'ERROR', 'handleToolCall');
          log(`Error stack: ${error.stack}`, 'ERROR', 'handleToolCall');
          sendErrorResponse(request, -32603, `Error creating object: ${error.message}`);
        });
      break;
      
    case 'send_message':
      if (!args.objectId || !args.message) {
        log('Missing objectId or message parameter in send_message call', 'ERROR', 'handleToolCall');
        sendErrorResponse(request, -32602, "Invalid params: missing objectId or message");
        return;
      }
      
      log(`Processing send_message tool call to object: ${args.objectId}, message: ${args.message}`, 'INFO', 'handleToolCall');
      if (args.args) logObject('send_message additional args', args.args, 'DEBUG', 'handleToolCall');
      
      const path = `${GENDL_BASE_PATH}/theo?object=${args.objectId}&message=${args.message}`;
      const method = args.args ? 'POST' : 'GET';
      const body = args.args ? JSON.stringify(args.args) : null;
      
      makeGendlRequest(path, method, body)
        .then(data => {
          log(`send_message HTTP request successful`, 'INFO', 'handleToolCall');
          log(`Raw send_message response: "${data}"`, 'DEBUG', 'handleToolCall');
          
          try {
            // Try to parse the response as JSON
            const jsonData = JSON.parse(data);
            logObject('Parsed send_message response', jsonData, 'DEBUG', 'handleToolCall');
            
            let resultText = "";
            
            if (jsonData.success) {
              log(`Successfully sent message, result: ${jsonData.result}`, 'INFO', 'handleToolCall');
              resultText = `Message "${args.message}" sent to object ${args.objectId}\nResult: ${jsonData.result}`;
            } else {
              log(`Failed to send message: ${jsonData.error || "Unknown error"}`, 'ERROR', 'handleToolCall');
              resultText = `Failed to send message to ${args.objectId}: ${jsonData.error || "Unknown error"}`;
            }
            
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
          } catch (e) {
            log(`Error parsing send_message response as JSON: ${e.message}`, 'ERROR', 'handleToolCall');
            log(`Error stack: ${e.stack}`, 'ERROR', 'handleToolCall');
            log(`Raw data causing JSON parse error: "${data}"`, 'ERROR', 'handleToolCall');
            
            const response = {
              jsonrpc: '2.0',
              id: request.id,
              result: {
                content: [
                  {
                    type: 'text',
                    text: `Raw response from send_message: ${data}`
                  }
                ]
              }
            };
            sendResponse(response);
          }
        })
        .catch(error => {
          log(`Error in HTTP request for send_message: ${error.message}`, 'ERROR', 'handleToolCall');
          log(`Error stack: ${error.stack}`, 'ERROR', 'handleToolCall');
          sendErrorResponse(request, -32603, `Error sending message: ${error.message}`);
        });
      break;
      
    default:
      log(`Unknown tool: ${toolName}`, 'ERROR', 'handleToolCall');
      sendErrorResponse(request, -32601, `Unknown tool: ${toolName}`);
  }
}

// Helper to make HTTP requests to Gendl server with extreme verbosity
function makeGendlRequest(path, method = 'GET', body = null) {
  return new Promise((resolve, reject) => {
    const options = {
      hostname: GENDL_HOST,
      port: GENDL_PORT,
      path: path,
      method: method.toUpperCase(),
      headers: {
        'Accept': 'application/json, text/plain, */*'
      },
      timeout: 10000 // 10 second timeout
    };
    
    if (body) {
      options.headers['Content-Type'] = 'application/json';
      options.headers['Content-Length'] = Buffer.byteLength(body);
    }
    
    log(`Making ${method} request to http://${GENDL_HOST}:${GENDL_PORT}${path}`, 'INFO', 'makeGendlRequest');
    logObject('Request options', options, 'DEBUG', 'makeGendlRequest');
    if (body) log(`Request body: ${body}`, 'DEBUG', 'makeGendlRequest');
    
    const req = http.request(options, (res) => {
      let responseData = '';
      
      log(`Response status code: ${res.statusCode}`, 'DEBUG', 'makeGendlRequest');
      log(`Response headers: ${JSON.stringify(res.headers)}`, 'DEBUG', 'makeGendlRequest');
      
      res.on('data', (chunk) => {
        log(`Received chunk length: ${chunk.length}`, 'DEBUG', 'makeGendlRequest');
        log(`Chunk data: "${chunk.toString()}"`, 'DEBUG', 'makeGendlRequest');
        responseData += chunk;
      });
      
      res.on('end', () => {
        log(`Response completed, total data length: ${responseData.length}`, 'DEBUG', 'makeGendlRequest');
        log(`Complete response data: "${responseData}"`, 'DEBUG', 'makeGendlRequest');
        
        if (res.statusCode >= 200 && res.statusCode < 300) {
          resolve(responseData);
        } else {
          const error = new Error(`HTTP ${res.statusCode}: ${responseData}`);
          log(`HTTP error response: ${error.message}`, 'ERROR', 'makeGendlRequest');
          reject(error);
        }
      });
    });
    
    req.on('error', (error) => {
      log(`HTTP request error: ${error.message}`, 'ERROR', 'makeGendlRequest');
      log(`Error stack: ${error.stack}`, 'ERROR', 'makeGendlRequest');
      reject(error);
    });
    
    req.on('timeout', () => {
      log('HTTP request timed out', 'ERROR', 'makeGendlRequest');
      req.destroy();
      reject(new Error('Request timeout'));
    });
    
    if (body) {
      req.write(body);
    }
    
    req.end();
    log('Request sent', 'DEBUG', 'makeGendlRequest');
  });
}

// Test connection to Gendl server
function testGendlConnection() {
  log('Testing connection to Gendl server', 'INFO', 'testGendlConnection');
  return makeGendlRequest(`${GENDL_BASE_PATH}/claude/ping`)
    .then(result => {
      log(`Connection test successful: ${result}`, 'INFO', 'testGendlConnection');
      return result;
    })
    .catch(error => {
      log(`Connection test failed: ${error.message}`, 'ERROR', 'testGendlConnection');
      throw error;
    });
}

// Helper to send MCP responses
function sendResponse(response) {
  const jsonResponse = JSON.stringify(response);
  log(`Sending response to Claude`, 'INFO', 'sendResponse');
  logObject('Response object', response, 'DEBUG', 'sendResponse');
  log(`Raw JSON response: ${jsonResponse}`, 'DEBUG', 'sendResponse');
  console.log(jsonResponse);
}

// Helper to send error responses
function sendErrorResponse(request, code, message) {
  log(`Sending error response: code=${code}, message=${message}`, 'ERROR', 'sendErrorResponse');
  
  const response = {
    jsonrpc: '2.0',
    id: request.id,
    error: {
      code: code,
      message: message
    }
  };
  
  logObject('Error response object', response, 'DEBUG', 'sendErrorResponse');
  sendResponse(response);
}

// Handle process events
process.on('exit', () => {
  log('Wrapper script exiting', 'INFO', 'process.on("exit")');
  if (debugLogStream) {
    debugLogStream.end();
  }
  if (rl) rl.close();
});

// Keep the wrapper alive
process.stdin.resume();

// Handle errors
process.on('uncaughtException', (error) => {
  log(`Uncaught exception: ${error.message}`, 'ERROR', 'process.on("uncaughtException")');
  log(`Error stack: ${error.stack}`, 'ERROR', 'process.on("uncaughtException")');
  // Continue running
});

process.on('unhandledRejection', (reason, promise) => {
  log(`Unhandled rejection at ${promise}: ${reason}`, 'ERROR', 'process.on("unhandledRejection")');
  // Continue running
});

// Add event handlers to prevent unexpected termination
process.on('SIGINT', () => {
  log('Received SIGINT signal - keeping wrapper alive', 'INFO', 'process.on("SIGINT")');
  // Don't exit
});

process.on('SIGTERM', () => {
  log('Received SIGTERM signal - keeping wrapper alive', 'INFO', 'process.on("SIGTERM")');
  // Don't exit
});

log('Ultra-debug Gendl MCP wrapper initialized - ready to handle requests', 'INFO');
