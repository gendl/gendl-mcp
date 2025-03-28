#!/usr/bin/env node

/*
 * mcp-format-wrapper.js
 * 
 * Wrapper script for Gendl MCP integration with improved dynamic tool handling
 */

const http = require('http');
const readline = require('readline');
const fs = require('fs');
const { exec, spawn } = require('child_process');
const path = require('path');

// Configure the Gendl HTTP server details
const GENDL_HOST = '127.0.0.1';
const GENDL_PORT = 9081;
const GENDL_BASE_PATH = '/mcp';

// Docker configuration
const GENDL_DOCKER_IMAGE = 'dcooper8/gendl-ccl';
const GENDL_DOCKER_CONTAINER_NAME = 'gendl-mcp-server';
const CURRENT_DIR = path.resolve('.');

// Lisp initialization code
const LOAD_MCP_SERVICES = true; // Set to false to disable auto-loading

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
  
  // First check if Gendl server is running
  checkGendlServer()
    .then(running => {
      // If server is running, we're good
      if (running) {
        log('Gendl server is already running');
        return Promise.resolve(true);
      }
      
      // Otherwise ensure the Docker image exists
      return ensureDockerImage().then(imageExists => {
        if (!imageExists) {
          throw new Error('Failed to ensure Docker image existence');
        }
        
        log('Gendl server not running. Attempting to start container...');
        return startGendlContainer();
      });
    })
    .then(containerStarted => {
      // At this point either the server was already running
      // or we've started the container successfully (or failed)
    })
    .then(containerStarted => {
      if (!containerStarted) {
        throw new Error('Failed to start Gendl container');
      }
      // Continue with original initialization
      return Promise.all([
        testGendlConnection(),
        fetchAndCacheTools()
      ]);
    })
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
    
    // Add the knowledge base query tool to the tools list if it's not already there
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
      endpointPath = `${GENDL_BASE_PATH}/lisp-eval`;
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

// Check if Gendl server is running
function checkGendlServer() {
  return new Promise(resolve => {
    const options = {
      hostname: GENDL_HOST,
      port: GENDL_PORT,
      path: `${GENDL_BASE_PATH}/ping-gendl`,
      method: 'GET',
      timeout: 2000 // Short timeout for quick check
    };
    
    const req = http.request(options, res => {
      resolve(res.statusCode >= 200 && res.statusCode < 300);
    });
    
    req.on('error', () => {
      // If we get an error, the server is likely not running
      resolve(false);
    });
    
    req.on('timeout', () => {
      req.abort();
      resolve(false);
    });
    
    req.end();
  });
}

// Ensure Docker image exists, pull if needed
function ensureDockerImage() {
  return new Promise((resolve, reject) => {
    log(`Checking if Docker image ${GENDL_DOCKER_IMAGE} exists locally...`);
    
    // Check if the image exists locally
    exec(`docker image inspect ${GENDL_DOCKER_IMAGE}`, (error, stdout, stderr) => {
      if (!error) {
        log('Docker image exists locally');
        resolve(true);
        return;
      }
      
      // Image doesn't exist, try to pull it
      log(`Docker image not found locally. Pulling ${GENDL_DOCKER_IMAGE}...`);
      
      const dockerPull = spawn('docker', ['pull', GENDL_DOCKER_IMAGE]);
      
      dockerPull.stdout.on('data', (data) => {
        log(`Docker pull stdout: ${data.toString().trim()}`);
      });
      
      dockerPull.stderr.on('data', (data) => {
        // This isn't necessarily an error for docker pull
        log(`Docker pull stderr: ${data.toString().trim()}`);
      });
      
      dockerPull.on('close', (code) => {
        if (code === 0) {
          log('Docker image pulled successfully');
          resolve(true);
        } else {
          log(`Failed to pull Docker image. Exit code: ${code}`);
          resolve(false);
        }
      });
      
      dockerPull.on('error', (error) => {
        log(`Error executing docker pull: ${error.message}`);
        resolve(false);
      });
    });
  });
}

// Start Gendl container
function startGendlContainer() {
  return new Promise((resolve, reject) => {
    log(`Starting Gendl container using image ${GENDL_DOCKER_IMAGE}...`);
    log(`Current directory to mount: ${CURRENT_DIR}`);
    
    // Run docker command to start the container
    const dockerRun = spawn('docker', [
      'run',
      '--rm',
      '-d',
      '--name', GENDL_DOCKER_CONTAINER_NAME,
      '-p', `${GENDL_PORT}:${GENDL_PORT}`,
      '-v', `${CURRENT_DIR}:/gendl-mcp`,
      GENDL_DOCKER_IMAGE
    ]);
    
    let stdout = '';
    let stderr = '';
    
    dockerRun.stdout.on('data', (data) => {
      stdout += data.toString();
      log(`Docker stdout: ${data.toString().trim()}`);
    });
    
    dockerRun.stderr.on('data', (data) => {
      stderr += data.toString();
      log(`Docker stderr: ${data.toString().trim()}`);
    });
    
    dockerRun.on('close', (code) => {
      if (code === 0) {
        log(`Docker container started with ID: ${stdout.trim()}`);
        
        // Wait for Gendl server to be ready
        waitForGendlServer(resolve, reject);
      } else if (code === 125) {
        // Special case: container already exists but isn't running
        log('Container already exists. Attempting to start it...');
        exec(`docker start ${GENDL_DOCKER_CONTAINER_NAME}`, (error, stdout, stderr) => {
          if (error) {
            log(`Error starting existing container: ${error.message}`);
            resolve(false);
            return;
          }
          log(`Started existing container: ${stdout.trim()}`);
          waitForGendlServer(resolve, reject);
        });
      } else {
        log(`Failed to start Docker container: ${stderr}`);
        resolve(false);
      }
    });
    
    dockerRun.on('error', (error) => {
      log(`Error starting Docker container: ${error.message}`);
      resolve(false);
    });
  });
}

// Wait for Gendl server to be available
function waitForGendlServer(resolve, reject, attempts = 0) {
  const maxAttempts = 30; // Wait up to 30 seconds
  const interval = 1000; // Check every second
  
  if (attempts >= maxAttempts) {
    log('Timed out waiting for Gendl server to start');
    resolve(false);
    return;
  }
  
  checkGendlServer()
    .then(running => {
      if (running) {
        log(`Gendl server is now running after ${attempts} seconds`);
        containerStartedByWrapper = true;
        
        // If auto-loading is enabled, load MCP services
        if (LOAD_MCP_SERVICES) {
          loadGendlMcpServices()
            .then(success => {
              if (success) {
                log('Successfully loaded Gendl MCP services');
              } else {
                log('Failed to load Gendl MCP services, but container is running');
              }
              resolve(true);
            })
            .catch(error => {
              log(`Error loading Gendl MCP services: ${error.message}`);
              resolve(true); // Still resolve as true since the container is running
            });
        } else {
          resolve(true);
        }
      } else {
        log(`Waiting for Gendl server to start (${attempts}/${maxAttempts})...`);
        setTimeout(() => waitForGendlServer(resolve, reject, attempts + 1), interval);
      }
    })
    .catch(error => {
      log(`Error checking Gendl server status: ${error.message}`);
      setTimeout(() => waitForGendlServer(resolve, reject, attempts + 1), interval);
    });
}

// Test connection to Gendl server
function testGendlConnection() {
  return makeGendlRequest(`${GENDL_BASE_PATH}/ping-gendl`);
}

// Load Gendl MCP services
function loadGendlMcpServices() {
  return new Promise((resolve, reject) => {
    log('Loading Gendl MCP services...');
    
    // Execute Lisp code inside the container to load MCP services
    const lispLoadCommand = `
      (load "/gendl-mcp/init.lisp")
      (in-package :gdl-user)
      (gendl-mcp:start-mcp-server :port ${GENDL_PORT})
    `;
    
    // Ensure the command is properly escaped
    const escapedLispCommand = lispLoadCommand.replace(/"/g, '\\"').trim();
    
    // Execute the command inside the Docker container
    const dockerExecCmd = `docker exec ${GENDL_DOCKER_CONTAINER_NAME} /bin/bash -c "echo \"${escapedLispCommand}\" | ./gendl/ccl64 -l ./gendl/startup.txt"`;
    
    log(`Executing command: ${dockerExecCmd}`);
    
    exec(dockerExecCmd, (error, stdout, stderr) => {
      if (error) {
        log(`Error loading MCP services: ${error.message}`);
        log(`stderr: ${stderr}`);
        resolve(false);
        return;
      }
      
      log(`MCP services loaded successfully`);
      log(`stdout: ${stdout.substring(0, 500)}${stdout.length > 500 ? '...' : ''}`);
      
      if (stderr) {
        log(`stderr: ${stderr}`);
      }
      
      resolve(true);
    });
  });
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

// Global state to track if we started the container
let containerStartedByWrapper = false;

// Store container shutdown function
function shutdownContainer() {
  if (containerStartedByWrapper) {
    log('Shutting down Gendl container that was started by this wrapper...');
    exec(`docker stop ${GENDL_DOCKER_CONTAINER_NAME}`, (error, stdout, stderr) => {
      if (error) {
        log(`Error stopping container: ${error.message}`);
        return;
      }
      log(`Container stopped: ${stdout.trim()}`);
    });
  }
}

// Handle process events
process.on('exit', () => {
  log('Wrapper script exiting');
  shutdownContainer();
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
  log('Received SIGINT signal');
  shutdownContainer();
  // Exit with a short delay to allow container shutdown
  setTimeout(() => process.exit(0), 1000);
});

process.on('SIGTERM', () => {
  log('Received SIGTERM signal');
  shutdownContainer();
  // Exit with a short delay to allow container shutdown
  setTimeout(() => process.exit(0), 1000);
});

log('Gendl MCP wrapper initialized - ready to handle requests');
