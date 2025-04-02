#!/usr/bin/env node

/**
 * enhanced-mcp-wrapper.js
 * 
 * Copyright (C) 2025 Gendl Project Contributors
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 * 
 * This file is part of the Gendl Model Context Protocol integration.
 * 
 * Enhanced wrapper script for Gendl MCP integration with:
 * - Configurable Gendl host and port via environment variables or CLI arguments
 * - Docker container management for local deployments
 * - Support for running inside a container or directly on the host
 * - Support for mounting volumes into the Gendl container
 */

const readline = require('readline');
const fs = require('fs');
const http = require('http');
const { exec, execSync, spawn } = require('child_process');
const net = require('net');
const path = require('path');
const os = require('os');
const { program } = require('commander');

// Helper function to collect multiple mount options
function collect(value, previous) {
  return previous.concat([value]);
}

// Parse command line arguments
program
  .option('-H, --host <host>', 'Gendl server host (default: 127.0.0.1)')
  .option('--swank-host-port <port>', 'SWANK port on host system (default: 5200)')
  .option('--http-host-port <port>', 'HTTP port on host system (default: 10080)')
  .option('--https-host-port <port>', 'HTTPS port on host system (default: 10443)')
  .option('--telnet-host-port <port>', 'TELNET port on host system (default: 5023)')
  .option('--http-port <port>', 'HTTP port inside container (default: 9080)')
  .option('--https-port <port>', 'HTTPS port inside container (default: 9443)')
  .option('--swank-port <port>', 'SWANK port inside container (default: 4200)')
  .option('--telnet-port <port>', 'TELNET port inside container (default: 4023)')
  .option('--docker-image <image>', 'Docker image for Gendl (default: auto-detected from current branch)')
  .option('--lisp-impl <impl>', 'Lisp implementation to use, ccl or sbcl (default: ccl)')
  .option('--no-auto-start', 'Do not auto-start Gendl docker container if not running')
  .option('--docker-socket <path>', 'Path to docker socket (default: /var/run/docker.sock)')
  .option('--log-file <path>', 'Path to log file (default: /tmp/enhanced-mcp-wrapper.log)')
  .option('--debug', 'Enable debug logging')
  .option('--mount <mounts...>', 'Mount volumes in format "src:dst" (can specify multiple times)', collect, [])
  .option('--start-http', 'Start HTTP service in Gendl container (default: true)')
  .option('--start-https', 'Start HTTPS service in Gendl container (default: false)')
  .option('--start-swank', 'Start SWANK service in Gendl container (default: true)')
  .option('--start-telnet', 'Start TELNET service in Gendl container (default: false)')
  .parse(process.argv);

const options = program.opts();

// Configure the Gendl server details - Use CLI args, env vars, or defaults
const GENDL_HOST = options.host || process.env.GENDL_HOST || '127.0.0.1';

// Host ports (ports exposed on the host system)
const SWANK_HOST_PORT = parseInt(options.swankHostPort || process.env.SWANK_HOST_PORT || '5200', 10);
const HTTP_HOST_PORT = parseInt(options.httpHostPort || process.env.HTTP_HOST_PORT || '10080', 10);
const HTTPS_HOST_PORT = parseInt(options.httpsHostPort || process.env.HTTPS_HOST_PORT || '10443', 10);
const TELNET_HOST_PORT = parseInt(options.telnetHostPort || process.env.TELNET_HOST_PORT || '5023', 10);

// Internal ports (inside the container)
const INTERNAL_HTTP_PORT = parseInt(options.httpPort || process.env.HTTP_PORT || '9080', 10);
const INTERNAL_HTTPS_PORT = parseInt(options.httpsPort || process.env.HTTPS_PORT || '9443', 10);
const INTERNAL_SWANK_PORT = parseInt(options.swankPort || process.env.SWANK_PORT || '4200', 10);
const INTERNAL_TELNET_PORT = parseInt(options.telnetPort || process.env.TELNET_PORT || '4023', 10);

// Service startup flags
const START_HTTP = options.startHttp !== false && (process.env.START_HTTP !== 'false'); // Default to true like SWANK
const START_HTTPS = options.startHttps || process.env.START_HTTPS === 'true' || false;
const START_SWANK = options.startSwank !== false && (process.env.START_SWANK !== 'false'); // Default to true
const START_TELNET = options.startTelnet || process.env.START_TELNET === 'true' || false;

// Other configuration
// Version information
const VERSION = '1.0.1';

// Constants for Docker image configuration
const DEFAULT_IMPL = 'ccl';
const DEFAULT_BRANCH = 'master';

// Get Lisp implementation from arguments or environment variable
const LISP_IMPL = (options.lispImpl || process.env.GENDL_LISP_IMPL || DEFAULT_IMPL).toLowerCase();

// Validate Lisp implementation
const SUPPORTED_IMPLS = ['ccl', 'sbcl'];
if (!SUPPORTED_IMPLS.includes(LISP_IMPL)) {
  logger.warn(`Unsupported Lisp implementation: ${LISP_IMPL}, defaulting to ${DEFAULT_IMPL}`);
}

// Function to get the current branch name
function getCurrentBranch() {
  try {
    // Try to read the current branch from git
    const gitHeadContent = fs.readFileSync(path.join(__dirname, '..', '.git', 'HEAD'), 'utf8').trim();
    // Check if HEAD points to a branch reference
    if (gitHeadContent.startsWith('ref: refs/heads/')) {
      // Extract branch name from ref
      return gitHeadContent.substring('ref: refs/heads/'.length);
    }
    logger.warn('Git HEAD does not point to a branch reference, using default branch');
    return DEFAULT_BRANCH;
  } catch (error) {
    logger.warn(`Could not determine current branch: ${error.message}, using default branch`);
    return DEFAULT_BRANCH;
  }
}

// Get branch name and attempt to use corresponding Docker image or fall back to master
function getValidBranchName() {
  try {
    // Try to read the current branch from git
    const currentBranch = getCurrentBranch();
    logger.info(`Current git branch detected: ${currentBranch}`);
    return currentBranch;
  } catch (error) {
    logger.warn(`Could not determine current branch: ${error.message}, using default branch`);
    return DEFAULT_BRANCH;
  }
}

// Construct Docker image name
function constructDockerImageName() {
  const branchName = getValidBranchName();
  // Convert any slashes in branch name to double hyphens for Docker image tag
  const formattedBranch = branchName.replace(/\//g, '--');
  const impl = SUPPORTED_IMPLS.includes(LISP_IMPL) ? LISP_IMPL : DEFAULT_IMPL;
  return `dcooper8/gendl:${formattedBranch}-${impl}`;
}

const GENDL_DOCKER_IMAGE = options.dockerImage || process.env.GENDL_DOCKER_IMAGE || constructDockerImageName();
const AUTO_START = options.autoStart !== false && (process.env.GENDL_AUTO_START !== 'false');
const DOCKER_SOCKET = options.dockerSocket || process.env.DOCKER_SOCKET || '/var/run/docker.sock';
const DEBUG_MODE = options.debug || process.env.DEBUG_GENDL === 'true';
const MOUNTS = options.mount || []; // Mount points from command line
const ENV_MOUNTS = process.env.GENDL_MOUNTS ? process.env.GENDL_MOUNTS.split(',') : []; // Mount points from env
const ALL_MOUNTS = [...MOUNTS, ...ENV_MOUNTS];

// Path to knowledge base query script - relative to current script
const GENDL_KB_SCRIPT = path.join(__dirname, 'gendl_kb.py');
const GENDL_BASE_PATH = process.env.GENDL_BASE_PATH || '/mcp';

// Set up logging to file for debugging
const LOG_FILE = options.logFile || process.env.GENDL_LOG_FILE || '/tmp/enhanced-mcp-wrapper.log';
let logStream;

try {
  logStream = fs.createWriteStream(LOG_FILE, { flags: 'a' });
  logStream.write(`\n\n---- STARTING ENHANCED MCP WRAPPER LOG AT ${new Date().toISOString()} ----\n\n`);
} catch (error) {
  console.error(`Failed to create log file: ${error.message}`);
}

// Logger implementation
const logger = {
  error: (message) => log('ERROR', message),
  warn: (message) => log('WARN', message),
  info: (message) => log('INFO', message),
  debug: (message) => DEBUG_MODE ? log('DEBUG', message) : undefined
};

// Enhanced logging function
const log = (level, message) => {
  const timestamp = new Date().toISOString();
  const logMessage = `[${timestamp}] [${level}] ${message}`;
  
  // Log to stderr
  console.error(`[ENHANCED-MCP-WRAPPER] ${logMessage}`);
  
  // Log to file if available
  if (logStream) {
    logStream.write(logMessage + '\n');
  }
};

logger.info(`Starting Enhanced MCP wrapper with Gendl host: ${GENDL_HOST}, SWANK port: ${SWANK_HOST_PORT}, HTTP port: ${HTTP_HOST_PORT}`);
logger.info(`Auto-start is ${AUTO_START ? 'enabled' : 'disabled'}, Docker image: ${GENDL_DOCKER_IMAGE}`);
if (ALL_MOUNTS.length > 0) {
  logger.info(`Configured mounts: ${ALL_MOUNTS.join(', ')}`);
}

// First check if we can connect to the Gendl server
checkGendlAvailability()
  .then(available => {
    if (available) {
      logger.info(`Gendl service already available at ${GENDL_HOST}:${HTTP_HOST_PORT}`);
      
      // If we have a local service and we could auto-start, warn about all ignored settings
      const isLocalHost = (GENDL_HOST === '127.0.0.1' || GENDL_HOST === 'localhost');
      const wouldAutoStart = AUTO_START && isLocalHost;
      const hasCustomConfig = options.dockerImage || options.lispImpl || 
                             options.mount?.length > 0 || 
                             options.startHttp !== undefined || 
                             options.startHttps !== undefined ||
                             options.startSwank !== undefined ||
                             options.startTelnet !== undefined ||
                             options.autoStart === false ||
                             options.httpPort || options.httpsPort || 
                             options.swankPort || options.telnetPort ||
                             options.dockerSocket;
      
      if (wouldAutoStart && hasCustomConfig) {
        logger.warn(`Using existing service at ${GENDL_HOST}:${HTTP_HOST_PORT}.`);
        logger.warn('The following settings will be ignored when an existing service is detected:');
        
        // List all ignored settings that were provided
        if (options.dockerImage) logger.warn('- Docker image (--docker-image)');
        if (options.lispImpl) logger.warn('- Lisp implementation (--lisp-impl)');
        if (options.mount?.length > 0) logger.warn('- Mount points (--mount)');
        if (options.startHttp !== undefined) logger.warn('- HTTP service control (--start-http)');
        if (options.startHttps !== undefined) logger.warn('- HTTPS service control (--start-https)');
        if (options.startSwank !== undefined) logger.warn('- SWANK service control (--start-swank)');
        if (options.startTelnet !== undefined) logger.warn('- TELNET service control (--start-telnet)');
        if (options.autoStart === false) logger.warn('- Auto-start control (--no-auto-start)');
        if (options.httpPort) logger.warn('- Internal HTTP port (--http-port)');
        if (options.httpsPort) logger.warn('- Internal HTTPS port (--https-port)');
        if (options.swankPort) logger.warn('- Internal SWANK port (--swank-port)');
        if (options.telnetPort) logger.warn('- Internal TELNET port (--telnet-port)');
        if (options.dockerSocket) logger.warn('- Docker socket path (--docker-socket)');
      }
      
      // Start the MCP wrapper once Gendl is available
      startMcpWrapper();
    } else if (shouldStartGendlContainer()) {
      logger.info(`Gendl server not available, attempting to start container`);
      return startGendlContainer();
    } else {
      logger.error(`Gendl server not available at ${GENDL_HOST}:${HTTP_HOST_PORT} and auto-start is disabled or not a local host`);
      process.exit(1);
    }
    return Promise.resolve();
  })
  .then(() => {
    // If we're here from starting a container, start the MCP wrapper
    if (!isMcpWrapperStarted) {
      startMcpWrapper();
    }
  })
  .catch(error => {
    logger.error(`Failed to start MCP wrapper: ${error.message}`);
    process.exit(1);
  });

// Check if the Gendl server is available
function checkGendlAvailability() {
  return new Promise((resolve) => {
    // Check HTTP port first as it's the critical service for MCP operation
    logger.info(`Checking if Gendl HTTP service is available at ${GENDL_HOST}:${HTTP_HOST_PORT}`);
    
    const socket = new net.Socket();
    const timeout = 5000; // 5 second timeout
    
    // Set a timeout to abort the connection attempt
    socket.setTimeout(timeout);
    
    socket.on('connect', () => {
      logger.info(`Successfully connected to Gendl HTTP service at ${GENDL_HOST}:${HTTP_HOST_PORT}`);
      socket.destroy();
      
      // If user provided specific options but we're using existing service, warn them about ignored options
      if ((options.lispImpl || options.dockerImage || 
           options.mount?.length > 0 || 
           options.startHttp !== undefined || 
           options.startHttps !== undefined ||
           options.startSwank !== undefined ||
           options.startTelnet !== undefined ||
           options.autoStart === false ||
           options.httpPort || options.httpsPort || 
           options.swankPort || options.telnetPort ||
           options.dockerSocket) && 
          (GENDL_HOST !== '127.0.0.1' && GENDL_HOST !== 'localhost')) {
        logger.warn(`Configuration options are being ignored because an existing service was detected at ${GENDL_HOST}:${HTTP_HOST_PORT}`);
      }
      
      resolve(true);
    });
    
    socket.on('timeout', () => {
      logger.warn(`Connection attempt to Gendl HTTP service timed out after ${timeout}ms`);
      socket.destroy();
      
      // If HTTP service not available, check SWANK as a fallback (for developer usage)
      checkSwankAvailability().then(swankAvailable => {
        if (swankAvailable) {
          logger.warn(`HTTP service not detected, but SWANK is available. MCP may have limited functionality.`);
        }
        resolve(swankAvailable);
      });
    });
    
    socket.on('error', (error) => {
      logger.warn(`Failed to connect to Gendl HTTP service: ${error.message}`);
      socket.destroy();
      
      // If HTTP service not available, check SWANK as a fallback (for developer usage)
      checkSwankAvailability().then(swankAvailable => {
        if (swankAvailable) {
          logger.warn(`HTTP service not detected, but SWANK is available. MCP may have limited functionality.`);
        }
        resolve(swankAvailable);
      });
    });
    
    // Attempt to connect
    socket.connect(HTTP_HOST_PORT, GENDL_HOST);
  });
}

// Check if SWANK is available (secondary service, mainly for developers)
function checkSwankAvailability() {
  return new Promise((resolve) => {
    logger.info(`Checking if Gendl SWANK service is available at ${GENDL_HOST}:${SWANK_HOST_PORT}`);
    
    const socket = new net.Socket();
    const timeout = 5000; // 5 second timeout
    
    // Set a timeout to abort the connection attempt
    socket.setTimeout(timeout);
    
    socket.on('connect', () => {
      logger.info(`Successfully connected to Gendl SWANK service at ${GENDL_HOST}:${SWANK_HOST_PORT}`);
      socket.destroy();
      
      // If user provided specific options but we're using existing service, warn them about ignored options
      if ((options.lispImpl || options.dockerImage || 
           options.mount?.length > 0 || 
           options.startHttp !== undefined || 
           options.startHttps !== undefined ||
           options.startSwank !== undefined ||
           options.startTelnet !== undefined ||
           options.autoStart === false ||
           options.httpPort || options.httpsPort || 
           options.swankPort || options.telnetPort ||
           options.dockerSocket) && 
          (GENDL_HOST !== '127.0.0.1' && GENDL_HOST !== 'localhost')) {
        logger.warn(`Configuration options are being ignored because an existing service was detected at ${GENDL_HOST}:${SWANK_HOST_PORT}`);
      }
      
      resolve(true);
    });
    
    socket.on('timeout', () => {
      logger.warn(`Connection attempt to Gendl SWANK service timed out after ${timeout}ms`);
      socket.destroy();
      resolve(false);
    });
    
    socket.on('error', (error) => {
      logger.warn(`Failed to connect to Gendl SWANK service: ${error.message}`);
      socket.destroy();
      resolve(false);
    });
    
    // Attempt to connect
    socket.connect(SWANK_HOST_PORT, GENDL_HOST);
  });
}

// Determine if we should start a Gendl container
function shouldStartGendlContainer() {
  // Only start a container if:
  // 1. Auto-start is enabled
  // 2. Host is local (127.0.0.1 or localhost)
  // 3. User provided custom docker image or lisp implementation, indicating they want a specific configuration
  const isLocalHost = (GENDL_HOST === '127.0.0.1' || GENDL_HOST === 'localhost');
  const hasCustomConfig = options.dockerImage || options.lispImpl;
  
  if (AUTO_START && isLocalHost) {
    if (hasCustomConfig) {
      logger.info('Auto-starting container with custom image/implementation configuration');
    }
    return true;
  }
  
  return false;
}

// Check if docker is available
function isDockerAvailable() {
  try {
    // Check if we're running in a container
    const cgroup = fs.readFileSync('/proc/self/cgroup', 'utf8');
    const isContainer = cgroup.includes('docker');
    
    // If we're in a container, we need the Docker socket to be mounted
    if (isContainer) {
      return fs.existsSync(DOCKER_SOCKET);
    }
    
    // If we're on the host, just check if docker command works
    execSync('docker --version', { stdio: 'ignore' });
    return true;
  } catch (error) {
    logger.warn(`Docker does not seem to be available: ${error.message}`);
    return false;
  }
}

// Check if a Gendl container is already running
function isGendlContainerRunning() {
  try {
    // First check HTTP port as it's the critical service for MCP operation
    const httpCommand = getDockerCommand('ps', ['-q', '--filter', `publish=${HTTP_HOST_PORT}`]);
    const httpContainerId = execSync(httpCommand, { encoding: 'utf8' }).trim();
    
    if (httpContainerId !== '') {
      logger.info(`Found container ${httpContainerId} running HTTP service on port ${HTTP_HOST_PORT}`);
      return true;
    }
    
    // If HTTP not found, check SWANK (for developer usage)
    const swankCommand = getDockerCommand('ps', ['-q', '--filter', `publish=${SWANK_HOST_PORT}`]);
    const swankContainerId = execSync(swankCommand, { encoding: 'utf8' }).trim();
    
    if (swankContainerId !== '') {
      logger.warn(`Found container ${swankContainerId} running SWANK on port ${SWANK_HOST_PORT}, but no HTTP service detected. MCP may have limited functionality.`);
      return true;
    }
    
    return false;
  } catch (error) {
    logger.warn(`Failed to check for running Gendl container: ${error.message}`);
    return false;
  }
}

// Get the appropriate docker command based on environment
function getDockerCommand(subcommand, args = []) {
  // Always use the basic docker command (the previous implementation was causing issues)
  return `docker ${subcommand} ${args.join(' ')}`;
}

// Global variable to track if MCP wrapper is started
let isMcpWrapperStarted = false;

// Global variable to store the container ID for cleanup on exit
let gendlContainerId = null;

// Check if Docker login is valid and attempt login if necessary
async function ensureDockerLogin() {
  return new Promise((resolve) => {
    logger.info('Checking Docker Hub authentication status');
    exec('docker info', (error, stdout, stderr) => {
      // If docker info works, we have docker access
      if (error) {
        logger.warn(`Docker info failed: ${error.message}`);
        resolve(false);
        return;
      }
      
      // Check if we can access Docker Hub - try a simple pull of a small public image
      exec('docker pull hello-world:latest', (error, stdout, stderr) => {
        if (error) {
          logger.warn(`Docker authentication check failed: ${error.message}`);
          logger.info('Attempting Docker Hub login...');
          
          // Try to login interactively or with stored credentials
          exec('docker login', (error, stdout, stderr) => {
            if (error) {
              logger.warn(`Docker login failed: ${error.message}`);
              resolve(false);
            } else {
              logger.info('Docker login successful');
              resolve(true);
            }
          });
        } else {
          logger.info('Docker authentication is valid');
          resolve(true);
        }
      });
    });
  });
}

// First try to use the image matching the current branch
async function pullLatestGendlImage() {
  return new Promise(async (resolve) => {
    let currentImage = GENDL_DOCKER_IMAGE; // Store the current image name
    logger.info(`Attempting to pull latest Gendl image: ${currentImage}`);
    
    // Try pulling the image matching the current branch
    try {
      await execPromise(`docker pull ${currentImage}`);
      logger.info(`Successfully pulled latest Gendl image: ${currentImage}`);
      resolve({ success: true, image: currentImage });
      return;
    } catch (pullError) {
      logger.warn(`Failed to pull latest Gendl image: ${pullError.message}`);
      
      // Check if the image exists locally
      try {
        await execPromise(`docker image inspect ${currentImage}`);
        logger.info(`Using existing local Gendl image: ${currentImage}`);
        resolve({ success: true, image: currentImage });
        return;
      } catch (inspectError) {
        logger.warn(`Gendl image ${currentImage} does not exist locally`);
        
        // If the current image is already the default, we've run out of options
        const defaultImage = `dcooper8/gendl:${DEFAULT_BRANCH}-${DEFAULT_IMPL}`;
        if (currentImage === defaultImage) {
          logger.error(`No suitable Gendl image available`);
          resolve(false);
          return;
        }
        
        // Try to pull the default image as fallback
        logger.info(`Trying to pull default image: ${defaultImage}`);
        try {
          await execPromise(`docker pull ${defaultImage}`);
          logger.info(`Successfully pulled default Gendl image`);
          // Update the environment variable with the new value
          process.env.GENDL_DOCKER_IMAGE = defaultImage;
          resolve({ success: true, image: defaultImage });
          return;
        } catch (defaultPullError) {
          // Try to check if default image exists locally
          try {
            await execPromise(`docker image inspect ${defaultImage}`);
            logger.info(`Using existing local default Gendl image: ${defaultImage}`);
            process.env.GENDL_DOCKER_IMAGE = defaultImage;
            resolve({ success: true, image: defaultImage });
            return;
          } catch (defaultInspectError) {
            logger.error(`Failed to find or pull default Gendl image: ${defaultPullError.message}`);
            resolve(false);
            return;
          }
        }
      }
    }
  });
}

// Promise wrapper for exec
function execPromise(command) {
  return new Promise((resolve, reject) => {
    exec(command, (error, stdout, stderr) => {
      if (error) {
        reject(error);
      } else {
        resolve(stdout.trim());
      }
    });
  });
}

// Start a Gendl container if needed
function startGendlContainer() {
    return new Promise(async (resolve, reject) => {
	// First check if Docker is available
	if (!isDockerAvailable()) {
	    return reject(new Error('Docker is not available, cannot start Gendl container'));
	}
	
	// Check if a container is already running
	if (isGendlContainerRunning()) {
	    logger.info('Gendl container is already running');
	    return resolve();
	}
	
	// Ensure Docker login and pull latest image
	const loginStatus = await ensureDockerLogin();
	if (loginStatus) {
	    logger.info('Docker login confirmed, proceeding with image pull');
	} else {
	    logger.warn('Docker login not confirmed, will try to use local images');
	}
	
	// Try to pull the latest image
	const pullResult = await pullLatestGendlImage();
	if (!pullResult || !pullResult.success) {
	    logger.warn('Could not pull or find a suitable Gendl image');
	    return reject(new Error('No suitable Gendl image available'));
	}
	
	// Use the image from pullLatestGendlImage result
	const dockerImage = pullResult.image || GENDL_DOCKER_IMAGE;
	logger.info(`Starting Gendl container using image ${dockerImage}`);

	// Get the current user's UID and GID
        const uid = process.getuid ? process.getuid() : 1000; // Fallback to 1000 if not available (e.g., Windows)
        const gid = process.getgid ? process.getgid() : 1000; // Fallback to 1000 if not available (e.g., Windows)

	
	// Prepare the docker run command
	const args = [
	    '-di', // `d`etached in background but `i`nteractive so
		   // the lisp toplevel goes to logs and we don't die
	    '--rm', // Automatically remove the container when it stops
	    // '-u', `${uid}:${gid}`, // Set user and group ID to match the current user
	    '-p', `${SWANK_HOST_PORT}:${INTERNAL_SWANK_PORT}`,
	    '-p', `${HTTP_HOST_PORT}:${INTERNAL_HTTP_PORT}`,
	    '-e', `START_SWANK=${START_SWANK}`,
	    '-e', `START_HTTP=${START_HTTP}`,
	    '-e', `START_HTTPS=${START_HTTPS}`,
	    '-e', `START_TELNET=${START_TELNET}`,
	    '-e', `HTTP_PORT=${INTERNAL_HTTP_PORT}`,
	    '-e', `HTTPS_PORT=${INTERNAL_HTTPS_PORT}`,
	    '-e', `SWANK_PORT=${INTERNAL_SWANK_PORT}`,
	    '-e', `TELNET_PORT=${INTERNAL_TELNET_PORT}`
	];
	
	// Add additional port mappings if services are enabled
	if (START_HTTPS) {
	    args.push('-p', `${HTTPS_HOST_PORT}:${INTERNAL_HTTPS_PORT}`);
	}
	
	if (START_TELNET) {
	    args.push('-p', `${TELNET_HOST_PORT}:${INTERNAL_TELNET_PORT}`);
	}
	
	// Add mount points if specified
	if (ALL_MOUNTS.length > 0) {
	    logger.info(`Adding ${ALL_MOUNTS.length} mount points to container`);
	    ALL_MOUNTS.forEach(mount => {
		// Validate the mount format (src:dst)
		if (mount && mount.includes(':')) {
		    const [src, dst] = mount.split(':');
		    if (src && dst) {
			logger.debug(`Adding mount: ${src} -> ${dst}`);
			args.push('-v', `${src}:${dst}`);
		    } else {
			logger.warn(`Invalid mount format: ${mount}, expected src:dst`);
		    }
		} else {
		    logger.warn(`Invalid mount format: ${mount}, expected src:dst`);
		}
	    });
	}
	
	// Add the Docker image as the last argument
	args.push(dockerImage);
	
	const command = `docker run ${args.join(' ')}`;  // Fixed command construction
	logger.debug(`Running command: ${command}`);
	
	exec(command, (error, stdout, stderr) => {
	    if (error) {
		logger.error(`Failed to start Gendl container: ${error.message}`);
		logger.error(stderr);
		return reject(error);
	    }
	    
	    gendlContainerId = stdout.trim();
	    logger.info(`Started Gendl container with ID: ${gendlContainerId}`);
	    
	    // Wait for the server to become available
	    waitForGendlServer(30) // Wait up to 30 seconds
		.then(() => resolve())
		.catch(error => reject(error));
	});
    });
}

// Wait for the Gendl server to become available
function waitForGendlServer(maxWaitSeconds) {
  return new Promise((resolve, reject) => {
    logger.info(`Waiting up to ${maxWaitSeconds} seconds for Gendl server to become available`);
    
    let attempts = 0;
    const maxAttempts = maxWaitSeconds;
    const interval = 1000; // 1 second interval
    
    const check = () => {
      attempts++;
      
      checkGendlAvailability()
        .then(available => {
          if (available) {
            logger.info(`Gendl server is now available after ${attempts} seconds`);
            resolve();
          } else if (attempts < maxAttempts) {
            logger.debug(`Waiting for Gendl server (attempt ${attempts}/${maxAttempts})...`);
            setTimeout(check, interval);
          } else {
            reject(new Error(`Gendl server did not become available after ${maxAttempts} seconds`));
          }
        })
        .catch(error => {
          reject(error);
        });
    };
    
    // Start checking
    check();
  });
}

// Start the MCP wrapper
function startMcpWrapper() {
  // Set flag to prevent double-starting
  isMcpWrapperStarted = true;
  
  logger.info('Starting MCP wrapper');
  
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
  
  // Handle process events
  setupProcessEvents(rl);
}

// Set up process event handlers
function setupProcessEvents(rl) {
  // Function to clean up resources on exit
  const cleanup = () => {
    logger.info('Wrapper script exiting');
    
    // Terminate and remove the Gendl container if we started it
    if (gendlContainerId) {
      logger.info(`Stopping Gendl container with ID: ${gendlContainerId}`);
      try {
        execSync(`docker stop ${gendlContainerId}`, { stdio: 'ignore' });
        logger.info(`Successfully stopped Gendl container`);
      } catch (error) {
        logger.error(`Error stopping Gendl container: ${error.message}`);
      }
    }
    
    if (logStream) {
      logStream.end();
    }
    if (rl) rl.close();
  };
  
  process.on('exit', cleanup);
  
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
    logger.info('Received SIGINT signal - cleaning up and exiting');
    cleanup();
    process.exit(0);
  });
  
  process.on('SIGTERM', () => {
    logger.info('Received SIGTERM signal - cleaning up and exiting');
    cleanup();
    process.exit(0);
  });
  
  logger.info('Enhanced MCP wrapper initialized - ready to handle requests');
}

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
        name: 'enhanced-gendl-mcp',
        version: VERSION
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
      port: HTTP_HOST_PORT, // Use HTTP port for HTTP requests
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
    port: HTTP_HOST_PORT, // Use HTTP port for HTTP requests
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
  logger.info(`Handling lisp_eval with code: ${args.code?.substring(0, 100)}${args.code?.length > 100 ? '...' : ''}`);
  
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
      port: HTTP_HOST_PORT, // Use HTTP port for HTTP requests
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
    
    // Check knowledge base directory - relative to script directory
const kbPath = path.join(__dirname, '..', 'gendl-kb');
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
