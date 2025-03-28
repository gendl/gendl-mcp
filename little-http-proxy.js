/**
 * little-http-proxy.js
 * 
 * A lightweight HTTP proxy module for making requests to the Gendl server
 * with automatic redirect handling.
 * 
 * This module provides a simple interface for making HTTP requests while
 * automatically following redirects up to a configurable maximum.
 */

const http = require('http');
const https = require('https');
const { URL } = require('url');

// Constants
const MAX_REDIRECTS = 6; // Maximum number of redirects to follow

/**
 * Makes an HTTP request with automatic redirect following
 * Returns all four salient values from the HTTP response:
 * 1. content - The response body
 * 2. headers - Object containing response headers
 * 3. statusCode - HTTP status code
 * 4. redirectUrl - The final URL after redirects (if any)
 * 
 * @param {Object} options - Request options
 * @param {string} options.hostname - Target hostname
 * @param {number} options.port - Target port
 * @param {string} options.path - Request path
 * @param {string} [options.method='GET'] - HTTP method
 * @param {Object} [options.headers={}] - HTTP headers
 * @param {string} [options.body] - Request body for POST, PUT, etc.
 * @param {boolean} [options.rawResponse=false] - Return raw response instead of parsed JSON
 * @param {Object} [options.logger] - Logger object with error, warn, info methods
 * @param {number} [redirectCount=0] - Current redirect count (internal use)
 * @returns {Promise<Object>} - Response with content, headers, statusCode, redirectUrl
 */
function makeRequest(options, redirectCount = 0) {
  // Default parameters
  const hostname = options.hostname;
  const port = options.port;
  const path = options.path;
  const method = options.method || 'GET';
  const headers = options.headers || {};
  const body = options.body || null;
  const rawResponse = options.rawResponse || false;
  const logger = options.logger || console;
  
  // Create request options
  const requestOptions = {
    hostname: hostname,
    port: port,
    path: path,
    method: method,
    headers: {
      'Accept': 'application/json, text/plain, */*',
      ...headers
    }
  };
  
  // Set content length for requests with body
  if (body && !headers['Content-Length']) {
    requestOptions.headers['Content-Length'] = Buffer.byteLength(body);
  }
  
  logger.info(`Making ${method} request to http://${hostname}:${port}${path}`);
  logger.info(`Redirect count: ${redirectCount}/${MAX_REDIRECTS}`);
  
  return new Promise((resolve, reject) => {
    const req = http.request(requestOptions, (res) => {
      let responseData = '';
      
      res.on('data', (chunk) => {
        responseData += chunk;
      });
      
      res.on('end', () => {
        logger.info(`Response status: ${res.statusCode}`);
        
        // Check if this is a redirect
        if ([301, 302, 303, 307, 308].includes(res.statusCode) && redirectCount < MAX_REDIRECTS) {
          const location = res.headers.location;
          
          if (location) {
            logger.info(`Following redirect (${redirectCount + 1}/${MAX_REDIRECTS}): ${path} -> ${location}`);
            
            // Determine the next request parameters
            let nextHostname = hostname;
            let nextPort = port;
            let nextPath = location;
            let nextMethod = method;
            
            // Handle 303 redirects - always use GET
            if (res.statusCode === 303) {
              nextMethod = 'GET';
            }
            
            // Handle absolute URLs
            if (location.startsWith('http://') || location.startsWith('https://')) {
              try {
                const url = new URL(location);
                nextHostname = url.hostname;
                nextPort = url.port || (url.protocol === 'https:' ? 443 : 80);
                nextPath = url.pathname + url.search;
              } catch (error) {
                logger.error(`Error parsing redirect URL: ${error.message}`);
              }
            }
            
            // Follow the redirect
            return makeRequest({
              ...options,
              hostname: nextHostname,
              port: nextPort,
              path: nextPath,
              method: nextMethod
            }, redirectCount + 1).then(resolve).catch(reject);
          }
        }
        
        // Not a redirect or max redirects reached
        const finalUrl = res.headers.location || null;
        
        // Process response based on content type and rawResponse flag
        let content = responseData;
        
        if (!rawResponse && res.headers['content-type'] && 
            res.headers['content-type'].includes('application/json')) {
          try {
            content = JSON.parse(responseData);
          } catch (error) {
            logger.warn(`Failed to parse JSON response: ${error.message}`);
          }
        }
        
        // Resolve with the complete response object
        resolve({
          content: content,
          headers: res.headers,
          statusCode: res.statusCode,
          redirectUrl: finalUrl
        });
      });
    });
    
    req.on('error', (error) => {
      logger.error(`HTTP request error: ${error.message}`);
      reject(error);
    });
    
    req.on('timeout', () => {
      logger.error('HTTP request timed out');
      req.abort();
      reject(new Error('Request timeout'));
    });
    
    if (body) {
      req.write(body);
    }
    
    req.end();
  });
}

/**
 * Simplified interface for making HTTP requests to a Gendl server
 * 
 * @param {Object} args - Request parameters
 * @param {string} args.hostname - Gendl server hostname
 * @param {number} args.port - Gendl server port
 * @param {string} args.path - Request path
 * @param {string} [args.method='GET'] - HTTP method
 * @param {Object} [args.query] - Query parameters as key-value pairs
 * @param {Object} [args.headers] - HTTP headers as key-value pairs
 * @param {string} [args.body] - Request body
 * @param {boolean} [args.rawResponse=false] - Return raw response
 * @param {Object} [args.logger] - Logger object with error, warn, info methods
 * @returns {Promise<Object>} - Response with content, headers, statusCode, redirectUrl
 */
async function httpProxy(args) {
  const logger = args.logger || console;
  
  try {
    // Process path and query parameters
    let path = args.path || '/';
    if (!path.startsWith('/')) path = '/' + path;
    
    // Add query parameters if provided
    if (args.query && Object.keys(args.query).length > 0) {
      const queryString = Object.entries(args.query)
        .map(([key, value]) => `${encodeURIComponent(key)}=${encodeURIComponent(value)}`)
        .join('&');
      
      path += path.includes('?') ? `&${queryString}` : `?${queryString}`;
    }
    
    // Set content type for requests with body if not already specified
    const headers = { ...args.headers };
    if (args.body && !headers['Content-Type']) {
      headers['Content-Type'] = 'application/json';
    }
    
    // Make the request
    return await makeRequest({
      hostname: args.hostname,
      port: args.port,
      path: path,
      method: args.method,
      headers: headers,
      body: args.body,
      rawResponse: args.rawResponse,
      logger: logger
    });
  } catch (error) {
    logger.error(`Error in httpProxy: ${error.message}`);
    throw error;
  }
}

module.exports = {
  httpProxy
};
