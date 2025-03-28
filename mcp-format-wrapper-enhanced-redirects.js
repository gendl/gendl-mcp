/**
 * Enhanced MCP Format Wrapper with Redirect Support
 * Handles HTTP redirects automatically while making HTTP requests to the Gendl server
 * 
 * This module extends the HTTP request functionality of mcp-format-wrapper-enhanced.js
 * to properly handle redirects with a configurable maximum limit.
 */

const http = require('http');
const https = require('https');

// Constants
const MAX_REDIRECTS = 6; // Maximum number of redirects to follow
const GENDL_HOST = '127.0.0.1';
const GENDL_PORT = 9080; // Internal port within the container

/**
 * Makes an HTTP request to the Gendl server with automatic redirect following
 * Returns all four salient values from the HTTP response:
 * 1. content/body - The response body
 * 2. headers - Object containing response headers
 * 3. statusCode - HTTP status code
 * 4. redirectUrl - The final URL after redirects (if any)
 * 
 * @param {Object} args - Request parameters
 * @param {string} args.path - The request path
 * @param {string} [args.method='GET'] - HTTP method
 * @param {Object} [args.query] - Query parameters
 * @param {Object} [args.headers] - Request headers
 * @param {string} [args.body] - Request body
 * @param {boolean} [args.rawResponse=false] - Whether to return the raw response
 * @param {number} [redirectCount=0] - Current redirect count (internal use)
 * @returns {Promise<Object>} - Response object with all four values
 */
async function makeHttpRequest(args, redirectCount = 0) {
  return new Promise((resolve, reject) => {
    try {
      const method = args.method || 'GET';
      let path = args.path;
      
      // Ensure path starts with a slash
      if (!path.startsWith('/')) {
        path = '/' + path;
      }

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
      
      console.error(`Making ${method} request to http://${GENDL_HOST}:${GENDL_PORT}${path}`);
      console.error(`Redirect count: ${redirectCount}/${MAX_REDIRECTS}`);
      
      const req = http.request(options, (res) => {
        let responseData = '';
        
        res.on('data', (chunk) => {
          responseData += chunk;
        });
        
        res.on('end', () => {
          console.error(`Response status: ${res.statusCode}`);
          console.error(`Response headers: ${JSON.stringify(res.headers)}`);
          
          // Check if this is a redirect (status codes 301, 302, 303, 307, 308)
          if ([301, 302, 303, 307, 308].includes(res.statusCode) && redirectCount < MAX_REDIRECTS) {
            const location = res.headers.location;
            
            if (location) {
              console.error(`Following redirect (${redirectCount + 1}/${MAX_REDIRECTS}): ${path} -> ${location}`);
              
              // Prepare for the next request
              let nextPath = location;
              let nextMethod = method;
              
              // For 303 responses, always use GET for the redirect
              if (res.statusCode === 303) {
                nextMethod = 'GET';
              }
              
              // Handle absolute URLs
              if (location.startsWith('http://') || location.startsWith('https://')) {
                try {
                  const locationUrl = new URL(location);
                  nextPath = locationUrl.pathname + locationUrl.search;
                } catch (error) {
                  console.error(`Error parsing redirect URL: ${error.message}`);
                  nextPath = location;
                }
              }
              
              // Follow the redirect with a new request
              makeHttpRequest({
                ...args,
                path: nextPath,
                method: nextMethod
              }, redirectCount + 1).then(resolve).catch(reject);
              
              return;
            }
          }
          
          // Not a redirect or max redirects reached, return the response
          const response = {
            statusCode: res.statusCode,
            headers: res.headers,
            body: responseData,
            redirectUrl: res.headers.location || null
          };
          
          resolve(response);
        });
      });
      
      req.on('error', (error) => {
        console.error(`HTTP request error: ${error.message}`);
        reject(error);
      });
      
      req.on('timeout', () => {
        console.error('HTTP request timed out');
        req.abort();
        reject(new Error('Request timeout'));
      });
      
      if (args.body) {
        req.write(args.body);
      }
      
      req.end();
    } catch (error) {
      console.error(`Error in makeHttpRequest: ${error.message}`);
      reject(error);
    }
  });
}

/**
 * HTTP request handler for Gendl MCP
 * This is the main exported function that will be used by the MCP system
 * 
 * @param {Object} args - Request parameters (same as makeHttpRequest)
 * @returns {Promise<Object>} - Response with all four values
 */
async function http_request(args) {
  try {
    // Make the HTTP request with redirect support
    const response = await makeHttpRequest(args);
    
    // Prepare the result format based on rawResponse flag
    if (args.rawResponse) {
      // For raw responses, return just the body content
      return response.body;
    } else {
      // Try to parse JSON responses
      let content = response.body;
      
      try {
        if (response.headers['content-type'] && 
            response.headers['content-type'].includes('application/json')) {
          content = JSON.parse(response.body);
        }
      } catch (e) {
        // If parsing fails, keep as string
        console.error(`Failed to parse JSON response: ${e.message}`);
      }
      
      // Return all four values in the response
      return {
        content: content,
        headers: response.headers,
        statusCode: response.statusCode,
        redirectUrl: response.redirectUrl
      };
    }
  } catch (error) {
    console.error(`Error in http_request: ${error.message}`);
    throw new Error(`Error making HTTP request: ${error.message}`);
  }
}

// Export the HTTP request function for use in the MCP system
module.exports = {
  http_request
};
