#!/usr/bin/env node

/**
 * Test script for the HTTP proxy
 * This script directly calls the HTTP proxy to test functionality without the MCP wrapper
 */

const { httpProxy } = require('./little-http-proxy');

// Set up a simple logger
const logger = {
  error: (message) => console.error(`[ERROR] ${message}`),
  warn: (message) => console.warn(`[WARN] ${message}`),
  info: (message) => console.log(`[INFO] ${message}`)
};

// Test the HTTP proxy with the color-map endpoint
async function testColorMap() {
  try {
    logger.info('Testing HTTP request to /color-map with rawResponse=false');
    
    const response = await httpProxy({
      hostname: '127.0.0.1',
      port: 9081,
      path: '/color-map',
      method: 'GET',
      rawResponse: false,
      logger: logger
    });
    
    logger.info(`Response status code: ${response.statusCode}`);
    logger.info(`Response content type: ${typeof response.content}`);
    logger.info(`Response redirect URL: ${response.redirectUrl}`);
    
    // Print first 200 characters of content if it's a string
    if (typeof response.content === 'string') {
      logger.info(`Content (first 200 chars): ${response.content.substring(0, 200)}`);
    } else {
      logger.info('Content is not a string');
    }
    
    // Test with rawResponse=true
    logger.info('\nTesting HTTP request to /color-map with rawResponse=true');
    
    const rawResponse = await httpProxy({
      hostname: '127.0.0.1',
      port: 9081,
      path: '/color-map',
      method: 'GET',
      rawResponse: true,
      logger: logger
    });
    
    logger.info(`Raw response status code: ${rawResponse.statusCode}`);
    logger.info(`Raw response content type: ${typeof rawResponse.content}`);
    logger.info(`Raw response redirect URL: ${rawResponse.redirectUrl}`);
    
    // Print first 200 characters of content if it's a string
    if (typeof rawResponse.content === 'string') {
      logger.info(`Raw content (first 200 chars): ${rawResponse.content.substring(0, 200)}`);
    } else {
      logger.info('Raw content is not a string');
    }
    
    // Log headers for more debugging
    logger.info('\nResponse headers:');
    console.log(response.headers);
    
  } catch (error) {
    logger.error(`Test failed: ${error.message}`);
    logger.error(error.stack);
  }
}

// Run the test
testColorMap();
