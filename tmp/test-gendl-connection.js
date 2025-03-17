#!/usr/bin/env node

/**
 * Test script to verify connectivity to the Gendl HTTP server.
 * This script makes a simple HTTP GET request to the Gendl ping endpoint.
 */

const http = require('http');

console.log('Testing connection to Gendl server at http://127.0.0.1:9081/mcp/claude/ping');

const options = {
  hostname: '127.0.0.1',
  port: 9081,
  path: '/mcp/claude/ping',
  method: 'GET'
};

const req = http.request(options, (res) => {
  console.log(`Status: ${res.statusCode}`);
  console.log(`Headers: ${JSON.stringify(res.headers)}`);
  
  let data = '';
  res.on('data', (chunk) => {
    data += chunk;
  });
  
  res.on('end', () => {
    console.log('Response body:');
    console.log(data);
    console.log('\nConnection test completed.');
  });
});

req.on('error', (error) => {
  console.error(`Error connecting to Gendl server: ${error.message}`);
  if (error.code === 'ECONNREFUSED') {
    console.error('Connection refused. Make sure the Gendl server is running at 127.0.0.1:9081');
  }
});

req.end();
