f# Gendl Model Context Protocol (MCP)

A Model Context Protocol implementation for Gendl that exposes Gendl
objects via HTTP endpoints, making them accessible as MCP tools for
Claude.

## Overview

This package allows you to:

1. Expose Gendl objects to Claude via HTTP endpoints
2. Let Claude instantiate, probe, and manipulate Gendl objects 
3. Rely on Gendl's built-in HTTP server to provide the MCP interface

## Installation

1. Place this system where ASDF can find it
2. Load it with:

```lisp
(load-quicklisp)
(pushnew "<path-to-gendl-mcp>" ql:*local-project-directories*)
(ql:quickload :gendl-mcp)
```
Loading the system will also publish the standard endpoints by side-effect. We 
assume you already have your AllegroServe HTTP server accessible 
on http://127.0.0.1:9081 

The MCP server provides several standard endpoints:

- `/mcp/objects` - List all registered objects
- `/mcp/make-object` - Create a new object instance and return integer id (POST)
- `/mcp/theo?object=<object-id>&message=<message-name>` - send a message and get the yielded value (POST) -- FLAG -- this should be extended to accept arguments. Some messages accept args. 

## Claude-Specific Endpoints

There are also endpoints specifically designed for Claude integration:

- `/mcp/claude/ping` - Check if the MCP server is accessible

- `/mcp/claude/tools` - get this server's tools info e.g. for Claude

- `/mcp/claude/specs` - get this server's config text for claude-desktop-json.conf

## Creating Custom Endpoints

You can define custom MCP endpoints with the `net.aserve:publish`
function  as seen in `source/mcp-endpoints.lisp`.

## Example Usage

1. Start the Gendl HTTP server (if not already running)

2. Load the gendl-mcp system:
   ```lisp
   (load-quicklisp)
   (pushnew <gendl-mcp-dir> ql:*local-project-directories*)
   (ql:quickload :gendl-mcp)
   ```

4. Test with a simple HTTP request:
   ```
   GET http://127.0.0.1:9081/mcp/claude/ping
   ```


