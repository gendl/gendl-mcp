# Gendl Model Context Protocol (MCP)

A Model Context Protocol implementation for Gendl that exposes Gendl functionality via HTTP endpoints, making it accessible as MCP tools for Claude.

## Overview

This package allows you to:

1. Expose Gendl Lisp evaluation to Claude via MCP tools
2. Let Claude evaluate arbitrary Lisp code in your Gendl environment
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

## MCP Tools

The Gendl MCP implementation provides two tools for Claude interaction:

- `ping_gendl` - Check if the Gendl server is accessible
- `lisp_eval` - Evaluate Lisp code in the Gendl environment

## MCP Protocol Endpoints

The system provides the required MCP protocol endpoints:

- `/mcp/tools/list` - List available tools for Claude
- `/mcp/resources/list` - List available resources (currently empty)
- `/mcp/prompts/list` - List available prompts (currently empty)
- `/mcp/claude/ping` - Check if the server is accessible (used by ping_gendl tool)
- `/mcp/lisp-eval` - Evaluate Lisp code (used by lisp_eval tool)
- `/mcp/specs` - Get MCP configuration for claude-desktop-json.conf

## Architecture

The MCP implementation consists of:

1. **Lisp server endpoints** - HTTP endpoints that handle requests from Claude
2. **JavaScript wrapper** - Node.js script that translates between Claude's MCP protocol and the Lisp server endpoints
3. **Claude desktop configuration** - Configuration that connects Claude to the MCP server

## Using with Claude

You can use the `lisp_eval` tool to accomplish various tasks:

### Listing Objects

```lisp
(lisp_eval "(gendl-objects)")
```

### Creating Objects

```lisp
(lisp_eval "(let ((box (make-object 'box :length 10 :width 5 :height 3)))
             (the id box))")
```

### Sending Messages to Objects

```lisp
(lisp_eval "(let ((obj (lookup-object <object-id>)))
             (the <message-name> obj))")
```

With arguments:

```lisp
(lisp_eval "(let ((obj (lookup-object <object-id>)))
             (the (<message-name> <arg1> <arg2>) obj))")
```

## Example HTTP Testing

You can test the endpoints directly:

1. Test the ping endpoint:
   ```
   GET http://127.0.0.1:9081/mcp/claude/ping
   ```

2. Test Lisp evaluation:
   ```
   POST http://127.0.0.1:9081/mcp/lisp-eval
   Content-Type: application/json
   
   {"code": "(+ 1 2 3)"}
   ```

3. List available tools:
   ```
   GET http://127.0.0.1:9081/mcp/tools/list
   ```

## Claude Desktop Configuration

To configure Claude Desktop to use this MCP server, add the following to your `claude_desktop_config.json`:

```json
{
  "mcpServers": {
    "gendl": {
      "command": "wsl",
      "args": ["node", "/path/to/gendl-mcp/mcp-format-wrapper.js"],
      "env": {
        "NODE_ENV": "production",
        "DEBUG": "*"
      },
      "persistent": true,
      "timeout": 60000
    }
  }
}
```

You can get the exact configuration by querying the `/mcp/specs` endpoint.
