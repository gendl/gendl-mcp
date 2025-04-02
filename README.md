# Enhanced MCP Wrapper for Gendl

This is an enhanced MCP wrapper script for Gendl integration with Claude Desktop. It provides a powerful interface between Claude and Gendl, enabling AI-assisted generative engineering design.

## What is Gendl?

Gendl is a Generative Programming and Knowledge Based Engineering (KBE) framework that implements powerful concepts used by cutting-edge companies for mission-critical engineering applications. With Gendl, you can:

- Define complex engineering problems using a high-level declarative, object-oriented syntax
- Generate and manipulate 3D geometry programmatically
- Decompose complex problems into manageable object trees
- Benefit from automatic dependency tracking and value caching

Gendl is available as open source software under the AGPL license at [https://gitlab.common-lisp.net/gendl/gendl](https://gitlab.common-lisp.net/gendl/gendl).

## Features

- **Configurable Gendl Host & Port**: Configure via command-line arguments or environment variables
- **Docker Container Management**: Automatically start Gendl container when needed 
- **Volume Mounting**: Mount host directories into the Gendl container
- **Environment Detection**: Run directly on host or inside a container with Docker socket
- **Improved Error Handling**: Better detection and reporting of errors
- **Enhanced Logging**: Detailed logs with timestamps and optional debug mode

## Installation

1. Install the required dependencies:
```bash
cd /projects/gendl/gwl/mcp/scripts
npm install
chmod +x enhanced-mcp-wrapper.js
```

You also need docker installed.

2. Test the script:
```bash
node enhanced-mcp-wrapper.js --help
```

## Command-Line Arguments

```
Options:
  -H, --host <host>            Gendl server host (default: 127.0.0.1)
  --swank-host-port <port>     SWANK port on host system (default: 5200)
  --http-host-port <port>      HTTP port on host system (default: 10080)
  --https-host-port <port>     HTTPS port on host system (default: 10443)
  --telnet-host-port <port>    TELNET port on host system (default: 5023)
  --http-port <port>           HTTP port inside container (default: 9080)
  --https-port <port>          HTTPS port inside container (default: 9443)
  --swank-port <port>          SWANK port inside container (default: 4200)
  --telnet-port <port>         TELNET port inside container (default: 4023)
  --docker-image <image>       Docker image for Gendl (default: dcooper8/gendl:release--1598-ccl)
  --no-auto-start              Do not auto-start Gendl docker container if not running
  --docker-socket <path>       Path to docker socket (default: /var/run/docker.sock)
  --log-file <path>            Path to log file (default: /tmp/enhanced-mcp-wrapper.log)
  --debug                      Enable debug logging
  --mount <mounts...>          Mount volumes in format "src:dst" (can specify multiple times)
  --start-http                 Start HTTP service in Gendl container (default: true)
  --start-https                Start HTTPS service in Gendl container (default: false)
  --start-swank                Start SWANK service in Gendl container (default: true)
  --start-telnet               Start TELNET service in Gendl container (default: false)
  -h, --help                   Display help for command
```

## Environment Variables

The script also supports configuration via environment variables:

- `GENDL_HOST`: Gendl server host (default: 127.0.0.1)
- `SWANK_HOST_PORT`: SWANK port on host system (default: 5200)
- `HTTP_HOST_PORT`: HTTP port on host system (default: 10080)
- `HTTPS_HOST_PORT`: HTTPS port on host system (default: 10443)
- `TELNET_HOST_PORT`: TELNET port on host system (default: 5023)
- `HTTP_PORT`: HTTP port inside container (default: 9080)
- `HTTPS_PORT`: HTTPS port inside container (default: 9443)
- `SWANK_PORT`: SWANK port inside container (default: 4200)
- `TELNET_PORT`: TELNET port inside container (default: 4023)
- `START_HTTP`: Set to "true" to enable HTTP service (default: true)
- `START_HTTPS`: Set to "true" to enable HTTPS service (default: false)
- `START_SWANK`: Set to "true" to enable SWANK service (default: true)
- `START_TELNET`: Set to "true" to enable TELNET service (default: false)
- `GENDL_DOCKER_IMAGE`: Docker image for Gendl
- `GENDL_AUTO_START`: Set to "false" to disable auto-starting container
- `DOCKER_SOCKET`: Path to Docker socket
- `GENDL_LOG_FILE`: Path to log file
- `DEBUG_GENDL`: Set to "true" to enable debug logging
- `GENDL_MOUNTS`: Comma-separated list of mount points in format "src:dst"

## Usage Examples 

All the below examples can be tested on command line and used in
`claude_desktop_config.json` configuration (see example below):

### Basic Usage

Run with default settings (localhost:5200):
```bash
node enhanced-mcp-wrapper.js
```

### Custom Host and Port

Specify a custom Gendl server:
```bash
node enhanced-mcp-wrapper.js --host 192.168.1.100 --swank-host-port 5200 --http-host-port 10080
```

### Enabling Services

Enable HTTP and HTTPS services:
```bash
node enhanced-mcp-wrapper.js --start-http --start-https
```

Or using environment variables:
```bash
START_HTTP=true START_HTTPS=true node enhanced-mcp-wrapper.js
```

### Configuring Internal Container Ports

Specify all internal ports:
```bash
node enhanced-mcp-wrapper.js --http-port 9080 --https-port 9443 --swank-port 4200 --telnet-port 4023
```

Or using environment variables:
```bash
HTTP_PORT=9080 HTTPS_PORT=9443 SWANK_PORT=4200 TELNET_PORT=4023 node enhanced-mcp-wrapper.js
```

### Mounting Directories

Mount host directories into the Gendl container:
```bash
node enhanced-mcp-wrapper.js --mount /home/user/projects:/projects
```

Multiple mount points:
```bash
node enhanced-mcp-wrapper.js --mount /home/user/projects:/projects --mount /home/user/data:/data
```

### Running in a Container

If running the wrapper inside a container, make sure to mount the Docker socket:
```bash
docker run -v /var/run/docker.sock:/var/run/docker.sock -v /path/to/scripts:/app node:18 node /app/enhanced-mcp-wrapper.js
```

## Tools for Claude Integration

This wrapper includes several tools that enable Claude to interact with Gendl:

### Lisp Evaluation Tool

The `lisp_eval` tool allows Claude to evaluate Lisp code directly within the Gendl environment. This enables Claude to:

- Create and manipulate GDL objects
- Perform complex calculations using Gendl's geometric primitives
- Access and modify Gendl's state

See the [Lisp Evaluation Examples](./lisp-eval-examples.md) file for detailed examples of how to use this tool.

### Knowledge Base Integration

This wrapper includes integration with a Gendl knowledge base through the `query_gendl_kb` tool. This allows Claude to search for information about Gendl/GDL (General-purpose Declarative Language) directly.

The knowledge base is accessed via a Python script located in the same directory as the wrapper script. When packaging this wrapper, make sure to include:

1. The Python script for knowledge base queries (`gendl_kb.py`)
2. The knowledge base files themselves (typically stored in the `gendl-kb` directory relative to the script)

#### Knowledge Base Tool Usage

The `query_gendl_kb` tool can be invoked by Claude with a query string, and it will return relevant information from the Gendl documentation and knowledge base.

## Claude Desktop Configuration

Here's an example of how to configure Claude Desktop to use this enhanced wrapper:

```json
{
  "mcpServers": {
    "filesystem": {
      "command": "wsl",
      "args": [
        "docker",
        "run",
        "-i",
        "--rm",
        "-u",
        "1000:1000",
        "--mount",
        "type=bind,src=/home/user/projects,dst=/projects",
        "mcp/filesystem",
        "/projects"
      ]
    },
    "gendl": {
      "command": "wsl",
      "args": [
        "node",
        "/home/user/projects/gendl-mcp/scripts/enhanced-mcp-wrapper.js",
        "--mount", "/home/user/projects:/projects"
      ],
      "env": {
        "NODE_ENV": "production",
        "DEBUG": "*"
      }
    }
  },
  "globalShortcut": ""
}
```

## Troubleshooting

If you encounter issues:

1. Check if the Gendl server is running:
```bash
docker ps | grep gendl
```

2. Enable debug logging:
```bash
node enhanced-mcp-wrapper.js --debug
```

3. Check the log file:
```bash
tail -f /tmp/enhanced-mcp-wrapper.log
```

4. Try pinging the Gendl HTTP server:
```bash
curl http://localhost:10080/mcp/ping-gendl
```

5. Try connecting to the Gendl SWANK server:
```bash
telnet localhost 5200
```

## License

This software is licensed under the GNU Affero General Public License v3.0 (AGPL-3.0), the same license used by Gendl.

### License Implications

**Important Note**: Simply using this MCP server to interact with Gendl and obtain outputs does not trigger the requirements of the AGPL. You can use this wrapper to interact with Gendl without being required to share your code.

However, if you modify this wrapper and host a service based on the modified software, the AGPL would require you to share your modifications with the users of that service. In other words, if you run a modified version of this software as a network service, you must make the modified source code available to the users of that service.

For applications that need to keep their source code closed, Genworks offers an "AGPL escape clause" in the form of a 5% self-reported quarterly royalty. More information and a payment gateway are available at [payments.genworks.com](https://payments.genworks.com).

The full text of the license can be found in the COPYING.txt file in this directory.


