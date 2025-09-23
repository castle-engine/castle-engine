# MCP Server for Castle Game Engine Editor

The Castle Game Engine editor includes an integrated MCP (Model Context Protocol) server that allows AI clients to interact with the editor programmatically. This enables AI assistants to help with game development by accessing project information, component hierarchies, and design data.

## What is MCP?

The Model Context Protocol (MCP) is an open standard that enables AI assistants to securely connect to data sources and tools. It provides a standardized way for AI clients to:

- Access project resources and data
- Execute tools and commands
- Retrieve contextual information
- Interact with development environments

## Features

The Castle Game Engine MCP server provides the following capabilities:

### 🔧 **Current Implementation Status**

#### ✅ **Fully Implemented**
- **Editor UI Integration**: Complete preferences dialog with MCP server controls
- **Project Information Access**: Real-time access to project files, Pascal sources, and data files
- **Component Hierarchy**: Full access to design-time component trees and properties
- **Property Access**: Get and set component properties with type safety
- **JSON-RPC 2.0 Compliance**: Standards-compliant message handling
- **Screenshot Capture**: Real design viewport image capture with base64 PNG encoding

#### 🚧 **Partially Implemented (Framework Ready)**
- **Network Server**: UI controls implemented, stdio mode working, TCP server framework ready for completion

### 📋 **Available Resources**

1. **Project Information** (`project://info`)
   - Project name, caption, and path
   - List of Pascal source files (`.pas`, `.dpr`, `.lpr`, `.pp`)
   - List of data files in the `data/` directory
   - Real-time file system scanning

2. **Component Hierarchy** (`design://hierarchy`)
   - Complete component tree structure
   - Component names, classes, and paths
   - Parent-child relationships
   - Real-time design state

3. **Design Screenshot** (`design://screenshot`)
   - Base64-encoded PNG of current design viewport
   - Real-time capture of design viewport with automatic fallback

### 🛠️ **Available Tools**

1. **get_project_info**
   - Retrieve comprehensive project information
   - File listings and project metadata

2. **get_component_property**
   - Read component property values
   - Type-safe property access
   - Support for basic types (string, integer, float, boolean, enum)

3. **set_component_property**
   - Modify component property values
   - Automatic type conversion and validation
   - Real-time design updates

### 💬 **Available Prompts**

1. **project_overview**
   - Generate comprehensive project analysis
   - Code structure and architecture insights
   - Development recommendations

## Quick Start Guide

### Step 1: Enable the MCP Server

1. **Open Castle Game Engine Editor**
   - Launch the Castle Game Engine editor
   - Open an existing project or create a new one

2. **Access Preferences**
   - Go to **Edit → Preferences** (or **Castle Editor → Preferences** on macOS)
   - Navigate to the **MCP Server** tab

3. **Configure Server Settings**
   - Check **"Enable MCP Server"** to activate the functionality
   - Set the **Port** (default: 3000, or choose any available port)
   - The status should show "Stopped" initially

4. **Start the Server**
   - Click **"Start Server"** button
   - Status should change to "Running on port XXXX"
   - The server is now ready to accept connections

### Step 2: Connect Your AI Client

The MCP server supports the standard MCP protocol. AI clients can connect using the initialization handshake:

```json
{
  "jsonrpc": "2.0",
  "id": "1",
  "method": "initialize",
  "params": {
    "protocolVersion": "2025-06-18",
    "capabilities": {},
    "clientInfo": {
      "name": "your-ai-client",
      "version": "1.0.0"
    }
  }
}
```

### Step 3: Verify Connection

After successful initialization, you should receive a response like:

```json
{
  "jsonrpc": "2.0",
  "id": "1",
  "result": {
    "protocolVersion": "2025-06-18",
    "serverInfo": {
      "name": "castle-engine-editor",
      "version": "1.0.0"
    },
    "capabilities": {
      "resources": {},
      "tools": {},
      "prompts": {}
    }
  }
}
```

### Step 4: AI Assistant Integration

For detailed instructions on integrating with specific AI assistants:

- **Auggie**: See [Auggie Integration Guide](mcp_server_auggie_integration.md) for complete setup instructions
- **Other MCP Clients**: Use the standard MCP protocol as documented in this guide

**Note**: The standalone MCP server executable is not included in the repository. Users must compile it locally using the provided source code.

## Complete API Reference

### Core MCP Methods

#### 1. Server Initialization

**Method:** `initialize`

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": "1",
  "method": "initialize",
  "params": {
    "protocolVersion": "2025-06-18",
    "capabilities": {},
    "clientInfo": {
      "name": "your-client-name",
      "version": "1.0.0"
    }
  }
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": "1",
  "result": {
    "protocolVersion": "2025-06-18",
    "serverInfo": {
      "name": "castle-engine-editor",
      "version": "1.0.0"
    },
    "capabilities": {
      "resources": {},
      "tools": {},
      "prompts": {}
    }
  }
}
```

#### 2. List Available Resources

**Method:** `resources/list`

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": "2",
  "method": "resources/list",
  "params": {}
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": "2",
  "result": {
    "resources": [
      {
        "uri": "project://info",
        "name": "Project Information",
        "description": "Basic project information",
        "mimeType": "application/json"
      },
      {
        "uri": "design://hierarchy",
        "name": "Component Hierarchy",
        "description": "Current design component tree",
        "mimeType": "application/json"
      },
      {
        "uri": "design://screenshot",
        "name": "Design Screenshot",
        "description": "Current design viewport image",
        "mimeType": "image/png"
      }
    ]
  }
}
```

### Resource Access

#### 1. Get Project Information

**Method:** `resources/read`
**URI:** `project://info`

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": "3",
  "method": "resources/read",
  "params": {
    "uri": "project://info"
  }
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": "3",
  "result": {
    "contents": [
      {
        "type": "text",
        "text": "{\"name\":\"MyGame\",\"caption\":\"My Awesome Game\",\"path\":\"/path/to/project\",\"pascalFiles\":[\"src/main.pas\",\"src/gameunit.pas\"],\"dataFiles\":[\"data/player.png\",\"data/sound.wav\"]}"
      }
    ]
  }
}
```

#### 2. Get Component Hierarchy

**Method:** `resources/read`
**URI:** `design://hierarchy`

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": "4",
  "method": "resources/read",
  "params": {
    "uri": "design://hierarchy"
  }
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": "4",
  "result": {
    "contents": [
      {
        "type": "text",
        "text": "{\"name\":\"MainForm\",\"class\":\"TCastleView\",\"path\":\"MainForm\",\"children\":[{\"name\":\"ButtonStart\",\"class\":\"TCastleButton\",\"path\":\"MainForm.ButtonStart\",\"children\":[]}]}"
      }
    ]
  }
}
```

#### 3. Get Design Screenshot

**Method:** `resources/read`
**URI:** `design://screenshot`

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": "5",
  "method": "resources/read",
  "params": {
    "uri": "design://screenshot"
  }
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": "5",
  "result": {
    "contents": [
      {
        "type": "resource",
        "resource": {
          "uri": "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChAI9jU77zgAAAABJRU5ErkJggg=="
        }
      }
    ]
  }
}
```

### Tool Execution

#### 1. List Available Tools

**Method:** `tools/list`

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": "6",
  "method": "tools/list",
  "params": {}
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": "6",
  "result": {
    "tools": [
      {
        "name": "get_project_info",
        "description": "Get basic project information"
      },
      {
        "name": "get_component_property",
        "description": "Get component property value"
      },
      {
        "name": "set_component_property",
        "description": "Set component property value"
      }
    ]
  }
}
```

#### 2. Get Component Property

**Method:** `tools/call`
**Tool:** `get_component_property`

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": "7",
  "method": "tools/call",
  "params": {
    "name": "get_component_property",
    "arguments": {
      "component_path": "MainForm.ButtonStart",
      "property_name": "Caption"
    }
  }
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": "7",
  "result": {
    "content": [
      {
        "type": "text",
        "text": "Property 'Caption' of component 'MainForm.ButtonStart' = 'Start Game'"
      }
    ]
  }
}
```

#### 3. Set Component Property

**Method:** `tools/call`
**Tool:** `set_component_property`

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": "8",
  "method": "tools/call",
  "params": {
    "name": "set_component_property",
    "arguments": {
      "component_path": "MainForm.ButtonStart",
      "property_name": "Caption",
      "value": "Begin Game"
    }
  }
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": "8",
  "result": {
    "content": [
      {
        "type": "text",
        "text": "Property 'Caption' of component 'MainForm.ButtonStart' set to 'Begin Game'"
      }
    ]
  }
}
```

### Prompt Templates

#### 1. List Available Prompts

**Method:** `prompts/list`

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": "9",
  "method": "prompts/list",
  "params": {}
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": "9",
  "result": {
    "prompts": [
      {
        "name": "project_overview",
        "description": "Get an overview of the current project"
      }
    ]
  }
}
```

#### 2. Get Project Overview Prompt

**Method:** `prompts/get`
**Prompt:** `project_overview`

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": "10",
  "method": "prompts/get",
  "params": {
    "name": "project_overview",
    "arguments": {}
  }
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": "10",
  "result": {
    "description": "Comprehensive project analysis",
    "messages": [
      {
        "role": "user",
        "content": {
          "type": "text",
          "text": "Please analyze this Castle Game Engine project and provide insights about its structure, components, and potential improvements."
        }
      }
    ]
  }
}
```

## Architecture

### Core Components

1. **MCP Server Core** (`tools/castle-editor/code/castlemcpserver_simple.pas`)
   - JSON-RPC 2.0 message handling
   - MCP protocol implementation
   - Network communication (stdio and TCP)

2. **Editor Integration** (`tools/castle-editor/code/castlemcpeditorintegration.pas`)
   - Project data providers
   - Design-time component access
   - Real editor state integration

3. **Preferences UI** (`tools/castle-editor/code/formpreferences.pas`)
   - Server configuration and control
   - Status monitoring
   - User-friendly management interface

### Data Flow

```
AI Client → JSON-RPC → MCP Server → Editor Integration → Castle Editor
                                                              ↓
AI Client ← JSON Response ← MCP Server ← Project/Design Data ←
```

## Development

### Building

The MCP server is automatically included when building the Castle Game Engine editor:

```bash
make
```

### Testing

A standalone test program is available:

```bash
cd tools/castle-editor/code
fpc test_mcp_compile.dpr
./test_mcp_compile
```

### Extending

To add new MCP capabilities:

1. **Add Resources**: Extend `HandleResourcesList` and `HandleResourcesRead`
2. **Add Tools**: Extend `HandleToolsList` and `HandleToolsCall`
3. **Add Prompts**: Extend `HandlePromptsList` and `HandlePromptsGet`

## Security Considerations

- The MCP server only accepts connections from localhost by default
- All component property modifications are validated for type safety
- File system access is restricted to the project directory
- No arbitrary code execution capabilities

## Practical Examples

### Example 1: Basic Project Analysis

This example shows how to get comprehensive project information:

```bash
# 1. Initialize connection
curl -X POST http://localhost:3000 \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": "1",
    "method": "initialize",
    "params": {
      "protocolVersion": "2025-06-18",
      "capabilities": {},
      "clientInfo": {"name": "curl-client", "version": "1.0.0"}
    }
  }'

# 2. Get project information
curl -X POST http://localhost:3000 \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": "2",
    "method": "resources/read",
    "params": {"uri": "project://info"}
  }'

# 3. Get component hierarchy
curl -X POST http://localhost:3000 \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": "3",
    "method": "resources/read",
    "params": {"uri": "design://hierarchy"}
  }'
```

### Example 2: Component Property Management

```python
import json
import requests

class CastleEditorMCP:
    def __init__(self, port=3000):
        self.url = f"http://localhost:{port}"
        self.id_counter = 1

    def call(self, method, params=None):
        payload = {
            "jsonrpc": "2.0",
            "id": str(self.id_counter),
            "method": method,
            "params": params or {}
        }
        self.id_counter += 1

        response = requests.post(self.url, json=payload)
        return response.json()

    def initialize(self):
        return self.call("initialize", {
            "protocolVersion": "2025-06-18",
            "capabilities": {},
            "clientInfo": {"name": "python-client", "version": "1.0.0"}
        })

    def get_property(self, component_path, property_name):
        return self.call("tools/call", {
            "name": "get_component_property",
            "arguments": {
                "component_path": component_path,
                "property_name": property_name
            }
        })

    def set_property(self, component_path, property_name, value):
        return self.call("tools/call", {
            "name": "set_component_property",
            "arguments": {
                "component_path": component_path,
                "property_name": property_name,
                "value": value
            }
        })

# Usage example
client = CastleEditorMCP()
client.initialize()

# Get current button caption
result = client.get_property("MainForm.ButtonStart", "Caption")
print(f"Current caption: {result}")

# Change button caption
client.set_property("MainForm.ButtonStart", "Caption", "New Caption")

# Verify the change
result = client.get_property("MainForm.ButtonStart", "Caption")
print(f"New caption: {result}")
```

### Example 3: Screenshot Capture and Analysis

```javascript
const axios = require('axios');
const fs = require('fs');

class CastleEditorClient {
    constructor(port = 3000) {
        this.baseURL = `http://localhost:${port}`;
        this.idCounter = 1;
    }

    async call(method, params = {}) {
        const payload = {
            jsonrpc: "2.0",
            id: String(this.idCounter++),
            method,
            params
        };

        const response = await axios.post(this.baseURL, payload);
        return response.data;
    }

    async initialize() {
        return this.call("initialize", {
            protocolVersion: "2025-06-18",
            capabilities: {},
            clientInfo: { name: "js-client", version: "1.0.0" }
        });
    }

    async getScreenshot() {
        const result = await this.call("resources/read", {
            uri: "design://screenshot"
        });

        // Extract base64 data from data URI
        const dataUri = result.result.contents[0].resource.uri;
        const base64Data = dataUri.split(',')[1];

        // Save to file
        const buffer = Buffer.from(base64Data, 'base64');
        fs.writeFileSync('design_screenshot.png', buffer);

        return buffer;
    }
}

// Usage
(async () => {
    const client = new CastleEditorClient();
    await client.initialize();

    const screenshot = await client.getScreenshot();
    console.log(`Screenshot saved: ${screenshot.length} bytes`);
})();
```

## Troubleshooting Guide

### Common Issues

#### 1. Server Won't Start

**Symptoms:**
- "Start Server" button doesn't change status
- Error messages in editor log
- Status remains "Stopped"

**Solutions:**
- **Port Already in Use**: Choose a different port number (try 3001, 3002, etc.)
- **Firewall Blocking**: Check firewall settings, allow the chosen port
- **Permissions**: Ensure editor has permission to bind to the port
- **Check Logs**: Look at Castle Game Engine editor log for specific error messages

**Diagnostic Commands:**
```bash
# Check if port is in use (Linux/macOS)
netstat -an | grep :3000
lsof -i :3000

# Check if port is in use (Windows)
netstat -an | findstr :3000
```

#### 2. Client Can't Connect

**Symptoms:**
- Connection refused errors
- Timeout when connecting
- No response from server

**Solutions:**
- **Verify Server Status**: Check preferences dialog shows "Running on port XXXX"
- **Correct Port**: Ensure client uses the same port as configured in editor
- **Network Issues**: Try connecting from same machine first (localhost)
- **Protocol**: Ensure using HTTP, not HTTPS (unless specifically configured)

**Test Connection:**
```bash
# Simple connectivity test
curl -v http://localhost:3000

# Test with basic JSON-RPC call
curl -X POST http://localhost:3000 \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":"1","method":"initialize","params":{}}'
```

#### 3. Property Changes Don't Apply

**Symptoms:**
- Tool calls succeed but properties don't change
- Error messages about invalid components
- Properties revert to original values

**Solutions:**
- **Component Path**: Verify exact component path using hierarchy resource
- **Property Name**: Check exact property name (case-sensitive)
- **Property Type**: Ensure value type matches property type
- **Design State**: Ensure design is open and active

**Debug Component Paths:**
```json
{
  "jsonrpc": "2.0",
  "id": "1",
  "method": "resources/read",
  "params": {"uri": "design://hierarchy"}
}
```

#### 4. Screenshot Capture Issues

**Symptoms:**
- Empty or black screenshots
- Error messages during capture
- Fallback 1x1 pixel images

**Solutions:**
- **Design Open**: Ensure a design file is open in the editor
- **Viewport Visible**: Make sure design viewport is visible and rendered
- **Graphics Context**: Verify OpenGL context is active
- **Timing**: Allow time for rendering before capture

### Performance Considerations

#### 1. Screenshot Capture
- Screenshots are captured in real-time and can be large
- Consider caching screenshots if requesting frequently
- Base64 encoding increases size by ~33%

#### 2. Component Hierarchy
- Large designs may have extensive hierarchies
- Consider filtering or pagination for very large component trees
- Hierarchy data is generated on-demand

#### 3. Network Latency
- All communication is synchronous JSON-RPC
- Consider connection pooling for multiple requests
- Implement timeouts in client code

### Logging and Debugging

#### Enable Detailed Logging

The Castle Game Engine editor logs all MCP server activity. To see detailed logs:

1. **Editor Console**: Check the editor's built-in log console
2. **Log Files**: Look for log files in the editor's data directory
3. **Verbose Mode**: Enable verbose logging in editor preferences

#### Log Message Examples

```
[MCP] Server started on port 3000
[MCP] Client connected from 127.0.0.1
[MCP] Processing request: initialize
[MCP] Screenshot captured successfully (800x600 pixels, 12345 bytes base64)
[MCP] Property 'Caption' of component 'MainForm.ButtonStart' set to 'New Value'
[MCP] Error capturing screenshot: No design frame available
```

#### Debug Mode

For development and debugging, you can enable additional logging:

```pascal
// In editor preferences or configuration
WritelnLog('MCP', 'Debug message here');
```

### Best Practices

#### 1. Connection Management
- Always call `initialize` before other methods
- Handle connection errors gracefully
- Implement reconnection logic for long-running clients

#### 2. Error Handling
- Check JSON-RPC error responses
- Implement fallbacks for failed operations
- Log errors for debugging

#### 3. Resource Management
- Don't request screenshots too frequently
- Cache component hierarchy data when possible
- Close connections properly when done

#### 4. Security
- Only connect from trusted networks
- Validate all data received from server
- Consider authentication for production use

## Future Enhancements

- **TCP Network Server**: Complete implementation for remote connections (framework ready)
- **Advanced Tools**: Code generation, refactoring, and analysis tools
- **Streaming Updates**: Real-time notifications of design changes
- **Authentication**: Secure client authentication and authorization
- **Plugin System**: Extensible MCP capabilities through plugins
- **Performance Optimization**: Caching, compression, and connection pooling

## References

- [Auggie Integration Guide](mcp_server_auggie_integration.md) - Step-by-step guide for using with Auggie AI assistant
- [Quick Reference Guide](mcp_server_quick_reference.md) - One-page developer cheat sheet
- [Implementation Summary](mcp_server_implementation_summary.md) - Technical overview and achievements
- [Model Context Protocol Specification](https://spec.modelcontextprotocol.io/)
- [Castle Game Engine Documentation](https://castle-engine.io/documentation.php)
- [JSON-RPC 2.0 Specification](https://www.jsonrpc.org/specification)
