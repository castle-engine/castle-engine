# How to Use Castle Game Engine MCP Server with Auggie

This guide explains how to integrate the Castle Game Engine MCP server with Auggie (or other MCP-compatible AI assistants) for AI-assisted game development.

## 🎯 Overview

The Castle Game Engine MCP server enables AI assistants to:
- Access project structure and file listings
- Read and modify component properties
- Capture design viewport screenshots
- Analyze project architecture and provide development guidance

## 🔧 Prerequisites

- Castle Game Engine editor installed and working
- Auggie or another MCP-compatible AI client
- A Castle Game Engine project (optional for testing)

## 📦 Testing Methods

### Method 1: Mock standalone MCP Server

The standalone server provides a working MCP interface with mock data, perfect for testing and development.

#### Step 1: Build the Standalone Server

```bash
# Compile the mock MCP server
cd tools/internal/mcp_server_mock/
castle-engine compile
```

#### Step 2: Configure Auggie

Add the following to your Augment (and thus also Auggie) MCP client configuration in `~/.augment/settings.json`:

```json
{
  "mcpServers": {
    "castle-engine": {
      "command": "/path/to/castle-engine/tools/internal/mcp_server_mock/mcp_server_mock",
      "args": [],
      "env": {}
    }
  }
}
```

Or execute this command:

```
auggie mcp add castle-engine --command /path/to/castle-engine/tools/internal/mcp_server_mock/mcp_server_mock
auggie mcp list
```

See https://docs.augmentcode.com/cli/integrations

Adding MCP server to _Augment_ GUI in VS Code is also possible -- e.g. use _"Import from JSON"_ is Augument settings in VS Code, and provide JSON snippet like above.

Our MCP server should be also available from:

- GitHub Copilot in VS Code: https://code.visualstudio.com/docs/copilot/customization/mcp-servers

- Claude Desktop: https://modelcontextprotocol.io/docs/develop/connect-local-servers

#### Step 3: Test the Connection

```bash
# Test basic functionality
cd /path/to/castle-engine/tools/internal/mcp_server_mock/
echo '{"jsonrpc":"2.0","id":"1","method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"test","version":"1.0.0"}}}' | ./mcp_server_mock
```

Expected response:
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

### Method 2: Full Editor Integration (Future)

When the TCP server implementation is complete, you'll be able to connect directly to the running Castle Game Engine editor.

#### Step 1: Enable MCP Server in Editor
1. Open Castle Game Engine editor
2. Go to **Edit → Preferences**
3. Navigate to **MCP Server** tab
4. Check **"Enable MCP Server"**
5. Set port (default: 3000)
6. Click **"Start Server"**

#### Step 2: Configure Auggie for Network Connection
```json
{
  "mcpServers": {
    "castle-engine": {
      "command": "curl",
      "args": ["-X", "POST", "http://localhost:3000", "-H", "Content-Type: application/json", "-d", "@-"],
      "env": {}
    }
  }
}
```

## 🎮 Available Capabilities

### Resources

#### 1. Project Information (`project://info`)
Get comprehensive project metadata and file listings.

**Example Request:**
```json
{
  "jsonrpc": "2.0",
  "id": "2",
  "method": "resources/read",
  "params": {
    "uri": "project://info"
  }
}
```

**Response Data:**
- Project name, caption, and path
- List of Pascal source files (`.pas`, `.dpr`, `.lpr`)
- List of data files in the `data/` directory
- Real-time file system scanning

#### 2. Component Hierarchy (`design://hierarchy`)
Access the complete component tree structure.

**Example Request:**
```json
{
  "jsonrpc": "2.0",
  "id": "3",
  "method": "resources/read",
  "params": {
    "uri": "design://hierarchy"
  }
}
```

**Response Data:**
- Component names, classes, and paths
- Parent-child relationships
- Real-time design state

#### 3. Design Screenshot (`design://screenshot`)
Capture the current design viewport as a base64-encoded PNG.

**Example Request:**
```json
{
  "jsonrpc": "2.0",
  "id": "4",
  "method": "resources/read",
  "params": {
    "uri": "design://screenshot"
  }
}
```

**Response Data:**
- Base64-encoded PNG image
- Real-time viewport capture
- Automatic fallback on errors

### Tools

#### 1. Get Project Info (`get_project_info`)
Retrieve detailed project information.

**Example Request:**
```json
{
  "jsonrpc": "2.0",
  "id": "5",
  "method": "tools/call",
  "params": {
    "name": "get_project_info",
    "arguments": {}
  }
}
```

#### 2. Get Component Property (`get_component_property`)
Read component property values with type safety.

**Example Request:**
```json
{
  "jsonrpc": "2.0",
  "id": "6",
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

#### 3. Set Component Property (`set_component_property`)
Modify component properties with validation.

**Example Request:**
```json
{
  "jsonrpc": "2.0",
  "id": "7",
  "method": "tools/call",
  "params": {
    "name": "set_component_property",
    "arguments": {
      "component_path": "MainForm.ButtonStart",
      "property_name": "Caption",
      "value": "New Button Text"
    }
  }
}
```

### Prompts

#### 1. Project Overview (`project_overview`)
Get AI-friendly project analysis prompts.

**Example Request:**
```json
{
  "jsonrpc": "2.0",
  "id": "8",
  "method": "prompts/get",
  "params": {
    "name": "project_overview",
    "arguments": {}
  }
}
```

## 🛠️ Practical Usage Examples

### Example 1: Project Analysis with Auggie

1. **Connect to MCP Server**: Auggie automatically connects using your configuration
2. **Ask for Project Overview**: "Can you analyze this Castle Game Engine project?"
3. **Auggie Actions**:
   - Calls `resources/read` with `project://info` to get file structure
   - Calls `resources/read` with `design://hierarchy` to understand components
   - Calls `resources/read` with `design://screenshot` to see the design
   - Provides comprehensive analysis and suggestions

### Example 2: Component Property Modification

1. **Request Change**: "Change the button caption to 'Start Game'"
2. **Auggie Actions**:
   - Calls `resources/read` with `design://hierarchy` to find buttons
   - Calls `tools/call` with `get_component_property` to check current caption
   - Calls `tools/call` with `set_component_property` to update the caption
   - Confirms the change was successful

### Example 3: Visual Design Review

1. **Request Review**: "Review the current design layout"
2. **Auggie Actions**:
   - Calls `resources/read` with `design://screenshot` to capture current design
   - Analyzes the visual layout from the screenshot
   - Calls `resources/read` with `design://hierarchy` to understand structure
   - Provides design feedback and improvement suggestions

## 📋 Current Limitations

### Mock Server (Current Implementation)
- **Mock Data**: Uses simulated project and component data
- **No Real Editor**: Not connected to actual Castle Game Engine projects
- **Testing Purpose**: Primarily for MCP protocol testing and development

### Future Full Integration
- **Real Project Data**: Access to actual open projects
- **Live Updates**: Real-time changes reflected immediately
- **Network Access**: TCP server for remote connections

## 🚀 Getting Started Checklist

- [ ] Castle Game Engine installed and working
- [ ] Standalone MCP server compiled successfully
- [ ] Auggie MCP configuration updated with correct path
- [ ] Basic connection test successful
- [ ] First MCP request/response working
- [ ] Ready to explore AI-assisted development!

## 📚 Additional Resources

- [Complete MCP Server Documentation](mcp_server.md)
- [Quick Reference Guide](mcp_server_quick_reference.md)
- [Implementation Summary](mcp_server_implementation_summary.md)
- [MCP Protocol Specification](https://spec.modelcontextprotocol.io/)

## 🎯 Next Steps

1. **Test Basic Functionality**: Verify connection and basic requests work
2. **Explore Capabilities**: Try different resources and tools
3. **Integrate with Workflow**: Use Auggie for actual game development tasks
4. **Provide Feedback**: Report issues or suggestions for improvement
5. **Stay Updated**: Watch for TCP server implementation and enhanced features

**Happy AI-assisted game development! 🎮🤖**
