# Standalone MCP server for Castle Game Engine editor (simplified version).

This program provides a working MCP server that can be used with AI clients
like Auggie. It uses mock providers to simulate project and design data.

Usage:
  mcp_server_simple_standalone

The server will read JSON-RPC messages from stdin and write
responses to stdout, following the MCP protocol.

Example usage with Auggie:

Add this to your MCP client configuration:

```
{
  "command": "/path/to/mcp_server_simple_standalone",
  "args": []
}
```