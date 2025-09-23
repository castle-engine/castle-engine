# Standalone MCP server for Castle Game Engine editor.

This program can be used to test the MCP server functionality
without running the full editor. It uses mock providers to
simulate project and design data.

Usage:
  mcp_server_standalone

The server will read JSON-RPC messages from stdin and write
responses to stdout, following the MCP protocol.
