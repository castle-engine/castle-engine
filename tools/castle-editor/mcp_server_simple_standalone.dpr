program mcp_server_simple_standalone;

{$mode objfpc}{$H+}

{ Standalone MCP server for Castle Game Engine editor (simplified version).
  
  This program provides a working MCP server that can be used with AI clients
  like Auggie. It uses mock providers to simulate project and design data.
  
  Usage:
    mcp_server_simple_standalone
  
  The server will read JSON-RPC messages from stdin and write
  responses to stdout, following the MCP protocol.
  
  Example usage with Auggie:
    Add this to your MCP client configuration:
    {
      "command": "/path/to/mcp_server_simple_standalone",
      "args": []
    }
}

uses
  {$ifdef UNIX}
  CThreads,
  {$endif}
  Classes, SysUtils, CastleLog, CastleApplicationProperties,
  castlemcpserver_simple;

var
  Server: TSimpleMcpServer;

begin
  { Initialize logging }
  InitializeLog;
  
  { Create simple MCP server }
  WritelnLog('Starting Castle Game Engine MCP Server (simple standalone mode)');
  WritelnLog('Using mock project and design providers for testing');
  WritelnLog('Ready to accept MCP requests on stdin');
  
  Server := CreateSimpleMcpServer;
  try
    { Run the server in stdio mode }
    Server.RunStdio;
  finally
    FreeAndNil(Server);
  end;
  
  WritelnLog('MCP Server shutdown complete');
end.
