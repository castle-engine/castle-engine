program mcp_server_standalone;

{$mode objfpc}{$H+}

{ Standalone MCP server for Castle Game Engine editor.
  
  This program can be used to test the MCP server functionality
  without running the full editor. It uses mock providers to
  simulate project and design data.
  
  Usage:
    mcp_server_standalone
  
  The server will read JSON-RPC messages from stdin and write
  responses to stdout, following the MCP protocol.
}

uses
  {$ifdef UNIX}
  CThreads,
  {$endif}
  Classes, SysUtils, CastleLog, CastleApplicationProperties,
  CastleMcpServer;

var
  Server: TMcpServer;

begin
  { Initialize logging }
  InitializeLog;
  
  { Create mock MCP server }
  WritelnLog('Starting Castle Game Engine MCP Server (standalone mode)');
  WritelnLog('Using mock project and design providers for testing');
  
  Server := CreateMockMcpServer;
  try
    { Run the server in stdio mode }
    Server.RunStdio;
  finally
    FreeAndNil(Server);
  end;
  
  WritelnLog('MCP Server shutdown complete');
end.
