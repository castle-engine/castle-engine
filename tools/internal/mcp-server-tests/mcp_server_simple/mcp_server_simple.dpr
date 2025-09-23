program mcp_server_simple;

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
