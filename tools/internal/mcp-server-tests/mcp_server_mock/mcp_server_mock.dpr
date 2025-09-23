program mcp_server_mock;

uses
  {$ifdef UNIX}
  CThreads,
  {$endif}
  Classes, SysUtils, CastleLog, CastleApplicationProperties,
  CastleClassUtils, CastleMcpServer;

var
  Server: TMcpServer;

begin
  { Initialize logging to stderr for MCP compatibility }
  InitializeLog(StdErrStream);

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
