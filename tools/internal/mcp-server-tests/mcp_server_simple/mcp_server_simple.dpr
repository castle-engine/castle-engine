program mcp_server_simple;

uses
  {$ifdef UNIX}
  CThreads,
  {$endif}
  Classes, SysUtils, CastleLog, CastleApplicationProperties,
  CastleClassUtils, CastleFilesUtils, castlemcpserver_simple;

var
  Server: TSimpleMcpServer;

begin
  { Note: Do not send log to stdout, as it may be confused with MCP responses.
    Stderr is fine, or any dedicated file. }
  LogFileName := GetTempFileNameCheck + '.log';
  InitializeLog;
  // Pointless, you will not see this from MCP user output
  // Writeln(ErrOutput, 'Logging to: ' + LogOutput);

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
