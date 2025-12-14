program mcp_server_mock;

uses
  {$ifdef UNIX}
  CThreads,
  {$endif}
  Classes, SysUtils, CastleLog, CastleApplicationProperties, CastleFilesUtils,
  CastleMcpServer;

function GetTempFileNameWithDateTime: String;
var
  DateTimeStr: String;
begin
  { Convert current time to string, such that files sorted alphabetically
    are also sorted by time. Makes it easy to look at correct log. }
  DateTimeStr := FormatDateTime('YYYY-MM-DD_HH-MM-SS', Now);
  Result := GetTempFileName('', ApplicationName + '_' + DateTimeStr);
end;

var
  Server: TMcpServer;

begin
  { Note: Do not send log to stdout, as it may be confused with MCP responses.
    Stderr is fine, or any dedicated file. }
  LogFileName := GetTempFileNameWithDateTime + '.log';
  InitializeLog;
  // Pointless, you will not see this from MCP user output
  // Writeln(ErrOutput, 'Logging to: ' + LogOutput);

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
