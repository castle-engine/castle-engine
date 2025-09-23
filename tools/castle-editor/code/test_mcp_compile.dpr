program test_mcp_compile;

{$mode objfpc}{$H+}

{ Simple test program to verify MCP server units compile correctly }

uses
  Classes, SysUtils, CastleLog, CastleApplicationProperties,
  castlemcpserver_simple;

var
  Server: TSimpleMcpServer;
  Message: TSimpleJsonRpcMessage;

begin
  InitializeLog;
  WritelnLog('Testing MCP server compilation...');
  
  // Test creating a simple server
  Server := CreateSimpleMcpServer;
  try
    WritelnLog('Simple MCP server created successfully');

    // Test creating a JSON-RPC message
    Message := TSimpleJsonRpcMessage.CreateResponse('1', 'test response');
    try
      WritelnLog('JSON-RPC message created: %s', [Message.ToJson]);
    finally
      FreeAndNil(Message);
    end;
    
  finally
    FreeAndNil(Server);
  end;
  
  WritelnLog('MCP server compilation test completed successfully');
end.
