{
  Copyright 2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleMcpServerSimple unit. }
unit TestCastleMcpServerSimple;

interface

uses
  Classes, SysUtils, CastleTester, FpJson, JsonParser;

type
  TTestCastleMcpServerSimple = class(TCastleTestCase)
  published
    procedure TestJsonRpcMessageParsing;
    procedure TestJsonRpcMessageCreation;
    procedure TestMcpServerInitialize;
    procedure TestMcpServerResourcesList;
    procedure TestMcpServerToolsList;
    procedure TestMcpServerPrompts;
  end;

implementation

uses CastleMcpServer, CastleUtils, CastleStringUtils;

{ TTestCastleMcpServerSimple }

procedure TTestCastleMcpServerSimple.TestJsonRpcMessageParsing;
var
  JsonStr: String;
  Message: TSimpleJsonRpcMessage;
begin
  // Test request parsing
  JsonStr := '{"jsonrpc":"2.0","id":"1","method":"initialize","params":{}}';
  Message := TSimpleJsonRpcMessage.FromJson(JsonStr);
  try
    AssertEquals('Message type should be request', Ord(jrmtRequest), Ord(Message.MessageType));
    AssertEquals('Method should be initialize', 'initialize', Message.Method);
    AssertEquals('ID should be 1', '1', Message.Id);
  finally
    FreeAndNil(Message);
  end;

  // Test response parsing
  JsonStr := '{"jsonrpc":"2.0","id":"1","result":{"protocolVersion":"2025-06-18"}}';
  Message := TSimpleJsonRpcMessage.FromJson(JsonStr);
  try
    AssertEquals('Message type should be response', Ord(jrmtResponse), Ord(Message.MessageType));
    AssertEquals('ID should be 1', '1', Message.Id);
    AssertTrue('Result should be assigned', Assigned(Message.ResultData));
  finally
    FreeAndNil(Message);
  end;

  // Test notification parsing
  JsonStr := '{"jsonrpc":"2.0","method":"initialized","params":{}}';
  Message := TSimpleJsonRpcMessage.FromJson(JsonStr);
  try
    AssertEquals('Message type should be notification', Ord(jrmtNotification), Ord(Message.MessageType));
    AssertEquals('Method should be initialized', 'initialized', Message.Method);
  finally
    FreeAndNil(Message);
  end;
end;

procedure TTestCastleMcpServerSimple.TestJsonRpcMessageCreation;
var
  Message: TSimpleJsonRpcMessage;
  JsonStr: String;
begin
  // Test response creation
  Message := TSimpleJsonRpcMessage.CreateResponse('1', 'test response');
  try
    JsonStr := Message.ToJson;
    AssertTrue('JSON should contain jsonrpc', Pos('"jsonrpc":"2.0"', JsonStr) > 0);
    AssertTrue('JSON should contain result', Pos('"result":', JsonStr) > 0);
    AssertTrue('JSON should contain id', Pos('"id":"1"', JsonStr) > 0);
  finally
    FreeAndNil(Message);
  end;

  // Test error creation
  Message := TSimpleJsonRpcMessage.CreateError('1', -32603, 'Internal error');
  try
    JsonStr := Message.ToJson;
    AssertTrue('JSON should contain jsonrpc', Pos('"jsonrpc":"2.0"', JsonStr) > 0);
    AssertTrue('JSON should contain error', Pos('"error":', JsonStr) > 0);
    AssertTrue('JSON should contain id', Pos('"id":"1"', JsonStr) > 0);
  finally
    FreeAndNil(Message);
  end;
end;

procedure TTestCastleMcpServerSimple.TestMcpServerInitialize;
var
  Server: TSimpleMcpServer;
  Request, Response: String;
  ResponseJson: TJsonData;
  ResponseObj: TJsonObject;
begin
  Server := CreateSimpleMcpServer;
  try
    // Test initialize request
    Request := '{"jsonrpc":"2.0","id":"1","method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"test-client","version":"1.0.0"}}}';
    Response := Server.ProcessMessage(Request);

    AssertTrue('Response should not be empty', Response <> '');

    ResponseJson := GetJSON(Response);
    try
      AssertTrue('Response should be JSON object', ResponseJson is TJsonObject);
      ResponseObj := TJsonObject(ResponseJson);

      AssertEquals('Should be JSON-RPC 2.0', '2.0', ResponseObj.Get('jsonrpc', ''));
      AssertEquals('ID should match', '1', ResponseObj.Get('id', ''));
      AssertTrue('Should have result', ResponseObj.IndexOfName('result') >= 0);

      // Check result structure
      ResponseObj := TJsonObject(ResponseObj.Get('result'));
      AssertEquals('Protocol version should match', '2025-06-18', ResponseObj.Get('protocolVersion', ''));
      AssertTrue('Should have serverInfo', ResponseObj.IndexOfName('serverInfo') >= 0);
      AssertTrue('Should have capabilities', ResponseObj.IndexOfName('capabilities') >= 0);
    finally
      FreeAndNil(ResponseJson);
    end;

    AssertTrue('Server should be initialized', Server.Initialized);
  finally
    FreeAndNil(Server);
  end;
end;

procedure TTestCastleMcpServerSimple.TestMcpServerResourcesList;
var
  Server: TSimpleMcpServer;
  Request, Response: String;
  ResponseJson: TJsonData;
  ResponseObj: TJsonObject;
  Resources: TJsonArray;
begin
  Server := CreateSimpleMcpServer;
  try
    // Initialize first
    Request := '{"jsonrpc":"2.0","id":"1","method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{}}}';
    Server.ProcessMessage(Request);

    // Test resources/list
    Request := '{"jsonrpc":"2.0","id":"2","method":"resources/list","params":{}}';
    Response := Server.ProcessMessage(Request);

    AssertTrue('Response should not be empty', Response <> '');

    ResponseJson := GetJSON(Response);
    try
      ResponseObj := TJsonObject(ResponseJson);
      AssertTrue('Should have result', ResponseObj.IndexOfName('result') >= 0);

      ResponseObj := TJsonObject(ResponseObj.Get('result'));
      AssertTrue('Should have resources array', ResponseObj.IndexOfName('resources') >= 0);

      Resources := TJsonArray(ResponseObj.Get('resources'));
      AssertTrue('Should have at least one resource', Resources.Count > 0);

      // Check first resource structure
      ResponseObj := TJsonObject(Resources.Items[0]);
      AssertTrue('Resource should have uri', ResponseObj.IndexOfName('uri') >= 0);
      AssertTrue('Resource should have name', ResponseObj.IndexOfName('name') >= 0);
      AssertTrue('Resource should have description', ResponseObj.IndexOfName('description') >= 0);
    finally
      FreeAndNil(ResponseJson);
    end;
  finally
    FreeAndNil(Server);
  end;
end;

procedure TTestCastleMcpServerSimple.TestMcpServerToolsList;
var
  Server: TSimpleMcpServer;
  Request, Response: String;
  ResponseJson: TJsonData;
  ResponseObj: TJsonObject;
  Tools: TJsonArray;
begin
  Server := CreateSimpleMcpServer;
  try
    // Initialize first
    Request := '{"jsonrpc":"2.0","id":"1","method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{}}}';
    Server.ProcessMessage(Request);

    // Test tools/list
    Request := '{"jsonrpc":"2.0","id":"2","method":"tools/list","params":{}}';
    Response := Server.ProcessMessage(Request);

    AssertTrue('Response should not be empty', Response <> '');

    ResponseJson := GetJSON(Response);
    try
      ResponseObj := TJsonObject(ResponseJson);
      AssertTrue('Should have result', ResponseObj.IndexOfName('result') >= 0);

      ResponseObj := TJsonObject(ResponseObj.Get('result'));
      AssertTrue('Should have tools array', ResponseObj.IndexOfName('tools') >= 0);

      Tools := TJsonArray(ResponseObj.Get('tools'));
      AssertTrue('Should have at least one tool', Tools.Count > 0);

      // Check first tool structure
      ResponseObj := TJsonObject(Tools.Items[0]);
      AssertTrue('Tool should have name', ResponseObj.IndexOfName('name') >= 0);
      AssertTrue('Tool should have description', ResponseObj.IndexOfName('description') >= 0);
    finally
      FreeAndNil(ResponseJson);
    end;
  finally
    FreeAndNil(Server);
  end;
end;

procedure TTestCastleMcpServerSimple.TestMcpServerPrompts;
var
  Server: TSimpleMcpServer;
  Request, Response: String;
  ResponseJson: TJsonData;
  ResponseObj: TJsonObject;
  Prompts: TJsonArray;
begin
  Server := CreateSimpleMcpServer;
  try
    // Initialize first
    Request := '{"jsonrpc":"2.0","id":"1","method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{}}}';
    Server.ProcessMessage(Request);

    // Test prompts/list
    Request := '{"jsonrpc":"2.0","id":"2","method":"prompts/list","params":{}}';
    Response := Server.ProcessMessage(Request);

    AssertTrue('Response should not be empty', Response <> '');

    ResponseJson := GetJSON(Response);
    try
      ResponseObj := TJsonObject(ResponseJson);
      AssertTrue('Should have result', ResponseObj.IndexOfName('result') >= 0);

      ResponseObj := TJsonObject(ResponseObj.Get('result'));
      AssertTrue('Should have prompts array', ResponseObj.IndexOfName('prompts') >= 0);

      Prompts := TJsonArray(ResponseObj.Get('prompts'));
      AssertTrue('Should have at least one prompt', Prompts.Count > 0);
    finally
      FreeAndNil(ResponseJson);
    end;
  finally
    FreeAndNil(Server);
  end;
end;

end.
