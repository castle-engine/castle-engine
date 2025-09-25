{
  Copyright 2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleMcpServer unit. }
unit TestCastleMcpServer;

interface

uses Classes, SysUtils, CastleTester;

type
  TTestCastleMcpServer = class(TCastleTestCase)
  published
    procedure TestJsonRpcMessageParsing;
    procedure TestJsonRpcMessageCreation;
    procedure TestMcpServerInitialize;
    procedure TestMcpServerResourcesList;
    procedure TestMcpServerResourcesRead;
    procedure TestMcpServerToolsList;
    procedure TestMcpServerToolsCall;
    procedure TestMcpServerPrompts;
    procedure TestMockProviders;
  end;

implementation

uses FpJson, JsonParser,
  CastleMcpServer, CastleUtils, CastleStringUtils;

procedure TTestCastleMcpServer.TestJsonRpcMessageParsing;
var
  JsonStr: String;
  Message: TJsonRpcMessage;
begin
  // Test request parsing
  JsonStr := '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{}}}';
  Message := TJsonRpcMessage.FromJson(JsonStr);
  try
    AssertEquals('Message type should be request', Ord(jrmtRequest), Ord(Message.MessageType));
    AssertEquals('Method should be initialize', 'initialize', Message.Method);
    AssertTrue('ID type should be integer', itInteger = Message.Id.IdType);
    AssertEquals('ID should be 1', 1, Message.Id.Int);
    AssertTrue('Params should be assigned', Assigned(Message.Params));
  finally
    FreeAndNil(Message);
  end;

  // Test response parsing
  JsonStr := '{"jsonrpc":"2.0","id":1,"result":{"protocolVersion":"2025-06-18"}}';
  Message := TJsonRpcMessage.FromJson(JsonStr);
  try
    AssertEquals('Message type should be response', Ord(jrmtResponse), Ord(Message.MessageType));
    AssertTrue('ID type should be integer', itInteger = Message.Id.IdType);
    AssertTrue('ID should be 1', 1 = Message.Id.Int);
    AssertTrue('Result should be assigned', Message.JsonResult <> nil);
  finally
    FreeAndNil(Message);
  end;

  // Test notification parsing
  JsonStr := '{"jsonrpc":"2.0","method":"initialized","params":{}}';
  Message := TJsonRpcMessage.FromJson(JsonStr);
  try
    AssertEquals('Message type should be notification', Ord(jrmtNotification), Ord(Message.MessageType));
    AssertEquals('Method should be initialized', 'initialized', Message.Method);
  finally
    FreeAndNil(Message);
  end;
end;

procedure TTestCastleMcpServer.TestJsonRpcMessageCreation;
var
  Message: TJsonRpcMessage;
  JsonStr: String;
  Params: TJsonObject;
  TestId: TMcpId;
begin
  // Test request creation
  Params := TJsonObject.Create;
  Params.Add('test', 'value');
  TestId.IdType := itInteger;
  TestId.Int := 1;
  Message := TJsonRpcMessage.CreateRequest(TestId, 'test_method', Params);
  try
    JsonStr := Message.ToJson;
    // debug: Writeln('Output:' + JsonStr);
    AssertTrue('JSON should contain jsonrpc', Pos('"jsonrpc" : "2.0"', JsonStr) > 0);
    AssertTrue('JSON should contain method', Pos('"method" : "test_method"', JsonStr) > 0);
    AssertTrue('JSON should contain id', Pos('"id" : 1', JsonStr) > 0);
  finally
    FreeAndNil(Message);
  end;

  // Test response creation
  Params := TJsonObject.Create;
  Params.Add('success', True);
  TestId.IdType := itInteger;
  TestId.Int := 1;
  Message := TJsonRpcMessage.CreateResponse(TestId, Params);
  try
    JsonStr := Message.ToJson;
    AssertTrue('JSON should contain jsonrpc', Pos('"jsonrpc" : "2.0"', JsonStr) > 0);
    AssertTrue('JSON should contain result', Pos('"result" :', JsonStr) > 0);
    AssertTrue('JSON should contain id', Pos('"id" : 1', JsonStr) > 0);
  finally
    FreeAndNil(Message);
  end;
end;

procedure TTestCastleMcpServer.TestMcpServerInitialize;
var
  Server: TMcpServer;
  Request, Response: String;
  ResponseJson: TJsonData;
  ResponseObj: TJsonObject;
begin
  Server := CreateMockMcpServer;
  try
    // Test initialize request
    Request := '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"test-client","version":"1.0.0"}}}';
    Response := Server.ProcessMessage(Request);

    AssertTrue('Response should not be empty', Response <> '');

    ResponseJson := GetJSON(Response);
    try
      AssertTrue('Response should be JSON object', ResponseJson is TJsonObject);
      ResponseObj := TJsonObject(ResponseJson);

      AssertEquals('Should be JSON-RPC 2.0', '2.0', ResponseObj.Get('jsonrpc', ''));
      AssertEquals('ID should match', 1, ResponseObj.Get('id', 0));
      AssertTrue('Should have result', ResponseObj.IndexOfName('result') >= 0);

      // Check result structure
      ResponseObj := ResponseObj.Objects['result'];
      AssertEquals('Protocol version should match', '2025-06-18', ResponseObj.Get('protocolVersion', ''));
      AssertTrue('Should have serverInfo', ResponseObj.IndexOfName('serverInfo') >= 0);
      AssertTrue('Should have capabilities', ResponseObj.IndexOfName('capabilities') >= 0);
    finally
      FreeAndNil(ResponseJson);
    end;

    AssertFalse('Server should NOT be initialized yet', Server.Initialized);

    Request := '{"method":"notifications/initialized","jsonrpc":"2.0"}';
    Response := Server.ProcessMessage(Request);
    AssertTrue('Response should not be empty (for notification)', Response = '');

    AssertTrue('Server should be initialized', Server.Initialized);
  finally
    FreeAndNil(Server);
  end;
end;

procedure TTestCastleMcpServer.TestMcpServerResourcesList;
var
  Server: TMcpServer;
  Request, Response: String;
  ResponseJson: TJsonData;
  ResponseObj: TJsonObject;
  Resources: TJsonArray;
begin
  Server := CreateMockMcpServer;
  try
    // Initialize first
    Request := '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{}}}';
    Server.ProcessMessage(Request);

    // Test resources/list
    Request := '{"jsonrpc":"2.0","id":2,"method":"resources/list","params":{}}';
    Response := Server.ProcessMessage(Request);

    AssertTrue('Response should not be empty', Response <> '');

    ResponseJson := GetJSON(Response);
    try
      ResponseObj := TJsonObject(ResponseJson);
      AssertTrue('Should have result', ResponseObj.IndexOfName('result') >= 0);

      ResponseObj := ResponseObj.Objects['result'];
      AssertTrue('Should have resources array', ResponseObj.IndexOfName('resources') >= 0);

      Resources := ResponseObj.Arrays['resources'];
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

procedure TTestCastleMcpServer.TestMcpServerResourcesRead;
var
  Server: TMcpServer;
  Request, Response: String;
  ResponseJson: TJsonData;
  ResponseObj: TJsonObject;
begin
  Server := CreateMockMcpServer;
  try
    // Initialize first
    Request := '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{}}}';
    Server.ProcessMessage(Request);

    // Test resources/read for project info
    Request := '{"jsonrpc":"2.0","id":2,"method":"resources/read","params":{"uri":"project://info"}}';
    Response := Server.ProcessMessage(Request);

    AssertTrue('Response should not be empty', Response <> '');

    ResponseJson := GetJSON(Response);
    try
      ResponseObj := TJsonObject(ResponseJson);
      AssertTrue('Should have result', ResponseObj.IndexOfName('result') >= 0);

      ResponseObj := ResponseObj.Objects['result'];
      AssertTrue('Should have contents', ResponseObj.IndexOfName('contents') >= 0);
    finally
      FreeAndNil(ResponseJson);
    end;
  finally
    FreeAndNil(Server);
  end;
end;

procedure TTestCastleMcpServer.TestMcpServerToolsList;
var
  Server: TMcpServer;
  Request, Response: String;
  ResponseJson: TJsonData;
  ResponseObj: TJsonObject;
  Tools: TJsonArray;
begin
  Server := CreateMockMcpServer;
  try
    // Initialize first
    Request := '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{}}}';
    Server.ProcessMessage(Request);

    // Test tools/list
    Request := '{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}';
    Response := Server.ProcessMessage(Request);

    AssertTrue('Response should not be empty', Response <> '');

    ResponseJson := GetJSON(Response);
    try
      ResponseObj := TJsonObject(ResponseJson);
      AssertTrue('Should have result', ResponseObj.IndexOfName('result') >= 0);

      ResponseObj := ResponseObj.Objects['result'];
      AssertTrue('Should have tools array', ResponseObj.IndexOfName('tools') >= 0);

      Tools := ResponseObj.Arrays['tools'];
      AssertTrue('Should have at least one tool', Tools.Count > 0);

      // Check first tool structure
      ResponseObj := TJsonObject(Tools.Items[0]);
      AssertTrue('Tool should have name', ResponseObj.IndexOfName('name') >= 0);
      AssertTrue('Tool should have description', ResponseObj.IndexOfName('description') >= 0);
      AssertTrue('Tool should have inputSchema', ResponseObj.IndexOfName('inputSchema') >= 0);
    finally
      FreeAndNil(ResponseJson);
    end;
  finally
    FreeAndNil(Server);
  end;
end;

procedure TTestCastleMcpServer.TestMcpServerToolsCall;
var
  Server: TMcpServer;
  Request, Response: String;
  ResponseJson: TJsonData;
  ResponseObj: TJsonObject;
begin
  Server := CreateMockMcpServer;
  try
    // Initialize first
    Request := '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{}}}';
    Server.ProcessMessage(Request);

    // Test tools/call for get_component_property
    Request := '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"get_component_property","arguments":{"componentPath":"ViewMain.LabelTitle","propertyName":"Caption"}}}';
    Response := Server.ProcessMessage(Request);

    AssertTrue('Response should not be empty', Response <> '');

    ResponseJson := GetJSON(Response);
    try
      ResponseObj := TJsonObject(ResponseJson);
      AssertTrue('Should have result', ResponseObj.IndexOfName('result') >= 0);

      ResponseObj := ResponseObj.Objects['result'];
      AssertTrue('Should have content', ResponseObj.IndexOfName('content') >= 0);
    finally
      FreeAndNil(ResponseJson);
    end;
  finally
    FreeAndNil(Server);
  end;
end;

procedure TTestCastleMcpServer.TestMcpServerPrompts;
var
  Server: TMcpServer;
  Request, Response: String;
  ResponseJson: TJsonData;
  ResponseObj: TJsonObject;
  Prompts: TJsonArray;
begin
  Server := CreateMockMcpServer;
  try
    // Initialize first
    Request := '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-06-18","capabilities":{}}}';
    Server.ProcessMessage(Request);

    // Test prompts/list
    Request := '{"jsonrpc":"2.0","id":2,"method":"prompts/list","params":{}}';
    Response := Server.ProcessMessage(Request);

    AssertTrue('Response should not be empty', Response <> '');

    ResponseJson := GetJSON(Response);
    try
      ResponseObj := TJsonObject(ResponseJson);
      AssertTrue('Should have result', ResponseObj.IndexOfName('result') >= 0);

      ResponseObj := ResponseObj.Objects['result'];
      AssertTrue('Should have prompts array', ResponseObj.IndexOfName('prompts') >= 0);

      Prompts := ResponseObj.Arrays['prompts'];
      AssertTrue('Should have at least one prompt', Prompts.Count > 0);
    finally
      FreeAndNil(ResponseJson);
    end;
  finally
    FreeAndNil(Server);
  end;
end;

procedure TTestCastleMcpServer.TestMockProviders;
var
  ProjectProvider: TMockProjectProvider;
  DesignProvider: TMockDesignProvider;
  Files: TStringList;
  Hierarchy: TJsonObject;
  PropertyValue: String;
  Success: Boolean;
begin
  // Test mock project provider
  ProjectProvider := TMockProjectProvider.Create('TestProject', 'Test Caption', '/test/path/');
  try
    AssertEquals('Project name should match', 'TestProject', ProjectProvider.GetProjectName);
    AssertEquals('Project caption should match', 'Test Caption', ProjectProvider.GetProjectCaption);
    AssertEquals('Project path should match', '/test/path/', ProjectProvider.GetProjectPath);

    Files := ProjectProvider.GetPascalFiles;
    try
      AssertTrue('Should have Pascal files', Files.Count > 0);
    finally
      FreeAndNil(Files);
    end;

    Files := ProjectProvider.GetDataFiles;
    try
      AssertTrue('Should have data files', Files.Count > 0);
    finally
      FreeAndNil(Files);
    end;
  finally
    FreeAndNil(ProjectProvider);
  end;

  // Test mock design provider
  DesignProvider := TMockDesignProvider.Create(True);
  try
    AssertTrue('Design should be open', DesignProvider.IsDesignOpen);

    Hierarchy := DesignProvider.GetComponentHierarchy;
    try
      AssertTrue('Should have hierarchy', Assigned(Hierarchy));
      AssertTrue('Should have name', Hierarchy.IndexOfName('name') >= 0);
      AssertTrue('Should have class', Hierarchy.IndexOfName('class') >= 0);
    finally
      FreeAndNil(Hierarchy);
    end;

    PropertyValue := DesignProvider.GetComponentProperty('ViewMain.LabelTitle', 'Caption');
    AssertTrue('Should return property value', PropertyValue <> '');

    Success := DesignProvider.SetComponentProperty('ViewMain.LabelTitle', 'Caption', 'New Value');
    AssertTrue('Should succeed setting property', Success);
  finally
    FreeAndNil(DesignProvider);
  end;
end;

initialization
  RegisterTest(TTestCastleMcpServer);
end.
