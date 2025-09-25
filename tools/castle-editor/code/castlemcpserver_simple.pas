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

{ Simple MCP (Model Context Protocol) server implementation for Castle Game Engine editor.

  This is a simplified version that focuses on basic JSON-RPC functionality
  without complex dependencies.
}
unit castlemcpserver_simple;

{$I castleconf.inc}

{ Log every request and response. }
{$define MCP_VERBOSE_LOG}

// TODO: This code repeats lots of IndexOfName.
// TODO: split request / response into 2 classes from 1 common ancestor?
// would make it where we deal with what.

interface

uses
  Classes, SysUtils, FpJson, JsonParser, CastleUtils, CastleStringUtils,
  CastleClassUtils, CastleLog, CastleApplicationProperties, ssockets, sockets,
  SyncObjs;

type
  { Forward declarations }
  TSimpleMcpServer = class;

  { JSON-RPC 2.0 message types }
  TJsonRpcMessageType = (jrmtRequest, jrmtResponse, jrmtNotification, jrmtError);

  TMcpIdType = (jritEmpty, jritInt, jritString);

  { ID in MCP is either string or integer,
    so one (not more, not less) of Int, Str is used. }
  TMcpId = record
    { Define whether id is available at all, and if yes -> what type.
      Note that integer id = 0 is a valid id, so we cannot use Int <> 0 to check
      whether id is available, that is why we have IdType. }
    IdType: TMcpIdType;
    Int: Int64;
    Str: String;

    { Parse from JSON data, either string or number. }
    class function FromJson(const JsonData: TJsonData): TMcpId; static;

    { Empty ID (no int or string), special value we use in some cases. }
    class function Empty: TMcpId; static;

    function ToString: String;
  end;

  { Simple JSON-RPC 2.0 message }
  TSimpleJsonRpcMessage = class
  public
    MessageType: TJsonRpcMessageType;
    Id: TMcpId;
    Method: String;
    Params: TJsonObject;
    ResultData: TJsonObject;
    Error: TJsonObject;

    constructor Create;
    destructor Destroy; override;

    { Parse JSON-RPC message from JSON string }
    class function FromJson(const JsonStr: String): TSimpleJsonRpcMessage;

    { Convert to JSON string }
    function ToJson: String;

    { Create simple response }
    class function CreateResponse(const AId: TMcpId; const ResultStr: String): TSimpleJsonRpcMessage;

    { Create simple error response }
    class function CreateError(const AId: TMcpId; ACode: Integer; const AMessage: String): TSimpleJsonRpcMessage;
  end;

  { Future: Network server implementation will be added here }

  { Simple MCP Server implementation }
  TSimpleMcpServer = class
  private
    FInitialized: Boolean;
    FRunning: Boolean;
    FPort: Integer;
    { Future: Network server variables will be added here }

    { Message handlers }
    function HandleInitialize(const Params: TJsonObject): TJsonObject;
    function HandleResourcesList(const Params: TJsonObject): TJsonObject;
    function HandleToolsList(const Params: TJsonObject): TJsonObject;
    function HandlePromptsList(const Params: TJsonObject): TJsonObject;

    { Future: Network handling methods will be added here }

  public
    constructor Create;
    destructor Destroy; override;

    { Process a JSON-RPC message and return response }
    function ProcessMessage(const JsonStr: String): String;

    { Server control }
    procedure StartServer(APort: Integer);
    procedure StopServer;
    procedure RunStdio;

    { Properties }
    property Initialized: Boolean read FInitialized;
    property Running: Boolean read FRunning;
    property Port: Integer read FPort;
  end;

{ Create simple MCP server for testing }
function CreateSimpleMcpServer: TSimpleMcpServer;

implementation

{ TMcpId --------------------------------------------------------------------- }

class function TMcpId.FromJson(const JsonData: TJsonData): TMcpId; static;
begin
  if JsonData is TJSONNumber then
  begin
    Result.Int := JsonData.AsInt64;
    Result.IdType := jritInt;
  end else
  if JsonData is TJSONString then
  begin
    Result.IdType := jritString;
    Result.Str := JsonData.AsString
  end else
    raise Exception.CreateFmt('Invalid JSON-RPC id type: %s', [JsonData.ClassName]);
end;

class function TMcpId.Empty: TMcpId; static;
begin
  Result.IdType := jritEmpty;
  // Int and Str are not important, but set to be deterministic for easier debugging
  Result.Int := 0;
  Result.Str := '';
end;

function TMcpId.ToString: String;
begin
  case IdType of
    jritEmpty: Result := 'empty';
    jritInt: Result := 'integer ' + IntToStr(Int);
    jritString: Result := 'string "' + Str + '"';
    else raise Exception.Create('Invalid TMcpId.IdType');
  end;
end;

{ TSimpleJsonRpcMessage ------------------------------------------------------ }

constructor TSimpleJsonRpcMessage.Create;
begin
  inherited Create;
  Method := '';
  Params := nil;
  ResultData := nil;
  Error := nil;
end;

destructor TSimpleJsonRpcMessage.Destroy;
begin
  FreeAndNil(Params);
  FreeAndNil(ResultData);
  FreeAndNil(Error);
  inherited Destroy;
end;

class function TSimpleJsonRpcMessage.FromJson(const JsonStr: String): TSimpleJsonRpcMessage;
var
  JsonData, IdData: TJsonData;
  JsonObj: TJsonObject;
begin
  Result := TSimpleJsonRpcMessage.Create;
  try
    JsonData := GetJSON(JsonStr);
    try
      if not (JsonData is TJsonObject) then
        raise Exception.Create('JSON-RPC message must be an object');

      JsonObj := TJsonObject(JsonData);

      // Check for required jsonrpc field
      if JsonObj.Get('jsonrpc', '') <> '2.0' then
        raise Exception.Create('Invalid JSON-RPC version');

      // Determine message type and parse accordingly
      if JsonObj.IndexOfName('method') >= 0 then
      begin
        Result.Method := JsonObj.Get('method', '');
        IdData := JsonObj.Find('id');
        if IdData <> nil then
        begin
          Result.MessageType := jrmtRequest;
          Result.Id := TMcpId.FromJson(IdData);
          WritelnLog('Got message request, with id %s', [
            Result.Id.ToString
          ]);
        end else
        begin
          Result.MessageType := jrmtNotification;
        end;

        if JsonObj.IndexOfName('params') >= 0 then
        begin
          if JsonObj.Items[JsonObj.IndexOfName('params')] is TJsonObject then
            Result.Params := TJsonObject(JsonObj.Items[JsonObj.IndexOfName('params')].Clone);
        end;
      end
      else if JsonObj.IndexOfName('result') >= 0 then
      begin
        Result.MessageType := jrmtResponse;
        if JsonObj.Items[JsonObj.IndexOfName('result')] is TJsonObject then
          Result.ResultData := TJsonObject(JsonObj.Items[JsonObj.IndexOfName('result')].Clone);
      end
      else if JsonObj.IndexOfName('error') >= 0 then
      begin
        Result.MessageType := jrmtError;
        if JsonObj.Items[JsonObj.IndexOfName('error')] is TJsonObject then
          Result.Error := TJsonObject(JsonObj.Items[JsonObj.IndexOfName('error')].Clone);
      end
      else
        raise Exception.Create('Invalid JSON-RPC message format');

    finally
      FreeAndNil(JsonData);
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TSimpleJsonRpcMessage.ToJson: String;
var
  JsonObj: TJsonObject;
begin
  JsonObj := TJsonObject.Create;
  try
    JsonObj.Add('jsonrpc', '2.0');

    case Id.IdType of
      jritInt: JsonObj.Add('id', Id.Int);
      jritString: JsonObj.Add('id', Id.Str);
    end;

    case MessageType of
      jrmtRequest:
        begin
          JsonObj.Add('method', Method);
          if Assigned(Params) then
            JsonObj.Add('params', Params.Clone);
        end;
      jrmtNotification:
        begin
          JsonObj.Add('method', Method);
          if Assigned(Params) then
            JsonObj.Add('params', Params.Clone);
        end;
      jrmtResponse:
        begin
          if Assigned(ResultData) then
            JsonObj.Add('result', ResultData.Clone)
          else
            JsonObj.Add('result', TJsonNull.Create);
        end;
      jrmtError:
        begin
          if Assigned(Error) then
            JsonObj.Add('error', Error.Clone);
        end;
    end;

    Result := JsonObj.AsJSON;
  finally
    FreeAndNil(JsonObj);
  end;
end;

class function TSimpleJsonRpcMessage.CreateResponse(const AId: TMcpId; const ResultStr: String): TSimpleJsonRpcMessage;
begin
  Result := TSimpleJsonRpcMessage.Create;
  Result.MessageType := jrmtResponse;
  Result.Id := AId;
  Result.ResultData := TJsonObject.Create;
  Result.ResultData.Add('message', ResultStr);
end;

class function TSimpleJsonRpcMessage.CreateError(const AId: TMcpId; ACode: Integer; const AMessage: String): TSimpleJsonRpcMessage;
begin
  Result := TSimpleJsonRpcMessage.Create;
  Result.MessageType := jrmtError;
  Result.Id := AId;
  Result.Error := TJsonObject.Create;
  Result.Error.Add('code', ACode);
  Result.Error.Add('message', AMessage);
end;

{ TSimpleMcpServer }

constructor TSimpleMcpServer.Create;
begin
  inherited Create;
  FInitialized := False;
  FRunning := False;
  FPort := 0;
end;

destructor TSimpleMcpServer.Destroy;
begin
  StopServer;
  inherited Destroy;
end;

function TSimpleMcpServer.ProcessMessage(const JsonStr: String): String;
var
  Request, Response: TSimpleJsonRpcMessage;
  ResponseData: TJsonObject;
begin
  Result := '';
  Request := nil;
  Response := nil;
  try
    try
      Request := TSimpleJsonRpcMessage.FromJson(JsonStr);

      case Request.MessageType of
        jrmtRequest:
          begin
            ResponseData := nil;
            try
              if Request.Method = 'initialize' then
                ResponseData := HandleInitialize(Request.Params)
              else if Request.Method = 'resources/list' then
                ResponseData := HandleResourcesList(Request.Params)
              else if Request.Method = 'tools/list' then
                ResponseData := HandleToolsList(Request.Params)
              else if Request.Method = 'prompts/list' then
                ResponseData := HandlePromptsList(Request.Params)
              else
                raise Exception.CreateFmt('Unknown method: %s', [Request.Method]);

              Response := TSimpleJsonRpcMessage.Create;
              Response.MessageType := jrmtResponse;
              Response.Id := Request.Id;
              Response.ResultData := ResponseData;
            except
              on E: Exception do
              begin
                FreeAndNil(ResponseData);
                Response := TSimpleJsonRpcMessage.CreateError(Request.Id, -32603, E.Message);
              end;
            end;
          end;
        jrmtNotification:
          begin
            if Request.Method = 'initialized' then
              FInitialized := True;
            Exit; // No response for notifications
          end;
        else
          raise Exception.Create('Invalid message type for server');
      end;

      if Assigned(Response) then
        Result := Response.ToJson;

    except
      on E: Exception do
      begin
        FreeAndNil(Response);
        Response := TSimpleJsonRpcMessage.CreateError(TMcpId.Empty, -32700, 'Parse error: ' + E.Message);
        Result := Response.ToJson;
      end;
    end;
  finally
    FreeAndNil(Request);
    FreeAndNil(Response);
  end;
end;

function TSimpleMcpServer.HandleInitialize(const Params: TJsonObject): TJsonObject;
var
  ServerInfo, Capabilities: TJsonObject;
begin
  Result := TJsonObject.Create;
  Result.Add('protocolVersion', '2025-06-18');

  ServerInfo := TJsonObject.Create;
  ServerInfo.Add('name', 'castle-engine-editor');
  ServerInfo.Add('version', '1.0.0');
  Result.Add('serverInfo', ServerInfo);

  Capabilities := TJsonObject.Create;
  Capabilities.Add('resources', TJsonObject.Create);
  Capabilities.Add('tools', TJsonObject.Create);
  Capabilities.Add('prompts', TJsonObject.Create);
  Result.Add('capabilities', Capabilities);
end;

function TSimpleMcpServer.HandleResourcesList(const Params: TJsonObject): TJsonObject;
var
  Resources: TJsonArray;
  Resource: TJsonObject;
begin
  Result := TJsonObject.Create;
  Resources := TJsonArray.Create;

  Resource := TJsonObject.Create;
  Resource.Add('uri', 'project://info');
  Resource.Add('name', 'Project Information');
  Resource.Add('description', 'Basic project information');
  Resource.Add('mimeType', 'application/json');
  Resources.Add(Resource);

  Result.Add('resources', Resources);
end;

function TSimpleMcpServer.HandleToolsList(const Params: TJsonObject): TJsonObject;
var
  Tools: TJsonArray;
  Tool: TJsonObject;
begin
  Result := TJsonObject.Create;
  Tools := TJsonArray.Create;

  Tool := TJsonObject.Create;
  Tool.Add('name', 'get_project_info');
  Tool.Add('description', 'Get basic project information');
  Tool.Add('inputSchema', TJsonObject.Create);
  TJsonObject(Tool.Find('inputSchema')).Add('type', 'object');
  TJsonObject(Tool.Find('inputSchema')).Add('properties', TJsonObject.Create);
  TJsonObject(TJsonObject(Tool.Find('inputSchema')).Find('properties')).Add('path', TJsonObject.Create);
  TJsonObject(TJsonObject(Tool.Find('inputSchema')).Find('properties')).Objects['path'].Add('type', 'string');
  TJsonObject(TJsonObject(Tool.Find('inputSchema')).Find('properties')).Objects['path'].Add('description', 'Project path');
  TJsonObject(TJsonObject(Tool.Find('inputSchema')).Find('properties')).Add('detailed', TJsonObject.Create);
  TJsonObject(TJsonObject(Tool.Find('inputSchema')).Find('properties')).Objects['detailed'].Add('type', 'boolean');
  TJsonObject(TJsonObject(Tool.Find('inputSchema')).Find('properties')).Objects['detailed'].Add('description', 'Include detailed information');
  TJsonObject(Tool.Find('inputSchema')).Add('required', TJsonArray.Create);
  TJsonArray(TJsonObject(Tool.Find('inputSchema')).Arrays['required']).Add('path');
  Tools.Add(Tool);

  Result.Add('tools', Tools);
end;

function TSimpleMcpServer.HandlePromptsList(const Params: TJsonObject): TJsonObject;
var
  Prompts: TJsonArray;
  Prompt: TJsonObject;
begin
  Result := TJsonObject.Create;
  Prompts := TJsonArray.Create;

  Prompt := TJsonObject.Create;
  Prompt.Add('name', 'project_overview');
  Prompt.Add('description', 'Get an overview of the current project');
  Prompts.Add(Prompt);

  Result.Add('prompts', Prompts);
end;

procedure TSimpleMcpServer.StartServer(APort: Integer);
begin
  if FRunning then
    StopServer;

  try
    FPort := APort;
    // For now, just mark as running - actual network implementation will be added later
    FRunning := True;

    WritelnLog('MCP Server started on port %d (stdio mode)', [FPort]);
  except
    on E: Exception do
    begin
      WritelnLog('Failed to start MCP Server on port %d: %s', [APort, E.Message]);
      FRunning := False;
      raise;
    end;
  end;
end;

procedure TSimpleMcpServer.StopServer;
begin
  if FRunning then
  begin
    FRunning := False;
    WritelnLog('MCP Server stopped');
  end;
end;

procedure TSimpleMcpServer.RunStdio;
var
  InputLine, Response: String;
begin
  WritelnLog('MCP Server running in stdio mode');

  while not EOF do
  begin
    ReadLn(InputLine);
    if InputLine <> '' then
    begin
      {$ifdef MCP_VERBOSE_LOG} WritelnLog('Input: ' + InputLine); {$endif}
      Response := ProcessMessage(InputLine);
      if Response <> '' then
      begin
        {$ifdef MCP_VERBOSE_LOG} WritelnLog('Output: ' + Response); {$endif}
        WriteLn(Response);
        Flush(Output); // Ensure response is sent immediately
      end else
        {$ifdef MCP_VERBOSE_LOG} WritelnLog('No output') {$endif};
    end;
  end;
end;

{ Future: Network server implementation will be added here }

function CreateSimpleMcpServer: TSimpleMcpServer;
begin
  Result := TSimpleMcpServer.Create;
end;

end.
