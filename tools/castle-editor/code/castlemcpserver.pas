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

{ MCP (Model Context Protocol) server implementation for Castle Game Engine editor.

  This unit implements a JSON-RPC 2.0 server following the MCP specification
  (https://modelcontextprotocol.io/specification/2025-06-18) to allow AI clients
  to interact with the Castle Game Engine editor.

  Features:
  - Project information (name, caption, file listings)
  - Component hierarchy access when design is open
  - Component property get/set operations
  - Design screenshot capture
  - Mock mode for testing without full editor
}
unit CastleMcpServer;

{$I castleconf.inc}

interface

uses
  Classes, SysUtils, FpJson, JsonParser, CastleUtils, CastleStringUtils,
  CastleClassUtils, CastleLog, CastleApplicationProperties,
  CastleUriUtils, CastleFilesUtils, CastleFindFiles;

type
  { MCP server capabilities }
  TMcpServerCapabilities = record
    Resources: Boolean;
    Tools: Boolean;
    Prompts: Boolean;
  end;

  { MCP client capabilities }
  TMcpClientCapabilities = record
    Roots: Boolean;
    Sampling: Boolean;
    Elicitation: Boolean;
  end;

  { Project information provider interface }
  IMcpProjectProvider = interface
    function GetProjectName: String;
    function GetProjectCaption: String;
    function GetProjectPath: String;
    function GetPascalFiles: TStringList;
    function GetDataFiles: TStringList;
  end;

  { Design information provider interface }
  IMcpDesignProvider = interface
    function IsDesignOpen: Boolean;
    function GetComponentHierarchy: TJsonObject;
    function GetComponentProperty(const ComponentPath, PropertyName: String): String;
    function SetComponentProperty(const ComponentPath, PropertyName, Value: String): Boolean;
    function GetDesignScreenshot: String; // Returns base64-encoded PNG data
  end;

  { Mock project provider for testing }
  TMockProjectProvider = class(TInterfacedObject, IMcpProjectProvider)
  private
    FProjectName: String;
    FProjectCaption: String;
    FProjectPath: String;
  public
    constructor Create(const AProjectName, AProjectCaption, AProjectPath: String);
    function GetProjectName: String;
    function GetProjectCaption: String;
    function GetProjectPath: String;
    function GetPascalFiles: TStringList;
    function GetDataFiles: TStringList;
  end;

  { Mock design provider for testing }
  TMockDesignProvider = class(TInterfacedObject, IMcpDesignProvider)
  private
    FDesignOpen: Boolean;
  public
    constructor Create(ADesignOpen: Boolean = True);
    function IsDesignOpen: Boolean;
    function GetComponentHierarchy: TJsonObject;
    function GetComponentProperty(const ComponentPath, PropertyName: String): String;
    function SetComponentProperty(const ComponentPath, PropertyName, Value: String): Boolean;
    function GetDesignScreenshot: String; // Returns base64-encoded PNG data
  end;

  { JSON-RPC 2.0 message types }
  TJsonRpcMessageType = (jrmtRequest, jrmtResponse, jrmtNotification, jrmtError);

  { JSON-RPC 2.0 message }
  TJsonRpcMessage = class
  public
    MessageType: TJsonRpcMessageType;
    Id: Variant; // can be string, number, or null
    Method: String; // for requests and notifications
    Params: TJsonData; // for requests and notifications
    Result: TJsonData; // for responses
    Error: TJsonObject; // for error responses

    constructor Create;
    destructor Destroy; override;

    { Parse JSON-RPC message from JSON string }
    class function FromJson(const JsonStr: String): TJsonRpcMessage;

    { Convert to JSON string }
    function ToJson: String;

    { Create request message }
    class function CreateRequest(const AId: Variant; const AMethod: String; AParams: TJsonData = nil): TJsonRpcMessage;

    { Create response message }
    class function CreateResponse(const AId: Variant; AResult: TJsonData): TJsonRpcMessage;

    { Create error response }
    class function CreateError(const AId: Variant; ACode: Integer; const AMessage: String; AData: TJsonData = nil): TJsonRpcMessage;

    { Create notification }
    class function CreateNotification(const AMethod: String; AParams: TJsonData = nil): TJsonRpcMessage;
  end;

  { MCP Server implementation }
  TMcpServer = class
  private
    FProjectProvider: IMcpProjectProvider;
    FDesignProvider: IMcpDesignProvider;
    FServerCapabilities: TMcpServerCapabilities;
    FClientCapabilities: TMcpClientCapabilities;
    FInitialized: Boolean;
    FServerInfo: TJsonObject;

    { Message handlers }
    function HandleInitialize(const Params: TJsonData): TJsonData;
    function HandleInitialized(const Params: TJsonData): TJsonData;
    function HandleResourcesList(const Params: TJsonData): TJsonData;
    function HandleResourcesRead(const Params: TJsonData): TJsonData;
    function HandleToolsList(const Params: TJsonData): TJsonData;
    function HandleToolsCall(const Params: TJsonData): TJsonData;
    function HandlePromptsList(const Params: TJsonData): TJsonData;
    function HandlePromptsGet(const Params: TJsonData): TJsonData;

    { Helper methods }
    function CreateServerInfo: TJsonObject;
    function CreateServerCapabilities: TJsonObject;
    function GetProjectResources: TJsonArray;
    function GetDesignResources: TJsonArray;
    function GetAvailableTools: TJsonArray;
    function GetAvailablePrompts: TJsonArray;

  public
    constructor Create(AProjectProvider: IMcpProjectProvider = nil; ADesignProvider: IMcpDesignProvider = nil);
    destructor Destroy; override;

    { Process a JSON-RPC message and return response }
    function ProcessMessage(const JsonStr: String): String;

    { Run server in stdio mode (read from stdin, write to stdout) }
    procedure RunStdio;

    { Properties }
    property Initialized: Boolean read FInitialized;
    property ProjectProvider: IMcpProjectProvider read FProjectProvider write FProjectProvider;
    property DesignProvider: IMcpDesignProvider read FDesignProvider write FDesignProvider;
  end;

{ Create MCP server with mock providers for testing }
function CreateMockMcpServer: TMcpServer;

implementation

uses
  Variants, TypInfo, base64;

{ Helper functions for JSON creation }

function CreateJsonArray(const StringArray: array of String): TJsonArray;
var
  I: Integer;
begin
  Result := TJsonArray.Create;
  for I := Low(StringArray) to High(StringArray) do
    Result.Add(StringArray[I]);
end;

function CreateJsonArrayWithTextContent(const Text: String): TJsonArray;
var
  ContentObj: TJsonObject;
begin
  Result := TJsonArray.Create;
  ContentObj := TJsonObject.Create;
  ContentObj.Add('type', 'text');
  ContentObj.Add('text', Text);
  Result.Add(ContentObj);
end;

function CreateJsonArrayWithPromptArgument(const Name, Description: String; Required: Boolean): TJsonArray;
var
  ArgObj: TJsonObject;
begin
  Result := TJsonArray.Create;
  ArgObj := TJsonObject.Create;
  ArgObj.Add('name', Name);
  ArgObj.Add('description', Description);
  ArgObj.Add('required', Required);
  Result.Add(ArgObj);
end;

function CreatePropertySchema(const PropType, Description: String): TJsonObject;
begin
  Result := TJsonObject.Create;
  Result.Add('type', PropType);
  Result.Add('description', Description);
end;

function CreatePropertiesObject(const ComponentPathDesc, PropertyNameDesc: String; const ValueDesc: String = ''): TJsonObject;
begin
  Result := TJsonObject.Create;
  Result.Add('componentPath', CreatePropertySchema('string', ComponentPathDesc));
  Result.Add('propertyName', CreatePropertySchema('string', PropertyNameDesc));
  if ValueDesc <> '' then
    Result.Add('value', CreatePropertySchema('string', ValueDesc));
end;

function CreateInputSchema(Properties: TJsonObject; Required: TJsonArray): TJsonObject;
begin
  Result := TJsonObject.Create;
  Result.Add('type', 'object');
  Result.Add('properties', Properties);
  Result.Add('required', Required);
end;

function CreateTextContent(const Text: String): TJsonObject;
begin
  Result := TJsonObject.Create;
  Result.Add('type', 'text');
  Result.Add('text', Text);
end;

{ TMockProjectProvider }

constructor TMockProjectProvider.Create(const AProjectName, AProjectCaption, AProjectPath: String);
begin
  inherited Create;
  FProjectName := AProjectName;
  FProjectCaption := AProjectCaption;
  FProjectPath := AProjectPath;
end;

function TMockProjectProvider.GetProjectName: String;
begin
  Result := FProjectName;
end;

function TMockProjectProvider.GetProjectCaption: String;
begin
  Result := FProjectCaption;
end;

function TMockProjectProvider.GetProjectPath: String;
begin
  Result := FProjectPath;
end;

function TMockProjectProvider.GetPascalFiles: TStringList;
begin
  Result := TStringList.Create;
  Result.Add('code/gameviewmain.pas');
  Result.Add('code/gameinitialize.pas');
  Result.Add('my_game_standalone.dpr');
end;

function TMockProjectProvider.GetDataFiles: TStringList;
begin
  Result := TStringList.Create;
  Result.Add('data/gameviewmain.castle-user-interface');
  Result.Add('data/images/logo.png');
  Result.Add('data/sounds/click.wav');
  Result.Add('data/models/character.gltf');
end;

{ TMockDesignProvider }

constructor TMockDesignProvider.Create(ADesignOpen: Boolean);
begin
  inherited Create;
  FDesignOpen := ADesignOpen;
end;

function TMockDesignProvider.IsDesignOpen: Boolean;
begin
  Result := FDesignOpen;
end;

function TMockDesignProvider.GetComponentHierarchy: TJsonObject;
var
  Root, Child1, Child2: TJsonObject;
  Children: TJsonArray;
begin
  Result := TJsonObject.Create;
  Result.Add('name', 'ViewMain');
  Result.Add('class', 'TCastleView');
  Result.Add('path', 'ViewMain');

  Children := TJsonArray.Create;

  Child1 := TJsonObject.Create;
  Child1.Add('name', 'LabelTitle');
  Child1.Add('class', 'TCastleLabel');
  Child1.Add('path', 'ViewMain.LabelTitle');
  Children.Add(Child1);

  Child2 := TJsonObject.Create;
  Child2.Add('name', 'ButtonStart');
  Child2.Add('class', 'TCastleButton');
  Child2.Add('path', 'ViewMain.ButtonStart');
  Children.Add(Child2);

  Result.Add('children', Children);
end;

function TMockDesignProvider.GetComponentProperty(const ComponentPath, PropertyName: String): String;
begin
  // Mock implementation
  if (ComponentPath = 'ViewMain.LabelTitle') and (PropertyName = 'Caption') then
    Result := 'My Game Title'
  else if (ComponentPath = 'ViewMain.ButtonStart') and (PropertyName = 'Caption') then
    Result := 'Start Game'
  else
    Result := 'MockValue';
end;

function TMockDesignProvider.SetComponentProperty(const ComponentPath, PropertyName, Value: String): Boolean;
begin
  // Mock implementation - always succeeds
  WritelnLog('Mock: Setting %s.%s = %s', [ComponentPath, PropertyName, Value]);
  Result := True;
end;

function TMockDesignProvider.GetDesignScreenshot: String;
begin
  // Return a mock base64-encoded PNG data
  // This is a minimal 1x1 transparent PNG in base64
  Result := 'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChAI9jU77zgAAAABJRU5ErkJggg==';
end;

{ TJsonRpcMessage }

constructor TJsonRpcMessage.Create;
begin
  inherited Create;
  Id := Null;
  Params := nil;
  Result := nil;
  Error := nil;
end;

destructor TJsonRpcMessage.Destroy;
begin
  FreeAndNil(Params);
  FreeAndNil(Result);
  FreeAndNil(Error);
  inherited Destroy;
end;

class function TJsonRpcMessage.FromJson(const JsonStr: String): TJsonRpcMessage;
var
  JsonData: TJsonData;
  JsonObj: TJsonObject;
begin
  Result := TJsonRpcMessage.Create;
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
        if JsonObj.IndexOfName('id') >= 0 then
        begin
          Result.MessageType := jrmtRequest;
          Result.Id := JsonObj.Get('id');
        end else
        begin
          Result.MessageType := jrmtNotification;
        end;

        if JsonObj.IndexOfName('params') >= 0 then
          Result.Params := JsonObj.Extract('params');
      end
      else if JsonObj.IndexOfName('result') >= 0 then
      begin
        Result.MessageType := jrmtResponse;
        Result.Id := JsonObj.Get('id');
        Result.Result := JsonObj.Extract('result');
      end
      else if JsonObj.IndexOfName('error') >= 0 then
      begin
        Result.MessageType := jrmtError;
        Result.Id := JsonObj.Get('id');
        Result.Error := TJsonObject(JsonObj.Extract('error'));
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

function TJsonRpcMessage.ToJson: String;
var
  JsonObj: TJsonObject;
begin
  JsonObj := TJsonObject.Create;
  try
    JsonObj.Add('jsonrpc', '2.0');

    case MessageType of
      jrmtRequest:
        begin
          if VarIsNull(Id) then
            JsonObj.Add('id', TJsonNull.Create)
          else if VarIsStr(Id) then
            JsonObj.Add('id', VarToStr(Id))
          else
            JsonObj.Add('id', Integer(Id));
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
          if VarIsNull(Id) then
            JsonObj.Add('id', TJsonNull.Create)
          else if VarIsStr(Id) then
            JsonObj.Add('id', VarToStr(Id))
          else
            JsonObj.Add('id', Integer(Id));
          if Assigned(Self.Result) then
            JsonObj.Add('result', Self.Result.Clone)
          else
            JsonObj.Add('result', TJsonNull.Create);
        end;
      jrmtError:
        begin
          if VarIsNull(Id) then
            JsonObj.Add('id', TJsonNull.Create)
          else if VarIsStr(Id) then
            JsonObj.Add('id', VarToStr(Id))
          else
            JsonObj.Add('id', Integer(Id));
          if Assigned(Error) then
            JsonObj.Add('error', Error.Clone);
        end;
    end;

    Result := JsonObj.AsJSON;
  finally
    FreeAndNil(JsonObj);
  end;
end;

class function TJsonRpcMessage.CreateRequest(const AId: Variant; const AMethod: String; AParams: TJsonData): TJsonRpcMessage;
begin
  Result := TJsonRpcMessage.Create;
  Result.MessageType := jrmtRequest;
  Result.Id := AId;
  Result.Method := AMethod;
  Result.Params := AParams;
end;

class function TJsonRpcMessage.CreateResponse(const AId: Variant; AResult: TJsonData): TJsonRpcMessage;
begin
  Result := TJsonRpcMessage.Create;
  Result.MessageType := jrmtResponse;
  Result.Id := AId;
  Result.Result := AResult;
end;

class function TJsonRpcMessage.CreateError(const AId: Variant; ACode: Integer; const AMessage: String; AData: TJsonData): TJsonRpcMessage;
begin
  Result := TJsonRpcMessage.Create;
  Result.MessageType := jrmtError;
  Result.Id := AId;
  Result.Error := TJsonObject.Create;
  Result.Error.Add('code', ACode);
  Result.Error.Add('message', AMessage);
  if Assigned(AData) then
    Result.Error.Add('data', AData);
end;

class function TJsonRpcMessage.CreateNotification(const AMethod: String; AParams: TJsonData): TJsonRpcMessage;
begin
  Result := TJsonRpcMessage.Create;
  Result.MessageType := jrmtNotification;
  Result.Method := AMethod;
  Result.Params := AParams;
end;

{ TMcpServer }

constructor TMcpServer.Create(AProjectProvider: IMcpProjectProvider; ADesignProvider: IMcpDesignProvider);
begin
  inherited Create;
  FProjectProvider := AProjectProvider;
  FDesignProvider := ADesignProvider;
  FInitialized := False;

  // Set default capabilities
  FServerCapabilities.Resources := True;
  FServerCapabilities.Tools := True;
  FServerCapabilities.Prompts := True;

  FServerInfo := CreateServerInfo;
end;

destructor TMcpServer.Destroy;
begin
  FreeAndNil(FServerInfo);
  inherited Destroy;
end;

function TMcpServer.CreateServerInfo: TJsonObject;
begin
  Result := TJsonObject.Create;
  Result.Add('name', 'castle-engine-editor');
  Result.Add('version', '1.0.0');
  Result.Add('description', 'Castle Game Engine Editor MCP Server');
end;

function TMcpServer.CreateServerCapabilities: TJsonObject;
begin
  Result := TJsonObject.Create;
  if FServerCapabilities.Resources then
    Result.Add('resources', TJsonObject.Create);
  if FServerCapabilities.Tools then
    Result.Add('tools', TJsonObject.Create);
  if FServerCapabilities.Prompts then
    Result.Add('prompts', TJsonObject.Create);
end;

function TMcpServer.ProcessMessage(const JsonStr: String): String;
var
  Request, Response: TJsonRpcMessage;
  ResponseData: TJsonData;
begin
  Result := '';
  Request := nil;
  Response := nil;
  try
    try
      Request := TJsonRpcMessage.FromJson(JsonStr);

      // Handle different message types
      case Request.MessageType of
        jrmtRequest:
          begin
            ResponseData := nil;
            try
              // Route to appropriate handler
              if Request.Method = 'initialize' then
                ResponseData := HandleInitialize(Request.Params)
              else if Request.Method = 'initialized' then
                ResponseData := HandleInitialized(Request.Params)
              else if Request.Method = 'resources/list' then
                ResponseData := HandleResourcesList(Request.Params)
              else if Request.Method = 'resources/read' then
                ResponseData := HandleResourcesRead(Request.Params)
              else if Request.Method = 'tools/list' then
                ResponseData := HandleToolsList(Request.Params)
              else if Request.Method = 'tools/call' then
                ResponseData := HandleToolsCall(Request.Params)
              else if Request.Method = 'prompts/list' then
                ResponseData := HandlePromptsList(Request.Params)
              else if Request.Method = 'prompts/get' then
                ResponseData := HandlePromptsGet(Request.Params)
              else
                raise Exception.CreateFmt('Unknown method: %s', [Request.Method]);

              Response := TJsonRpcMessage.CreateResponse(Request.Id, ResponseData);
            except
              on E: Exception do
              begin
                FreeAndNil(ResponseData);
                Response := TJsonRpcMessage.CreateError(Request.Id, -32603, E.Message);
              end;
            end;
          end;
        jrmtNotification:
          begin
            // Handle notifications (no response needed)
            if Request.Method = 'initialized' then
              HandleInitialized(Request.Params);
            // For notifications, we don't send a response
            Exit;
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
        Response := TJsonRpcMessage.CreateError(Null, -32700, 'Parse error: ' + E.Message);
        Result := Response.ToJson;
      end;
    end;
  finally
    FreeAndNil(Request);
    FreeAndNil(Response);
  end;
end;

function TMcpServer.HandleInitialize(const Params: TJsonData): TJsonData;
var
  ParamsObj: TJsonObject;
  ClientInfo: TJsonObject;
  Capabilities: TJsonObject;
begin
  if not (Params is TJsonObject) then
    raise Exception.Create('initialize params must be an object');

  ParamsObj := TJsonObject(Params);

  // Extract client capabilities
  if ParamsObj.IndexOfName('capabilities') >= 0 then
  begin
    Capabilities := ParamsObj.Objects['capabilities'];
    if Assigned(Capabilities) then
    begin
      FClientCapabilities.Roots := Capabilities.IndexOfName('roots') >= 0;
      FClientCapabilities.Sampling := Capabilities.IndexOfName('sampling') >= 0;
      FClientCapabilities.Elicitation := Capabilities.IndexOfName('elicitation') >= 0;
    end;
  end;

  // Create response
  Result := TJsonObject.Create;
  TJsonObject(Result).Add('protocolVersion', '2025-06-18');
  TJsonObject(Result).Add('serverInfo', FServerInfo.Clone);
  TJsonObject(Result).Add('capabilities', CreateServerCapabilities);
end;

function TMcpServer.HandleInitialized(const Params: TJsonData): TJsonData;
begin
  FInitialized := True;
  WritelnLog('MCP Server initialized');
  Result := nil; // No response for initialized notification
end;

function TMcpServer.GetProjectResources: TJsonArray;
var
  Resource: TJsonObject;
  Files: TStringList;
  I: Integer;
begin
  Result := TJsonArray.Create;

  if not Assigned(FProjectProvider) then
    Exit;

  // Add project info resource
  Resource := TJsonObject.Create;
  Resource.Add('uri', 'project://info');
  Resource.Add('name', 'Project Information');
  Resource.Add('description', 'Basic project information including name and caption');
  Resource.Add('mimeType', 'application/json');
  Result.Add(Resource);

  // Add Pascal files
  Files := FProjectProvider.GetPascalFiles;
  try
    for I := 0 to Files.Count - 1 do
    begin
      Resource := TJsonObject.Create;
      Resource.Add('uri', 'project://pascal/' + Files[I]);
      Resource.Add('name', 'Pascal File: ' + ExtractFileName(Files[I]));
      Resource.Add('description', 'Pascal source file');
      Resource.Add('mimeType', 'text/x-pascal');
      Result.Add(Resource);
    end;
  finally
    FreeAndNil(Files);
  end;

  // Add data files
  Files := FProjectProvider.GetDataFiles;
  try
    for I := 0 to Files.Count - 1 do
    begin
      Resource := TJsonObject.Create;
      Resource.Add('uri', 'project://data/' + Files[I]);
      Resource.Add('name', 'Data File: ' + ExtractFileName(Files[I]));
      Resource.Add('description', 'Project data file');
      Resource.Add('mimeType', 'application/octet-stream');
      Result.Add(Resource);
    end;
  finally
    FreeAndNil(Files);
  end;
end;

function TMcpServer.GetDesignResources: TJsonArray;
var
  Resource: TJsonObject;
begin
  Result := TJsonArray.Create;

  if not Assigned(FDesignProvider) or not FDesignProvider.IsDesignOpen then
    Exit;

  // Add component hierarchy resource
  Resource := TJsonObject.Create;
  Resource.Add('uri', 'design://hierarchy');
  Resource.Add('name', 'Component Hierarchy');
  Resource.Add('description', 'Current design component hierarchy');
  Resource.Add('mimeType', 'application/json');
  Result.Add(Resource);

  // Add screenshot resource
  Resource := TJsonObject.Create;
  Resource.Add('uri', 'design://screenshot');
  Resource.Add('name', 'Design Screenshot');
  Resource.Add('description', 'Current design viewport screenshot');
  Resource.Add('mimeType', 'image/png');
  Result.Add(Resource);
end;

function TMcpServer.HandleResourcesList(const Params: TJsonData): TJsonData;
var
  Resources: TJsonArray;
  ProjectResources, DesignResources: TJsonArray;
  I: Integer;
begin
  Resources := TJsonArray.Create;

  // Add project resources
  ProjectResources := GetProjectResources;
  try
    for I := 0 to ProjectResources.Count - 1 do
      Resources.Add(ProjectResources.Items[I].Clone);
  finally
    FreeAndNil(ProjectResources);
  end;

  // Add design resources if available
  DesignResources := GetDesignResources;
  try
    for I := 0 to DesignResources.Count - 1 do
      Resources.Add(DesignResources.Items[I].Clone);
  finally
    FreeAndNil(DesignResources);
  end;

  Result := TJsonObject.Create;
  TJsonObject(Result).Add('resources', Resources);
end;

function TMcpServer.HandleResourcesRead(const Params: TJsonData): TJsonData;
var
  ParamsObj: TJsonObject;
  Uri: String;
  Content: String;
  ProjectInfo: TJsonObject;
  Hierarchy: TJsonObject;
  Base64Data: String;
  ContentArray: TJsonArray;
  ContentItem: TJsonObject;
  ResourceObj: TJsonObject;
begin
  if not (Params is TJsonObject) then
    raise Exception.Create('resources/read params must be an object');

  ParamsObj := TJsonObject(Params);
  Uri := ParamsObj.Get('uri', '');

  if Uri = '' then
    raise Exception.Create('uri parameter is required');

  Result := TJsonObject.Create;

  if Uri = 'project://info' then
  begin
    if not Assigned(FProjectProvider) then
      raise Exception.Create('No project provider available');

    ProjectInfo := TJsonObject.Create;
    ProjectInfo.Add('name', FProjectProvider.GetProjectName);
    ProjectInfo.Add('caption', FProjectProvider.GetProjectCaption);
    ProjectInfo.Add('path', FProjectProvider.GetProjectPath);

    ContentArray := TJsonArray.Create;
    ContentItem := TJsonObject.Create;
    ContentItem.Add('type', 'text');
    ContentItem.Add('text', ProjectInfo.AsJSON);
    ContentArray.Add(ContentItem);
    TJsonObject(Result).Add('contents', ContentArray);
    FreeAndNil(ProjectInfo);
  end
  else if Uri = 'design://hierarchy' then
  begin
    if not Assigned(FDesignProvider) or not FDesignProvider.IsDesignOpen then
      raise Exception.Create('No design is currently open');

    Hierarchy := FDesignProvider.GetComponentHierarchy;
    try
      ContentArray := TJsonArray.Create;
      ContentItem := TJsonObject.Create;
      ContentItem.Add('type', 'text');
      ContentItem.Add('text', Hierarchy.AsJSON);
      ContentArray.Add(ContentItem);
      TJsonObject(Result).Add('contents', ContentArray);
    finally
      FreeAndNil(Hierarchy);
    end;
  end
  else if Uri = 'design://screenshot' then
  begin
    if not Assigned(FDesignProvider) or not FDesignProvider.IsDesignOpen then
      raise Exception.Create('No design is currently open');

    Base64Data := FDesignProvider.GetDesignScreenshot;
    ContentArray := TJsonArray.Create;
    ContentItem := TJsonObject.Create;
    ContentItem.Add('type', 'resource');
    ResourceObj := TJsonObject.Create;
    ResourceObj.Add('uri', 'data:image/png;base64,' + Base64Data);
    ContentItem.Add('resource', ResourceObj);
    ContentArray.Add(ContentItem);
    TJsonObject(Result).Add('contents', ContentArray);
  end
  else
    raise Exception.CreateFmt('Unknown resource URI: %s', [Uri]);
end;

function TMcpServer.GetAvailableTools: TJsonArray;
var
  Tool: TJsonObject;
begin
  Result := TJsonArray.Create;

  // Get component property tool
  Tool := TJsonObject.Create;
  Tool.Add('name', 'get_component_property');
  Tool.Add('description', 'Get the value of a component property');
  Tool.Add('inputSchema', CreateInputSchema(
    CreatePropertiesObject(
      'Path to the component (e.g., "ViewMain.ButtonStart")',
      'Name of the property to get'
    ),
    CreateJsonArray(['componentPath', 'propertyName'])
  ));
  Result.Add(Tool);

  // Set component property tool
  Tool := TJsonObject.Create;
  Tool.Add('name', 'set_component_property');
  Tool.Add('description', 'Set the value of a component property');
  Tool.Add('inputSchema', CreateInputSchema(
    CreatePropertiesObject(
      'Path to the component (e.g., "ViewMain.ButtonStart")',
      'Name of the property to set',
      'New value for the property'
    ),
    CreateJsonArray(['componentPath', 'propertyName', 'value'])
  ));
  Result.Add(Tool);
end;

function TMcpServer.HandleToolsList(const Params: TJsonData): TJsonData;
var
  Tools: TJsonArray;
begin
  Tools := GetAvailableTools;
  Result := TJsonObject.Create;
  TJsonObject(Result).Add('tools', Tools);
end;

function TMcpServer.HandleToolsCall(const Params: TJsonData): TJsonData;
var
  ParamsObj: TJsonObject;
  ToolName: String;
  Arguments: TJsonObject;
  ComponentPath, PropertyName, Value: String;
  PropertyValue: String;
  Success: Boolean;
begin
  if not (Params is TJsonObject) then
    raise Exception.Create('tools/call params must be an object');

  ParamsObj := TJsonObject(Params);
  if ParamsObj.IndexOfName('name') >= 0 then
    ToolName := ParamsObj.Strings['name']
  else
    ToolName := '';

  if ParamsObj.IndexOfName('arguments') >= 0 then
    Arguments := ParamsObj.Objects['arguments']
  else
    Arguments := nil;

  Result := TJsonObject.Create;

  if ToolName = 'get_component_property' then
  begin
    if not Assigned(Arguments) then
      raise Exception.Create('arguments required for get_component_property');

    if Arguments.IndexOfName('componentPath') >= 0 then
      ComponentPath := Arguments.Strings['componentPath']
    else
      ComponentPath := '';
    if Arguments.IndexOfName('propertyName') >= 0 then
      PropertyName := Arguments.Strings['propertyName']
    else
      PropertyName := '';

    if (ComponentPath = '') or (PropertyName = '') then
      raise Exception.Create('componentPath and propertyName are required');

    if not Assigned(FDesignProvider) or not FDesignProvider.IsDesignOpen then
      raise Exception.Create('No design is currently open');

    PropertyValue := FDesignProvider.GetComponentProperty(ComponentPath, PropertyName);
    TJsonObject(Result).Add('content', CreateJsonArrayWithTextContent(PropertyValue));
  end
  else if ToolName = 'set_component_property' then
  begin
    if not Assigned(Arguments) then
      raise Exception.Create('arguments required for set_component_property');

    if Arguments.IndexOfName('componentPath') >= 0 then
      ComponentPath := Arguments.Strings['componentPath']
    else
      ComponentPath := '';
    if Arguments.IndexOfName('propertyName') >= 0 then
      PropertyName := Arguments.Strings['propertyName']
    else
      PropertyName := '';
    if Arguments.IndexOfName('value') >= 0 then
      Value := Arguments.Strings['value']
    else
      Value := '';

    if (ComponentPath = '') or (PropertyName = '') then
      raise Exception.Create('componentPath and propertyName are required');

    if not Assigned(FDesignProvider) or not FDesignProvider.IsDesignOpen then
      raise Exception.Create('No design is currently open');

    Success := FDesignProvider.SetComponentProperty(ComponentPath, PropertyName, Value);
    TJsonObject(Result).Add('content', CreateJsonArrayWithTextContent(Format('Property %s.%s set to "%s": %s', [ComponentPath, PropertyName, Value, BoolToStr(Success, True)])));
  end
  else
    raise Exception.CreateFmt('Unknown tool: %s', [ToolName]);
end;

function TMcpServer.GetAvailablePrompts: TJsonArray;
var
  Prompt: TJsonObject;
begin
  Result := TJsonArray.Create;

  // Project overview prompt
  Prompt := TJsonObject.Create;
  Prompt.Add('name', 'project_overview');
  Prompt.Add('description', 'Get an overview of the current project structure and files');
  Result.Add(Prompt);

  // Component analysis prompt
  Prompt := TJsonObject.Create;
  Prompt.Add('name', 'component_analysis');
  Prompt.Add('description', 'Analyze the component hierarchy of the current design');
  Prompt.Add('arguments', CreateJsonArrayWithPromptArgument('focus', 'Specific component to focus on', False));
  Result.Add(Prompt);
end;

function TMcpServer.HandlePromptsList(const Params: TJsonData): TJsonData;
var
  Prompts: TJsonArray;
begin
  Prompts := GetAvailablePrompts;
  Result := TJsonObject.Create;
  TJsonObject(Result).Add('prompts', Prompts);
end;

function TMcpServer.HandlePromptsGet(const Params: TJsonData): TJsonData;
var
  ParamsObj: TJsonObject;
  PromptName: String;
  Messages: TJsonArray;
  Message: TJsonObject;
  ProjectInfo: String;
  Hierarchy: TJsonObject;
begin
  if not (Params is TJsonObject) then
    raise Exception.Create('prompts/get params must be an object');

  ParamsObj := TJsonObject(Params);
  PromptName := ParamsObj.Get('name', '');

  Messages := TJsonArray.Create;
  Result := TJsonObject.Create;

  if PromptName = 'project_overview' then
  begin
    if Assigned(FProjectProvider) then
    begin
      ProjectInfo := Format('Project: %s (%s)' + LineEnding + 'Path: %s', [
        FProjectProvider.GetProjectName,
        FProjectProvider.GetProjectCaption,
        FProjectProvider.GetProjectPath
      ]);

      Message := TJsonObject.Create;
      Message.Add('role', 'user');
      Message.Add('content', CreateTextContent('Please analyze this Castle Game Engine project:' + LineEnding + ProjectInfo));
      Messages.Add(Message);
    end;
  end
  else if PromptName = 'component_analysis' then
  begin
    if Assigned(FDesignProvider) and FDesignProvider.IsDesignOpen then
    begin
      Hierarchy := FDesignProvider.GetComponentHierarchy;
      try
        Message := TJsonObject.Create;
        Message.Add('role', 'user');
        Message.Add('content', CreateTextContent('Please analyze this component hierarchy:' + LineEnding + Hierarchy.AsJSON));
        Messages.Add(Message);
      finally
        FreeAndNil(Hierarchy);
      end;
    end;
  end
  else
    raise Exception.CreateFmt('Unknown prompt: %s', [PromptName]);

  TJsonObject(Result).Add('messages', Messages);
end;

procedure TMcpServer.RunStdio;
var
  InputLine, Response: String;
begin
  WritelnLog('MCP Server starting in stdio mode');

  while not EOF do
  begin
    ReadLn(InputLine);
    if InputLine <> '' then
    begin
      Response := ProcessMessage(InputLine);
      if Response <> '' then
        WriteLn(Response);
    end;
  end;

  WritelnLog('MCP Server shutting down');
end;

function CreateMockMcpServer: TMcpServer;
var
  ProjectProvider: TMockProjectProvider;
  DesignProvider: TMockDesignProvider;
begin
  ProjectProvider := TMockProjectProvider.Create('TestProject', 'Test Project Caption', '/path/to/test/project/');
  DesignProvider := TMockDesignProvider.Create(True);
  Result := TMcpServer.Create(ProjectProvider, DesignProvider);
end;

end.
