{
  Copyright 2024-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the files COPYING*,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main view, where most of the application logic takes place. }
unit GameViewMain;

interface

uses Classes,
  { Enable https downloads. }
  {$ifdef FPC} OpenSslSockets, {$endif}
  FpJson, JsonParser,
  CastleVectors, CastleComponentSerialize, CastleDownload,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleTimeUtils;

type
  TRequestSuccessEvent = procedure(const Response: TJsonData) of object;

  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    ButtonSend: TCastleButton;
    EditQuery: TCastleEdit;
    LabelAnswer: TCastleLabel;
  private
    { Call OpenAiHttpRequest to make HTTP request to OpenAi API.
      It initializes CurrentRequest and OnRequestSuccess.

      When the HTTP request has finished with success, we
      - clear CurrentRequest
      - call OnCurrentRequestSuccess.
        From OnCurrentRequestSuccess, you can reliably call again
        OpenAiHttpRequest to start another HTTP request.

      When the request has finished with error,
      we show it to the user and clear CurrentRequest. }
    CurrentRequest: TCastleDownload;
    OnCurrentRequestSuccess: TRequestSuccessEvent;

    { Through a sequence of OpenAI API calls, we learn the ids of these things. }
    ThreadId, MessageId, RunId: String;

    { Used by WaitAndCall. }
    WaitSeconds: TFloatTime;
    WaitCallback: TNotifyEvent;

    { Make HTTP request to OpenAi API.
      Query is URL suffix, after https://api.openai.com/v1/ .
      OnCurrentRequestSuccess is called with returned parsed JSON. }
    procedure OpenAiHttpRequest(const Query: String;
      const InputContents: String; const HttpMethod: THttpMethod;
      const OnRequestSuccess: TRequestSuccessEvent);

    { Wait Seconds (fraction of a second) and then call Callback.

      Calling this again, cancels previously scheduled @link(WaitAndCall).

      Calling this with Callback = @nil also cancels previously
      scheduled @link(WaitAndCall) and makes nothing scheduled. }
    procedure WaitAndCall(const Seconds: TFloatTime; const Callback: TNotifyEvent);

    procedure ClickSend(Sender: TObject);

    { Making queries to OpenAI API, using OpenAiHttpRequest
      and processing the results. }
    procedure ThreadsResponse(const Response: TJsonData);
    procedure MessageResponse(const Response: TJsonData);
    procedure RunResponse(const Response: TJsonData);
    procedure RunStatusQuery(Sender: TObject);
    procedure RunStatusResponse(const Response: TJsonData);
    procedure FinalAnswerResponse(const Response: TJsonData);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleClassUtils, CastleStringUtils, CastleLog, CastleApplicationProperties;

const
  {$I openai_config.inc}

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;
  ButtonSend.OnClick := {$ifdef FPC}@{$endif} ClickSend;

  { TODO: Below is a temporary hacky way to make edit capture all keypresses.
    Good on desktops.
    Not good on Android, when this would actually conflict with on-screen keyboard
    that should automatically appear when you focus the edit. }
  if not ApplicationProperties.TouchDevice then
  begin
    Container.ForceCaptureInput := EditQuery;
  end;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);

  procedure ProcessRequestResponse;
  var
    JsonData: TJsonData;
    SuccessEvent: TRequestSuccessEvent;
  begin
    if (CurrentRequest <> nil) and
       (CurrentRequest.Status in [dsSuccess, dsError]) then
    begin
      if CurrentRequest.Status = dsSuccess then
      begin
        JsonData := GetJson(StreamToString(CurrentRequest.Contents));
        try
          { Free CurrentRequest and save to local variable
            OnCurrentRequestSuccess before calling OnCurrentRequestSuccess.
            Reason: OnCurrentRequestSuccess may call OpenAiHttpRequest again
            setting CurrentRequest and OnCurrentRequestSuccess. }
          SuccessEvent := OnCurrentRequestSuccess;
          FreeAndNil(CurrentRequest);
          OnCurrentRequestSuccess := nil;

          if Assigned(SuccessEvent) then
            SuccessEvent(JsonData);
        finally FreeAndNil(JsonData) end;
      end else
      begin
        LabelAnswer.Caption := Format('Error downloading from OpenAI: %s, contents: %s', [
          CurrentRequest.ErrorMessage,
          StreamToString(CurrentRequest.Contents)
        ]);
        FreeAndNil(CurrentRequest);
        OnCurrentRequestSuccess := nil;
      end;
    end;
  end;

  procedure ProcessWaitSeconds;
  begin
    if (WaitSeconds > 0) and Assigned(WaitCallback) then
    begin
      WaitSeconds := WaitSeconds - SecondsPassed;
      if WaitSeconds <= 0 then
      begin
        WaitCallback(Self);
        WaitCallback := nil;
      end;
    end;
  end;

begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
  ProcessRequestResponse;
  ProcessWaitSeconds;
end;

procedure TViewMain.WaitAndCall(const Seconds: TFloatTime; const Callback: TNotifyEvent);
begin
  WaitSeconds := Seconds;
  WaitCallback := Callback;
end;

procedure TViewMain.OpenAiHttpRequest(const Query: String;
  const InputContents: String; const HttpMethod: THttpMethod;
  const OnRequestSuccess: TRequestSuccessEvent);
begin
  if CurrentRequest <> nil then
  begin
    WritelnWarning('OpenAI', 'Previous request not finished yet, interrupting it');
    FreeAndNil(CurrentRequest);
    OnCurrentRequestSuccess := nil;
  end;

  CurrentRequest := TCastleDownload.Create(nil);
  CurrentRequest.HttpHeader('Authorization', 'Bearer ' + OpenAIApiKey);
  CurrentRequest.HttpHeader('Content-Type', 'application/json');
  CurrentRequest.HttpHeader('OpenAI-Beta', 'assistants=v2');
  CurrentRequest.Url := 'https://api.openai.com/v1/' + Query;
  CurrentRequest.HttpMethod := HttpMethod;
  WriteStr(CurrentRequest.HttpRequestBody, InputContents);
  CurrentRequest.Start;

  OnCurrentRequestSuccess := OnRequestSuccess;
end;

procedure TViewMain.ClickSend(Sender: TObject);
begin
  { Communicate using OpenAI REST API to get the answer to the question.
    See ../tests/test_openai.sh for the curl command that does this,
    with links to docs.
    Here, we just do this in Pascal, using TCastleDownload,
    in a general way. }

  // reset fields, we will fill them in the following requests
  ThreadId := '';
  MessageId := '';
  RunId := '';

  { prepare first request: create a thread }
  OpenAiHttpRequest('threads', '', hmPost,
    {$ifdef FPC}@{$endif} ThreadsResponse);

  LabelAnswer.Caption := 'Threads request send...';
end;

procedure TViewMain.ThreadsResponse(const Response: TJsonData);
var
  MessageRequest: TJsonObject;
  MessageRequestStr: String;
begin
  ThreadId := (Response as TJSONObject).Strings['id'];
  WritelnLog('OpenAI', 'Thread id: ' + ThreadId);
  if not IsPrefix('thread_', ThreadId) then
    raise Exception.Create('Unexpected thread id: ' + ThreadId);

  { prepare next request }
  MessageRequest := TJsonObject.Create;
  try
    MessageRequest.Strings['role'] := 'user';
    MessageRequest.Strings['content'] := EditQuery.Text;
    MessageRequestStr := MessageRequest.AsJSON;
  finally FreeAndNil(MessageRequest) end;

  OpenAiHttpRequest('threads/' + ThreadId + '/messages', MessageRequestStr, hmPost,
    {$ifdef FPC}@{$endif} MessageResponse);

  LabelAnswer.Caption := 'Messages request send...';
end;

procedure TViewMain.MessageResponse(const Response: TJsonData);
var
  RunRequest: TJsonObject;
  RunRequestStr: String;
begin
  MessageId := (Response as TJSONObject).Strings['id'];
  WritelnLog('OpenAI', 'Message id: ' + MessageId);
  if not IsPrefix('msg_', MessageId) then
    raise Exception.Create('Unexpected message id: ' + MessageId);

  { prepare next request }
  RunRequest := TJsonObject.Create;
  try
    RunRequest.Strings['assistant_id'] := OpenAiAssistantId;
    RunRequestStr := RunRequest.AsJSON;
  finally FreeAndNil(RunRequest) end;

  OpenAiHttpRequest('threads/' + ThreadId + '/runs', RunRequestStr, hmPost,
    {$ifdef FPC}@{$endif} RunResponse);

  LabelAnswer.Caption := 'Run request send...';
end;

procedure TViewMain.RunResponse(const Response: TJsonData);
begin
  RunId := (Response as TJSONObject).Strings['id'];
  WritelnLog('OpenAI', 'Run id: ' + RunId);
  if not IsPrefix('run_', RunId) then
    raise Exception.Create('Unexpected run id: ' + RunId);

  RunStatusQuery(Self);
end;

procedure TViewMain.RunStatusQuery(Sender: TObject);
begin
  { prepare next request: query run status, in a loop, until completed }
  OpenAiHttpRequest('threads/' + ThreadId + '/runs/' + RunId,'', hmGet,
    {$ifdef FPC}@{$endif} RunStatusResponse);

  LabelAnswer.Caption := 'Run status querying...';
end;

procedure TViewMain.RunStatusResponse(const Response: TJsonData);
var
  ResponseObj: TJsonObject;

  { Get error code and message from OpenAI API response.
    Returns @false if this is impossible for some reason,
    when JSON response doesn't specify an error. }
  function GetLastErrorFromResponse(const ResponseObj: TJsonObject;
    out ErrorCode, ErrorMessage: String): Boolean;
  var
    LastError: TJsonObject;
  begin
    if ResponseObj.Objects['last_error'] is TJsonObject then
    begin
      LastError := ResponseObj.Objects['last_error'] as TJsonObject;
      ErrorCode := LastError.Strings['code'];
      ErrorMessage := LastError.Strings['message'];
      Result := (ErrorCode <> '') or (ErrorMessage <> '');
    end else
      Result := false;
  end;

const
  { Delay before sending next query to ask for status.
    This avoids flooding the server with requests (OpenAI can likely handle
    the load anyway, still its nice thing to do, and for us querying
    too fast is not useful anyway -- ultimately we have to wait for the answer). }
  DelayBeforeNextQuery = 0.25;
var
  RunStatus: String;
  LastErrorCode, LastErrorMessage: String;
begin
  ResponseObj := Response as TJsonObject;
  RunStatus := ResponseObj.Strings['status'];
  WritelnLog('OpenAI', 'Run status: ' + RunStatus);

  if (RunStatus = 'queued') or
     (RunStatus = 'in_progress') then
  begin
    WaitAndCall(DelayBeforeNextQuery, {$ifdef FPC}@{$endif} RunStatusQuery);
  end else
  if RunStatus = 'completed' then
  begin
    { prepare next request: get final answer }
    OpenAiHttpRequest('threads/' + ThreadId + '/messages?limit=1', '', hmGet,
      {$ifdef FPC}@{$endif} FinalAnswerResponse);
  end else
  if RunStatus = 'failed' then
  begin
    WritelnWarning('OpenAI', 'Run failed: ' + ResponseObj.FormatJson);
    if GetLastErrorFromResponse(ResponseObj, LastErrorCode, LastErrorMessage) then
      raise Exception.CreateFmt('Run failed: %s, %s', [LastErrorCode, LastErrorMessage])
    else
      raise Exception.Create('Run failed. Consult log for JSON response');
  end else
  begin
    WritelnWarning('OpenAI', 'Unexpected run status: ' + ResponseObj.FormatJson);
    raise Exception.Create('Unexpected run status: ' + RunStatus + '. Consult log for full JSON response');
  end;
end;

procedure TViewMain.FinalAnswerResponse(const Response: TJsonData);
var
  FirstMessage, FirstMessageContent: TJsonObject;
  Answer: String;
begin
  WritelnLog('OpenAI', 'Messages list: ' + Response.FormatJson);
  FirstMessage := (Response as TJSONObject).Arrays['data'][0] as TJsonObject;
  FirstMessageContent := FirstMessage.Arrays['content'][0] as TJsonObject;
  Answer := FirstMessageContent.Objects['text'].Strings['value'];

  LabelAnswer.Caption := 'Answer: ' + Answer;
end;

end.
