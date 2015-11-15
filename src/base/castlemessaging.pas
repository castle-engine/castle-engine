{
  Copyright 2015-2015 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Message system to communicate between native code and Java on Android
  (TMessaging). }
unit CastleMessaging;

interface

uses {$ifdef ANDROID} JNI, {$endif} SyncObjs,
  CastleStringUtils, CastleGenericLists;

type
  { Called by TMessaging when a new message from Java is received.
    Returns if the message was handled (this does @bold(not) block
    the message from being passed to other callbacks, it only means
    we will not report a warning about unhandled message). }
  TMessageReceivedEvent = function (const Received: TCastleStringList): boolean of object;

  { Used by TMessaging to manage a list of listeners. }
  TMessageReceivedEventList = class(specialize TGenericStructList<TMessageReceivedEvent>)
  public
    procedure ExecuteAll(const Received: TCastleStringList);
  end;

  { Message system to communicate between native code (Pascal) and Java
    on Android. Use through auto-created @link(Messaging) singleton.
    On other platforms than Android, right now it simply does nothing
    --- messsages are not send anywhere.

    This is used automatically by various engine classes like
    @link(TGooglePlayGames), @link(TAds), @link(TAnalytics), @link(TInAppPurchases).
    User code typically only needs to call @code(Messaging.Update) continously,
    for example from @link(TCastleWindowCustom.OnUpdate) or
    @link(TCastleControlCustom.OnUpdate). }
  TMessaging = class
  private
    ToJava: TCastleStringList;
    FromJava: TCastleStringList;
    FOnReceive: TMessageReceivedEventList;
    FLog: boolean;
    procedure SendStr(const S: string);
    function ReceiveStr: string;
    { Receive next message from Java. @nil if none. }
    function Receive: TCastleStringList;
  public
    constructor Create;
    destructor Destroy; override;

    { Send a message to our Java integration code. }
    procedure Send(const Strings: array of string);
    { Callbacks called when new message from Java is received. }
    property OnReceive: TMessageReceivedEventList read FOnReceive;

    { Call this constantly to process messages from Java.
      For example call it in @link(TCastleWindow.OnUpdate) or
      @link(TCastleControl.OnUpdate). }
    procedure Update;

    { Log each message send/received from/to Java.
      Note that this is sometimes quite verbose, and it also allows cheaters
      to easier debug what happens in your game (e.g. how to fake getting
      some achievement), so in general don't leave it "on" in production. }
    property Log: boolean read FLog write FLog default false;
  end;

{$ifdef ANDROID}
{ Export this function from your Android library. }
function Java_net_sourceforge_castleengine_MainActivity_jniMessage(
  Env: PJNIEnv; This: jobject; JavaToNative: jstring): jstring; cdecl;
{$endif}

{ Auto-created single instance of @link(TMessaging) to communicate
  between native code and Java on Android. }
function Messaging: TMessaging;

implementation

uses SysUtils,
  CastleUtils, CastleLog, CastleWarnings;

var
  JavaCommunicationCS: TCriticalSection;

{ TMessageReceivedEventList -------------------------------------------------- }

procedure TMessageReceivedEventList.ExecuteAll(const Received: TCastleStringList);
var
  I: Integer;
  Handled: boolean;
begin
  Handled := false;
  for I := 0 to Count - 1 do
    Handled := Items[I](Received) or Handled;
  if not Handled then
    OnWarning(wtMajor, 'JNI', 'Unhandled message from Java:' + NL + Received.Text);
end;

{ TMessaging ----------------------------------------------------------------- }

constructor TMessaging.Create;
begin
  inherited;
  ToJava := TCastleStringList.Create;
  FromJava := TCastleStringList.Create;
  FOnReceive := TMessageReceivedEventList.Create;
end;

destructor TMessaging.Destroy;
begin
  FreeAndNil(ToJava);
  FreeAndNil(FromJava);
  FreeAndNil(FOnReceive);
  inherited;
end;

const
  MessageDelimiter = '=';

procedure TMessaging.SendStr(const S: string);
begin
  { secure in case this is called from state Finish when things are finalized }
  if Self = nil then Exit;
  JavaCommunicationCS.Acquire;
  try
    if CastleLog.Log and Log then
      WritelnLog('JNI', 'Native code posting message to Java: ' + S);
    ToJava.Add(S);
  finally JavaCommunicationCS.Release end;
end;

procedure TMessaging.Send(const Strings: array of string);
var
  I: Integer;
  S: string;
begin
  if High(Strings) = -1 then Exit; // exit in case of empty list
  S := Strings[0];
  for I := 1 to High(Strings) do
    S += MessageDelimiter + Strings[I];
  SendStr(S);
end;

function TMessaging.ReceiveStr: string;
begin
  JavaCommunicationCS.Acquire;
  try
    if FromJava.Count <> 0 then
    begin
      Result := FromJava[0];
      if CastleLog.Log and Log then
        WritelnLog('JNI', 'Native code received a message from Java: ' + Result);
      FromJava.Delete(0);
    end else
      Result := '';
  finally JavaCommunicationCS.Release end;
end;

function TMessaging.Receive: TCastleStringList;
var
  S: string;
begin
  S := ReceiveStr;
  if S <> '' then
    Result := CreateTokens(S, [MessageDelimiter]) else
    Result := nil;
end;

procedure TMessaging.Update;
var
  Received: TCastleStringList;
begin
  Received := Receive;
  while Received <> nil do
  begin
    try
      OnReceive.ExecuteAll(Received);
    finally FreeAndNil(Received) end;
    Received := Receive;
  end;
end;

{ globals -------------------------------------------------------------------- }

var
  FMessaging: TMessaging;
  FinalizationDone: boolean;

procedure DoInitialization;
begin
  if (not FinalizationDone) and (FMessaging = nil) then
  begin
    JavaCommunicationCS := TCriticalSection.Create;
    FMessaging := TMessaging.Create;
  end;
end;

function Messaging: TMessaging;
begin
  { in case you access Messaging before our unit "initialization" works
    (for example, in case your unit "initiazalition" is executed first and
    it does does some Messaging.Send), then initialize us now. }
  DoInitialization;

  Result := FMessaging;
end;

{$ifdef ANDROID}
function Java_net_sourceforge_castleengine_MainActivity_jniMessage(
  Env: PJNIEnv; This: jobject; JavaToNative: jstring): jstring; cdecl;
var
  JavaToNativeStr: PChar;
  Dummy: JBoolean;
begin
  JavaCommunicationCS.Acquire;
  try
    { this may be called from different thread, secure from being called
      in weird state }
    if (FMessaging <> nil) and
       (FMessaging.ToJava <> nil) and
       (FMessaging.FromJava <> nil) then
    begin
      if FMessaging.ToJava.Count <> 0 then
      begin
        Result := Env^^.NewStringUTF(Env, PChar(FMessaging.ToJava[0]));
        FMessaging.ToJava.Delete(0);
      end else
        Result := Env^^.NewStringUTF(Env, nil);

      if (JavaToNative <> nil) and
         (Env^^.GetStringUTFLength(Env, JavaToNative) <> 0) then
      begin
        Dummy := 0;
        JavaToNativeStr := Env^^.GetStringUTFChars(Env, JavaToNative,
          {$ifdef VER2} Dummy {$else} @Dummy {$endif});
        try
          FMessaging.FromJava.Add(AnsiString(JavaToNativeStr)); // will copy characters
        finally Env^^.ReleaseStringUTFChars(Env, JavaToNative, JavaToNativeStr) end;
      end;
    end;
  finally JavaCommunicationCS.Release end;
end;
{$endif}

initialization
  DoInitialization;
finalization
  FinalizationDone := true;
  FreeAndNil(FMessaging);
  FreeAndNil(JavaCommunicationCS);
end.
