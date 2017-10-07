{
  Copyright 2015-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Message system to communicate with services implemented in other languages
  (in Java on Android, or Objective-C on iOS)
  (TMessaging class). }
unit CastleMessaging;

{$I castleconf.inc}

interface

uses
  {$ifdef ANDROID} JNI, SyncObjs, {$endif}
  {$ifdef IOS} CTypes, {$endif}
  Generics.Collections,
  CastleStringUtils, CastleTimeUtils;

type
  { Called by TMessaging when a new message from service is received.
    Returns if the message was handled (this does @bold(not) block
    the message from being passed to other callbacks, it only means
    we will not report a warning about unhandled message). }
  TMessageReceivedEvent = function (const Received: TCastleStringList): boolean of object;

  { Used by TMessaging to manage a list of listeners. }
  TMessageReceivedEventList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TList<TMessageReceivedEvent>)
  public
    procedure ExecuteAll(const Received: TCastleStringList);
  end;

  { Message system to communicate between native code (Pascal) and other languages
    (Java on Android, Objective-C on iOS).
    Use through auto-created @link(Messaging) singleton.
    On platforms other than Android / iOS, it simply does nothing
    --- messsages are not send anywhere.

    To make this work, on Android you need to declare your Android project type
    as "integrated". See
    https://github.com/castle-engine/castle-engine/wiki/Android-Project-Components-Integrated-with-Castle-Game-Engine .
    For iOS, you don't need to do anything.
    You should also use the @code(game_units) attribute
    in @code(CastleEngineManifest.xml), so that the build tool automatically
    generates a proper Android / iOS library code, exporting the proper functions.

    On Android, all the communication is asynchronous -- Pascal code sends a message,
    and any answers from the Java code will come asynchronously later.
    On iOS, communication happens synchronously -- when Pascal sends the message,
    the relevant Objective-C code is executed immediately,
    and messages from Objective-C code execute appropriate Pascal code immediately.
    Take this into account, e.g. @link(TGooglePlayGames.OnSignedInChanged) callback
    @italic(may) be executed right inside the @link(TGooglePlayGames.Initialize)
    or @link(TGooglePlayGames.RequestSignedIn)
    call on iOS, if there's no network communication necessary to answer.

    This is used automatically by various engine classes like
    @link(TGooglePlayGames), @link(TAds), @link(TAnalytics), @link(TInAppPurchases). }
  TMessaging = class
  private
    {$ifdef ANDROID}
    JavaCommunicationCS: TCriticalSection;
    ToJava: TCastleStringList;
    FromJava: TCastleStringList;
    { Called constantly to process messages from Java. }
    procedure Update(Sender: TObject);
    {$endif}

    {$ifdef IOS}
    type
      TReceiveMessageFromPascalCallback = procedure (Message: PCChar); cdecl;
    class var
      FReceiveMessageFromPascalCallback: TReceiveMessageFromPascalCallback;
    {$endif}

    var
      FOnReceive: TMessageReceivedEventList;
      FLog: boolean;
    procedure SendStr(const Message: string);
    procedure ReceiveStr(const Message: string);
  public
    constructor Create;
    destructor Destroy; override;

    { Send a message to a service (implemented in other language,
      like Java on Android or Objective-C on iOS). }
    procedure Send(const Strings: array of string);

    { Callbacks called when new message from service is received. }
    property OnReceive: TMessageReceivedEventList read FOnReceive;

    { Log each message send/received from/to service.
      Note that this is sometimes quite verbose, and it also allows cheaters
      to easier debug what happens in your game (e.g. how to fake getting
      some achievement), so in general don't leave it "on" in production. }
    property Log: boolean read FLog write FLog default false;

    { Convert boolean to 'true' or 'false' string, which will be understood correctly
      by the service receiving the messages. }
    class function BoolToStr(const Value: boolean): string;

    { Convert float time (in seconds) to integer miliseconds, which are understood correctly
      by the service receiving the messages. }
    class function TimeToStr(const Value: TFloatTime): string;
  end;

{$ifdef ANDROID}
{ Export this function from your Android library. }
function Java_net_sourceforge_castleengine_MainActivity_jniMessage(
  Env: PJNIEnv; This: jobject; JavaToNative: jstring): jstring; cdecl;
{$endif}

{$ifdef IOS}
procedure CGEApp_SetReceiveMessageFromPascalCallback(
  ACallback: TMessaging.TReceiveMessageFromPascalCallback); cdecl;
procedure CGEApp_SendMessageToPascal(Message: PCChar); cdecl;
{$endif}

{ Auto-created single instance of @link(TMessaging) to communicate
  between native code (Pascal) and other languages (Java on Android, Objective-C on iOS). }
function Messaging: TMessaging;

implementation

uses SysUtils,
  CastleUtils, CastleLog, CastleApplicationProperties;

{ TMessageReceivedEventList -------------------------------------------------- }

procedure TMessageReceivedEventList.ExecuteAll(const Received: TCastleStringList);
var
  I: Integer;
  EventResult, Handled: boolean;
begin
  Handled := false;
  for I := 0 to Count - 1 do
  begin
    { Use EventResult to workaround FPC 3.1.1 bug (reproducible
      with FPC SVN revision 35460 (from 2016-02-20) on Linux x86_64).
      The bug has since been fixed http://bugs.freepascal.org/view.php?id=31421 ,
      so we may remove this workaround soon. }
    EventResult := Items[I](Received);
    Handled := EventResult or Handled;
  end;
  if not Handled then
    WritelnWarning('Messaging', 'Unhandled message from service:' + NL + Received.Text);
end;

{ TMessaging ----------------------------------------------------------------- }

constructor TMessaging.Create;
begin
  inherited;
  FOnReceive := TMessageReceivedEventList.Create;

  {$ifdef ANDROID}
  ToJava := TCastleStringList.Create;
  FromJava := TCastleStringList.Create;
  JavaCommunicationCS := TCriticalSection.Create;
  { Only Android needs the Update to communicate. }
  ApplicationProperties.OnUpdate.Add({$ifdef CASTLE_OBJFPC}@{$endif} Update);
  {$endif ANDROID}
end;

destructor TMessaging.Destroy;
begin
  {$ifdef ANDROID}
  if ApplicationProperties(false) <> nil then
    ApplicationProperties(false).OnUpdate.Remove({$ifdef CASTLE_OBJFPC}@{$endif} Update);
  FreeAndNil(JavaCommunicationCS);
  FreeAndNil(ToJava);
  FreeAndNil(FromJava);
  {$endif ANDROID}

  FreeAndNil(FOnReceive);
  inherited;
end;

const
  { This is a nice separator, as it has really low chance of occuring in non-binary data.
    - It's also not 0, so it will not be confused with "end of string" (Pascal is invulnerable
      to this, and can have #0 in the middle of AnsiString freely
      but I'm not so sure about Java or Objective-C NSString).
    - It's also within ASCII range, so it will not occur within any UTF-8 multibyte sequence
      (UTF-8 treats specially only stuff > 128, and you can search for ASCII substrings disregaring
      the UTF-8 multibyte stuff, as far as I know). }
  MessageDelimiter = #1;

procedure TMessaging.SendStr(const Message: string);
begin
  { secure in case this is called from state Finish when things are finalized }
  if Self = nil then Exit;

  if CastleLog.Log and Log then
    WritelnLog('Messaging', 'Pascal code sends a message to service: ' + SReadableForm(Message));

  {$ifdef ANDROID}
  JavaCommunicationCS.Acquire;
  try
    ToJava.Add(Message);
  finally JavaCommunicationCS.Release end;
  {$endif ANDROID}

  {$ifdef IOS}
  if Assigned(FReceiveMessageFromPascalCallback) then
    FReceiveMessageFromPascalCallback(PCChar(Message))
  else
    WritelnWarning('Messaging', 'Message cannot be delivered, iOS application not finished loading yet');
  {$endif IOS}
end;

procedure TMessaging.Send(const Strings: array of string);
begin
  if High(Strings) = -1 then Exit; // exit in case of empty list
  SendStr(GlueStrings(Strings, MessageDelimiter));
end;

procedure TMessaging.ReceiveStr(const Message: string);
var
  MessageAsList: TCastleStringList;
begin
  if CastleLog.Log and Log then
    WritelnLog('Messaging', 'Pascal code received a message from service: ' + SReadableForm(Message));
  if Message = '' then
    WritelnWarning('Messaging', 'Pascal code received an empty message');

  MessageAsList := SplitString(Message, MessageDelimiter);
  try
    OnReceive.ExecuteAll(MessageAsList);
  finally FreeAndNil(MessageAsList) end;
end;

{$ifdef ANDROID}
procedure TMessaging.Update(Sender: TObject);

  function GetNextMessageToPascal: string;
  begin
    JavaCommunicationCS.Acquire;
    try
      if FromJava.Count <> 0 then
      begin
        Result := FromJava[0];
        FromJava.Delete(0);
      end else
        Result := '';
    finally JavaCommunicationCS.Release end;
  end;

var
  Received: string;
begin
  Received := GetNextMessageToPascal;
  while Received <> '' do
    ReceiveStr(Received);
end;
{$endif ANDROID}

class function TMessaging.BoolToStr(const Value: boolean): string;
begin
  Result := {$ifdef FPC} SysUtils.BoolToStr {$else} Iff {$endif}
    (Value, 'true', 'false');
end;

class function TMessaging.TimeToStr(const Value: TFloatTime): string;
begin
  Result := IntToStr(Trunc(Value * 1000));
end;

{ globals -------------------------------------------------------------------- }

var
  FMessaging: TMessaging;
  FinalizationDone: boolean;

procedure DoInitialization;
begin
  if (not FinalizationDone) and (FMessaging = nil) then
    FMessaging := TMessaging.Create;
end;

function Messaging: TMessaging;
begin
  { in case you access Messaging before our unit "initialization" works
    (for example, in case your unit "initiazalition" is executed first and
    it does does some Messaging.Send), then initialize us now. }
  DoInitialization;

  Result := FMessaging;
end;

{ iOS specific --------------------------------------------------------------- }

{$ifdef IOS}
procedure CGEApp_SetReceiveMessageFromPascalCallback(
  ACallback: TMessaging.TReceiveMessageFromPascalCallback); cdecl;
begin
  Messaging.FReceiveMessageFromPascalCallback := ACallback;
end;

procedure CGEApp_SendMessageToPascal(Message: PCChar); cdecl;
begin
  Messaging.ReceiveStr(AnsiString(PChar(Message)));
end;
{$endif IOS}

{ Android specific ----------------------------------------------------------- }

{$ifdef ANDROID}
function Java_net_sourceforge_castleengine_MainActivity_jniMessage(
  Env: PJNIEnv; This: jobject; JavaToNative: jstring): jstring; cdecl;
var
  JavaToNativeStr: PChar;
  Dummy: JBoolean;
begin
  { As this may be called from different thread, secure from being called
    in weird state. }
  if (FMessaging <> nil) and
     (FMessaging.JavaCommunicationCS <> nil) and
     (FMessaging.ToJava <> nil) and
     (FMessaging.FromJava <> nil) then
  begin
    FMessaging.JavaCommunicationCS.Acquire;
    try
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
    finally FMessaging.JavaCommunicationCS.Release end;
  end;
end;
{$endif ANDROID}

initialization
  DoInitialization;
finalization
  FinalizationDone := true;
  FreeAndNil(FMessaging);
end.
