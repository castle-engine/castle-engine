{
  Copyright 2015-2024 Michalis Kamburelis.

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
  {$ifdef CASTLE_IOS} CTypes, {$endif}
  Generics.Collections, Classes,
  CastleStringUtils, CastleTimeUtils;

type
  { Called by TMessaging when a new message from service is received.

    Returns if the message was handled (this does @bold(not) block
    the message from being passed to other callbacks, it only means
    we will not report a warning about unhandled message).

    ReceivedStream may be @nil if service didn't provide any binary data stream.
  }
  TMessageReceivedEvent = function (const Received: TCastleStringList;
    const ReceivedStream: TMemoryStream): Boolean of object;

  { Used by TMessaging to manage a list of listeners. }
  TMessageReceivedEventList = class({$ifdef FPC}specialize{$endif} TList<TMessageReceivedEvent>)
  public
    procedure ExecuteAll(const Received: TCastleStringList; const ReceivedStream: TMemoryStream);
  end;

  { Message system to communicate between native code (Pascal) and other languages
    (Java on Android, Objective-C on iOS) that possibly run in other thread.
    Use through auto-created @link(Messaging) singleton.
    On platforms other than Android / iOS, it simply does nothing
    --- messsages are not send anywhere.

    To make this work, on Android you need to declare your Android project type
    as "integrated" (this is actually the default now).
    See https://castle-engine.io/android-Services .
    For iOS it is always enabled.

    All the communication is asynchronous on all platforms -- Pascal code sends a message,
    and any answers will come asynchronously later. This means that e.g.
    @link(TGameService.RequestSignedIn) will never call
    @link(TGameService.OnStatusChanged) right inside,
    the call to @link(TGameService.OnStatusChanged) will always happen at a later time.

    This is used automatically by various engine classes like
    @link(TGameService), @link(TAds), @link(TAnalytics), @link(TInAppPurchases). }
  TMessaging = class
  private
    {$ifdef ANDROID}
    JavaCommunicationCS: TCriticalSection;
    FromPascal: TCastleStringList;
    {$endif}

    {$ifdef CASTLE_IOS}
    type
      TReceiveMessageFromPascalCallback = procedure (Message: PCChar); cdecl;
    class var
      FReceiveMessageFromPascalCallback: TReceiveMessageFromPascalCallback;
    {$endif}

    var
      ToPascal: TCastleStringList;
      FOnReceive: TMessageReceivedEventList;
      FLog: boolean;
    { Called constantly to empty the ToPascal list. }
    procedure Update(Sender: TObject);
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

    { Convert string to a boolean, assuming the string was send by the external service.
      The counterpart of this in Android is ServiceAbstract.booleanToString . }
    class function MessageToBoolean(const Value: String): Boolean;
  end;

{$ifdef ANDROID}
{ Export this function from your Android library. }
function Java_io_castleengine_MainActivity_jniMessage(
  Env: PJNIEnv; This: jobject;
  MessageToPascal: jstring;
  MessageToPascalStream: jbyteArray): jstring; cdecl;
{$endif}

{$ifdef CASTLE_IOS}
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

procedure TMessageReceivedEventList.ExecuteAll(const Received: TCastleStringList;
  const ReceivedStream: TMemoryStream);
var
  I: Integer;
  EventResult, Handled: boolean;
begin
  Handled := false;

  // The permission-xxx messages do not have to be handled by anything.
  if (Received.Count > 0) and
     ( (Received[0] = 'permission-granted') or
       (Received[0] = 'permission-cancelled') ) then
    Handled := true;

  for I := 0 to Count - 1 do
  begin
    { Use EventResult to workaround FPC 3.1.1 bug (reproducible
      with FPC SVN revision 35460 (from 2016-02-20) on Linux x86_64).
      The bug has since been fixed http://bugs.freepascal.org/view.php?id=31421 ,
      so we may remove this workaround soon. }
    EventResult := Items[I](Received, ReceivedStream);
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
  ToPascal := TCastleStringList.Create;

  {$ifdef ANDROID}
  JavaCommunicationCS := TCriticalSection.Create;
  FromPascal := TCastleStringList.Create;
  {$endif ANDROID}

  { Only register the Update on platforms where CastleMessaging is actually used. }
  {$if defined(ANDROID) or defined(CASTLE_IOS)}
  ApplicationProperties.OnUpdate.Add({$ifdef FPC}@{$endif} Update);
  {$endif}
end;

destructor TMessaging.Destroy;
begin
  if ApplicationProperties(false) <> nil then
    ApplicationProperties(false).OnUpdate.Remove({$ifdef FPC}@{$endif} Update);
  FreeAndNil(ToPascal);
  FreeAndNil(FOnReceive);

  {$ifdef ANDROID}
  FreeAndNil(JavaCommunicationCS);
  FreeAndNil(FromPascal);
  {$endif ANDROID}

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

procedure TMessaging.Send(const Strings: array of string);

  procedure SendStr(const Message: string);
  begin
    { secure in case this is called from state Finish when things are finalized }
    if Self = nil then Exit;

    if Log then
      WritelnLog('Messaging', 'Pascal code sends a message to service: ' + SReadableForm(Message));

    {$ifdef ANDROID}
    JavaCommunicationCS.Acquire;
    try
      FromPascal.Add(Message);
    finally JavaCommunicationCS.Release end;
    {$endif ANDROID}

    {$ifdef CASTLE_IOS}
    if Assigned(FReceiveMessageFromPascalCallback) then
      FReceiveMessageFromPascalCallback(PCChar(Message))
    else
      WritelnWarning('Messaging', 'Message cannot be delivered, iOS application not finished loading yet');
    {$endif CASTLE_IOS}
  end;

begin
  if High(Strings) = -1 then Exit; // exit in case of empty list
  SendStr(GlueStrings(Strings, MessageDelimiter));
end;

procedure TMessaging.Update(Sender: TObject);

  { Handle message using Pascal logic.
    MessageStream may be @nil if service didn't provide any binary data stream. }
  procedure ReceiveStr(const Message: String; const MessageStream: TMemoryStream);
  var
    MessageAsList: TCastleStringList;
  begin
    if Log then
      WritelnLog('Messaging', 'Pascal code received a message from service: ' + SReadableForm(Message));
    if Message = '' then
      WritelnWarning('Messaging', 'Pascal code received an empty message');

    MessageAsList := CastleStringUtils.SplitString(Message, MessageDelimiter);
    try
      OnReceive.ExecuteAll(MessageAsList, MessageStream);
    finally FreeAndNil(MessageAsList) end;
  end;

  function GetNextMessageToPascal(out Received: String; out ReceivedStream: TMemoryStream): Boolean;
  begin
    {$ifdef ANDROID}
    JavaCommunicationCS.Acquire;
    try
    {$endif ANDROID}

    Result := ToPascal.Count <> 0;
    if Result then
    begin
      Received := ToPascal[0];
      ReceivedStream := ToPascal.Objects[0] as TMemoryStream;
      ToPascal.Delete(0);
    end;

    {$ifdef ANDROID}
    finally JavaCommunicationCS.Release end;
    {$endif ANDROID}
  end;

var
  Received: String;
  ReceivedStream: TMemoryStream;
begin
  while GetNextMessageToPascal(Received, ReceivedStream) do
  begin
    ReceiveStr(Received, ReceivedStream);
    FreeAndNil(ReceivedStream);
  end;
end;

class function TMessaging.BoolToStr(const Value: boolean): string;
begin
  Result := Iff(Value, 'true', 'false');
end;

class function TMessaging.TimeToStr(const Value: TFloatTime): string;
begin
  Result := IntToStr(Trunc(Value * 1000));
end;

class function TMessaging.MessageToBoolean(const Value: String): Boolean;
begin
  if Value = 'true' then
    Result := true
  else
  if Value = 'false' then
    Result := false
  else
    raise EInternalError.CreateFmt('Invalid boolean value in message: %s', [Value]);
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

{$ifdef CASTLE_IOS}
procedure CGEApp_SetReceiveMessageFromPascalCallback(
  ACallback: TMessaging.TReceiveMessageFromPascalCallback); cdecl;
begin
  Messaging.FReceiveMessageFromPascalCallback := ACallback;
end;

procedure CGEApp_SendMessageToPascal(Message: PCChar); cdecl;
begin
  { For consistent behavior with Android, do not receive and process messages synchronously. }
  // Messaging.ReceiveStr(AnsiString(PChar(Message)));

  Messaging.ToPascal.Add(AnsiString(PChar(Message)));
end;
{$endif CASTLE_IOS}

{ Android specific ----------------------------------------------------------- }

{$ifdef ANDROID}
function Java_io_castleengine_MainActivity_jniMessage(
  Env: PJNIEnv; This: jobject;
  MessageToPascal: jstring;
  MessageToPascalStream: jbyteArray): jstring; cdecl;

  { Read Java byte[] into TMemoryStream. }
  function GetBinaryDataStream(const BytesObject: jbyteArray): TMemoryStream;
  var
    Bytes: PJByte;
    Dummy: JBoolean;
    Len: JSize;
  begin
    { See https://docs.oracle.com/javase/8/docs/technotes/guides/jni/spec/functions.html
      about JNI functions meaning. }
    Len := Env^^.GetArrayLength(Env, BytesObject);
    Bytes := Env^^.GetByteArrayElements(Env, BytesObject, Dummy);
    try
      Result := TMemoryStream.Create;
      if Len <> 0 then
        Result.WriteBuffer(Bytes^, Len);
    finally Env^^.ReleaseByteArrayElements(Env, BytesObject, Bytes, 0) end;
  end;

var
  MessageToPascalStr: PChar;
  Dummy: JBoolean;
  Stream: TObject;
begin
  { As this may be called from different thread, secure from being called
    in weird state. }
  if (FMessaging <> nil) and
     (FMessaging.JavaCommunicationCS <> nil) and
     (FMessaging.FromPascal <> nil) and
     (FMessaging.ToPascal <> nil) then
  begin
    FMessaging.JavaCommunicationCS.Acquire;
    try
      if FMessaging.FromPascal.Count <> 0 then
      begin
        Result := Env^^.NewStringUTF(Env, PChar(FMessaging.FromPascal[0]));
        FMessaging.FromPascal.Delete(0);
      end else
        Result := Env^^.NewStringUTF(Env, nil);

      if MessageToPascalStream <> nil then
        Stream := GetBinaryDataStream(MessageToPascalStream)
      else
        Stream := nil;

      if (MessageToPascal <> nil) and
         (Env^^.GetStringUTFLength(Env, MessageToPascal) <> 0) then
      begin
        Dummy := 0;
        MessageToPascalStr := Env^^.GetStringUTFChars(Env, MessageToPascal,
          {$ifdef VER2} Dummy {$else} @Dummy {$endif});
        try
          FMessaging.ToPascal.AddObject(AnsiString(MessageToPascalStr), Stream); // will copy characters
        finally Env^^.ReleaseStringUTFChars(Env, MessageToPascal, MessageToPascalStr) end;
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
