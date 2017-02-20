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

{ Message system to communicate between native code and Java on Android
  (TMessaging). }
unit CastleMessaging;

{$I castleconf.inc}

interface

uses {$ifdef ANDROID} JNI, {$endif} SyncObjs,
  CastleStringUtils, CastleGenericLists, CastleTimeUtils;

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

    To make this work:

    @unorderedList(
      @item(Include the necessary integration code in your Android project.
        Simply declare your Android project type as "integrated".
        https://github.com/castle-engine/castle-engine/wiki/Android-Project-Components-Integrated-with-Castle-Game-Engine .)

      @item(In your main Android library lpr file, you need to export
        the JNI function @code(Java_net_sourceforge_castleengine_MainActivity_jniMessage)
        defined in this unit.
        So change your xxx_android.lpr file from

        @longCode(#
          library xxx;
          uses CastleAndroidNativeAppGlue, Game;
          exports
            ANativeActivity_onCreate;
          end.
        #)

        to this:

        @longCode(#
          library xxx;
          uses CastleAndroidNativeAppGlue, Game, CastleMessaging;
          exports
            Java_net_sourceforge_castleengine_MainActivity_jniMessage,
            ANativeActivity_onCreate;
          end.
        #)

      )
    )

    This is used automatically by various engine classes like
    @link(TGooglePlayGames), @link(TAds), @link(TAnalytics), @link(TInAppPurchases). }
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
    { Called constantly to process messages from Java. }
    procedure Update(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    { Send a message to our Java integration code. }
    procedure Send(const Strings: array of string);
    { Callbacks called when new message from Java is received. }
    property OnReceive: TMessageReceivedEventList read FOnReceive;

    { Log each message send/received from/to Java.
      Note that this is sometimes quite verbose, and it also allows cheaters
      to easier debug what happens in your game (e.g. how to fake getting
      some achievement), so in general don't leave it "on" in production. }
    property Log: boolean read FLog write FLog default false;

    { Convert boolean to 'true' or 'false' string, which will be understood correctly
      by the Java components receiving the messages. }
    class function BoolToStr(const Value: boolean): string;
    { Convert float time (in seconds) to integer miliseconds, which are understood correctly
      by the Java components receiving the messages. }
    class function TimeToStr(const Value: TFloatTime): string;
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
  CastleUtils, CastleLog, CastleApplicationProperties;

var
  JavaCommunicationCS: TCriticalSection;

{ TMessageReceivedEventList -------------------------------------------------- }

procedure TMessageReceivedEventList.ExecuteAll(const Received: TCastleStringList);
var
  I: Integer;
  EventResult, Handled: boolean;
begin
  Handled := false;
  for I := 0 to Count - 1 do
  begin
    { use EventResult to workaround FPC 3.1.1 bug (reproducible
      with FPC SVN revision 35460 (from 2016-02-20) on Linux x86_64). }
    EventResult := Items[I](Received);
    Handled := EventResult or Handled;
  end;
  if not Handled then
    WritelnWarning('JNI', 'Unhandled message from Java:' + NL + Received.Text);
end;

{ TMessaging ----------------------------------------------------------------- }

constructor TMessaging.Create;
begin
  inherited;
  ToJava := TCastleStringList.Create;
  FromJava := TCastleStringList.Create;
  FOnReceive := TMessageReceivedEventList.Create;

  {$ifdef ANDROID}
  { No point in doing this on non-Android, as only Android communicates
    through it. }
  ApplicationProperties.OnUpdate.Add(@Update);
  {$endif}
end;

destructor TMessaging.Destroy;
begin
  if ApplicationProperties(false) <> nil then
    ApplicationProperties(false).OnUpdate.Remove(@Update);
  FreeAndNil(ToJava);
  FreeAndNil(FromJava);
  FreeAndNil(FOnReceive);
  inherited;
end;

const
  { This is a nice separator, as it has really low chance of occuring in non-binary data.
    - It's also not 0, so it will not be confused with "end of string" (Pascal is invulnerable
      to this, and can have #0 in the middle of AnsiString freely
      but I'm not so sure about Java).
    - It's also within ASCII range, so it will not occur within any UTF-8 multibyte sequence
      (UTF-8 treats specially only stuff > 128, and you can search for ASCII substrings disregaring
      the UTF-8 multibyte stuff, as far as I know). }
  MessageDelimiter = #1;

procedure TMessaging.SendStr(const S: string);
begin
  { secure in case this is called from state Finish when things are finalized }
  if Self = nil then Exit;
  JavaCommunicationCS.Acquire;
  try
    if CastleLog.Log and Log then
      WritelnLog('JNI', 'Native code posting message to Java: ' + SReadableForm(S));
    ToJava.Add(S);
  finally JavaCommunicationCS.Release end;
end;

procedure TMessaging.Send(const Strings: array of string);
begin
  if High(Strings) = -1 then Exit; // exit in case of empty list
  SendStr(GlueStrings(Strings, MessageDelimiter));
end;

function TMessaging.ReceiveStr: string;
begin
  JavaCommunicationCS.Acquire;
  try
    if FromJava.Count <> 0 then
    begin
      Result := FromJava[0];
      if CastleLog.Log and Log then
        WritelnLog('JNI', 'Native code received a message from Java: ' + SReadableForm(Result));
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
    Result := SplitString(S, MessageDelimiter) else
    Result := nil;
end;

procedure TMessaging.Update(Sender: TObject);
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

class function TMessaging.BoolToStr(const Value: boolean): string;
begin
  Result := SysUtils.BoolToStr(Value, 'true', 'false');
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
