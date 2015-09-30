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

{ Simple message system to communicate between native code and Java
  on Android. On other platforms, right now it simply does nothing
  --- messsages are not send anywhere.

  The counterpart Java code of this, that implements jniMessage in Java,
  will soon be integrated with Castle Game Engine. }
unit CastleJavaMessaging;

{ Log each message send/received from/to Java. }
{ $define CASTLE_DEBUG_JAVA_MESSAGING}

interface

{$ifdef ANDROID} uses JNI; {$endif}

procedure MessageToJava(const S: string);
function MessageFromJava: string;

{$ifdef ANDROID}
function Java_net_sourceforge_castleengine_MainActivity_jniMessage(
  Env: PJNIEnv; This: jobject; JavaToNative: jstring): jstring; cdecl;
{$endif}

const
  { Gravity constants for some messages, for example to indicate ad placement.
    Equal to constants on
    http://developer.android.com/reference/android/view/Gravity.html }
  GravityLeft = $00000003; //< Push object to the left of its container, not changing its size.
  GravityRight = $00000005; //< Push object to the right of its container, not changing its size.
  GravityTop = $00000030; //< Push object to the top of its container, not changing its size.
  GravityBottom = $00000050; //< Push object to the bottom of its container, not changing its size.
  GravityCenterHorizontal = $00000001; //< Place object in the horizontal center of its container, not changing its size.
  GravityCenterVertical = $00000010; //< Place object in the vertical center of its container, not changing its size.
  GravityNo = 0; //< Constant indicating that no gravity has been set.

implementation

uses SysUtils, SyncObjs, CastleLog, CastleStringUtils;

var
  JavaCommunicationCS: TCriticalSection;
  ToJava: TCastleStringList;
  FromJava: TCastleStringList;

procedure MessageToJava(const S: string);
begin
  { secure in case this is called from state Finish when things are finalized }
  if JavaCommunicationCS = nil then Exit;
  JavaCommunicationCS.Acquire;
  try
    {$ifdef CASTLE_DEBUG_JAVA_MESSAGING}
    WritelnLog('JNI', 'Native code posting message to Java: ' + S);
    {$endif}
    ToJava.Add(S);
  finally JavaCommunicationCS.Release end;
end;

function MessageFromJava: string;
begin
  JavaCommunicationCS.Acquire;
  try
    if FromJava.Count <> 0 then
    begin
      Result := FromJava[0];
      {$ifdef CASTLE_DEBUG_JAVA_MESSAGING}
      WritelnLog('JNI', 'Native code received a message from Java: ' + Result);
      {$endif}
      FromJava.Delete(0);
    end else
      Result := '';
  finally JavaCommunicationCS.Release end;
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
    if (ToJava <> nil) and (FromJava <> nil) then
    begin
      if ToJava.Count <> 0 then
      begin
        Result := Env^^.NewStringUTF(Env, PChar(ToJava[0]));
        ToJava.Delete(0);
      end else
        Result := Env^^.NewStringUTF(Env, nil);

      if (JavaToNative <> nil) and
         (Env^^.GetStringUTFLength(Env, JavaToNative) <> 0) then
      begin
        Dummy := 0;
        JavaToNativeStr := Env^^.GetStringUTFChars(Env, JavaToNative,
          {$ifdef VER2} Dummy {$else} @Dummy {$endif});
        try
          FromJava.Add(AnsiString(JavaToNativeStr)); // will copy characters
        finally Env^^.ReleaseStringUTFChars(Env, JavaToNative, JavaToNativeStr) end;
      end;
    end;
  finally JavaCommunicationCS.Release end;
end;
{$endif}

initialization
  JavaCommunicationCS := TCriticalSection.Create;
  ToJava := TCastleStringList.Create;
  FromJava := TCastleStringList.Create;
finalization
  FreeAndNil(JavaCommunicationCS);
  FreeAndNil(ToJava);
  FreeAndNil(FromJava);
end.
