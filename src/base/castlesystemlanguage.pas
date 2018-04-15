{
  Copyright 2018 Benedikt Magnus.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Cross-platform recoginition of the system language/local. }
unit CastleSystemLanguage;

{$I castleconf.inc}

interface

uses
  GetText {$ifdef ANDROID}, JNI{$endif};

const
  SystemDefaultLanguage = 'en';
  SystemDefaultLocal = 'en_US';

{ Returns the language code of the system language. See SystemDefaultLanguage. }
function SystemLanguage(const ADefaultLanguage: String = SystemDefaultLanguage): String; inline;
{ Returns the local code of the system local. See SystemDefaultLocal. }
function SystemLocal(const ADefaultLocal: String = SystemDefaultLocal): String; inline;

{$ifdef ANDROID}
  { Export this function from your Android library. }
  function Java_net_sourceforge_castleengine_MainActivity_jniLanguage(Env: PJNIEnv; This: jobject; JavaToNative: jstring): jstring; cdecl;
{$endif}

implementation

{$ifdef ANDROID}
var
  MobileSystemLanguage: String;
{$endif}

{$ifdef ANDROID}
  function Java_net_sourceforge_castleengine_MainActivity_jniLanguage(Env: PJNIEnv; This: jobject; JavaToNative: jstring): jstring; cdecl;
  var
    JavaToNativeStr: PChar;
    Dummy: JBoolean;
  begin
    Result := Env^^.NewStringUTF(Env, nil);

    if (JavaToNative <> nil) and (Env^^.GetStringUTFLength(Env, JavaToNative) <> 0) then
    begin
      Dummy := 0;
      JavaToNativeStr := Env^^.GetStringUTFChars(Env, JavaToNative,{$ifdef VER2}Dummy{$else}@Dummy{$endif});
      try
        MobileSystemLanguage := AnsiString(JavaToNativeStr); // will copy characters
      finally
        Env^^.ReleaseStringUTFChars(Env, JavaToNative, JavaToNativeStr) end;
    end;
  end;
{$endif ANDROID}

function SystemLanguage(const ADefaultLanguage: String = SystemDefaultLanguage): String;
begin
  Result := SystemLocal(ADefaultLanguage);
  if Length(Result) > 2 then
    Result := Copy(Result, 1, 2);
end;

function SystemLocal(const ADefaultLocal: String = SystemDefaultLocal): String;
{$ifndef ANDROID}
var
  TempDefaultLocal: String;
{$endif}
begin
  {$ifdef ANDROID}
    Result := MobileSystemLanguage;
  {$else}
    TempDefaultLocal := ADefaultLocal; //Because GetLanguageIDs, whyever, the default language as var parameter...
    GetLanguageIDs(Result, TempDefaultLocal);
  {$endif}

  if Result = '' then
    Result := ADefaultLocal
  else
  if Length(Result) > 5 then //Because on Unix, for example, the encoding is put behind the local info. 
    Result := Copy(Result, 1, 5);
end;

end.