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

{ Cross-platform recognition of the system language/locale. }
unit CastleSystemLanguage;

{$I castleconf.inc}

{$ifndef FPC}
  {$message fatal 'This unit is only for FPC, not Delphi.'}
{$endif}

interface

uses
  GetText {$ifdef ANDROID}, JNI{$endif};

const
  SystemDefaultLanguage = 'en';
  SystemDefaultLocale = 'en_US';

{ Returns the language code of the system language. See SystemDefaultLanguage. }
function SystemLanguage(const ADefaultLanguage: String = SystemDefaultLanguage): String; inline;
{ Returns the locale code of the system locale. See SystemDefaultLocale. }
function SystemLocale(const ADefaultLocale: String = SystemDefaultLocale): String; inline;

{$ifdef ANDROID}
{ The CGE build tool will make sure to export this function from Android library.
  @exclude }
procedure Java_net_sourceforge_castleengine_MainActivity_jniLanguage(Env: PJNIEnv; This: jobject; JavaLanguageCode: jstring); cdecl;
{$endif}

implementation

{$ifdef ANDROID}
var
  MobileSystemLanguage: String;
{$endif}

{$ifdef ANDROID}
procedure Java_net_sourceforge_castleengine_MainActivity_jniLanguage(Env: PJNIEnv; This: jobject; JavaLanguageCode: jstring); cdecl;
var
  JavaLanguageCodeStr: PChar;
  Dummy: JBoolean;
begin
  if (JavaLanguageCode <> nil) and (Env^^.GetStringUTFLength(Env, JavaLanguageCode) <> 0) then
  begin
    Dummy := 0;
    JavaLanguageCodeStr := Env^^.GetStringUTFChars(Env, JavaLanguageCode,{$ifdef VER2}Dummy{$else}@Dummy{$endif});
    try
      MobileSystemLanguage := AnsiString(JavaLanguageCodeStr); // will copy characters
    finally Env^^.ReleaseStringUTFChars(Env, JavaLanguageCode, JavaLanguageCodeStr) end;
  end;
end;
{$endif ANDROID}

function SystemLanguage(const ADefaultLanguage: String = SystemDefaultLanguage): String;
begin
  Result := SystemLocale(ADefaultLanguage);
  Delete(Result, 3, Length(Result)); //Removes the locale info behind the language code.
end;

function SystemLocale(const ADefaultLocale: String = SystemDefaultLocale): String;
{$ifndef ANDROID}
var
  TempDefaultLocale: String;
{$endif}
begin
  {$ifdef ANDROID}
    Result := MobileSystemLanguage;
  {$else}
    TempDefaultLocale := ADefaultLocale; //Because GetLanguageIDs, whyever, the default language as var parameter...
    GetLanguageIDs(Result, TempDefaultLocale);
  {$endif}

  if Result = '' then
    Result := ADefaultLocale
  else
    Delete(Result, 6, Length(Result)); //There can be more than the language code and the locale info in the result string.
                                       //For example, on Debian based systems there can be the encoding as suffix. ("en_GB.UTF-8")
                                       //So if we really only want the langauge code and the locale info, we have to delete everything behind it.
end;

end.
