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

{ Cross-platform recoginition of the system language/locale. }
unit CastleSystemLanguage;

{$I castleconf.inc}

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