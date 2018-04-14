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

{ Localisation system for handling localisation.
  Use this in your games for easy localisation. }
unit CastleLocalisation;

{$I castleconf.inc}
{$interfaces corba}

interface

uses
  Classes, SysUtils, Generics.Collections, {$ifdef ANDROID}JNI,{$endif}
  CastleStringUtils,
  CastleControls, CastleOnScreenMenu;

type
  TLanguage = {$ifdef CASTLE_OBJFPC}specialize{$endif} TDictionary<String, String>;

  TOnLocalisationUpdatedEvent = procedure of object;
  TOnLocalisationUpdatedEventList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TList<TOnLocalisationUpdatedEvent>;

  TOnUpdateLocalisationEvent = procedure(ALocalisedText: String) of object;
  TOnUpdateLocalisationEventList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TList<TOnUpdateLocalisationEvent>;

  TLocalisationIDList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TDictionary<TOnUpdateLocalisationEvent, String>;

type
  { Interface for all user components using the localisation.
    Allows to automatically localise and adjust a TComponent to language changes. }
  ICastleLocalisation = interface
    ['{4fa1cb64-f806-2409-07cc-ca1a77e5c0e4}']
    procedure OnUpdateLocalisation(ALocalisedText: String);
    procedure FreeNotification(AComponent: TComponent);
  end;

type
  TCastleLocalisation = class (TComponent)
    protected
      const DefaultLanguage = 'en';
    protected
      FLanguage: TLanguage;
      FLanguageURL: String;
      FLocalisationIDList: TLocalisationIDList;
      FOnUpdateLocalisationEventList: TOnUpdateLocalisationEventList;
      FOnLocalisationUpdatedEventList: TOnLocalisationUpdatedEventList;
      function Get(AKey: String): String;
      procedure LoadLanguage(const ALanguageURL: String);
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      function SystemLanguage(const ADefaultLanguage: String = DefaultLanguage): String;
      procedure Add(ALocalisationComponent: ICastleLocalisation; ALocalisationID: String);
    public
      property LanguageURL: String read FLanguageURL write LoadLanguage;
      property Items[AKey: String]: String read Get; default;
      property OnUpdateLocalisation: TOnLocalisationUpdatedEventList read FOnLocalisationUpdatedEventList;
  end;

{$ifdef ANDROID}
  { Export this function from your Android library. }
  function Java_net_sourceforge_castleengine_MainActivity_jniLanguage(Env: PJNIEnv; This: jobject; JavaToNative: jstring): jstring; cdecl;
{$endif}

var
  Localisation: TCastleLocalisation; //Singleton.

{$define read_interface}
{$I castlelocalisation_caslecore.inc}
{$undef read_interface}

implementation

{$warnings off}
  uses
    StrUtils, DOM, XMLRead, {$ifdef MSWINDOWS}Windows,{$endif}
    CastleXMLUtils, CastleURIUtils, CastleUtils, CastleDownload;
{$warnings on}

{$define read_implementation}
{$I castlelocalisation_caslecore.inc}
{$undef read_implementation}

var
  MobileSystemLanguage: String;

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

//////////////////////////
//Constructor/Destructor//
//////////////////////////

constructor TCastleLocalisation.Create(AOwner: TComponent);
begin
  inherited;

  FLanguage := TLanguage.Create;
  FLocalisationIDList := TLocalisationIDList.Create;
  FOnLocalisationUpdatedEventList := TOnLocalisationUpdatedEventList.Create;
  FOnUpdateLocalisationEventList := TOnUpdateLocalisationEventList.Create;
end;

destructor TCastleLocalisation.Destroy;
begin
  FreeAndNil(FOnUpdateLocalisationEventList);
  FreeAndNil(FOnLocalisationUpdatedEventList);
  FreeAndNil(FLocalisationIDList);
  FreeAndNil(FLanguage);

  inherited;
end;

/////////////////////
//Private/Protected//
/////////////////////

function TCastleLocalisation.Get(AKey: String): String;
begin
  if not FLanguage.TryGetValue(AKey, Result) then
    Result := AKey; //When no translation is found, return the key.
end;

procedure TCastleLocalisation.LoadLanguage(const ALanguageURL: String);
var
  FileURLAbsolute: String;
  Stream: TStream;
  LanguageXML: TXMLDocument;
  I: TXMLElementIterator;
  LOnUpdateLocalisationEvent: TOnUpdateLocalisationEvent;
  LLocalisedText: String;
begin
  if FLanguageURL = ALanguageURL then Exit;
  FLanguageURL := ALanguageURL;

  FLanguage.Clear;

  if ALanguageURL = '' then Exit; //If there's no language XML file, then that's it, no more localisation.

  FileURLAbsolute := AbsoluteURI(ALanguageURL); //This should be an absolute URL so we doesn't depend on the current directory.

  Stream := Download(FileURLAbsolute);
  try
    ReadXMLFile(LanguageXML, Stream, FileURLAbsolute);
  finally
    Stream.Free;
  end;

  try
    Check(LanguageXML.DocumentElement.TagName = 'strings', 'Root node of local/index.xml must be <strings>');

    I := LanguageXML.DocumentElement.ChildrenIterator;
    try
      while I.GetNext do
      begin
        Check(I.Current.TagName = 'string', 'Each child of local/index.xml root node must be the <string> element');

        FLanguage.AddOrSetValue(I.Current.AttributeString('key'), I.Current.AttributeString('value'));
      end;
    finally
      I.Free;
    end;
  finally
    LanguageXML.Free;
  end;

  //Tell every registered object to update its localisation:
  for LOnUpdateLocalisationEvent in FOnUpdateLocalisationEventList do
  begin
    FLocalisationIDList.TryGetValue(LOnUpdateLocalisationEvent, LLocalisedText);
    LOnUpdateLocalisationEvent(Items[LLocalisedText]);
  end;
end;

procedure TCastleLocalisation.Notification(AComponent: TComponent; Operation: TOperation);
var
  LCastleLocalisationComponent: ICastleLocalisation;
begin
  if Operation = opRemove then
  begin
    LCastleLocalisationComponent := AComponent as ICastleLocalisation;
    FOnUpdateLocalisationEventList.Remove(@LCastleLocalisationComponent.OnUpdateLocalisation);
    FLocalisationIDList.Remove(@LCastleLocalisationComponent.OnUpdateLocalisation);
  end;
end;

//////////////
////Public////
//////////////

function TCastleLocalisation.SystemLanguage(const ADefaultLanguage: String = DefaultLanguage): String;
  {$ifdef MSWINDOWS}
    function GetLocaleInformation(Flag: integer): string;
    var
      pcLCA: array[0..20] of char;
    begin
      if (GetLocaleInfo(LOCALE_SYSTEM_DEFAULT, Flag, pcLCA, 19) <= 0) then
      begin
        pcLCA[0] := #0;
      end;
      Result := pcLCA;
    end;
  {$endif}
begin
  {$ifdef MSWINDOWS}
    Result := GetLocaleInformation(LOCALE_SISO639LANGNAME);
  {$else}
    {$ifdef ANDROID}
      Result := MobileSystemLanguage;
    {$else}
      Result := Copy(GetEnvironmentVariable('LANG'), 1, 2);
    {$endif}
  {$endif}

  if Result = '' then
    Result := ADefaultLanguage;
end;

procedure TCastleLocalisation.Add(ALocalisationComponent: ICastleLocalisation; ALocalisationID: String);
begin
  FLocalisationIDList.AddOrSetValue(@ALocalisationComponent.OnUpdateLocalisation, ALocalisationID);
  FOnUpdateLocalisationEventList.Add(@ALocalisationComponent.OnUpdateLocalisation);
  ALocalisationComponent.FreeNotification(Self);

  ALocalisationComponent.OnUpdateLocalisation(Items[ALocalisationID]);
end;

initialization
  Localisation := TCastleLocalisation.Create(nil);

finalization
  FreeAndNil(Localisation);

end.