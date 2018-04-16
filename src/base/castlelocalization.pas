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

{ Localization system for handling localization.
  Use this in your games for easy localization.
  Note that this is not the only way to localize your Castle Game Engine games.
  You can as well use resourcestrings and standard FPC GetText unit directly,
  as shown in the example code in "examples/localization". }
unit CastleLocalization;

{$I castleconf.inc}
{$ifdef FPC}{$interfaces corba}{$endif}

interface

uses
  Classes, GetText, Generics.Collections,
  CastleSystemLanguage,
  CastleStringUtils,
  CastleControls, CastleOnScreenMenu;

type
  TLanguageDictionary = {$ifdef CASTLE_OBJFPC}specialize{$endif} TDictionary<String, String>;

  TFileLoaderAction = procedure(const AFileStream: TStream; const ALanguageDictionary: TLanguageDictionary);
  TFileLoaderDictionary = {$ifdef CASTLE_OBJFPC}specialize{$endif} TDictionary<String, TFileLoaderAction>;

  TOnLocalizationUpdatedEvent = procedure of object;
  TOnLocalizationUpdatedEventList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TList<TOnLocalizationUpdatedEvent>;

  TOnUpdateLocalizationEvent = procedure(const ALocalizedText: String) of object;
  TOnUpdateLocalizationEventList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TList<TOnUpdateLocalizationEvent>;

  TLocalizationIDList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TDictionary<TOnUpdateLocalizationEvent, String>;

type
  { Interface for all user components using the localisation.
    Allows to automatically localise and adjust a TComponent to language changes. }
  ICastleLocalization = interface
    ['{4fa1cb64-f806-2409-07cc-ca1a77e5c0e4}']
    procedure OnUpdateLocalization(const ALocalizedText: String);
    procedure FreeNotification(AComponent: TComponent);
  end;

type
  TCastleLocalization = class (TComponent)
    protected
      FLanguageDictionary: TLanguageDictionary;
      FLanguageURL: String;
      FFileLoaderDictionary: TFileLoaderDictionary;
      FLocalizationIDList: TLocalizationIDList;
      FOnUpdateLocalizationEventList: TOnUpdateLocalizationEventList;
      FOnLocalizationUpdatedEventList: TOnLocalizationUpdatedEventList;
      function Get(AKey: String): String;
      procedure LoadLanguage(const ALanguageURL: String);
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      function SystemLanguage(const ADefaultLanguage: String = SystemDefaultLanguage): String; inline;
      function SystemLocal(const ADefaultLocal: String = SystemDefaultLocal): String; inline;
      procedure AddOrSet(ALocalizationComponent: ICastleLocalization; ALocalizationID: String);
    public
      property Items[AKey: String]: String read Get; default;
      property LanguageURL: String read FLanguageURL write LoadLanguage;
      property FileLoader: TFileLoaderDictionary read FFileLoaderDictionary;
      property OnUpdateLocalization: TOnLocalizationUpdatedEventList read FOnLocalizationUpdatedEventList;
  end;

var
  Localization: TCastleLocalization; //Singleton.

{$define read_interface}
{$I castlelocalization_fileloader.inc}
{$I castlelocalization_castlecore.inc}
{$undef read_interface}

implementation

uses
  SysUtils, StrUtils,
  {$warnings off}
    DOM, XMLRead, fpjsonrtti, CSVDocument,
    CastleXMLUtils, CastleURIUtils, CastleUtils, CastleDownload;
  {$warnings on}

{$define read_implementation}
{$I castlelocalization_fileloader.inc}
{$I castlelocalization_castlecore.inc}
{$undef read_implementation}

//////////////////////////
//Constructor/Destructor//
//////////////////////////

constructor TCastleLocalization.Create(AOwner: TComponent);
begin
  inherited;

  FLanguageDictionary := TLanguageDictionary.Create;
  FFileLoaderDictionary := TFileLoaderDictionary.Create;
  FLocalizationIDList := TLocalizationIDList.Create;
  FOnLocalizationUpdatedEventList := TOnLocalizationUpdatedEventList.Create;
  FOnUpdateLocalizationEventList := TOnUpdateLocalizationEventList.Create;
end;

destructor TCastleLocalization.Destroy;
begin
  FreeAndNil(FOnUpdateLocalizationEventList);
  FreeAndNil(FOnLocalizationUpdatedEventList);
  FreeAndNil(FLocalizationIDList);
  FreeAndNil(FFileLoaderDictionary);
  FreeAndNil(FLanguageDictionary);

  inherited;
end;

/////////////////////
//Private/Protected//
/////////////////////

function TCastleLocalization.Get(AKey: String): String;
begin
  if not FLanguageDictionary.TryGetValue(AKey, Result) then
    Result := AKey; //When no translation is found, return the key.
end;

procedure TCastleLocalization.LoadLanguage(const ALanguageURL: String);
var
  FileLoaderAction: TFileLoaderAction;
  Stream: TStream;
  LocalizedText: String;
  OnUpdateLocalizationEvent: TOnUpdateLocalizationEvent;
  OnLocalizationUpdatedEvent: TOnLocalizationUpdatedEvent;
begin
  if FLanguageURL = ALanguageURL then Exit;
  FLanguageURL := ALanguageURL;

  FLanguageDictionary.Clear;

  if ALanguageURL = '' then Exit; //If there's no language XML file, then that's it, no more localisation.

  FFileLoaderDictionary.TryGetValue(ExtractFileExt(ALanguageURL), FileLoaderAction);
  Check(Assigned(FileLoaderAction), 'There is no file loader associated with the extension of the given file.');

  Stream := Download(AbsoluteURI(ALanguageURL));
  try
    FileLoaderAction(Stream, FLanguageDictionary);
  finally
    Stream.Free;
  end;

  //Tell every registered object to update its localisation:
  for OnUpdateLocalizationEvent in FOnUpdateLocalizationEventList do
  begin
    FLocalizationIDList.TryGetValue(OnUpdateLocalizationEvent, LocalizedText);
    OnUpdateLocalizationEvent(Items[LocalizedText]);
  end;

  //Tell every custom object to update its localisation:
  for OnLocalizationUpdatedEvent in FOnLocalizationUpdatedEventList do
    OnLocalizationUpdatedEvent();
end;

procedure TCastleLocalization.Notification(AComponent: TComponent; Operation: TOperation);
var
  LCastleLocalizationComponent: ICastleLocalization;
begin
  if Operation = opRemove then
  begin
    AComponent.RemoveFreeNotification(Self);

    LCastleLocalizationComponent := AComponent as ICastleLocalization;
    FOnUpdateLocalizationEventList.Remove(@LCastleLocalizationComponent.OnUpdateLocalization);
    FLocalizationIDList.Remove(@LCastleLocalizationComponent.OnUpdateLocalization);
  end;
end;

//////////////
////Public////
//////////////

function TCastleLocalization.SystemLanguage(const ADefaultLanguage: String = SystemDefaultLanguage): String;
begin
  Result := CastleSystemLanguage.SystemLanguage(ADefaultLanguage);
end;

function TCastleLocalization.SystemLocal(const ADefaultLocal: String = SystemDefaultLocal): String;
begin
  Result := CastleSystemLanguage.SystemLocal(ADefaultLocal);
end;

procedure TCastleLocalization.AddOrSet(ALocalizationComponent: ICastleLocalization; ALocalizationID: String);
var
  IsNewEntry: Boolean;
begin
  if ALocalizationID = '' then
    Exit;

  IsNewEntry := not FLocalizationIDList.ContainsKey(@ALocalizationComponent.OnUpdateLocalization);
  FLocalizationIDList.AddOrSetValue(@ALocalizationComponent.OnUpdateLocalization, ALocalizationID);

  if IsNewEntry then
  begin
    FOnUpdateLocalizationEventList.Add(@ALocalizationComponent.OnUpdateLocalization);
    ALocalizationComponent.FreeNotification(Self);
  end;

  ALocalizationComponent.OnUpdateLocalization(Items[ALocalizationID]);
end;

initialization
  Localization := TCastleLocalization.Create(nil);
  ActivateAllFileLoader;

finalization
  FreeAndNil(Localization);

end.