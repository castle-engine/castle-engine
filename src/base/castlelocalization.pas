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
  Classes, Generics.Collections,
  CastleSystemLanguage,
  CastleStringUtils,
  CastleControls, CastleOnScreenMenu;

type
  { Dictionary (LocalizationID/TranslatedText as String/String) for storing all translated strings of the current language. }
  TLanguageDictionary = {$ifdef CASTLE_OBJFPC}specialize{$endif} TDictionary<String, String>;

  { Procedure of a file loader called by CastleLocalization to fill the language dictionary from a file stream. }
  TFileLoaderAction = procedure(const AFileStream: TStream; const ALanguageDictionary: TLanguageDictionary);
  { Dictionaty (FileExtension/FileLoaderAction as String/TFileLoaderAction) to connect the known file loaders with it's file extensions.}
  TFileLoaderDictionary = {$ifdef CASTLE_OBJFPC}specialize{$endif} TDictionary<String, TFileLoaderAction>;

  { Called by CastleLocalization to all subscribed procedures when a new language is set. }
  TOnLocalizationUpdatedEvent = procedure of object;
  TOnLocalizationUpdatedEventList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TList<TOnLocalizationUpdatedEvent>;

  { Called by CastleLocalization to all subscribed components when a new language is set. }
  TOnUpdateLocalizationEvent = procedure(const ALocalizedText: String) of object;
  TOnUpdateLocalizationEventList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TList<TOnUpdateLocalizationEvent>;

  { List (dictionary) for the localisation IDs of all subscribed components. }
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
  { Main comonent for localisation, singleton as Localization. }
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
      { Returns the current system language as language code.
        For example: en, de, pl }
      function SystemLanguage(const ADefaultLanguage: String = SystemDefaultLanguage): String; inline;
      { Returns the current system local as langauge code and local info.
        For example: en_US, en_GB, es_ES }
      function SystemLocal(const ADefaultLocal: String = SystemDefaultLocal): String; inline;
      { Adds a new component to the automised localisation list or, if it already is listed, updates it's localisation ID. }
      procedure AddOrSet(ALocalizationComponent: ICastleLocalization; ALocalizationID: String);
    public
      property Items[AKey: String]: String read Get; default;
      { The URL to the language file that shall be loaded for localisation. }
      property LanguageURL: String read FLanguageURL write LoadLanguage;
      { A list (dictionary) of file loaders.
        You can use this to add custom file loader for new file extensions or overwrite existing ones to change the file format. }
      property FileLoader: TFileLoaderDictionary read FFileLoaderDictionary;
      { A list of subscribed procedures of that each will be called when the langauge changes.
        You can add procedure to this to localise images or such that is no descendent of TComponent. }
      property OnUpdateLocalization: TOnLocalizationUpdatedEventList read FOnLocalizationUpdatedEventList;
  end;

var
  { Singleton for TCastleLocalization. }
  Localization: TCastleLocalization;

{$define read_interface}
{$I castlelocalization_castlecore.inc}
{$undef read_interface}

implementation

uses
  SysUtils, StrUtils,
  CastleURIUtils, CastleUtils, CastleDownload,
  CastleLocalizationFileLoader;

{$define read_implementation}
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