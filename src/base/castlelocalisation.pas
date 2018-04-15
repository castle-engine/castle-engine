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
  Classes, Generics.Collections,
  CastleSystemLanguage,
  CastleStringUtils,
  CastleControls, CastleOnScreenMenu;

type
  TLanguageDictionary = {$ifdef CASTLE_OBJFPC}specialize{$endif} TDictionary<String, String>;

  TFileLoaderAction = procedure(const APathURL: String; const ALanguageDictionary: TLanguageDictionary);
  TFileLoaderDictionary = {$ifdef CASTLE_OBJFPC}specialize{$endif} TDictionary<String, TFileLoaderAction>;

  TOnLocalisationUpdatedEvent = procedure of object;
  TOnLocalisationUpdatedEventList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TList<TOnLocalisationUpdatedEvent>;

  TOnUpdateLocalisationEvent = procedure(const ALocalisedText: String) of object;
  TOnUpdateLocalisationEventList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TList<TOnUpdateLocalisationEvent>;

  TLocalisationIDList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TDictionary<TOnUpdateLocalisationEvent, String>;

type
  { Interface for all user components using the localisation.
    Allows to automatically localise and adjust a TComponent to language changes. }
  ICastleLocalisation = interface
    ['{4fa1cb64-f806-2409-07cc-ca1a77e5c0e4}']
    procedure OnUpdateLocalisation(const ALocalisedText: String);
    procedure FreeNotification(AComponent: TComponent);
  end;

type
  TCastleLocalisation = class (TComponent)
    protected
      FLanguageDictionary: TLanguageDictionary;
      FLanguageURL: String;
      FFileLoaderDictionary: TFileLoaderDictionary;
      FLocalisationIDList: TLocalisationIDList;
      FOnUpdateLocalisationEventList: TOnUpdateLocalisationEventList;
      FOnLocalisationUpdatedEventList: TOnLocalisationUpdatedEventList;
      function Get(AKey: String): String;
      procedure LoadLanguage(const ALanguageURL: String);
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      function SystemLanguage(const ADefaultLanguage: String = SystemDefaultLanguage): String; inline;
      function SystemLocal(const ADefaultLocal: String = SystemDefaultLocal): String; inline;
      procedure AddOrSet(ALocalisationComponent: ICastleLocalisation; ALocalisationID: String);
    public
      property Items[AKey: String]: String read Get; default;
      property LanguageURL: String read FLanguageURL write LoadLanguage;
      property FileLoader: TFileLoaderDictionary read FFileLoaderDictionary;
      property OnUpdateLocalisation: TOnLocalisationUpdatedEventList read FOnLocalisationUpdatedEventList;
  end;

var
  Localisation: TCastleLocalisation; //Singleton.

{$define read_interface}
{$I castlelocalisation_fileloader.inc}
{$I castlelocalisation_castlecore.inc}
{$undef read_interface}

implementation

uses
  SysUtils, StrUtils,
  {$warnings off}
    DOM, XMLRead,
    CastleXMLUtils, CastleURIUtils, CastleUtils, CastleDownload;
  {$warnings on}

{$define read_implementation}
{$I castlelocalisation_fileloader.inc}
{$I castlelocalisation_castlecore.inc}
{$undef read_implementation}

//////////////////////////
//Constructor/Destructor//
//////////////////////////

constructor TCastleLocalisation.Create(AOwner: TComponent);
begin
  inherited;

  FLanguageDictionary := TLanguageDictionary.Create;
  FFileLoaderDictionary := TFileLoaderDictionary.Create;
  FLocalisationIDList := TLocalisationIDList.Create;
  FOnLocalisationUpdatedEventList := TOnLocalisationUpdatedEventList.Create;
  FOnUpdateLocalisationEventList := TOnUpdateLocalisationEventList.Create;
end;

destructor TCastleLocalisation.Destroy;
begin
  FreeAndNil(FOnUpdateLocalisationEventList);
  FreeAndNil(FOnLocalisationUpdatedEventList);
  FreeAndNil(FLocalisationIDList);
  FreeAndNil(FFileLoaderDictionary);
  FreeAndNil(FLanguageDictionary);

  inherited;
end;

/////////////////////
//Private/Protected//
/////////////////////

function TCastleLocalisation.Get(AKey: String): String;
begin
  if not FLanguageDictionary.TryGetValue(AKey, Result) then
    Result := AKey; //When no translation is found, return the key.
end;

procedure TCastleLocalisation.LoadLanguage(const ALanguageURL: String);
var
  FileLoaderAction: TFileLoaderAction;
  LocalisedText: String;
  OnUpdateLocalisationEvent: TOnUpdateLocalisationEvent;
begin
  if FLanguageURL = ALanguageURL then Exit;
  FLanguageURL := ALanguageURL;

  FLanguageDictionary.Clear;

  if ALanguageURL = '' then Exit; //If there's no language XML file, then that's it, no more localisation.

  FFileLoaderDictionary.TryGetValue(ExtractFileExt(ALanguageURL), FileLoaderAction);
  Check(Assigned(FileLoaderAction), 'There is no file loader associated with the extension of the given file.');

  FileLoaderAction(AbsoluteURI(ALanguageURL), FLanguageDictionary);

  //Tell every registered object to update its localisation:
  for OnUpdateLocalisationEvent in FOnUpdateLocalisationEventList do
  begin
    FLocalisationIDList.TryGetValue(OnUpdateLocalisationEvent, LocalisedText);
    OnUpdateLocalisationEvent(Items[LocalisedText]);
  end;
end;

procedure TCastleLocalisation.Notification(AComponent: TComponent; Operation: TOperation);
var
  LCastleLocalisationComponent: ICastleLocalisation;
begin
  if Operation = opRemove then
  begin
    AComponent.RemoveFreeNotification(Self);

    LCastleLocalisationComponent := AComponent as ICastleLocalisation;
    FOnUpdateLocalisationEventList.Remove(@LCastleLocalisationComponent.OnUpdateLocalisation);
    FLocalisationIDList.Remove(@LCastleLocalisationComponent.OnUpdateLocalisation);
  end;
end;

//////////////
////Public////
//////////////

function TCastleLocalisation.SystemLanguage(const ADefaultLanguage: String = SystemDefaultLanguage): String;
begin
  Result := CastleSystemLanguage.SystemLanguage(ADefaultLanguage);
end;

function TCastleLocalisation.SystemLocal(const ADefaultLocal: String = SystemDefaultLocal): String;
begin
  Result := CastleSystemLanguage.SystemLocal(ADefaultLocal);
end;

procedure TCastleLocalisation.AddOrSet(ALocalisationComponent: ICastleLocalisation; ALocalisationID: String);
var
  IsNewEntry: Boolean;
begin
  if ALocalisationID = '' then
    Exit;

  IsNewEntry := true;
  try
    FLocalisationIDList.Add(@ALocalisationComponent.OnUpdateLocalisation, ALocalisationID);
  except
    //There is an exception raised if the value already exists.
    IsNewEntry := false;
    FLocalisationIDList.AddOrSetValue(@ALocalisationComponent.OnUpdateLocalisation, ALocalisationID);
  end;

  if IsNewEntry then
  begin
    FOnUpdateLocalisationEventList.Add(@ALocalisationComponent.OnUpdateLocalisation);
    ALocalisationComponent.FreeNotification(Self);
  end;

  ALocalisationComponent.OnUpdateLocalisation(Items[ALocalisationID]);
end;

initialization
  Localisation := TCastleLocalisation.Create(nil);
  ActivateAllFileLoader;

finalization
  FreeAndNil(Localisation);

end.