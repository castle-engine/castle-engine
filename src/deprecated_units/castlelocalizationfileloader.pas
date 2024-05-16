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

{ Contains the standard file loader for the CastleLocalization unit. }
unit CastleLocalizationFileLoader deprecated 'use CastleLocalizationGetText instead, it offers more features for now (GenerateGetTextPo, TranslateAllDesigns, TCastleComponent.TranslateProperties)';

{$I castleconf.inc}

interface

uses
  Classes, SysUtils, Generics.Collections,
  {$warnings off}
    DOM, XMLRead, fpjson, fpjsonrtti, CSVDocument, GetText,
  {$warnings on}
  CastleUtils, CastleClassUtils,
  CastleXmlUtils;

type
  { Represents a single translated string in the language JSON file. }
  TFileLoaderJSONEntry = class(TCollectionItem)
  private
    FKey: String;
    FValue: String;
  published //Properties are case-sensitive in JSON!
    property key: String read FKey write FKey;
    property value: String read FValue write FValue;
  end;

{ Called by CastleLocalization to load all standard file loader.
  See Castle examples or documentation for detailed description of the file formats. }
procedure ActivateAllFileLoader;

implementation

{$warnings off} // using deprecated unit here
uses CastleLocalization,
  { For TCastleMOFile }
  CastleLocalizationGetText;
{$warnings on}

{ TGenericCollection -------------------------------------------------------- }

type
  { A generic version of TCollection.
    Main usage is preventing code redundancy when working with JSON serialisation. }
  generic TGenericCollection<T> = class(TCollection)
  private
    function GetItems(AIndex: Integer): T;
    procedure SetItems(AIndex: Integer; AValue: T);
  public
    constructor Create;
    function Add: T;
    property Items[AIndex: Integer]: T read GetItems write SetItems; default;
  end;

function TGenericCollection.GetItems(AIndex: Integer): T;
begin
  Result := T(inherited Items[AIndex]);
end;

procedure TGenericCollection.SetItems(AIndex: Integer; AValue: T);
begin
  Items[AIndex].Assign(AValue);
end;

constructor TGenericCollection.Create;
begin
  inherited Create(T);
end;

function TGenericCollection.Add: T;
begin
  Result := T(inherited Add);
end;

{ TFileLoaderJSONList -------------------------------------------------------- }

type
  { Represents the full language JSON file containing all translated strings. }
  TFileLoaderJSONList = {$ifdef FPC}specialize{$endif} TGenericCollection<TFileLoaderJSONEntry>;

{ LoadLanguageFiles ---------------------------------------------------------- }

procedure LoadLanguageFileXML(const AFileStream: TStream; const ALanguageDictionary: TLanguageDictionary);
var
  LanguageXML: TXMLDocument;
  I: TXMLElementIterator;
begin
  ReadXMLFile(LanguageXML, AFileStream);

  try
    Check(LanguageXML.DocumentElement.TagName = 'strings', 'Root node of local/index.xml must be <strings>');

    I := LanguageXML.DocumentElement.ChildrenIterator;
    try
      while I.GetNext do
      begin
        Check(I.Current.TagName = 'string', 'Each child of local/index.xml root node must be the <string> element');

        ALanguageDictionary.AddOrSetValue(I.Current.AttributeString('key'), I.Current.AttributeString('value'));
      end;
    finally
      I.Free;
    end;
  finally
    LanguageXML.Free;
  end;
end;

procedure LoadLanguageFileJSONObj(const AFileStream: TStream; const ALanguageDictionary: TLanguageDictionary);
var
  StringStream: TStringStream;
  DeStreamer: TJSONDeStreamer;
  LanguageJSON: TFileLoaderJSONList;
  i: Integer;
begin
  StringStream := TStringStream.Create('');
  try
    StringStream.CopyFrom(AFileStream, AFileStream.Size);

    DeStreamer := TJSONDeStreamer.Create(nil);
    try
      LanguageJSON := TFileLoaderJSONList.Create;
      try
        DeStreamer.JSONToObject(StringStream.DataString, LanguageJSON);

        //Save RAM:
        FreeAndNil(StringStream);
        FreeAndNil(DeStreamer);

        for i := 0 to LanguageJSON.Count - 1 do
          ALanguageDictionary.AddOrSetValue(LanguageJSON[i].Key, LanguageJSON[i].Value);
      finally
        LanguageJSON.Free;
      end;
    finally
      DeStreamer.Free;
    end;
  finally
    StringStream.Free;
  end;
end;

procedure LoadLanguageFileJSON(const AFileStream: TStream; const ALanguageDictionary: TLanguageDictionary);
var
  StringStream: TStringStream;
  Data: TJSONData;
  i: Integer;
  Key: String;
begin
  StringStream := TStringStream.Create('');
  try
    StringStream.CopyFrom(AFileStream, AFileStream.Size);

    Data := GetJSON(StringStream.DataString);
    try
      FreeAndNil(StringStream); //Save RAM.

      for i := 0 to Data.Count - 1 do
      begin
        Key := TJSONObject(Data).Names[i];
        ALanguageDictionary.AddOrSetValue(Key, Data.FindPath(Key).AsString);
      end;
    finally
      Data.Free;
    end;
  finally
    StringStream.Free;
  end;
end;

procedure LoadLanguageFileMO(const AFileStream: TStream; const ALanguageDictionary: TLanguageDictionary);
var
  LanguageMO: TCastleMOFile;
  i: Integer;
begin
  LanguageMO := TCastleMOFile.Create(AFileStream);
  try
    for i := 0 to LanguageMO.Count - 1 do
      ALanguageDictionary.AddOrSetValue(LanguageMO.Keys[i], LanguageMO.Values[i]);
  finally
    LanguageMO.Free;
  end;
end;

procedure LoadLanguageFileCSV(const AFileStream: TStream; const ALanguageDictionary: TLanguageDictionary);
var
  Parser: TCSVParser;
  Key: String;
begin
  Parser := TCSVParser.Create;
  try
    Parser.SetSource(AFileStream);
    Key := '';
    while Parser.ParseNextCell do
    begin
      if Parser.CurrentCol mod 2 = 0 then
        Key := Parser.CurrentCellText
      else
        ALanguageDictionary.AddOrSetValue(Key, Parser.CurrentCellText);
    end;
  finally
    Parser.Free;
  end;
end;

procedure ActivateAllFileLoader;
begin
  Localization.FileLoader.Add('.xml', @LoadLanguageFileXML);
  Localization.FileLoader.Add('.json', @LoadLanguageFileJSON);
  Localization.FileLoader.Add('.jsonobj', @LoadLanguageFileJSONObj);
  Localization.FileLoader.Add('.mo', @LoadLanguageFileMO);
  Localization.FileLoader.Add('.csv', @LoadLanguageFileCSV);
end;

end.
