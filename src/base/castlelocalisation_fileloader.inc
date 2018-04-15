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

{$ifdef read_interface}

  {$I castlelocalisation_genericcollection.inc}

  type
    TFileLoaderJSONEntry = class(TCollectionItem)
    private
      FKey: String;
      FValue: String;
    published //Properties are case-sensitive in JSON!
      property key: String read FKey write FKey;
      property value: String read FValue write FValue;
    end;

    TFileLoaderJSONList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TGenericCollection<TFileLoaderJSONEntry>;

    TMOFileIterable = class(TMOFile)
    protected
      function GetKey(AIndex: Cardinal): String;
      function GetValue(AIndex: Cardinal): String;
    public
      property Count: Cardinal read StringCount;
      property Keys[AIndex: Cardinal]: String read GetKey;
      property Values[AIndex: Cardinal]: String read GetValue;
    end;

{$endif read_interface}

{$ifdef read_implementation}

  {$I castlelocalisation_genericcollection.inc}

  { TMOFileIterable }

  function TMOFileIterable.GetKey(AIndex: Cardinal): String;
  begin
      Result := OrigStrings^[AIndex];
  end;

  function TMOFileIterable.GetValue(AIndex: Cardinal): String;
  begin
      Result := TranslStrings^[AIndex];
  end;

  { LoadLanguageFiles }

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

  procedure LoadLanguageFileJSON(const AFileStream: TStream; const ALanguageDictionary: TLanguageDictionary);
  var
    StringStream: TStringStream;
    DeStreamer: TJSONDeStreamer;
    LanguageJSON: TFileLoaderJSONList;
    i: Integer;
  begin
    StringStream := TStringStream.Create;
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

  procedure LoadLanguageFileMO(const AFileStream: TStream; const ALanguageDictionary: TLanguageDictionary);
  var
    LanguageMO: TMOFileIterable;
    i: Integer;
  begin
    LanguageMO := TMOFileIterable.Create(AFileStream);
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
    Localisation.FileLoader.Add('.xml', @LoadLanguageFileXML);
    Localisation.FileLoader.Add('.json', @LoadLanguageFileJSON);
    Localisation.FileLoader.Add('.mo', @LoadLanguageFileMO);
    Localisation.FileLoader.Add('.csv', @LoadLanguageFileCSV);
  end;

{$endif read_implementation}