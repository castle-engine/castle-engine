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

{$endif read_interface}

{$ifdef read_implementation}

  {$I castlelocalisation_genericcollection.inc}

  procedure LoadLanguageFileXML(const APathURL: String; const ALanguageDictionary: TLanguageDictionary);
  var
    Stream: TStream;
    LanguageXML: TXMLDocument;
    I: TXMLElementIterator;
  begin
    Stream := Download(APathURL);
    try
      ReadXMLFile(LanguageXML, Stream, APathURL);
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

          ALanguageDictionary.AddOrSetValue(I.Current.AttributeString('key'), I.Current.AttributeString('value'));
        end;
      finally
        I.Free;
      end;
    finally
      LanguageXML.Free;
    end;
  end;

  procedure LoadLanguageFileJSON(const APathURL: String; const ALanguageDictionary: TLanguageDictionary);
  var
    Stream: TStream;
    StringStream: TStringStream;
    DeStreamer: TJSONDeStreamer;
    LanguageJSON: TFileLoaderJSONList;
    i: Integer;
  begin
    Stream := Download(APathURL);
    try
      StringStream := TStringStream.Create;
      try
        StringStream.CopyFrom(Stream, Stream.Size);
        FreeAndNil(Stream); //Saves RAM.

        DeStreamer := TJSONDeStreamer.Create(nil);
        try
          LanguageJSON := TFileLoaderJSONList.Create;
          try
            DeStreamer.JSONToObject(StringStream.DataString, LanguageJSON);
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
    finally
      Stream.Free;
    end;
  end;

  procedure ActivateAllFileLoader;
  begin
    Localisation.FileLoader.Add('.xml', @LoadLanguageFileXML);
    Localisation.FileLoader.Add('.json', @LoadLanguageFileJSON);
  end;

{$endif read_implementation}