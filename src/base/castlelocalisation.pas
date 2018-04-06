unit CastleLocalisation;

{$mode objfpc}{$H+}

interface

uses
  Classes, Generics.Collections;

type
  TLanguage = specialize TDictionary<String, String>;

type
  TCastleLocalisation = class
    protected
      FLanguage: TLanguage;
      FLanguageURL: String;
      function Get(AKey: String): String;
    public
      constructor Create; virtual;
      destructor Destroy; override;
      procedure LoadLanguage(const ALanguageURL: String);
    public
      property Items[AKey: String]: String read Get; default;
  end;

function Localisation: TCastleLocalisation; //Singleton.

implementation

{$warnings off}
uses
  SysUtils, DOM, XMLRead,
  CastleXMLUtils, CastleURIUtils, CastleUtils, CastleDownload;
{$warnings on}

//////////////////////////
//Constructor/Destructor//
//////////////////////////

constructor TCastleLocalisation.Create;
begin
  FLanguage := TLanguage.Create;
end;

destructor TCastleLocalisation.Destroy;
begin
  FLanguage.Free;
end;

/////////////////////
//Private/Protected//
/////////////////////

function TCastleLocalisation.Get(AKey: String): String;
begin
  Result := AKey; //When no translation is found, return the key.
  FLanguage.TryGetValue(AKey, Result);
end;

//////////////
////Public////
//////////////

procedure TCastleLocalisation.LoadLanguage(const ALanguageURL: String);
var
  FileURLAbsolute: string;
  Stream: TStream;
  LanguageXML: TXMLDocument;
  I: TXMLElementIterator;
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
end;

//////////
//Global//
//////////

var
  FLocalisation: TCastleLocalisation; //Singleton!

function Localisation: TCastleLocalisation;
begin
  if not Assigned(FLocalisation) then
    FLocalisation := TCastleLocalisation.Create;
  Result := FLocalisation;
end;

finalization
  FreeAndNil(FLocalisation);

end.