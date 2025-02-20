{
  Copyright 2025-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
{ Generate Pascal code IFC API. See README.md for details. }

uses SysUtils, Classes, Dom, XmlRead,
  {$ifdef FPC} OpenSSLSockets, {$endif} // support HTTPS
  CastleClassUtils, CastleDownload, CastleXmlUtils;

const
  SchemaUrl = 'https://standards.buildingsmart.org/IFC/RELEASE/IFC4_3/HTML/IFC4X3_ADD2.xsd';
  SchemaFile = 'IFC4X3_ADD2.xsd';

procedure DownloadSchema;
var
  SchemaStream: TMemoryStream;
begin
  if FileExists(SchemaFile) then
    Writeln('Schema file exists: ', SchemaFile)
  else
  begin
    Writeln('Schema file does not exist, downloading: ', SchemaFile);
    EnableBlockingDownloads := true;
    SchemaStream := Download(SchemaUrl, [soForceMemoryStream]) as TMemoryStream;
    try
      SchemaStream.SaveToFile(SchemaFile);
    finally FreeAndNil(SchemaStream) end;
  end;
end;

procedure HandleSimpleType(Node: TDomElement);
begin
  Writeln('Simple type: ', Node.AttributeString('name'));
end;

procedure HandleComplexType(Node: TDomElement);
begin
  Writeln('Complex type: ', Node.AttributeString('name'));
end;

procedure HandleOtherType(Node: TDomElement);
begin
  Writeln('Other type: ', Node.AttributeString('name'));
end;

{ Just like UrlReadXml, but forces encoding from ASCII to UTF-8.
  Reason: IFC XSD uses ASCII encoding,

    <?xml version='1.0' encoding='ASCII'?>

  which causes

    Encoding 'ASCII' is not supported (in file "IFC4X3_ADD2.xsd")

  error from FPC XMLRead (because TXMLDecodingSource.SetEncoding
  does not support ASCII). }
function UrlReadXml_ForceEncodingUtf8(const Url: String): TXmlDocument;
var
  Stream: TStream;
  Str: String;
  NewStream: TStringStream;
begin
  Stream := Download(Url);
  try
    Str := ReadGrowingStreamToDefaultString(Stream);
    Str := StringReplace(Str, 'encoding=''ASCII''', 'encoding=''UTF-8''', []);
    NewStream := TStringStream.Create(Str);
    try
      ReadXmlFile(Result, NewStream);
    finally FreeAndNil(NewStream) end;
  finally FreeAndNil(Stream) end;
end;

var
  Doc: TXmlDocument;
  I: TXMLElementIterator;
begin
  DownloadSchema;

  Doc := UrlReadXml_ForceEncodingUtf8(SchemaFile);
  try
    I := Doc.DocumentElement.ChildrenIterator;
    try
      while I.GetNext do
      begin
        if I.Current.TagName8 = 'xs:simpleType' then
          HandleSimpleType(I.Current)
        else
        if I.Current.TagName8 = 'xs:complexType' then
          HandleComplexType(I.Current)
        else
          HandleOtherType(I.Current);
      end;
    finally FreeAndNil(I) end;
  finally FreeAndNil(Doc) end;
end.
