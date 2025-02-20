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

uses SysUtils, Classes,
  {$ifdef FPC} OpenSSLSockets, {$endif} // support HTTPS
  CastleDownload, CastleXmlUtils;

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

begin
  DownloadSchema;


end.
