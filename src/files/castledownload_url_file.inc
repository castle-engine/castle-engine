{%MainUnit castledownload.pas}
{
  Copyright 2013-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

{$endif}

{$ifdef read_implementation}

{ Handle file URLs. }

{ TUrlFile ------------------------------------------------------------------- }

type
  TUrlFile = class
    class function Read(const Url: String; out MimeType: String): TStream;
    class function Write(const Url: String): TStream;
  end;

class function TUrlFile.Read(const Url: String; out MimeType: String): TStream;
var
  FileName: String;
begin
  CheckFileAccessSafe(URL);

  FileName := URIToFilenameSafe(URL);
  if FileName = '' then
    raise EDownloadError.CreateFmt('Cannot convert URL "%s" to filename', [URL]);

  Result := TFileStream.Create(FileName, fmOpenRead);
  MimeType := URIMimeType(URL);
end;

class function TUrlFile.Write(const Url: String): TStream;
var
  FileName: String;
begin
  FileName := URIToFilenameSafe(URL);
  if FileName = '' then
    raise ESaveError.CreateFmt('Cannot convert URL to a filename: "%s"', [URL]);

  Result := TFileStream.Create(FileName, fmCreate);
end;

{$endif}
