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

procedure CheckFileAccessSafe(const URL: string);
begin
  if not ApplicationProperties._FileAccessSafe then
    WritelnWarning('Opening file "%s" before the Application.OnInitialize was called. ' +
      'This is not reliable on mobile platforms (Android, iOS). ' +
      'This usually happens if you open a file from the "initialization" section of a unit. ' +
      'You should do it in Application.OnInitialize instead.',
      [URL]);
end;

{ TUrlFile ------------------------------------------------------------------- }

type
  TUrlFile = class
    class function Read(const Url: string; out MimeType: string): TStream;
    class function Write(const Url: string): TStream;
  end;

class function TUrlFile.Read(const Url: string; out MimeType: string): TStream;
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

class function TUrlFile.Write(const Url: string): TStream;
var
  FileName: String;
begin
  FileName := URIToFilenameSafe(URL);
  if FileName = '' then
    raise ESaveError.CreateFmt('Cannot convert URL to a filename: "%s"', [URL]);

  Result := TFileStream.Create(FileName, fmCreate);
end;

{$endif}
