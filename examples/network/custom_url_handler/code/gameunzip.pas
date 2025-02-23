{
  Copyright 2019-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Unzip routines that both with both FPC and Delphi. }
unit GameUnzip;

interface

uses SysUtils, Classes, CastleZip;

{ Unpack single file from zip, to a TStream.
  FileInZip should be relative path within the zip archive. }
function UnzipFile(const ZipFileName, FileInZip: String): TStream;

implementation

function UnzipFile(const ZipFileName, FileInZip: String): TStream;
var
  Zip: TCastleZip;
begin
  Zip := TCastleZip.Create;
  try
    Zip.Open(ZipFileName);
    Result := Zip.Read(FileInZip);
  finally FreeAndNil(Zip) end;
end;

end.
