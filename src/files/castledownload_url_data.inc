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

{ Handle data URI ("data URI" allows to embed data in a very long URI;
  this is *not* related to castle-data protocol). }

{ TUrlData ------------------------------------------------------------------- }

type
  TUrlData = class
    class function Read(const Url: String; out MimeType: String): TStream;
  end;

class function TUrlData.Read(const Url: String; out MimeType: String): TStream;
var
  DataUri: TDataUri;
begin
  DataUri := TDataUri.Create;
  try
    DataUri.Uri := Url;
    if not DataUri.Valid then
      raise EDownloadError.Create('Invalid data: URI scheme');
    Result := DataUri.ExtractStream;
    MimeType := DataUri.MimeType;
    Assert(Result <> nil, 'DataUri.ExtractStream must be non-nil when DataUri.Valid is true');
  finally FreeAndNil(DataUri) end;
end;

{$endif}
