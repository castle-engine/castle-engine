{%MainUnit castledownload.pas}
{
  Copyright 2021-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

{ @exclude
  CastleUriUtils.UriMimeType actually calls this, for now.
  Maybe it should be in higher unit, used by both CastleDownload and CastleUriUtils,
  like CastleUrls. }
function InternalUriMimeType(const URI: string; out Gzipped: boolean): string; overload;

{$endif}

{$ifdef read_implementation}

function InternalUriMimeType(const URI: string; out Gzipped: boolean): string;
var
  P: string;
  RegisteredProtocol: TRegisteredProtocol;
begin
  Result := '';
  Gzipped := false;

  P := URIProtocol(URI);
  RegisteredProtocol := RegisteredProtocols.Find(P);
  if (RegisteredProtocol <> nil) and
     (Assigned(RegisteredProtocol.MimeTypeEvent)) then
  begin
    Result := RegisteredProtocol.MimeTypeEvent(URI, Gzipped);
  end else
  begin
    WritelnWarning('Querying MIME type for URL protocol "%s" not supported', [P]);
  end;
end;

{$endif}
