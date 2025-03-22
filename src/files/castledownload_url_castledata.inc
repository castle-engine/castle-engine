{%MainUnit castledownload.pas}
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

{$ifdef read_interface}

{$endif}

{$ifdef read_implementation}

{ Handle castle-data URLs.
  Note: There's not much here, because most of our routines
  do ResolveCastleDataUrl, so they effectively have hardcoded handling
  to read and write castle-data URLs.
  The castle-data protocol is quite special for our engine,
  see https://castle-engine.io/data , so for now it deserves this "special,
  hardcoded" handling. }

{ TUrlFile ------------------------------------------------------------------- }

type
  TUrlCastleData = class
    class function Exists(const Url: String): TUriExists;
  end;

class function TUrlCastleData.Exists(const Url: String): TUriExists;

  // Detect existence of castle-data:/xxx URL using DataDirectoryInformation.
  function UseDataDirectoryInformation(const Url: String): TUriExists;
  var
    U: TUri;
    UrlPath: String;
    PathEntry: TDirectoryInformation.TEntry;
  begin
    U := ParseUri(Url);
    UrlPath := PrefixRemove('/', U.Path + U.Document, false);
    PathEntry := DataDirectoryInformation.FindEntry(UrlPath);
    if PathEntry = nil then
      Exit(ueNotExists)
    else
    if PathEntry is TDirectoryInformation.TDirectory then
      Exit(ueDirectory)
    else
      Exit(ueFile);
  end;

begin
  if (DisableDataDirectoryInformation = 0) and
     (DataDirectoryInformation <> nil) then
    Result := UseDataDirectoryInformation(Url)
  else
    { Recursively call UriExists, on URL that it no longer castle-data.

      This way we can work in case we have castle-data:/xxx URL that resolves
      to something handled by other code (like file:/xxx) but wasn't handled
      by DataDirectoryInformation (e.g. because DataDirectoryInformation = nil). }
    Result := UriExists(ResolveCastleDataUrl(Url));
end;

{$endif}
