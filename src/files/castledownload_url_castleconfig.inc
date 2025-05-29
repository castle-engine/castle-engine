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

{ Handle castle-config URLs.
  Note: There's not much here, because most of our routines
  do ResolveCastleConfigUrl, so they effectively have hardcoded handling
  to read and write castle-config URLs. }

{ TUrlCastleConfig ------------------------------------------------------------------- }

type
  TUrlCastleConfig = class
    class function Exists(const Url: String): TUriExists;
  end;

class function TUrlCastleConfig.Exists(const Url: String): TUriExists;
begin
  { Recursively call UriExists, on URL that is no longer castle-config. }
  Result := UriExists(ResolveCastleConfigUrl(Url));
end;

{$endif}
