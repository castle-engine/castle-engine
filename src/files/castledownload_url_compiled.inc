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

{ Handle compiled: protocol. }

{ TUrlCompiled ------------------------------------------------------------------- }

type
  TUrlCompiled = class
    class function Read(const Url: string; out MimeType: string): TStream;
  end;

class function TUrlCompiled.Read(const Url: string; out MimeType: string): TStream;
begin
  MimeType := 'text/x-castle-compiled';
  Result := MemoryStreamLoadFromString(URIDeleteProtocol(URL));
end;

{$endif}
