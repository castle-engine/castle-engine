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
    class function Read(const Url: String; out MimeType: string): TStream;
    class function MimeType(const URI: string; out Gzipped: Boolean): String;
  end;

class function TUrlCompiled.Read(const Url: String; out MimeType: string): TStream;
begin
  MimeType := 'text/x-castle-compiled';
  Result := MemoryStreamLoadFromString(InternalURIDeleteProtocol(URL));
end;

class function TUrlCompiled.MimeType(const URI: string; out Gzipped: Boolean): String;
begin
  Result := 'text/x-castle-compiled';
  Gzipped := false;
end;

{$endif}
