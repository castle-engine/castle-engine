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

{ Handle castlescript protocols, to specify inline CastleScript code. }

{ For historic reasons, special X3D scripting protocols
  like ecmascript: or castle-script: are read by just stripping protocol,
  not decoding with ParseURI. }
function InternalURIDeleteProtocol(const S: String): String;
begin
  {$warnings off}
  Result := URIDeleteProtocol(S);
  {$warnings on}
end;

{ TUrlCastleScript ------------------------------------------------------------------- }

type
  TUrlCastleScript = class
    class function Read(const Url: String; out MimeType: string): TStream;
    class function MimeType(const URI: string; out Gzipped: Boolean): String;
  end;

class function TUrlCastleScript.Read(const Url: String; out MimeType: string): TStream;
begin
  MimeType := 'text/x-castlescript';
  Result := MemoryStreamLoadFromString(InternalURIDeleteProtocol(URL));
end;

class function TUrlCastleScript.MimeType(const URI: string; out Gzipped: Boolean): String;
begin
  Result := 'text/x-castlescript';
  Gzipped := false;
end;

{$endif}
