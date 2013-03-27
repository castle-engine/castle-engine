{
  Copyright 2007-2013 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ URI utilities. These extend standard FPC URIParser unit. }
unit CastleURIUtils;

interface

{ Extracts #anchor from URI. On input, URI contains full URI.
  On output, Anchor is removed from URI and saved in Anchor.
  If no #anchor existed, Anchor is set to ''. }
procedure URIExtractAnchor(var URI: string; out Anchor: string);

{ Replace all sequences like %xx with their actual 8-bit characters.

  The intention is that this is similar to PHP function with the same name.

  To account for badly encoded strings, invalid encoded URIs do not
  raise an error --- they are only reported to OnWarning.
  So you can simply ignore them, or write a warning about them for user.
  This is done because often you will use this with
  URIs provided by the user, read from some file etc., so you can't be sure
  whether they are correctly encoded, and raising error unconditionally
  is not OK. (Considering the number of bad HTML pages on WWW.)

  The cases of badly encoded strings are:

  @unorderedList(
    @item("%xx" sequence ends unexpectedly at the end of the string.
      That is, string ends with "%" or "%x". In this case we simply
      keep "%" or "%x" in resulting string.)

    @item("xx" in "%xx" sequence is not a valid hexadecimal number.
      In this case we also simply keep "%xx" in resulting string.)
  )
}
function RawURIDecode(const S: string): string;

{ Get protocol from given URI.

  This is very similar to how URIParser.ParseURI function detects the protocol,
  although not 100% compatible:

  @unorderedList(
    @item(We allow whitespace (including newline) before protocol name.

      This is useful, because some VRML/X3D files have the ECMAScript code
      inlined and there is sometimes whitespace before "ecmascript:" protocol.)

    @item(We never detect a single-letter protocol name.

      This is useful, because we do not use any single-letter protocol name,
      and it allows to detect Windows absolute filenames like
      @code(c:\blah.txt) as filenames. Otherwise, Windows absolute filenames
      could not be accepted by any of our routines that work with URLs
      (like the @link(Download) function),
      since they would be detected as URLs with unknown protocol "c".

      Our URIProtocol will answer that protocol is empty for @code(c:\blah.txt).
      Which means no protocol, so our engine will treat it as a filename.
      (In contrast with URIParser.ParseURI that would detect protocol called "c".)
      See doc/uri_filename.txt in sources for more comments about differentiating
      URI and filenames in our engine.)
  )
}
function URIProtocol(const URI: string): string;

{ Check does URI contain given Protocol.
  This is equivalent to checking URIProtocol(S) = Protocol, ignoring case,
  although may be a little faster. Given Protocol string cannot contain
  ":" character. }
function URIProtocolIs(const S: string; const Protocol: string; out Colon: Integer): boolean;

function URIDeleteProtocol(const S: string): string;

{ Return absolute URI, given base and relative URI.

  Base URI must be either an absolute (with protocol) URI, or only
  an absolute filename (in which case we'll convert it to file:// URI under
  the hood, if necessary). This is usually the URI of the containing file,
  for example an HTML file referencing the image, processed by AbsoluteURI.

  Relative URI may be a relative URI or an absolute URI.
  In the former case it is merged with Base.
  In the latter case it is simply returned. }
function CombineURI(const Base, Relative: string): string;

{ Make sure that the URI is absolute (always has a protocol).
  This function treats an URI without a protocol as a simple filename
  (absolute or relative to the current directory). }
function AbsoluteURI(const URI: string): string;

implementation

uses SysUtils, CastleStringUtils, CastleWarnings, CastleFilesUtils,
  URIParser, CastleUtils;

procedure URIExtractAnchor(var URI: string; out Anchor: string);
var
  HashPos: Integer;
begin
  HashPos := BackPos('#', URI);
  if HashPos <> 0 then
  begin
    Anchor := SEnding(URI, HashPos + 1);
    SetLength(URI, HashPos - 1);
  end;
end;

function RawURIDecode(const S: string): string;

  { Assume Position <= Length(S).
    Check is S[Positon] is a start of %xx sequence:
    - if not, exit false
    - if yes, but %xx is invalid, report OnWarning and exit false
    - if yes and %xx is valid, set DecodedChar and exit true }
  function ValidSequence(const S: string; Position: Integer;
    out DecodedChar: char): boolean;
  const
    ValidHexaChars = ['a'..'f', 'A'..'F', '0'..'9'];

    { Assume C is valid hex digit, return it's value (in 0..15 range). }
    function HexDigit(const C: char): Byte;
    begin
      if C in ['0'..'9'] then
        Result := Ord(C) - Ord('0') else
      if C in ['a'..'f'] then
        Result := 10 + Ord(C) - Ord('a') else
      if C in ['A'..'F'] then
        Result := 10 + Ord(C) - Ord('A');
    end;

  begin
    Result := S[Position] = '%';
    if Result then
    begin
      if Position + 2 > Length(S) then
      begin
        OnWarning(wtMajor, 'URI', Format(
          'URI "%s" incorrectly encoded, %%xx sequence ends unexpectedly', [S]));
        Exit(false);
      end;

      if (not (S[Position + 1] in ValidHexaChars)) or
         (not (S[Position + 2] in ValidHexaChars)) then
      begin
        OnWarning(wtMajor, 'URI', Format(
          'URI "%s" incorrectly encoded, %s if not a valid hexadecimal number',
          [S, S[Position + 1] + S[Position + 2]]));
        Exit(false);
      end;

      Byte(DecodedChar) := (HexDigit(S[Position + 1]) shl 4) or
                            HexDigit(S[Position + 2]);
    end;
  end;

var
  I, ResultI: Integer;
  DecodedChar: char;
begin
  { Allocate Result string at the beginning, to save time later for
    memory reallocations. We can do this, since we know that final
    Result is shorter or equal to S. }
  SetLength(Result, Length(S));

  ResultI := 1;
  I := 1;

  while I <= Length(S) do
  begin
    if ValidSequence(S, I, DecodedChar) then
    begin
      Result[ResultI] := DecodedChar;
      Inc(ResultI);
      Inc(I, 3);
    end else
    begin
      Result[ResultI] := S[I];
      Inc(ResultI);
      Inc(I);
    end;
  end;

  SetLength(Result, ResultI - 1);
end;

{ Detect protocol delimiting positions.
  If returns true, then for sure:
  - FirstCharacter < Colon
  - FirstCharacter >= 1
  - Colon > 1 }
function URIProtocolIndex(const S: string; out FirstCharacter, Colon: Integer): boolean;
const
  { These constants match URIParser algorithm, which in turn follows RFC. }
  ALPHA = ['A'..'Z', 'a'..'z'];
  DIGIT = ['0'..'9'];
  ProtoFirstChar = ALPHA;
  ProtoChar = ALPHA + DIGIT + ['+', '-', '.'];
var
  I: Integer;
begin
  Result := false;
  Colon := Pos(':', S);
  if Colon <> 0 then
  begin
    (* Skip beginning whitespace from protocol.
       This allows us to detect properly "ecmascript:" protocol in VRML/X3D:
      Script { url "
        ecmascript:..." }
    *)
    FirstCharacter := 1;
    while (FirstCharacter < Colon) and (S[FirstCharacter] in WhiteSpaces) do
      Inc(FirstCharacter);
    if FirstCharacter >= Colon then
      Exit;

    { Protocol name can only contain specific characters. }
    if not (S[FirstCharacter] in ProtoFirstChar) then
      Exit;
    for I := FirstCharacter + 1 to Colon - 1 do
      if not (S[I] in ProtoChar) then
        Exit;

    { Do not treat drive names in Windows filenames as protocol.
      To allow stable testing, do this on all platforms, even non-Windows.
      We do not use any single-letter protocol, so no harm. }
    Result := not ((FirstCharacter = 1) and (Colon = 2));
  end;
end;

function URIProtocol(const URI: string): string;
var
  FirstCharacter, Colon: Integer;
begin
  if URIProtocolIndex(URI, FirstCharacter, Colon) then
    Result := CopyPos(URI, FirstCharacter, Colon - 1) else
    Result := '';
end;

function URIProtocolIs(const S: string; const Protocol: string; out Colon: Integer): boolean;
var
  FirstCharacter, I: Integer;
begin
  Result := false;
  if URIProtocolIndex(S, FirstCharacter, Colon) and
     (Colon - FirstCharacter = Length(Protocol)) then
  begin
    for I := 1 to Length(Protocol) do
      if UpCase(Protocol[I]) <> UpCase(S[I - FirstCharacter + 1]) then
        Exit;
    Result := true;
  end;
end;

function URIDeleteProtocol(const S: string): string;
var
  FirstCharacter, Colon: Integer;
begin
  if URIProtocolIndex(S, FirstCharacter, Colon) then
    { Cut off also whitespace before FirstCharacter }
    Result := SEnding(S, Colon + 1) else
    Result := S;
end;

function CombineURI(const Base, Relative: string): string;
begin
  if not ResolveRelativeURI(AbsoluteURI(Base), Relative, Result) then
  begin
    { The only case when ResolveRelativeURI may fail is when neither argument
      contains a protocol. But we just used AbsoluteURI, which makes sure
      that AbsoluteURI(Base) has some protocol. }
    raise EInternalError.CreateFmt('Failed to resolve relative URI "%s" with base "%s"',
      [Relative, Base]);
  end;
end;

function AbsoluteURI(const URI: string): string;
begin
  if URIProtocol(URI) = '' then
    Result := FilenameToURI(ExpandFileName(URI)) else
    Result := URI;
end;

end.