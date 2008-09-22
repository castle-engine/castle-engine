{
  Copyright 2007 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ URL utilities. Not much for now, will be much more when handling URLs
  in VRML engine will be really implemented. }
unit KambiURLUtils;

interface

{ This extracts #anchor from URL. On input, URL contains full URL.
  On output, Anchor is removed from URL and saved in Anchor.
  If no #anchor existed, Anchor is set to ''. }
procedure URLExtractAnchor(var URL: string; out Anchor: string);

{ Returns S with all sequences like %xx replaced with their actual
  8-bit characters.

  The intention is that this is similat to PHP function with the same name.

  To account for badly encoded strings, invalid encoded URLs do not
  raise an error --- they are only reported to DataNonFatalError.
  So you can simply ignore them, or write a warning about them for user.
  This is done because often you will use this with
  URLs provided by the user, read from some file etc., so you can't be sure
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
function RawUrlDecode(const S: string): string;

function UrlProtocol(const S: string): string;
function UrlDeleteProtocol(const S: string): string;

implementation

uses SysUtils, KambiStringUtils, DataErrors;

procedure URLExtractAnchor(var URL: string; out Anchor: string);
var
  HashPos: Integer;
begin
  HashPos := BackPos('#', URL);
  if HashPos <> 0 then
  begin
    Anchor := SEnding(URL, HashPos + 1);
    SetLength(URL, HashPos - 1);
  end;
end;

function RawUrlDecode(const S: string): string;

  { Assume Position <= Length(S).
    Check is S[Positon] is a start of %xx sequence:
    - if not, exit false
    - if yes, but %xx is invalid, report DataNonFatalError and exit false
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
        DataNonFatalError(Format(
          'URL "%s" incorrectly encoded, %%xx sequence ends unexpectedly', [S]));
        Exit(false);
      end;

      if (not (S[Position + 1] in ValidHexaChars)) or
         (not (S[Position + 2] in ValidHexaChars)) then
      begin
        DataNonFatalError(Format(
          'URL "%s" incorrectly encoded, %s if not a valid hexadecimal number',
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

function UrlProtocol(const S: string): string;
var
  P: Integer;
begin
  P := Pos(':', S);
  if P = 0 then
    Result := '' else
    Result := Copy(S, 1, P - 1);
end;

function UrlDeleteProtocol(const S: string): string;
var
  P: Integer;
begin
  P := Pos(':', S);
  if P = 0 then
    Result := S else
    Result := SEnding(S, P + 1);
end;

end.