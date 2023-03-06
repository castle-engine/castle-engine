{
  Copyright 2014-2023 Michalis Kamburelis,
  parts based on LazUTF8 unit copyright by Lazarus developers.
  Parts of this source code are based on Lazarus LazUTF8 source code,
  but no worries --- Lazarus license is exactly the same as Castle Game Engine :)

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Unicode utilities. }
unit CastleUnicode;

{$I castleconf.inc}

interface

uses CastleUtils, CastleStringUtils;

type
  TUnicodeChar = Cardinal;

  TUnicodeCharList = class(TCardinalList)
  public
    { Add a single Unicode character.
      Doesn't add duplicates (contrary to ancestor Add). }
    procedure Add(const C: TUnicodeChar); reintroduce; overload;

    { Add all characters from SampleText.
      Useful to fill TUnicodeCharList
      when you have a sample text of international letters.
      Doesn't add duplicates. }
    procedure Add(const SampleText: string); overload;

    { Add all characters from given set. Try e.g. SimpleAsciiCharacters.
      Doesn't add duplicates. }
    procedure Add(const Characters: TSetOfChars); overload;

    { Express all characters inside as one UTF-8 string. }
    function ToString: String; override;
  end;

{$ifdef FPC}
function UTF8CharacterLength(p: PChar): integer;
function UTF8Length(const s: string): PtrInt; overload;
function UTF8Length(p: PChar; ByteCount: PtrInt): PtrInt; overload;

function UTF8CharStart(UTF8Str: PChar; Len, CharIndex: PtrInt): PChar;
function UTF8Copy(const s: string; StartCharIndex, CharCount: PtrInt): string;
function UTF8SEnding(const S: String; const StartCharIndex: PtrInt): String;

{ Return unicode character pointed by P.
  CharLen is set to 0 only when pointer P is @nil, otherwise it's always > 0.

  The typical usage of this is to iterate over UTF-8 string char-by-char, like this:

  @longCode(#
  var
    C: TUnicodeChar;
    TextPtr: PChar;
    CharLen: Integer;
  begin
    TextPtr := PChar(S);
    C := UTF8CharacterToUnicode(TextPtr, CharLen);
    while (C > 0) and (CharLen > 0) do
    begin
      Inc(TextPtr, CharLen);
      // here process C...
      C := UTF8CharacterToUnicode(TextPtr, CharLen);
    end;
  end;
  #)
}
function UTF8CharacterToUnicode(p: PChar; out CharLen: integer): TUnicodeChar;
//function UTF8CharacterToUnicode(const S: string): TUnicodeChar;

function UnicodeToUTF8(CodePoint: TUnicodeChar): string;
function UnicodeToUTF8Inline(CodePoint: TUnicodeChar; Buf: PChar): integer;

{ Convert all special Unicode characters in the given UTF-8 string to HTML entities.
  This is a helpful routine to visualize a string with any Unicode characters
  using simple ASCII.

  "Special" Unicode characters is "anything outside of safe ASCII range,
  which is between space and ASCII code 128".
  The resulting string contains these special characters encoded
  as HTML entities that show the Unicode code point in hex.
  Like @code(&#xNNNN;) (see https://en.wikipedia.org/wiki/Unicode_and_HTML ).
  Converts also ampersand @code(&) to @code(&amp;) to prevent ambiguities.

  Tip: You can check Unicode codes by going to e.g. https://codepoints.net/U+F3
  for @code(&#xF3;). Just edit this URL in the WWW browser address bar.
}
function UTF8ToHtmlEntities(const S: String): String;

{$else}

{ Get Unicode char code at given position in a standard Delphi String (that is, UnicodeString holding UTF-16). }
function UnicodeStringNextChar(const Text: String; const Index: Integer; out NextCharIndex: Integer): TUnicodeChar;
{$endif FPC}

{ Length of the string, in Unicode characters.

  This works taking into account that:
  @unorderedList(
    @item(with FPC, we expect String = AnsiString and holding UTF-8 data,)
    @item(with Delphi we expect String = UnicodeString and holding UTF-16 data.)
  )
  See https://castle-engine.io/coding_conventions#strings_unicode . }
function StringLength(const S: String): Integer;

{ Copy a number of given Unicode characters from given string.

  StartIndex is 1-based, i.e. the first Unicode character in String has index 1,
  last Unicode character has index StringLength(S).

  In case CountToCopy, it is guaranteed to only copy the maximum possible
  characters, without causing any memory overruns.

  Note that it doesn't try to deal with strings that may end abruptly
  in the middle of a Unicode character (that may span multiple Pascal Char
  (AnsiChar or WideChar) values, possible both in case
  of UTF-8 in AnsiString and UTF-16 in UnicodeString).
  The results of such abrupt ending are undefined: this routine may copy the partial
  (unfinished) Unicode character, or it may reject the unfinished partial character altogether.

  This works taking into account that:
  @unorderedList(
    @item(with FPC, we expect String = AnsiString and holding UTF-8 data,)
    @item(with Delphi we expect String = UnicodeString and holding UTF-16 data.)
  )
  See https://castle-engine.io/coding_conventions#strings_unicode . }
function StringCopy(const S: String; const StartIndex, CountToCopy: Integer): String;

(* TODO:

{ Get Unicode char code at given position in the String.

  This works taking into account that:
  @unorderedList(
    @item(with FPC, we expect String = AnsiString and holding UTF-8 data,)
    @item(with Delphi we expect String = UnicodeString and holding UTF-16 data.)
  )
  See https://castle-engine.io/coding_conventions#strings_unicode . }
function StringUnicodeChar(const S: String; const Index: Integer): TUnicodeChar;
*)

implementation

uses SysUtils{$ifndef FPC}, Character{$endif};

{ TUnicodeCharList ----------------------------------------------------------- }

procedure TUnicodeCharList.Add(const C: TUnicodeChar);
begin
  if IndexOf(C) = -1 then
    inherited Add(C);
end;

procedure TUnicodeCharList.Add(const SampleText: string);
var
  C: TUnicodeChar;
  {$ifdef FPC}
  TextPtr: PChar;
  CharLen: Integer;
  {$else}
  TextIndex: Integer;
  NextTextIndex: Integer;
  TextLength: Integer;
  {$endif}
begin
  {$ifdef FPC}
  TextPtr := PChar(SampleText);
  C := UTF8CharacterToUnicode(TextPtr, CharLen);
  while (C > 0) and (CharLen > 0) do
  {$else}
  TextIndex := 1;
  TextLength := Length(SampleText);
  while (TextIndex <= TextLength) do
  {$endif}
  begin
    {$ifdef FPC}
    Inc(TextPtr, CharLen);
    {$else}
    C := UnicodeStringNextChar(SampleText, TextIndex, NextTextIndex);
    TextIndex := NextTextIndex;
    {$endif}
    Add(C);
    {$ifdef FPC}
    C := UTF8CharacterToUnicode(TextPtr, CharLen);
    {$endif}
  end;
end;

procedure TUnicodeCharList.Add(const Characters: TSetOfChars);
var
  C: char;
begin
  for C in Characters do
    Add(Ord(C));
end;

function TUnicodeCharList.ToString: String;
var
  C: TUnicodeChar;
begin
  Result := '';
  for C in Self do
    Result := Result + {$ifdef FPC}UnicodeToUTF8(C){$else}ConvertFromUtf32(C){$endif};
end;

{ global --------------------------------------------------------------------- }
{$ifdef FPC}

function UTF8CharacterLength(p: PChar): integer;
begin
  if p<>nil then begin
    if ord(p^)<$C0 { binary 11000000 } then begin
      // regular single byte character (#0 is a character, this is pascal ;)
      Result:=1;
    end
    else begin
      // multi byte
      if ((ord(p^) and $E0 { binary 11100000 }) = $C0 { binary 11000000 }) then begin
        // could be 2 byte character
        if (ord(p[1]) and $C0 { binary 11000000 }) = $80 { binary 10000000 } then
          Result:=2
        else
          Result:=1;
      end
      else if ((ord(p^) and $F0 { binary 11110000 }) = $E0 { binary 11100000 }) then begin
        // could be 3 byte character
        if ((ord(p[1]) and $C0 { binary 11000000 }) = $80 { binary 10000000 })
        and ((ord(p[2]) and $C0 { binary 11000000 }) = $80 { binary 10000000 }) then
          Result:=3
        else
          Result:=1;
      end
      else if ((ord(p^) and $F8 { binary 11111000 }) = $F0 { binary 11110000 }) then begin
        // could be 4 byte character
        if ((ord(p[1]) and $C0 { binary 11000000 }) = $80 { binary 10000000 })
        and ((ord(p[2]) and $C0 { binary 11000000 }) = $80 { binary 10000000 })
        and ((ord(p[3]) and $C0 { binary 11000000 }) = $80 { binary 10000000 }) then
          Result:=4
        else
          Result:=1;
      end
      else
        Result:=1;
    end;
  end else
    Result:=0;
end;

function UTF8Length(const s: string): PtrInt;
begin
  Result:=UTF8Length(PChar(s),length(s));
end;

function UTF8Length(p: PChar; ByteCount: PtrInt): PtrInt;
var
  CharLen: LongInt;
begin
  Result:=0;
  while (ByteCount>0) do begin
    inc(Result);
    CharLen:=UTF8CharacterLength(p);
    inc(p,CharLen);
    dec(ByteCount,CharLen);
  end;
end;

{ Len is the length in bytes of UTF8Str
  CharIndex is the position of the desired char (starting at 0), in chars
}
function UTF8CharStart(UTF8Str: PChar; Len, CharIndex: PtrInt): PChar;
var
  CharLen: LongInt;
begin
  Result:=UTF8Str;
  if Result<>nil then begin
    while (CharIndex>0) and (Len>0) do begin
      CharLen:=UTF8CharacterLength(Result);
      dec(Len,CharLen);
      dec(CharIndex);
      inc(Result,CharLen);
    end;
    if (CharIndex<>0) or (Len<0) then
      Result:=nil;
  end;
end;

function UTF8Copy(const s: string; StartCharIndex, CharCount: PtrInt): string;
// returns substring
var
  StartBytePos: PChar;
  EndBytePos: PChar;
  MaxBytes: PtrInt;
begin
  StartBytePos:=UTF8CharStart(PChar(s),length(s),StartCharIndex-1);
  if StartBytePos=nil then
    Result:=''
  else begin
    MaxBytes:=PtrInt(PChar(s)+length(s)-StartBytePos);
    EndBytePos:=UTF8CharStart(StartBytePos,MaxBytes,CharCount);
    if EndBytePos=nil then
      Result:=copy(s,StartBytePos-PChar(s)+1,MaxBytes)
    else
      Result:=copy(s,StartBytePos-PChar(s)+1,EndBytePos-StartBytePos);
  end;
end;

function UTF8SEnding(const S: String; const StartCharIndex: PtrInt): String;
begin
  result := UTF8Copy(S, StartCharIndex, MaxInt)
end;

function UTF8CharacterToUnicode(p: PChar; out CharLen: integer): Cardinal;
{ if p=nil then CharLen=0 otherwise CharLen>0
  If there is an encoding error the Result is undefined.
  Use UTF8FixBroken to fix UTF-8 encoding.
  It does not check if the codepoint is defined in the Unicode tables.
}
begin
  if p<>nil then begin
    if ord(p^)<$C0 { binary 11000000 } then begin
      // regular single byte character (#0 is a normal char, this is pascal ;)
      Result:=ord(p^);
      CharLen:=1;
    end
    else if ((ord(p^) and $E0 { binary 11100000 }) = $C0 { binary 11000000 }) then begin
      // starts with %110 => could be double byte character
      if (ord(p[1]) and $C0 { binary 11000000 }) = $80 { binary 10000000 } then begin
        CharLen:=2;
        Result:=((ord(p^) and $1F { binary 00011111 }) shl 6)
                or (ord(p[1]) and $3F { binary 00111111 });
        if Result<(1 shl 7) then begin
          // wrong encoded, could be an XSS attack
          Result:=0;
        end;
      end else begin
        Result:=ord(p^);
        CharLen:=1;
      end;
    end
    else if ((ord(p^) and $F0 { binary 11110000 }) = $E0 { binary 11100000 }) then begin
      // starts with %1110 => could be triple byte character
      if ((ord(p[1]) and $C0 { binary 11000000 }) = $80 { binary 10000000 })
      and ((ord(p[2]) and $C0 { binary 11000000 }) = $80 { binary 10000000 }) then begin
        CharLen:=3;
        Result:=((ord(p^) and $1F { binary 00011111 }) shl 12)
                or ((ord(p[1]) and $3F { binary 00111111 }) shl 6)
                or (ord(p[2]) and $3F { binary 00111111 });
        if Result<(1 shl 11) then begin
          // wrong encoded, could be an XSS attack
          Result:=0;
        end;
      end else begin
        Result:=ord(p^);
        CharLen:=1;
      end;
    end
    else if ((ord(p^) and $F8 { binary 11111000 }) = $F0 { binary 11110000 }) then begin
      // starts with %11110 => could be 4 byte character
      if ((ord(p[1]) and $C0 { binary 11000000 }) = $80 { binary 10000000 })
      and ((ord(p[2]) and $C0 { binary 11000000 }) = $80 { binary 10000000 })
      and ((ord(p[3]) and $C0 { binary 11000000 }) = $80 { binary 10000000 }) then begin
        CharLen:=4;
        Result:=((ord(p^) and $0F { binary 00001111 }) shl 18)
                or ((ord(p[1]) and $3F { binary 00111111 }) shl 12)
                or ((ord(p[2]) and $3F { binary 00111111 }) shl 6)
                or (ord(p[3]) and $3F { binary 00111111 });
        if Result<(1 shl 16) then begin
          // wrong encoded, could be an XSS attack
          Result:=0;
        end;
      end else begin
        Result:=ord(p^);
        CharLen:=1;
      end;
    end
    else begin
      // invalid character
      Result:=ord(p^);
      CharLen:=1;
    end;
  end else begin
    Result:=0;
    CharLen:=0;
  end;
end;

{
function UTF8CharacterToUnicode(const S: string): TUnicodeChar;
var
  IgnoredCharLen: integer;
begin
  Result := UTF8CharacterToUnicode(PChar(S), IgnoredCharLen);
end;
}

function UnicodeToUTF8(CodePoint: TUnicodeChar): string;
var
  Buf: array[0..6] of Char;
  Len: Integer;
begin
  Len:=UnicodeToUTF8Inline(CodePoint, @Buf[0]);
  Buf[Len]:=#0;
  Result := StrPas(@Buf[0]);
end;

function UnicodeToUTF8Inline(CodePoint: TUnicodeChar; Buf: PChar): integer;
begin
  case CodePoint of
    0..$7f:
      begin
        Result:=1;
        Buf[0]:=char(byte(CodePoint));
      end;
    $80..$7ff:
      begin
        Result:=2;
        Buf[0]:=char(byte($c0 or (CodePoint shr 6)));
        Buf[1]:=char(byte($80 or (CodePoint and $3f)));
      end;
    $800..$ffff:
      begin
        Result:=3;
        Buf[0]:=char(byte($e0 or (CodePoint shr 12)));
        Buf[1]:=char(byte((CodePoint shr 6) and $3f) or $80);
        Buf[2]:=char(byte(CodePoint and $3f) or $80);
      end;
    $10000..$10ffff:
      begin
        Result:=4;
        Buf[0]:=char(byte($f0 or (CodePoint shr 18)));
        Buf[1]:=char(byte((CodePoint shr 12) and $3f) or $80);
        Buf[2]:=char(byte((CodePoint shr 6) and $3f) or $80);
        Buf[3]:=char(byte(CodePoint and $3f) or $80);
      end;
  else
    Result:=0;
  end;
end;

function UTF8ToHtmlEntities(const S: String): String;
var
  C: TUnicodeChar;
  TextPtr: PChar;
  CharLen: Integer;
begin
  TextPtr := PChar(S);
  C := UTF8CharacterToUnicode(TextPtr, CharLen);
  Result := '';
  while (C > 0) and (CharLen > 0) do
  begin
    Inc(TextPtr, CharLen);

    if (C < Ord(' ')) or (C >= 128) then
      Result := Result + '&#x' + IntToHex(C, 1) + ';'
    else
    if C = Ord('&') then
      Result := Result + '&amp;'
    else
      Result := Result + Chr(C);

    C := UTF8CharacterToUnicode(TextPtr, CharLen);
  end;
end;

{$else FPC}

{ Length of string assuming it is a standard Delphi String (that is, UnicodeString holding UTF-16). }
function UnicodeStringLength(const Text: String): Integer;
var
  I: Integer;
begin
  Result := 0;
  I := 1;
  while I <= Length(Text) do
  begin
    if IsSurrogate(Text, I) then
    begin
      Inc(I);
    end;
    Inc(Result);
    Inc(I);
  end;
end;

function UnicodeStringNextChar(const Text: String; const Index: Integer; out NextCharIndex: Integer): TUnicodeChar;
begin
  NextCharIndex := Index + 1;

  // check 4 byte char
  if IsSurrogate(Text, Index) then
  begin
    Inc(NextCharIndex);
  end;

  Result := ConvertToUtf32(Text, Index);
end;

{ Copy a number of given Unicode characters from given string.
  Assumes it is a standard Delphi String (that is, UnicodeString holding UTF-16). }
function UnicodeStringCopy(const S: string; StartCharIndex, CharCount: PtrInt): String;
var
  I: Integer;
  AddedChars: Integer;
begin
  I := StartCharIndex;
  AddedChars := 0;
  while (I <= Length(S)) or (AddedChars < CharCount) do
  begin
    if IsSurrogate(S, I) then
    begin
      Result := Result + S[I];
      Inc(I);
    end;

    Result := Result + S[I]; // TODO: can be dangerous when string is not valid UTF16
    Inc(AddedChars);
    Inc(I);
  end;
end;

{$endif FPC}

function StringLength(const S: String): Integer;
begin
  Result := {$ifdef FPC} UTF8Length {$else} UnicodeStringLength {$endif} (S);
end;

function StringCopy(const S: String; const StartIndex, CountToCopy: Integer): String;
begin
  Result := {$ifdef FPC} UTF8Copy {$else} UnicodeStringCopy {$endif} (S, StartIndex, CountToCopy);
end;

(*
function StringUnicodeChar(const S: String; const Index: Integer): TUnicodeChar;
begin
  Result := {$ifdef FPC} xxx {$else} xxx {$endif} (S, Index);
end;
*)

end.
