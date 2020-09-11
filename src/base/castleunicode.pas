{
  Copyright 2014-2018 Michalis Kamburelis,
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
  end;

function UTF8CharacterLength(p: PChar): integer;
function UTF8Length(const s: string): PtrInt; overload;
function UTF8Length(p: PChar; ByteCount: PtrInt): PtrInt; overload;

function UTF8CharStart(UTF8Str: PChar; Len, CharIndex: PtrInt): PChar;
function UTF8CodepointSizeFull(p: PChar): integer;
function UTF8Copy(const s: string; StartCharIndex, CharCount: PtrInt): string;

procedure UTF8Insert(const source: String; var s: String; StartCharIndex: PtrInt);
function UTF8CodepointStart(UTF8Str: PChar; Len, CodepointIndex: PtrInt): PChar;
function UTF8CodepointSize(p: PChar): integer; inline;
function UTF8FactLength(s:char): integer;
procedure UTF8Delete(var s: String; StartCharIndex, CharCount: PtrInt);
function UTF8PosP(SearchForText: PChar; SearchForTextLen: SizeInt;
  SearchInText: PChar; SearchInTextLen: SizeInt): PChar;
function UTF8Pos(const SearchForText, SearchInText: string;
  StartPos: SizeInt = 1): PtrInt;
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

implementation

uses SysUtils;

procedure TUnicodeCharList.Add(const C: TUnicodeChar);
begin
  if IndexOf(C) = -1 then
    inherited Add(C);
end;

procedure TUnicodeCharList.Add(const SampleText: string);
var
  C: TUnicodeChar;
  TextPtr: PChar;
  CharLen: Integer;
begin
  TextPtr := PChar(SampleText);
  C := UTF8CharacterToUnicode(TextPtr, CharLen);
  while (C > 0) and (CharLen > 0) do
  begin
    Inc(TextPtr, CharLen);
    Add(C);
    C := UTF8CharacterToUnicode(TextPtr, CharLen);
  end;
end;

procedure TUnicodeCharList.Add(const Characters: TSetOfChars);
var
  C: char;
begin
  for C in Characters do
    Add(Ord(C));
end;

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

function UTF8CodepointSizeFull(p: PChar): integer;
begin
  case p^ of
  #0..#191: // %11000000
    // regular single byte character (#0 is a character, this is Pascal ;)
    Result:=1;
  #192..#223: // p^ and %11100000 = %11000000
    begin
      // could be 2 byte character
      if (ord(p[1]) and %11000000) = %10000000 then
        Result:=2
      else
        Result:=1;
    end;
  #224..#239: // p^ and %11110000 = %11100000
    begin
      // could be 3 byte character
      if ((ord(p[1]) and %11000000) = %10000000)
      and ((ord(p[2]) and %11000000) = %10000000) then
        Result:=3
      else
        Result:=1;
    end;
  #240..#247: // p^ and %11111000 = %11110000
    begin
      // could be 4 byte character
      if ((ord(p[1]) and %11000000) = %10000000)
      and ((ord(p[2]) and %11000000) = %10000000)
      and ((ord(p[3]) and %11000000) = %10000000) then
        Result:=4
      else
        Result:=1;
    end;
  else
    Result:=1;
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

procedure UTF8Insert(const source: String; var s: String; StartCharIndex: PtrInt
  );
var
  StartBytePos: PChar;
begin
  StartBytePos:=UTF8CodepointStart(PChar(s),length(s),StartCharIndex-1);
  if StartBytePos <> nil then
    Insert(source, s, StartBytePos-PChar(s)+1);
end;

function UTF8CodepointStart(UTF8Str: PChar; Len, CodepointIndex: PtrInt): PChar;
var
  CharLen: LongInt;
begin
  Result:=UTF8Str;
  if Result<>nil then begin
    while (CodepointIndex>0) and (Len>0) do begin
      CharLen:=UTF8CodepointSize(Result);
      dec(Len,CharLen);
      dec(CodepointIndex);
      inc(Result,CharLen);
    end;
    if (CodepointIndex<>0) or (Len<0) then
      Result:=nil;
  end;
end;

function UTF8CodepointSize(p: PChar): integer;
begin
  if p=nil then exit(0);
  if p^<#192 then exit(1);
  Result:=UTF8CodepointSizeFull(p);
end;

function UTF8FactLength(s: char): integer;
var
  S1:integer;
begin
 S1:=Str2ToInt(Utf8ToAnsi(s));
 Result:=s1;
end;

procedure UTF8Delete(var s: String; StartCharIndex, CharCount: PtrInt);
var
    StartBytePos: PChar;
  EndBytePos: PChar;
  MaxBytes: PtrInt;
begin
  StartBytePos:=UTF8CodepointStart(PChar(s),length(s),StartCharIndex-1);
  if StartBytePos <> nil then
  begin
    MaxBytes:=PtrInt(PChar(s)+length(s)-StartBytePos);
    EndBytePos:=UTF8CodepointStart(StartBytePos,MaxBytes,CharCount);
    if EndBytePos=nil then
      Delete(s,StartBytePos-PChar(s)+1,MaxBytes)
    else
      Delete(s,StartBytePos-PChar(s)+1,EndBytePos-StartBytePos);
  end;

end;

function UTF8PosP(SearchForText: PChar; SearchForTextLen: SizeInt;
  SearchInText: PChar; SearchInTextLen: SizeInt): PChar;
// returns the position where SearchInText starts in SearchForText
// returns nil if not found
var
  p: SizeInt;
begin
  Result:=nil;
  if (SearchForText=nil) or (SearchForTextLen=0) or (SearchInText=nil) then
    exit;
  while SearchInTextLen>0 do begin
    p:=IndexByte(SearchInText^,SearchInTextLen,PByte(SearchForText)^);
    if p<0 then exit;
    inc(SearchInText,p);
    dec(SearchInTextLen,p);
    if SearchInTextLen<SearchForTextLen then exit;
    if CompareMem(SearchInText,SearchForText,SearchForTextLen) then
      exit(SearchInText);
    inc(SearchInText);
    dec(SearchInTextLen);
  end;
end;

function UTF8Pos(const SearchForText, SearchInText: string;
  StartPos: SizeInt = 1): PtrInt;
// returns the character index, where the SearchForText starts in SearchInText
// an optional StartPos can be given (in UTF-8 codepoints, not in byte)
// returns 0 if not found
var
  i: SizeInt;
  p: PChar;
  StartPosP: PChar;
begin
  Result:=0;
  if StartPos=1 then
  begin
    i:=System.Pos(SearchForText,SearchInText);
    if i>0 then
      Result:=UTF8Length(PChar(SearchInText),i-1)+1;
  end
  else if StartPos>1 then
  begin
    // skip
    StartPosP:=UTF8CodepointStart(PChar(SearchInText),Length(SearchInText),StartPos-1);
    if StartPosP=nil then exit;
    // search
    p:=UTF8PosP(PChar(SearchForText),length(SearchForText),
                StartPosP,length(SearchInText)+PChar(SearchInText)-StartPosP);
    // get UTF-8 position
    if p=nil then exit;
    Result:=StartPos+UTF8Length(StartPosP,p-StartPosP);
  end;
end;

function UTF8CharacterToUnicode(p: PChar; out CharLen: integer): TUnicodeChar;
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

end.
