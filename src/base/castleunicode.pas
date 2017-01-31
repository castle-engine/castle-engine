{
  Copyright 2014-2017 Michalis Kamburelis,
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
    { Add a single Unicode character. }
    procedure Add(const C: TUnicodeChar);
    { Add all characters from SampleText.
      Useful to fill TUnicodeCharList
      when you have a sample text of international letters. }
    procedure Add(const SampleText: string);
    { Add all characters from given set. Try e.g. SimpleAsciiCharacters. }
    procedure Add(const Characters: TSetOfChars);
  end;

function UTF8CharacterLength(p: PChar): integer;
function UTF8Length(const s: string): PtrInt;
function UTF8Length(p: PChar; ByteCount: PtrInt): PtrInt;

function UTF8CharStart(UTF8Str: PChar; Len, CharIndex: PtrInt): PChar;
function UTF8Copy(const s: string; StartCharIndex, CharCount: PtrInt): string;

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

implementation

procedure TUnicodeCharList.Add(const C: TUnicodeChar);
begin
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
    if IndexOf(C) = -1 then
    begin
      Add(C);
      //Writeln('Adding extra character ', C);
    end;
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
    if ord(p^)<%11000000 then begin
      // regular single byte character (#0 is a character, this is pascal ;)
      Result:=1;
    end
    else begin
      // multi byte
      if ((ord(p^) and %11100000) = %11000000) then begin
        // could be 2 byte character
        if (ord(p[1]) and %11000000) = %10000000 then
          Result:=2
        else
          Result:=1;
      end
      else if ((ord(p^) and %11110000) = %11100000) then begin
        // could be 3 byte character
        if ((ord(p[1]) and %11000000) = %10000000)
        and ((ord(p[2]) and %11000000) = %10000000) then
          Result:=3
        else
          Result:=1;
      end
      else if ((ord(p^) and %11111000) = %11110000) then begin
        // could be 4 byte character
        if ((ord(p[1]) and %11000000) = %10000000)
        and ((ord(p[2]) and %11000000) = %10000000)
        and ((ord(p[3]) and %11000000) = %10000000) then
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

function UTF8CharacterToUnicode(p: PChar; out CharLen: integer): Cardinal;
{ if p=nil then CharLen=0 otherwise CharLen>0
  If there is an encoding error the Result is undefined.
  Use UTF8FixBroken to fix UTF-8 encoding.
  It does not check if the codepoint is defined in the Unicode tables.
}
begin
  if p<>nil then begin
    if ord(p^)<%11000000 then begin
      // regular single byte character (#0 is a normal char, this is pascal ;)
      Result:=ord(p^);
      CharLen:=1;
    end
    else if ((ord(p^) and %11100000) = %11000000) then begin
      // starts with %110 => could be double byte character
      if (ord(p[1]) and %11000000) = %10000000 then begin
        CharLen:=2;
        Result:=((ord(p^) and %00011111) shl 6)
                or (ord(p[1]) and %00111111);
        if Result<(1 shl 7) then begin
          // wrong encoded, could be an XSS attack
          Result:=0;
        end;
      end else begin
        Result:=ord(p^);
        CharLen:=1;
      end;
    end
    else if ((ord(p^) and %11110000) = %11100000) then begin
      // starts with %1110 => could be triple byte character
      if ((ord(p[1]) and %11000000) = %10000000)
      and ((ord(p[2]) and %11000000) = %10000000) then begin
        CharLen:=3;
        Result:=((ord(p^) and %00011111) shl 12)
                or ((ord(p[1]) and %00111111) shl 6)
                or (ord(p[2]) and %00111111);
        if Result<(1 shl 11) then begin
          // wrong encoded, could be an XSS attack
          Result:=0;
        end;
      end else begin
        Result:=ord(p^);
        CharLen:=1;
      end;
    end
    else if ((ord(p^) and %11111000) = %11110000) then begin
      // starts with %11110 => could be 4 byte character
      if ((ord(p[1]) and %11000000) = %10000000)
      and ((ord(p[2]) and %11000000) = %10000000)
      and ((ord(p[3]) and %11000000) = %10000000) then begin
        CharLen:=4;
        Result:=((ord(p^) and %00001111) shl 18)
                or ((ord(p[1]) and %00111111) shl 12)
                or ((ord(p[2]) and %00111111) shl 6)
                or (ord(p[3]) and %00111111);
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

end.
