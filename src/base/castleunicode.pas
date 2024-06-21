{
  Copyright 2014-2024 Michalis Kamburelis,
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

    { Express all characters inside as one string.

      The resulting String follows the encoding conventions used throughout CGE,
      which means it will be UTF-8 with FPC (AnsiString)
      or UTF-16 with Delphi (UnicodeString).
      See https://castle-engine.io/coding_conventions#strings_unicode . }
    function ToString: String; override;
  end;

{$ifdef FPC}
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

  @deprecated
  This is FPC-specific, and in practice was only useful for iteration.
  Use TCastleStringIterator instead. }
function UTF8CharacterToUnicode(p: PChar; out CharLen: integer): TUnicodeChar;
  deprecated 'use TCastleStringIterator instead';
{$else}

{ Get Unicode char code at given position in a standard Delphi String
  (that is, UnicodeString holding UTF-16).
  This is useful when iterating over string characters.

  @deprecated
  This is Delphi-specific, and in practice was only useful for iteration.
  Use TCastleStringIterator instead. }
function UnicodeStringNextChar(const Text: String; const Index: Integer; out NextCharIndex: Integer): TUnicodeChar;
  deprecated 'use TCastleStringIterator instead';
{$endif FPC}

{ Length of the string, in Unicode characters.

  This is like standard Pascal @code(Length), but safe for Unicode, and working with
  both FPC and Delphi default String (see https://castle-engine.io/coding_conventions#strings_unicode ).

  This works taking into account that:
  @unorderedList(
    @item(with FPC, we expect String = AnsiString and holding UTF-8 data,)
    @item(with Delphi we expect String = UnicodeString and holding UTF-16 data.)
  )
  See https://castle-engine.io/coding_conventions#strings_unicode . }
function StringLength(const S: String): Integer;

{ Copy a number of Unicode characters from given string, from given position.

  This is like standard Pascal @code(Copy), but safe for Unicode, and working with
  both FPC and Delphi default String (see https://castle-engine.io/coding_conventions#strings_unicode ).

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

{ Copy all characters from given string, from given position.
  StartIndex is 1-based, i.e. the first Unicode character in String has index 1,
  last Unicode character has index StringLength(S).

  This is like @link(SEnding), but safe for Unicode, and working with
  both FPC and Delphi default String (see https://castle-engine.io/coding_conventions#strings_unicode ). }
function StringEnding(const S: String; const StartIndex: Integer): String;

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

{ Express single Unicode character code as a String that you can write. }
function UnicodeCharToString(const C: TUnicodeChar): String;

{ Like UnicodeCharToString, but in case C is not a printable character
  (like ASCII control characters with code < 32),
  show it as '#' + character number.

  Use this only for debugging, or to display error messages,
  because the output is not 100% unambiguous: if the original string
  contains a sequence like #xxx, we make no attempt to "quoute" this sequence.
  This the output is ambiguous, both for human and machine processing.
  It is just "useful enough" for some cases of debugging output.

  To have unambiguous output, use StringWithHtmlEntities.
  This uses HTML entity encoding and takes care to also quote special '&'.
  StringWithHtmlEntities it converts also characters above 128, like Polish and Chinese,
  to numbers -- it is up to your needs whether this is more readable or not,
  depends on how do you output this in practice. }
function UnicodeCharToReadableString(const C: TUnicodeChar): String;

{ Convert all special Unicode characters in the given string to HTML entities.
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
function StringWithHtmlEntities(const S: String): String;

type
  { Iterate over String that contains Unicode characters suitable
    for both FPC (with default String = AnsiString)
    and Delphi (with default String = UnicodeString).

    This should be used for all iteration over strings in Castle Game Engine.
    It abstracts away various details about UTF-8 processing (in case of FPC)
    and UTF-16 processing (in case of Delphi).
    See https://castle-engine.io/coding_conventions#strings_unicode .

    The typical usage looks like this:

    @longCode(#
    var
      Iter: TCastleStringIterator;
    begin
      Iter.Start('my_string');
      while Iter.GetNext do
      begin
        // do something with Iter.Current now
        WritelnLog('Got Unicode character: %d, %s', [
          Iter.Current,
          UnicodeCharToString(Iter.Current)
        ]);
      end;
    end;
    #)

    It is allowed to use @link(Start) again, with the same iterator
    (regardless if it finished or not), to start processing a new (or the same)
    String. }
  TCastleStringIterator = record
  strict private
    FCurrent: TUnicodeChar;
    {$ifdef FPC}
    TextPtr: PChar;
    CharLen: Integer;
    {$else}
    TextCopy: String;
    TextIndex: Integer;
    TextLength: Integer;
    {$endif}
  public
    { Start processing the given String.
      Must be called before accessing @link(Current) or @link(GetNext). }
    procedure Start(const S: String);
    { Call this in a loop.
      Must be called (and return @true) before accessing @link(Current). }
    function GetNext: Boolean;
    { After @link(GetNext) was called, and returned @true,
      read this to get the current Unicode character.
      Do not use it after @link(GetNext) returned @false. }
    property Current: TUnicodeChar read FCurrent;
  end;

implementation

uses SysUtils{$ifndef FPC}, Character{$endif}, CastleLog;

{ TUnicodeCharList ----------------------------------------------------------- }

procedure TUnicodeCharList.Add(const C: TUnicodeChar);
begin
  if IndexOf(C) = -1 then
    inherited Add(C);
end;

procedure TUnicodeCharList.Add(const SampleText: string);
var
  Iter: TCastleStringIterator;
begin
  Iter.Start(SampleText);
  while Iter.GetNext do
    Add(Iter.Current);
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
    Result := Result + UnicodeCharToString(C);
end;

{ global --------------------------------------------------------------------- }
{$ifdef FPC}

function UTF8CharacterLength(p: PChar): Integer;
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

function UTF8Length(const s: string): PtrInt; forward; overload;
function UTF8Length(p: PChar; ByteCount: PtrInt): PtrInt; forward; overload;

function UTF8Length(const s: string): PtrInt;
begin
  Result:=UTF8Length(PChar(s),length(s));
end;

function UTF8Length(p: PChar; ByteCount: PtrInt): PtrInt;
var
  CharLen: Integer;
begin
  Result:=0;
  while (ByteCount>0) do begin
    inc(Result);
    CharLen := UTF8CharacterLength(p);
    inc(p,CharLen);
    dec(ByteCount,CharLen);
  end;
end;

{ Len is the length in bytes of UTF8Str
  CharIndex is the position of the desired char (starting at 0), in chars
}
function UTF8CharStart(UTF8Str: PChar; Len, CharIndex: PtrInt): PChar;
var
  CharLen: Integer;
begin
  Result:=UTF8Str;
  if Result<>nil then begin
    while (CharIndex>0) and (Len>0) do begin
      CharLen := UTF8CharacterLength(Result);
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

function UnicodeToUTF8(CodePoint: TUnicodeChar): string;
var
  Buf: array[0..6] of Char;
  Len: Integer;
begin
  Len:=UnicodeToUTF8Inline(CodePoint, @Buf[0]);
  Buf[Len]:=#0;
  Result := StrPas(@Buf[0]);
end;

{$else FPC}

{ About IsSurrogate usage below:

  IsSurrogate detects either leading or trailing surrogate.
  And in valid UTF-16 string, you either have:

  - 1 code unit (16 bit char) == 1 Unicode code point

    And then this 16-bit character is not surrogate.

  - 2 code unit (2x 16 bit char) == 1 Unicode code point

    And then both 16-bit characters are surrogate (they both will have
    IsSurrogate true).
    1st is high, leading.
    2nd is low, trailing.

  Note that Delphi also has IsHighSurrogate and IsLowSurrogate.

  See https://stackoverflow.com/questions/52584308/convert-unicode-surrogate-pair-to-literal-string :

  """
  In Unicode, you have code points. These are 21 bits long.
  Your character 𝐀, Mathematical Bold Capital A, has a code point of U+1D400.

  In Unicode encodings, you have code units.
  These are the natural unit of the encoding: 8-bit for UTF-8, 16-bit for UTF-16, and so on.
  One or more code units encode a single code point.

  In UTF-16, two code units that form a single code point are called a surrogate pair.
  Surrogate pairs are used to encode any code point greater than 16 bits, i.e. U+10000 and up.
  """

  https://datacadamia.com/data/type/text/surrogate

  """
  A surrogate pair:

  is composed of a two pair of code point (H and L) 1) (with a value in a special range)
  represents a unicode code point known as S with a value above 0xFFFF
  """
}

{ Length of string assuming it is a standard Delphi String
  (that is, UnicodeString holding UTF-16). }
function UnicodeStringLength(const Text: String): Integer;
var
  I: Integer;
begin
  Result := 0;
  I := 1;
  while I <= Length(Text) do
  begin
    if IsSurrogate(Text, I) then
      Inc(I);
    Inc(Result);
    Inc(I);
  end;
end;

function UnicodeStringNextChar(const Text: String; const Index: Integer; out NextCharIndex: Integer): TUnicodeChar;
begin
  NextCharIndex := Index + 1;

  if IsLowSurrogate(Text, Index) then // skip trailing surrogate
    Inc(NextCharIndex);

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
  while (I <= Length(S)) and (AddedChars < CharCount) do
  begin
    if IsSurrogate(S, I) then
    begin
      Result := Result + S[I];
      Inc(I);
    end;

    if I > Length(S) then
    begin
      WritelnWarning('StringCopy', 'String ends in the middle of a Unicode character, cannot copy it');
      Break;
    end;

    Result := Result + S[I];
    Inc(AddedChars);
    Inc(I);
  end;
end;

{$endif FPC}

function StringWithHtmlEntities(const S: String): String;
var
  Iter: TCastleStringIterator;
  C: TUnicodeChar;
begin
  Iter.Start(S);
  Result := '';
  while Iter.GetNext do
  begin
    C := Iter.Current;
    if (C < Ord(' ')) or (C >= 128) then
      Result := Result + '&#x' + IntToHex(C, 1) + ';'
    else
    if C = Ord('&') then
      Result := Result + '&amp;'
    else
      Result := Result + Chr(C);
  end;
end;

function StringLength(const S: String): Integer;
begin
  Result := {$ifdef FPC} UTF8Length {$else} UnicodeStringLength {$endif} (S);
end;

function StringCopy(const S: String; const StartIndex, CountToCopy: Integer): String;
begin
  Result := {$ifdef FPC} UTF8Copy {$else} UnicodeStringCopy {$endif} (S, StartIndex, CountToCopy);
end;

function StringEnding(const S: String; const StartIndex: Integer): String;
begin
  Result := {$ifdef FPC} UTF8Copy {$else} UnicodeStringCopy {$endif} (S, StartIndex, MaxInt);
end;

function UnicodeCharToString(const C: TUnicodeChar): String;
begin
  Result := {$ifdef FPC} UnicodeToUTF8 {$else}ConvertFromUtf32 {$endif} (C);
end;

function UnicodeCharToReadableString(const C: TUnicodeChar): String;
begin
  if C < Ord(' ') then
    Result := '#' + IntToStr(C)
  else
    Result := UnicodeCharToString(C);
end;

(*
function StringUnicodeChar(const S: String; const Index: Integer): TUnicodeChar;
begin
  Result := {$ifdef FPC} xxx {$else} xxx {$endif} (S, Index);
end;
*)
{$ifdef FPC}

procedure TCastleStringIterator.Start(const S: String);
begin
  TextPtr := PChar(S);
end;

function TCastleStringIterator.GetNext: Boolean;
begin
  {$warnings off} // using deprecated function, should be internal in this unit
  FCurrent := UTF8CharacterToUnicode(TextPtr, CharLen);
  {$warnings on}
  Result := (FCurrent > 0) and (CharLen > 0);
  if Result then
    Inc(TextPtr, CharLen);
end;

{$else}

procedure TCastleStringIterator.Start(const S: String);
begin
  TextIndex := 1;
  TextCopy := S;
  TextLength := Length(S);
end;

function TCastleStringIterator.GetNext: Boolean;
var
  NextTextIndex: Integer;
begin
  Result := TextIndex <= TextLength;
  if Result then
  begin
    FCurrent := UnicodeStringNextChar(TextCopy, TextIndex, NextTextIndex);
    TextIndex := NextTextIndex;
  end;
end;

{$endif}

end.
