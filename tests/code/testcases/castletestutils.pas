{
  Copyright 2023-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Useful utilities for tests. }
unit CastleTestUtils;

{$I ../../../src/common_includes/castleconf.inc}

interface

const
  { UTF-8 bytes of the sample text used by our UTF-8 tests.

    The text is "żółć" (a Polish word, 4 characters, all non-ASCII but inside
    BMP) followed by U+1F600 (GRINNING FACE, outside of BMP -- so it needs
    a surrogate pair when expressed in UTF-16, or in JSON \uXXXX escapes).

    We spell out the bytes explicitly, instead of just writing the text
    in this Pascal file, to not depend on the encoding of this source file
    (and on how the compiler interprets a string literal in it).
    So tests comparing with this are not relying on the very conversions
    that they check. }
  SampleTextUtf8Bytes: array [0..11] of Byte = (
    $C5, $BC,           // U+017C LATIN SMALL LETTER Z WITH DOT ABOVE
    $C3, $B3,           // U+00F3 LATIN SMALL LETTER O WITH ACUTE
    $C5, $82,           // U+0142 LATIN SMALL LETTER L WITH STROKE
    $C4, $87,           // U+0107 LATIN SMALL LETTER C WITH ACUTE
    $F0, $9F, $98, $80  // U+1F600 GRINNING FACE
  );

  { Number of Unicode characters (code points) in the sample text.
    Note that this is not the number of bytes (12)
    and not the number of UTF-16 words (6, as the last character
    needs a surrogate pair). }
  SampleTextLength = 5;

{ Sample text (see @link(SampleTextUtf8Bytes)) as an 8-bit string,
  with UTF-8 encoding. }
function SampleTextUtf8: Utf8String;

{ Sample text (see @link(SampleTextUtf8Bytes)) as the default String
  (which means UTF-8 with FPC, UTF-16 with Delphi). }
function SampleText: String;

type
  TSavedLocale = record
    OldDecimalSeparator: Char;
  end;

{ Change global settings to pretend current locale has
  DecimalSeparator = ','.

  This happens on some systems and locales, e.g. Polish Windows or German Windows.
  It causes bugs, as on most locales and systems DecimalSeparator = '.',
  which is also what most code expects because it is what most file formats assume.
  See https://castle-engine.io/coding_conventions#float_string_dot

  This routine allows to test this situation, on any system/locale. }
function FakeLocaleDecimalSeparatorComma: TSavedLocale;

{ Restore global settings changed by FakeLocaleDecimalSeparatorComma. }
procedure RestoreLocaleDecimalSeparatorComma(const Saved: TSavedLocale);

{ Does given String match a regular expression.

  When compiled with FPC, this uses FPC RegExpr unit with TRegExpr class.
  When compiled with Delphi, this uses Delphi RegularExpressions unit with TRegEx record.

  @bold(The FPC and Delphi implementations are not guaranteed to be perfectly compatible.)
  The FPC implementation has also some important fixes between FPC 3.2.0 and 3.2.2
  (see our auto-tests in TTestCastleStringUtils.TestRegexpMatches).
  Using this routine is only safe for the subset of regular expressions that are compatible
  between FPC and Delphi implementations.

  Simple things, like +, *, ranges like [0-9] and [\d] are compatible,
  so in many practical cases this is acceptable.
  But in general, if your application needs to support both FPC and Delphi,
  be sure to double-test that the regular expressions you use
  are interpreted the same by both FPC and Delphi.

  It is possible we will use some consistent regular expression library in the future
  (e.g. FPC RegExpr should be compatible with Delphi too) to avoid this issue.
  Fow now, regexps are not very important for CGE code (or typical games using CGE),
  so this isn't a critical issue. }
function StringMatchesRegexp(const S, RegexpPattern: String): Boolean;

implementation

uses SysUtils, {$ifdef FPC} Regexpr {$else} RegularExpressions, Character {$endif};

function SampleTextUtf8: Utf8String;
begin
  SetLength(Result, Length(SampleTextUtf8Bytes));
  { Note: Copy the bytes, do not use any conversion, as the bytes
    in SampleTextUtf8Bytes are already UTF-8. }
  Move(SampleTextUtf8Bytes[0], Result[1], Length(SampleTextUtf8Bytes));
end;

function SampleText: String;
begin
  { Converts UTF-8 -> String. Does nothing when String is 8-bit with UTF-8
    (FPC), converts to UTF-16 when String is 16-bit (Delphi). }
  Result := SampleTextUtf8;
end;

function FakeLocaleDecimalSeparatorComma: TSavedLocale;
begin
  Result.OldDecimalSeparator :=
    {$ifdef FPC}DefaultFormatSettings{$else}FormatSettings{$endif}.DecimalSeparator;
  {$ifdef FPC}DefaultFormatSettings{$else}FormatSettings{$endif}.DecimalSeparator := ',';
end;

procedure RestoreLocaleDecimalSeparatorComma(const Saved: TSavedLocale);
begin
  {$ifdef FPC}DefaultFormatSettings{$else}FormatSettings{$endif}.DecimalSeparator :=
    Saved.OldDecimalSeparator;
end;

function StringMatchesRegexp(const S, RegexpPattern: String): Boolean;
{$ifdef FPC}
(*
var
  R:  TRegExpr;
begin
  R := TRegExpr.Create;
  try
    R.Expression := RegexpPattern;
    Result := R.Exec(S);
  finally FreeAndNil(R) end;
*)
// Simpler:
begin
  Result := ExecRegExpr(RegexpPattern, S);
{$else}
begin
  Result := TRegEx.IsMatch(S, RegexpPattern);
{$endif}
end;

end.