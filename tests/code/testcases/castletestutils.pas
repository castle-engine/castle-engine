{
  Copyright 2023-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Useful utilities for tests. }
unit CastleTestUtils;

interface

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