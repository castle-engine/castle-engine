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

implementation

uses SysUtils;

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

end.