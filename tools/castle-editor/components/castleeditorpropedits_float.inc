{%MainUnit castleeditorpropedits.pas}
{
  Copyright 2024-2024 Trung Le (Kagamma).

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TCastleFloatPropertyEditor ------------------------------------------------- }

{$ifdef read_interface}

type
  TCastleFloatPropertyEditor = class(TFloatPropertyEditor)
  public
    function FormatValue(const AValue: Extended): String;
    function GetDefaultValue: AnsiString; override;
    function GetValue: AnsiString; override;
  end;
{$endif read_interface}

{$ifdef read_implementation}

function TCastleFloatPropertyEditor.FormatValue(const AValue: Extended): String;
begin
  Result := FloatToStrDisplay(AValue, 5);
end;

function TCastleFloatPropertyEditor.GetDefaultValue: AnsiString;
begin
  if not HasDefaultValue then
    raise EPropertyError.Create('No property default available');
  Result := FormatValue(0);
end;

function TCastleFloatPropertyEditor.GetValue: AnsiString;
begin
  Result := FormatValue(GetFloatValue);
end;

{$endif read_implementation}