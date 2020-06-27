{%MainUnit castlepropedits.pas}
{
  Copyright 2020-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Improved float property editor. }

type
  { Property editor for floats, improves standard TFloatPropertyEditor. }
  TCastleFloatPropertyEditor = class(TFloatPropertyEditor)
    { Determines whether the object inspector shows
      the current property state as default or not (bold).

      The default implementation is faulty: it compares "GetDefaultValue<>GetVisualValue"
      by GetDefaultValue always returns zero.

      TODO: Submit to fix on LCL. }
    function GetDefaultValue: String; override;
  end;

function TCastleFloatPropertyEditor.GetDefaultValue: String;
var
  PropInfo: PPropInfo;
  DefFloatValue: Extended;
begin
  if not HasDefaultValue then
    raise EPropertyError.Create('No property default available');

  PropInfo := GetPropInfo;
  { Hard-cast Default to Single,
    this is how default value for Single property is stored,
    consistent with HasDefaultValue in CastleComponentSerialize
    which is consistent with other FPC/Lazarus code. }
  DefFloatValue := PSingle(@PropInfo^.Default)^;
  Result := FormatValue(DefFloatValue);
end;
