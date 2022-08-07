{%MainUnit castlepropedits.pas}
{
  Copyright 2020-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Require 2.0.8.

  With older LCL (1.6.4, at the time of FPC 3.0.2) we get
    src/lcl/castlepropedits_float.inc(37,10) Error: (5000) Identifier not found "HasDefaultValue"
    src/lcl/castlepropedits_float.inc(46,13) Error: (5000) Identifier not found "FormatValue"
  Possibly this could be defined for more versions, not only >= 2.0.8
  -- testing needed.
}
{$if LCL_FULLVERSION >= 02000800}
  {$define LCL_HAS_DEFAULT_VALUE}
{$endif}

{ TCastleFloatPropertyEditor ------------------------------------------------- }

type
  { Improved property editor for floats.
    This is used e.g. for TCastleSpotLight.CutOffAngle editing. }
  TCastleFloatPropertyEditor = class(TFloatPropertyEditor)
  public
    {$ifdef LCL_HAS_DEFAULT_VALUE}
    { Determines whether the object inspector shows
      the current property state as default or not (bold).

      The default implementation is faulty: it compares "GetDefaultValue<>GetVisualValue"
      but GetDefaultValue always returns zero.

      TODO: Submit to fix in LCL. }
    function GetDefaultValue: String; override;
    {$endif}

    { In addition to ancestor features (interprets float, with dot or current locale DecimalSeparator)
      this also interprets floats from CastleScript expressions,
      so you can write e.g. "pi / 2". }
    procedure SetValue(const NewValue: String); override;
  end;

{$ifdef LCL_HAS_DEFAULT_VALUE}
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
{$endif}

procedure TCastleFloatPropertyEditor.SetValue(const NewValue: String);
begin
  try
    inherited SetValue(NewValue);
  except
    // if ancestor could not interpret NewValue, try to interpret it as CastleScript expression
    on EConvertError do
      SetFloatValue(ParseConstantFloatExpression(NewValue));
  end;
end;

{ TCastleTagPropertyEditor ------------------------------------------------- }

{$ifdef CPU64}

type
  { Improved property editor for TComponent.Tag (PtrInt, which means Int64 on x86_64). }
  TCastleTagPropertyEditor = class(TInt64PropertyEditor)
  public
    {$ifdef LCL_HAS_DEFAULT_VALUE}
    { Determines whether the object inspector shows
      the current property state as default or not (bold). }
    function GetDefaultValue: String; override;
    {$endif}
  end;

{$ifdef LCL_HAS_DEFAULT_VALUE}
{ We know that Tag has always default 0. }
function TCastleTagPropertyEditor.GetDefaultValue: String;
begin
  Result := IntToStr(0);
end;
{$endif}

{$endif}
