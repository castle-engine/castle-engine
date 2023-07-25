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

{ TCastleFloatPropertyEditor ------------------------------------------------- }

type
  { Improved property editor for floats.
    This is used e.g. for TCastleSpotLight.CutOffAngle editing. }
  TCastleFloatPropertyEditor = class(TFloatPropertyEditor)
  protected
    { Used to display value.
      Virtual (unlike ancestor FormatValue) to can be overridden. }
    function FormatValueVirt(const AValue: Extended): String; virtual;
  public
    function GetValue: String; override;

    { Determines whether the object inspector shows
      the current property state as default or not (bold).

      The default implementation is faulty: it compares "GetDefaultValue<>GetVisualValue"
      but GetDefaultValue always returns zero.

      TODO: Submit to fix in LCL. }
    function GetDefaultValue: String; override;

    { In addition to ancestor features (interprets float, with dot or current locale DecimalSeparator)
      this also interprets floats from CastleScript expressions,
      so you can write e.g. "pi / 2". }
    procedure SetValue(const NewValue: String); override;
  end;

  { Property editor for floats in rotation vectors, improves
    standard TCastleFloatPropertyEditor. }
  TCastleFloatRotationPropertyEditor = class(TCastleFloatPropertyEditor)
  protected
    function FormatValueVirt(const AValue: Extended): String; override;
  public
    function GetName: ShortString; override;
    procedure SetValue(const NewValue: String); override;
  end;

{ TCastleFloatPropertyEditor ------------------------------------------------- }

function TCastleFloatPropertyEditor.FormatValueVirt(const AValue: Extended): String;
begin
  { Using FloatToStrDisplay for consistency, so that e.g. expanding
    TCastleTransform.Rotation will show same precision for each row (X, Y...)
    as in 4D vector display. }
  Result := FloatToStrDisplay(AValue);
  //inherited FormatValue(AValue);
end;

function TCastleFloatPropertyEditor.GetValue: String;
begin
  { Like ancestor, but use virtual FormatValueVirt. }
  Result := FormatValueVirt(GetFloatValue);
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
    consistent with PropertyHasDefaultValue in CastleInternalRttiUtils
    which is consistent with other FPC/Lazarus code. }
  DefFloatValue := PSingle(@PropInfo^.Default)^;
  Result := FormatValueVirt(DefFloatValue);
end;

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

{ TCastleFloatRotationPropertyEditor ----------------------------------------- }

function TCastleFloatRotationPropertyEditor.GetName: ShortString;
begin
  Result := inherited GetName;

  {
  if (Result = 'W') and (GetInstProp^.Instance.ClassType = TCastleVector4RotationPersistent) then
    Result := 'Angle (W)';
  }

  if Result = 'W' then
    Result := 'Angle (W)';
end;

function TCastleFloatRotationPropertyEditor.FormatValueVirt(const AValue: Extended): String;
begin
  { Display value as Deg(...).
    CastleScript also accepts Deg(...) as input. }
  Result := inherited FormatValueVirt(RadToDeg(AValue));
  Result := 'deg(' + Result + ')';
end;

procedure TCastleFloatRotationPropertyEditor.SetValue(const NewValue: String);
begin
  inherited SetValue(WrapDegIfNeeded(NewValue));
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
