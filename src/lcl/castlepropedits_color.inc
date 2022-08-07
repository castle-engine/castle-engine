{%MainUnit castlepropedits.pas}
{
  Copyright 2010-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Color property editors. }

{ TCastleColorPropertyEditor ------------------------------------------------- }

type
  TCastleColorPropertyEditor = class(TSubPropertiesEditor)
  strict private
    procedure SetAllValues(const NewValue: TCastleColor);
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    function GetName: shortstring; override;
    function GetValue: String; override;
    procedure SetValue(const NewValue: String); override;
    function GetDefaultValue: String; override;
    function ValueIsStreamed: boolean; override;
    function AllEqual: Boolean; override;
  end;

function TCastleColorPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paRevertable]
    // our SetValue allows to set color from string
    - [paReadOnly];
end;

procedure TCastleColorPropertyEditor.Edit;
var
  Dialog: TColorDialog;
  ColorPersistent: TCastleColorPersistent;
  Color: TCastleColor;
  ColorByte: TVector3Byte;
begin
  Dialog := TColorDialog.Create(nil);
  try
    ColorPersistent := (GetObjectValue as TCastleColorPersistent);
    Color := ColorPersistent.Value;
    ColorByte := Vector3Byte(Color.XYZ); // edit only Color RGB
    Dialog.Color := RGBToColor(ColorByte[0], ColorByte[1], ColorByte[2]);
    if Dialog.Execute then
    begin
      RedGreenBlue(Dialog.Color, ColorByte.X, ColorByte.Y, ColorByte.Z);
      // keep Color alpha unchanged ...
      Color := Vector4(Vector3(ColorByte), Color.W);
      // ... unless it is zero, then change to 1.
      { This makes changing TCastleUserInterface.BorderColor changing more natural,
        setting it using dialog turns color from transparent -> opaque. }
      if Color.W = 0 then
        Color.W := 1;
      SetAllValues(Color);
    end;
  finally FreeAndNil(Dialog) end;
end;

function TCastleColorPropertyEditor.GetName: shortstring;
begin
  Result := inherited GetName;
  Result := SuffixRemove('persistent', Result, true);
end;

function TCastleColorPropertyEditor.GetValue: String;
var
  ColorPersistent: TCastleColorPersistent;
begin
  ColorPersistent := (GetObjectValue as TCastleColorPersistent);
  Result := ColorToHex(ColorPersistent.Value);
end;

procedure TCastleColorPropertyEditor.SetAllValues(const NewValue: TCastleColor);
var
  I: Integer;
  ColorPersistent: TCastleColorPersistent;
begin
  for I := 0 to PropCount - 1 do
  begin
    ColorPersistent := GetObjectValueAt(I) as TCastleColorPersistent;
    ColorPersistent.Value := NewValue;
  end;
  Modified;
end;

procedure TCastleColorPropertyEditor.SetValue(const NewValue: String);
begin
  SetAllValues(HexToColor(NewValue));
end;

function TCastleColorPropertyEditor.GetDefaultValue: String;
var
  ColorPersistent: TCastleColorPersistent;
begin
  ColorPersistent := (GetObjectValue as TCastleColorPersistent);
  Result := ColorPersistent.InternalDefaultValue.ToString;
end;

function TCastleColorPropertyEditor.ValueIsStreamed: boolean;
var
  ColorPersistent: TCastleColorPersistent;
begin
  ColorPersistent := (GetObjectValue as TCastleColorPersistent);
  Result := not ColorPersistent.HasDefaultValue;
end;

function TCastleColorPropertyEditor.AllEqual: Boolean;
var
  I: Integer;
  Value1, Value2: TCastleColor;
begin
  if PropCount > 1 then
  begin
    Value1 := (GetObjectValueAt(0) as TCastleColorPersistent).Value;
    for I := 1 to PropCount - 1 do
    begin
      Value2 := (GetObjectValueAt(I) as TCastleColorPersistent).Value;
      if not TCastleColor.PerfectlyEquals(Value1, Value2) then
        Exit(false);
    end;
  end;

  Result := true;
end;

{ TCastleColorRGBPropertyEditor ------------------------------------------------- }

type
  TCastleColorRGBPropertyEditor = class(TSubPropertiesEditor)
  strict private
    procedure SetAllValues(const NewValue: TCastleColorRGB);
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    function GetName: shortstring; override;
    function GetValue: String; override;
    procedure SetValue(const NewValue: String); override;
    function GetDefaultValue: String; override;
    function ValueIsStreamed: boolean; override;
    function AllEqual: Boolean; override;
  end;

function TCastleColorRGBPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paRevertable]
    // our SetValue allows to set color from string
    - [paReadOnly];
end;

procedure TCastleColorRGBPropertyEditor.Edit;
var
  Dialog: TColorDialog;
  ColorPersistent: TCastleColorRGBPersistent;
  Color: TCastleColorRGB;
  ColorByte: TVector3Byte;
begin
  Dialog := TColorDialog.Create(nil);
  try
    ColorPersistent := (GetObjectValue as TCastleColorRGBPersistent);
    Color := ColorPersistent.Value;
    ColorByte := Vector3Byte(Color);
    Dialog.Color := RGBToColor(ColorByte[0], ColorByte[1], ColorByte[2]);
    if Dialog.Execute then
    begin
      RedGreenBlue(Dialog.Color, ColorByte.X, ColorByte.Y, ColorByte.Z);
      Color := Vector3(ColorByte);
      SetAllValues(Color);
    end;
  finally FreeAndNil(Dialog) end;
end;

function TCastleColorRGBPropertyEditor.GetName: shortstring;
begin
  Result := inherited GetName;
  Result := SuffixRemove('persistent', Result, true);
end;

function TCastleColorRGBPropertyEditor.GetValue: String;
var
  ColorPersistent: TCastleColorRGBPersistent;
begin
  ColorPersistent := (GetObjectValue as TCastleColorRGBPersistent);
  Result := ColorRGBToHex(ColorPersistent.Value);
end;

procedure TCastleColorRGBPropertyEditor.SetAllValues(const NewValue: TCastleColorRGB);
var
  I: Integer;
  ColorPersistent: TCastleColorRGBPersistent;
begin
  for I := 0 to PropCount - 1 do
  begin
    ColorPersistent := GetObjectValueAt(I) as TCastleColorRGBPersistent;
    ColorPersistent.Value := NewValue;
  end;
  Modified;
end;

procedure TCastleColorRGBPropertyEditor.SetValue(const NewValue: String);
begin
  SetAllValues(HexToColorRGB(NewValue));
end;

function TCastleColorRGBPropertyEditor.GetDefaultValue: String;
var
  ColorPersistent: TCastleColorRGBPersistent;
begin
  ColorPersistent := (GetObjectValue as TCastleColorRGBPersistent);
  Result := ColorPersistent.InternalDefaultValue.ToString;
end;

function TCastleColorRGBPropertyEditor.ValueIsStreamed: boolean;
var
  ColorPersistent: TCastleColorRGBPersistent;
begin
  ColorPersistent := (GetObjectValue as TCastleColorRGBPersistent);
  Result := not ColorPersistent.HasDefaultValue;
end;

function TCastleColorRGBPropertyEditor.AllEqual: Boolean;
var
  I: Integer;
  Value1, Value2: TCastleColorRGB;
begin
  if PropCount > 1 then
  begin
    Value1 := (GetObjectValueAt(0) as TCastleColorRGBPersistent).Value;
    for I := 1 to PropCount - 1 do
    begin
      Value2 := (GetObjectValueAt(I) as TCastleColorRGBPersistent).Value;
      if not TCastleColorRGB.PerfectlyEquals(Value1, Value2) then
        Exit(false);
    end;
  end;

  Result := true;
end;
