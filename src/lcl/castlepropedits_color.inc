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
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    function GetName: shortstring; override;
    function GetValue: String; override;
    procedure SetValue(const NewValue: String); override;
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
      // This makes changing TCastleUserInterface.BorderColor changing more natural.
      if Color.W = 0 then
        Color.W := 1;
      ColorPersistent.Value := Color;
      Modified;
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

procedure TCastleColorPropertyEditor.SetValue(const NewValue: String);
var
  ColorPersistent: TCastleColorPersistent;
begin
  ColorPersistent := (GetObjectValue as TCastleColorPersistent);
  ColorPersistent.Value := HexToColor(NewValue);
  Modified;
end;

{ TCastleColorRGBPropertyEditor ------------------------------------------------- }

type
  TCastleColorRGBPropertyEditor = class(TSubPropertiesEditor)
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    function GetName: shortstring; override;
    function GetValue: String; override;
    procedure SetValue(const NewValue: String); override;
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
      ColorPersistent.Value := Color;
      Modified;
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

procedure TCastleColorRGBPropertyEditor.SetValue(const NewValue: String);
var
  ColorPersistent: TCastleColorRGBPersistent;
begin
  ColorPersistent := (GetObjectValue as TCastleColorRGBPersistent);
  ColorPersistent.Value := HexToColorRGB(NewValue);
  Modified;
end;
