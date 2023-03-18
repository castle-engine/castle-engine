{%MainUnit castleeditorpropedits.pas}
{
  Copyright 2010-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Color property editors and TCastlePropertyEditorHook
  (for now used only by color property editors). }

{ TCastleColorPropertyEditor ------------------------------------------------- }

{$ifdef read_interface}
type
  TCastlePropertyEditorHook = class(TPropertyEditorHook)
  public
    { Used by color picker to specify the position to show the window. }
    InspectorAreaOnScreen: TRect;
  end;

  { Abstract class for grouping all color property editors }
  TCastleAbstractColorPropertyEditor = class(TSubPropertiesEditor)
  end;

  TCastleColorPropertyEditor = class(TCastleAbstractColorPropertyEditor)
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
{$endif read_interface}

{$ifdef read_implementation}

function TCastleColorPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paRevertable]
    // our SetValue allows to set color from string
    - [paReadOnly];
end;

procedure TCastleColorPropertyEditor.Edit;
var
  ColorPersistent: TCastleColorPersistent;
  Color: TCastleColor;
  ColorPicker: TCastleColorPickerForm;
  InspectorArea: TRect;
  CastlePropertyHook: TCastlePropertyEditorHook;
begin
  ColorPicker := TCastleColorPickerForm.Create(nil);
  try
    ColorPersistent := (GetObjectValue as TCastleColorPersistent);
    Color := ColorPersistent.Value;
    ColorPicker.Init(Self, Color);

    if (PropertyHook <> nil) and (PropertyHook is TCastlePropertyEditorHook) then
    begin
      CastlePropertyHook := PropertyHook as TCastlePropertyEditorHook;
      ColorPicker.Position := poDesigned;
      InspectorArea := CastlePropertyHook.InspectorAreaOnScreen;
      ColorPicker.Left := InspectorArea.Left -
        (ColorPicker.Width - InspectorArea.Width);
      if InspectorArea.Height > ColorPicker.Height then
        ColorPicker.Top := InspectorArea.Top +
          Round((InspectorArea.Height - ColorPicker.Height) / 2)
      else
        ColorPicker.Top := InspectorArea.Top;
    end else
      ColorPicker.Position := poOwnerFormCenter;

    ColorPicker.ShowModal;
    SetAllValues(ColorPicker.CurrentCastleColor);
  finally
    FreeAndNil(ColorPicker);
  end;
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
  TCastleColorRGBPropertyEditor = class(TCastleAbstractColorPropertyEditor)
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
  ColorPersistent: TCastleColorRGBPersistent;
  Color: TCastleColorRGB;
  ColorPicker: TCastleColorPickerForm;
  InspectorArea: TRect;
  CastlePropertyHook: TCastlePropertyEditorHook;
begin
  ColorPicker := TCastleColorPickerForm.Create(nil);
  try
    ColorPersistent := (GetObjectValue as TCastleColorRGBPersistent);
    Color := ColorPersistent.Value;
    ColorPicker.Init(Self, Color);

    if (PropertyHook <> nil) and (PropertyHook is TCastlePropertyEditorHook) then
    begin
      CastlePropertyHook := PropertyHook as TCastlePropertyEditorHook;
      ColorPicker.Position := poDefaultPosOnly;
      InspectorArea := CastlePropertyHook.InspectorAreaOnScreen;
      ColorPicker.Left := InspectorArea.Left -
        (ColorPicker.Width - InspectorArea.Width);
      if InspectorArea.Height > ColorPicker.Height then
        ColorPicker.Top := InspectorArea.Top +
          Round((InspectorArea.Height - ColorPicker.Height) / 2)
      else
        ColorPicker.Top := InspectorArea.Top;
    end else
      ColorPicker.Position := poOwnerFormCenter;

    ColorPicker.ShowModal;
    SetAllValues(ColorPicker.CurrentCastleColorRGB);
  finally
    FreeAndNil(ColorPicker);
  end;
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

{$endif read_implementation}
