{%MainUnit castleeditorpropedits.pas}
{
  Copyright 2022-2022 Michalis Kamburelis, Andrzej Kilijanski

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
type

  { Property editor for layer collisions }
  TPhysicsLayerCollisionsPropertyEditor = class(TSubPropertiesEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TPhysicsLayerNamesPropertyEditor = class(TSubPropertiesEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  { Property editor for choosing physics layer in rigid body }
  TPhysicsLayerPropertyEditor = class(TOrdinalPropertyEditor)
  strict private
    function GetLayerName(OrdValue: LongInt): String;
  public
    function GetAttributes: TPropertyAttributes; override;
    function OrdValueToVisualValue(OrdValue: LongInt): string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const NewValue: ansistring); override;
  end;
{$endif read_interface}

{$ifdef read_implementation}
{ TPhysicsLayerNamesPropertyEditor ------------------------------------------ }

function TPhysicsLayerNamesPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paRevertable] - [paSubProperties];
end;

procedure TPhysicsLayerNamesPropertyEditor.Edit;
var
  LayerNamesForm: TPhysicsLayerNamesPropertyEditorForm;
  LayerNames: TCastleLayerNames;
begin
  LayerNames := GetObjectValue as TCastleLayerNames;
  if LayerNames = nil then
    Exit;

  LayerNamesForm := TPhysicsLayerNamesPropertyEditorForm.Create(nil);
  try
    LayerNamesForm.Init(LayerNames);
    if LayerNamesForm.ShowModal = mrOK then
      Modified;
  finally
    FreeAndNil(LayerNamesForm);
  end;
end;

{ TLayerCollisionsPropertyEditor --------------------------------------------- }

function TPhysicsLayerCollisionsPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paRevertable] - [paSubProperties];
end;

procedure TPhysicsLayerCollisionsPropertyEditor.Edit;
var
  LayerCollisionsForm: TLayerCollisionsPropertyEditorForm;
  LayerCollisions: TCastleLayerCollisions;
begin
  LayerCollisions := GetObjectValue as TCastleLayerCollisions;
  if LayerCollisions = nil then
    Exit;

  LayerCollisionsForm := TLayerCollisionsPropertyEditorForm.Create(nil);
  try
    LayerCollisionsForm.Init(LayerCollisions);
    if LayerCollisionsForm.ShowModal = mrOK then
      Modified;
  finally
    FreeAndNil(LayerCollisionsForm);
  end;
end;

{ TPhysicsLayerPropertyEditor ------------------------------------------------ }

function TPhysicsLayerPropertyEditor.GetLayerName(OrdValue: LongInt): String;
var
  RigidBody: TCastleRigidBody;
  Transform: TCastleTransform;
begin
  { We assume that Layer property is in TCastleRigidBody }
  if (GetInstProp^.Instance = nil) then
    Exit('');

  if not (GetInstProp^.Instance is TCastleRigidBody) then
    Exit('');

  RigidBody := GetInstProp^.Instance as TCastleRigidBody;
  Transform := RigidBody.Parent;

  if (Transform = nil) or (Transform.World = nil) then
    Exit('');

  if High(TPhysicsLayer) < OrdValue then
    Exit('');

  Result := Transform.World.PhysicsProperties.LayerNames.Names[TPhysicsLayer(OrdValue)];
end;

function TPhysicsLayerPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := (inherited GetAttributes) + [paValueList];
end;

function TPhysicsLayerPropertyEditor.OrdValueToVisualValue(OrdValue: LongInt): string;
var
  L: Longint;
begin
  L := OrdValue;
  Result := IntToStr(L) + ': ' + GetLayerName(L);
end;

procedure TPhysicsLayerPropertyEditor.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := Low(TPhysicsLayer) to High(TPhysicsLayer) do
    Proc(IntToStr(I) + ': ' + GetLayerName(I));
end;

procedure TPhysicsLayerPropertyEditor.SetValue(const NewValue: AnsiString);
var
  ColonPos: Integer;
  StringValue: AnsiString;
  IntValue: Integer;
begin
  ColonPos := Pos(':', NewValue);
  if ColonPos < 2 then
    StringValue := NewValue
  else
    StringValue := Copy(NewValue, 1, ColonPos -1);

  if TryStrToInt(StringValue, IntValue) then
    SetOrdValue(IntValue);
end;

{$endif read_implementation}

