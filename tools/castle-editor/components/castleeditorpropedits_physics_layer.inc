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

  { Property editor for choosing physics layers in TModularMovement }
  TPhysicsLayersPropertyEditor = class(TSetPropertyEditor)
  strict private
    function GetLayerName(OrdValue: LongInt): String;
  public
    { Use TOnePhysicsLayerPropertyEditor to show nice names for layers when
      expanded in object inspector. }
    procedure GetProperties(Proc: TGetPropEditProc); override;
    { Makes value in object inspector look nice. When only one selected add also
      layer name }
    function OrdValueToVisualValue(OrdValue: LongInt): String; override;
  end;

  { Editor for each layer (row) in TPhysicsLayers }
  TOnePhysicsLayerPropertyEditor = class(TSetElementPropertyEditor)
  protected
    FElementNumber: Integer;
    FElementName: String;
  public
    constructor Create(Parent: TPropertyEditor; AElementNumber: Integer;
      AElementName: String); reintroduce;
    { Item name when we expand TPhysicsLayersPropertyEditor }
    function GetName: ShortString; override;
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


{ TPhysicsLayersPropertyEditor ----------------------------------------------- }

function TPhysicsLayersPropertyEditor.GetLayerName(OrdValue: LongInt): String;
var
  Behavior: TCastleBehavior;
  Transform: TCastleTransform;
begin
  { We assume that Layer property is in TCastleRigidBody }
  if (GetInstProp^.Instance = nil) then
    Exit('');

  if (GetInstProp^.Instance is TCastleBehavior) then
  begin
    Behavior := GetInstProp^.Instance as TCastleBehavior;
    Transform := Behavior.Parent;
  end else
  if (GetInstProp^.Instance is TCastleTransform) then
  begin
    Transform := GetInstProp^.Instance as TCastleTransform;
  end else
    Exit('');

  if (Transform = nil) or (Transform.World = nil) then
    Exit('');

  if High(TPhysicsLayer) < OrdValue then
    Exit('');

  Result := Transform.World.PhysicsProperties.LayerNames.Names[TPhysicsLayer(OrdValue)];
end;

procedure TPhysicsLayersPropertyEditor.GetProperties(Proc: TGetPropEditProc);
var
  I: Integer;
begin
  { Make TOnePhysicsLayerPropertyEditor actually used. }
  with GetTypeData(GetTypeData(GetPropType)^.CompType)^ do
    for I := MinValue to MaxValue do
      Proc(TOnePhysicsLayerPropertyEditor.Create(Self, I, GetLayerName(I)));
end;

function TPhysicsLayersPropertyEditor.OrdValueToVisualValue(OrdValue: LongInt
  ): String;
var
  Layers: TPhysicsLayers;
  Layer: TPhysicsLayer;
  NumberOfCheckedLayers: Integer;
  FirstCheckedLayer: TPhysicsLayer;
begin
  Layers := TPhysicsLayers(OrdValue);
  NumberOfCheckedLayers := 0;
  if Layers = AllLayers then
    Exit('[all]');

  Result := '';
  for Layer := Low(Layer) to High(Layer) do
    if Layer in Layers then
    begin
      Result := SAppendPart(Result, ',', IntToStr(Layer));
      Inc(NumberOfCheckedLayers);
      if NumberOfCheckedLayers = 1 then
        FirstCheckedLayer := Layer;
    end;

  if NumberOfCheckedLayers = 1 then
    Result := '[' + Result + ': ' + GetLayerName(Integer(FirstCheckedLayer)) + ']'
  else
    Result := '[' + Result + ']';
end;

{ TOnePhysicsLayerPropertyEditor --------------------------------------------- }

constructor TOnePhysicsLayerPropertyEditor.Create(Parent: TPropertyEditor;
  AElementNumber: Integer; AElementName: String);
begin
  inherited Create(Parent, AElementNumber);
  FElementNumber := AElementNumber;
  FElementName := AElementName;
end;

function TOnePhysicsLayerPropertyEditor.GetName: ShortString;
begin
  Result := Format('%2d',[FElementNumber]) + ': ' +FElementName;
end;


{$endif read_implementation}

