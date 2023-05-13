{
  Copyright 2022-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main view, where most of the application logic takes place. }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleScene, CastleTransform,
  X3DNodes;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps,
      LabelAddForceAtPosition,
      LabelAddForce,
      LabelAddTorque,
      LabelApplyImpulse: TCastleLabel;
    SceneArrow: TCastleScene;
    DynamicBodies: TCastleTransform;
    SceneVisualizeVelocities: TCastleScene;
    CheckboxVisualizeVelocities: TCastleCheckbox;
  private
    RigidBodies: TCastleRigidBodyList;
    VisualizeVelocitiesAngularLines: TCoordinateNode;
    VisualizeVelocitiesLinearLines: TCoordinateNode;
    procedure AddForceAtPosition;
    procedure AddForce;
    procedure AddTorque;
    procedure ApplyImpulse;
    function ForceScale: Single;
    procedure CreateVisualizeVelocitiesNodes;
    procedure UpdateVisualizeVelocities;
    procedure ChangeVisualizeVelocities(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils, Math,
  CastleColors, CastleUtils;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
var
  T: TCastleTransform;
  RBody: TCastleRigidBody;
begin
  inherited;

  RigidBodies := TCastleRigidBodyList.Create;
  for T in DynamicBodies do
  begin
    RBody := T.FindBehavior(TCastleRigidBody) as TCastleRigidBody;
    if RBody <> nil then
      RigidBodies.Add(RBody);
  end;

  CreateVisualizeVelocitiesNodes;
  CheckboxVisualizeVelocities.OnChange := {$ifdef FPC}@{$endif} ChangeVisualizeVelocities;
end;

procedure TViewMain.Stop;
begin
  FreeAndNil(RigidBodies);
  inherited;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);

  procedure ColorLabel(const Lab: TCastleLabel; const Active: Boolean);
  begin
    if Active then
      Lab.Color := Blue
    else
      Lab.Color := White;
  end;

const
  MoveSpeed = 10;
  ScaleIncrease = 2;
  RotationSpeed = 10;
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  { Transform SceneArrow by keys }

  if Container.Pressed[keyW] then
    SceneArrow.Translation := SceneArrow.Translation + Vector3(0, 0, -1) * SecondsPassed * MoveSpeed;
  if Container.Pressed[keyS] then
    SceneArrow.Translation := SceneArrow.Translation + Vector3(0, 0,  1) * SecondsPassed * MoveSpeed;
  if Container.Pressed[keyA] then
    SceneArrow.Translation := SceneArrow.Translation + Vector3(-1, 0, 0) * SecondsPassed * MoveSpeed;
  if Container.Pressed[keyD] then
    SceneArrow.Translation := SceneArrow.Translation + Vector3( 1, 0, 0) * SecondsPassed * MoveSpeed;

  if Container.Pressed[keyQ] then
    SceneArrow.Scale := SceneArrow.Scale * Vector3(1, 1, Power(ScaleIncrease, SecondsPassed));
  if Container.Pressed[keyE] then
    SceneArrow.Scale := SceneArrow.Scale * Vector3(1, 1, Power(1 / ScaleIncrease, SecondsPassed));

  if Container.Pressed[keyZ] then
    SceneArrow.Rotation := Vector4(0, 1, 0, SceneArrow.Rotation.W + SecondsPassed * RotationSpeed);
  if Container.Pressed[keyC] then
    SceneArrow.Rotation := Vector4(0, 1, 0, SceneArrow.Rotation.W - SecondsPassed * RotationSpeed);

  if Container.Pressed[key7] then
    AddForceAtPosition;
  if Container.Pressed[key8] then
    AddForce;
  if Container.Pressed[key9] then
    AddTorque;

  ColorLabel(LabelAddForceAtPosition, Container.Pressed[key7]);
  ColorLabel(LabelAddForce, Container.Pressed[key8]);
  ColorLabel(LabelAddTorque, Container.Pressed[key9]);
  ColorLabel(LabelApplyImpulse, Container.Pressed[key0]);

  if SceneVisualizeVelocities.Exists then
    UpdateVisualizeVelocities;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsKey(key0) then
  begin
    ApplyImpulse;
    Exit(true);
  end;
end;

function TViewMain.ForceScale: Single;
begin
  Result := SceneArrow.Scale.Z * 1;
end;

procedure TViewMain.AddForceAtPosition;
var
  RBody: TCastleRigidBody;
begin
  for RBody in RigidBodies do
    RBody.AddForceAtPosition(SceneArrow.Direction * ForceScale, SceneArrow.Translation);
end;

procedure TViewMain.AddForce;
var
  RBody: TCastleRigidBody;
begin
  for RBody in RigidBodies do
    RBody.AddForce(SceneArrow.Direction * ForceScale, false);
end;

procedure TViewMain.AddTorque;
var
  RBody: TCastleRigidBody;
begin
  for RBody in RigidBodies do
    RBody.AddTorque(SceneArrow.Direction * ForceScale);
end;

procedure TViewMain.ApplyImpulse;
var
  RBody: TCastleRigidBody;
begin
  for RBody in RigidBodies do
    RBody.ApplyImpulse(SceneArrow.Direction * ForceScale, SceneArrow.Translation);
end;

procedure TViewMain.ChangeVisualizeVelocities(Sender: TObject);
begin
  SceneVisualizeVelocities.Exists := CheckboxVisualizeVelocities.Checked;
end;

procedure TViewMain.CreateVisualizeVelocitiesNodes;
var
  LineSet: TLineSetNode;
  Appearance: TAppearanceNode;
  Material: TUnlitMaterialNode;
  Shape: TShapeNode;
  RootNode: TX3DRootNode;
begin
  RootNode := TX3DRootNode.Create;

  { create visualization for angular velocities }

  Material := TUnlitMaterialNode.Create;
  Material.EmissiveColor := YellowRGB;

  Appearance := TAppearanceNode.Create;
  Appearance.Material := Material;

  LineSet := TLineSetNode.CreateWithShape(Shape);
  LineSet.Mode := lmPair; // each pair of vertexes on LineSet.Coord forms a line segment
  Shape.Appearance := Appearance;
  RootNode.AddChildren(Shape);

  VisualizeVelocitiesAngularLines := TCoordinateNode.Create;
  LineSet.Coord := VisualizeVelocitiesAngularLines;

  { create visualization for linear velocities }

  Material := TUnlitMaterialNode.Create;
  Material.EmissiveColor := RedRGB;

  Appearance := TAppearanceNode.Create;
  Appearance.Material := Material;

  LineSet := TLineSetNode.CreateWithShape(Shape);
  LineSet.Mode := lmPair; // each pair of vertexes on LineSet.Coord forms a line segment
  Shape.Appearance := Appearance;
  RootNode.AddChildren(Shape);

  VisualizeVelocitiesLinearLines := TCoordinateNode.Create;
  LineSet.Coord := VisualizeVelocitiesLinearLines;

  UpdateVisualizeVelocities;

  SceneVisualizeVelocities.Load(RootNode, true);
  SceneVisualizeVelocities.Exists := CheckboxVisualizeVelocities.Checked;
end;

procedure TViewMain.UpdateVisualizeVelocities;

  { Make the direction have some minimal and maximum length, to be nicely visible. }
  function VelocityScale(const Dir: TVector3): TVector3;
  var
    L: Single;
  begin
    L := Dir.Length;
    if IsZero(L) then
      Exit(Dir);

    L := MapRangeClamped(L, 0, 10, 2, 5);
    Result := Dir.AdjustToLength(L);
  end;

var
  VertexesLinearVelocity, VertexesAngularVelocity: TVector3List;
  I: Integer;
  RBody: TCastleRigidBody;
  Origin: TVector3;
begin
  VertexesLinearVelocity := VisualizeVelocitiesLinearLines.FdPoint.Items;
  VertexesAngularVelocity := VisualizeVelocitiesAngularLines.FdPoint.Items;

  VertexesLinearVelocity.Count := 2 * RigidBodies.Count;
  VertexesAngularVelocity.Count := 2 * RigidBodies.Count;

  for I := 0 to RigidBodies.Count - 1 do
  begin
    RBody := RigidBodies[I];
    Origin := RBody.Parent.LocalToWorld(TVector3.Zero);
    VertexesLinearVelocity.L[I * 2    ] := Origin;
    VertexesLinearVelocity.L[I * 2 + 1] := Origin + VelocityScale(RBody.LinearVelocity);
    VertexesAngularVelocity.L[I * 2    ] := Origin;
    VertexesAngularVelocity.L[I * 2 + 1] := Origin + VelocityScale(RBody.AngularVelocity);
  end;

  VisualizeVelocitiesLinearLines.FdPoint.Changed;
  VisualizeVelocitiesAngularLines.FdPoint.Changed;
end;

end.
