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

{ Main view, where most of the application logic takes place. }
unit GameViewMain;

interface

uses Classes,
  CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleScene, CastleTransform, CastleViewport;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    TransformMoving: TCastleTransform;
    SceneMovingBox: TCastleScene;
    SceneMovingSphere: TCastleScene;
    SceneMovingRay: TCastleScene;
    ButtonTestMove: TCastleButton;
    ButtonTestBox: TCastleButton;
    ButtonTestSphere: TCastleButton;
    ButtonTestRay: TCastleButton;
    ButtonTestPhysicsRay: TCastleButton;
    MainViewport: TCastleViewport;
  private
    type
      TTestMode = (tmMove, tmBox, tmSphere, tmRay, tmPhysicsRay);
    var
      FTestMode: TTestMode;
    procedure SetTestMode(const Value: TTestMode);
    { Update SceneMovingXxx material color, to show whether it collides,
      following current FTestMode. }
    procedure UpdateCollision;
    procedure ClickTestMove(Sender: TObject);
    procedure ClickTestBox(Sender: TObject);
    procedure ClickTestSphere(Sender: TObject);
    procedure ClickTestRay(Sender: TObject);
    procedure ClickTestPhysicsRay(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  X3DNodes, CastleVectors, CastleBoxes;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;

  ButtonTestMove.OnClick := {$ifdef FPC}@{$endif} ClickTestMove;
  ButtonTestBox.OnClick := {$ifdef FPC}@{$endif} ClickTestBox;
  ButtonTestSphere.OnClick := {$ifdef FPC}@{$endif} ClickTestSphere;
  ButtonTestRay.OnClick := {$ifdef FPC}@{$endif} ClickTestRay;
  ButtonTestPhysicsRay.OnClick := {$ifdef FPC}@{$endif} ClickTestPhysicsRay;

  SetTestMode(tmMove);
end;

procedure TViewMain.SetTestMode(const Value: TTestMode);
begin
  FTestMode := Value;

  ButtonTestMove.Pressed := FTestMode = tmMove;
  ButtonTestBox.Pressed := FTestMode = tmBox;
  ButtonTestSphere.Pressed := FTestMode = tmSphere;
  ButtonTestRay.Pressed := FTestMode = tmRay;
  ButtonTestPhysicsRay.Pressed := FTestMode = tmPhysicsRay;

  SceneMovingBox.Exists := FTestMode = tmBox;
  SceneMovingSphere.Exists := FTestMode in [tmMove, tmSphere];
  SceneMovingRay.Exists := FTestMode in [tmRay, tmPhysicsRay];

  UpdateCollision;
end;

procedure TViewMain.UpdateCollision;

  procedure ShowCollision(const Collides: Boolean);
  var
    Appearance: TAppearanceNode;
    Mat: TPhysicalMaterialNode;
  begin
    case FTestMode of
      tmMove, tmSphere:
        Appearance := SceneMovingSphere.Node('MainMaterial') as TAppearanceNode;
      tmBox:
        Appearance := SceneMovingBox.Node('MainMaterial') as TAppearanceNode;
      tmRay, tmPhysicsRay:
        Appearance := SceneMovingRay.Node('MainMaterial') as TAppearanceNode;
    end;
    { Here we simply assume that material is TPhysicalMaterialNode, which means it is a PBR
      material as e.g. designed in Blender and exported to glTF. }
    Mat := Appearance.Material as TPhysicalMaterialNode;
    if Collides then
      Mat.BaseColor := Vector3(0.9, 0.1, 0.1) // reddish
    else
      Mat.BaseColor := Vector3(0.9, 0.9, 0.9); // almost white
  end;

var
  Box: TBox3D;
  SavedExists: Boolean;
begin
  { Make TransformMoving temporarily non-existing,
    so that WorldXxxCollision call doesn't detect collisions with TransformMoving
    (so it only detects them with SceneLevel). }
  SavedExists := TransformMoving.Exists;
  TransformMoving.Exists := false;

  case FTestMode of
    tmMove, tmSphere:
      begin
        ShowCollision(MainViewport.Items.WorldSphereCollision(
          TransformMoving.Translation, 0.5));
      end;
    tmBox:
      begin
        Box := Box3D(
          Vector3(-0.5, -0.5, -0.5),
          Vector3( 0.5,  0.5,  0.5)
        ).Translate(TransformMoving.Translation);
        ShowCollision(MainViewport.Items.WorldBoxCollision(Box));
      end;
    tmRay:
      begin
        ShowCollision(MainViewport.Items.WorldRayCast(
          TransformMoving.Translation, Vector3(0, 0, -1)) <> nil);
      end;
    tmPhysicsRay:
      begin
        ShowCollision(MainViewport.Items.PhysicsRayCast(
          TransformMoving.Translation, Vector3(0, 0, -1)).Transform <> nil);
      end;
  end;

  TransformMoving.Exists := SavedExists;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
const
  MoveSpeed = 4.0;
var
  MoveVector: TVector3;
begin
  inherited;
  { This virtual method is executed every frame.}
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  MoveVector := TVector3.Zero;
  if Container.Pressed.Keys[KeyA] then
    MoveVector := MoveVector + Vector3(-1, 0, 0) * MoveSpeed * SecondsPassed;
  if Container.Pressed.Keys[KeyD] then
    MoveVector := MoveVector + Vector3( 1, 0, 0) * MoveSpeed * SecondsPassed;
  if Container.Pressed.Keys[KeyW] then
    MoveVector := MoveVector + Vector3(0, 0, -1) * MoveSpeed * SecondsPassed;
  if Container.Pressed.Keys[KeyS] then
    MoveVector := MoveVector + Vector3(0, 0,  1) * MoveSpeed * SecondsPassed;
  if Container.Pressed.Keys[KeyE] then
    MoveVector := MoveVector + Vector3(0,  1, 0) * MoveSpeed * SecondsPassed;
  if Container.Pressed.Keys[KeyQ] then
    MoveVector := MoveVector + Vector3(0, -1, 0) * MoveSpeed * SecondsPassed;

  if not MoveVector.IsPerfectlyZero then
  begin
    if FTestMode = tmMove then
    begin
      // change TransformMoving.Translation only if possible
      TransformMoving.Move(MoveVector, false)
    end else
    begin
      // unconditionally change TransformMoving.Translation
      TransformMoving.Translation := TransformMoving.Translation + MoveVector;
      UpdateCollision;
    end;
  end;
end;

procedure TViewMain.ClickTestMove(Sender: TObject);
begin
  SetTestMode(tmMove);
end;

procedure TViewMain.ClickTestBox(Sender: TObject);
begin
  SetTestMode(tmBox);
end;

procedure TViewMain.ClickTestSphere(Sender: TObject);
begin
  SetTestMode(tmSphere);
end;

procedure TViewMain.ClickTestRay(Sender: TObject);
begin
  SetTestMode(tmRay);
end;

procedure TViewMain.ClickTestPhysicsRay(Sender: TObject);
begin
  SetTestMode(tmPhysicsRay);
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys
end;

end.
