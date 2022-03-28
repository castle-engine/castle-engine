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

{ Main state, where most of the application logic takes place. }
unit GameStateMain;

interface

uses Classes,
  CastleUIState, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleScene, CastleTransform, CastleViewport;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    type
      TTestMode = (tmMove, tmBox, tmSphere, tmRay);
    var
      { Components designed using CGE editor, loaded from gamestatemain.castle-user-interface. }
      LabelFps: TCastleLabel;
      TransformMoving: TCastleTransform;
      SceneMovingBox: TCastleScene;
      SceneMovingSphere: TCastleScene;
      SceneMovingRay: TCastleScene;
      ButtonTestMove: TCastleButton;
      ButtonTestBox: TCastleButton;
      ButtonTestSphere: TCastleButton;
      ButtonTestRay: TCastleButton;
      MainViewport: TCastleViewport;

      FTestMode: TTestMode;
    procedure SetTestMode(const Value: TTestMode);
    { Update SceneMovingXxx material color, to show whether it collides,
      following current FTestMode. }
    procedure UpdateCollision;
    procedure ClickTestMove(Sender: TObject);
    procedure ClickTestBox(Sender: TObject);
    procedure ClickTestSphere(Sender: TObject);
    procedure ClickTestRay(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils,
  X3DNodes, CastleVectors, CastleBoxes;

{ TStateMain ----------------------------------------------------------------- }

constructor TStateMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemain.castle-user-interface';
end;

procedure TStateMain.Start;
begin
  inherited;

  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
  TransformMoving := DesignedComponent('TransformMoving') as TCastleTransform;
  SceneMovingBox := DesignedComponent('SceneMovingBox') as TCastleScene;
  SceneMovingSphere := DesignedComponent('SceneMovingSphere') as TCastleScene;
  SceneMovingRay := DesignedComponent('SceneMovingRay') as TCastleScene;
  ButtonTestMove := DesignedComponent('ButtonTestMove') as TCastleButton;
  ButtonTestBox := DesignedComponent('ButtonTestBox') as TCastleButton;
  ButtonTestSphere := DesignedComponent('ButtonTestSphere') as TCastleButton;
  ButtonTestRay := DesignedComponent('ButtonTestRay') as TCastleButton;
  MainViewport := DesignedComponent('MainViewport') as TCastleViewport;

  ButtonTestMove.OnClick := {$ifdef FPC}@{$endif}ClickTestMove;
  ButtonTestBox.OnClick := {$ifdef FPC}@{$endif}ClickTestBox;
  ButtonTestSphere.OnClick := {$ifdef FPC}@{$endif}ClickTestSphere;
  ButtonTestRay.OnClick := {$ifdef FPC}@{$endif}ClickTestRay;

  SetTestMode(tmMove);
end;

procedure TStateMain.SetTestMode(const Value: TTestMode);
begin
  FTestMode := Value;

  ButtonTestMove.Pressed := FTestMode = tmMove;
  ButtonTestBox.Pressed := FTestMode = tmBox;
  ButtonTestSphere.Pressed := FTestMode = tmSphere;
  ButtonTestRay.Pressed := FTestMode = tmRay;

  SceneMovingBox.Exists := FTestMode = tmBox;
  SceneMovingSphere.Exists := FTestMode in [tmMove, tmSphere];
  SceneMovingRay.Exists := FTestMode = tmRay;

  UpdateCollision;
end;

procedure TStateMain.UpdateCollision;

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
      tmRay:
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
  end;

  TransformMoving.Exists := SavedExists;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
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

procedure TStateMain.ClickTestMove(Sender: TObject);
begin
  SetTestMode(tmMove);
end;

procedure TStateMain.ClickTestBox(Sender: TObject);
begin
  SetTestMode(tmBox);
end;

procedure TStateMain.ClickTestSphere(Sender: TObject);
begin
  SetTestMode(tmSphere);
end;

procedure TStateMain.ClickTestRay(Sender: TObject);
begin
  SetTestMode(tmRay);
end;

function TStateMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys
end;

end.
