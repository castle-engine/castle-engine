{
  Copyright 2019-2019 Michalis Kamburelis, Andrzej Kilijański.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Implements the game logic. }
unit GameInitialize;

interface

implementation

uses SysUtils, Classes, Generics.Collections,
  CastleWindow, CastleLog, CastleScene, CastleControls, X3DNodes, CastleTransform,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse, CastleColors,
  CastleCameras, CastleVectors, CastleRenderer, CastleBoxes, Castle2DSceneManager,
  CastleUIControls, CastleTimeUtils, CastleUtils, CastleApplicationProperties;

type
  TWall = class;
  TPlane = class;

{ Global variables ----------------------------------------------------------- }

var
  Window: TCastleWindowBase;
  SceneManager: TCastle2DSceneManager;
  Status: TCastleLabel;
  Plane: TPlane;
  LeftWall: TWall;
  RightWall: TWall;
  TopWall: TWall;
  BottomWall: TWall;

type

  TWall = class(TCastleScene)
  public
    constructor Create(AOwner: TComponent; const WallName: TComponentName; const Pos, Size, Color: TVector3); reintroduce;
  end;

  TPlane = class(TCastleScene)
  public
    LastCollisionEnter: string;
    procedure OnCollisionEnter(const CollisionDetails: TPhysicsCollisionDetails);
    constructor Create(AOwner: TComponent); override;
  end;

{ TPlane }

procedure TPlane.OnCollisionEnter(const CollisionDetails: TPhysicsCollisionDetails);
begin
  if CollisionDetails.OtherTransform is TWall then
    LastCollisionEnter := TWall(CollisionDetails.OtherTransform).Name
  else
    LastCollisionEnter := 'other thing';
end;

constructor TPlane.Create(AOwner: TComponent);
var
  RBody: TRigidBody;
  Collider: TBoxCollider;
begin
  inherited Create(AOwner);
  Load('castle-data:/plane.x3d');
  Translation := Vector3(50, 50, 0); // initial position

  RBody := TRigidBody.Create(Plane);
  RBody.Dynamic := true;
  RBody.Animated := true;
  RBody.Setup2D;
  RBody.OnCollisionEnter := @OnCollisionEnter;
  Collider := TBoxCollider.Create(RBody);
  Collider.Size := LocalBoundingBox.Size * 5;

  RigidBody := RBody;
end;

{ TWall }

constructor TWall.Create(AOwner: TComponent; const WallName: TComponentName; const Pos, Size, Color: TVector3);
var
  Box: TBoxNode;
  Shape: TShapeNode;
  Root: TX3DRootNode;
  RBody: TRigidBody;
  Collider: TBoxCollider;
begin
  inherited Create(AOwner);

  Translation := Pos; // initial position
  Name := WallName;

  Box := TBoxNode.CreateWithShape(Shape);
  Box.Size := Size;

  Shape.Appearance := TAppearanceNode.Create;
  Shape.Appearance.Material := TMaterialNode.Create;
  Shape.Appearance.Material.ForcePureEmissive;
  Shape.Appearance.Material.EmissiveColor := Color;

  Root := TX3DRootNode.Create;
  Root.AddChildren(Shape);

  Load(Root, true);

  RBody := TRigidBody.Create(Self);
  RBody.Dynamic := false;
  RBody.Setup2D;

  Collider := TBoxCollider.Create(RBody);
  Collider.Size := Size;

  RigidBody := RBody;
end;

{ ---------------------------------------------------------------------------- }

{ One-time initialization of resources. }
procedure ApplicationInitialize;

  procedure LoadWalls;
  begin
    LeftWall := TWall.Create(Application, 'LeftWall', Vector3(10, 768/2, 0), Vector3(20, 700, 4), Vector3(0.5, 0.5, 1.0));
    SceneManager.Items.Add(LeftWall);

    RightWall := TWall.Create(Application, 'RightWall', Vector3(1014, 768/2, 0), Vector3(20, 700, 4), Vector3(0.5, 0.5, 1.0));
    SceneManager.Items.Add(RightWall);

    TopWall := TWall.Create(Application, 'TopWall', Vector3(1024/2, 758, 0), Vector3(1000, 20, 4), Vector3(0.5, 0.5, 1.0));
    SceneManager.Items.Add(TopWall);

    BottomWall := TWall.Create(Application, 'BottomWall', Vector3(1024/2, 10, 0), Vector3(1000, 20, 4), Vector3(0.5, 0.5, 1.0));
    SceneManager.Items.Add(BottomWall);
  end;

  procedure LoadPlane;
  begin
    Plane := TPlane.Create(Application);
    SceneManager.Items.Add(Plane);
    Plane.Scale := Vector3(5, 5, 0);
  end;

begin
  { make UI automatically scaled }
  Window.Container.UIReferenceWidth := 1024;
  Window.Container.UIReferenceHeight := 768;
  Window.Container.UIScaling := usEncloseReferenceSize;

  SceneManager := TCastle2DSceneManager.Create(Application);
  SceneManager.FullSize := true;
  SceneManager.ProjectionHeight := 768;
  SceneManager.ProjectionAutoSize := false;
  // easy way to make the simulation feel more dynamic
  SceneManager.TimeScale := 1.5;
  Window.Controls.InsertFront(SceneManager);

  LoadWalls;
  LoadPlane;
  SceneManager.NavigationType := ntNone;

  Status := TCastleLabel.Create(Application);
  Status.Anchor(hpLeft, 10);
  Status.Anchor(vpTop, -10);
  Status.Color := Yellow;
  Window.Controls.InsertFront(Status);
end;

procedure WindowUpdate(Container: TUIContainer);
var
  CollisionsList : TCastleTransformList;
  CollisionsListTXT: String;
  I: Integer;
begin
  CollisionsList := Plane.RigidBody.GetCollidingTransforms;
  CollisionsListTXT := 'Colissions: ';

  for I := 0 to CollisionsList.Count - 1 do
  begin
    if CollisionsList[I] is TWall then
       CollisionsListTXT := CollisionsListTXT + ' ' + TWall(CollisionsList[I]).Name
    else
       CollisionsListTXT := CollisionsListTXT + ' other thing';
  end;

  if CollisionsList.Count = 0 then
  begin
    CollisionsListTXT := CollisionsListTXT + 'nothing';
  end;

  Status.Caption := Format(
    'FPS: %s' + LineEnding +
    'Scene Manager Objects: %d' + LineEnding +
    'Right click to add plane velocity.' + LineEnding +
    '%s' + LineEnding +
    'Last plane collision enter: %s.', [
     Container.Fps.ToString,
     SceneManager.Items.Count,
     CollisionsListTXT,
     Plane.LastCollisionEnter
     ]);
end;

procedure WindowPress(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.IsMouseButton(mbRight) then
     Plane.RigidBody.LinearVelocity := Plane.RigidBody.LinearVelocity + Vector3(0, 20, 0);
end;

initialization
  { Set ApplicationName early, as our log uses it. }
  ApplicationProperties.ApplicationName := 'physics_2d_collisions';

  InitializeLog;

  { initialize Application callbacks }
  Application.OnInitialize := @ApplicationInitialize;

  { create Window and initialize Window callbacks }
  Window := TCastleWindowBase.Create(Application);
  Application.MainWindow := Window;
  Window.OnUpdate := @WindowUpdate;
  Window.OnPress := @WindowPress;
end.
