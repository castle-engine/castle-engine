{
  Copyright 2017-2018 Michalis Kamburelis.

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

{ Global variables ----------------------------------------------------------- }

const
  BoxDropInterval = 0.2;
  MissileShootInterval = 0.1;

var
  Window: TCastleWindowCustom;
  SceneManager: TCastle2DSceneManager;
  Status: TCastleLabel;
  Level: TCastleScene;
  Plane: TCastleScene;
  BoxScene, MissileScene: TCastleScene;
  BoxDropTime, MissileShootTime: TTimerResult;

{ TAutoDisappearTransform ---------------------------------------------------------- }

type
  { Descendant of TCastleTransform that is removed from scene as soon
    as it disappears from view. This means we don't waste time
    calculating missiles or boxes that flew outside of the screen. }
  TAutoDisappearTransform = class(TCastleTransform)
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  end;

procedure TAutoDisappearTransform.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
var
  B: TBox3D;
begin
  inherited;
  B := BoundingBox;
  if (B.Min.X > SceneManager.CurrentProjectionWidth) or
     (B.Max.X < 0) or
     (B.Max.Y < 0) then
    RemoveMe := rtRemoveAndFree;
end;

{ ---------------------------------------------------------------------------- }

{ One-time initialization of resources. }
procedure ApplicationInitialize;

  procedure LoadLevel;
  var
    RigidBody: TRigidBody;
    Collider: TMeshCollider;
  begin
    Level := TCastleScene.Create(Application);
    Level.Load(ApplicationData('level.x3d'));

    RigidBody := TRigidBody.Create(Level);
    RigidBody.Dynamic := false;
    RigidBody.Setup2D; // not really needed for objects with Dynamic = false

    Collider := TMeshCollider.Create(RigidBody);
    Collider.Scene := Level;

    { assign this only once RigidBody and Collider
      are fully configured, this initializes physics engine }
    Level.RigidBody := RigidBody;

    SceneManager.Items.Add(Level);
    SceneManager.MainScene := Level;
  end;

  procedure LoadPlane;
  var
    RigidBody: TRigidBody;
    Collider: TBoxCollider;
  begin
    Plane := TCastleScene.Create(Application);
    Plane.Load(ApplicationData('plane.x3d'));
    Plane.Translation := Vector3(50, 50, 0); // initial position

    RigidBody := TRigidBody.Create(Plane);
    RigidBody.Dynamic := false;
    RigidBody.Animated := true;
    RigidBody.Setup2D; // not really needed for objects with Dynamic = false

    Collider := TBoxCollider.Create(RigidBody);
    Collider.Size := Plane.LocalBoundingBox.Size;

    { assign this only once RigidBody and Collider
      are fully configured, this initializes physics engine }
    Plane.RigidBody := RigidBody;

    SceneManager.Items.Add(Plane);
  end;

  procedure InitializeBoxScene;
  var
    Box: TBoxNode;
    Shape: TShapeNode;
    Root: TX3DRootNode;
  begin
    BoxScene := TCastleScene.Create(Application);

    Box := TBoxNode.CreateWithShape(Shape);
    Box.Size := Vector3(4, 4, 4);

    Shape.Appearance := TAppearanceNode.Create;
    Shape.Appearance.Material := TMaterialNode.Create;
    Shape.Appearance.Material.ForcePureEmissive;
    Shape.Appearance.Material.EmissiveColor := Vector3(0.5, 0.5, 1.0);

    Root := TX3DRootNode.Create;
    Root.AddChildren(Shape);

    BoxScene.Load(Root, true);
  end;

  procedure InitializeMissileScene;
  var
    Sphere: TSphereNode;
    Shape: TShapeNode;
    Root: TX3DRootNode;
  begin
    MissileScene := TCastleScene.Create(Application);

    Sphere := TSphereNode.CreateWithShape(Shape);
    Sphere.Radius := 1;

    Shape.Appearance := TAppearanceNode.Create;
    Shape.Appearance.Material := TMaterialNode.Create;
    Shape.Appearance.Material.ForcePureEmissive;
    Shape.Appearance.Material.EmissiveColor := Vector3(1, 0, 0);

    Root := TX3DRootNode.Create;
    Root.AddChildren(Shape);

    MissileScene.Load(Root, true);
  end;

begin
  { make UI automatically scaled }
  Window.Container.UIReferenceWidth := 1024;
  Window.Container.UIReferenceHeight := 768;
  Window.Container.UIScaling := usEncloseReferenceSize;

  SceneManager := TCastle2DSceneManager.Create(Application);
  SceneManager.FullSize := true;
  SceneManager.ProjectionHeight := 100; // matches height in Blender
  SceneManager.ProjectionAutoSize := false;
  // easy way to make the simulation feel more dynamic
  SceneManager.TimeScale := 1.5;
  Window.Controls.InsertFront(SceneManager);

  LoadLevel;
  LoadPlane;
  InitializeBoxScene;
  InitializeMissileScene;

  { initialize the times, to pretend that we shot the missile/dropped the box
    *right now*. This avoids shooting the 1st missile at 1st WindowUpdate,
    when SecondsPassed are 0 (not known yet). }
  BoxDropTime := Timer;
  MissileShootTime := Timer;

  SceneManager.NavigationType := ntNone;

  Status := TCastleLabel.Create(Application);
  Status.Anchor(hpLeft, 10);
  Status.Anchor(vpTop, -10);
  Status.Color := Yellow;
  Window.Controls.InsertFront(Status);
end;

procedure WindowUpdate(Container: TUIContainer);

  procedure DropBox;
  var
    RigidBody: TRigidBody;
    Collider: TBoxCollider;
    Transform: TCastleTransform;
  begin
    // stop dropping boxes when too many, it would slow down the game
    if SceneManager.Items.Count >= 50 then
      Exit;

    Transform := TAutoDisappearTransform.Create(Application);
    Transform.Translation := Vector3(100, 110, 0); // initial position
    { we keep reusing the same BoxScene, instead of doing BoxScene.Clone.
      This is an optimization, allowed because BoxScene doesn't change at all inside. }
    Transform.Add(BoxScene);

    RigidBody := TRigidBody.Create(Transform);
    RigidBody.Setup2D;

    Collider := TBoxCollider.Create(RigidBody);
    Collider.Size := BoxScene.BoundingBox.Size;
    Collider.Mass := 10;

    { assign this only once RigidBody and Collider
      are fully configured, this initializes physics engine }
    Transform.RigidBody := RigidBody;

    SceneManager.Items.Add(Transform);
  end;

  procedure ShootMissile;
  var
    RigidBody: TRigidBody;
    Collider: TSphereCollider;
    Transform: TCastleTransform;
  begin
    Transform := TAutoDisappearTransform.Create(Application);
    Transform.Translation := Plane.Translation + Vector3(10, 0, 0);
    { we keep reusing the same MissileScene, instead of doing MissileScene.Clone.
      This is an optimization, allowed because MissileScene doesn't change at all inside. }
    Transform.Add(MissileScene);

    RigidBody := TRigidBody.Create(Transform);
    RigidBody.Setup2D;
    RigidBody.InitialLinearVelocity := Vector3(100, 0, 0);

    Collider := TSphereCollider.Create(RigidBody);
    Collider.Radius := MissileScene.BoundingBox.Size.X / 2;
    Collider.Mass := 10;

    { assign this only once RigidBody and Collider
      are fully configured, this initializes physics engine }
    Transform.RigidBody := RigidBody;

    SceneManager.Items.Add(Transform);
  end;

begin
  Status.Caption := Format(
    'FPS: %s' + LineEnding +
    'Scene Manager Objects: %d' + LineEnding +
    'Click or drag with mouse to move the plane.',
    [Container.Fps.ToString,
     SceneManager.Items.Count]);

  if TimerSeconds(Timer, BoxDropTime) > BoxDropInterval then
  begin
    BoxDropTime := Timer;
    DropBox;
  end;

  if TimerSeconds(Timer, MissileShootTime) > MissileShootInterval then
  begin
    MissileShootTime := Timer;
    ShootMissile;
  end;
end;

procedure UpdatePlanePosition(const EventPosition: TVector2);
begin
  Plane.Translation := Vector3(
    MapRange(EventPosition.X, 0, Window.Width, 0, SceneManager.CurrentProjectionWidth),
    MapRange(EventPosition.Y, 0, Window.Height, 0, SceneManager.CurrentProjectionHeight),
    0);
end;

procedure WindowPress(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.IsMouseButton(mbLeft) then
    UpdatePlanePosition(Event.Position);
end;

procedure WindowMotion(Container: TUIContainer; const Event: TInputMotion);
begin
  if mbLeft in Event.Pressed then
    UpdatePlanePosition(Event.Position);
end;

initialization
  { Set ApplicationName early, as our log uses it. }
  ApplicationProperties.ApplicationName := 'physics_2d_game_sopwith';

  InitializeLog;

  { initialize Application callbacks }
  Application.OnInitialize := @ApplicationInitialize;

  { create Window and initialize Window callbacks }
  Window := TCastleWindowCustom.Create(Application);
  Application.MainWindow := Window;
  Window.OnUpdate := @WindowUpdate;
  Window.OnPress := @WindowPress;
  Window.OnMotion := @WindowMotion;
end.
