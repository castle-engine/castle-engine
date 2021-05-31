{
  Copyright 2017-2021 Michalis Kamburelis.

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
  CastleCameras, CastleVectors, CastleBoxes, CastleViewport,
  CastleUIControls, CastleTimeUtils, CastleUtils, CastleApplicationProperties;

{ Global variables ----------------------------------------------------------- }

const
  BoxDropInterval = 0.2;
  MissileShootInterval = 0.1;

var
  Window: TCastleWindowBase;
  Viewport: TCastleViewport;
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
  if (B.Min.X > Viewport.Camera.Orthographic.EffectiveWidth) or
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
    Level.Load('castle-data:/level.x3d');

    RigidBody := TRigidBody.Create(Level);
    RigidBody.Dynamic := false;
    RigidBody.Setup2D; // not really needed for objects with Dynamic = false

    Collider := TMeshCollider.Create(RigidBody);
    Collider.Scene := Level;

    { assign this only once RigidBody and Collider
      are fully configured, this initializes physics engine }
    Level.RigidBody := RigidBody;

    Viewport.Items.Add(Level);
    Viewport.Items.MainScene := Level;
  end;

  procedure LoadPlane;
  var
    RigidBody: TRigidBody;
    Collider: TBoxCollider;
  begin
    Plane := TCastleScene.Create(Application);
    Plane.Load('castle-data:/plane.x3d');
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

    Viewport.Items.Add(Plane);
  end;

  procedure InitializeBoxScene;
  var
    Box: TBoxNode;
    Shape: TShapeNode;
    Material: TUnlitMaterialNode;
    Root: TX3DRootNode;
  begin
    BoxScene := TCastleScene.Create(Application);

    Box := TBoxNode.CreateWithShape(Shape);
    Box.Size := Vector3(4, 4, 4);

    Material := TUnlitMaterialNode.Create;
    Material.EmissiveColor := Vector3(0.5, 0.5, 1.0);
    Shape.Material := Material;

    Root := TX3DRootNode.Create;
    Root.AddChildren(Shape);

    BoxScene.Load(Root, true);
  end;

  procedure InitializeMissileScene;
  var
    Sphere: TSphereNode;
    Shape: TShapeNode;
    Root: TX3DRootNode;
    Material: TUnlitMaterialNode;
  begin
    MissileScene := TCastleScene.Create(Application);

    Sphere := TSphereNode.CreateWithShape(Shape);
    Sphere.Radius := 1;

    Material := TUnlitMaterialNode.Create;
    Material.EmissiveColor := Vector3(1, 0, 0);
    Shape.Material := Material;

    Root := TX3DRootNode.Create;
    Root.AddChildren(Shape);

    MissileScene.Load(Root, true);
  end;

begin
  { make UI automatically scaled }
  Window.Container.UIReferenceWidth := 1024;
  Window.Container.UIReferenceHeight := 768;
  Window.Container.UIScaling := usEncloseReferenceSize;

  Viewport := TCastleViewport.Create(Application);
  Viewport.Setup2D;
  Viewport.FullSize := true;
  Viewport.Camera.Orthographic.Height := 100; // matches height in Blender
  // easy way to make the simulation feel more dynamic
  Viewport.Items.TimeScale := 1.5;
  Window.Controls.InsertFront(Viewport);

  LoadLevel;
  LoadPlane;
  InitializeBoxScene;
  InitializeMissileScene;

  { initialize the times, to pretend that we shot the missile/dropped the box
    *right now*. This avoids shooting the 1st missile at 1st WindowUpdate,
    when SecondsPassed are 0 (not known yet). }
  BoxDropTime := Timer;
  MissileShootTime := Timer;

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
    if Viewport.Items.Count >= 50 then
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

    Viewport.Items.Add(Transform);
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
    RigidBody.LinearVelocity := Vector3(100, 0, 0);

    Collider := TSphereCollider.Create(RigidBody);
    Collider.Radius := MissileScene.BoundingBox.Size.X / 2;
    Collider.Mass := 10;

    { assign this only once RigidBody and Collider
      are fully configured, this initializes physics engine }
    Transform.RigidBody := RigidBody;

    Viewport.Items.Add(Transform);
  end;

begin
  Status.Caption := Format(
    'FPS: %s' + LineEnding +
    'Viewport Objects: %d' + LineEnding +
    'Click or drag with mouse to move the plane.', [
    Container.Fps.ToString,
    Viewport.Items.Count
  ]);

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
  Plane.Translation := Vector3(Viewport.PositionTo2DWorld(EventPosition, true), 0);
end;

procedure WindowPress(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.IsMouseButton(buttonLeft) then
    UpdatePlanePosition(Event.Position);
end;

procedure WindowMotion(Container: TUIContainer; const Event: TInputMotion);
begin
  if buttonLeft in Event.Pressed then
    UpdatePlanePosition(Event.Position);
end;

initialization
  { initialize Application callbacks }
  Application.OnInitialize := @ApplicationInitialize;

  { create Window and initialize Window callbacks }
  Window := TCastleWindowBase.Create(Application);
  Window.ParseParameters; // allows to control window size / fullscreen on the command-line
  Application.MainWindow := Window;

  Window.OnUpdate := @WindowUpdate;
  Window.OnPress := @WindowPress;
  Window.OnMotion := @WindowMotion;
end.
