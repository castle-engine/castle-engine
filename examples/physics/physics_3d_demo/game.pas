{
  Copyright 2017-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Implements the game logic. }
unit Game;

interface

implementation

uses SysUtils, Classes, Generics.Collections,
  CastleWindow, CastleScene, CastleControls, CastleLog, X3DNodes, Castle3D,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse, CastleColors,
  CastleCameras, CastleVectors, CastleRenderer, CastleBoxes, CastleSceneManager;

var
  Window: TCastleWindow;
  SceneManager: TCastleSceneManager; //< Shortcut for Window.SceneManager
  Level: T3DTransform;
  BoxTemplate, SphereTemplate: TCastleScene;

{ One-time initialization of resources. }
procedure ApplicationInitialize;
var
  LevelScene: TCastleScene;
  LevelBody: TRigidBody;
  LevelCollider: TPlaneCollider;
  MoveLimit: TBox3D;
begin
  { TODO:
    This code will be simpler once we merge various T3D descendants
    into TCastleTransform,
    and make TCastleScene descend from TCastleTransform.
    Coming in next CGE release (6.4),
    see https://castle-engine.sourceforge.io/planned_features.php }

  LevelScene := TCastleScene.Create(Application);
  LevelScene.Load(ApplicationData('level.x3dv'));
  LevelScene.Spatial := [ssRendering, ssDynamicCollisions];
  LevelScene.ProcessEvents := true;
  LevelScene.Attributes.Shaders := srAlways; // nicer lighting

  Level := T3DTransform.Create(Application);
  Level.Add(LevelScene);

  LevelBody := TRigidBody.Create(LevelScene);
  LevelBody.Dynamic := false;

  LevelCollider := TPlaneCollider.Create(LevelBody);
  LevelCollider.Normal := Vector3(0, 1, 0);
  LevelCollider.Distance := 0;
  LevelBody.Collider := LevelCollider;

  { assign this only once LevelBody and LevelCollider
    are fully configured, this initializes physics engine }
  Level.RigidBody := LevelBody;

  SceneManager := Window.SceneManager;
  SceneManager.Items.Add(Level);
  SceneManager.MainScene := LevelScene;

  // make gravity work even if your position is over the world bbox
  MoveLimit := SceneManager.Items.BoundingBox;
  MoveLimit.Max := MoveLimit.Max + Vector3(0, 1000, 0);
  SceneManager.MoveLimit := MoveLimit;

  SceneManager.NavigationType := ntWalk;
  // rotating by dragging would cause trouble when clicking to spawn boxes/spheres
  SceneManager.WalkCamera.Input :=
    SceneManager.WalkCamera.Input - [ciMouseDragging];
  SceneManager.WalkCamera.HeadBobbing := 0; // looks bad

  BoxTemplate := TCastleScene.Create(Application);
  BoxTemplate.Load(ApplicationData('box.x3d'));

  SphereTemplate := TCastleScene.Create(Application);
  SphereTemplate.Load(ApplicationData('sphere.x3d'));
end;

procedure WindowRender(Container: TUIContainer);
begin
  UIFont.PrintStrings(10, 10, Yellow, [
    Format('FPS: %f', [Container.Fps.RealTime]),
    'Left mouse button - spawn box',
    'Right mouse button - spawn sphere',
    'AWSD, arrows - move, rotate',
    'F4 - toggle mouse look'
  ], false, 0);
end;

procedure WindowPress(Container: TUIContainer; const Event: TInputPressRelease);

  procedure Spawn(const Template: TCastleScene; const Collider: TCollider);
  var
    Scene: TCastleScene;
    Transform: T3DTransform;
    CameraPos, CameraDir, CameraUp: TVector3;
    RigidBody: TRigidBody;
  begin
    Scene := Template.Clone(Application);

    Transform := T3DTransform.Create(Application);
    SceneManager.Camera.GetView(CameraPos, CameraDir, CameraUp);
    Transform.Translation := CameraPos + CameraDir * 2.0;
    // TODO: apply Transform.Direction from SceneManager.Camera.Direction
    Transform.Add(Scene);

    SceneManager.Items.Add(Transform);

    RigidBody := TRigidBody.Create(Scene);
    RigidBody.Collider := Collider;
    RigidBody.InitialLinearVelocity := CameraDir * 4.0;
    Transform.RigidBody := RigidBody;
  end;

var
  C: TWalkCamera;
  BoxCollider: TBoxCollider;
  SphereCollider: TSphereCollider;
begin
  if Event.IsKey(K_F4) then
  begin
    C := SceneManager.WalkCamera;
    C.MouseLook := not C.MouseLook;
  end;

  if Event.IsMouseButton(mbLeft) then
  begin
    BoxCollider := TBoxCollider.Create(Application);
    // TODO: assuming that box center is 0,0,0
    BoxCollider.Size := BoxTemplate.BoundingBox.Size;
    Spawn(BoxTemplate, BoxCollider);
  end;

  if Event.IsMouseButton(mbRight) then
  begin
    SphereCollider := TSphereCollider.Create(Application);
    // TODO: assuming that sphere center is 0,0,0
    SphereCollider.Radius := SphereTemplate.BoundingBox.Size.X / 2;
    Spawn(SphereTemplate, SphereCollider);
  end;
end;

function MyGetApplicationName: string;
begin
  Result := 'physics_3d_demo';
end;

initialization
  { This sets SysUtils.ApplicationName.
    It is useful to make sure it is correct (as early as possible)
    as our log routines use it. }
  OnGetApplicationName := @MyGetApplicationName;

  InitializeLog;

  { initialize Application callbacks }
  Application.OnInitialize := @ApplicationInitialize;

  { create Window and initialize Window callbacks }
  Window := TCastleWindow.Create(Application);
  Application.MainWindow := Window;
  Window.OnRender := @WindowRender;
  Window.OnPress := @WindowPress;
end.
