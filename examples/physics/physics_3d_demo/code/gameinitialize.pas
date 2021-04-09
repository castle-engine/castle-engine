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
  CastleWindow, CastleScene, CastleControls, CastleLog, X3DNodes, CastleTransform,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse, CastleColors,
  CastleCameras, CastleVectors, CastleBoxes, CastleViewport,
  CastleUIControls, CastleApplicationProperties;

var
  Window: TCastleWindowBase;
  Viewport: TCastleViewport;
  Navigation: TCastleWalkNavigation;
  Level: TCastleScene;
  BoxTemplate, SphereTemplate: TCastleScene;

procedure LoadLevel(const URL: string; const MeshCollider: boolean);

  function CreatePlaneCollider(const ParentBody: TRigidBody): TPlaneCollider;
  begin
    Result := TPlaneCollider.Create(ParentBody);
    Result.Normal := Vector3(0, 1, 0);
    Result.Distance := 0;
    Result.Restitution := 0.3;
  end;

  function CreateMeshCollider(const ParentBody: TRigidBody): TMeshCollider;
  begin
    Result := TMeshCollider.Create(ParentBody);
    Result.Scene := Level;
    Result.Restitution := 0.3;
  end;

var
  LevelBody: TRigidBody;
  MoveLimit: TBox3D;
begin
  { free previous level, which also frees all related rigid bodies }
  FreeAndNil(Level);

  // Viewport.Items.Clear; // not needed, we already freed everything

  Level := TCastleScene.Create(Application);
  Level.Load(URL);
  Level.Spatial := [ssRendering, ssDynamicCollisions];
  Level.ProcessEvents := true;
  Level.RenderOptions.PhongShading := true; // nicer lighting

  LevelBody := TRigidBody.Create(Level);
  LevelBody.Dynamic := false;

  if MeshCollider then
    CreateMeshCollider(LevelBody)
  else
    CreatePlaneCollider(LevelBody);

  { assign this only once LevelBody and LevelCollider
    are fully configured, this initializes physics engine }
  Level.RigidBody := LevelBody;

  Viewport.Items.Add(Level);
  Viewport.Items.MainScene := Level;

  { Make movement possible only within the world box,
    and make gravity work even if you're far above the world. }
  MoveLimit := Viewport.Items.BoundingBox;
  MoveLimit.Max := MoveLimit.Max + Vector3(0, 1000, 0);
  Viewport.Items.MoveLimit := MoveLimit;

  Viewport.AssignDefaultCamera;
end;

type
  TEventHandler = class
    class procedure LoadLevelSimple(Sender: TObject);
    class procedure LoadLevelComplex(Sender: TObject);
  end;

class procedure TEventHandler.LoadLevelSimple(Sender: TObject);
begin
  LoadLevel('castle-data:/level_simple.x3dv', false);
end;

class procedure TEventHandler.LoadLevelComplex(Sender: TObject);
begin
  LoadLevel('castle-data:/level_complex.x3dv', true);
end;

{ One-time initialization of resources. }
procedure ApplicationInitialize;
var
  ButtonLevelSimple, ButtonLevelComplex: TCastleButton;
begin
  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Viewport.AutoCamera := true;
  Window.Controls.InsertFront(Viewport);

  LoadLevel('castle-data:/level_simple.x3dv', false);

  // create Navigation
  Navigation := TCastleWalkNavigation.Create(Application);
  Navigation.PreferredHeight := 2;
  Navigation.Gravity := true;
  // rotating by dragging would cause trouble when clicking to spawn boxes/spheres
  Navigation.Input := Navigation.Input - [niMouseDragging];
  Viewport.Navigation := Navigation;

  // easy way to make the simulation feel more dynamic
  Viewport.Items.TimeScale := 2;

  BoxTemplate := TCastleScene.Create(Application);
  BoxTemplate.Load('castle-data:/box.x3d');

  SphereTemplate := TCastleScene.Create(Application);
  SphereTemplate.Load('castle-data:/sphere.x3d');

  Window.Container.UIReferenceWidth := 1024;
  Window.Container.UIReferenceHeight := 768;
  Window.Container.UIScaling := usEncloseReferenceSize;

  ButtonLevelSimple := TCastleButton.Create(Application);
  ButtonLevelSimple.Caption := 'Simple Level (Plane Collider)';
  ButtonLevelSimple.OnClick := @TEventHandler(nil).LoadLevelSimple;
  ButtonLevelSimple.Anchor(hpLeft, 10);
  ButtonLevelSimple.Anchor(vpTop, -10);
  Window.Controls.InsertFront(ButtonLevelSimple);

  ButtonLevelComplex := TCastleButton.Create(Application);
  ButtonLevelComplex.Caption := 'Complex Level (Mesh Collider)';
  ButtonLevelComplex.OnClick := @TEventHandler(nil).LoadLevelComplex;
  ButtonLevelComplex.Anchor(hpLeft, 10);
  ButtonLevelComplex.Anchor(vpTop, -10 - ButtonLevelSimple.EffectiveHeight - 10);
  Window.Controls.InsertFront(ButtonLevelComplex);
end;

procedure WindowRender(Container: TUIContainer);
begin
  UIFont.PrintStrings(10, 10, Yellow, [
    Format('FPS: %s', [Container.Fps.ToString]),
    'Left mouse button - spawn box',
    'Right mouse button - spawn sphere',
    'AWSD, arrows - move, rotate',
    'F4 - toggle mouse look'
  ], false, 0);
end;

procedure WindowPress(Container: TUIContainer; const Event: TInputPressRelease);

  procedure Spawn(const Template: TCastleScene; const Collider: TCollider;
    const RigidBody: TRigidBody);
  var
    Scene: TCastleScene;
    CameraPos, CameraDir, CameraUp: TVector3;
  begin
    Scene := Template.Clone(Level);

    Viewport.Camera.GetView(CameraPos, CameraDir, CameraUp);
    Scene.Translation := CameraPos + CameraDir * 2.0;
    Scene.Direction := CameraDir;

    Viewport.Items.Add(Scene);

    RigidBody.LinearVelocity := CameraDir * 4.0;
    Scene.RigidBody := RigidBody;
  end;

var
  RigidBody: TRigidBody;
  BoxCollider: TBoxCollider;
  SphereCollider: TSphereCollider;
begin
  if Event.IsKey(keyF4) then
    Navigation.MouseLook := not Navigation.MouseLook;

  if Event.IsKey(keyF6) then
    Viewport.Items.EnablePhysics := not Viewport.Items.EnablePhysics;

  if Event.IsMouseButton(buttonLeft) then
  begin
    RigidBody := TRigidBody.Create(BoxTemplate);

    BoxCollider := TBoxCollider.Create(RigidBody);
    BoxCollider.Size := BoxTemplate.BoundingBox.Size;
    BoxCollider.Restitution := 0.3;
    BoxCollider.Density := 100.0;
    Spawn(BoxTemplate, BoxCollider, RigidBody);
  end;

  if Event.IsMouseButton(buttonRight) then
  begin
    RigidBody := TRigidBody.Create(SphereTemplate);

    SphereCollider := TSphereCollider.Create(RigidBody);
    SphereCollider.Radius := SphereTemplate.BoundingBox.Size.X / 2;
    SphereCollider.Friction := 0.4;
    SphereCollider.Restitution := 0.2;
    SphereCollider.Density := 20.0;
    Spawn(SphereTemplate, SphereCollider, RigidBody);
  end;
end;

initialization
  { Set ApplicationName early, as our log uses it. }
  ApplicationProperties.ApplicationName := 'physics_3d_demo';

  InitializeLog;

  { initialize Application callbacks }
  Application.OnInitialize := @ApplicationInitialize;

  { create Window and initialize Window callbacks }
  Window := TCastleWindowBase.Create(Application);
  Application.MainWindow := Window;
  Window.OnRender := @WindowRender;
  Window.OnPress := @WindowPress;
end.
