{
  Copyright 2010-2016 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Demo of TCastleScene, scene manager and related functionality.
  Follow the relevant tutorial pages
  http://castle-engine.sourceforge.net/tutorial_load_3d.php
  http://castle-engine.sourceforge.net/tutorial_scene.php
}
program cars_demo;

uses SysUtils, CastleVectors, Castle3D, CastleUIControls, CastleUtils,
  CastleFilesUtils, CastleWindow, CastleSceneCore, CastleScene,
  CastleKeysMouse;

var
  Window: TCastleWindow;
  CarScene, RoadScene: TCastleScene;
  CarTransforms: array [1..20] of T3DTransform;

procedure WindowUpdate(Container: TUIContainer);

  procedure UpdateCarTransform(const CarTransform: T3DTransform);
  var
    T: TVector3Single;
  begin
    T := CarTransform.Translation;
    { Thanks to multiplying by SecondsPassed, it is a time-based operation,
      and will always move 40 units / per second along the -Z axis. }
    T := T + Vector3Single(0, 0, -40) * Container.Fps.UpdateSecondsPassed;
    { Wrap the Z position, to move in a loop }
    if T[2] < -70.0 then
      T[2] := 50.0;
    CarTransform.Translation := T;
  end;

var
  I: Integer;
begin
  for I := Low(CarTransforms) to High(CarTransforms) do
    UpdateCarTransform(CarTransforms[I]);
end;

procedure WindowPress(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.IsKey('c') then
    CarTransforms[1].Exists := not CarTransforms[1].Exists;

  { capture a screenshot }
  if Event.IsKey(K_F5) then
    Window.SaveScreen(FileNameAutoInc(ApplicationName + '_screen_%d.png'));
end;

var
  I: Integer;
begin
  Window := TCastleWindow.Create(Application);

  CarScene := TCastleScene.Create(Application);
  CarScene.Load(ApplicationData('car.x3d'));
  CarScene.Spatial := [ssRendering, ssDynamicCollisions];
  CarScene.ProcessEvents := true;
  CarScene.PlayAnimation('wheels_turning', paForceLooping);

  for I := Low(CarTransforms) to High(CarTransforms) do
  begin
    CarTransforms[I] := T3DTransform.Create(Application);
    CarTransforms[I].Translation := Vector3Single(
      -6 + Random(4) * 6, 0, RandomFloatRange(-70, 50));
    CarTransforms[I].Add(CarScene);
    Window.SceneManager.Items.Add(CarTransforms[I]);
  end;

  RoadScene := TCastleScene.Create(Application);
  RoadScene.Load(ApplicationData('road.x3d'));
  RoadScene.Spatial := [ssRendering, ssDynamicCollisions];
  RoadScene.ProcessEvents := true;

  Window.SceneManager.Items.Add(RoadScene);
  Window.SceneManager.MainScene := RoadScene;

  Window.SceneManager.RequiredCamera.SetView(
    Vector3Single(-43.30, 27.23, -80.74),
    Vector3Single(  0.60, -0.36,   0.70),
    Vector3Single(  0.18,  0.92,   0.32)
  );
  // better camera for only a car:
  {Window.SceneManager.RequiredCamera.SetView(
    Vector3Single(-7.83,  6.15, -7.55),
    Vector3Single( 0.47, -0.30,  0.82),
    Vector3Single( 0.16,  0.95,  0.25)
  );}

  Window.OnUpdate := @WindowUpdate;
  Window.OnPress := @WindowPress;
  Window.Open;
  Application.Run;
end.
