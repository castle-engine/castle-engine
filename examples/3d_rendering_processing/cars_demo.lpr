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

uses SysUtils, CastleVectors, Castle3D, CastleUIControls,
  CastleFilesUtils, CastleWindow, CastleSceneCore, CastleScene;

var
  Window: TCastleWindow;
  CarScene, RoadScene: TCastleScene;
  CarTransform: T3DTransform;

procedure WindowUpdate(Container: TUIContainer);
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

begin
  Window := TCastleWindow.Create(Application);

  CarScene := TCastleScene.Create(Application);
  CarScene.Load(ApplicationData('car.x3d'));
  CarScene.Spatial := [ssRendering, ssDynamicCollisions];
  CarScene.ProcessEvents := true;

  CarTransform := T3DTransform.Create(Application);
  CarTransform.Add(CarScene);

  RoadScene := TCastleScene.Create(Application);
  RoadScene.Load(ApplicationData('road.x3d'));
  RoadScene.Spatial := [ssRendering, ssDynamicCollisions];
  RoadScene.ProcessEvents := true;

  Window.SceneManager.Items.Add(CarTransform);
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
  Window.Open;
  Application.Run;
end.
