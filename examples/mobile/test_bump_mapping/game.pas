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

{ Implements the game logic, independent from mobile / standalone. }
unit Game;

interface

uses CastleWindowTouch;

var
  Window: TCastleWindowTouch;

implementation

uses SysUtils, Math,
  CastleWindow, CastleScene, CastleControls, CastleLog,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse, CastleColors,
  CastleVectors, CastleTransform, X3DNodes, CastleTimeUtils;

var
  SceneVisualizeLight: TCastleScene;
  MainLight: TPointLightNode;
  Time: TFloatTime;

procedure UpdateMainLightLocation;
const
  Radius = 2;
var
  S, C: Float;
begin
  SinCos(Time * 2, S, C);
  MainLight.Location := Vector3(S * Radius, C * Radius + 1, 2);
end;

{ One-time initialization of resources. }
procedure ApplicationInitialize;

  function CreateSphereModel: TX3DRootNode;
  var
    Sphere: TSphereNode;
    Shape: TShapeNode;
  begin
    Sphere := TSphereNode.CreateShape(Shape);
    Sphere.Radius := 0.1;

    Shape.Material := TMaterialNode.Create;
    Shape.Material.EmissiveColor := YellowRGB * 0.5;

    Result := TX3DRootNode.Create;
    Result.AddChildren(Shape);
  end;

var
  Scene1, Scene2: TCastleScene;
  Transform2: TCastleTransform;
begin
  Scene1 := TCastleScene.Create(Application);
  Scene1.Load(ApplicationData('steep_parallax.x3dv'));
  Scene1.Spatial := [ssRendering, ssDynamicCollisions];
  Scene1.ProcessEvents := true;
  Window.SceneManager.Items.Add(Scene1);

  SceneVisualizeLight := TCastleScene.Create(Application);
  SceneVisualizeLight.Load(CreateSphereModel, true);
  Window.SceneManager.Items.Add(SceneVisualizeLight);

  // TODO: Transform2 should not be necessary, Scene2.Translation should work OK
  Transform2 := TCastleTransform.Create(Application);
  Transform2.Translation := Vector3(0, 2.1, 0);
  Window.SceneManager.Items.Add(Transform2);

  Scene2 := TCastleScene.Create(Application);
  Scene2.Load(ApplicationData('leaf.x3dv'));
  Scene2.Spatial := [ssRendering, ssDynamicCollisions];
  Scene2.ProcessEvents := true;
  // Scene2.Translation := Vector3(0, 2, 0);
  // Window.SceneManager.Items.Add(Scene2);
  Transform2.Add(Scene2);

  // make MainLight on Scene1 affect all scenes, Scene1 and Scene2
  Window.SceneManager.MainScene := Scene1;
  Window.SceneManager.UseGlobalLights := true;

  MainLight := Scene1.Node('MainLight') as TPointLightNode;
  Time := 0;
  UpdateMainLightLocation;
end;

procedure WindowRender(Container: TUIContainer);
begin
  UIFont.Print(10, 10, Yellow, Format('FPS: %f', [Container.Fps.RealTime]));
end;

procedure WindowUpdate(Container: TUIContainer);
begin
  Time := Time + Container.Fps.UpdateSecondsPassed;
  UpdateMainLightLocation;
  SceneVisualizeLight.Translation := MainLight.Location;
end;

function MyGetApplicationName: string;
begin
  Result := 'test_bump_mapping';
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
  Window := TCastleWindowTouch.Create(Application);
  Application.MainWindow := Window;
  Window.OnRender := @WindowRender;
  Window.OnUpdate := @WindowUpdate;
end.
