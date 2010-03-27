{ -*- compile-command: "./compile_console.sh" -*- }
{
  Copyright 2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestOpeningAndRendering3D;

interface

uses fpcunit, testutils, testregistry;

type
  TTestOpeningAndRendering3D = class(TTestCase)
  published
    procedure Test1;
  end;

implementation

uses SysUtils, GLWindow, VRMLScene, VRMLGLScene, KambiSceneManager;

procedure TTestOpeningAndRendering3D.Test1;
var
  Window: TGLUIWindow;
  SceneManager: TKamSceneManager;
  Scene: TVRMLGLScene;

  { FileName is relative to kambi_vrml_test_suite.
    Empty means to load empty scene. }
  procedure TestScene(const FileName: string);
  begin
    if FileName = '' then
      Scene.Load(nil, true) else
      Scene.Load('../../kambi_vrml_test_suite/' + FileName);

    { Force preparing and using OpenGL resources for the scene.
      This way we also check that next Load frees them Ok. }
    Window.EventBeforeDraw;
    Window.EventDraw;
  end;

begin
  Window := TGLUIWindow.Create(nil);
  try
    Scene := TVRMLGLScene.Create(Window);
    Scene.Spatial := [ssRendering, ssDynamicCollisions];
    Scene.ProcessEvents := true;

    SceneManager := TKamSceneManager.Create(Window);
    SceneManager.Items.Add(Scene);
    SceneManager.MainScene := Scene;

    Window.Controls.Add(SceneManager);
    Window.Init;

    TestScene('');
    TestScene('x3d/follow_camera_by_proximity_sensor.x3dv');
    TestScene('x3d/follow_camera_by_proximity_sensor.x3dv');

    Window.Close;
  finally FreeAndNil(Window) end;
end;

initialization
  RegisterTest(TTestOpeningAndRendering3D);
end.
