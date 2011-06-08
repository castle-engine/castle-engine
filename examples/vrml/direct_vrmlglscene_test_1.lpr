{
  Copyright 2003-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Low-level demo how to access TVRMLGLScene without a SceneManager, part 1.
  This is highly discouraged now --- SceneManager does a lot of stuff
  for you, results in more functional and at the same time shorter code.
  This demo remains just to show that it's possible (even if
  uncomfortable) to work without the SceneManager.

  It loads a model from a filename given as command-line parameter.
  Supports all known model formats (VRML, X3D, Collada etc.,
  for a full list of supported formats see view3dscene documentation).

  It allows user to walk/examine the scene by a Camera,
  see view3dscene documentation for keys/mouse controls available,
  [http://vrmlengine.sourceforge.net/view3dscene.php#section_keys].

  Thanks to using TGLUIWindow class, that descends
  from TGLWindowDemo, this program automatically handles keys Escape (Quit)
  and F11 (Swap fullscreen) and displays FPS on window's Caption.

  For the sake of simplicity, this example takes some shortcuts
  (that are fixed in the direct_vrmlglscene_test_2.lpr):
  - Collision detection is *not* done.
    We even turn off gravity (because it would make camera fall down infinitely,
    since without collision detection you sink through the floor.)
  - Scene is rendered without frustum culling, since we do not pass
    current frustum to Scene.Render.
  - If you will load large VRML scene, then right after program start
    you may notice for a very short time that your camera moves too fast.
}

program direct_vrmlglscene_test_1;

uses VectorMath, VRMLNodes, GL, GLU, GLWindow,
  KambiClassUtils, KambiUtils, SysUtils, Classes,
  KambiGLUtils, VRMLGLScene, Cameras, KambiFilesUtils, VRMLErrors;

var
  Window: TGLUIWindow;
  Scene: TVRMLGLScene;
  Camera: TCamera;
  RenderParams: TBasicRenderParams;

procedure Draw(Window: TGLWindow);
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadMatrix(Camera.Matrix);
  Scene.Render(nil, RenderParams);
end;

procedure Close(Window: TGLWindow);
begin
  Scene.GLContextClose;
end;

procedure Resize(Window: TGLWindow);
begin
  Scene.GLProjection(Camera, Scene.BoundingBox,
    0, 0, Window.Width, Window.Height);
end;

begin
  Window := TGLUIWindow.Create(Application);

  Parameters.CheckHigh(1);

  VRMLWarning := @VRMLWarning_Write;
  Scene := TVRMLGLScene.Create(nil);
  try
    Scene.Load(Parameters[1]);

    RenderParams := TBasicRenderParams.Create;

    Writeln(Scene.Info(true, true, false));

    { init camera }
    Camera := Scene.CreateCamera(Window);
    if Camera is TWalkCamera then
      (Camera as TWalkCamera).Gravity := false;
    Window.Controls.Add(Camera);

    Window.OnClose := @Close;
    Window.OnResize := @Resize;
    Window.OpenAndRun(ProgramName, @Draw);
  finally
    Scene.Free;
    RenderParams.Free;
  end;
end.
