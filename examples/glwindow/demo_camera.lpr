{
  Copyright 2006-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Simple demo that can you can create SceneManager.Camera instance yourself.

  As you can see, in the simplest case you just
  - use TGLUIWindow class, adding SceneManager to Window.Controls
  - init SceneManager.Camera at the beginning of your program
    (actually, we could left it not initialized,
    it would be automatically initialized to a default camera by scene manager,
    see TKamSceneManager.CreateDefaultCamera.)

  And that's it. User gets the ability to move and rotate the object
  by arrow keys, mouse etc. (see view3dscene keys is "Examine" mode:
  [http://vrmlengine.sourceforge.net/view3dscene.php]),
  because SceneManager passes relevant window events to camera.
  Also everything 3D is rendered with this camera,
  because SceneManager loads Camera.Matrix to OpenGL modelview matrix.
}

program demo_camera;

{$apptype GUI}

uses VectorMath, Boxes3D, GL, GLU, GLWindow, Frustum,
  KambiClassUtils, KambiUtils, SysUtils, Classes, Base3D,
  KambiGLUtils, Cameras, KambiFilesUtils, KambiSceneManager;

type
  TCube = class(T3D)
  public
    procedure Render(const Frustum: TFrustum; const Params: TRenderParams); override;
    function BoundingBox: TBox3D; override;
  end;

procedure TCube.Render(const Frustum: TFrustum; const Params: TRenderParams);
begin
  if Params.TransparentGroup in [tgAll, tgOpaque] then
  begin
    glPushAttrib(GL_ENABLE_BIT);
      glEnable(GL_LIGHTING);
      glEnable(GL_LIGHT0);
      glEnable(GL_DEPTH_TEST);
      DrawGLBox(-1, -1, -1, 1, 1, 1, 0, 0, 0, true, false);
    glPopAttrib;
  end;
end;

function TCube.BoundingBox: TBox3D;
begin
  Result := Box3D(
    Vector3Single(-1, -1, -1),
    Vector3Single( 1,  1,  1));
end;

var
  Window: TGLUIWindow;
  SceneManager: TKamSceneManager;
  Cube: TCube;
begin
  Window := TGLUIWindow.Create(Application);

  SceneManager := TKamSceneManager.Create(Application);
  Window.Controls.Add(SceneManager);

  Cube := TCube.Create(Application);
  SceneManager.Items.Add(Cube);

  { init SceneManager.Camera }
  SceneManager.Camera := TExamineCamera.Create(Application);
  (SceneManager.Camera as TExamineCamera).Init(Box3D(
    Vector3Single(-1, -1, -1),
    Vector3Single( 1,  1,  1)), 0.1);

  Window.OpenAndRun;
end.
