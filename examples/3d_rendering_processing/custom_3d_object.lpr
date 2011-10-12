{
  Copyright 2006-2011 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ If you want to write your own rendering and collisions code,
  you can define your own T3D descendant.

  Note that most programs should be happy just using the T3D descendants
  already implemented inside our engine. First of all there's TCastleScene,
  that allows to express virtually everything through VRML/X3D nodes,
  and has an optimized renderer, collision solver etc.

  But if you really want, you can define your own T3D descendant. }

program demo_camera;

{$apptype GUI}

uses VectorMath, Boxes3D, GL, GLU, CastleWindow, Frustum,
  CastleClassUtils, CastleUtils, SysUtils, Classes, Base3D,
  CastleGLUtils, Cameras, CastleFilesUtils, CastleSceneManager, CastleStringUtils;

type
  TCube = class(T3D)
  public
    procedure Render(const Frustum: TFrustum; const Params: TRenderParams); override;
    function BoundingBox: TBox3D; override;
  end;

procedure TCube.Render(const Frustum: TFrustum; const Params: TRenderParams);
begin
  if not Params.RenderTransformIdentity then
  begin
    glPushMatrix;
    glMultMatrix(Params.RenderTransform);
  end;

  if not Params.Transparent then
  begin
    glPushAttrib(GL_ENABLE_BIT);
      glEnable(GL_LIGHTING);
      glEnable(GL_LIGHT0);
      glEnable(GL_DEPTH_TEST);
      DrawGLBox(-1, -1, -1, 1, 1, 1, 0, 0, 0, true, false);
    glPopAttrib;
  end;

  if not Params.RenderTransformIdentity then
    glPopMatrix;
end;

function TCube.BoundingBox: TBox3D;
begin
  Result := Box3D(
    Vector3Single(-1, -1, -1),
    Vector3Single( 1,  1,  1));
end;

var
  Window: TCastleWindow;
  Cube: TCube;
begin
  Window := TCastleWindow.Create(Application);

  Cube := TCube.Create(Application);
  Window.SceneManager.Items.Add(Cube);

  { init SceneManager.Camera.
    This is optional, if SceneManager.Camera is left unassigned, a suitable
    default camera will be created by the scene manager during ApplyProjection
    (before first rendering), see TCastleSceneManager.CreateDefaultCamera docs. }
  Window.SceneManager.Camera := TExamineCamera.Create(Application);
  (Window.SceneManager.Camera as TExamineCamera).Init(Box3D(
    Vector3Single(-1, -1, -1),
    Vector3Single( 1,  1,  1)), 0.1);

  Window.SetDemoOptions(K_F11, CharEscape, true);
  Window.OpenAndRun;
end.
