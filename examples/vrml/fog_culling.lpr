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

{ Demo of fog culling. When rendering with fog turned "on" (the default),
  we do not render objects outside of the fog visibility radius.
  This can make a hude speedup if you have dense fog and exterior-like level
  (where frustum culing leaves too many shapes visible).

  This always loads models/fog_culling_final.wrl VRML file.
  Be sure to run it with proper current directory (examples/vrml/).
  It's a crafted scene, with some green and dense fog and a lot of
  spheres scattered around. Fog culling will work best on it.

  Handles keys:
    'f' turns fog on/off
    F5 makes a screenshot
}

program fog_culling;

uses VectorMath, GL, GLU, GLWindow,
  KambiClassUtils, KambiUtils, SysUtils, Classes, Cameras,
  KambiGLUtils, VRMLScene, VRMLGLScene,
  ProgressUnit, ProgressConsole, KambiFilesUtils, VRMLErrors,
  KambiSceneManager;

var
  Glw: TGLUIWindow;
  Scene: TVRMLGLScene;

type
  TMySceneManager = class(TKamSceneManager)
  private
    function TestFogVisibility(Shape: TVRMLGLShape): boolean;
  protected
    procedure Render3D(TransparentGroup: TTransparentGroup; InShadow: boolean); override;
  end;

var
  SceneManager: TMySceneManager;

function TMySceneManager.TestFogVisibility(Shape: TVRMLGLShape): boolean;
begin
  { Test for collision between two spheres.
    1st is the bounding sphere of Shape.
    2nd is the sphere around current camera position,
      with the radius taken from fog visibilityRadius (we should
      always multiply this by FogDistanceScaling).
    If there is no collision than we don't have to render given Shape. }
  Result := PointsDistanceSqr(
    Shape.BoundingSphereCenter,
    (Camera as TWalkCamera).Position) <=
      Sqr(Scene.FogNode.FdVisibilityRange.Value * Scene.FogDistanceScaling +
        Sqrt(Shape.BoundingSphereRadiusSqr));
end;

procedure TMySceneManager.Render3D(TransparentGroup: TTransparentGroup; InShadow: boolean);
begin
  if Scene.Attributes.UseFog then
    Scene.Render(@TestFogVisibility, TransparentGroup) else
    inherited;

  Writeln(Format('Rendered Shapes: %d / %d',
    [ Scene.LastRender_RenderedShapesCount,
      Scene.LastRender_VisibleShapesCount ]));
end;

procedure Init(glwin: TGLWindow);
begin
  { We use quite large triangles for fog_culling level demo wall.
    This means that fog must be correctly rendered,
    with perspective correction hint, otherwise ugly artifacts
    will be visible. }
  glHint(GL_FOG_HINT, GL_NICEST);
end;

procedure KeyDown(glwin: TGLWindow; Key: TKey; c: char);
begin
  case Key of
    K_F:
      begin
        with Scene do Attributes.UseFog := not Attributes.UseFog;
        Glw.PostRedisplay;
      end;
    K_F5: glwin.SaveScreenDialog(FNameAutoInc('fog_culling_screen_%d.png'));
  end;
end;

begin
  Parameters.CheckHigh(0);

  Glw := TGLUIWindow.Create(Application);

  SceneManager := TMySceneManager.Create(Application);
  Glw.Controls.Add(SceneManager);

  Scene := TVRMLGLScene.Create(Application);
  VRMLWarning := @VRMLWarning_Write;
  Scene.Load('models' + PathDelim + 'fog_culling_final.wrl');
  SceneManager.MainScene := Scene;
  SceneManager.Items.Add(Scene);

  Writeln(Scene.Info(true, true, false));

  { build octrees }
  Progress.UserInterface := ProgressConsoleInterface;
  Scene.TriangleOctreeProgressTitle := 'Building triangle octree';
  Scene.ShapeOctreeProgressTitle := 'Building Shape octree';
  Scene.Spatial := [ssRendering, ssDynamicCollisions];

  Glw.OnInit := @Init;
  Glw.OnKeyDown := @KeyDown;
  Glw.InitAndRun;
end.
