{
  Copyright 2003-2006 Michalis Kamburelis.

  This file is part of "Kambi's 3dmodels.gl Pascal units".

  "Kambi's 3dmodels.gl Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's 3dmodels.gl Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's 3dmodels.gl Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ This is based on simpleViewModel_2.dpr, with some changes:

  1. It always loads models/fog_culling_final.wrl VRML file.
     Be sure to run it with proper current directory
     (units/3dmodels.gl/examples/).

  2. It handles Background node (because our models/fog_culling_final.wrl
     model uses it).

  3. It handles some keys:
     'f' key turns fog on/off
     F5 makes a screenshot

  4. And finally, the most important thing: when rendering with fog on,
     we cull to fog visibilityRange. Objects outside of fog
     visibilityRange are not rendered at all. This allows us to render
     very quickly when fog is on.
}

program fog_culling;

uses VectorMath, Boxes3d, VRMLNodes, VRMLOpenGLRenderer, OpenGLh, GLWindow,
  GLW_Navigated, KambiClassUtils, KambiUtils, SysUtils, Classes, Object3dAsVRML,
  KambiGLUtils, VRMLFlatScene, VRMLFlatSceneGL, MatrixNavigation,
  ProgressUnit, ProgressConsole, KambiFilesUtils, VRMLTriangleOctree,
  VRMLErrors;

var
  Scene: TVRMLFlatSceneGL;
  { CameraRadius is needed for collision detection }
  CameraRadius: Single;

procedure BeforeDraw(glwin: TGLWindow);
begin
 Scene.PrepareRender([tgAll], false, true, false, false);
end;

type
  TFogVisibilityTester = class
    class function Test(ShapeStateNum: Integer): boolean;
  end;

var
  FogVisibilityTester: TFogVisibilityTester = nil;

class function TFogVisibilityTester.Test(ShapeStateNum: Integer): boolean;
begin
  { Test for collision between two spheres.
    1st is the bounding sphere of Scene.ShapeStates[ShapeStateNum].
    2nd is the sphere around current camera position,
      with the radius taken from fog visibilityRadius (we should
      always multiply this by FogDistanceScaling).
    If there is no collision than we don't have to render given ShapeState. }
  Result := PointsDistanceSqr(
    Scene.ShapeStates[ShapeStateNum].BoundingSphereCenter,
    Glw.NavWalker.CameraPos) <=
      Sqr(Scene.FogNode.FdVisibilityRange.Value * Scene.FogDistanceScaling +
        Sqrt(Scene.ShapeStates[ShapeStateNum].BoundingSphereRadiusSqr));
end;

procedure Draw(glwin: TGLWindow);
begin
  if Scene.Background <> nil then
  begin
    glLoadMatrix(Glw.Navigator.RotationOnlyMatrix);
    Scene.Background.Render;
    glClear(GL_DEPTH_BUFFER_BIT);
  end else
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  glLoadMatrix(glw.Navigator.Matrix);

  if Scene.Attributes.UseFog then
    Scene.Render(@FogVisibilityTester.Test, tgAll) else
    Scene.RenderFrustumOctree(Glw.NavWalker.Frustum, tgAll);

  Writeln(Format('Rendered ShapeStates: %d / %d',
    [ Scene.LastRender_RenderedShapeStatesCount,
      Scene.LastRender_AllShapeStatesCount ]));
end;

procedure Init(glwin: TGLWindow);
begin
 glEnable(GL_LIGHTING);
 glEnable(GL_LIGHT0);
end;

procedure Close(glwin: TGLWindow);
begin
 Scene.CloseGL;
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

procedure Resize(glwin: TGLWindow);

  procedure UpdateNavigatorProjectionMatrix;
  var
    ProjectionMatrix: TMatrix4f;
  begin
   glGetFloatv(GL_PROJECTION_MATRIX, @ProjectionMatrix);
   Glw.NavWalker.ProjectionMatrix := ProjectionMatrix;
  end;

begin
 glViewport(0, 0, glwin.Width, glwin.Height);
 ProjectionGLPerspective(45.0, glwin.Width/glwin.Height,
   CameraRadius * 0.6,
   Box3dMaxSize(Scene.BoundingBox) * 3.0);

 UpdateNavigatorProjectionMatrix;
end;

type
  TDummy = class
    class function MoveAllowed(Navigator: TMatrixWalker;
      const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
      const BecauseOfGravity: boolean): boolean;
  end;

class function TDummy.MoveAllowed(Navigator: TMatrixWalker;
  const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
  const BecauseOfGravity: boolean): boolean;
begin
 Result := Scene.DefaultTriangleOctree.MoveAllowed(
   Navigator.CameraPos, ProposedNewPos, NewPos, CameraRadius,
   NoItemIndex, nil);
end;

var
  //i: Integer;
  CamPos, CamDir, CamUp: TVector3Single;
  Dummy: TDummy = nil;
begin
 Parameters.CheckHigh(0);
 try
  VRMLNonFatalError := @VRMLNonFatalError_WarningWrite;

  Scene := TVRMLFlatSceneGL.Create(LoadAsVRML(
    'models' + PathDelim + 'fog_culling_final.wrl', true),
    true, roSeparateShapeStates);

  Writeln(Scene.Info(true, true));

  { build octrees }
  Progress.UserInterface := ProgressConsoleInterface;
  Scene.DefaultTriangleOctree :=
    Scene.CreateTriangleOctree('Building triangle octree');
  Scene.DefaultShapeStateOctree :=
    Scene.CreateShapeStateOctree('Building ShapeState octree');

  Scene.GetPerspectiveViewpoint(CamPos, CamDir, CamUp);

  { init Glw.Navigator }
  Glw.Navigator := TMatrixWalker.Create(@Glw.PostRedisplayOnMatrixChanged);
  Glw.NavWalker.Init(CamPos,
    VectorAdjustToLength(CamDir, Box3dAvgSize(Scene.BoundingBox) * 0.01*0.4),
    CamUp,
    0.0, 0.0 { unused, we don't use Gravity here });

  { init collision detection }
  if IsEmptyBox3d(Scene.BoundingBox) then
   CameraRadius := 1.0 { any non-zero dummy value } else
   CameraRadius := Box3dAvgSize(Scene.BoundingBox) * 0.01;
  Glw.NavWalker.OnMoveAllowed := @Dummy.MoveAllowed;

  { allow the scene to use it's own lights }
  Scene.Attributes.UseLights := true;
  Scene.Attributes.FirstGLFreeLight := 1;

  Glw.OnInit := @Init;
  Glw.OnClose := @Close;
  Glw.OnResize := @Resize;
  Glw.OnBeforeDraw := @BeforeDraw;
  Glw.OnKeyDown := @KeyDown;
  Glw.InitLoop(ProgramName, @Draw);
 finally Scene.Free end;
end.
