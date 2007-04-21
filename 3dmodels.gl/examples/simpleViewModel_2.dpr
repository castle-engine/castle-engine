{
  Copyright 2003-2007 Michalis Kamburelis.

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

{ This is the next version of simpleViewModel.dpr.
  If you want to learn how to use my units, you should first understand
  sources of simpleViewModel.dpr, then move to this example.

  This version adds:

  1. Collision detection
  - constructing triangle octree, see line "Scene.DefaultTriangleOctree := ..."
  - doing collision detection using this octree, see MoveAllowed procedure

  2. Rendering with frustum culling (with the help of octree)
  - scene is loaded with Optimization roSeparateShapeStates instead of roSceneAsAWhole
  - constructing ShapeState octree, see line "Scene.DefaultShapeStateOctree := ..."
  - initializing Glw.NavWalker.ProjectionMatrix (this is needed to get
    Glw.NavWalker.Frustum)
  - using Glw.NavWalker.Frustum and ShapeState octree to render the scene,
    see line "Scene.RenderFrustumOctree"

  3. Tries to get default camera settings from VRML file
  - see Scene.GetPerspectiveViewpoint call

  4. Makes FPS timings right after starting the program correct:
  - uses Glw.OnBeforeDraw and Scene.PrepareRender(false)

  5. Allows the scene to use it's own lights:
  - see Scene.Attributes.UseLights line
}

program simpleViewModel_2;

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
 Scene.PrepareRender([tgAll], false, true, false, false, false);
end;

procedure Draw(glwin: TGLWindow);
begin
 glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
 glLoadMatrix(glw.Navigator.Matrix);
 Scene.RenderFrustumOctree(Glw.NavWalker.Frustum, tgAll);
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
  CamPos, CamDir, CamUp, GravityUp: TVector3Single;
  Dummy: TDummy;
begin
 Parameters.CheckHigh(1);
 try
  VRMLNonFatalError := @VRMLNonFatalError_WarningWrite;

  Scene := TVRMLFlatSceneGL.Create(LoadAsVRML(Parameters[1], true),
    true, {roSceneAsAWhole}roSeparateShapeStates);

  Writeln(Scene.Info(true, true));

  { Uncomment this to get some info about scene nodes:
  for i := 0 to Scene.ShapeStates.Count-1 do
   with Scene.ShapeStates[i] do
   begin
    Writeln(Format('Shape name ''%10s'' (''%20s'') : LocalBBox = %50s, BBox = %50s',
      [ShapeNode.NodeName, ShapeNode.NodeTypeName,
       Box3dToNiceStr(LocalBoundingBox), Box3dToNiceStr(BoundingBox)]));
   end;}

  { build octrees }
  Progress.UserInterface := ProgressConsoleInterface;
  Scene.DefaultTriangleOctree :=
    Scene.CreateTriangleOctree('Building triangle octree');
  Scene.DefaultShapeStateOctree :=
    Scene.CreateShapeStateOctree('Building ShapeState octree');

  Scene.GetPerspectiveViewpoint(CamPos, CamDir, CamUp, GravityUp);

  { init Glw.Navigator }
  Glw.Navigator := TMatrixWalker.Create(@Glw.PostRedisplayOnMatrixChanged);
  Glw.NavWalker.Init(CamPos,
    VectorAdjustToLength(CamDir, Box3dAvgSize(Scene.BoundingBox) * 0.01*0.4),
    CamUp, GravityUp,
    0.0, 0.0 { unused, we don't use Gravity here });

  { init collision detection }
  if IsEmptyBox3d(Scene.BoundingBox) then
   CameraRadius := 1.0 { any non-zero dummy value } else
   CameraRadius := Box3dAvgSize(Scene.BoundingBox) * 0.01;
  Dummy := nil;
  Glw.NavWalker.OnMoveAllowed := @Dummy.MoveAllowed;

  { allow the scene to use it's own lights }
  Scene.Attributes.UseLights := true;
  Scene.Attributes.FirstGLFreeLight := 1;

  Glw.OnInit := @Init;
  Glw.OnClose := @Close;
  Glw.OnResize := @Resize;
  Glw.OnBeforeDraw := @BeforeDraw;
  Glw.InitLoop(ProgramName, @Draw);
 finally Scene.Free end;
end.
