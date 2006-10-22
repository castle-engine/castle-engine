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

{ This an example how to write the simplest VRML, 3DS and OBJ viewer
  using my units. (for a current list of supported 3d models' formats,
  see view3dscene documentation)

  It loads a model from a filename given as command-line parameter.

  It allows user to walk in the scene using keys Up/Down (forward/back),
  Right/Left (rotate), PageUp/PageDown (raise/bow your head) etc.
  -- for a complete list of keys available see documentation
  of @link(TMatrixWalker) class or view3dscene documentation
  (look for "keys available in Walk navigation method").

  Thanks to using GLW_Navigated unit (that exposes Glw of class
  TGLWindowNavgated that descends from TGLWindowDemo),
  this program automatically handles keys Escape (Quit)
  and Ctrl+F (Swap fullscreen) and displays FPS on window's Caption.

  Note that this example exposes some shortcomings that are not fixed here
  for the sake of simplicity of this example.
  They are all fixed in a more sophisticated version of this example
  in file simpleViewModel_2.dpr.
  - In this program starting camera position is *not* read from file.
    Camera always starts positioned at the middle of 3d model,
    looking in (1, 0, 0) direction with up vector (0, 1, 0).
  - Collision detection is *not* done.
  - Also, scene is rendered using roSceneAsAWhole optimization.
    No frustum culling, or anything like that.
  - If you will load large VRML scene, then right after program start
    you may notice for a very short time that your camera moves too fast.
  - Scene lights (written in VRML file) are not used.
}

program simpleViewModel;

uses VectorMath, Boxes3d, VRMLNodes, VRMLOpenGLRenderer, OpenGLh, GLWindow,
  GLW_Navigated, KambiClassUtils, KambiUtils, SysUtils, Classes, Object3dAsVRML,
  KambiGLUtils, VRMLFlatScene, VRMLFlatSceneGL, MatrixNavigation,
  KambiFilesUtils, VRMLErrors;

var
  Scene: TVRMLFlatSceneGL;

procedure Draw(glwin: TGLWindow);
begin
 glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
 glLoadMatrix(glw.Navigator.Matrix);
 Scene.Render(nil);
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
begin
 glViewport(0, 0, glwin.Width, glwin.Height);
 ProjectionGLPerspective(45.0, glwin.Width/glwin.Height,
   Box3dMaxSize(Scene.BoundingBox) * 0.05,
   Box3dMaxSize(Scene.BoundingBox) * 3.0);
end;

begin
 Parameters.CheckHigh(1);
 try
  VRMLNonFatalError := @VRMLNonFatalError_WarningWrite;

  Scene := TVRMLFlatSceneGL.Create(LoadAsVRML(Parameters[1], true),
    true, roSceneAsAWhole);

  Writeln(Scene.Info(true, true));

  { Uncomment this to get some info about scene nodes:
  for i := 0 to Scene.ShapeStates.Count-1 do
   with Scene.ShapeStates[i] do
   begin
    Writeln(Format('Shape name ''%10s'' (''%20s'') : LocalBBox = %50s, BBox = %50s',
      [ShapeNode.NodeName, ShapeNode.NodeTypeName,
       Box3dToNiceStr(LocalBoundingBox), Box3dToNiceStr(BoundingBox)]));
   end;}

  { init Glw.Navigator }
  Glw.Navigator := TMatrixWalker.Create(@Glw.PostRedisplayOnMatrixChanged);
  Glw.NavWalker.Init(Box3dMiddle(scene.BoundingBox),
      VectorAdjustToLength(Vector3Single(1, 0, 0),
        Box3dAvgSize(Scene.BoundingBox) * 0.01*0.4),
      Vector3Single(0, 1, 0), 0.0, 0.0);

  Glw.OnInit := @Init;
  Glw.OnClose := @Close;
  Glw.OnResize := @Resize;
  Glw.InitLoop(ProgramName, @Draw);
 finally Scene.Free end;
end.
