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

{ Low-level demo how to access TVRMLGLScene without a SceneManager, part 2.
  Again, let me emphasize that this is highly discouraged ---
  all new programs should really use our new and great SceneManager.
  This demo remains just to show that it's possible (even if
  uncomfortable) to work without the SceneManager.

  This is the next version of direct_vrmlglscene_test_1.lpr,
  see there for initial comments.

  This version adds:

  1. Collision detection
  - constructing octrees, by "Scene.Spatial := ..."
  - doing collision detection using OctreeCollisions, see MoveAllowed
    and GetHeightAbove methods

  2. Rendering with frustum culling
  (- This is already done by Scene.GLProjection:
    updating Camera.ProjectionMatrix.)
  - using Camera.Frustum and OctreeRendering
    (ssRendering in Scene.Spatial) to render the scene,
    see line "Scene.RenderFrustum"

  3. This also shows how you can force created camera to be of 'WALK'
     type --- just pass 'WALK' to Scene.CreateCamera.
     This turns gravity on and makes camera behave FPS-like.
     We still get a fully-capable capable camera, of TUniversalCamera class.

     Also, just for demo, we set the initial position / direction explicitly
     by code (instead of relying on Viewpoint node in VRML/X3D).

     Note that you could also abandon Scene.CreateCamera completely,
     and just do

       Camera := TWalkCamera.Create(Window);
       Camera.Gravity := true;
       Camera.CameraRadius := 0.1;
       // and possibly some more Camera initialization

  4. Makes FPS timings right after starting the program correct:
  - uses Window.OnBeforeDraw and Scene.PrepareResources
}

program direct_vrmlglscene_test_2;

uses VectorMath, Boxes3D, VRMLNodes, GL, GLU, GLWindow,
  KambiClassUtils, KambiUtils, SysUtils, Classes,
  KambiGLUtils, VRMLScene, VRMLGLScene, Cameras, Base3D,
  ProgressUnit, ProgressConsole, KambiFilesUtils, VRMLErrors, VRMLTriangle;

var
  Window: TGLUIWindow;
  Scene: TVRMLGLScene;
  Camera: TUniversalCamera;

procedure BeforeDraw(Window: TGLWindow);
begin
  Scene.PrepareResources([prRender, prBoundingBox], false);
end;

procedure Draw(Window: TGLWindow);
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadMatrix(Camera.Matrix);
  Scene.RenderFrustum(Camera.Frustum, 1, tgAll);
end;

procedure Open(Window: TGLWindow);
begin
  glEnable(GL_LIGHT0); { headlight }
end;

procedure Close(Window: TGLWindow);
begin
  Scene.GLContextClose;
end;

procedure Resize(Window: TGLWindow);
begin
  { This sets OpenGL projection, also updating Camera.ProjectionMatrix,
    which is important for having correct Camera.Frustum. }
  Scene.GLProjection(Camera, Scene.BoundingBox,
    0, 0, Window.Width, Window.Height);
end;

type
  THelperObj = class
    class function MoveAllowed(Camera: TWalkCamera;
      const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
      const BecauseOfGravity: boolean): boolean;

    class procedure GetHeightAbove(Camera: TWalkCamera;
      out IsAbove: boolean; out AboveHeight: Single; out AboveGround: P3DTriangle);
  end;

class function THelperObj.MoveAllowed(Camera: TWalkCamera;
  const ProposedNewPos: TVector3Single; out NewPos: TVector3Single;
  const BecauseOfGravity: boolean): boolean;
begin
  Result := Scene.OctreeCollisions.MoveAllowed(
    Camera.Position, ProposedNewPos, NewPos, Camera.CameraRadius);
  { Usually, you don't want to allow user to fall outside of your model,
    otherwise user would fall into infinity }
  if Result and BecauseOfGravity then
    Result := Box3DPointInside(NewPos, Scene.BoundingBox);
end;

class procedure THelperObj.GetHeightAbove(Camera: TWalkCamera;
  out IsAbove: boolean; out AboveHeight: Single; out AboveGround: P3DTriangle);
begin
  Scene.OctreeCollisions.GetHeightAbove(
    Camera.Position, Camera.GravityUp,
    IsAbove, AboveHeight, PVRMLTriangle(AboveGround), nil, nil);
end;

begin
  Window := TGLUIWindow.Create(Application);

  Parameters.CheckHigh(1);

  VRMLWarning := @VRMLWarning_Write;
  Scene := TVRMLGLScene.Create(nil);
  try
    Scene.Load(Parameters[1]);

    Writeln(Scene.Info(true, true, false));

    { build octrees }
    Progress.UserInterface := ProgressConsoleInterface;
    Scene.TriangleOctreeProgressTitle := 'Building triangle octree';
    Scene.ShapeOctreeProgressTitle := 'Building Shape octree';
    Scene.Spatial := [ssRendering, ssDynamicCollisions];

    { init camera }
    Camera := Scene.CreateCamera(Window, 'WALK');
    Camera.SetInitialView(
      Vector3Single(0, 0, 0),
      Vector3Single(1, 0, 0),
      Vector3Single(0, 1, 0), false);
    Camera.Walk.GoToInitial;
    Camera.Walk.OnMoveAllowed    := @THelperObj(nil).MoveAllowed;
    Camera.Walk.OnGetHeightAbove := @THelperObj(nil).GetHeightAbove;
    Window.Controls.Add(Camera);

    Window.OnOpen := @Open;
    Window.OnClose := @Close;
    Window.OnResize := @Resize;
    Window.OnBeforeDraw := @BeforeDraw;
    Window.OpenAndRun(ProgramName, @Draw);
  finally Scene.Free end;
end.
