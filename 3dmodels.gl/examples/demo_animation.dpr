{ Demo of TVRMLGLAnimation class.

  Run this passing 2 command-line parameters: filenames of two 3D files
  that have identical structure. Effect: this will display nice animation
  that animates between 1st model and 2nd.

  I prepared some sets of sample models in models/ subdirectory.
  Try these commands
    ./demo_animation models/sphere_1.wrl             models/sphere_2.wrl
    ./demo_animation models/raptor_1.wrl             models/raptor_2.wrl
    ./demo_animation models/gus_1_final.wrl          models/gus_2_final.wrl
    ./demo_animation models/cube_opening_1_final.wrl models/cube_opening_2_final.wrl

  You can navigate in the scene using the standard arrow keys, escape exits.
  (for full list of supported keys -- see view3dscene documentation,
  [http://www.camelot.homedns.org/~michalis/view3dscene.php],
  at Walk navigation method).

  You may notice that the 1st pass of the animation is much slower than the
  following ones, that's because in the 1st pass OpenGL display lists
  are created, in later passes they are only used.
  (Of course this strange effect is avoidable, that's the purpose
  of TVRMLFlatSceneGL.PrepareRender -- in real program you should
  first prepare all animation scenes, because usually user prefers
  to wait some time "while the level is loading" and then have a smooth
  play. I didn't implement this in this demo program for simplicity.)
}

program demo_animation;

uses Math, VectorMath, Boxes3d, VRMLNodes, VRMLOpenGLRenderer, OpenGLh, GLWindow,
  GLW_Navigated, KambiClassUtils, KambiUtils, SysUtils, Classes, Object3dAsVRML,
  KambiGLUtils, VRMLFlatScene, VRMLFlatSceneGL, MatrixNavigation, VRMLGLAnimation,
  KambiFilesUtils;

const
  { How fast animation frames change. }
  AnimationSpeed = 0.01;

  { This is the number of animation frames constructed per one unit of time.
    Increase this to get more smoother animation. }
  ScenesPerTime = 100;

var
  Animation: TVRMLGLAnimation;
  AnimationTime: Float = 0.0;

procedure Draw(glwin: TGLWindow);
var
  RenderTime: Single;
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadMatrix(glw.Navigator.Matrix);

  RenderTime := Frac(AnimationTime);
  If not Odd(Floor(AnimationTime)) then
    { In the even rounds, we run the same animation backwards. }
    RenderTime := 1 - RenderTime;

  Animation.SceneFromTime(RenderTime).Render(nil);
end;

procedure Idle(glwin: TGLWindow);
begin
  AnimationTime += AnimationSpeed * glwin.FpsCompSpeed;
end;

procedure Init(glwin: TGLWindow);
begin
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
end;

procedure Close(glwin: TGLWindow);
begin
  Animation.CloseGL;
end;

procedure Resize(glwin: TGLWindow);
begin
  glViewport(0, 0, glwin.Width, glwin.Height);
  ProjectionGLPerspective(45.0, glwin.Width/glwin.Height,
    Box3dMaxSize(Animation.Scenes[0].BoundingBox) * 0.05,
    Box3dMaxSize(Animation.Scenes[0].BoundingBox) * 3.0);
end;

var
  CamPos, CamDir, CamUp: TVector3Single;
begin
  Parameters.CheckHigh(2);
  try
    VRMLNonFatalError := VRMLNonFatalError_WarningWrite;

    Animation := TVRMLGLAnimation.Create(
      [LoadAsVRML(Parameters[1], false), LoadAsVRML(Parameters[2], false)],
      [0.0, 1.0],
      ScenesPerTime,
      roSceneAsAWhole);

    { get camera from 1st scene in Animation }
    Animation.Scenes[0].GetPerspectiveCamera(CamPos, CamDir, CamUp);

    { init Glw.Navigator }
    Glw.Navigator := TMatrixWalker.Create(Glw.PostRedisplayOnMatrixChanged);
    Glw.NavWalker.Init(CamPos, VectorAdjustToLength(CamDir,
      Box3dAvgSize(Animation.Scenes[0].BoundingBox) * 0.01*0.4), CamUp, 0.0, 0.0);

    Glw.AutoRedisplay := true;
    Glw.OnInit := Init;
    Glw.OnClose := Close;
    Glw.OnResize := Resize;
    Glw.OnIdle := Idle;
    Glw.InitLoop(ProgramName, Draw);
  finally Animation.Free end;
end.
