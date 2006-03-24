{ Demo of TVRMLGLAnimation class.

  Run this passing an even number of command-line parameters.
  Each parameters pair specifies scene filename, and position in time
  of this scene. Scenes must be specified in increasing order of time.
  Time is in seconds.

  Effect: this will display animation going from 1st scene to the 2nd,
  then to the 3rd etc. to the last scene. And then it will
  go back again to the 1st scene.

  I prepared some sets of sample models in models/ subdirectory.
  Example commands with two scenes:
    ./demo_animation models/sphere_1.wrl 0 models/sphere_2.wrl 1
    ./demo_animation models/raptor_1.wrl 0 models/raptor_2.wrl 1
    ./demo_animation models/gus_1_final.wrl 0 models/gus_2_final.wrl 1
    ./demo_animation models/cube_opening_1_final.wrl 0 models/cube_opening_2_final.wrl 1

  Additional command-line options are:
    --loop
    --backwards
    --no-loop
    --no-backwards
  For precise meaning, see TVRMLGLAnimation documentation.
  In short, --loop causes animation to loop and --backwards causes
  animation to go backward after going forward.
  The default is --loop --no-backwards.

  Example command with more scenes:
    ./demo_animation models/gus_1_final.wrl 0 \
                     models/gus_2_final.wrl 1 \
                     models/gus_3_final.wrl 1.5 --backwards

  This is all implemented in TVRMLGLAnimation class, see docs of this class
  for precise description how things work.

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
  KambiFilesUtils, ParseParametersUnit;

const
  { This is the number of animation frames constructed per one unit of time.
    Increase this to get smoother animation. }
  ScenesPerTime = 100;

var
  Animation: TVRMLGLAnimation;
  AnimationTime: Float = 0.0;

procedure Draw(glwin: TGLWindow);
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadMatrix(glw.Navigator.Matrix);

  Animation.SceneFromTime(AnimationTime).Render(nil);
end;

procedure Idle(glwin: TGLWindow);
begin
  AnimationTime += glwin.FpsCompSpeed / 50;
end;

procedure Init(glwin: TGLWindow);
begin
  glEnable(GL_LIGHTING);
  glEnable(GL_LIGHT0);
  AnimationTime := Animation.TimeBegin;
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
  AnimTimeLoop: boolean = true;
  AnimTimeBackwards: boolean = false;

const
  Options: array[0..3] of TOption =
  (
    (Short:  #0; Long: 'loop'; Argument: oaNone),
    (Short:  #0; Long: 'backwards'; Argument: oaNone),
    (Short:  #0; Long: 'no-loop'; Argument: oaNone),
    (Short:  #0; Long: 'no-backwards'; Argument: oaNone)
  );

  procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
    const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
  begin
    case OptionNum of
      0: AnimTimeLoop := true;
      1: AnimTimeBackwards := true;
      2: AnimTimeLoop := false;
      3: AnimTimeBackwards := false;
      else raise EInternalError.Create('OptionProc');
    end;
  end;

var
  CamPos, CamDir, CamUp: TVector3Single;
  AnimRootNodes: array of TVRMLNode;
  AnimTimes: array of Single;
  I: Integer;
begin
  Glw.ParseParameters(StandardParseOptions);
  ParseParameters(Options, OptionProc, nil);

  { parse parameters to AnimRootNodes and AnimTimes }
  if Odd(Parameters.High) then
    raise EInvalidParams.Create('You must supply even number of paramaters: ' +
      '2 parameters "<scene> <time>" for each frame');
  SetLength(AnimRootNodes, Parameters.High div 2);
  SetLength(AnimTimes    , Parameters.High div 2);
  for I := 0 to Parameters.High div 2 - 1 do
  begin
    AnimRootNodes[I] := LoadAsVRML(Parameters[(I+1) * 2 - 1], false);
    AnimTimes[I] := StrToFloat(Parameters[(I+1) * 2]);
  end;

  try
    VRMLNonFatalError := VRMLNonFatalError_WarningWrite;

    Animation := TVRMLGLAnimation.Create(
      AnimRootNodes, AnimTimes, ScenesPerTime, roSceneAsAWhole);
    Animation.TimeLoop := AnimTimeLoop;
    Animation.TimeBackwards := AnimTimeBackwards;

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
