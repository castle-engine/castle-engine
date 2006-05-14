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

  Also --renderer-optimization option is accepted, just like for
  view3dscene (see view3dscene --help). Default is
  separate-shape-states-no-transform, as this is the best
  for TVRMLGLAnimation.

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
  Space key restarts the animation (definitely useful if you passed
  --no-loop option).

  At the beginning there is some preprocessing time
  ("Preparing animation") when we create display lists,
  to make future animation run smoothly.
  That's done by TVRMLGLAnimation.PrepareRender.
}

program demo_animation;

uses Math, VectorMath, Boxes3d, VRMLNodes, VRMLOpenGLRenderer, OpenGLh, GLWindow,
  GLW_Navigated, KambiClassUtils, KambiUtils, SysUtils, Classes, Object3dAsVRML,
  KambiGLUtils, VRMLFlatScene, VRMLFlatSceneGL, MatrixNavigation, VRMLGLAnimation,
  KambiFilesUtils, ParseParametersUnit, ProgressGL, ProgressUnit;

const
  { This is the number of animation frames constructed per one unit of time.
    Increase this to get smoother animation. }
  ScenesPerTime = 50;

  { EqualityEpsilon used to marge nodes when creating animation.
    Larger values may speed up animation loading time and save memory use. }
  EqualityEpsilon = 0.001;

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
  AnimationTime += glwin.IdleCompSpeed / 50;
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
    Box3dMaxSize(Animation.Scenes[0].BoundingBox) * 10.0);
end;

procedure KeyDown(Glwin: TGLWindow; Key: TKey; C: char);
begin
  if C = ' ' then
    AnimationTime := 0.0;
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
  Param_RendererOptimization: TGLRendererOptimization =
    roSeparateShapeStatesNoTransform;
begin
  Glw.ParseParameters(StandardParseOptions);
  RendererOptimizationOptionsParse(Param_RendererOptimization);
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
      AnimRootNodes, true,
      AnimTimes, ScenesPerTime, roSeparateShapeStatesNoTransform,
      EqualityEpsilon);
    Animation.TimeLoop := AnimTimeLoop;
    Animation.TimeBackwards := AnimTimeBackwards;

    { get camera from 1st scene in Animation }
    Animation.Scenes[0].GetPerspectiveCamera(CamPos, CamDir, CamUp);

    { init Glw.Navigator }
    Glw.Navigator := TMatrixWalker.Create(Glw.PostRedisplayOnMatrixChanged);
    Glw.NavWalker.Init(CamPos, VectorAdjustToLength(CamDir,
      Box3dAvgSize(Animation.Scenes[0].BoundingBox) * 0.01*0.4), CamUp, 0.0, 0.0);

    ProgressGLInterface.Window := Glw;
    Progress.UserInterface := ProgressGLInterface;

    Glw.AutoRedisplay := true;
    Glw.OnInit := Init;
    Glw.OnClose := Close;
    Glw.OnResize := Resize;
    Glw.OnIdle := Idle;
    Glw.OnKeyDown := KeyDown;
    Glw.Caption := ProgramName;
    Glw.OnDraw := Draw;

    Glw.Init;

    Progress.Init(Animation.ScenesCount, 'Preparing animation');
    try
      Animation.PrepareRender(true, true, false, false, true, false);
    finally Progress.Fini end;

    Glwm.Loop;
  finally Animation.Free end;
end.
