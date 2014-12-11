{
  Copyright 2014-2014 Michalis Kamburelis.

  This file is part of "Castle Spine".

  "Castle Spine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Spine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Implements the game logic, independent from Android / standalone. }
unit Game;

interface

uses CastleWindow;

var
  Window: TCastleWindowCustom;

implementation

uses SysUtils,
  CastleControls, CastleKeysMouse, CastleFilesUtils, Castle2DSceneManager,
  CastleVectors, Castle3D, CastleSceneCore, CastleUtils;

var
  SceneManager: T2DSceneManager;
  Background: T2DScene;
  DragonTransform: T3DTransform;
  Dragon: T2DScene;
  CameraView3D: TCastleButton;
  CameraFollowsDragon: TCastleButton;
  DragonFlying: boolean;
  DragonFlyingTarget: TVector2Single;

type
  TButtonsHandler = class
    procedure CameraView3DClick(Sender: TObject);
    procedure CameraFollowsDragonClick(Sender: TObject);
  end;

const
  DragonInitialPosition: TVector3Single = (4800, 600, 400);
  DragonSpeedX = 1000.0;
  DragonSpeedY =  500.0;
  DragonScale = 0.5;

procedure AddBackgroundItems;

  { Easily add a Spine animation, translated and scaled,
    and run it's animation. Path is processed by ApplicationData,
    so it used slahes and is relative to application data directory. }
  procedure AddItem(const X, Y, Z, Scale: Single; const Path: string);
  var
    Transform: T3DTransform;
    Scene: T2DScene;
  begin
    Transform := T3DTransform.Create(Application);
    Transform.Scale := Vector3Single(Scale, Scale, Scale);
    Transform.Translation := Vector3Single(X, Y, Z);
    SceneManager.Items.Add(Transform);

    Scene := T2DScene.Create(Application);
    Transform.Add(Scene);
    Scene.Load(ApplicationData(Path));
    Scene.ProcessEvents := true;
    Scene.PlayAnimation('animation', paForceLooping);
  end;

begin
  { z = 200 to place in front, only behind dragon }
  AddItem(3700, 0, 200, 0.75, 'trees/tree1.json');
  AddItem(3700, 0, 200, 0.78, 'trees/tree2.json');
  AddItem(3700, 0, 200, 0.71, 'trees/tree3.json');
  { z = 50 to place between background tower and background trees }
  AddItem(0,    0,  50, 1, 'background/smoktlo2.json');
  SceneManager.Items.SortZ;
end;

{ One-time initialization. }
procedure ApplicationInitialize;
begin
  SceneManager := T2DSceneManager.Create(Application);
  { show SceneManager.BackgroundColor underneath scene manager }
  SceneManager.Transparent := false;
  Window.Controls.InsertFront(SceneManager);

  { add to scene manager an X3D scene with background and trees.
    See data/background.x3dv (go ahead, open it in a text editor --- X3D files
    can be easily created and edited as normal text files) for what it does.

    This is just one way to create a background for 2D game, there are many others!
    Some alternatives: you could use a normal 2D UI for a background,
    like TCastleSimpleBackground or TCastleImageControl instead of X3D model.
    Or you could load a scene from any format --- e.g. your background
    could also be a Spine scene. }
  Background := T2DScene.Create(Application);
  SceneManager.Items.Add(Background);
  SceneManager.MainScene := Background;
  Background.Load(ApplicationData('background.x3dv'));
  { not really necessary now, but in case some animations will appear
    on Background }
  Background.ProcessEvents := true;
  { this is useful to have precise collisions (not just with bounding box),
    which in turn is useful here for Background.PointingDeviceOverPoint value }
  Background.Spatial := [ssRendering, ssDynamicCollisions];

  AddBackgroundItems;

  { We always want to see full height of background.x3dv,
    we know it starts from bottom = 0.
    BoudingBox.Data[1][1] is the maximum Y value, i.e. our height.
    So projection height should adjust to background.x3dv height. }
  SceneManager.ProjectionAutoSize := false;
  SceneManager.ProjectionHeight := Background.BoundingBox.Data[1][1];
  SceneManager.ProjectionSpan := 10000.0;

  DragonTransform := T3DTransform.Create(Application);
  DragonTransform.Scale := Vector3Single(DragonScale, DragonScale, DragonScale);
  { translate in XY to set initial position in the middle of the screen.
    translate in Z to push dragon in front of trees
    (on Z = 20, see data/background.x3dv) }
  DragonTransform.Translation := DragonInitialPosition;
  SceneManager.Items.Add(DragonTransform);

  Dragon := T2DScene.Create(Application);
  DragonTransform.Add(Dragon);
  Dragon.Load(ApplicationData('dragon/dragon.json'));
  Dragon.ProcessEvents := true;
  Dragon.PlayAnimation('idle', paForceLooping);

  CameraView3D := TCastleButton.Create(Window);
  CameraView3D.Caption := '3D Camera View';
  CameraView3D.OnClick := @TButtonsHandler(nil).CameraView3DClick;
  CameraView3D.Toggle := true;
  CameraView3D.Left := 10;
  CameraView3D.Bottom := 10;
  Window.Controls.InsertFront(CameraView3D);

  CameraFollowsDragon := TCastleButton.Create(Window);
  CameraFollowsDragon.Caption := 'Camera Follows Dragon';
  CameraFollowsDragon.OnClick := @TButtonsHandler(nil).CameraFollowsDragonClick;
  CameraFollowsDragon.Toggle := true;
  CameraFollowsDragon.Left := 10;
  CameraFollowsDragon.Bottom := 60;
  Window.Controls.InsertFront(CameraFollowsDragon);
end;

{ Looking at current state of CameraView3D.Pressed
  and CameraFollowsDragon.Pressed, calculate camera vectors. }
procedure CalculateCamera(out Pos, Dir, Up: TVector3Single);
const
  { camera X position limits, just to avoid showing blackness underneath }
  MinX = 113;
  MaxX = 4980;
begin
  if not CameraView3D.Pressed then
  begin
    { initial camera, like initialized by T2DSceneManager,
      but shifted to see the middle of the background scene,
      to see dragon at initial position }
    Pos := Vector3Single(3800, 0, 0);
    Dir := Vector3Single(0, 0, -1);
    Up  := Vector3Single(0, 1, 0);
  end else
  begin
    { show alternative camera view where it is clearly visible we are in 3D :) }
    { hint: to pick camera values experimentally, use view3dscene
      and Console->Print Current Camera.. menu item. }
    Pos := Vector3Single(329.62554931640625, 581.32476806640625, 2722.44921875);
    Dir := Vector3Single(0.6533169150352478, -0.13534674048423767, -0.7448880672454834);
    Up  := Vector3Single(0.10390279442071915, 0.99060952663421631, -0.088864780962467194);
  end;
  if CameraFollowsDragon.Pressed then
    Pos[0] += (DragonTransform.Translation[0] - DragonInitialPosition[0]);
  if not CameraView3D.Pressed then
    Pos[0] := Clamped(Pos[0], MinX, MaxX);
  //Writeln(Pos[0]:1:10);
end;

procedure WindowUpdate(Container: TUIContainer);
var
  SecondsPassed: Single;
  T: TVector3Single;
  Pos, Dir, Up: TVector3Single;
begin
  if { check SceneManager.Camera existence, because in this game
       we just depend on SceneManager creating camera automatically,
       so we should not depend that it exists early, like at 1st OnUpdate.
       Alternatively, we could assign camera, e.g.
       SceneManager.Camera := SceneManager.CreateDefaultCamera,
       it ApplicationInitialize. }
     (SceneManager.Camera = nil) or
     { check SceneManager.Camera.Animation, to not mess in the middle
       of Camera.AnimateTo (we could mess it by changing DragonTransform now
       or by calling Camera.SetView directly) }
     SceneManager.Camera.Animation then
    Exit;

  if DragonFlying then
  begin
    { update DragonTransform.Translation to reach DragonFlyingTarget.
      Be careful to not overshoot, and to set DragonFlying to false when
      necessary. }
    T := DragonTransform.Translation;
    SecondsPassed := Container.Fps.UpdateSecondsPassed;
    if T[0] < DragonFlyingTarget[0] then
      T[0] := Min(DragonFlyingTarget[0], T[0] + DragonSpeedX * SecondsPassed) else
      T[0] := Max(DragonFlyingTarget[0], T[0] - DragonSpeedX * SecondsPassed);
    if T[1] < DragonFlyingTarget[1] then
      T[1] := Min(DragonFlyingTarget[1], T[1] + DragonSpeedY * SecondsPassed) else
      T[1] := Max(DragonFlyingTarget[1], T[1] - DragonSpeedY * SecondsPassed);
    DragonTransform.Translation := T;

    { check did we reach the target. Note that we can compare floats
      using exact "=" operator (no need to use FloatsEqual), because
      our Min/Maxes above make sure that we will reach the *exact* target
      at some point. }
    if (T[0] = DragonFlyingTarget[0]) and
       (T[1] = DragonFlyingTarget[1]) then
    begin
      DragonFlying := false;
      Dragon.PlayAnimation('idle', paForceLooping);
    end;
  end;

  { move camera, in case CameraFollowsDragon.Pressed.
    Do it in every update, to react to window resize and to DragonTransform
    changes. }
  CalculateCamera(Pos, Dir, Up);
  SceneManager.Camera.SetView(Pos, Dir, Up);
end;

procedure WindowPress(Container: TUIContainer; const Event: TInputPressRelease);
var
  S: TVector3Single;
begin
  if Event.IsKey(K_F5) then
    Window.SaveScreen(FileNameAutoInc(ApplicationName + '_screen_%d.png'));
  if Event.IsKey(K_Escape) then
    Application.Quit;

  if Event.IsMouseButton(mbLeft) then
  begin
    { The mouse click position is in Event.Position,
      but instead we look at Background.PointingDeviceOverPoint that
      contains a ready position in our world coordinates.
      So we do not have to care about translating mouse positions
      into world positions (in case camera moves over the world),
      it is already done for us. }
    if { check "PointingDeviceOverItem <> nil" before accessing
         Background.PointingDeviceOverPoint, because when we're in 3D-like view
         (when CameraView3D.Pressed) then user can press on empty black space
         outside of our space. }
       (Background.PointingDeviceOverItem <> nil) then
    begin
      if not DragonFlying then
        Dragon.PlayAnimation('flying', paForceLooping);
      DragonFlying := true;
      DragonFlyingTarget := Vector2Single(
        { ignore 3rd dimension from Background.PointingDeviceOverPoint }
        Background.PointingDeviceOverPoint[0],
        Background.PointingDeviceOverPoint[1]);

      { force scale in X to be negative or positive, to easily make
        flying left/right animations from single "flying" animation. }
      S := DragonTransform.Scale;
      if DragonFlyingTarget[0] > DragonTransform.Translation[0] then
        S[0] := -Abs(S[0]) else
        S[0] := Abs(S[0]);
      DragonTransform.Scale := S;
    end;
  end;
end;

procedure TButtonsHandler.CameraView3DClick(Sender: TObject);
var
  Pos, Dir, Up: TVector3Single;
begin
  if not SceneManager.Camera.Animation then { do not mess when Camera.AnimateTo is in progress }
  begin
    CameraView3D.Pressed := not CameraView3D.Pressed;
    CalculateCamera(Pos, Dir, Up);
    SceneManager.Camera.AnimateTo(Pos, Dir, Up, 1.0);
  end;
end;

procedure TButtonsHandler.CameraFollowsDragonClick(Sender: TObject);
var
  Pos, Dir, Up: TVector3Single;
begin
  if not SceneManager.Camera.Animation then { do not mess when Camera.AnimateTo is in progress }
  begin
    CameraFollowsDragon.Pressed := not CameraFollowsDragon.Pressed;
    CalculateCamera(Pos, Dir, Up);
    SceneManager.Camera.AnimateTo(Pos, Dir, Up, 1.0);
  end;
end;

function MyGetApplicationName: string;
begin
  Result := 'castle_spine';
end;

initialization
  { This should be done as early as possible to mark our log lines correctly. }
  OnGetApplicationName := @MyGetApplicationName;

  { initialize Application callbacks }
  Application.OnInitialize := @ApplicationInitialize;

  { create Window and initialize Window callbacks }
  Window := TCastleWindowCustom.Create(Application);
  Window.OnPress := @WindowPress;
  Window.OnUpdate := @WindowUpdate;
  Window.FpsShowOnCaption := true;
  Application.MainWindow := Window;
end.
