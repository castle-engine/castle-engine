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
  DragonFlying: boolean;
  DragonFlyingTarget: TVector2Single;

type
  TButtonsHandler = class
    procedure CameraView3DClick(Sender: TObject);
  end;

procedure TButtonsHandler.CameraView3DClick(Sender: TObject);
const
  AnimateTime = 1.0;
begin
  { since this is really 3D, show alternative camera view where is clearly visible }
  CameraView3D.Pressed := not CameraView3D.Pressed;
  if not CameraView3D.Pressed then
    SceneManager.Camera.AnimateTo(
      { camera values like initialized by T2DSceneManager }
      { pos } Vector3Single(0, 0, 0),
      { dir } Vector3Single(0, 0, -1),
      { up } Vector3Single(0, 1, 0),
      AnimateTime) else
    SceneManager.Camera.AnimateTo(
      { hint: to pick camera values experimentally, use view3dscene
        and Console->Print Current Camera.. menu item }
      { pos } Vector3Single(8.8673858642578125, 1.2955703735351563, -19.951961517333984),
      { dir } Vector3Single(0.6533171534538269, -0.13534677028656006, -0.7448880672454834),
      { up } Vector3Single(0.10390207171440125, 0.99060958623886108, -0.088865458965301514),
      AnimateTime);
end;

{ One-time initialization. }
procedure ApplicationInitialize;
const
  DragonScale = 0.05;
begin
  SceneManager := T2DSceneManager.Create(Application);
  { show SceneManager.BackgroundColor underneath scene manager }
  SceneManager.Transparent := false;
  Window.Controls.InsertFront(SceneManager);

  { add to scene manager an X3D scene with background and trees.
    See data/background.x3dv (go ahead, open it in a text editor --- X3D files
    can be easily created and edited as normal text files) for what it does.

    This is just one way to create a background, out of many!
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

  { We always want to see full height of background.x3dv,
    we know it starts from bottom = 0.
    BoudingBox.Data[1][1] is the maximum Y value, i.e. our height.
    So projection height should adjust to background.x3dv height. }
  SceneManager.ProjectionAutoSize := false;
  SceneManager.ProjectionHeight := Background.BoundingBox.Data[1][1];

  DragonTransform := T3DTransform.Create(Application);
  DragonTransform.Scale := Vector3Single(DragonScale, DragonScale, DragonScale);
  { translate in XY to set initial position in the middle of the screen.
    translate in Z to push dragon in front of trees
    (on Z = 20, see data/background.x3dv) }
  DragonTransform.Translation := Vector3Single(60, 40, 40);
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
end;

procedure WindowUpdate(Container: TUIContainer);

  procedure ProcessInputs;
  begin
    { capture input by looking at Background.PointingDeviceActive.
      They are other ways to detect clicks, for example checking
      Event.IsMouseButton(mbLeft) inside WindowPress callback.
      Other ways are overriding TCastleSceneManager.PointingDeviceActivate.

      Using Background.PointingDeviceActive is comfortable for us here,
      as Background.PointingDeviceOverItem and
      Background.PointingDeviceOverPoint give us ready positions in our
      world. We do not have to care of translating mouse positions
      into world positions (in case camera moves over the world),
      it is already done for us. }
    if Background.PointingDeviceActive then
    begin
      if not DragonFlying then
        Dragon.PlayAnimation('flying', paForceLooping);
      DragonFlying := true;
      DragonFlyingTarget := Vector2Single(
        { ignore 3rd dimension from Background.PointingDeviceOverPoint }
        Background.PointingDeviceOverPoint[0],
        Background.PointingDeviceOverPoint[1]);
    end;
  end;

  procedure Fly;
  const
    SpeedX = 50.0;
    SpeedY = 25.0;
  var
    SecondsPassed: Single;
    T: TVector3Single;
  begin
    if DragonFlying then
    begin
      { update DragonTransform.Translation to reach DragonFlyingTarget.
        Be careful to not overshoot, and to set DragonFlying to false when
        necessary. }
      T := DragonTransform.Translation;
      SecondsPassed := Container.Fps.UpdateSecondsPassed;
      if T[0] < DragonFlyingTarget[0] then
        T[0] := Min(DragonFlyingTarget[0], T[0] + SpeedX * SecondsPassed) else
        T[0] := Max(DragonFlyingTarget[0], T[0] - SpeedX * SecondsPassed);
      if T[1] < DragonFlyingTarget[1] then
        T[1] := Min(DragonFlyingTarget[1], T[1] + SpeedY * SecondsPassed) else
        T[1] := Max(DragonFlyingTarget[1], T[1] - SpeedY * SecondsPassed);
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
  end;

begin
  ProcessInputs;
  Fly;
end;

procedure WindowPress(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.IsKey(K_F5) then
    Window.SaveScreen(FileNameAutoInc(ApplicationName + '_screen_%d.png'));
  if Event.IsKey(K_Escape) then
    Application.Quit;
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
