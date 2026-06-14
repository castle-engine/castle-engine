{
  Copyright 2022-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Playing the game. }
unit GameViewPlay;

interface

uses Classes, CastleCameras,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  CastleViewport, CastleSceneCore, X3DNodes, CastleScene, CastleSoundEngine,
  CastleBehaviors, CastleNotifications, CastleTransform,
  // necessary to deserialize TCastleMoveAttack from design
  CastleLivingBehaviors;

type
  TViewPlay = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    WalkNavigation: TCastleWalkNavigation;
    MainViewport: TCastleViewport;
    SceneGun: TCastleScene;
    SoundSourceFootsteps: TCastleSoundSource;
    SoundShoot: TCastleSound;
    BoxDieDetect, BoxWinDetect: TCastleTransform;
    DesignHud: TCastleDesign;
    MapCamera: TCastleCamera;
    PlayerLiving: TCastleLiving;
  private
    { components in DesignHud }
    LabelFps: TCastleLabel;
    MainNotifications: TCastleNotifications;
    MapViewport: TCastleViewport;

    procedure WeaponShootAnimationStop(const Scene: TCastleSceneCore;
      const Animation: TTimeSensorNode);
    procedure PointerLockUserCancelled(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Resume; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    function Release(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewPlay: TViewPlay;

implementation

uses SysUtils, Math,
  CastleComponentSerialize, CastleLog, CastleGameControllers,
  GameViewWin, GameViewDeath, GameViewOptions;

constructor TViewPlay.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewplay.castle-user-interface';
end;

procedure TViewPlay.Start;
begin
  inherited;

  { initialize components in DesignHud }
  LabelFps := DesignHud.DesignedComponent('LabelFps') as TCastleLabel;
  MainNotifications := DesignHud.DesignedComponent('MainNotifications') as TCastleNotifications;
  MapViewport := DesignHud.DesignedComponent('MapViewport') as TCastleViewport;

  MapViewport.Items := MainViewport.Items;
  MapViewport.Camera := MapCamera;

  Controllers.Initialize;
  WalkNavigation.UseGameController;

  Container.PointerLock.AddUserCancelledListener(
    {$ifdef FPC}@{$endif} PointerLockUserCancelled);
end;

procedure TViewPlay.Stop;
begin
  Container.PointerLock.RemoveUserCancelledListener(
    {$ifdef FPC}@{$endif} PointerLockUserCancelled);
  inherited;
end;

procedure TViewPlay.Resume;
begin
  inherited;

  { This is called when user resumed the game.

    Two ways how we can reach this code path:

    1. User starts playing the game, so engine calls Start, and then Resume.

    2. User presses Escape, which
       - sets WalkNavigation.MouseLook and Container.PointerLock.Active to false
       - only on web: calls PointerLockUserCancelled
       - ... which calls Container.PushView(ViewOptions)
       - user clicks "Resume Game" button in options menu
       - ... which calls Container.PopView(ViewOptions)
       - which calls this method, Resume.

    See https://castle-engine.io/web#pointer_lock .
  }
  WalkNavigation.MouseLook := true;
end;

procedure TViewPlay.Update(const SecondsPassed: Single; var HandleInput: Boolean);
var
  DirectionHorizontal: TVector3;
  GameActive: Boolean;
begin
  inherited;
  LabelFps.Caption := Container.Fps.ToString;

  GameActive := Container.CurrentFrontView = Self;

  SoundSourceFootsteps.Volume := IfThen(WalkNavigation.IsWalkingOnTheGround and GameActive, 1, 0);

  MainViewport.Items.Paused := not GameActive;

  if GameActive then
  begin
    DirectionHorizontal := MainViewport.Camera.Direction;
    if not VectorsParallel(DirectionHorizontal, MainViewport.Camera.GravityUp) then
      MakeVectorsOrthoOnTheirPlane(DirectionHorizontal, MainViewport.Camera.GravityUp);

    MapViewport.Camera.SetView(
      MainViewport.Camera.WorldTranslation + Vector3(0, 30, 0),
      Vector3(0, -1, 0),
      DirectionHorizontal
    );

    if BoxWinDetect.WorldBoundingBox.Contains(MainViewport.Camera.WorldTranslation) then
    begin
      Container.PushView(ViewWin);
      Exit;
    end;

    if BoxDieDetect.WorldBoundingBox.Contains(MainViewport.Camera.WorldTranslation) then
    begin
      Container.PushView(ViewDeath);
      Exit;
    end;
  end;
end;

procedure TViewPlay.WeaponShootAnimationStop(const Scene: TCastleSceneCore;
  const Animation: TTimeSensorNode);
begin
  Scene.PlayAnimation('idle', true);
end;

function TViewPlay.Press(const Event: TInputPressRelease): Boolean;
var
  HitLiving: TCastleLiving;
  PlayAnimationParams: TPlayAnimationParameters;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Container.CurrentFrontView = Self then
  begin
    if Event.IsMouseButton(buttonLeft) then
    begin
      if MainViewport.TransformUnderMouse <> nil then
        WritelnLog('Clicked on ' + MainViewport.TransformUnderMouse.Name);

      PlayAnimationParams := TPlayAnimationParameters.Create;
      try
        PlayAnimationParams.Name := 'primary';
        PlayAnimationParams.StopNotification := {$ifdef FPC}@{$endif} WeaponShootAnimationStop;
        PlayAnimationParams.Loop := false;
        SceneGun.PlayAnimation(PlayAnimationParams);
      finally FreeAndNil(PlayAnimationParams) end;

      SoundEngine.Play(SoundShoot);

      { We clicked on enemy if
        - TransformUnderMouse indicates we hit something
        - It has a behavior of TCastleLiving
        - It's not PlayerLiving. }
      if (MainViewport.TransformUnderMouse <> nil) and
         (MainViewport.TransformUnderMouse.FindBehavior(TCastleLiving) <> nil) then
      begin
        HitLiving := MainViewport.TransformUnderMouse.FindBehavior(TCastleLiving) as TCastleLiving;
        if HitLiving <> PlayerLiving then
        begin
          HitLiving.Hurt(20 + Random(20), MainViewport.Camera.WorldDirection, 0, PlayerLiving);
          MainNotifications.Show('Hit ' + HitLiving.Parent.Name);
        end;
      end;

      Exit(true);
    end;

    if Event.IsKey(keyEscape) then
    begin
      ViewOptions.OverGame := true;
      Container.PushView(ViewOptions);
      Exit(true);
    end;

    if Event.IsKey(keyP) then
    begin
      Container.PushView(ViewWin);
      Exit(true);
    end;

    if Event.IsKey(keyO) then
    begin
      Container.PushView(ViewDeath);
      Exit(true);
    end;
  end;

  if Event.IsKey(keyF5) then
  begin
    MainNotifications.Show('Saved screenshot to ' + Container.SaveScreenToDefaultFile);
    Exit(true);
  end;

  if Event.IsMouseButton(buttonRight) then
  begin
    { Start mouse look.

      Note: we enable/disable mouse look on TViewPlay.Press/Release,
      and we *do not* call in TViewPlay.Update something like
      "WalkNavigation.MouseLook := buttonRight in Container.MousePressed",
      because forcing mouse look in Update would be bad UX on web after
      user cancels pointer lock. See https://castle-engine.io/web#pointer_lock . }
    WalkNavigation.MouseLook := true;
    Exit(true);
  end;
end;

function TViewPlay.Release(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.IsMouseButton(buttonRight) then
  begin
    { Stop mouse look. See comment in Press. }
    WalkNavigation.MouseLook := false;
    Exit(true);
  end;
end;

procedure TViewPlay.PointerLockUserCancelled(Sender: TObject);
begin
  { When user presses Escape on web to cancel pointer lock,
    we want to show options menu. }
  if Container.PendingFrontView = Self then
  begin
    ViewOptions.OverGame := true;
    Container.PushView(ViewOptions);
  end;
end;

end.
