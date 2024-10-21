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
    PersistentMouseLook: Boolean;

    { components in DesignHud }
    LabelFps: TCastleLabel;
    MainNotifications: TCastleNotifications;
    MapViewport: TCastleViewport;

    procedure UpdateMouseLook;
    procedure WeaponShootAnimationStop(const Scene: TCastleSceneCore;
      const Animation: TTimeSensorNode);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewPlay: TViewPlay;

implementation

uses SysUtils, Math,
  CastleComponentSerialize, CastleLog,
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

  PersistentMouseLook := true;
end;

procedure TViewPlay.Stop;
begin
  inherited;
end;

procedure TViewPlay.Update(const SecondsPassed: Single; var HandleInput: Boolean);
var
  DirectionHorizontal: TVector3;
  GameActive: Boolean;
begin
  inherited;
  LabelFps.Caption := Container.Fps.ToString;
  UpdateMouseLook;

  GameActive := Container.FrontView = Self;

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

procedure TViewPlay.UpdateMouseLook;
begin
  WalkNavigation.MouseLook := (Container.FrontView = Self) and
    ( PersistentMouseLook or
      (buttonRight in Container.MousePressed) );
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

  if Container.FrontView = Self then
  begin
    if Event.IsKey(keyF4) then
    begin
      PersistentMouseLook := not PersistentMouseLook;
      UpdateMouseLook;
      Exit(true);
    end;

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
end;

end.
