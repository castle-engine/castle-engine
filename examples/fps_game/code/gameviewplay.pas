{
  Copyright 2022-2022 Michalis Kamburelis.

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
  GameEnemy;

type
  TViewPlay = class(TCastleView)
  private
    PersistentMouseLook: Boolean;
    Enemies: TEnemyList;
    procedure UpdateMouseLook;
    procedure WeaponShootAnimationStop(const Scene: TCastleSceneCore;
      const Animation: TTimeSensorNode);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    WalkNavigation: TCastleWalkNavigation;
    LabelFps: TCastleLabel;
    MainViewport, MapViewport: TCastleViewport;
    SceneGun: TCastleScene;
    SoundSourceFootsteps: TCastleSoundSource;
    SoundShoot: TCastleSound;
    MainNotifications: TCastleNotifications;
    BoxDieDetect, BoxWinDetect: TCastleTransform;
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
var
  SceneName: String;
  SceneEnemy: TCastleScene;
  Enemy: TEnemy;
  I: Integer;
begin
  inherited;

  Enemies := TEnemyList.Create(true);

  for I := 1 to 7 do
  begin
    SceneName := 'SceneKnight' + IntToStr(I);
    try
      SceneEnemy := DesignedComponent(SceneName) as TCastleScene;
      Enemy := TEnemy.Create(FreeAtStop);
      SceneEnemy.AddBehavior(Enemy);
    except
      on EComponentNotFound do
        WritelnWarning('Cannot find "%s"', [SceneName]);
    end;
  end;

  MapViewport.Items := MainViewport.Items;
end;

procedure TViewPlay.Stop;
begin
  FreeAndNil(Enemies);
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
  HitEnemy: TEnemy;
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
        - It has a behavior of TEnemy. }
      if (MainViewport.TransformUnderMouse <> nil) and
         (MainViewport.TransformUnderMouse.FindBehavior(TEnemy) <> nil) then
      begin
        HitEnemy := MainViewport.TransformUnderMouse.FindBehavior(TEnemy) as TEnemy;
        HitEnemy.Hurt;
        MainNotifications.Show('Killed ' + HitEnemy.Parent.Name);
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
