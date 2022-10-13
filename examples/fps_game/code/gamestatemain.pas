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

{ Main state handles playing the game. }
unit GameStateMain;

interface

uses Classes, CastleCameras,
  CastleVectors, CastleUIState, CastleUIControls, CastleControls, CastleKeysMouse,
  CastleViewport, CastleSceneCore, X3DNodes, CastleScene,
  GameEnemy;

type
  TStateMain = class(TUIState)
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
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils,
  CastleComponentSerialize, CastleLog;

constructor TStateMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemain.castle-user-interface';
end;

procedure TStateMain.Start;
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

procedure TStateMain.Stop;
begin
  FreeAndNil(Enemies);
  inherited;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
var
  DirectionHorizontal: TVector3;
begin
  inherited;
  LabelFps.Caption := Container.Fps.ToString;
  UpdateMouseLook;

  DirectionHorizontal := MainViewport.Camera.Direction;
  if not VectorsParallel(DirectionHorizontal, MainViewport.Camera.GravityUp) then
    MakeVectorsOrthoOnTheirPlane(DirectionHorizontal, MainViewport.Camera.GravityUp);

  MapViewport.Camera.SetView(
    MainViewport.Camera.Translation + Vector3(0, 30, 0),
    Vector3(0, -1, 0),
    DirectionHorizontal
  );
end;

procedure TStateMain.UpdateMouseLook;
begin
  WalkNavigation.MouseLook := PersistentMouseLook or (buttonRight in Container.MousePressed);
end;

procedure TStateMain.WeaponShootAnimationStop(const Scene: TCastleSceneCore;
  const Animation: TTimeSensorNode);
begin
  Scene.PlayAnimation('idle', true);
end;

function TStateMain.Press(const Event: TInputPressRelease): Boolean;
var
  HitEnemy: TEnemy;
  PlayAnimationParams: TPlayAnimationParameters;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

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

    { We clicked on enemy if
      - TransformUnderMouse indicates we hit something
      - It has a behavior of TEnemy. }
    if (MainViewport.TransformUnderMouse <> nil) and
       (MainViewport.TransformUnderMouse.FindBehavior(TEnemy) <> nil) then
    begin
      HitEnemy := MainViewport.TransformUnderMouse.FindBehavior(TEnemy) as TEnemy;
      HitEnemy.Hurt;
    end;

    Exit(true);
  end;
end;

end.
