{
  Copyright 2020-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main "playing game" view, where most of the game logic takes place. }
unit GameViewPlay;

interface

uses Classes,
  CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleViewport, CastleScene, CastleVectors, CastleCameras,
  CastleTransform, CastleBehaviors, CastleClassUtils;

type
  { Main "playing game" view, where most of the game logic takes place. }
  TViewPlay = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    MainViewport: TCastleViewport;
    WalkNavigation: TCastleWalkNavigation;
    SceneWolf1: TCastleScene;
  private
    Enemies: TCastleTransformList;
    PlayerAlive: TCastleAliveBehavior;
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
  CastleSoundEngine, CastleLog, CastleStringUtils, CastleFilesUtils,
  GameViewMenu;

{ TViewPlay ----------------------------------------------------------------- }

constructor TViewPlay.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewplay.castle-user-interface';
end;

procedure TViewPlay.Start;

  procedure AddEnemy(const EnemyScene: TCastleScene);
  // TODO MoveAttackBehavior: TCastleMoveAttack;
  begin
    Enemies.Add(EnemyScene);

    // TODO: TCastleMoveAttack should take care of this
    EnemyScene.PlayAnimation('walk', true);

    EnemyScene.AddBehavior(TCastleAliveBehavior.Create(FreeAtStop));

    // TODO
    // MoveAttackBehavior := TCastleMoveAttack.Create(FreeAtStop);
    // MoveAttackBehavior.Enemy := PlayerAlive;
    // EnemyScene.AddBehavior(MoveAttackBehavior);
  end;

var
  I: Integer;
begin
  inherited;

  PlayerAlive := TCastleAliveBehavior.Create(FreeAtStop);
  MainViewport.Camera.AddBehavior(PlayerAlive);

  { Initialize Enemies }
  Enemies := TCastleTransformList.Create(false);
  for I := 1 to 4 do
    AddEnemy(DesignedComponent('SceneSoldier' + IntToStr(I)) as TCastleScene);
  AddEnemy(SceneWolf1);
end;

procedure TViewPlay.Stop;
begin
  FreeAndNil(Enemies);
  inherited;
end;

procedure TViewPlay.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

function TViewPlay.Press(const Event: TInputPressRelease): Boolean;
var
  HitAlive: TCastleAliveBehavior;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  { This virtual method is executed when user presses
    a key, a mouse button, or touches a touch-screen.

    Note that each UI control has also events like OnPress and OnClick.
    These events can be used to handle the "press", if it should do something
    specific when used in that UI control.
    The TViewPlay.Press method should be used to handle keys
    not handled in children controls.
  }

  if Event.IsMouseButton(buttonLeft) then
  begin
    SoundEngine.Play(SoundEngine.SoundFromName('shoot_sound'));

    { We clicked on enemy if
      - TransformUnderMouse indicates we hit something
      - It has a behavior of TCastleAliveBehavior. }
    if (MainViewport.TransformUnderMouse <> nil) and
       (MainViewport.TransformUnderMouse.FindBehavior(TCastleAliveBehavior) <> nil) then
    begin
      // TODO: TCastleMoveAttack should take care of this
      HitAlive := MainViewport.TransformUnderMouse.FindBehavior(TCastleAliveBehavior) as TCastleAliveBehavior;
      HitAlive.Hurt(1000, MainViewport.Camera.Direction);
      if HitAlive.Dead then
      begin
        (HitAlive.Parent as TCastleScene).PlayAnimation('die', false);
        // dead corpse no longer collides
        HitAlive.Parent.Pickable := false;
        HitAlive.Parent.Collides := false;
      end;
    end;

    Exit(true);
  end;

  if Event.IsKey(CtrlM) then
  begin
    WalkNavigation.MouseLook := not WalkNavigation.MouseLook;
    Exit(true);
  end;

  if Event.IsKey(keyF5) then
  begin
    Container.SaveScreenToDefaultFile;
    Exit(true);
  end;

  if Event.IsKey(keyEscape) then
  begin
    Container.View := ViewMenu;
    Exit(true);
  end;
end;

end.
