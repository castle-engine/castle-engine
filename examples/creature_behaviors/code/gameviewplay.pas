{
  Copyright 2020-2024 Michalis Kamburelis.

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
  CastleTransform, CastleBehaviors, CastleLivingBehaviors, CastleClassUtils,
  CastleSoundEngine, CastleFlashEffect;

type
  { Main "playing game" view, where most of the game logic takes place. }
  TViewPlay = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    MainViewport: TCastleViewport;
    WalkNavigation: TCastleWalkNavigation;
    SoundShoot: TCastleSound;
    PlayerHurtFlash: TCastleFlashEffect;
    LabelPlayerLife: TCastleLabel;
    PlayerLiving: TCastleLiving;
  private
    procedure PlayerHurt(Sender: TObject);
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
  CastleLog, CastleStringUtils, CastleFilesUtils, CastleColors, CastleUtils,
  GameViewMenu;

{ TViewPlay ----------------------------------------------------------------- }

constructor TViewPlay.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewplay.castle-user-interface';
end;

procedure TViewPlay.Start;
begin
  inherited;
  PlayerLiving.OnHurt := {$ifdef FPC}@{$endif} PlayerHurt;
end;

procedure TViewPlay.Stop;
begin
  inherited;
end;

procedure TViewPlay.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
  LabelPlayerLife.Caption := FormatDot('Life: %f', [PlayerLiving.Life]);

  { Mouse look only active when right mouse button pressed.
    This is nice for demo, allows to release mouse look easily.
    For normal FPS games, you usually just keep MouseLook always true
    during play, and only release it during a "pause" menu or such. }
  WalkNavigation.MouseLook := buttonRight in Container.MousePressed;
end;

function TViewPlay.Press(const Event: TInputPressRelease): Boolean;
var
  HitTransform: TCastleTransform;
  HitLiving: TCastleLiving;
  HitScene: TCastleScene;
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
    SoundEngine.Play(SoundShoot);

    { We clicked on enemy if
      - MainViewport.TransformHit(...) indicates we hit something
      - It has a behavior of TCastleLiving.

      Note: we use TransformHit, not TransformUnderMouse,
      because we want to shoot from viewport center, not from the mouse cursor
      (regardless if MouseLook is on or off). We consistently always show
      crosshair in the screen center. }
    HitTransform := MainViewport.TransformHit(MainViewport.RenderRect.Center, true);
    if (HitTransform <> nil) and
       (HitTransform.FindBehavior(TCastleLiving) <> nil) then
    begin
      HitLiving := HitTransform.FindBehavior(TCastleLiving) as TCastleLiving;
      HitLiving.Hurt(1000, MainViewport.Camera.WorldDirection);
      if HitLiving.Dead then
      begin
        HitScene := HitTransform as TCastleScene;
        // dead corpse no longer collides
        HitScene.Pickable := false;
        HitScene.Collides := false;
      end;
    end;

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

procedure TViewPlay.PlayerHurt(Sender: TObject);
begin
  PlayerHurtFlash.Flash(Red, true);
  if PlayerLiving.Attacker <> nil then
    WritelnLog('Player hurt by ' + PlayerLiving.Attacker.Name);

  // TODO: play some hurt sound
  //SoundEngine.Play(SoundHurt);
end;

end.
