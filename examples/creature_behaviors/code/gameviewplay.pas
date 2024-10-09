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

  { Mouse look always active.
    In this case MainViewport.TransformUnderMouse queries always screen middle,
    which is what we want.
    We display also always crosshair in the middle of the screen.

    Note: If we wanted to use mouse cursor position instead of screen middle,
    this is also possible.
    - We can hide Crosshair1 (Crosshair1.Exists:=false or just remove it from the design)
    - We can leave WalkNavigation.MouseLook := false
    - We can query hit at any screen position,
      use

        Viewport.PositionToRay(Container.MousePosition, true, RayOrigin, RayDirection);
        RayCollision := Viewport.Items.WorldRay(RayOrigin, RayDirection);
        if RayCollision <> nil then
        try
          HitTransform := RayCollision.Transform;
          if HitTransform <> nil then
            ...
        finally
          FreeAndNil(RayCollision);
        end;
  }
  WalkNavigation.MouseLook := true;
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
end;

function TViewPlay.Press(const Event: TInputPressRelease): Boolean;
var
  HitLiving: TCastleLiving;
  EnemyScene: TCastleScene;
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
      - TransformUnderMouse indicates we hit something
      - It has a behavior of TCastleLiving. }
    if (MainViewport.TransformUnderMouse <> nil) and
       (MainViewport.TransformUnderMouse.FindBehavior(TCastleLiving) <> nil) then
    begin
      HitLiving := MainViewport.TransformUnderMouse.FindBehavior(TCastleLiving) as TCastleLiving;
      HitLiving.Hurt(1000, MainViewport.Camera.WorldDirection);
      if HitLiving.Dead then
      begin
        EnemyScene := MainViewport.TransformUnderMouse as TCastleScene;
        // dead corpse no longer collides
        EnemyScene.Pickable := false;
        EnemyScene.Collides := false;
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
