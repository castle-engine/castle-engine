{
  Copyright 2014-2023 Michalis Kamburelis.

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
  CastleKeysMouse, CastleViewport, CastleScene, CastleVectors;

type
  { Main "playing game" view, where most of the game logic takes place. }
  TViewPlay = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    MainViewport: TCastleViewport;
    SceneDragon: TCastleScene;
    CheckboxCameraFollow: TCastleCheckbox;
    ButtonShowAchievements: TCastleButton;
  private
    { DragonFlying and DragonFlyingTarget manage currect dragon (SceneDragon)
      animation and it's movement. }
    DragonFlying: Boolean;
    DragonFlyingTarget: TVector2;
    procedure ChangeCheckboxCameraFollow(Sender: TObject);
    procedure ClickShowAchievements(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewPlay: TViewPlay;

implementation

uses SysUtils, Math,
  GameViewMenu, GameAchievements;

{ TViewPlay ----------------------------------------------------------------- }

constructor TViewPlay.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewplay.castle-user-interface';
end;

procedure TViewPlay.Start;
begin
  inherited;
  CheckboxCameraFollow.OnChange := {$ifdef FPC}@{$endif} ChangeCheckboxCameraFollow;
  ButtonShowAchievements.OnClick := {$ifdef FPC}@{$endif} ClickShowAchievements;
end;

procedure TViewPlay.Update(const SecondsPassed: Single; var HandleInput: Boolean);
const
  DragonSpeed: TVector2 = (X: 3000; Y: 1500);
var
  T: TVector2;
  CamPos: TVector3;
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }

  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  if DragonFlying then
  begin
    { Update SceneDragon.TranslationXY to reach DragonFlyingTarget. }
    T := SceneDragon.TranslationXY;
    if T.X < DragonFlyingTarget.X then
      T.X := Min(DragonFlyingTarget.X, T.X + DragonSpeed.X * SecondsPassed)
    else
      T.X := Max(DragonFlyingTarget.X, T.X - DragonSpeed.X * SecondsPassed);
    if T.Y < DragonFlyingTarget.Y then
      T.Y := Min(DragonFlyingTarget.Y, T.Y + DragonSpeed.Y * SecondsPassed)
    else
      T.Y := Max(DragonFlyingTarget.Y, T.Y - DragonSpeed.Y * SecondsPassed);
    SceneDragon.TranslationXY := T;

    { Check did we reach the DragonFlyingTarget. Note that we can compare floats
      using exact "=" operator (no need to use SameValue), because
      our Min/Maxes above make sure that we will reach the *exact* DragonFlyingTarget
      value. }
    if (T.X = DragonFlyingTarget.X) and
       (T.Y = DragonFlyingTarget.Y) then
    begin
      DragonFlying := false;
      SceneDragon.PlayAnimation('idle', true);
    end else
    { If we're still flying then
      update SceneDragon.Scale to reflect direction we're flying to.
      Flipping Scale.X is an easy way to flip 2D objects. }
    if DragonFlyingTarget.X > SceneDragon.Translation.X then
      SceneDragon.Scale := Vector3(-1, 1, 1)
    else
      SceneDragon.Scale := Vector3(1, 1, 1);
  end;

  if (SceneDragon.Translation.X < 1000) and not AchievementSeeLeftSubmitted then
  begin
    GameService.Achievement(AchievementSeeLeft);
    AchievementSeeLeftSubmitted := true;
  end;

  if (SceneDragon.Translation.X > 7000) and not AchievementSeeRightSubmitted then
  begin
    GameService.Achievement(AchievementSeeRight);
    AchievementSeeRightSubmitted := true;
  end;

  if CheckboxCameraFollow.Checked then
  begin
    CamPos := MainViewport.Camera.Translation;
    CamPos.X := SceneDragon.Translation.X;
    MainViewport.Camera.Translation := CamPos;
  end;
end;

function TViewPlay.Press(const Event: TInputPressRelease): Boolean;
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
    DragonFlyingTarget := MainViewport.PositionTo2DWorld(Event.Position, true);
    if not DragonFlying then
    begin
      SceneDragon.PlayAnimation('flying', true);
      DragonFlying := true;
    end;

    GameService.Achievement(AchievementMove);
    Exit(true); // click was handled
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

procedure TViewPlay.ChangeCheckboxCameraFollow(Sender: TObject);
begin
  GameService.Achievement(AchievementClickFollow);
end;

procedure TViewPlay.ClickShowAchievements(Sender: TObject);
begin
  GameService.ShowAchievements;
end;

end.
