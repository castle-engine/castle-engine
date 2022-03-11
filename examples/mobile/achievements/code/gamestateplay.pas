{
  Copyright 2014-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main "playing game" state, where most of the game logic takes place. }
unit GameStatePlay;

interface

uses Classes,
  CastleUIState, CastleComponentSerialize, CastleUIControls, CastleControls,
  CastleKeysMouse, CastleViewport, CastleScene, CastleVectors;

type
  { Main "playing game" state, where most of the game logic takes place. }
  TStatePlay = class(TUIState)
  private
    { DragonFlying and DragonFlyingTarget manage currect dragon (SceneDragon)
      animation and it's movement. }
    DragonFlying: Boolean;
    DragonFlyingTarget: TVector2;

    { Components designed using CGE editor, loaded from state_play.castle-user-interface. }
    LabelFps: TCastleLabel;
    MainViewport: TCastleViewport;
    SceneDragon: TCastleScene;
    CheckboxCameraFollow: TCastleCheckbox;
    ButtonShowAchievements: TCastleButton;

    procedure ChangeCheckboxCameraFollow(Sender: TObject);
    procedure ClickShowAchievements(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  StatePlay: TStatePlay;

implementation

uses SysUtils, Math,
  GameStateMenu, GameAchievements;

{ TStatePlay ----------------------------------------------------------------- }

constructor TStatePlay.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestateplay.castle-user-interface';
end;

procedure TStatePlay.Start;
begin
  inherited;

  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
  MainViewport := DesignedComponent('MainViewport') as TCastleViewport;
  SceneDragon := DesignedComponent('SceneDragon') as TCastleScene;
  CheckboxCameraFollow := DesignedComponent('CheckboxCameraFollow') as TCastleCheckbox;
  ButtonShowAchievements := DesignedComponent('ButtonShowAchievements') as TCastleButton;

  CheckboxCameraFollow.OnChange := {$ifdef FPC}@{$endif} ChangeCheckboxCameraFollow;
  ButtonShowAchievements.OnClick := {$ifdef FPC}@{$endif} ClickShowAchievements;
end;

procedure TStatePlay.Update(const SecondsPassed: Single; var HandleInput: Boolean);
const
  DragonSpeed: TVector2 = (X: 3000; Y: 1500);
var
  T: TVector2;
  CamPos: TVector3;
begin
  inherited;
  { This virtual method is executed every frame.}

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
    CamPos := MainViewport.Camera.Position;
    CamPos.X := SceneDragon.Translation.X;
    MainViewport.Camera.Position := CamPos;
  end;
end;

function TStatePlay.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  { This virtual method is executed when user presses
    a key, a mouse button, or touches a touch-screen.

    Note that each UI control has also events like OnPress and OnClick.
    These events can be used to handle the "press", if it should do something
    specific when used in that UI control.
    The TStatePlay.Press method should be used to handle keys
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
    TUIState.Current := StateMenu;
    Exit(true);
  end;
end;

procedure TStatePlay.ChangeCheckboxCameraFollow(Sender: TObject);
begin
  GameService.Achievement(AchievementClickFollow);
end;

procedure TStatePlay.ClickShowAchievements(Sender: TObject);
begin
  GameService.ShowAchievements;
end;

end.
