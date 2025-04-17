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
  CastleKeysMouse, CastleViewport, CastleScene, CastleVectors,
  CastleNotifications, CastleTimeUtils, CastleGameService;

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
    ButtonShowLeaderboardRandomScores: TCastleButton;
    ButtonShowLeaderboardTimes: TCastleButton;
    ButtonGetPlayerBestRandomScore: TCastleButton;
    ButtonSendLeaderboardRandomScores: TCastleButton;
    ButtonSendLeaderboardTimes: TCastleButton;
    ButtonGetPlayerBestTime: TCastleButton;
    ButtonSaveGame: TCastleButton;
    ButtonLoadGame: TCastleButton;
    ButtonChooseSaveGame: TCastleButton;
    GameNotifications: TCastleNotifications;
    LabelGameServiceStatus: TCastleLabel;
  private
    { DragonFlying and DragonFlyingTarget manage currect dragon (SceneDragon)
      animation and it's movement. }
    DragonFlying: Boolean;
    DragonFlyingTarget: TVector2;
    PlayTime: TFloatTime;

    // UI callbacks
    procedure ChangeCheckboxCameraFollow(Sender: TObject);
    procedure ClickShowAchievements(Sender: TObject);
    procedure ClickShowLeaderboardRandomScores(Sender: TObject);
    procedure ClickShowLeaderboardTimes(Sender: TObject);
    procedure ClickSendLeaderboardRandomScores(Sender: TObject);
    procedure ClickSendLeaderboardTimes(Sender: TObject);
    procedure ClickGetPlayerBestRandomScore(Sender: TObject);
    procedure ClickGetPlayerBestTime(Sender: TObject);
    procedure ClickSaveGame(Sender: TObject);
    procedure ClickLoadGame(Sender: TObject);
    procedure ClickChooseSaveGame(Sender: TObject);

    // Game service callbacks
    procedure GameServiceStatusChanged(Sender: TObject);
    procedure PlayerBestScoreReceived(Sender: TObject; const LeaderboardId: string; const Score: Int64);
    procedure GameServiceSaveGameLoaded(Sender: TObject; const Success: boolean; const Content: string);
    procedure GameServiceSaveGameChosen(Sender: TObject; const Choice: TSaveGameChoice; const SaveGameName: string);
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
  CastleConfig, CastleUtils,
  GameViewMenu, GameIds;

{ TViewPlay ----------------------------------------------------------------- }

constructor TViewPlay.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewplay.castle-user-interface';
end;

procedure TViewPlay.Start;
begin
  inherited;

  LabelGameServiceStatus.Caption := 'Status: ' + GameServiceStatusToStr(GameService.Status);

  // UI callbacks
  CheckboxCameraFollow.OnChange := {$ifdef FPC}@{$endif} ChangeCheckboxCameraFollow;
  ButtonShowAchievements.OnClick := {$ifdef FPC}@{$endif} ClickShowAchievements;
  ButtonShowLeaderboardRandomScores.OnClick := {$ifdef FPC}@{$endif} ClickShowLeaderboardRandomScores;
  ButtonShowLeaderboardTimes.OnClick := {$ifdef FPC}@{$endif} ClickShowLeaderboardTimes;
  ButtonSendLeaderboardRandomScores.OnClick := {$ifdef FPC}@{$endif} ClickSendLeaderboardRandomScores;
  ButtonSendLeaderboardTimes.OnClick := {$ifdef FPC}@{$endif} ClickSendLeaderboardTimes;
  ButtonGetPlayerBestRandomScore.OnClick := {$ifdef FPC}@{$endif} ClickGetPlayerBestRandomScore;
  ButtonGetPlayerBestTime.OnClick := {$ifdef FPC}@{$endif} ClickGetPlayerBestTime;
  ButtonSaveGame.OnClick := {$ifdef FPC}@{$endif} ClickSaveGame;
  ButtonLoadGame.OnClick := {$ifdef FPC}@{$endif} ClickLoadGame;
  ButtonChooseSaveGame.OnClick := {$ifdef FPC}@{$endif} ClickChooseSaveGame;

  // game service callbacks
  GameService.OnStatusChanged := {$ifdef FPC}@{$endif} GameServiceStatusChanged;
  GameService.OnPlayerBestScoreReceived := {$ifdef FPC}@{$endif} PlayerBestScoreReceived;
  GameService.OnSaveGameLoaded := {$ifdef FPC}@{$endif} GameServiceSaveGameLoaded;
  GameService.OnSaveGameChosen := {$ifdef FPC}@{$endif} GameServiceSaveGameChosen;
end;

procedure TViewPlay.Stop;
begin
  { GameService instance will exist throughout this application lifetime,
    even after this view is stopped.
    So unregisted our callbacks from it, to not let e.g. PlayerBestScoreReceived
    be called when view is stopped and UI is destroyed. }
  GameService.OnStatusChanged := nil;
  GameService.OnPlayerBestScoreReceived := nil;
  GameService.OnSaveGameLoaded := nil;
  GameService.OnSaveGameChosen := nil;
  inherited;
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
  PlayTime := PlayTime + SecondsPassed;

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

  if (SceneDragon.Translation.X < -10 * 1000) and not AchievementSeeLeftSubmitted then
  begin
    GameService.Achievement(AchievementSeeLeft);
    GameNotifications.Show('Achievement "see left" completed');
    AchievementSeeLeftSubmitted := true;
  end;

  if (SceneDragon.Translation.X > 10 * 1000) and not AchievementSeeRightSubmitted then
  begin
    GameService.Achievement(AchievementSeeRight);
    GameNotifications.Show('Achievement "see right" completed');
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
    // too spammy
    // GameNotifications.Show('Achievement "move" completed');
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
  GameNotifications.Show('Achievement "click follow" completed');
end;

procedure TViewPlay.ClickShowAchievements(Sender: TObject);
begin
  GameService.ShowAchievements;
end;

procedure TViewPlay.ClickShowLeaderboardRandomScores(Sender: TObject);
begin
  GameService.ShowLeaderboard(LeaderboardRandomScores);
end;

procedure TViewPlay.ClickShowLeaderboardTimes(Sender: TObject);
begin
  GameService.ShowLeaderboard(LeaderboardTimes);
end;

procedure TViewPlay.ClickSendLeaderboardRandomScores(Sender: TObject);
var
  Send: Int64;
begin
  Send := Random(1000);
  GameService.SubmitScore(LeaderboardRandomScores, Send);
  GameNotifications.Show('Send score ' + IntToStr(Send) + ' to ' + LeaderboardRandomScores);
end;

procedure TViewPlay.ClickSendLeaderboardTimes(Sender: TObject);
var
  Send: Int64;
begin
  Send := Trunc(PlayTime);
  GameService.SubmitScore(LeaderboardTimes, Send);
  GameNotifications.Show('Send score ' + IntToStr(Send) + ' to ' + LeaderboardTimes);
end;

procedure TViewPlay.ClickGetPlayerBestRandomScore(Sender: TObject);
begin
  GameService.RequestPlayerBestScore(LeaderboardRandomScores);
end;

procedure TViewPlay.ClickGetPlayerBestTime(Sender: TObject);
begin
  GameService.RequestPlayerBestScore(LeaderboardTimes);
end;

procedure TViewPlay.PlayerBestScoreReceived(Sender: TObject; const LeaderboardId: string; const Score: Int64);
begin
  GameNotifications.Show('Your best score on ' + LeaderboardId + ' is ' + IntToStr(Score));
end;

procedure TViewPlay.GameServiceStatusChanged(Sender: TObject);
begin
  LabelGameServiceStatus.Caption := 'Status: ' + GameServiceStatusToStr(GameService.Status);
end;

procedure TViewPlay.ClickSaveGame(Sender: TObject);
var
  StringStream: TStringStream;
  UserConfigAsString: String;
begin
  { We could just save a String straight using GameService.SaveGameSave.
    But to make it a more realistic demo, we do 3 steps:
    - Save state to UserConfig
    - Then convert UserConfig into a String
    - And save that.
  }
  UserConfig.SetVector3('dragon/translation', SceneDragon.Translation);
  UserConfig.SetVector3('dragon/scale', SceneDragon.Scale);
  UserConfig.SetValue('camera_follows', CheckboxCameraFollow.Checked);

  StringStream := TStringStream.Create('');
  try
    UserConfig.SaveToStream(StringStream);
    UserConfigAsString := StringStream.DataString;
  finally FreeAndNil(StringStream) end;

  GameService.SaveGameSave('default-save', UserConfigAsString, 'Default savegame.', Trunc(PlayTime));
  GameNotifications.Show('Starting saving game online.');
end;

procedure TViewPlay.ClickLoadGame(Sender: TObject);
begin
  GameService.SaveGameLoad('default-save');
  GameNotifications.Show('Starting loading game online.');
end;

procedure TViewPlay.GameServiceSaveGameLoaded(Sender: TObject;
  const Success: boolean; const Content: string);
var
  StringStream: TStringStream;
begin
  { Corresponding to TViewPlay.ClickSaveGame, we unpack received Content
    (String) into UserConfig, and then use UserConfig to restore the state. }
  if Success then
  begin
    StringStream := TStringStream.Create(Content);
    try
      UserConfig.LoadFromStream(StringStream,
        { PretendURL; we choose to just not modify it } UserConfig.URL);
    finally FreeAndNil(StringStream) end;

    SceneDragon.Translation := UserConfig.GetVector3('dragon/translation');
    SceneDragon.Scale := UserConfig.GetVector3('dragon/scale');
    CheckboxCameraFollow.Checked := UserConfig.GetValue('camera_follows', true);
    // loading game stops dragon flying
    DragonFlying := false;
    SceneDragon.PlayAnimation('idle', true);

    GameNotifications.Show('Successfully loaded online game.');
  end else
    GameNotifications.Show('Failed to load game online: ' + Content);
end;

procedure TViewPlay.ClickChooseSaveGame(Sender: TObject);
begin
  GameService.ShowSaveGames('Choose save game', true, true, -1);
  GameNotifications.Show('Starting choosing savegame online.');
end;

procedure TViewPlay.GameServiceSaveGameChosen(Sender: TObject; const Choice: TSaveGameChoice; const SaveGameName: string);
begin
  { We just display the user choice, without acting on it now.
    In a real game, you would probably want to do something
    depending on the user choice -- follow the ClickSaveGame or ClickLoadGame
    code to see how to save/load the game. }
  case Choice of
    sgCancel: GameNotifications.Show('Savegame choice cancelled.');
    sgNew: GameNotifications.Show('Savegame choice: new savegame.');
    sgExisting: GameNotifications.Show('Savegame choice: existing savegame.' + SaveGameName);
    else raise EInternalError.Create('GameServiceSaveGameChosen:Choice?');
  end;
end;

end.
