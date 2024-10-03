{
  Copyright 2023-2024 Michalis Kamburelis, Eugene Loza.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
{ Main view, where most of the game logic takes place. }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleColors,
  CastleSteam;

type
  TViewMain = class(TCastleView)
  published
    LabelFps: TCastleLabel;
    LabelSteamStatus: TCastleLabel;
    VerticalGroupAchievements: TCastleVerticalGroup;
    VerticalGroupLog: TCastleVerticalGroup;
  strict private
    procedure ClickAchievement(Sender: TObject);
    procedure ClickAchievementProgress(Sender: TObject);
    procedure FillInAchievements;
    procedure SteamUserStatsReceived(Sender: TObject);
    procedure Log(const Message: String);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  GameSteam;

// TODO: Separate clear achievement / set achievemnts to different buttons, to test separetely

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;
  Steam.OnUserStatsReceived := {$ifdef FPC}@{$endif} SteamUserStatsReceived;
end;

procedure TViewMain.Stop;
begin
  { Disconnect our callback from Steam.

    This is not really necessary in this simple example, that has only
    one view. Moreover, OnUserStatsReceived right now occurs only once,
    soon after the game starts.

    But it is a good practice to disconnect events,
    as it is necessary in larger applications, and it makes the code safe
    regardless of how many times / when is the event called.
    Since Steam instance "lives"
    independently of our views, we don't want it to call methods of our
    view when the view is stopped, as our methods may be not ready
    for this (e.g. FillInAchievements assumes that the view is not stopped).

    The check "if Steam <> nil then" is only in case GameSteam finalization
    will happen before this view stops (which is done from CastleWindow
    finalization). }
  if Steam <> nil then
    Steam.OnUserStatsReceived := nil;
  inherited;
end;

procedure TViewMain.SteamUserStatsReceived(Sender: TObject);
begin
  FillInAchievements;
end;

procedure TViewMain.FillInAchievements;
var
  S: String;
  Horiz: TCastleHorizontalGroup;
  Button: TCastleButton;
  Lab: TCastleLabel;
begin
  VerticalGroupAchievements.ClearControls;  // Warning: hidden memory leak here, as ClearControls doesn't free them
  for S in Steam.Achievements do
  begin
    Horiz := TCastleHorizontalGroup.Create(VerticalGroupAchievements);
    Horiz.Spacing := 20;
    VerticalGroupAchievements.InsertFront(Horiz);
    Button := TCastleButton.Create(Horiz);
    Button.Caption := S;
    Button.OnClick := {$ifdef FPC}@{$endif}ClickAchievement;
    Horiz.InsertFront(Button);
    Lab := TCastleLabel.Create(Horiz);
    if Steam.GetAchievement(S) then
      Lab.Caption := 'YES'
    else
      Lab.Caption := 'NO';
    Lab.Color := CastleColors.White;
    Horiz.InsertFront(Lab);
  end;
  Button := TCastleButton.Create(VerticalGroupAchievements);
  Button.Caption := 'Indicate Achievement Progress';
  Button.OnClick := {$ifdef FPC}@{$endif} ClickAchievementProgress;
  VerticalGroupAchievements.InsertFront(Button);
end;

procedure TViewMain.ClickAchievement(Sender: TObject);
begin
  if Steam.GetAchievement((Sender as TCastleButton).Caption) then
  begin
    Log('Achievement set, clearing');
    Steam.ClearAchievement((Sender as TCastleButton).Caption);
  end else
  begin
    Log('Achievement not set, adding');
    Steam.SetAchievement((Sender as TCastleButton).Caption);
  end;
  FillInAchievements;
end;

procedure TViewMain.ClickAchievementProgress(Sender: TObject);
begin
  Steam.ClearAchievement('ACH_WIN_100_GAMES');
  Steam.IndicateAchievementProgress('ACH_WIN_100_GAMES', Random(99) + 1, 100);
end;

procedure TViewMain.Log(const Message: String);
var
  NewLabel: TCastleLabel;
begin
  // TODO: use OnScreenNotifications
  NewLabel := TCastleLabel.Create(VerticalGroupLog);
  NewLabel.Caption := Message;
  NewLabel.Color := CastleColors.White;
  VerticalGroupLog.InsertFront(NewLabel);
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  // update LabelSteamStatus.Caption
  if Steam.Enabled then
  begin
    if Steam.UserStatsReceived then
      LabelSteamStatus.Caption := 'Steam enabled and user stats received OK.'
    else
      LabelSteamStatus.Caption := 'Steam enabled, waiting for user stats...'
  end else
    LabelSteamStatus.Caption := 'Steam disabled (dynamic library not found or other reason -- consult the logs and the docs https://castle-engine.io/steam )';
end;

end.
