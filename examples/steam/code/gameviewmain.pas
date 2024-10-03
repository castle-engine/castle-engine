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
    AchievementRowFactory: TCastleComponentFactory;
    procedure ClickSetAchievement(Sender: TObject);
    procedure ClickClearAchievement(Sender: TObject);
    procedure ClickIndicateProgress(Sender: TObject);
    procedure UpdateAchievementsUi;
    procedure SteamUserStatsReceived(Sender: TObject);
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
  CastleUtils,
  GameSteam;

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

  AchievementRowFactory := TCastleComponentFactory.Create(FreeAtStop);
  AchievementRowFactory.Url := 'castle-data:/achievement_row.castle-user-interface';
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
    for this (e.g. UpdateAchievementsUi assumes that the view is not stopped).

    The check "if Steam <> nil then" is only in case GameSteam finalization
    will happen before this view stops (which is done from CastleWindow
    finalization). }
  if Steam <> nil then
    Steam.OnUserStatsReceived := nil;
  inherited;
end;

procedure TViewMain.SteamUserStatsReceived(Sender: TObject);
begin
  UpdateAchievementsUi;
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
      LabelSteamStatus.Caption := 'Steam enabled, waiting for user stats...';
    // Add more information to LabelSteamStatus.Caption
    LabelSteamStatus.Caption := LabelSteamStatus.Caption + NL +
      NL +
      'Country: ' + Steam.Country + NL +
      'Language: ' + Steam.Language + NL +
      NL +
      'Overlay Enabled: ' + BoolToStr(Steam.OverlayEnabled, true) + NL +
      'Running in VR: ' + BoolToStr(Steam.RunningInVR, true) + NL +
      'Running on Steam Deck: ' + BoolToStr(Steam.RunningOnSteamDeck, true) + NL +
      NL +
      'App ID: ' + IntToStr(Steam.AppId) + NL +
      'Build ID: ' + IntToStr(Steam.BuildId);
  end else
    LabelSteamStatus.Caption := 'Steam disabled (dynamic library not found or other reason -- consult the logs and the docs https://castle-engine.io/steam )';
end;

type
  { Published fields of this match the component names in
    achievement_row.castle-user-interface.
    They will be set by TCastleComponentFactory.ComponentLoad
    automatically, see API docs of it. }
  TAchievementRowDesign = class(TPersistent)
  published
    LabelAchievementName: TCastleLabel;
    LabelAchievedState: TCastleLabel;
    ButtonSetAchievement: TCastleButton;
    ButtonClearAchievement: TCastleButton;
    ButtonIndicateProgress: TCastleButton;
  end;

procedure TViewMain.UpdateAchievementsUi;
var
  Row: TCastleUserInterface;
  RowDesign: TAchievementRowDesign;
  I: Integer;
  AchievementName: String;
begin
  { Free all previous achievement rows.
    Freeing the component (TCastleUserInterface instance) automatically
    removes it also from the parent, so we just free all
    VerticalGroupAchievements children, and in effect
    the VerticalGroupAchievements.Controls list will be empty. }
  while VerticalGroupAchievements.ControlsCount > 0 do
    VerticalGroupAchievements.Controls[0].Free;

  for I := 0 to Steam.Achievements.Count - 1 do
  begin
    RowDesign := TAchievementRowDesign.Create;
    try
      Row := AchievementRowFactory.ComponentLoad(FreeAtStop, RowDesign) as TCastleUserInterface;
      VerticalGroupAchievements.InsertFront(Row);

      AchievementName := Steam.Achievements[I];
      RowDesign.LabelAchievementName.Caption := Format('Achievement %d: %s', [
        I,
        AchievementName
      ]);
      RowDesign.LabelAchievedState.Exists := Steam.GetAchievement(AchievementName);

      RowDesign.ButtonSetAchievement.OnClick := {$ifdef FPC}@{$endif} ClickSetAchievement;
      RowDesign.ButtonSetAchievement.Tag := I;

      RowDesign.ButtonClearAchievement.OnClick := {$ifdef FPC}@{$endif} ClickClearAchievement;
      RowDesign.ButtonClearAchievement.Tag := I;

      RowDesign.ButtonIndicateProgress.OnClick := {$ifdef FPC}@{$endif} ClickIndicateProgress;
      RowDesign.ButtonIndicateProgress.Tag := I;
      // Steam allows to call IndicateAchievementProgress only for achievements that are not yet achieved
      RowDesign.ButtonIndicateProgress.Enabled := not Steam.GetAchievement(AchievementName);
    finally FreeAndNil(RowDesign) end;
  end;
end;

procedure TViewMain.ClickSetAchievement(Sender: TObject);
var
  AchievementName: String;
begin
  { Note that in a normal game, you would not set achievements like this.
    Instead you should call Steam.SetAchievement with a hardcoded name,
    when user achieves something.
    This example allows to "achieve" anything, just for testing purposes. }
  AchievementName := Steam.Achievements[(Sender as TCastleButton).Tag];
  Steam.SetAchievement(AchievementName);
  UpdateAchievementsUi;
end;

procedure TViewMain.ClickClearAchievement(Sender: TObject);
var
  AchievementName: String;
begin
  { Note that in a normal game, you would not clear achievements.
    Never take away achievements from the user.
    This functionality is only for testing purposes. }
  AchievementName := Steam.Achievements[(Sender as TCastleButton).Tag];
  Steam.ClearAchievement(AchievementName);
  UpdateAchievementsUi;
end;

procedure TViewMain.ClickIndicateProgress(Sender: TObject);
var
  AchievementName: String;
begin
  { Note that in a normal game, you would not call IndicateAchievementProgress
    like this.
    You should call IndicateAchievementProgress as a result of user actually
    doing something, progressing the achievement.
    And you should do this only for some achievements, that are prepared
    to be progressed and display nice message in overlay in response. }
  AchievementName := Steam.Achievements[(Sender as TCastleButton).Tag];
  Steam.IndicateAchievementProgress(AchievementName, RandomIntRange(1, 100), 100);
  UpdateAchievementsUi;
end;

end.
