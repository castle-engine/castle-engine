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
    VerticalGroupAchievements: TCastleVerticalGroup;
    VerticalGroupLog: TCastleVerticalGroup;
  strict private
    SteamAchievementsReceived: Boolean;
    procedure ClickAchievement(Sender: TObject);
    procedure ClickAchievementProgress(Sender: TObject);
    procedure FillInAchievements;
  public
    procedure Log(const Message: String);
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils;

{ TViewMain ----------------------------------------------------------------- }

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
    Button.OnClick := @ClickAchievement;
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
  Button.OnClick := @ClickAchievementProgress;
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
  Steam.IndicateAchievementProgress('ACH_WIN_100_GAMES', Random(99) + 1, 100);
end;

procedure TViewMain.Log(const Message: String);
var
  NewLabel: TCastleLabel;
begin
  NewLabel := TCastleLabel.Create(VerticalGroupLog);
  NewLabel.Caption := Message;
  NewLabel.Color := CastleColors.White;
  VerticalGroupLog.InsertFront(NewLabel);
end;

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;
  SteamAchievementsReceived := false;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  { Some Steam features (like callbacks) require calling Update often, usually every frame }
  Steam.Update;

  if not SteamAchievementsReceived and Steam.Initialized then
  begin
    SteamAchievementsReceived := true;
    FillInAchievements;
  end;
end;

end.
