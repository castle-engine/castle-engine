unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleColors,
  CastleSteam;

type
  TViewMain = class(TCastleView)
  private
    procedure ClickAddAchievement1(Sender: TObject);
  published
    LabelFps: TCastleLabel;
    ButtonAddAchievement1: TCastleButton;
    VerticalGroupLog: TCastleVerticalGroup;
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

procedure TViewMain.ClickAddAchievement1(Sender: TObject);
begin
  if Steam.GetAchievement('ACH_WIN_ONE_GAME') then
  begin
    Log('Achievement set, clearing');
    Steam.ClearAchievement('ACH_WIN_ONE_GAME');
  end else
  begin
    Log('Achievement not set, adding');
    Steam.SetAchievement('ACH_WIN_ONE_GAME');
  end;
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
  ButtonAddAchievement1.OnClick := @ClickAddAchievement1;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  { Some Steam features (like callbacks) require calling Update often, usually every frame }
  Steam.Update;
end;

end.
