{ Main view, where most of the application logic takes place.

  Feel free to use this code as a starting point for your own projects.
  This template code is in public domain, unlike most other CGE code which
  is covered by BSD or LGPL (see https://castle-engine.io/license). }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonHelloWorldNow, ButtonHello1Send, ButtonHello1Cancel,
      ButtonHello2Send, ButtonHello2Cancel,
      ButtonPlant, ButtonDigUp: TCastleButton;
    LabelGarden, LabelStatus: TCastleLabel;
    Stem: TCastleRectangleControl;
    Flower: TCastleShape;
  private
    { When was the seed planted, as Unix time (seconds), 0 if nothing is planted.
      Saved in UserConfig, so the garden survives closing the application. }
    PlantedTime: Int64;
    procedure ClickHelloWorldNow(Sender: TObject);
    procedure ClickHello1Send(Sender: TObject);
    procedure ClickHello1Cancel(Sender: TObject);
    procedure ClickHello2Send(Sender: TObject);
    procedure ClickHello2Cancel(Sender: TObject);
    procedure ClickPlant(Sender: TObject);
    procedure ClickDigUp(Sender: TObject);
    procedure Status(const S: String);
    procedure SavePlantedTime;
    { Show the plant as it should look now. }
    procedure UpdateGarden;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils, DateUtils,
  CastleLocalNotifications, CastleConfig, CastleLog, CastleUtils;

const
  { How long does the plant grow, in seconds. }
  GrowSeconds = 60;
  { Height of the stem of a fully grown plant. }
  StemHeight = 190;
  { Height of the pot, the stem starts on top of it. }
  PotHeight = 90;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;
  ButtonHelloWorldNow.OnClick := {$ifdef FPC}@{$endif} ClickHelloWorldNow;
  ButtonHello1Send.OnClick := {$ifdef FPC}@{$endif} ClickHello1Send;
  ButtonHello1Cancel.OnClick := {$ifdef FPC}@{$endif} ClickHello1Cancel;
  ButtonHello2Send.OnClick := {$ifdef FPC}@{$endif} ClickHello2Send;
  ButtonHello2Cancel.OnClick := {$ifdef FPC}@{$endif} ClickHello2Cancel;
  ButtonPlant.OnClick := {$ifdef FPC}@{$endif} ClickPlant;
  ButtonDigUp.OnClick := {$ifdef FPC}@{$endif} ClickDigUp;

  UserConfig.Load;
  PlantedTime := UserConfig.GetInt64('garden/planted_time', 0);
  UpdateGarden;

  {$if not defined(ANDROID)}
  Status('Notifications are shown only on Android now. On this platform, the buttons do nothing.');
  {$endif}
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  UpdateGarden;
end;

procedure TViewMain.Status(const S: String);
begin
  LabelStatus.Caption := S;
  WritelnLog('Notifications', S);
end;

procedure TViewMain.ClickHelloWorldNow(Sender: TObject);
begin
  TLocalNotifications.Schedule('hello_world', 0, 'Hello World', 'Sent right away.');
  Status('Sent "Hello World".');
end;

procedure TViewMain.ClickHello1Send(Sender: TObject);
begin
  TLocalNotifications.Schedule('hello_1', 10, 'Hello 1', 'Sent after 10 seconds.');
  Status('"Hello 1" will appear in 10 seconds. Close the application to see it arrive anyway.');
end;

procedure TViewMain.ClickHello1Cancel(Sender: TObject);
begin
  TLocalNotifications.Cancel('hello_1');
  Status('Canceled "Hello 1".');
end;

procedure TViewMain.ClickHello2Send(Sender: TObject);
begin
  TLocalNotifications.Schedule('hello_2', 60, 'Hello 2', 'Sent after 60 seconds.');
  Status('"Hello 2" will appear in 60 seconds. Close the application to see it arrive anyway.');
end;

procedure TViewMain.ClickHello2Cancel(Sender: TObject);
begin
  TLocalNotifications.Cancel('hello_2');
  Status('Canceled "Hello 2".');
end;

procedure TViewMain.SavePlantedTime;
begin
  UserConfig.SetDeleteInt64('garden/planted_time', PlantedTime, 0);
  UserConfig.Save;
end;

procedure TViewMain.ClickPlant(Sender: TObject);
begin
  PlantedTime := DateTimeToUnix(Now, false);
  SavePlantedTime;
  { The notification tells the player about something that happens
    while they are not looking. Scheduling it again (same id)
    replaces the previous one, so planting again just moves it. }
  TLocalNotifications.Schedule('garden_plant', GrowSeconds,
    'Your plant has grown', 'Come and see the flower!');
  Status(Format('Planted. The flower will bloom in %d seconds.', [GrowSeconds]));
end;

procedure TViewMain.ClickDigUp(Sender: TObject);
begin
  PlantedTime := 0;
  SavePlantedTime;
  TLocalNotifications.Cancel('garden_plant');
  Status('Dug up. The notification is canceled.');
end;

procedure TViewMain.UpdateGarden;
var
  Elapsed: Int64;
  Progress: Single;
begin
  if PlantedTime = 0 then
  begin
    Stem.Height := 0;
    Flower.Exists := false;
    LabelGarden.Caption := 'Plant a seed. It grows in one minute,' + NL +
      'even if you close the application.';
    ButtonPlant.Caption := 'Plant a Seed';
    ButtonDigUp.Enabled := false;
    Exit;
  end;

  Elapsed := DateTimeToUnix(Now, false) - PlantedTime;
  if Elapsed >= GrowSeconds then
    Progress := 1
  else
  if Elapsed <= 0 then
    Progress := 0
  else
    Progress := Elapsed / GrowSeconds;

  Stem.Height := StemHeight * Progress;
  Flower.Exists := Progress >= 1;
  Flower.Anchor(vpMiddle, vpBottom, PotHeight + StemHeight);
  ButtonPlant.Caption := 'Plant Again';
  ButtonDigUp.Enabled := true;

  if Progress >= 1 then
    LabelGarden.Caption := 'The flower has bloomed!' + NL +
      'Did the notification tell you?'
  else
    LabelGarden.Caption := Format('Growing... %d seconds left.' + NL +
      'Close the application, you will be notified.',
      [GrowSeconds - Elapsed]);
end;

end.
