{
  Copyright 2016-2025 Tomasz Wojtyś, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main view. }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleUiControls, CastleControls, CastleKeysMouse,
  CastleImages, CastleFilesUtils, CastleColors,
  CastleNotifications, CastleLog, CastleGameControllers,
  GameUtils;

type
  { View to handle events. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    Notifications: TCastleNotifications;
    ButtonReinitialize, ButtonUnselect: TCastleButton;
    LabelControllersCount: TCastleLabel;
    GroupControllers: TCastleVerticalGroup;
    SelectedControllerUi: TCastleUserInterface;
  strict private
    SelectedController: Integer;
    AxisLeftVisualize, AxisRightVisualize: T2DAxisVisualize;
    AxisLeftTriggerVisualize, AxisRightTriggerVisualize: T1DAxisVisualize;

    procedure ClearAllControllersUI;
    procedure ClearSelectedControllerUI;

    // UI events handlers
    procedure InitializeControllersUI(Sender: TObject);
    procedure ClickReinitialize(Sender: TObject);
    procedure ClickControllerSelect(Sender: TObject);
    procedure ClickUnselect(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    function Release(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleUtils;

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;

  SelectedController := -1; // default

  ButtonReinitialize.OnClick := {$ifdef FPC}@{$endif} ClickReinitialize;
  ButtonUnselect.OnClick := {$ifdef FPC}@{$endif} ClickUnselect;
  Controllers.OnChange := {$ifdef FPC}@{$endif} InitializeControllersUI;

  { Actually detect controllers.
    This will immediately call InitializeControllersUI on some platforms. }
  Controllers.Initialize;
end;

procedure TViewMain.ClickReinitialize(Sender: TObject);
begin
  // If any controller is plugged or unplugged then reinitialize is needed
  Notifications.Show('Detecting game controllers again...');
  Controllers.Initialize;
end;

procedure TViewMain.ClearSelectedControllerUI;
var
  C: TCastleUserInterface;
begin
  SelectedControllerUi.Exists := false;
  FreeAndNil(AxisLeftVisualize);
  FreeAndNil(AxisRightVisualize);
  FreeAndNil(AxisLeftTriggerVisualize);
  FreeAndNil(AxisRightTriggerVisualize);

  SelectedController := -1;

  // Unpress all buttons in GroupControllers
  for C in GroupControllers do
    (C as TCastleButton).Pressed := false;
end;

procedure TViewMain.ClearAllControllersUI;
begin
  GroupControllers.ClearControls;
  ClearSelectedControllerUI;
end;

procedure TViewMain.ClickControllerSelect(Sender: TObject);
begin
  ClearSelectedControllerUI;
  SelectedControllerUi.Exists := true;

  // Show selected controller details
  SelectedController := (Sender as TComponent).Tag;
  (GroupControllers.Controls[SelectedController] as TCastleButton).Pressed := true;

  AxisLeftVisualize := T2DAxisVisualize.Create(FreeAtStop);
  AxisLeftVisualize.Anchor(hpLeft, 10);
  AxisLeftVisualize.Anchor(vpBottom, 100);
  AxisLeftVisualize.Caption := 'Left Axis';
  SelectedControllerUi.InsertFront(AxisLeftVisualize);

  AxisRightVisualize := T2DAxisVisualize.Create(FreeAtStop);
  AxisRightVisualize.Anchor(hpLeft, 256 + 10 + 10);
  AxisRightVisualize.Anchor(vpBottom, 100);
  AxisRightVisualize.Caption := 'Right Axis';
  SelectedControllerUi.InsertFront(AxisRightVisualize);

  AxisLeftTriggerVisualize := T1DAxisVisualize.Create(FreeAtStop);
  AxisLeftTriggerVisualize.Anchor(hpLeft, 10);
  AxisLeftTriggerVisualize.Anchor(vpBottom,
    AxisLeftVisualize.Translation.Y +
    AxisLeftVisualize.EffectiveHeight + 10);
  AxisLeftTriggerVisualize.Caption := 'Left Trigger Axis';
  SelectedControllerUi.InsertFront(AxisLeftTriggerVisualize);

  AxisRightTriggerVisualize := T1DAxisVisualize.Create(FreeAtStop);
  AxisRightTriggerVisualize.Anchor(hpLeft, 256 + 10 + 10);
  AxisRightTriggerVisualize.Anchor(vpBottom,
    AxisRightVisualize.Translation.Y +
    AxisRightVisualize.EffectiveHeight + 10);
  AxisRightTriggerVisualize.Caption := 'Right Trigger Axis';
  SelectedControllerUi.InsertFront(AxisRightTriggerVisualize);
end;

procedure TViewMain.ClickUnselect(Sender: TObject);
begin
  ClearSelectedControllerUI;
end;

procedure TViewMain.InitializeControllersUI(Sender: TObject);
var
  Controller: TGameController;
  ControllerSelectButton: TCastleButton;
begin
  ClearAllControllersUI;

  LabelControllersCount.Caption := Format('Number of game controllers found: %d', [Controllers.Count]);

  // List all controllers
  for Controller in Controllers do
  begin
    ControllerSelectButton := TCastleButton.Create(FreeAtStop);
    ControllerSelectButton.Caption := Format('Controller %d: "%s"', [
      Controller.Index,
      Controller.Name
    ]);
    ControllerSelectButton.OnClick := {$ifdef FPC}@{$endif} ClickControllerSelect;
    ControllerSelectButton.Tag := Controller.Index;
    ControllerSelectButton.Toggle := true;
    GroupControllers.InsertFront(ControllerSelectButton);
  end;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  { update game controller axes visualization }
  if SelectedController <> -1 then
  begin
    Assert(AxisLeftVisualize <> nil);
    Assert(AxisRightVisualize <> nil);
    AxisLeftVisualize.Axis := Controllers[SelectedController].AxisLeft;
    AxisRightVisualize.Axis := Controllers[SelectedController].AxisRight;
    AxisLeftTriggerVisualize.Axis := Controllers[SelectedController].AxisLeftTrigger;
    AxisRightTriggerVisualize.Axis := Controllers[SelectedController].AxisRightTrigger;
  end;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.EventType = itGameController then
  begin
    Notifications.Show('Game controller press: ' + Event.ToString);
    Exit(true); // handled
  end;
end;

function TViewMain.Release(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.EventType = itGameController then
  begin
    Notifications.Show('Game controller release: ' + Event.ToString);
    Exit(true); // handled
  end;
end;

end.
