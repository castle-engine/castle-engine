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
    GroupControllers, SelectedControllerDynamicUi: TCastleVerticalGroup;
    SelectedControllerUi: TCastleUserInterface;
  strict private
    SelectedController: Integer;
    ControllerButtons: array of TCastleButton;
    ControllerAxes: array [TInternalGameControllerAxis] of TCastleLabel;
    AxisLeftVisualize, AxisRightVisualize: T2DAxisVisualize;

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
  Button: TInternalGameControllerButton;
  Axis: TInternalGameControllerAxis;
  C: TCastleUserInterface;
begin
  if Length(ControllerButtons) <> 0 then
    for Button := Low(ControllerButtons) to High(ControllerButtons) do
      FreeAndNil(ControllerButtons[Button]);
  SetLength(ControllerButtons, 0);

  for Axis := Low(ControllerAxes) to High(ControllerAxes) do
    FreeAndNil(ControllerAxes[Axis]);

  FreeAndNil(AxisLeftVisualize);
  FreeAndNil(AxisRightVisualize);
  SelectedControllerDynamicUi.ClearControls;
  SelectedControllerUi.Exists := false;

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
var
  Button: TInternalGameControllerButton;
  Axis: TInternalGameControllerAxis;
  ButtonsGroup: TCastleHorizontalGroup;
begin
  ClearSelectedControllerUI;

  SelectedControllerUi.Exists := true;

  // Show selected controller details
  SelectedController := (Sender as TComponent).Tag;
  (GroupControllers.Controls[SelectedController] as TCastleButton).Pressed := true;

  ButtonsGroup := TCastleHorizontalGroup.Create(FreeAtStop);
  SelectedControllerDynamicUi.InsertBack(ButtonsGroup);

  // Create array of buttons
  SetLength(ControllerButtons, Controllers[SelectedController].InternalButtonsCount);
  for Button := 0 to High(ControllerButtons) do
  begin
    ControllerButtons[Button] := TCastleButton.Create(FreeAtStop);
    ControllerButtons[Button].Toggle := true;
    ControllerButtons[Button].Enabled := false;
    ControllerButtons[Button].Caption := IntToStr(Button);
    ButtonsGroup.InsertFront(ControllerButtons[Button]);
  end;
  Notifications.Show(Format('Found %d buttons', [
    Controllers[SelectedController].InternalButtonsCount
  ]));

  // Create axis labels
  for Axis := Low(TInternalGameControllerAxis) to High(TInternalGameControllerAxis) do
  begin
    ControllerAxes[Axis] := TCastleLabel.Create(FreeAtStop);
    ControllerAxes[Axis].Caption := Format('Axis %d (%s): no input so far', [Ord(Axis), InternalAxisName(Axis)]);
    ControllerAxes[Axis].Color := White;
    SelectedControllerDynamicUi.InsertControl(Ord(Axis), ControllerAxes[Axis]);
  end;
  Notifications.Show(Format('Found %d axes', [
    Controllers[SelectedController].InternalAxesCount
  ]));

  AxisLeftVisualize := T2DAxisVisualize.Create(FreeAtStop);
  AxisLeftVisualize.Anchor(hpRight, -256 - 10 - 10);
  AxisLeftVisualize.Anchor(vpBottom, 100);
  AxisLeftVisualize.Caption := 'Left Axis';
  InsertFront(AxisLeftVisualize);

  AxisRightVisualize := T2DAxisVisualize.Create(FreeAtStop);
  AxisRightVisualize.Anchor(hpRight, -10);
  AxisRightVisualize.Anchor(vpBottom, 100);
  AxisRightVisualize.Caption := 'Right Axis';
  InsertFront(AxisRightVisualize);
end;

procedure TViewMain.ClickUnselect(Sender: TObject);
begin
  ClearSelectedControllerUI;
end;

procedure TViewMain.InitializeControllersUI(Sender: TObject);
var
  I: Integer;
  ControllerSelectButton: TCastleButton;
begin
  ClearAllControllersUI;

  LabelControllersCount.Caption := Format('Number of game controllers found: %d', [Controllers.Count]);

  // List all controllers
  for I := 0 to Controllers.Count - 1 do
  begin
    ControllerSelectButton := TCastleButton.Create(FreeAtStop);
    ControllerSelectButton.Caption := Format('Controller %d: "%s" (%d axes, %d buttons)', [
      I,
      Controllers[I].Name,
      { WARNING: Do not use Controller.InternalAxesCount, InternalButtonsCount
        in your own code. They are only used here to debug game controllers
        implementation. }
      Controllers[I].InternalAxesCount,
      Controllers[I].InternalButtonsCount
    ]);
    ControllerSelectButton.OnClick := {$ifdef FPC}@{$endif} ClickControllerSelect;
    ControllerSelectButton.Tag := I;
    ControllerSelectButton.Toggle := true;
    GroupControllers.InsertFront(ControllerSelectButton);
  end;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: boolean);
var
  InternalAxis: TInternalGameControllerAxis;
begin
  { update game controller axes visualization }
  if SelectedController <> -1 then
  begin
    Assert(AxisLeftVisualize <> nil);
    Assert(AxisRightVisualize <> nil);
    AxisLeftVisualize.Axis := Controllers[SelectedController].AxisLeft;
    AxisRightVisualize.Axis := Controllers[SelectedController].AxisRight;

    { WARNING: Do not use TInternalGameControllerAxis or Controller.InternalAxis
      in your own code.
      Use instead nice Controller.AxisLeft and Controller.AxisRight,
      as shown above.
      This is used here only to debug game controllers implementation. }
    for InternalAxis := Low(TInternalGameControllerAxis) to High(TInternalGameControllerAxis) do
      ControllerAxes[InternalAxis].Caption := FormatDot('Axis %d (%s): %f', [
        Ord(InternalAxis),
        InternalAxisName(InternalAxis),
        Controllers[SelectedController].InternalAxis[InternalAxis]
      ]);
  end;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.EventType = itGameController then
  begin
    Notifications.Show('Game controller press: ' + Event.ToString);

    { WARNING: Do not use Controller.InternalButton in your own code.
      Use instead Controller.Button.
      We only show here InternalButton to debug game controllers
      implementation. }
    if Event.Controller.ControllerIndex = SelectedController then
      ControllerButtons[Event.Controller.InternalButton].Pressed := true;

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

    { WARNING: Do not use Controller.InternalButton in your own code.
      Use instead Controller.Button.
      We only show here InternalButton to debug game controllers
      implementation. }
    if Event.Controller.ControllerIndex = SelectedController then
      ControllerButtons[Event.Controller.InternalButton].Pressed := false;

    Exit(true); // handled
  end;
end;

end.
