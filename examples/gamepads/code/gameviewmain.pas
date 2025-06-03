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
  CastleNotifications, CastleLog, CastleJoysticks,
  GameUtils;

type
  { View to handle events. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    Notifications: TCastleNotifications;
    ButtonReinitialize, ButtonUnselect: TCastleButton;
    LabelJGamepadsCount: TCastleLabel;
    GroupGamepads, SelectedGamepadDynamicUi: TCastleVerticalGroup;
    SelectedGamepadUi: TCastleUserInterface;
  strict private
    SelectedJoystick: Integer;
    JoyButtons: array of TCastleButton;
    JoyAxes: array [TInternalGamepadAxis] of TCastleLabel;
    JoyLeftAxisVisualize, JoyRightAxisVisualize: TJoyAxisVisualize;

    procedure ClearJoystickUI;
    procedure ClearSelectedJoystickUI;

    // UI events handlers
    procedure InitializeJoystickUI(Sender: TObject);
    procedure JoystickDisconnected(Sender: TObject);
    procedure JoystickConnected(Sender: TObject);
    procedure ClickReinitialize(Sender: TObject);
    procedure ClickGamepadSelect(Sender: TObject);
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

  SelectedJoystick := -1; // default

  ButtonReinitialize.OnClick := {$ifdef FPC}@{$endif} ClickReinitialize;
  ButtonUnselect.OnClick := {$ifdef FPC}@{$endif} ClickUnselect;

  Joysticks.OnChange := {$ifdef FPC}@{$endif} InitializeJoystickUI;
  Joysticks.OnDisconnect := {$ifdef FPC}@{$endif} JoystickDisconnected;
  Joysticks.OnConnect := {$ifdef FPC}@{$endif} JoystickConnected;

  { Actually detect joysticks.
    This will immediately call InitializeJoystickUI on some platforms. }
  Joysticks.Initialize;
end;

procedure TViewMain.ClickReinitialize(Sender: TObject);
begin
  // If any joystick is plugged or unplugged then reinitialize is needed
  Notifications.Show('Detecting joysticks again...');
  Joysticks.Initialize;
end;

procedure TViewMain.ClearSelectedJoystickUI;
var
  Button: TInternalGamepadButton;
  Axis: TInternalGamepadAxis;
  C: TCastleUserInterface;
begin
  if Length(JoyButtons) <> 0 then
    for Button := Low(JoyButtons) to High(JoyButtons) do
      FreeAndNil(JoyButtons[Button]);
  SetLength(JoyButtons, 0);

  for Axis := Low(JoyAxes) to High(JoyAxes) do
    FreeAndNil(JoyAxes[Axis]);

  FreeAndNil(JoyLeftAxisVisualize);
  FreeAndNil(JoyRightAxisVisualize);
  SelectedGamepadDynamicUi.ClearControls;
  SelectedGamepadUi.Exists := false;

  SelectedJoystick := -1;

  // Unpress all buttons in GroupGamepads
  for C in GroupGamepads do
    (C as TCastleButton).Pressed := false;
end;

procedure TViewMain.ClearJoystickUI;
begin
  GroupGamepads.ClearControls;
  ClearSelectedJoystickUI;
end;

procedure TViewMain.ClickGamepadSelect(Sender: TObject);
var
  Button: TInternalGamepadButton;
  Axis: TInternalGamepadAxis;
  ButtonsGroup: TCastleHorizontalGroup;
begin
  ClearSelectedJoystickUI;

  SelectedGamepadUi.Exists := true;

  // Show selected joystick details
  SelectedJoystick := (Sender as TComponent).Tag;
  (GroupGamepads.Controls[SelectedJoystick] as TCastleButton).Pressed := true;

  ButtonsGroup := TCastleHorizontalGroup.Create(FreeAtStop);
  SelectedGamepadDynamicUi.InsertBack(ButtonsGroup);

  // Create array of buttons
  SetLength(JoyButtons, Joysticks[SelectedJoystick].InternalButtonsCount);
  for Button := 0 to High(JoyButtons) do
  begin
    JoyButtons[Button] := TCastleButton.Create(FreeAtStop);
    JoyButtons[Button].Toggle := true;
    JoyButtons[Button].Enabled := false;
    JoyButtons[Button].Caption := IntToStr(Button);
    ButtonsGroup.InsertFront(JoyButtons[Button]);
  end;
  Notifications.Show(Format('Found %d buttons', [
    Joysticks[SelectedJoystick].InternalButtonsCount
  ]));

  // Create axis labels
  for Axis := Low(TInternalGamepadAxis) to High(TInternalGamepadAxis) do
  begin
    JoyAxes[Axis] := TCastleLabel.Create(FreeAtStop);
    JoyAxes[Axis].Caption := Format('Axis %d (%s): no input so far', [Ord(Axis), InternalAxisName(Axis)]);
    JoyAxes[Axis].Color := White;
    SelectedGamepadDynamicUi.InsertControl(Ord(Axis), JoyAxes[Axis]);
  end;
  Notifications.Show(Format('Found %d axes', [
    Joysticks[SelectedJoystick].InternalAxesCount
  ]));

  JoyLeftAxisVisualize := TJoyAxisVisualize.Create(FreeAtStop);
  JoyLeftAxisVisualize.Anchor(hpRight, -256 - 10 - 10);
  JoyLeftAxisVisualize.Anchor(vpBottom, 100);
  JoyLeftAxisVisualize.Caption := 'Left Axis';
  InsertFront(JoyLeftAxisVisualize);

  JoyRightAxisVisualize := TJoyAxisVisualize.Create(FreeAtStop);
  JoyRightAxisVisualize.Anchor(hpRight, -10);
  JoyRightAxisVisualize.Anchor(vpBottom, 100);
  JoyRightAxisVisualize.Caption := 'Right Axis';
  InsertFront(JoyRightAxisVisualize);
end;

procedure TViewMain.ClickUnselect(Sender: TObject);
begin
  ClearSelectedJoystickUI;
end;

procedure TViewMain.InitializeJoystickUI(Sender: TObject);
var
  I: Integer;
  GamepadSelectButton: TCastleButton;
begin
  ClearJoystickUI;

  LabelJGamepadsCount.Caption := Format('Number of joysticks found: %d', [Joysticks.Count]);

  // List all joysticks
  for I := 0 to Joysticks.Count - 1 do
  begin
    GamepadSelectButton := TCastleButton.Create(FreeAtStop);
    GamepadSelectButton.Caption := Format('Gamepad %d: "%s" (%d axes, %d buttons)', [
      I,
      Joysticks[I].Name,
      { WARNING: Do not use Controller.InternalAxesCount, InternalButtonsCount
        in your own code. They are only used here to debug game controllers
        implementation. }
      Joysticks[I].InternalAxesCount,
      Joysticks[I].InternalButtonsCount
    ]);
    GamepadSelectButton.OnClick := {$ifdef FPC}@{$endif} ClickGamepadSelect;
    GamepadSelectButton.Tag := I;
    GamepadSelectButton.Toggle := true;
    GroupGamepads.InsertFront(GamepadSelectButton);
  end;
end;

procedure TViewMain.JoystickDisconnected(Sender: TObject);
begin
  Joysticks.Initialize;
  //will call OnChange and therefore InitializeJoystickUI
end;

procedure TViewMain.JoystickConnected(Sender: TObject);
begin
  Joysticks.Initialize;
  //will call OnChange and therefore InitializeJoystickUI
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: boolean);
var
  InternalAxis: TInternalGamepadAxis;
begin
  { update game controller axes visualization }
  if SelectedJoystick <> -1 then
  begin
    Assert(JoyLeftAxisVisualize <> nil);
    Assert(JoyRightAxisVisualize <> nil);
    JoyLeftAxisVisualize.Axis := Joysticks[SelectedJoystick].LeftAxis;
    JoyRightAxisVisualize.Axis := Joysticks[SelectedJoystick].RightAxis;

    { WARNING: Do not use TInternalGamepadAxis or Controller.InternalAxis
      in your own code.
      Use instead nice Controller.LeftAxis and Controller.RightAxis,
      as shown above.
      This is used here only to debug game controllers implementation. }
    for InternalAxis := Low(TInternalGamepadAxis) to High(TInternalGamepadAxis) do
      JoyAxes[InternalAxis].Caption := FormatDot('Axis %d (%s): %f', [
        Ord(InternalAxis),
        InternalAxisName(InternalAxis),
        Joysticks[SelectedJoystick].InternalAxis[InternalAxis]
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
    if Event.Controller.ControllerIndex = SelectedJoystick then
      JoyButtons[Event.Controller.InternalButton].Pressed := true;

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
    if Event.Controller.ControllerIndex = SelectedJoystick then
      JoyButtons[Event.Controller.InternalButton].Pressed := false;

    Exit(true); // handled
  end;
end;

end.
