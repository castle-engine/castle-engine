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
  CastleOnScreenMenu,
  CastleImages, CastleFilesUtils, CastleColors,
  CastleNotifications, CastleLog, CastleJoysticks,
  GameUtils;

type
  { View to handle events. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    // ButtonXxx: TCastleButton;
  strict private
    MenuGroup: TCastleVerticalGroup;
    Notifications: TCastleNotifications;
    ButtonReinitialize: TCastleButton;
    OnScreenMenu: TCastleOnScreenMenu;
    LabelJoysticksCount: TCastleLabel;
    LabelSelectedJoystick: TCastleLabel;
    SelectedJoystick: Integer;
    JoyButtons: array of TCastleButton;
    JoyAxes: array of TCastleLabel;
    JoyLeftAxisVisualize, JoyRightAxisVisualize: TJoyAxisVisualize;

    procedure ClearJoystickUI;
    procedure ClearSelectedJoystickUI;

    procedure InitializeJoystickUI(Sender: TObject);
    procedure JoystickDisconnected;
    procedure JoystickConnected;
    procedure ClickReinitialize(Sender: TObject);
    procedure ClickJoystickSelect(Sender: TObject);
    procedure MyJoyAxisMove(const Joy: TJoystick; const Axis: Byte; const Value: Single);
    procedure MyJoyButtonPress(const Joy: TJoystick; const Button: Byte);
    procedure JoyButtonUp(const Joy: TJoystick; const Button: Byte);
    procedure JoyButtonDown(const Joy: TJoystick; const Button: Byte);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
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

  Notifications := TCastleNotifications.Create(FreeAtStop);
  Notifications.Anchor(vpBottom, 10);
  Notifications.Anchor(hpMiddle);
  Notifications.TextAlignment := hpMiddle;
  Notifications.Timeout := 2.0;
  Notifications.Fade := 0.5;
  InsertBack(Notifications);

  ButtonReinitialize := TCastleButton.Create(FreeAtStop);
  ButtonReinitialize.Caption := 'Detect connected joysticks again (Joysticks.Initialize)';
  ButtonReinitialize.Anchor(hpLeft, 10);
  ButtonReinitialize.Anchor(vpBottom, 10);
  ButtonReinitialize.OnClick := {$ifdef FPC}@{$endif} ClickReinitialize;
  InsertFront(ButtonReinitialize);

  MenuGroup := TCastleVerticalGroup.Create(FreeAtStop);
  MenuGroup.Anchor(hpLeft, 10);
  MenuGroup.Anchor(vpTop, -10);
  MenuGroup.Spacing := 10;
  InsertFront(MenuGroup);

  LabelJoysticksCount := TCastleLabel.Create(FreeAtStop);
  LabelJoysticksCount.Color := White;
  MenuGroup.InsertFront(LabelJoysticksCount);

  OnScreenMenu := TCastleOnScreenMenu.Create(FreeAtStop);
  MenuGroup.InsertFront(OnScreenMenu);

  LabelSelectedJoystick := TCastleLabel.Create(FreeAtStop);
  LabelSelectedJoystick.Color := White;
  LabelSelectedJoystick.Caption := 'Selected: none';
  MenuGroup.InsertFront(LabelSelectedJoystick);

  Joysticks.OnChange := {$ifdef FPC}@{$endif} InitializeJoystickUI;
  Joysticks.OnDisconnect := {$ifdef FPC}@{$endif} JoystickDisconnected;
  Joysticks.OnConnect := {$ifdef FPC}@{$endif} JoystickConnected;
  Joysticks.OnAxisMove := {$ifdef FPC}@{$endif} MyJoyAxisMove;
  Joysticks.OnButtonDown := {$ifdef FPC}@{$endif} JoyButtonDown;
  Joysticks.OnButtonPress := {$ifdef FPC}@{$endif} MyJoyButtonPress;
  Joysticks.OnButtonUp := {$ifdef FPC}@{$endif} JoyButtonUp;

  { Actually detect joysticks.
    This will automatically call TEventsHandler.JoysticksChanged on some platforms. }
  Joysticks.Initialize;
end;

procedure TViewMain.MyJoyAxisMove(const Joy: TJoystick; const Axis: Byte;
  const Value: Single);
begin
  // We show axes position only for the selected joystick.
  if (SelectedJoystick = -1) or
     (Joy <> Joysticks[SelectedJoystick]) then
    Exit;

  // If axes labels not initialized yet then exit.
  if Length(JoyAxes) = 0 then Exit;

  // Check whether Axis is in range, because some joysticks report Axis
  // numbers outside of their declared Axis count,
  // see https://github.com/castle-engine/castle-engine/issues/106 .
  if Axis <= High(JoyAxes) then
    JoyAxes[Axis].Caption := AxisNames[Axis] + ': ' + FloatToStrDot(Value);
end;

procedure TViewMain.MyJoyButtonPress(const Joy: TJoystick; const Button: Byte);
begin
  Notifications.Show(Format('Button press event (b: %d)', [Button]));
end;

procedure TViewMain.JoyButtonUp(const Joy: TJoystick; const Button: Byte);
begin
  if (SelectedJoystick = -1) or
     (Joy <> Joysticks[SelectedJoystick]) then
    Exit;
  if Length(JoyButtons) = 0 then Exit;

  JoyButtons[Button].Pressed := False;

  Notifications.Show('Joy button up');
end;

procedure TViewMain.JoyButtonDown(const Joy: TJoystick; const Button: Byte);
begin
  if (SelectedJoystick = -1) or
     (Joy <> Joysticks[SelectedJoystick]) then
    Exit;
  if Length(JoyButtons) = 0 then Exit;

  JoyButtons[Button].Pressed := True;

  Notifications.Show('Joy button down');
end;

procedure TViewMain.ClickReinitialize(Sender: TObject);
begin
  // If any joystick is plugged or unplugged then reinitialize is needed
  Notifications.Show('Detecting joysticks again...');
  Joysticks.Initialize;
end;

procedure TViewMain.ClearSelectedJoystickUI;
var
  I: Integer;
begin
  for I := Low(JoyButtons) to High(JoyButtons) do
    JoyButtons[i].Free;
  SetLength(JoyButtons, 0);
  for I := Low(JoyAxes) to High(JoyAxes) do
    JoyAxes[i].Free;
  SetLength(JoyAxes, 0);
  FreeAndNil(JoyLeftAxisVisualize);
  FreeAndNil(JoyRightAxisVisualize);

  SelectedJoystick := -1;
  LabelSelectedJoystick.Caption := 'Selected: none';
end;

procedure TViewMain.ClearJoystickUI;
begin
  OnScreenMenu.MenuItems.ClearControls;
  ClearSelectedJoystickUI;
end;

procedure TViewMain.ClickJoystickSelect(Sender: TObject);
var
  i: Integer;
begin
  ClearSelectedJoystickUI;

  // Show selected joystick details
  SelectedJoystick := (Sender as TComponent).Tag;
  LabelSelectedJoystick.Caption := Format('Selected: [%d] %s',
    [SelectedJoystick, Joysticks.GetInfo(SelectedJoystick)^.Name]);

  // Create array of buttons
  SetLength(JoyButtons, Joysticks.GetInfo(SelectedJoystick)^.Count.Buttons);
  for I := 0 to High(JoyButtons) do
  begin
    JoyButtons[i] := TCastleButton.Create(FreeAtStop);
    JoyButtons[i].Toggle := True;
    JoyButtons[i].Enabled := False;
    JoyButtons[i].Anchor(hpLeft, 10 + 45 * i);
    JoyButtons[i].Anchor(vpBottom, 60);
    JoyButtons[i].AutoSize := False;
    JoyButtons[i].Width := 40;
    JoyButtons[i].Height := 40;
    JoyButtons[i].Caption := IntToStr(i);
    InsertFront(JoyButtons[i]);
  end;
  Notifications.Show(Format('Found %d buttons',
    [Joysticks.GetInfo(SelectedJoystick)^.Count.Buttons]));

  // Create axis labels
  //SetLength(JoyAxes, Joysticks.GetInfo(SelectedJoystick)^.Count.Axes);
  { Show all 8 possible axes, as D-Pad/POV axes are sometimes "further than Count.Axes"
    This is a temporary measure until we can separate D-Pad/POV correctly in the backend }
  SetLength(JoyAxes, 8);
  for I := 0 to High(JoyAxes) do
  begin
    JoyAxes[i] := TCastleLabel.Create(FreeAtStop);
    JoyAxes[i].Anchor(hpLeft, 10);
    JoyAxes[i].Anchor(vpBottom, 120 + i * 45);
    JoyAxes[i].Caption := 'Axis: ' + IntToStr(i);
    JoyAxes[i].Color := White;
    InsertFront(JoyAxes[i]);
  end;
  Notifications.Show(Format('Found %d axes',
    [Joysticks.GetInfo(SelectedJoystick)^.Count.Axes]));

  JoyLeftAxisVisualize := TJoyAxisVisualize.Create(FreeAtStop);
  JoyLeftAxisVisualize.Anchor(hpRight, -256 - 10 - 10);
  JoyLeftAxisVisualize.Anchor(vpTop, -10);
  JoyLeftAxisVisualize.Caption := 'Left Axis';
  InsertFront(JoyLeftAxisVisualize);

  JoyRightAxisVisualize := TJoyAxisVisualize.Create(FreeAtStop);
  JoyRightAxisVisualize.Anchor(hpRight, -10);
  JoyRightAxisVisualize.Anchor(vpTop, -10);
  JoyRightAxisVisualize.Caption := 'Right Axis';
  InsertFront(JoyRightAxisVisualize);
end;

procedure TViewMain.InitializeJoystickUI(Sender: TObject);
var
  I: Integer;
  JoystickSelect: TCastleOnScreenMenuItem;
begin
  ClearJoystickUI;

  LabelJoysticksCount.Caption := Format('Number of joysticks found: %d', [Joysticks.Count]);

  // List all joysticks
  for I := 0 to Joysticks.Count - 1 do
  begin
    JoystickSelect := TCastleOnScreenMenuItem.Create(FreeAtStop);
    JoystickSelect.Caption := Format('[%d] %s', [I, Joysticks.GetInfo(I)^.Name]);
    JoystickSelect.OnClick := {$ifdef FPC}@{$endif}ClickJoystickSelect;
    JoystickSelect.Tag := I;
    OnScreenMenu.Add(JoystickSelect);
  end;
end;

procedure TViewMain.JoystickDisconnected;
begin
  Joysticks.Initialize;
  //will call OnChange and therefore InitializeJoystickUI
end;

procedure TViewMain.JoystickConnected;
begin
  Joysticks.Initialize;
  //will call OnChange and therefore InitializeJoystickUI
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  { update JoyAxisVisualize.Axis }
  if SelectedJoystick <> -1 then
  begin
    Assert(JoyLeftAxisVisualize <> nil);
    Assert(JoyRightAxisVisualize <> nil);
    JoyLeftAxisVisualize.Axis := Joysticks[SelectedJoystick].LeftAxis;
    JoyRightAxisVisualize.Axis := Joysticks[SelectedJoystick].RightAxis;
  end;
end;

end.
