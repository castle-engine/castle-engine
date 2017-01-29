{
  Copyright 2016-2017 Tomasz Wojtyś.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Demo lists all avalaible joysticks/gamepads in the system. Shows also how
  joystick events works. }

program joystick_demo;

{$apptype GUI}

uses SysUtils,
  CastleVectors, CastleWindow, CastleControls, CastleOnScreenMenu,
  CastleControlsImages, CastleImages, CastleFilesUtils, CastleColors,
  CastleUIControls, CastleNotifications, CastleLog, CastleJoysticks,
  Classes;

const
  AXIS_NAMES: array[JOY_AXIS_X..JOY_POVY] of string = ('JOY_AXIS_X', 'JOY_AXIS_Y',
              'JOY_AXIS_Z', 'JOY_AXIS_R', 'JOY_AXIS_U', 'JOY_AXIS_V', 'JOY_POVX', 'JOY_POVY');

var
  Window: TCastleWindowCustom;
  Notifications: TCastleNotifications;
  Background: TCastleSimpleBackground;
  btnReinitialize: TCastleButton;
  OnScreenMenu: TCastleOnScreenMenu;
  Title: TCastleLabel;
  SelectedJoystick: Integer;
  JoyButtons: array of TCastleButton;
  JoyAxes: array of TCastleLabel;

type
  TClicksHandler = class
    procedure btnReinitializeClick(Sender: TObject);
    procedure JoyButtonClick(Sender: TObject);
  end;

  TEventsHandler = class
    procedure JoyAxisMove(const Joy: PJoy; const Axis: Byte; const Value: Single);
    procedure JoyButtonPress(const Joy: PJoy; const Button: Byte);
    procedure JoyButtonUp(const Joy: PJoy; const Button: Byte);
    procedure JoyButtonDown(const Joy: PJoy; const Button: Byte);
  end;

function CreateJoyButton(ACaption: string): TCastleButton;
begin
  // Create a button for every joystick that has been found
  Result := TCastleButton.Create(Window);
  // In the caption will be the number of the joystick
  Result.Caption := ACaption;
  Result.OnClick := @TClicksHandler(nil).JoyButtonClick;
end;

procedure InitializeJoysticks;
var
  i: Integer;
begin
  // Initialize joysticks singelton instance
  EnableJoysticks;

  Title.Caption := Format('Number of joysticks found: %d', [Joysticks.JoyCount]);

  // List all joysticks
  for i := 0 to Joysticks.JoyCount - 1 do
  begin
    OnScreenMenu.Add(Joysticks.GetInfo(i)^.Name,
      CreateJoyButton(IntToStr(i)));
  end;

  // Bind joystick events
  Joysticks.OnAxisMove := @TEventsHandler(nil).JoyAxisMove;
  Joysticks.OnButtonDown := @TEventsHandler(nil).JoyButtonDown;
  Joysticks.OnButtonPress := @TEventsHandler(nil).JoyButtonPress;
  Joysticks.OnButtonUp := @TEventsHandler(nil).JoyButtonUp;
end;

procedure TEventsHandler.JoyAxisMove(const Joy: PJoy; const Axis: Byte;
  const Value: Single);
begin
  // We showing axes position only for selected joystick.
  if not (Joy = Joysticks.GetJoy(SelectedJoystick)) then Exit;

  // If axes labels not initialized yet then exit.
  if Length(JoyAxes) = 0 then Exit;

  JoyAxes[Axis].Caption := AXIS_NAMES[Axis] + ': ' + FloatToStr(Value);
end;

procedure TEventsHandler.JoyButtonPress(const Joy: PJoy; const Button: Byte);
begin
  Notifications.Show(Format('Button press event (b: %d)',
    [Button]));
end;

procedure TEventsHandler.JoyButtonUp(const Joy: PJoy; const Button: Byte);
begin
  Notifications.Show('Joy button up');
  if not (Joy = Joysticks.GetJoy(SelectedJoystick)) then Exit;
  if Length(JoyButtons) = 0 then Exit;

  JoyButtons[Button].Pressed := False;
end;

procedure TEventsHandler.JoyButtonDown(const Joy: PJoy; const Button: Byte);
begin
  Notifications.Show('Joy button down');
  if not (Joy = Joysticks.GetJoy(SelectedJoystick)) then Exit;
  if Length(JoyButtons) = 0 then Exit;

  JoyButtons[Button].Pressed := True;
end;

procedure TClicksHandler.btnReinitializeClick(Sender: TObject);
begin
  // If any joystick is plugged or unplugged then reinitialize is needed
  Notifications.Show('Reinitialization...');
  OnScreenMenu.ClearControls;
  Joysticks.Free;
  InitializeJoysticks;
end;

procedure TClicksHandler.JoyButtonClick(Sender: TObject);
var
  i: Integer;
begin
  // Show selected joystick details
  SelectedJoystick := StrToInt((Sender as TCastleButton).Caption);

  // Free previously selected data
  for i := Low(JoyButtons) to High(JoyButtons) do
    JoyButtons[i].Free;
  SetLength(JoyButtons, 0);
  for i := Low(JoyAxes) to High(JoyAxes) do
    JoyAxes[i].Free;
  SetLength(JoyAxes, 0);

  // Create array of buttons
  SetLength(JoyButtons, Joysticks.GetInfo(SelectedJoystick)^.Count.Buttons);
  for i := 0 to High(JoyButtons) do
  begin
    JoyButtons[i] := TCastleButton.Create(Window);
    JoyButtons[i].Toggle := True;
    JoyButtons[i].Enabled := False;
    JoyButtons[i].Left := 10 + 45 * i;
    JoyButtons[i].Bottom := 60;
    JoyButtons[i].AutoSize := False;
    JoyButtons[i].Width := 40;
    JoyButtons[i].Height := 40;
    JoyButtons[i].Caption := IntToStr(i);
    Window.Controls.InsertFront(JoyButtons[i]);
  end;
  Notifications.Show(Format('Found %d buttons',
    [Joysticks.GetInfo(SelectedJoystick)^.Count.Buttons]));

  // Create axis labels
  SetLength(JoyAxes, Joysticks.GetInfo(SelectedJoystick)^.Count.Axes);
  for i := 0 to High(JoyAxes) do
  begin
    JoyAxes[i] := TCastleLabel.Create(Window);
    JoyAxes[i].Left := 10;
    JoyAxes[i].Bottom := 120 + i * 45;
    JoyAxes[i].Caption := 'Axis: ' + IntToStr(i);
    Window.Controls.InsertFront(JoyAxes[i]);
  end;
  Notifications.Show(Format('Found %d axes',
    [Joysticks.GetInfo(SelectedJoystick)^.Count.Axes]));
end;

procedure WindowUpdate(Container: TUIContainer);
begin
  // Check joysticks states every frame
  Joysticks.Poll;
end;

begin
  Window := TCastleWindowCustom.Create(Application);

  InitializeLog;

  { customize tooltips to use rounded corners. }
  Theme.Images[tiTooltip] := TooltipRounded;
  Theme.Corners[tiTooltip] := Vector4Integer(9, 9, 9, 9);

  Notifications := TCastleNotifications.Create(Window);
  Notifications.Anchor(vpBottom, 10);
  Notifications.Anchor(hpMiddle);
  Notifications.TextAlignment := hpMiddle;
  Notifications.Timeout := 2.0;
  Notifications.Fade := 0.5;
  Window.Controls.InsertBack(Notifications);

  Background := TCastleSimpleBackground.Create(Window);
  Background.Color := Green;
  Window.Controls.InsertBack(Background);

  btnReinitialize := TCastleButton.Create(Window);
  btnReinitialize.Caption := 'Reinitialize joysticks';
  btnReinitialize.Tooltip := 'Free "Joysticks" instance and enable it again';
  btnReinitialize.Left := 10;
  btnReinitialize.Bottom := 10;
  btnReinitialize.OnClick := @TClicksHandler(nil).btnReinitializeClick;
  Window.Controls.InsertFront(btnReinitialize);

  Title := TCastleLabel.Create(Window);
  Title.Left := 10;
  Title.Anchor(vpTop, -10);
  Window.Controls.InsertFront(Title);

  OnScreenMenu := TCastleOnScreenMenu.Create(Window);
  OnScreenMenu.Left := 10;
  OnScreenMenu.Anchor(vpTop, -40);
  Window.Controls.InsertFront(OnScreenMenu);

  InitializeJoysticks;

  Window.OnUpdate := @WindowUpdate;
  Window.OpenAndRun;
end.
