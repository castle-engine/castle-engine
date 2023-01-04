{
  Copyright 2016-2019, 2022 Tomasz Wojtyś, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ List all avalaible joysticks/gamepads in the system,
  and allows to test their inputs. }
unit GameInitialize;

interface

implementation

uses SysUtils, Classes,
  CastleVectors, CastleWindow, CastleControls, CastleOnScreenMenu,
  CastleImages, CastleFilesUtils, CastleColors,
  CastleUIControls, CastleNotifications, CastleLog, CastleJoysticks,
  CastleApplicationProperties, CastleUtils;

{ TJoyAxisVisualize ---------------------------------------------------------- }

type
  { Visualize joystick axis (2D vector) state. }
  TJoyAxisVisualize = class(TCastleRectangleControl)
  strict private
    FAxis: TVector2;
    Shape: TCastleShape;
    Lab: TCastleLabel;
    procedure SetAxis(const Value: TVector2);
  public
    constructor Create(AOwner: TComponent); override;
    property Axis: TVector2 read FAxis write SetAxis;
  end;

constructor TJoyAxisVisualize.Create(AOwner: TComponent);
begin
  inherited;
  Color := Vector4(0, 0.5, 0, 1); // dark green
  Border.AllSides := 2;
  BorderColor := Yellow;
  Width := 500;
  Height := 500;

  Shape := TCastleShape.Create(Self);
  Shape.ShapeType := stCircle;
  Shape.Color := Yellow;
  Shape.Anchor(hpMiddle);
  Shape.Anchor(vpMiddle);
  Shape.Width := 100;
  Shape.Height := 100;
  InsertFront(Shape);

  Lab := TCastleLabel.Create(Self);
  Lab.Caption := 'Joystick Axis';
  Lab.Color := Yellow;
  Lab.Anchor(hpLeft);
  Lab.Anchor(vpBottom);
  InsertFront(Lab);
end;

procedure TJoyAxisVisualize.SetAxis(const Value: TVector2);
begin
  FAxis := Value;
  Shape.Translation := Value * Vector2(Width - Shape.Width, Height - Shape.Height) / 2;
end;

{ globals -------------------------------------------------------------------- }

const
  AxisNames: array [JOY_AXIS_X..JOY_POVY] of string =
  ( 'JOY_AXIS_X',
    'JOY_AXIS_Y',
    'JOY_AXIS_Z',
    'JOY_AXIS_R',
    'JOY_AXIS_U',
    'JOY_AXIS_V',
    'JOY_POVX',
    'JOY_POVY'
  );

var
  Window: TCastleWindow;
  Notifications: TCastleNotifications;
  ButtonReinitialize: TCastleButton;
  OnScreenMenu: TCastleOnScreenMenu;
  LabelJoysticksCount: TCastleLabel;
  LabelSelectedJoystick: TCastleLabel;
  SelectedJoystick: Integer = -1;
  JoyButtons: array of TCastleButton;
  JoyAxes: array of TCastleLabel;
  JoyAxisVisualize: TJoyAxisVisualize;

{ TEventsHandler ------------------------------------------------------------- }

type
  TEventsHandler = class
  strict private
    class procedure ClearJoystickUI;
    class procedure ClearSelectedJoystickUI;
  public
    class procedure InitializeJoystickUI(Sender: TObject);
    class procedure JoystickDisconnected;
    class procedure JoystickConnected;
    class procedure ClickReinitialize(Sender: TObject);
    class procedure ClickJoystickSelect(Sender: TObject);
    class procedure JoyAxisMove(const Joy: TJoystick; const Axis: Byte; const Value: Single);
    class procedure JoyButtonPress(const Joy: TJoystick; const Button: Byte);
    class procedure JoyButtonUp(const Joy: TJoystick; const Button: Byte);
    class procedure JoyButtonDown(const Joy: TJoystick; const Button: Byte);
  end;

class procedure TEventsHandler.JoyAxisMove(const Joy: TJoystick; const Axis: Byte;
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

class procedure TEventsHandler.JoyButtonPress(const Joy: TJoystick; const Button: Byte);
begin
  Notifications.Show(Format('Button press event (b: %d)', [Button]));
end;

class procedure TEventsHandler.JoyButtonUp(const Joy: TJoystick; const Button: Byte);
begin
  if (SelectedJoystick = -1) or
     (Joy <> Joysticks[SelectedJoystick]) then
    Exit;
  if Length(JoyButtons) = 0 then Exit;

  JoyButtons[Button].Pressed := False;

  Notifications.Show('Joy button up');
end;

class procedure TEventsHandler.JoyButtonDown(const Joy: TJoystick; const Button: Byte);
begin
  if (SelectedJoystick = -1) or
     (Joy <> Joysticks[SelectedJoystick]) then
    Exit;
  if Length(JoyButtons) = 0 then Exit;

  JoyButtons[Button].Pressed := True;

  Notifications.Show('Joy button down');
end;

class procedure TEventsHandler.ClickReinitialize(Sender: TObject);
begin
  // If any joystick is plugged or unplugged then reinitialize is needed
  Notifications.Show('Detecting joysticks again...');
  Joysticks.Initialize;
end;

class procedure TEventsHandler.ClearSelectedJoystickUI;
var
  I: Integer;
begin
  for I := Low(JoyButtons) to High(JoyButtons) do
    JoyButtons[i].Free;
  SetLength(JoyButtons, 0);
  for I := Low(JoyAxes) to High(JoyAxes) do
    JoyAxes[i].Free;
  SetLength(JoyAxes, 0);
  FreeAndNil(JoyAxisVisualize);

  SelectedJoystick := -1;
  LabelSelectedJoystick.Caption := 'Selected: none';
end;

class procedure TEventsHandler.ClearJoystickUI;
begin
  OnScreenMenu.MenuItems.ClearControls;
  ClearSelectedJoystickUI;
end;

class procedure TEventsHandler.ClickJoystickSelect(Sender: TObject);
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
    JoyButtons[i] := TCastleButton.Create(Application);
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
  //SetLength(JoyAxes, Joysticks.GetInfo(SelectedJoystick)^.Count.Axes);
  { Show all 8 possible axes, as D-Pad/POV axes are sometimes "further than Count.Axes"
    This is a temporary measure until we can separate D-Pad/POV correctly in the backend }
  SetLength(JoyAxes, 8);
  for I := 0 to High(JoyAxes) do
  begin
    JoyAxes[i] := TCastleLabel.Create(Application);
    JoyAxes[i].Left := 10;
    JoyAxes[i].Bottom := 120 + i * 45;
    JoyAxes[i].Caption := 'Axis: ' + IntToStr(i);
    JoyAxes[i].Color := White;
    Window.Controls.InsertFront(JoyAxes[i]);
  end;
  Notifications.Show(Format('Found %d axes',
    [Joysticks.GetInfo(SelectedJoystick)^.Count.Axes]));

  JoyAxisVisualize := TJoyAxisVisualize.Create(Application);
  JoyAxisVisualize.Anchor(hpRight, -10);
  JoyAxisVisualize.Anchor(vpTop, -10);
  Window.Controls.InsertFront(JoyAxisVisualize);
end;

class procedure TEventsHandler.InitializeJoystickUI(Sender: TObject);
var
  I: Integer;
  JoystickSelect: TCastleOnScreenMenuItem;
begin
  ClearJoystickUI;

  LabelJoysticksCount.Caption := Format('Number of joysticks found: %d', [Joysticks.Count]);

  // List all joysticks
  for I := 0 to Joysticks.Count - 1 do
  begin
    JoystickSelect := TCastleOnScreenMenuItem.Create(Application);
    JoystickSelect.Caption := Format('[%d] %s', [I, Joysticks.GetInfo(I)^.Name]);
    JoystickSelect.OnClick := {$ifdef FPC}@{$endif} TEventsHandler {$ifdef FPC}(nil){$endif}.ClickJoystickSelect;
    JoystickSelect.Tag := I;
    OnScreenMenu.Add(JoystickSelect);
  end;
end;

class procedure TEventsHandler.JoystickDisconnected;
begin
  Joysticks.Initialize;
  //will call OnChange and therefore InitializeJoystickUI
end;

class procedure TEventsHandler.JoystickConnected;
begin
  Joysticks.Initialize;
  //will call OnChange and therefore InitializeJoystickUI
end;

{ other routines ------------------------------------------------------------- }

procedure WindowUpdate(Container: TCastleContainer);
begin
  { update JoyAxisVisualize.Axis }
  if SelectedJoystick <> -1 then
  begin
    Assert(JoyAxisVisualize <> nil);
    JoyAxisVisualize.Axis := Joysticks[SelectedJoystick].Axis;
  end;
end;

{ One-time initialization of resources. }
procedure ApplicationInitialize;
var
  MenuGroup: TCastleVerticalGroup;
begin
  { Adjust container settings for a scalable UI (adjusts to any window size in a smart way). }
  Window.Container.LoadSettings('castle-data:/CastleSettings.xml');

  Window.Container.BackgroundColor := Green;
  Window.OnUpdate := @WindowUpdate;

  Notifications := TCastleNotifications.Create(Application);
  Notifications.Anchor(vpBottom, 10);
  Notifications.Anchor(hpMiddle);
  Notifications.TextAlignment := hpMiddle;
  Notifications.Timeout := 2.0;
  Notifications.Fade := 0.5;
  Window.Controls.InsertBack(Notifications);

  ButtonReinitialize := TCastleButton.Create(Application);
  ButtonReinitialize.Caption := 'Detect connected joysticks again (Joysticks.Initialize)';
  ButtonReinitialize.Left := 10;
  ButtonReinitialize.Bottom := 10;
  ButtonReinitialize.OnClick := {$ifdef FPC}@{$endif} TEventsHandler {$ifdef FPC}(nil){$endif}.ClickReinitialize;
  Window.Controls.InsertFront(ButtonReinitialize);

  MenuGroup := TCastleVerticalGroup.Create(Application);
  MenuGroup.Left := 10;
  MenuGroup.Anchor(vpTop, -10);
  MenuGroup.Spacing := 10;
  Window.Controls.InsertFront(MenuGroup);

  LabelJoysticksCount := TCastleLabel.Create(Application);
  LabelJoysticksCount.Color := White;
  MenuGroup.InsertFront(LabelJoysticksCount);

  OnScreenMenu := TCastleOnScreenMenu.Create(Application);
  MenuGroup.InsertFront(OnScreenMenu);

  LabelSelectedJoystick := TCastleLabel.Create(Application);
  LabelSelectedJoystick.Color := White;
  LabelSelectedJoystick.Caption := 'Selected: none';
  MenuGroup.InsertFront(LabelSelectedJoystick);

  Joysticks.OnChange := {$ifdef FPC}@{$endif} TEventsHandler {$ifdef FPC}(nil){$endif}.InitializeJoystickUI;
  Joysticks.OnDisconnect := {$ifdef FPC}@{$endif} TEventsHandler {$ifdef FPC}(nil){$endif}.JoystickDisconnected;
  Joysticks.OnConnect := {$ifdef FPC}@{$endif} TEventsHandler {$ifdef FPC}(nil){$endif}.JoystickConnected;
  Joysticks.OnAxisMove := {$ifdef FPC}@{$endif} TEventsHandler {$ifdef FPC}(nil){$endif}.JoyAxisMove;
  Joysticks.OnButtonDown := {$ifdef FPC}@{$endif} TEventsHandler {$ifdef FPC}(nil){$endif}.JoyButtonDown;
  Joysticks.OnButtonPress := {$ifdef FPC}@{$endif} TEventsHandler {$ifdef FPC}(nil){$endif}.JoyButtonPress;
  Joysticks.OnButtonUp := {$ifdef FPC}@{$endif} TEventsHandler {$ifdef FPC}(nil){$endif}.JoyButtonUp;

  { Actually detect joysticks.
    This will automatically call TEventsHandler.JoysticksChanged on some platforms. }
  Joysticks.Initialize;
end;

initialization
  { This initialization section configures:
    - Application.OnInitialize
    - Application.MainWindow
    - determines initial window size

    You should not need to do anything more in this initialization section.
    Most of your actual application initialization (in particular, any file reading)
    should happen inside ApplicationInitialize. }

  Application.OnInitialize := @ApplicationInitialize;

  Window := TCastleWindow.Create(Application);
  Application.MainWindow := Window;

  { Optionally, adjust window fullscreen state and size at this point.
    Examples:

    Run fullscreen:

      Window.FullScreen := true;

    Run in a 600x400 window:

      Window.FullScreen := false; // default
      Window.Width := 600;
      Window.Height := 400;

    Run in a window taking 2/3 of screen (width and height):

      Window.FullScreen := false; // default
      Window.Width := Application.ScreenWidth * 2 div 3;
      Window.Height := Application.ScreenHeight * 2 div 3;

    Note that some platforms (like mobile) ignore these window sizes.
  }

  { Handle command-line parameters like --fullscreen and --window.
    By doing this last, you let user to override your fullscreen / mode setup. }
  Window.ParseParameters;
end.
