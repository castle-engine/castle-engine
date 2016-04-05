{
  Copyright 2016 Tomasz Wojtyś.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Demo lists all avalaible joysticks/gamepads in the system. It also shows how
  joystick events work. }

program joystick_demo;

{$apptype GUI}

uses SysUtils,
  CastleVectors, CastleWindow, CastleControls, CastleOnScreenMenu,
  CastleControlsImages, CastleImages, CastleFilesUtils, CastleColors,
  CastleUIControls, CastleNotifications, CastleLog, CastleJoysticks;

var
  Window: TCastleWindowCustom;
  Notifications: TCastleNotifications;
  Background: TCastleSimpleBackground;
  Button, ButtonDemo: TCastleButton;
  Image, ImageInsideMenu: TCastleImageControl;
  OnScreenMenu: TCastleOnScreenMenu;
  JoyAxisBar: TCastleLabel;

type
  TClicksHandler = class
    procedure ButtonClick(Sender: TObject);
    procedure ButtonDemoClick(Sender: TObject);
    procedure UIScalingExplicitTwiceClick(Sender: TObject);
    procedure UIScalingExplicitHalfClick(Sender: TObject);
    procedure UIScalingExplicitNormalClick(Sender: TObject);
    procedure UIScalingEncloseReferenceClick(Sender: TObject);
    procedure UIScalingFitReferenceClick(Sender: TObject);
    procedure RotationChange(Sender: TObject);
  end;

  TEventsHandler = class
    procedure JoyAxisMove(const Joy: PJoy; const Axis: Byte; const Value: Single);
    procedure JoyButtonPress(const Joy: PJoy; const Button: Byte);
    procedure JoyButtonUp(const Joy: PJoy; const Button: Byte);
    procedure JoyButtonDown(const Joy: PJoy; const Button: Byte);
  end;

procedure TEventsHandler.JoyAxisMove(const Joy: PJoy; const Axis: Byte;
  const Value: Single);
begin
  JoyAxisBar.Caption := FloatToStr(Value);
end;

procedure TEventsHandler.JoyButtonPress(const Joy: PJoy; const Button: Byte);
begin

end;

procedure TEventsHandler.JoyButtonUp(const Joy: PJoy; const Button: Byte);
begin

end;

procedure TEventsHandler.JoyButtonDown(const Joy: PJoy; const Button: Byte);
begin

end;

procedure TClicksHandler.ButtonClick(Sender: TObject);
begin
  Notifications.Show('Button clicked');
end;

procedure TClicksHandler.ButtonDemoClick(Sender: TObject);
begin
  Notifications.Show('Button in on-screen menu clicked');
end;

procedure TClicksHandler.UIScalingExplicitTwiceClick(Sender: TObject);
begin
  Window.Container.UIExplicitScale := 2.0;
  Window.Container.UIScaling := usExplicitScale;
end;

procedure TClicksHandler.UIScalingExplicitHalfClick(Sender: TObject);
begin
  Window.Container.UIExplicitScale := 0.5;
  Window.Container.UIScaling := usExplicitScale;
end;

procedure TClicksHandler.UIScalingExplicitNormalClick(Sender: TObject);
begin
  Window.Container.UIExplicitScale := 1.0;
  Window.Container.UIScaling := usExplicitScale;
end;

procedure TClicksHandler.UIScalingEncloseReferenceClick(Sender: TObject);
begin
  Window.Container.UIReferenceWidth := 1024;
  Window.Container.UIReferenceHeight := 768;
  Window.Container.UIScaling := usEncloseReferenceSize;
end;

procedure TClicksHandler.UIScalingFitReferenceClick(Sender: TObject);
begin
  Window.Container.UIReferenceWidth := 1024;
  Window.Container.UIReferenceHeight := 768;
  Window.Container.UIScaling := usFitReferenceSize;
end;

procedure TClicksHandler.RotationChange(Sender: TObject);
begin

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

  Button := TCastleButton.Create(Window);
  Button.Caption := 'My Button (with a tooltip and icon!)';
  Button.Tooltip := 'Sample tooltip over my button';
  Button.Left := 10;
  Button.Bottom := 10;
  //Button.Image := LoadImage(ApplicationData('sample_button_icon.png'));
  Button.OwnsImage := true;
  Button.OnClick := @TClicksHandler(nil).ButtonClick;
  Window.Controls.InsertFront(Button);

  {Image := TCastleImageControl.Create(Window);
  Image.URL := ApplicationData('sample_image_with_alpha.png');
  Image.Left := 200;
  Image.Bottom := 150;
  Window.Controls.InsertFront(Image);   }

  {ImageInsideMenu := TCastleImageControl.Create(Window);
  ImageInsideMenu.URL := ApplicationData('sample_image_with_alpha.png');
  ImageInsideMenu.Stretch := true;
  ImageInsideMenu.Width := 100;
  ImageInsideMenu.Height := 100;  }

  ButtonDemo := TCastleButton.Create(Window);
  ButtonDemo.Caption := 'Button inside an on-screen menu';
  ButtonDemo.OnClick := @TClicksHandler(nil).ButtonDemoClick;

  JoyAxisBar := TCastleLabel.Create(Window);
  JoyAxisBar.Caption := 'joy axis';
  Window.Controls.InsertFront(JoyAxisBar);

  OnScreenMenu := TCastleOnScreenMenu.Create(Window);
  OnScreenMenu.Add('Demo item');
  OnScreenMenu.Add('Another demo item');
  OnScreenMenu.Add('Item with button', ButtonDemo);
  //OnScreenMenu.Add('Progress Bar', JoyAxisBar);
  OnScreenMenu.Add('UI Scale x2 (UIScaling := usExplicitScale, UIExplicitScale := 2.0)',
    @TClicksHandler(nil).UIScalingExplicitTwiceClick);
  OnScreenMenu.Add('UI Scale /2 (UIScaling := usExplicitScale, UIExplicitScale := 0.5)',
    @TClicksHandler(nil).UIScalingExplicitHalfClick);
  OnScreenMenu.Add('Do not Scale UI (UIScaling := usExplicitScale, UIExplicitScale := 1.0)',
    @TClicksHandler(nil).UIScalingExplicitNormalClick);
  OnScreenMenu.Add('UI Scale adjust to window size (usEncloseReferenceSize);',
    @TClicksHandler(nil).UIScalingEncloseReferenceClick);
  OnScreenMenu.Add('UI Scale adjust to window size (usFitReferenceSize);',
    @TClicksHandler(nil).UIScalingFitReferenceClick);
  OnScreenMenu.Left := 10;
  OnScreenMenu.Anchor(vpTop, -10);
  Window.Controls.InsertFront(OnScreenMenu);

  // Initialize joysticks singelton instance
  EnableJoysticks;

  // Bind joystick events
  Joysticks.OnAxisMove := @TEventsHandler(nil).JoyAxisMove;
  Joysticks.OnButtonDown := @TEventsHandler(nil).JoyButtonDown;
  Joysticks.OnButtonPress := @TEventsHandler(nil).JoyButtonPress;
  Joysticks.OnButtonUp := @TEventsHandler(nil).JoyButtonUp;

  // Window.Width := 400;
  // Window.Height := 500;
  Window.OnUpdate := @WindowUpdate;
  Window.OpenAndRun;
end.
