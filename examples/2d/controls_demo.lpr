{
  Copyright 2013-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Simple demo of a couple of 2D controls from CastleControls. }
program controls_demo;

uses SysUtils,
  CastleVectors, CastleWindow, CastleControls, CastleOnScreenMenu,
  CastleControlsImages, CastleImages, CastleFilesUtils, CastleColors,
  CastleUIControls, CastleNotifications;

var
  Window: TCastleWindowCustom;
  Notifications: TCastleNotifications;
  Background: TCastleSimpleBackground;
  Button, ButtonInOnScreenMenu: TCastleButton;
  Image, Image2, ImageInOnScreenMenu: TCastleImageControl;
  OnScreenMenu: TCastleOnScreenMenu;
  Touch: TCastleTouchControl;
  SliderInOnScreenMenu: TCastleFloatSlider;

type
  TClicksHandler = class
    procedure ButtonClick(Sender: TObject);
    procedure ButtonInOnScreenMenuClick(Sender: TObject);
    procedure OnScreenMenuItemChoose(Sender: TObject);
  end;

procedure TClicksHandler.ButtonClick(Sender: TObject);
begin
  Notifications.Show('Button clicked');
end;

procedure TClicksHandler.ButtonInOnScreenMenuClick(Sender: TObject);
begin
  Notifications.Show('Button in on-screen menu clicked');
end;

procedure TClicksHandler.OnScreenMenuItemChoose(Sender: TObject);
begin
  Notifications.Show(Format('On-screen menu item chosen: %d',
    [OnScreenMenu.CurrentItem]));
end;

begin
  Window := TCastleWindowCustom.Create(Application);

  { customize tooltips to use rounded corners.
    Just because we can :) }
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
  Button.Image := LoadImage(ApplicationData('sample_button_icon.png'));
  Button.OwnsImage := true;
  Button.OnClick := @TClicksHandler(nil).ButtonClick;
  Window.Controls.InsertFront(Button);

  Image := TCastleImageControl.Create(Window);
  Image.URL := ApplicationData('sample_image_with_alpha.png');
  Image.Left := 200;
  Image.Bottom := 150;
  Window.Controls.InsertFront(Image);

  Image2 := TCastleImageControl.Create(Window);
  Image2.URL := ApplicationData('sample_image_with_alpha.png');
  Image2.Bottom := 150;
  Image2.Stretch := true;
  Image2.Width := 400;
  Image2.Height := 200;
  Image2.Anchor(hpRight, -10);
  Window.Controls.InsertFront(Image2);

  ImageInOnScreenMenu := TCastleImageControl.Create(Window);
  ImageInOnScreenMenu.URL := ApplicationData('sample_image_with_alpha.png');
  ImageInOnScreenMenu.Stretch := true;
  ImageInOnScreenMenu.Width := 100;
  ImageInOnScreenMenu.Height := 100;

  SliderInOnScreenMenu := TCastleFloatSlider.Create(Window);

  ButtonInOnScreenMenu := TCastleButton.Create(Window);
  ButtonInOnScreenMenu.Caption := 'Button inside an on-screen menu';
  ButtonInOnScreenMenu.OnClick := @TClicksHandler(nil).ButtonInOnScreenMenuClick;

  OnScreenMenu := TCastleOnScreenMenu.Create(Window);
  OnScreenMenu.Add('one');
  OnScreenMenu.Add('two');
  OnScreenMenu.Add('item with image', ImageInOnScreenMenu);
  OnScreenMenu.Add('item with slider', SliderInOnScreenMenu);
  OnScreenMenu.Add('item with button', ButtonInOnScreenMenu);
  OnScreenMenu.Left := 10;
  OnScreenMenu.Anchor(vpTop, -10);
  OnScreenMenu.OnClick := @TClicksHandler(nil).OnScreenMenuItemChoose;
  Window.Controls.InsertFront(OnScreenMenu);

  Touch := TCastleTouchControl.Create(Window);
  Touch.Left := 10;
  Touch.Bottom := 150;
  Window.Controls.InsertFront(Touch);

  // Window.Width := 400;
  // Window.Height := 500;
  Window.OpenAndRun;
end.
