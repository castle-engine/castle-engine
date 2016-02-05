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
  CastleUIControls, CastleNotifications, CastleLog;

var
  Window: TCastleWindowCustom;
  Notifications: TCastleNotifications;
  Background: TCastleSimpleBackground;
  Button, ButtonDemo: TCastleButton;
  Image, Image2, ImageDemo: TCastleImageControl;
  OnScreenMenu: TCastleOnScreenMenu;
  Touch: TCastleTouchControl;
  SliderDemo, SliderUIExplicitScale: TCastleFloatSlider;
  ImageWithBorders: TCastleImageControl;

type
  TClicksHandler = class
    procedure ButtonClick(Sender: TObject);
    procedure ButtonDemoClick(Sender: TObject);
    procedure OnScreenMenuItemChoose(Sender: TObject);
    procedure UIExplicitScaleChange(Sender: TObject);
  end;

procedure TClicksHandler.ButtonClick(Sender: TObject);
begin
  Notifications.Show('Button clicked');
end;

procedure TClicksHandler.ButtonDemoClick(Sender: TObject);
begin
  Notifications.Show('Button in on-screen menu clicked');
end;

procedure TClicksHandler.OnScreenMenuItemChoose(Sender: TObject);
begin
  Notifications.Show(Format('On-screen menu item chosen: %d',
    [OnScreenMenu.CurrentItem]));
  case OnScreenMenu.CurrentItem of
    5: Window.Container.UIScaling := usExplicitScale;
    7: Window.Container.UIScaling := usEncloseReferenceSize;
    8: Window.Container.UIScaling := usFitReferenceSize;
  end;
end;

procedure TClicksHandler.UIExplicitScaleChange(Sender: TObject);
begin
  // to test UIScaling = usExplicitScale
  Window.Container.UIScaling := usExplicitScale;
  Window.Container.UIExplicitScale := SliderUIExplicitScale.Value;
end;

begin
  Window := TCastleWindowCustom.Create(Application);

  InitializeLog;

  // to test UIScaling = usEncloseReferenceSize or usFitReferenceSize
  Window.Container.UIReferenceWidth := 1024;
  Window.Container.UIReferenceHeight := 768;

  { customize tooltips to use rounded corners.
    Just because we can :) }
  Theme.Images[tiTooltip] := TooltipRounded;
  Theme.Corners[tiTooltip] := Vector4Integer(9, 9, 9, 9);

  ImageWithBorders := TCastleImageControl.Create(Window);
  ImageWithBorders.URL := ApplicationData('box_with_borders.png');
  ImageWithBorders.Corners := Vector4Integer(40, 40, 40, 40);
  ImageWithBorders.Stretch := true;
  ImageWithBorders.FullSize := true;
  //ImageWithBorders.Rotation := 0.5;
  Window.Controls.InsertBack(ImageWithBorders);

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
  //Image2.SmoothScaling := false;
  Window.Controls.InsertFront(Image2);

  ImageDemo := TCastleImageControl.Create(Window);
  ImageDemo.URL := ApplicationData('sample_image_with_alpha.png');
  ImageDemo.Stretch := true;
  //ImageDemo.SmoothScaling := false;
  ImageDemo.Width := 100;
  ImageDemo.Height := 100;

  SliderDemo := TCastleFloatSlider.Create(Window);

  SliderUIExplicitScale := TCastleFloatSlider.Create(Window);
  SliderUIExplicitScale.Min := 0.33;
  SliderUIExplicitScale.Max := 3.0;
  SliderUIExplicitScale.Value := Window.Container.UIExplicitScale;
  SliderUIExplicitScale.OnChange := @TClicksHandler(nil).UIExplicitScaleChange;

  ButtonDemo := TCastleButton.Create(Window);
  ButtonDemo.Caption := 'Button inside an on-screen menu';
  ButtonDemo.OnClick := @TClicksHandler(nil).ButtonDemoClick;

  OnScreenMenu := TCastleOnScreenMenu.Create(Window);
  OnScreenMenu.Add('demo item');
  OnScreenMenu.Add('another demo item');
  OnScreenMenu.Add('item with image', ImageDemo);
  OnScreenMenu.Add('item with slider', SliderDemo);
  OnScreenMenu.Add('item with button', ButtonDemo);
  OnScreenMenu.Add('Container.UIScaling := usExplicitScale;');
  OnScreenMenu.Add('Container.UIExplicitScale', SliderUIExplicitScale);
  OnScreenMenu.Add('Container.UIScaling := usEncloseReferenceSize;');
  OnScreenMenu.Add('Container.UIScaling := usFitReferenceSize;');
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
