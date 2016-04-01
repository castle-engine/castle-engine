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
  Image, Image2, ImageInsideMenu, ImageWithBorders: TCastleImageControl;
  OnScreenMenu: TCastleOnScreenMenu;
  Touch: TCastleTouchControl;
  SliderRotation: TCastleFloatSlider;

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
  Image.Rotation := SliderRotation.Value;
  Image2.Rotation := SliderRotation.Value;
  ImageInsideMenu.Rotation := SliderRotation.Value;
  ImageWithBorders.Rotation := SliderRotation.Value;
end;

begin
  Window := TCastleWindowCustom.Create(Application);

  InitializeLog;

  { customize tooltips to use rounded corners.
    Just because we can :) }
  Theme.Images[tiTooltip] := TooltipRounded;
  Theme.Corners[tiTooltip] := Vector4Integer(9, 9, 9, 9);

  ImageWithBorders := TCastleImageControl.Create(Window);
  ImageWithBorders.URL := ApplicationData('box_with_borders.png');
  ImageWithBorders.Corners := Vector4Integer(40, 40, 40, 40);
  ImageWithBorders.Stretch := true;
  ImageWithBorders.FullSize := true;
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

  ImageInsideMenu := TCastleImageControl.Create(Window);
  ImageInsideMenu.URL := ApplicationData('sample_image_with_alpha.png');
  ImageInsideMenu.Stretch := true;
  ImageInsideMenu.Width := 100;
  ImageInsideMenu.Height := 100;

  SliderRotation := TCastleFloatSlider.Create(Window);
  SliderRotation.Min := -Pi;
  SliderRotation.Max := Pi;
  SliderRotation.Value := 0;
  SliderRotation.OnChange := @TClicksHandler(nil).RotationChange;

  ButtonDemo := TCastleButton.Create(Window);
  ButtonDemo.Caption := 'Button inside an on-screen menu';
  ButtonDemo.OnClick := @TClicksHandler(nil).ButtonDemoClick;

  OnScreenMenu := TCastleOnScreenMenu.Create(Window);
  OnScreenMenu.Add('Demo item');
  OnScreenMenu.Add('Another demo item');
  OnScreenMenu.Add('Item with image', ImageInsideMenu);
  OnScreenMenu.Add('Item with button', ButtonDemo);
  OnScreenMenu.Add('Slider to test images rotation', SliderRotation);
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

  Touch := TCastleTouchControl.Create(Window);
  Touch.Left := 10;
  Touch.Bottom := 150;
  Window.Controls.InsertFront(Touch);

  // Window.Width := 400;
  // Window.Height := 500;
  Window.OpenAndRun;
end.
