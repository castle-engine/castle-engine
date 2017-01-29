{
  Copyright 2013-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Simple demo of a couple of 2D controls.
  This is the unit with common code for both Android and standalone. }
unit Game;

interface

uses CastleWindow;

var
  Window: TCastleWindowCustom;

implementation

uses SysUtils,
  CastleVectors, CastleControls, CastleOnScreenMenu,
  CastleControlsImages, CastleImages, CastleFilesUtils, CastleColors,
  CastleUIControls, CastleNotifications, CastleLog;

var
  Notifications: TCastleNotifications;
  Background: TCastleSimpleBackground;
  Button, ButtonDemo: TCastleButton;
  Image, Image2, ImageInsideMenu, ImageWithBorders: TCastleImageControl;
  OnScreenMenu: TCastleOnScreenMenu;
  Touch: TCastleTouchControl;
  SliderRotation: TCastleFloatSlider;
  Rect1, Rect2, Rect3, Rect4, Circle1, Circle2, Circle3, Circle4: TCastleShape;

type
  TClicksHandler = class
    class procedure ButtonClick(Sender: TObject);
    class procedure ButtonDemoClick(Sender: TObject);
    class procedure ClickableItemClick(Sender: TObject);
    class procedure UIScalingExplicitTwiceClick(Sender: TObject);
    class procedure UIScalingExplicitHalfClick(Sender: TObject);
    class procedure UIScalingExplicitNormalClick(Sender: TObject);
    class procedure UIScalingEncloseReferenceClick(Sender: TObject);
    class procedure UIScalingFitReferenceClick(Sender: TObject);
    class procedure RotationChange(Sender: TObject);
  end;

class procedure TClicksHandler.ButtonClick(Sender: TObject);
begin
  Notifications.Show('Button clicked');
end;

class procedure TClicksHandler.ButtonDemoClick(Sender: TObject);
begin
  Notifications.Show('Button in on-screen menu clicked');
end;

class procedure TClicksHandler.ClickableItemClick(Sender: TObject);
begin
  Notifications.Show('Clickable item in on-screen menu clicked');
end;

class procedure TClicksHandler.UIScalingExplicitTwiceClick(Sender: TObject);
begin
  Window.Container.UIExplicitScale := 2.0;
  Window.Container.UIScaling := usExplicitScale;
end;

class procedure TClicksHandler.UIScalingExplicitHalfClick(Sender: TObject);
begin
  Window.Container.UIExplicitScale := 0.5;
  Window.Container.UIScaling := usExplicitScale;
end;

class procedure TClicksHandler.UIScalingExplicitNormalClick(Sender: TObject);
begin
  Window.Container.UIExplicitScale := 1.0;
  Window.Container.UIScaling := usExplicitScale;
end;

class procedure TClicksHandler.UIScalingEncloseReferenceClick(Sender: TObject);
begin
  Window.Container.UIReferenceWidth := 1024;
  Window.Container.UIReferenceHeight := 768;
  Window.Container.UIScaling := usEncloseReferenceSize;
end;

class procedure TClicksHandler.UIScalingFitReferenceClick(Sender: TObject);
begin
  Window.Container.UIReferenceWidth := 1024;
  Window.Container.UIReferenceHeight := 768;
  Window.Container.UIScaling := usFitReferenceSize;
end;

class procedure TClicksHandler.RotationChange(Sender: TObject);
begin
  Image.Rotation := SliderRotation.Value;
  Image2.Rotation := SliderRotation.Value;
  ImageInsideMenu.Rotation := SliderRotation.Value;
  ImageWithBorders.Rotation := SliderRotation.Value;
end;

procedure ApplicationInitialize;
const
  TestClip = false;
  TestClipLine: TVector3Single = (0.5, -1, 0);
begin
  { customize tooltips to use rounded corners.
    Just because we can :) }
  Theme.Images[tiTooltip] := TooltipRounded;
  Theme.Corners[tiTooltip] := Vector4Integer(9, 9, 9, 9);

  ImageWithBorders := TCastleImageControl.Create(Window);
  ImageWithBorders.URL := ApplicationData('box_with_borders.png');
  ImageWithBorders.Corners := Vector4Integer(40, 40, 40, 40);
  ImageWithBorders.Stretch := true;
  ImageWithBorders.FullSize := true;
  ImageWithBorders.Clip := TestClip;
  ImageWithBorders.ClipLine := TestClipLine;
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
  Image.Clip := TestClip;
  Image.ClipLine := TestClipLine;
  Window.Controls.InsertFront(Image);

  Image2 := TCastleImageControl.Create(Window);
  Image2.URL := ApplicationData('sample_image_with_alpha.png');
  Image2.Bottom := 150;
  Image2.Stretch := true;
  Image2.Width := 400;
  Image2.Height := 200;
  Image2.Anchor(hpRight, -10);
  //Image2.SmoothScaling := false;
  Image2.Clip := TestClip;
  Image2.ClipLine := TestClipLine;
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
  OnScreenMenu.Add('Clickable item', @TClicksHandler(nil).ClickableItemClick);
  OnScreenMenu.Add('Another clickable item', @TClicksHandler(nil).ClickableItemClick);
  OnScreenMenu.Add('Non-clickable item');
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

  Rect1 := TCastleShape.Create(Window);
  Rect1.Width := 50;
  Rect1.Height := 100;
  Rect1.Anchor(vpBottom, 10);
  Rect1.Anchor(hpRight, -10);
  Rect1.Outline := true;
  Rect1.OutlineWidth := 4;
  Window.Controls.InsertFront(Rect1);

  Rect2 := TCastleShape.Create(Window);
  Rect2.Width := 50;
  Rect2.Height := 100;
  Rect2.Anchor(vpBottom, 10);
  Rect2.Anchor(hpRight, -10 * 2 - 50);
  Rect2.Filled := false;
  Rect2.Outline := true;
  Rect2.OutlineWidth := 2;
  Rect2.OutlineColor := Red;
  Window.Controls.InsertFront(Rect2);

  Circle1 := TCastleShape.Create(Window);
  Circle1.ShapeType := stCircle;
  Circle1.Width := 50;
  Circle1.Height := 100;
  Circle1.Anchor(vpBottom, 10);
  Circle1.Anchor(hpRight, -10 * 3 - 50 * 2);
  Circle1.Color := Yellow;
  Circle1.Outline := true;
  Circle1.OutlineWidth := 4;
  Circle1.OutlineColor := Blue;
  Window.Controls.InsertFront(Circle1);

  Circle2 := TCastleShape.Create(Window);
  Circle2.ShapeType := stCircle;
  Circle2.Width := 50;
  Circle2.Height := 100;
  Circle2.Anchor(vpBottom, 10);
  Circle2.Anchor(hpRight, -10 * 4 - 50 * 3);
  Circle2.Filled := false;
  Circle2.Color := Yellow;
  Circle2.Outline := true;
  Circle2.OutlineWidth := 1;
  Window.Controls.InsertFront(Circle2);

  { now the same thing as Rect1,2 and Circle1,2 but with OutlineThick = true }

  Rect3 := TCastleShape.Create(Window);
  Rect3.Width := 50;
  Rect3.Height := 100;
  Rect3.Anchor(vpBottom, 120);
  Rect3.Anchor(hpRight, -10);
  Rect3.Outline := true;
  Rect3.OutlineThick := true;
  Rect3.OutlineWidth := 4;
  Window.Controls.InsertFront(Rect3);

  Rect4 := TCastleShape.Create(Window);
  Rect4.Width := 50;
  Rect4.Height := 100;
  Rect4.Anchor(vpBottom, 120);
  Rect4.Anchor(hpRight, -10 * 2 - 50);
  Rect4.Filled := false;
  Rect4.Outline := true;
  Rect4.OutlineThick := true;
  Rect4.OutlineWidth := 2;
  Rect4.OutlineColor := Red;
  Window.Controls.InsertFront(Rect4);

  Circle3 := TCastleShape.Create(Window);
  Circle3.ShapeType := stCircle;
  Circle3.Width := 50;
  Circle3.Height := 100;
  Circle3.Anchor(vpBottom, 120);
  Circle3.Anchor(hpRight, -10 * 3 - 50 * 2);
  Circle3.Color := Yellow;
  Circle3.Outline := true;
  Circle3.OutlineThick := true;
  Circle3.OutlineWidth := 4;
  Circle3.OutlineColor := Blue;
  Window.Controls.InsertFront(Circle3);

  Circle4 := TCastleShape.Create(Window);
  Circle4.ShapeType := stCircle;
  Circle4.Width := 50;
  Circle4.Height := 100;
  Circle4.Anchor(vpBottom, 120);
  Circle4.Anchor(hpRight, -10 * 4 - 50 * 3);
  Circle4.Filled := false;
  Circle4.Color := Yellow;
  Circle4.Outline := true;
  Circle4.OutlineThick := true;
  Circle4.OutlineWidth := 1;
  Window.Controls.InsertFront(Circle4);
end;

function MyGetApplicationName: string;
begin
  Result := 'show_various_ui_controls';
end;

initialization
  { This should be done as early as possible to mark our log lines correctly. }
  OnGetApplicationName := @MyGetApplicationName;

  Window := TCastleWindowCustom.Create(Application);

  Application.MainWindow := Window;
  Application.OnInitialize := @ApplicationInitialize;
end.
