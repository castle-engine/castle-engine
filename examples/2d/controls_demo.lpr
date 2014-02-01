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

uses CastleVectors, CastleWindow, CastleControls, CastleOnScreenMenu,
  CastleControlsImages, CastleImages, CastleFilesUtils, CastleColors,
  CastleUIControls;
var
  Window: TCastleWindowCustom;
  Background: TCastleSimpleBackground;
  Button: TCastleButton;
  Image: TCastleImageControl;
  OnScreenMenu: TCastleOnScreenMenu;
  Touch: TCastleTouchControl;
begin
  Window := TCastleWindowCustom.Create(Application);

  { customize tooltips to use rounded corners.
    Just because we can :) }
  Theme.Images[tiTooltip] := TooltipRounded;
  Theme.Corners[tiTooltip] := Vector4Integer(9, 9, 9, 9);

  Background := TCastleSimpleBackground.Create(Window);
  Background.Color := Green;
  Window.Controls.InsertBack(Background);

  Button := TCastleButton.Create(Window);
  Button.Caption := 'My Button';
  Button.Tooltip := 'Sample tooltip over my button';
  Button.Left := 10;
  Button.Bottom := 10;
  Button.Image := LoadImage(ApplicationData('sample_button_icon.png'));
  Button.OwnsImage := true;
  Window.Controls.InsertFront(Button);

  Image := TCastleImageControl.Create(Window);
  Image.URL := ApplicationData('sample_image_with_alpha.png');
  Image.Left := 10;
  Image.Bottom := 150;
  Window.Controls.InsertFront(Image);

  OnScreenMenu := TCastleOnScreenMenu.Create(Window);
  OnScreenMenu.Items.Add('one');
  OnScreenMenu.Items.Add('two');
  OnScreenMenu.Position := Vector2Integer(10, 300);
  OnScreenMenu.PositionRelativeMenuX := prLow;
  OnScreenMenu.PositionRelativeMenuY := prLow;
  OnScreenMenu.PositionRelativeScreenX := prLow;
  OnScreenMenu.PositionRelativeScreenY := prLow;
  Window.Controls.InsertFront(OnScreenMenu);

  Touch := TCastleTouchControl.Create(Window);
  Touch.Left := 200;
  Touch.Bottom := 150;
  Window.Controls.InsertFront(Touch);

  Window.Width := 400;
  Window.Height := 500;
  Window.OpenAndRun;
end.

