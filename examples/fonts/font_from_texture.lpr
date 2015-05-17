{
  Copyright 2014-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Demo of using fonts from a texture:

  - Simple constant-width font with colorful glyphs,
    using TSimpleTextureFont class.
    Font data is supplied as an image (see data/sonic_asalga_0.png,
    null_terminator_0.png).

  - Font initialized from a FreeType font, using TTextureFont class.
    Font data is supplied as a ttf file (see data/DejaVuSans.ttf).
}

{$apptype CONSOLE}

program font_from_texture;

uses SysUtils, CastleWindow, CastleControls, CastleFonts, CastleImages,
  CastleColors, CastleVectors, CastleFilesUtils, CastleLog,
  CastleUIControls;

var
  Window: TCastleWindow;
  Label1, Label2,
    LabelDeja, LabelDejaBW, LabelDejaLarge, LabelDejaLargeOutline,
    LabelStylish, LabelStylishBW, LabelStylishLarge: TCastleLabel;

procedure Resize(Container: TUIContainer);
const
  Margin = 10;
var
  Y: Integer;

  procedure PositionLabel(L: TCastleLabel);
  begin
    Y -= Integer(L.Rect.Height) + Margin;
    L.Align(hpMiddle, hpMiddle);
    L.Bottom := Y;
  end;

begin
  { each time Window size changes, reposition labels }
  Y := Window.Height;
  PositionLabel(Label1);
  PositionLabel(Label2);
  PositionLabel(LabelDeja);
  PositionLabel(LabelDejaBw);
  PositionLabel(LabelDejaLarge);
  PositionLabel(LabelDejaLargeOutline);
  PositionLabel(LabelStylish);
  PositionLabel(LabelStylishBw);
  PositionLabel(LabelStylishLarge);
end;

var
  Background: TCastleSimpleBackground;
begin
  InitializeLog;

  Window := TCastleWindow.Create(Application);

  Background := TCastleSimpleBackground.Create(Window);
  Background.Color := Yellow;
  Window.Controls.InsertFront(Background);

  UIFont := TSimpleTextureFont.Create(
    LoadImage(ApplicationData('sonic_asalga_0.png')), 8, 12, 2, 2);
//  UIFont.Scale := 4; // test font Scale

  Label1 := TCastleLabel.Create(Window);
  Label1.Text.Append('A simple test of a font from an image.');
  Label1.Text.Append('Do cats eat bats?');
  Label1.Text.Append('and sometimes, Do bats eat cats?');
  Label1.Text.Append('1 + 2 + 3 = 6');
  Label1.Padding := 5;
  // do not assign Label1.CustomFont, so it will use global UIFont
  Window.Controls.InsertFront(Label1);

  Label2 := TCastleLabel.Create(Window);
  Label2.Text.Append('Yet another label.');
  Label2.Text.Append('With different font. Just because we can :)');
  Label2.Padding := 5;
  Label2.CustomFont := TSimpleTextureFont.Create(
    LoadImage(ApplicationData('null_terminator_0.png')), 8, 12, 1, 1);
  Label2.OwnsCustomFont := true;
  Label2.Color := Red;
  Window.Controls.InsertFront(Label2);

  LabelDeja := TCastleLabel.Create(Window);
  LabelDeja.Text.Append('DejaVuSans font');
  LabelDeja.Text.Append('with anti-aliasing.');
  LabelDeja.Padding := 5;
  LabelDeja.CustomFont := TTextureFont.Create(ApplicationData('DejaVuSans.ttf'), 15, true);
  LabelDeja.OwnsCustomFont := true;
  Window.Controls.InsertFront(LabelDeja);

  LabelDejaBW := TCastleLabel.Create(Window);
  LabelDejaBW.Text.Append('DejaVuSans font');
  LabelDejaBW.Text.Append('without anti-aliasing.');
  LabelDejaBW.Padding := 5;
  LabelDejaBW.CustomFont := TTextureFont.Create(ApplicationData('DejaVuSans.ttf'), 15, false);
  LabelDejaBW.OwnsCustomFont := true;
  Window.Controls.InsertFront(LabelDejaBW);

  LabelDejaLarge := TCastleLabel.Create(Window);
  LabelDejaLarge.Text.Append('DejaVuSans font with anti-aliasing');
  LabelDejaLarge.Text.Append('and larger size.');
  LabelDejaLarge.Color := Vector4Single(0.5, 0.5, 1, 1); // light blue
  LabelDejaLarge.Padding := 5;
  LabelDejaLarge.CustomFont := TTextureFont.Create(ApplicationData('DejaVuSans.ttf'), 30, true);
  LabelDejaLarge.OwnsCustomFont := true;
  Window.Controls.InsertFront(LabelDejaLarge);

  LabelDejaLargeOutline := TCastleLabel.Create(Window);
  LabelDejaLargeOutline.Text.Append('DejaVuSans font with anti-aliasing');
  LabelDejaLargeOutline.Text.Append('and larger size and outline.');
  LabelDejaLargeOutline.Frame := false;
  LabelDejaLargeOutline.Color := White;
  LabelDejaLargeOutline.Padding := 5;
  LabelDejaLargeOutline.CustomFont := TTextureFont.Create(ApplicationData('DejaVuSans.ttf'), 30, true);
  LabelDejaLargeOutline.CustomFont.Outline := 2;
  LabelDejaLargeOutline.CustomFont.OutlineColor := Red;
  LabelDejaLargeOutline.OwnsCustomFont := true;
  Window.Controls.InsertFront(LabelDejaLargeOutline);

  LabelStylish := TCastleLabel.Create(Window);
  LabelStylish.Text.Text :=
    'Stylish "old typewriter" font' +LineEnding+
    'with anti-aliasing.';
  LabelStylish.Padding := 5;
  LabelStylish.CustomFont := TTextureFont.Create(ApplicationData('PARPG.ttf'), 15, true);
  LabelStylish.OwnsCustomFont := true;
  Window.Controls.InsertFront(LabelStylish);

  LabelStylishBW := TCastleLabel.Create(Window);
  LabelStylishBW.Text.Text :=
    'Stylish "old typewriter" font' +LineEnding+
    'without anti-aliasing.';
  LabelStylishBW.Padding := 5;
  LabelStylishBW.CustomFont := TTextureFont.Create(ApplicationData('PARPG.ttf'), 15, false);
  LabelStylishBW.OwnsCustomFont := true;
  Window.Controls.InsertFront(LabelStylishBW);

  LabelStylishLarge := TCastleLabel.Create(Window);
  LabelStylishLarge.Text.Text :=
    'Stylish "old typewriter" font' +LineEnding+
    'with anti-aliasing' +LineEnding+
    'and larger size.';
  LabelStylishLarge.Padding := 5;
  LabelStylishLarge.CustomFont := TTextureFont.Create(ApplicationData('PARPG.ttf'), 30, true);
//  LabelStylishLarge.CustomFont.Scale := 4; // test font Scale
  LabelStylishLarge.OwnsCustomFont := true;
  Window.Controls.InsertFront(LabelStylishLarge);

  Window.OnResize := @Resize;
  Window.OpenAndRun;
end.