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
  CastleColors, CastleVectors, CastleFilesUtils, CastleLog;

var
  Window: TCastleWindow;
  Label1, Label2,
    LabelDeja, LabelDejaBW, LabelDejaLarge,
    LabelStylish, LabelStylishBW, LabelStylishLarge: TCastleLabel;

procedure Open(Window: TCastleWindowBase);
begin
  UIFont := TSimpleTextureFont.Create(
    LoadImage(ApplicationData('sonic_asalga_0.png')), 8, 12, 2, 2);

  Label2.CustomFont := TSimpleTextureFont.Create(
    LoadImage(ApplicationData('null_terminator_0.png')), 8, 12, 1, 1);
  Label2.OwnsCustomFont := true;
  Label2.Color := Red;

  LabelDeja.CustomFont := TTextureFont.Create(ApplicationData('DejaVuSans.ttf'), 15, true);
  LabelDeja.OwnsCustomFont := true;
  LabelDejaBW.CustomFont := TTextureFont.Create(ApplicationData('DejaVuSans.ttf'), 15, false);
  LabelDejaBW.OwnsCustomFont := true;
  LabelDejaLarge.CustomFont := TTextureFont.Create(ApplicationData('DejaVuSans.ttf'), 30, true);
  LabelDejaLarge.OwnsCustomFont := true;

  LabelStylish.CustomFont := TTextureFont.Create(ApplicationData('PARPG.ttf'), 15, true);
  LabelStylish.OwnsCustomFont := true;
  LabelStylishBW.CustomFont := TTextureFont.Create(ApplicationData('PARPG.ttf'), 15, false);
  LabelStylishBW.OwnsCustomFont := true;
  LabelStylishLarge.CustomFont := TTextureFont.Create(ApplicationData('PARPG.ttf'), 30, true);
  LabelStylishLarge.OwnsCustomFont := true;
end;

procedure Resize(Window: TCastleWindowBase);
const
  Margin = 10;
var
  Y: Integer;

  procedure PositionLabel(L: TCastleLabel);
  begin
    Y -= Integer(L.Rect.Height) + Margin;
    L.AlignHorizontal;
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

  Label1 := TCastleLabel.Create(Window);
  Label1.Text.Append('A simple test of a font from an image.');
  Label1.Text.Append('Do cats eat bats?');
  Label1.Text.Append('and sometimes, Do bats eat cats?');
  Label1.Text.Append('1 + 2 + 3 = 6');
  Label1.Padding := 5;
  Window.Controls.InsertFront(Label1);

  Label2 := TCastleLabel.Create(Window);
  Label2.Text.Append('Yet another label.');
  Label2.Text.Append('With different font.');
  Label2.Text.Append('Just because we can :)');
  Label2.Padding := 5;
  Window.Controls.InsertFront(Label2);

  LabelDeja := TCastleLabel.Create(Window);
  LabelDeja.Text.Append('DejaVuSans font');
  LabelDeja.Text.Append('with anti-aliasing.');
  LabelDeja.Padding := 5;
  Window.Controls.InsertFront(LabelDeja);

  LabelDejaBW := TCastleLabel.Create(Window);
  LabelDejaBW.Text.Append('DejaVuSans font');
  LabelDejaBW.Text.Append('without anti-aliasing.');
  LabelDejaBW.Padding := 5;
  Window.Controls.InsertFront(LabelDejaBW);

  LabelDejaLarge := TCastleLabel.Create(Window);
  LabelDejaLarge.Text.Append('DejaVuSans font');
  LabelDejaLarge.Text.Append('with anti-aliasing');
  LabelDejaLarge.Text.Append('and larger size.');
  LabelDejaLarge.Color := Blue;
  LabelDejaLarge.Padding := 5;
  Window.Controls.InsertFront(LabelDejaLarge);

  LabelStylish := TCastleLabel.Create(Window);
  LabelStylish.Text.Text :=
    'Stylish "old typewriter" font' +LineEnding+
    'with anti-aliasing.';
  LabelStylish.Padding := 5;
  Window.Controls.InsertFront(LabelStylish);

  LabelStylishBW := TCastleLabel.Create(Window);
  LabelStylishBW.Text.Text :=
    'Stylish "old typewriter" font' +LineEnding+
    'without anti-aliasing.';
  LabelStylishBW.Padding := 5;
  Window.Controls.InsertFront(LabelStylishBW);

  LabelStylishLarge := TCastleLabel.Create(Window);
  LabelStylishLarge.Text.Text :=
    'Stylish "old typewriter" font' +LineEnding+
    'with anti-aliasing' +LineEnding+
    'and larger size.';
  LabelStylishLarge.Padding := 5;
  Window.Controls.InsertFront(LabelStylishLarge);

  Window.OnOpen := @Open;
  Window.OnResize := @Resize;
  Window.OpenAndRun;
end.