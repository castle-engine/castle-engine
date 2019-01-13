{
  Copyright 2014-2018 Michalis Kamburelis.

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

{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}

program font_from_texture;

uses SysUtils, CastleWindow, CastleControls, CastleFonts, CastleImages,
  CastleColors, CastleVectors, CastleFilesUtils, CastleLog,
  CastleUIControls;

var
  Window: TCastleWindowBase;
  Label1, Label2,
    LabelDeja, LabelDejaBW, LabelDejaLarge, LabelDejaLargeOutline,
    LabelStylish, LabelStylishBW, LabelStylishLarge: TCastleLabel;

var
  MyTextureFont: TTextureFont;
  MySimpleTextureFont: TSimpleTextureFont;
  LabelsGroup: TCastleVerticalGroup;
begin
  InitializeLog;

  Window := TCastleWindowBase.Create(Application);
  Window.Container.BackgroundColor := Vector4(0.4, 0.4, 0.0, 1.0);

  LabelsGroup := TCastleVerticalGroup.Create(Window);
  LabelsGroup.Alignment := hpMiddle;
  LabelsGroup.Anchor(hpMiddle);
  LabelsGroup.Anchor(vpTop, -10);
  LabelsGroup.Spacing := 10;
  Window.Controls.InsertFront(LabelsGroup);

  MySimpleTextureFont := TSimpleTextureFont.Create(nil);
  MySimpleTextureFont.Load(LoadImage(ApplicationData('sonic_asalga_0.png')), 8, 12, 2, 2);
  UIFont := MySimpleTextureFont;
//  UIFont.Scale := 4; // test font Scale

  Label1 := TCastleLabel.Create(Window);
  Label1.Text.Append('A simple test of a font from an image.');
  Label1.Text.Append('Do cats eat bats?');
  Label1.Text.Append('and sometimes, Do bats eat cats?');
  Label1.Text.Append('1 + 2 + 3 = 6');
  Label1.Padding := 5;
  Label1.Frame := true;
  Label1.Color := White;
  // do not assign Label1.CustomFont, so it will use global UIFont
  LabelsGroup.InsertFront(Label1);

  Label2 := TCastleLabel.Create(Window);
  Label2.Text.Append('Yet another label.');
  Label2.Text.Append('With different font. Just because we can :)');
  Label2.Padding := 5;
  MySimpleTextureFont := TSimpleTextureFont.Create(Label2);
  MySimpleTextureFont.Load(LoadImage(ApplicationData('null_terminator_0.png')), 8, 12, 1, 1);
  Label2.CustomFont := MySimpleTextureFont;
  Label2.Color := Red;
  Label2.Frame := true;
  LabelsGroup.InsertFront(Label2);

  LabelDeja := TCastleLabel.Create(Window);
  LabelDeja.Text.Append('DejaVuSans font');
  LabelDeja.Text.Append('with anti-aliasing.');
  LabelDeja.Padding := 5;
  MyTextureFont := TTextureFont.Create(LabelDeja);
  MyTextureFont.Load(ApplicationData('DejaVuSans.ttf'), 15, true);
  LabelDeja.CustomFont := MyTextureFont;
  LabelDeja.Frame := true;
  LabelDeja.Color := White;
  LabelsGroup.InsertFront(LabelDeja);

  LabelDejaBW := TCastleLabel.Create(Window);
  LabelDejaBW.Text.Append('DejaVuSans font');
  LabelDejaBW.Text.Append('without anti-aliasing.');
  LabelDejaBW.Padding := 5;
  MyTextureFont := TTextureFont.Create(LabelDejaBW);
  MyTextureFont.Load(ApplicationData('DejaVuSans.ttf'), 15, false);
  LabelDejaBW.CustomFont := MyTextureFont;
  LabelDejaBW.Frame := true;
  LabelDejaBW.Color := White;
  LabelsGroup.InsertFront(LabelDejaBW);

  LabelDejaLarge := TCastleLabel.Create(Window);
  LabelDejaLarge.Text.Append('DejaVuSans font with anti-aliasing');
  LabelDejaLarge.Text.Append('and larger size.');
  LabelDejaLarge.Color := Vector4(0.5, 0.5, 1, 1); // light blue
  LabelDejaLarge.Padding := 5;
  MyTextureFont := TTextureFont.Create(LabelDejaLarge);
  MyTextureFont.Load(ApplicationData('DejaVuSans.ttf'), 30, true);
  LabelDejaLarge.CustomFont := MyTextureFont;
  LabelDejaLarge.Frame := true;
  LabelsGroup.InsertFront(LabelDejaLarge);

  LabelDejaLargeOutline := TCastleLabel.Create(Window);
  LabelDejaLargeOutline.Text.Append('DejaVuSans font with anti-aliasing');
  LabelDejaLargeOutline.Text.Append('and larger size and outline.');
  LabelDejaLargeOutline.Frame := false;
  LabelDejaLargeOutline.Color := White;
  LabelDejaLargeOutline.Padding := 5;
  MyTextureFont := TTextureFont.Create(LabelDejaLargeOutline);
  MyTextureFont.Load(ApplicationData('DejaVuSans.ttf'), 30, true);
  MyTextureFont.Outline := 2;
  MyTextureFont.OutlineHighQuality := true;
  MyTextureFont.OutlineColor := Red;
  LabelDejaLargeOutline.CustomFont := MyTextureFont;
  LabelDejaLargeOutline.Frame := true;
  LabelsGroup.InsertFront(LabelDejaLargeOutline);

  LabelStylish := TCastleLabel.Create(Window);
  LabelStylish.Caption :=
    'Stylish "old typewriter" font' +LineEnding+
    'with anti-aliasing.';
  LabelStylish.Padding := 5;
  MyTextureFont := TTextureFont.Create(LabelStylish);
  MyTextureFont.Load(ApplicationData('PARPG.ttf'), 15, true);
  LabelStylish.CustomFont := MyTextureFont;
  LabelStylish.Frame := true;
  LabelStylish.Color := White;
  LabelsGroup.InsertFront(LabelStylish);

  LabelStylishBW := TCastleLabel.Create(Window);
  LabelStylishBW.Caption :=
    'Stylish "old typewriter" font' +LineEnding+
    'without anti-aliasing.';
  LabelStylishBW.Padding := 5;
  MyTextureFont := TTextureFont.Create(LabelStylishBW);
  MyTextureFont.Load(ApplicationData('PARPG.ttf'), 15, false);
  LabelStylishBW.CustomFont := MyTextureFont;
  LabelStylishBW.Frame := true;
  LabelStylishBW.Color := White;
  LabelsGroup.InsertFront(LabelStylishBW);

  LabelStylishLarge := TCastleLabel.Create(Window);
  LabelStylishLarge.Caption :=
    'Stylish "old typewriter" font' +LineEnding+
    'with anti-aliasing' +LineEnding+
    'and larger size.';
  LabelStylishLarge.Padding := 5;
  MyTextureFont := TTextureFont.Create(LabelStylishLarge);
  MyTextureFont.Load(ApplicationData('PARPG.ttf'), 30, true);
//  MyTextureFont.Scale := 4; // test font Scale
  LabelStylishLarge.CustomFont := MyTextureFont;
  LabelStylishLarge.Frame := true;
  LabelStylishLarge.Color := White;
  LabelsGroup.InsertFront(LabelStylishLarge);

  Window.OpenAndRun;
end.
