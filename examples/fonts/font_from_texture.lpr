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

uses SysUtils,
  CastleWindow, CastleControls, CastleFonts, CastleImages,
  CastleColors, CastleVectors, CastleFilesUtils, CastleLog,
  CastleUIControls, CastleUtils;

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

  LabelsGroup := TCastleVerticalGroup.Create(Application);
  LabelsGroup.Alignment := hpMiddle;
  LabelsGroup.Anchor(hpMiddle);
  LabelsGroup.Anchor(vpTop, -10);
  LabelsGroup.Spacing := 10;
  Window.Controls.InsertFront(LabelsGroup);

  MySimpleTextureFont := TSimpleTextureFont.Create(Application);
  MySimpleTextureFont.Load(LoadImage('castle-data:/sonic_asalga_0.png'), 8, 12, 2, 2);
  UIFont := MySimpleTextureFont;
//  UIFont.Scale := 4; // test font Scale

  Label1 := TCastleLabel.Create(Application);
  // Label1.CustomFont remains nil, so Label1.Font will be global UIFont
  Label1.Caption :=
    'A simple test of a font from an image.' + NL +
    'Do cats eat bats?' + NL +
    'and sometimes, Do bats eat cats?' + NL +
    Format('Font properties: Size %f, RowHeight %f, RowHeightBase %f, Descend %f.', [
      Label1.Font.Size,
      Label1.Font.RowHeight,
      Label1.Font.RowHeightBase,
      Label1.Font.Descend
    ]);
  Label1.Padding := 5;
  Label1.Frame := true;
  Label1.Color := White;
  LabelsGroup.InsertFront(Label1);

  MySimpleTextureFont := TSimpleTextureFont.Create(Application);
  MySimpleTextureFont.Load(LoadImage('castle-data:/null_terminator_0.png'), 8, 12, 1, 1);

  Label2 := TCastleLabel.Create(Application);
  Label2.CustomFont := MySimpleTextureFont;
  Label2.Caption :=
    'Yet another label.' + NL +
    'With different font. Just to show that we can :)' + NL +
    Format('Font properties: Size %f, RowHeight %f, RowHeightBase %f, Descend %f.', [
      Label2.Font.Size,
      Label2.Font.RowHeight,
      Label2.Font.RowHeightBase,
      Label2.Font.Descend
    ]);
  Label2.Padding := 5;
  Label2.Color := Red;
  Label2.Frame := true;
  LabelsGroup.InsertFront(Label2);

  MyTextureFont := TTextureFont.Create(Application);
  MyTextureFont.Load('castle-data:/DejaVuSans.ttf', 15, true);

  LabelDeja := TCastleLabel.Create(Application);
  LabelDeja.CustomFont := MyTextureFont;
  LabelDeja.Caption :=
    'DejaVuSans font' + NL +
    'with anti-aliasing.' + NL +
    Format('Font properties: Size %f, RowHeight %f, RowHeightBase %f, Descend %f.', [
      LabelDeja.Font.Size,
      LabelDeja.Font.RowHeight,
      LabelDeja.Font.RowHeightBase,
      LabelDeja.Font.Descend
    ]);
  LabelDeja.Padding := 5;
  LabelDeja.Frame := true;
  LabelDeja.Color := White;
  LabelsGroup.InsertFront(LabelDeja);

  MyTextureFont := TTextureFont.Create(Application);
  MyTextureFont.Load('castle-data:/DejaVuSans.ttf', 15, false);

  LabelDejaBW := TCastleLabel.Create(Application);
  LabelDejaBW.CustomFont := MyTextureFont;
  LabelDejaBW.Caption :=
    'DejaVuSans font' + NL +
    'without anti-aliasing.' + NL +
    Format('Font properties: Size %f, RowHeight %f, RowHeightBase %f, Descend %f.', [
      LabelDejaBW.Font.Size,
      LabelDejaBW.Font.RowHeight,
      LabelDejaBW.Font.RowHeightBase,
      LabelDejaBW.Font.Descend
    ]);
  LabelDejaBW.Padding := 5;
  LabelDejaBW.Frame := true;
  LabelDejaBW.Color := White;
  LabelsGroup.InsertFront(LabelDejaBW);

  MyTextureFont := TTextureFont.Create(Application);
  MyTextureFont.Load('castle-data:/DejaVuSans.ttf', 30, true);

  LabelDejaLarge := TCastleLabel.Create(Application);
  LabelDejaLarge.CustomFont := MyTextureFont;
  LabelDejaLarge.Caption :=
    'DejaVuSans font with anti-aliasing' + NL +
    'and larger size.' + NL +
    Format('Font properties: Size %f, RowHeight %f, RowHeightBase %f, Descend %f.', [
      LabelDejaLarge.Font.Size,
      LabelDejaLarge.Font.RowHeight,
      LabelDejaLarge.Font.RowHeightBase,
      LabelDejaLarge.Font.Descend
    ]);
  LabelDejaLarge.Color := Vector4(0.5, 0.5, 1, 1); // light blue
  LabelDejaLarge.Padding := 5;
  LabelDejaLarge.Frame := true;
  LabelsGroup.InsertFront(LabelDejaLarge);

  MyTextureFont := TTextureFont.Create(Application);
  MyTextureFont.Load('castle-data:/DejaVuSans.ttf', 30, true);
  MyTextureFont.Outline := 2;
  MyTextureFont.OutlineHighQuality := true;
  MyTextureFont.OutlineColor := Red;

  LabelDejaLargeOutline := TCastleLabel.Create(Application);
  LabelDejaLargeOutline.CustomFont := MyTextureFont;
  LabelDejaLargeOutline.Caption :=
    'DejaVuSans font with anti-aliasing' + NL +
    'and larger size and outline.' + NL +
    Format('Font properties: Size %f, RowHeight %f, RowHeightBase %f, Descend %f.', [
      LabelDejaLargeOutline.Font.Size,
      LabelDejaLargeOutline.Font.RowHeight,
      LabelDejaLargeOutline.Font.RowHeightBase,
      LabelDejaLargeOutline.Font.Descend
    ]);
  LabelDejaLargeOutline.Frame := false;
  LabelDejaLargeOutline.Color := White;
  LabelDejaLargeOutline.Padding := 5;
  LabelDejaLargeOutline.Frame := true;
  LabelsGroup.InsertFront(LabelDejaLargeOutline);

  MyTextureFont := TTextureFont.Create(Application);
  MyTextureFont.Load('castle-data:/PARPG.ttf', 15, true);

  LabelStylish := TCastleLabel.Create(Application);
  LabelStylish.CustomFont := MyTextureFont;
  LabelStylish.Caption :=
    'Stylish "old typewriter" font' + NL +
    'with anti-aliasing.' + NL +
    Format('Font properties: Size %f, RowHeight %f, RowHeightBase %f, Descend %f.', [
      LabelStylish.Font.Size,
      LabelStylish.Font.RowHeight,
      LabelStylish.Font.RowHeightBase,
      LabelStylish.Font.Descend
    ]);
  LabelStylish.Padding := 5;
  LabelStylish.Frame := true;
  LabelStylish.Color := White;
  LabelsGroup.InsertFront(LabelStylish);

  MyTextureFont := TTextureFont.Create(Application);
  MyTextureFont.Load('castle-data:/PARPG.ttf', 15, false);

  LabelStylishBW := TCastleLabel.Create(Application);
  LabelStylishBW.CustomFont := MyTextureFont;
  LabelStylishBW.Caption :=
    'Stylish "old typewriter" font' + NL +
    'without anti-aliasing.' + NL +
    Format('Font properties: Size %f, RowHeight %f, RowHeightBase %f, Descend %f.', [
      LabelStylishBW.Font.Size,
      LabelStylishBW.Font.RowHeight,
      LabelStylishBW.Font.RowHeightBase,
      LabelStylishBW.Font.Descend
    ]);
  LabelStylishBW.Padding := 5;
  LabelStylishBW.Frame := true;
  LabelStylishBW.Color := White;
  LabelsGroup.InsertFront(LabelStylishBW);

  MyTextureFont := TTextureFont.Create(Application);
  MyTextureFont.Load('castle-data:/PARPG.ttf', 30, true);
//  MyTextureFont.Scale := 4; // test font Scale

  LabelStylishLarge := TCastleLabel.Create(Application);
  LabelStylishLarge.CustomFont := MyTextureFont;
  LabelStylishLarge.Caption :=
    'Stylish "old typewriter" font' + NL +
    'with anti-aliasing' + NL +
    'and larger size.' + NL +
    Format('Font properties: Size %f, RowHeight %f, RowHeightBase %f, Descend %f.', [
      LabelStylishLarge.Font.Size,
      LabelStylishLarge.Font.RowHeight,
      LabelStylishLarge.Font.RowHeightBase,
      LabelStylishLarge.Font.Descend
    ]);
  LabelStylishLarge.Padding := 5;
  LabelStylishLarge.Frame := true;
  LabelStylishLarge.Color := White;
  LabelsGroup.InsertFront(LabelStylishLarge);

  Window.OpenAndRun;
end.
