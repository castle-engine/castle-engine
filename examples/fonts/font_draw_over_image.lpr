{
  Copyright 2016-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Demo of using TCastleFont to draw text over an image (TCastleImage).
  Running this program will generate file font_draw_over_image_output.png
  in the current directory where the text is drawn.

  Of course, you could use the resulting TCastleImage instance in more ways.
  E.g. load it to TGLImage and display it. This would be an alternative way
  to display text: instead of drawing it on screen, you can draw it on an image,
  and then display the image.

  See also font_from_texture.lpr demo, that draws text directly on screen,
  and instead of the "PrintStrings" calls it uses TCastleLabel components. }

{$apptype CONSOLE}

program font_draw_over_image;

uses SysUtils, Classes,
  CastleControls, CastleFonts, CastleImages, CastleColors, CastleVectors,
  CastleFilesUtils, CastleLog, CastleOpenDocument, CastleRectangles;

const
  DefaultLineSpacing = 2;
  DefaultAlignment = hpMiddle;
var
  Image: TCastleImage;
  MyTextureFont: TTextureFont;
  MySimpleTextureFont: TSimpleTextureFont;
begin
  { image where the rendering will be done }

  // Test: destination as TRGBAlphaImage
  Image := TRGBAlphaImage.Create(1000, 1000);
  // Alternative test: destination as TRGBImage
  // Image := TRGBImage.Create(1000, 1000);
  // Alternative test: destination as TGrayscaleAlphaImage
  // Image := TGrayscaleAlphaImage.Create(1000, 1000);
  // Alternative test: destination as TGrayscaleImage
  // Image := TGrayscaleImage.Create(1000, 1000);
  try
    Image.Clear(Vector4Byte(100, 100, 0, 255));

    MySimpleTextureFont := TSimpleTextureFont.Create(nil);
    try
      MySimpleTextureFont.Load(LoadImage(ApplicationData('sonic_asalga_0.png')), 8, 12, 2, 2);
      MySimpleTextureFont.TargetImage := Image;
      MySimpleTextureFont.PrintStrings(Image.Width div 2, 100, White,
        ['A simple test of a font from an image.',
         'Do cats eat bats?',
         'and sometimes, Do bats eat cats?',
         '1 + 2 + 3 = 6'],
        false, DefaultLineSpacing, DefaultAlignment);
    finally FreeAndNil(MySimpleTextureFont) end;

    MySimpleTextureFont := TSimpleTextureFont.Create(nil);
    try
      MySimpleTextureFont.Load(LoadImage(ApplicationData('null_terminator_0.png')), 8, 12, 1, 1);
      MySimpleTextureFont.TargetImage := Image;
      MySimpleTextureFont.PrintStrings(Image.Width div 2, 200, Red,
        ['Yet another label.',
         'With different font. Just because we can :)'],
        false, DefaultLineSpacing, DefaultAlignment);
    finally FreeAndNil(MySimpleTextureFont) end;

    MyTextureFont := TTextureFont.Create(TComponent(nil));
    try
      MyTextureFont.Load(ApplicationData('DejaVuSans.ttf'), 15, true);
      MyTextureFont.TargetImage := Image;
      MyTextureFont.PrintStrings(Image.Width div 2, 300, White,
        ['DejaVuSans font',
         'with anti-aliasing.'],
        false, DefaultLineSpacing, DefaultAlignment);
    finally FreeAndNil(MyTextureFont) end;

    MyTextureFont := TTextureFont.Create(TComponent(nil));
    try
      MyTextureFont.Load(ApplicationData('DejaVuSans.ttf'), 15, false);
      MyTextureFont.TargetImage := Image;
      MyTextureFont.PrintStrings(Image.Width div 2, 400, White,
        ['DejaVuSans font',
         'without anti-aliasing.'],
        false, DefaultLineSpacing, DefaultAlignment);
    finally FreeAndNil(MyTextureFont) end;

    MyTextureFont := TTextureFont.Create(TComponent(nil));
    try
      MyTextureFont.Load(ApplicationData('DejaVuSans.ttf'), 30, true);
      MyTextureFont.TargetImage := Image;
      MyTextureFont.PrintStrings(Image.Width div 2, 500, Vector4Single(0.5, 0.5, 1, 1),
        ['DejaVuSans font with anti-aliasing',
         'and larger size and <font color="#ff0000">HTML colors</font>.'],
        true, DefaultLineSpacing, DefaultAlignment);
    finally FreeAndNil(MyTextureFont) end;

    MyTextureFont := TTextureFont.Create(TComponent(nil));
    try
      MyTextureFont.Load(ApplicationData('DejaVuSans.ttf'), 30, true);
      MyTextureFont.TargetImage := Image;
      MyTextureFont.Outline := 2;
      MyTextureFont.OutlineHighQuality := true;
      MyTextureFont.OutlineColor := Red;
      MyTextureFont.PrintStrings(Image.Width div 2, 600, White,
        ['DejaVuSans font with anti-aliasing',
         'and larger size and outline.'],
        false, DefaultLineSpacing, DefaultAlignment);
    finally FreeAndNil(MyTextureFont) end;

    MyTextureFont := TTextureFont.Create(TComponent(nil));
    try
      MyTextureFont.Load(ApplicationData('PARPG.ttf'), 15, true);
      MyTextureFont.TargetImage := Image;
      MyTextureFont.PrintStrings(Image.Width div 2, 700, White,
        ['Stylish "old typewriter" font',
         'with anti-aliasing.'],
        false, DefaultLineSpacing, DefaultAlignment);
    finally FreeAndNil(MyTextureFont) end;

    MyTextureFont := TTextureFont.Create(TComponent(nil));
    try
      MyTextureFont.Load(ApplicationData('PARPG.ttf'), 15, false);
      MyTextureFont.TargetImage := Image;
      MyTextureFont.PrintStrings(Image.Width div 2, 800, White,
        ['Stylish "old typewriter" font',
         'without anti-aliasing.'],
        false, DefaultLineSpacing, DefaultAlignment);
    finally FreeAndNil(MyTextureFont) end;

    MyTextureFont := TTextureFont.Create(TComponent(nil));
    try
      MyTextureFont.Load(ApplicationData('PARPG.ttf'), 30, true);
      MyTextureFont.TargetImage := Image;
      MyTextureFont.PrintStrings(Image.Width div 2, 900, White,
        ['Stylish "old typewriter" font',
         'with anti-aliasing',
         'and larger size.'],
        false, DefaultLineSpacing, DefaultAlignment);
    finally FreeAndNil(MyTextureFont) end;

    SaveImage(Image, 'font_draw_over_image_output.png');
  finally FreeAndNil(Image) end;
end.
