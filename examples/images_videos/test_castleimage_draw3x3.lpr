{
  Copyright 2018-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test TCastleImage.Draw3x3.

  Note that you should prefer to use TDrawableImage.Draw3x3 in most games
  (it's done on GPU and is much faster), see e.g. draw_images_on_gpu.lpr
  for an example how to draw on TDrawableImage. }

uses SysUtils,
  CastleVectors, CastleColors, CastleImages, CastleRectangles;

var
  SourceImage, DestImage: TCastleImage;
begin
  try
    SourceImage := LoadImage('castle-data:/box_with_borders.png');

    DestImage := TRGBAlphaImage.Create(400, 400);
    DestImage.Clear(Yellow);
    DestImage.DrawFrom3x3(Rectangle(150, 100, 200, 200),
      SourceImage, Vector4Integer(40, 40, 40, 40), dmBlend);

    SaveImage(DestImage, 'test_castleimage_draw3x3_output.png');
  finally
    FreeAndNil(SourceImage);
    FreeAndNil(DestImage);
  end;
end.
