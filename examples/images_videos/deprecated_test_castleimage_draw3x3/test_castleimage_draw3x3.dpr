{
  Copyright 2018-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test TCastleImage.Draw3x3.
  See README.md for details. }

uses SysUtils,
  CastleVectors, CastleColors, CastleImages, CastleRectangles;

var
  SourceImage, DestImage: TCastleImage;
begin
  try
    SourceImage := LoadImage('castle-data:/box_with_borders.png');

    DestImage := TRGBAlphaImage.Create(1000, 1000);
    DestImage.Clear(Yellow);
    DestImage.DrawFrom3x3(Rectangle(10, 10, 128, 128),
      SourceImage, Vector4Integer(40, 40, 40, 40), dmBlend);
    DestImage.DrawFrom3x3(Rectangle(300, 350, 200, 200),
      SourceImage, Vector4Integer(40, 40, 40, 40), dmBlend);
    DestImage.DrawFrom3x3(Rectangle(500, 500, 500, 500),
      SourceImage, Vector4Integer(40, 40, 40, 40), dmBlend);

    SaveImage(DestImage, 'test_castleimage_draw3x3_output.png');
  finally
    FreeAndNil(SourceImage);
    FreeAndNil(DestImage);
  end;
end.
