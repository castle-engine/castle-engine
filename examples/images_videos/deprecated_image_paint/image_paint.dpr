{
  Copyright 2017-2021 Yevhen Loza.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Drawing ellipses, rectangles, lines on TCastleImage (using CPU drawing). }
program image_paint;

uses SysUtils,
  CastleWindow, CastleImages, CastleGLImages, CastleVectors, CastleColors,
  CastleLog;

const
  TestSize = 100;
  HalfTestSize = TestSize div 2;
  NTests = 6;

var
  Window: TCastleWindow;
  RGBAlphaImageGL, RGBImageGL, GrayscaleAlphaImageGL, GrayscaleImageGL, RGBFloatImageGL: TDrawableImage;
  RGBAlphaImage, RGBImage, GrayscaleAlphaImage, GrayscaleImage, RGBFloatImage: TCastleImage;

procedure DoTest(aImage: TCastleImage);
begin
  aImage.Clear(Vector4Byte(0, 0, 0, 0));

  aImage.FillEllipse(HalfTestSize + 0 * TestSize, HalfTestSize, TestSize / 4, TestSize / 4, Lime);

  aImage.Ellipse(HalfTestSize + 1 * TestSize, HalfTestSize, TestSize / 4, TestSize / 4, 2, Lime);

  aImage.FillRectangle(2 * TestSize + 10.5, 10.5, 3 * TestSize - 10.5, TestSize - 10.5, Lime);

  aImage.Rectangle(3 * TestSize + 10.5, 10.5, 4 * TestSize - 10.5, TestSize - 10.5, 2, Lime);

  aImage.Line(4 * TestSize + 10.5, 10.5, 5 * TestSize - 10.5, TestSize - 10.5, 7, Lime); //straight line
  aImage.Line(5 * TestSize - 10.5, TestSize / 2, 4 * TestSize + 10.5, 10.5, 7, Lime); //invert line
  aImage.Line(4 * TestSize + 30.5, TestSize - 20.5, 5 * TestSize - 30.5, 20.5, 16, Lime);
  aImage.Line(4 * TestSize + 10.5, 19.5, 5 * TestSize - 29.5, TestSize - 10.5, 0.1, Lime);

  aImage.Line(5 * TestSize + TestSize / 2, 1.5, 5 * TestSize + 1.5, TestSize - 10.5, 1, Red);
  aImage.Line(5 * TestSize + TestSize / 2, 1.5, 6 * TestSize - 1.5, TestSize - 10.5, 1, Green);
  aImage.Line(5 * TestSize + 1.5, TestSize - 10.5, 6 * TestSize - 1.5, TestSize - 10.5, 1, Blue);
  aImage.FloodFill(5 * TestSize + HalfTestSize, HalfTestSize, Yellow, 0.4);
end;

procedure DoDraw;
begin
  RGBAlphaImage := TRGBAlphaImage.Create(TestSize*NTests, TestSize);
  DoTest(RGBAlphaImage);
  RGBAlphaImageGL := TDrawableImage.Create(RGBAlphaImage,true,true);
  {-------------}
  RGBImage := TRGBImage.Create(TestSize * NTests, TestSize);
  DoTest(RGBImage);
  RGBImageGL := TDrawableImage.Create(RGBImage,true,true);
  {-------------}
  GrayscaleAlphaImage := TGrayscaleAlphaImage.Create(TestSize*NTests, TestSize);
  DoTest(GrayscaleAlphaImage);
  GrayscaleAlphaImageGL := TDrawableImage.Create(GrayscaleAlphaImage,true,true);
  {-------------}
  GrayscaleImage := TGrayscaleImage.Create(TestSize*NTests, TestSize);
  DoTest(GrayscaleImage);
  GrayscaleImageGL := TDrawableImage.Create(GrayscaleImage,true,true);
  {-------------}
  RGBFloatImage := TRGBFloatImage.Create(TestSize*NTests, TestSize);
  DoTest(RGBFloatImage);
  RGBFloatImageGL := TDrawableImage.Create(RGBFloatImage,true,true);
end;

procedure DoRender(Container: TCastleContainer);
begin
  RGBAlphaImageGL.Draw(0, 0 * TestSize);
  RGBImageGL.Draw(0, 1 * TestSize);
  GrayscaleAlphaImageGL.Draw(0, 2 * TestSize);
  GrayscaleImageGL.Draw(0, 3 * TestSize);
  RGBFloatImageGL.Draw(0, 4 * TestSize);
end;

begin
  InitializeLog;

  Window := TCastleWindow.Create(Application);
  Window.Width := NTests * TestSize;
  Window.Height := 5 * TestSize;
  Window.OnRender := @DoRender;

  DoDraw;

  Window.OpenAndRun;

  FreeAndNil(RGBAlphaImageGL);
  FreeAndNil(RGBImageGL);
  FreeAndNil(GrayscaleAlphaImageGL);
  FreeAndNil(GrayscaleImageGL);
  FreeAndNil(RGBFloatImageGL);
end.
