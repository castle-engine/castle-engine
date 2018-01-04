program image_paint;
uses SysUtils,
  CastleWindow, CastleImages, CastleGLImages, castle_base, castle_window,
  CastleVectors, CastleColors, CastleLog;

const
  TestSize = 100;
  HalfTestSize = TestSize div 2;
  NTests = 5;

var
  Window: TCastleWindow;
  RGBAlphaImageGL, RGBImageGL, GrayscaleAlphaImageGL, GrayscaleImageGL, RGBFloatImageGL: TGLImage;
  RGBAlphaImage, RGBImage, GrayscaleAlphaImage, GrayscaleImage, RGBFloatImage: TCastleImage;

procedure DoTest(aImage: TCastleImage);
begin
  aImage.Clear(Vector4Byte(0, 0, 0, 0));
  aImage.FillEllipse(HalfTestSize + 0 * TestSize, HalfTestSize, TestSize / 4, TestSize / 4, Lime);
  aImage.Ellipse(HalfTestSize + 1 * TestSize, HalfTestSize, TestSize / 4, TestSize / 4, 2, Lime);
  aImage.FillRectangle(2 * TestSize + 10.5, 10.5, 3 * TestSize - 10.5, TestSize - 10.5, Lime);
  aImage.Rectangle(3 * TestSize + 10.5, 10.5, 4 * TestSize - 10.5, TestSize - 10.5, 2, Lime);
  aImage.Line(4 * TestSize + 10.5, 10.5, 5 * TestSize - 10.5, TestSize - 10.5, 7, Lime);
  aImage.Line(4 * TestSize + 30.5, TestSize - 20.5, 5 * TestSize - 30.5, 20.5, 16, Lime);
end;

procedure DoDraw;
begin
  RGBAlphaImage := TRGBAlphaImage.Create(TestSize*NTests, TestSize);
  DoTest(RGBAlphaImage);
  RGBAlphaImageGL := TGLImage.Create(RGBAlphaImage,true,true);
  {-------------}
  RGBImage := TRGBImage.Create(TestSize * NTests, TestSize);
  DoTest(RGBImage);
  RGBImageGL := TGLImage.Create(RGBImage,true,true);
  {-------------}
  GrayscaleAlphaImage := TGrayscaleAlphaImage.Create(TestSize*NTests, TestSize);
  DoTest(GrayscaleAlphaImage);
  GrayscaleAlphaImageGL := TGLImage.Create(GrayscaleAlphaImage,true,true);
  {-------------}
  GrayscaleImage := TGrayscaleImage.Create(TestSize*NTests, TestSize);
  DoTest(GrayscaleImage);
  GrayscaleImageGL := TGLImage.Create(GrayscaleImage,true,true);
  {-------------}
  RGBFloatImage := TRGBFloatImage.Create(TestSize*NTests, TestSize);
  DoTest(RGBFloatImage);
  RGBFloatImageGL := TGLImage.Create(RGBFloatImage,true,true);
end;

procedure DoRender(Container: TUIContainer);
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

