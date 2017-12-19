program TestPaint;
uses SysUtils,
  CastleWindow, CastleImages, CastleGLImages, CastlePaint, castle_base,
  CastleVectors, CastleLog;

const
  TestSize = 100;
  NTests = 1;

var
  Window: TCastleWindow;
  RGBAlphaImageGL, RGBImageGL, GrayscaleAlphaImageGL, GrayscaleImageGL, RGBFloatImageGL: TGLImage;
  RGBAlphaImage, RGBImage, GrayscaleAlphaImage, GrayscaleImage, RGBFloatImage: TCastleImage;

procedure DoDraw;
begin
  RGBAlphaImage := TRGBAlphaImage.Create(TestSize,TestSize*NTests);
  RGBAlphaImage.Clear(Vector4Byte(0,0,0,0));

  RGBAlphaImageGL := TGLImage.Create(RGBAlphaImage,true,true);
  {-------------}
  RGBImage := TRGBImage.Create(TestSize,TestSize*NTests);
  RGBImage.Clear(Vector4Byte(0,0,0,0));

  RGBImageGL := TGLImage.Create(RGBImage,true,true);
  {-------------}
  GrayscaleAlphaImage := TGrayscaleAlphaImage.Create(TestSize,TestSize*NTests);
  GrayscaleAlphaImage.Clear(Vector4Byte(0,0,0,0));

  GrayscaleAlphaImageGL := TGLImage.Create(GrayscaleAlphaImage,true,true);
  {-------------}
  GrayscaleImage := TGrayscaleImage.Create(TestSize,TestSize*NTests);
  GrayscaleImage.Clear(Vector4Byte(0,0,0,0));

  GrayscaleImageGL := TGLImage.Create(GrayscaleImage,true,true);
  {-------------}
  RGBFloatImage := TRGBFloatImage.Create(TestSize,TestSize*NTests);
  RGBFloatImage.Clear(Vector4Byte(0,0,0,0));

  RGBFloatImageGL := TGLImage.Create(RGBFloatImage,true,true);
end;

procedure DoRender(Container: TUIContainer);
begin
  RGBAlphaImageGL.Draw(0*TestSize,0);
  RGBImageGL.Draw(1*TestSize,0);
  GrayscaleAlphaImageGL.Draw(2*TestSize,0);
  GrayscaleImageGL.Draw(3*TestSize,0);
  RGBFloatImageGL.Draw(4*TestSize,0);
end;

begin
  InitializeLog;

  Window := TCastleWindow.Create(Application);
  Window.Width := 5*TestSize;
  Window.Height := NTests*TestSize;
  Window.OnRender := @DoRender;

  DoDraw;

  Window.OpenAndRun;

  FreeAndNil(RGBAlphaImageGL);
  FreeAndNil(RGBImageGL);
  FreeAndNil(GrayscaleAlphaImageGL);
  FreeAndNil(GrayscaleImageGL);
  FreeAndNil(RGBFloatImageGL);
end.

