program interpoldemo;
// Interpolation demo for fcl-image by Bernd Kreuss. Mantis #22245
// Loads original.png (not included) and scales it back to 64x64

{$mode objfpc}{$H+}

uses
  Classes,
  sysutils,
  FPimage,
  FPImgCanv,
  FPReadPNG,
  FPWritePNG;

var
  ImOriginal: TFPMemoryImage;
  ImScaled: TFPMemoryImage;
  CanvScaled: TFPImageCanvas;
  Reader: TFPReaderPNG;
  Writer: TFPWriterPNG;

begin
  ImOriginal := TFPMemoryImage.Create(0, 0);
  ImScaled := TFPMemoryImage.Create(64, 64);
  Reader := TFPReaderPNG.create;
  Writer := TFPWriterPNG.create;
  Writer.UseAlpha := True;
  ImOriginal.LoadFromFile('original.png', Reader);

  CanvScaled := TFPImageCanvas.create(ImScaled);
  CanvScaled.StretchDraw(0,0,63,63, ImOriginal);

  ImScaled.SaveToFile('scaled.png', Writer);
  Reader.Free;
  Writer.Free;
  ImOriginal.Free;
  ImScaled.Free;
end.

