{
  Vampyre Imaging Library
  by Marek Mauder
  https://github.com/galfar/imaginglib
  https://imaginglib.sourceforge.io
  - - - - -
  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at https://mozilla.org/MPL/2.0.
} 

{
  This unit contains various image comparing functions and image difference
  computations.
}
unit ImagingCompare;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, Classes, ImagingTypes, Imaging, ImagingFormats, ImagingUtility;

{ Computes various error metrics for two images. Images must have
  the same size and format. Only formats with 1, 2, and 4 byte samples are
  supported (no indexed, compressed, etc.).}
procedure ComputeErrorMetrics(const Image1, Image2: TImageData;
  var PSNR, MSE, RMSE, PAE, MAE: Single);

implementation

procedure ComputeErrorMetrics(const Image1, Image2: TImageData;
  var PSNR, MSE, RMSE, PAE, MAE: Single);
var
  I: Integer;
  Info: TImageFormatInfo;
  Samples, Bps: Integer;
  PixelPtr1, PixelPtr2: PByte;
  Diff, MaxSample: Single;
begin
  GetImageFormatInfo(Image1.Format, Info);
  Bps := Info.ChannelCount div Info.BytesPerPixel;
  Assert((Image1.Width = Image2.Width) and (Image1.Height = Image2.Height) and
    (Image1.Format = Image2.Format));
  Assert(not Info.IsIndexed and not Info.IsSpecial and not Info.UsePixelFormat
    and (Bps in [1, 2, 4]));

  Diff := 0;
  PSNR := 0;
  MSE  := 0;
  RMSE := 0;
  PAE  := 0;
  MAE  := 0;
  PixelPtr1 := Image1.Bits;
  PixelPtr2 := Image2.Bits;
  Samples := Image1.Width * Image1.Height * Info.ChannelCount;

  for I := 0 to Samples - 1 do
  begin
    // Compute difference between pixels
    case Bps of
      1: Diff := Abs(PixelPtr2^ - PixelPtr1^);
      2:
        begin
          if Info.IsFloatingPoint then
            Diff := Abs(HalfToFloat(PWord(PixelPtr2)^) - HalfToFloat(PWord(PixelPtr1)^))
          else
            Diff := Abs(PWord(PixelPtr2)^ - PWord(PixelPtr1)^);
        end;
      4:
        begin
          if Info.IsFloatingPoint then
            Diff := Abs(PSingle(PixelPtr2)^ - PSingle(PixelPtr1)^)
          else
            Diff := Abs(PUInt32(PixelPtr2)^ - PUInt32(PixelPtr1)^);
        end;
    end;

    // Update metrics
    MAE := MAE + Diff;
    PAE := MaxFloat(PAE, Diff);
    MSE := MSE + Diff * Diff;

    Inc(PixelPtr1, Bps);
    Inc(PixelPtr2, Bps);
  end;

  if Info.IsFloatingPoint then
    MaxSample := 1.0
  else
    MaxSample := Pow2Int(Bps * 8) - 1;

  // Final metrics calculations
  MAE := MAE / Samples;
  MSE := MSE / Samples;
  RMSE := Sqrt(MSE);
  if RMSE < 0.0001 then
    PSNR := 1e06
  else
    PSNR := 20 * Log10(MaxSample / RMSE);
end;

{
  File Notes:

  -- TODOS ----------------------------------------------------
    - none

  -- 0.26.5 Changes/Bug Fixes -----------------------------------
    - Added ComputeErrorMetrics.
    - Unit created.
}

end.
