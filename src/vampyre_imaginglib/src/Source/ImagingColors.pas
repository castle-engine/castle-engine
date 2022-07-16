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

{ This unit contains functions for manipulating and converting color values.}
unit ImagingColors;

interface

{$I ImagingOptions.inc}

uses
  SysUtils, ImagingTypes, ImagingUtility;

{ Converts RGB color to YUV.}
procedure RGBToYUV(R, G, B: Byte; var Y, U, V: Byte);
{ Converts YIV to RGB color.}
procedure YUVToRGB(Y, U, V: Byte; var R, G, B: Byte);

{ Converts RGB color to YCbCr as used in JPEG.}
procedure RGBToYCbCr(R, G, B: Byte; var Y, Cb, Cr: Byte);
{ Converts YCbCr as used in JPEG to RGB color.}
procedure YCbCrToRGB(Y, Cb, Cr: Byte; var R, G, B: Byte);
{ Converts RGB color to YCbCr as used in JPEG.}
procedure RGBToYCbCr16(R, G, B: Word; var Y, Cb, Cr: Word);
{ Converts YCbCr as used in JPEG to RGB color.}
procedure YCbCrToRGB16(Y, Cb, Cr: Word; var R, G, B: Word);

{ Converts RGB color to CMY.}
procedure RGBToCMY(R, G, B: Byte; var C, M, Y: Byte);
{ Converts CMY to RGB color.}
procedure CMYToRGB(C, M, Y: Byte; var R, G, B: Byte);
{ Converts RGB color to CMY.}
procedure RGBToCMY16(R, G, B: Word; var C, M, Y: Word);
{ Converts CMY to RGB color.}
procedure CMYToRGB16(C, M, Y: Word; var R, G, B: Word);

{ Converts RGB color to CMYK.}
procedure RGBToCMYK(R, G, B: Byte; var C, M, Y, K: Byte);
{ Converts CMYK to RGB color.}
procedure CMYKToRGB(C, M, Y, K: Byte; var R, G, B: Byte);
{ Converts RGB color to CMYK.}
procedure RGBToCMYK16(R, G, B: Word; var C, M, Y, K: Word);
{ Converts CMYK to RGB color.}
procedure CMYKToRGB16(C, M, Y, K: Word; var R, G, B: Word);

{ Converts RGB color to YCoCg.}
procedure RGBToYCoCg(R, G, B: Byte; var Y, Co, Cg: Byte);
{ Converts YCoCg to RGB color.}
procedure YCoCgToRGB(Y, Co, Cg: Byte; var R, G, B: Byte);

//procedure RGBToHSL(R, G, B: Byte; var H, S, L: Byte);
//procedure HSLToRGB(H, S, L: Byte; var R, G, B: Byte);

implementation

procedure RGBToYUV(R, G, B: Byte; var Y, U, V: Byte);
begin
  Y := ClampToByte(Round( 0.257 * R + 0.504 * G + 0.098 * B) + 16);
  V := ClampToByte(Round( 0.439 * R - 0.368 * G - 0.071 * B) + 128);
  U := ClampToByte(Round(-0.148 * R - 0.291 * G + 0.439 * B) + 128);
end;

procedure YUVToRGB(Y, U, V: Byte; var R, G, B: Byte);
var
  CY, CU, CV: LongInt;
begin
  CY := Y - 16;
  CU := U - 128;
  CV := V - 128;
  R := ClampToByte(Round(1.164 * CY - 0.002 * CU + 1.596 * CV));
  G := ClampToByte(Round(1.164 * CY - 0.391 * CU - 0.813 * CV));
  B := ClampToByte(Round(1.164 * CY + 2.018 * CU - 0.001 * CV));
end;

procedure RGBToYCbCr(R, G, B: Byte; var Y, Cb, Cr: Byte);
begin
  Y  := ClampToByte(Round( 0.29900 * R + 0.58700 * G + 0.11400 * B));
  Cb := ClampToByte(Round(-0.16874 * R - 0.33126 * G + 0.50000 * B  + 128));
  Cr := ClampToByte(Round( 0.50000 * R - 0.41869 * G - 0.08131 * B  + 128));
end;

procedure YCbCrToRGB(Y, Cb, Cr: Byte; var R, G, B: Byte);
begin
  R := ClampToByte(Round(Y                        + 1.40200 * (Cr - 128)));
  G := ClampToByte(Round(Y - 0.34414 * (Cb - 128) - 0.71414 * (Cr - 128)));
  B := ClampToByte(Round(Y + 1.77200 * (Cb - 128)));
end;

procedure RGBToYCbCr16(R, G, B: Word; var Y, Cb, Cr: Word);
begin
  Y  := ClampToWord(Round( 0.29900 * R + 0.58700 * G + 0.11400 * B));
  Cb := ClampToWord(Round(-0.16874 * R - 0.33126 * G + 0.50000 * B  + 32768));
  Cr := ClampToWord(Round( 0.50000 * R - 0.41869 * G - 0.08131 * B  + 32768));
end;

procedure YCbCrToRGB16(Y, Cb, Cr: Word; var R, G, B: Word);
begin
  R := ClampToWord(Round(Y                          + 1.40200 * (Cr - 32768)));
  G := ClampToWord(Round(Y - 0.34414 * (Cb - 32768) - 0.71414 * (Cr - 32768)));
  B := ClampToWord(Round(Y + 1.77200 * (Cb - 32768)));
end;

procedure RGBToCMY(R, G, B: Byte; var C, M, Y: Byte);
begin
  C := 255 - R;
  M := 255 - G;
  Y := 255 - B;
end;

procedure CMYToRGB(C, M, Y: Byte; var R, G, B: Byte);
begin
  R := 255 - C;
  G := 255 - M;
  B := 255 - Y;
end;

procedure RGBToCMY16(R, G, B: Word; var C, M, Y: Word);
begin
  C := 65535 - R;
  M := 65535 - G;
  Y := 65535 - B;
end;

procedure CMYToRGB16(C, M, Y: Word; var R, G, B: Word);
begin
  R := 65535 - C;
  G := 65535 - M;
  B := 65535 - Y;
end;

procedure RGBToCMYK(R, G, B: Byte; var C, M, Y, K: Byte);
begin
  RGBToCMY(R, G, B, C, M, Y);
  K := Min(C, Min(M, Y));
  if K = 255 then
  begin
    C := 0;
    M := 0;
    Y := 0;
  end
  else
  begin
    C := ClampToByte(Round((C - K) / (255 - K) * 255));
    M := ClampToByte(Round((M - K) / (255 - K) * 255));
    Y := ClampToByte(Round((Y - K) / (255 - K) * 255));
  end;
end;

procedure CMYKToRGB(C, M, Y, K: Byte; var R, G, B: Byte);
begin
   R := (255 - (C - MulDiv(C, K, 255) + K));
   G := (255 - (M - MulDiv(M, K, 255) + K));
   B := (255 - (Y - MulDiv(Y, K, 255) + K));
end;

procedure RGBToCMYK16(R, G, B: Word; var C, M, Y, K: Word);
begin
  RGBToCMY16(R, G, B, C, M, Y);
  K := Min(C, Min(M, Y));
  if K = 65535 then
  begin
    C := 0;
    M := 0;
    Y := 0;
  end
  else
  begin
    C := ClampToWord(Round((C - K) / (65535 - K) * 65535));
    M := ClampToWord(Round((M - K) / (65535 - K) * 65535));
    Y := ClampToWord(Round((Y - K) / (65535 - K) * 65535));
  end;
end;

procedure CMYKToRGB16(C, M, Y, K: Word; var R, G, B: Word);
begin
  R := 65535 - (C - MulDiv(C, K, 65535) + K);
  G := 65535 - (M - MulDiv(M, K, 65535) + K);
  B := 65535 - (Y - MulDiv(Y, K, 65535) + K);
end;

procedure RGBToYCoCg(R, G, B: Byte; var Y, Co, Cg: Byte);
begin
  // C and Delphi's SHR behaviour differs for negative numbers, use div instead.
  Y  := ClampToByte(( R +     G shl 1 + B       + 2) div 4);
  Co := ClampToByte(( R shl 1         - B shl 1 + 2) div 4 + 128);
  Cg := ClampToByte((-R +     G shl 1 - B       + 2) div 4 + 128);
end;

procedure YCoCgToRGB(Y, Co, Cg: Byte; var R, G, B: Byte);
var
  CoInt, CgInt: Integer;
begin
  CoInt := Co - 128;
  CgInt := Cg - 128;
  R := ClampToByte(Y + CoInt - CgInt);
  G := ClampToByte(Y + CgInt);
  B := ClampToByte(Y - CoInt - CgInt);
end;

{
  File Notes:

  -- TODOS ----------------------------------------------------
    - nothing now

  -- 0.26.3 Changes/Bug Fixes ---------------------------------
    - Added RGB<>YCoCg conversion functions.
    - Fixed RGB>>CMYK conversions.

  -- 0.23 Changes/Bug Fixes -----------------------------------
    - Added RGB<>CMY(K) conversion functions for 16 bit channels
      (needed by PSD loading code).

  -- 0.21 Changes/Bug Fixes -----------------------------------
    - Added some color space conversion functions and LUTs
      (RGB/YUV/YCrCb/CMY/CMYK).

  -- 0.17 Changes/Bug Fixes -----------------------------------
    - unit created (empty!)
}

end.
