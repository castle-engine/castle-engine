{
  Copyright 2001-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Image loaders using FPImage.

  FPImage docs:
  http://lazarus-ccr.sourceforge.net/docs/fcl/fpimage/index.html
  http://wiki.freepascal.org/fcl-image

  Also http://free-pascal-general.1045716.n5.nabble.com/Why-is-FPImage-dog-slow-in-reading-jpeg-s-td4439450.html
  is informative, explains UsePalette stuff.
  However, for some formats palette is needed --- e.g. reading XPM
  reader segfaults when UsePalette = false, tested on
  /usr/share/pixmaps/EasyTAG_icon.xpm
  /usr/share/pixmaps/flamerobin.xpm
}

function LoadFpImage(Stream: TStream;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions;
  Reader: TFPCustomImageReader;
  const AllowPalette: boolean = false): TImage;
var
  FPImage: TFPMemoryImage;

  { FPImage doesn't provide us any information does the image contain useful
    alpha channel. So just iterate over pixels, and check it ourselves. }
  function DetectAlpha: boolean;
  var
    X, Y: Integer;
  begin
    for Y := FPImage.Height - 1 downto 0 do
      for X := 0 to FPImage.Width - 1 do
        if (FPImage.Colors[X, Y].Alpha shr 8) <> $FF then
          Exit(true);
    Result := false;
  end;

  procedure ReadRGB;
  var
    X, Y: Integer;
    ResultPixels: PVector3Byte;
    Color: TFPColor;
  begin
    Result := TRGBImage.Create(FPImage.Width, FPImage.Height);
    try
      ResultPixels := TRGBImage(Result).RGBPixels;
      for Y := FPImage.Height - 1 downto 0 do
        for X := 0 to FPImage.Width - 1 do
        begin
          Color := FPImage.Colors[X, Y];
          ResultPixels^[0] := Color.Red shr 8;
          ResultPixels^[1] := Color.Green shr 8;
          ResultPixels^[2] := Color.Blue shr 8;
          Inc(ResultPixels);
        end;
    except FreeAndNil(Result) end;
  end;

  procedure ReadRGBA;
  var
    X, Y: Integer;
    ResultPixels: PVector4Byte;
    Color: TFPColor;
  begin
    Result := TRGBAlphaImage.Create(FPImage.Width, FPImage.Height);
    try
      ResultPixels := TRGBAlphaImage(Result).AlphaPixels;
      for Y := FPImage.Height - 1 downto 0 do
        for X := 0 to FPImage.Width - 1 do
        begin
          Color := FPImage.Colors[X, Y];
          ResultPixels^[0] := Color.Red shr 8;
          ResultPixels^[1] := Color.Green shr 8;
          ResultPixels^[2] := Color.Blue shr 8;
          ResultPixels^[3] := Color.Alpha shr 8;
          Inc(ResultPixels);
        end;
    except FreeAndNil(Result) end;
  end;

begin
  FPImage := TFPMemoryImage.Create(0, 0);
  try
    FPImage.UsePalette := AllowPalette;
    try
      FPImage.LoadFromStream(Stream, Reader);
      { FPImage.LoadFromStream may raise any kind of Exception (for example,
        in case of invalid JPEG header it just raises generic Exception class).
        Catch it and convert to our EImageLoadError. }
    except on E: Exception do raise EImageLoadError.Create(E.Message) end;
    if ClassAllowed(TRGBAlphaImage, AllowedImageClasses) and DetectAlpha then
      ReadRGBA else
      ReadRGB;
  finally
    FreeAndNil(FPImage);
    FreeAndNil(Reader);
  end;
end;

function LoadGIF(Stream: TStream;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions): TImage;
begin
  Result := LoadFpImage(Stream, AllowedImageClasses, ForbiddenConvs, TFPReaderGIF.Create);
end;

function LoadTGA(Stream: TStream;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions): TImage;
begin
  Result := LoadFpImage(Stream, AllowedImageClasses, ForbiddenConvs, TFPReaderTarga.Create);
end;

{
TIFF reader from FPReadTiff fails on ~/images/test_images/lena.tif
with "missing RowsPerStrip.."
AllowPalette = true or false, doesn't matter.

function LoadTIFF(Stream: TStream;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions): TImage;
begin
  Result := LoadFpImage(Stream, AllowedImageClasses, ForbiddenConvs, TFPReaderTIFF.Create, true);
end;
}

function LoadXPM(Stream: TStream;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions): TImage;
begin
  Result := LoadFpImage(Stream, AllowedImageClasses, ForbiddenConvs, TFPReaderXPM.Create, true);
end;

function LoadPSD(Stream: TStream;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions): TImage;
begin
  Result := LoadFpImage(Stream, AllowedImageClasses, ForbiddenConvs, TFPReaderPSD.Create);
end;

function LoadPCX(Stream: TStream;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions): TImage;
begin
  Result := LoadFpImage(Stream, AllowedImageClasses, ForbiddenConvs, TFPReaderPCX.Create);
end;

function LoadJPEG(Stream: TStream;
  const AllowedImageClasses: array of TImageClass;
  const ForbiddenConvs: TImageLoadConversions): TImage;
begin
  Result := LoadFpImage(Stream, AllowedImageClasses, ForbiddenConvs, TFPReaderJPEG.Create);
end;
