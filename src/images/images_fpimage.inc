{
  Copyright 2001-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
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
  const AllowedImageClasses: array of TCastleImageClass;
  Reader: TFPCustomImageReader;
  const AllowPalette: boolean = false): TCastleImage;
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

procedure SaveFpImage(Img: TCastleImage; Stream: TStream;
  Writer: TFPCustomImageWriter;
  const AllowPalette: boolean = false);
var
  FPImage: TFPMemoryImage;

  procedure WriteRGB;
  var
    X, Y: Integer;
    ImgPixels: PVector3Byte;
    Color: TFPColor;
  begin
    ImgPixels := TRGBImage(Img).RGBPixels;
    for Y := FPImage.Height - 1 downto 0 do
      for X := 0 to FPImage.Width - 1 do
      begin
        { It doesn't matter what you put in least-significat
          byte of color component, FPWriteJPEG will do "shr 8" on these values
          anyway (JPEG can only save 8-bit RGB). }
        Color.Red   := Word(ImgPixels^[0]) shl 8;
        Color.Green := Word(ImgPixels^[1]) shl 8;
        Color.Blue  := Word(ImgPixels^[2]) shl 8;
        Color.Alpha := $FFFF;
        FPImage.Colors[X, Y] := Color;
        Inc(ImgPixels);
      end;
  end;

  procedure WriteRGBA;
  var
    X, Y: Integer;
    ImgPixels: PVector4Byte;
    Color: TFPColor;
  begin
    ImgPixels := TRGBAlphaImage(Img).AlphaPixels;
    for Y := FPImage.Height - 1 downto 0 do
      for X := 0 to FPImage.Width - 1 do
      begin
        { It doesn't matter what you put in least-significat
          byte of color component, FPWriteJPEG will do "shr 8" on these values
          anyway (JPEG can only save 8-bit RGB). }
        Color.Red   := Word(ImgPixels^[0]) shl 8;
        Color.Green := Word(ImgPixels^[1]) shl 8;
        Color.Blue  := Word(ImgPixels^[2]) shl 8;
        Color.Alpha := Word(ImgPixels^[3]) shl 8;
        FPImage.Colors[X, Y] := Color;
        Inc(ImgPixels);
      end;
  end;

begin
  FPImage := TFPMemoryImage.Create(Img.Width, Img.Height);
  try
    FPImage.UsePalette := AllowPalette;
    if Img is TRGBAlphaImage then
      WriteRGBA else
    if Img is TRGBImage then
      WriteRGB else
      raise EImageSaveError.Create('Cannot save non-RGB image');
    try
      FPImage.SaveToStream(Stream, Writer);
      { FPImage.SaveToStream may raise any kind of Exception.
        Catch it and convert to our EImageSaveError. }
    except on E: Exception do raise EImageSaveError.Create(E.Message) end;
  finally
    FreeAndNil(FPImage);
    FreeAndNil(Writer);
  end;
end;

function LoadGIF(Stream: TStream;
  const AllowedImageClasses: array of TCastleImageClass): TCastleImage;
begin
  {$ifndef VER2_2}
  Result := LoadFpImage(Stream, AllowedImageClasses, TFPReaderGIF.Create);
  {$else}
  raise EImageLoadError.Create('Cannot load GIF when compiled with old FPC <= 2.2.x');
  {$endif}
end;

function LoadTGA(Stream: TStream;
  const AllowedImageClasses: array of TCastleImageClass): TCastleImage;
begin
  Result := LoadFpImage(Stream, AllowedImageClasses, TFPReaderTarga.Create);
end;

{
TIFF reader from FPReadTiff fails on ~/images/test_images/lena.tif
with "missing RowsPerStrip.."
AllowPalette = true or false, doesn't matter.

function LoadTIFF(Stream: TStream;
  const AllowedImageClasses: array of TCastleImageClass): TCastleImage;
begin
  Result := LoadFpImage(Stream, AllowedImageClasses, TFPReaderTIFF.Create, true);
end;
}

function LoadXPM(Stream: TStream;
  const AllowedImageClasses: array of TCastleImageClass): TCastleImage;
begin
  Result := LoadFpImage(Stream, AllowedImageClasses, TFPReaderXPM.Create, true);
end;

function LoadPSD(Stream: TStream;
  const AllowedImageClasses: array of TCastleImageClass): TCastleImage;
begin
  {$ifndef VER2_2}
  Result := LoadFpImage(Stream, AllowedImageClasses, TFPReaderPSD.Create);
  {$else}
  raise EImageLoadError.Create('Cannot load PSD when compiled with old FPC <= 2.2.x');
  {$endif}
end;

function LoadPCX(Stream: TStream;
  const AllowedImageClasses: array of TCastleImageClass): TCastleImage;
begin
  Result := LoadFpImage(Stream, AllowedImageClasses, TFPReaderPCX.Create);
end;

function LoadJPEG(Stream: TStream;
  const AllowedImageClasses: array of TCastleImageClass): TCastleImage;
begin
  Result := LoadFpImage(Stream, AllowedImageClasses, TFPReaderJPEG.Create);
end;

{$ifdef CASTLE_PNG_USING_FCL_IMAGE}
function LoadPNG(Stream: TStream;
  const AllowedImageClasses: array of TCastleImageClass): TCastleImage;
begin
  Result := LoadFpImage(Stream, AllowedImageClasses, TFPReaderPNG.Create);
end;
{$endif}

function LoadPNM(Stream: TStream;
  const AllowedImageClasses: array of TCastleImageClass): TCastleImage;
begin
  Result := LoadFpImage(Stream, AllowedImageClasses, TFPReaderPNM.Create);
end;

procedure SaveJPEG(Img: TCastleImage; Stream: TStream);
begin
  SaveFpImage(Img, Stream, TFPWriterJPEG.Create);
end;

{$ifdef CASTLE_PNG_USING_FCL_IMAGE}
procedure SavePNG(Img: TCastleImage; Stream: TStream);
begin
  SaveFpImage(Img, Stream, TFPWriterPNG.Create);
end;
{$endif}
