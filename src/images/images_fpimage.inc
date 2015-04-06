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

{ TFPMemoryImage -> TEncodedImage --------------------------------------------- }

class function TRGBImage.FromFpImage(const FPImage: TFPMemoryImage): TRGBImage;
var
  X, Y: Integer;
  ResultPixels: PVector3Byte;
  Color: TFPColor;
begin
  Result := TRGBImage.Create(FPImage.Width, FPImage.Height);
  try
    ResultPixels := Result.RGBPixels;
    for Y := FPImage.Height - 1 downto 0 do
      for X := 0 to FPImage.Width - 1 do
      begin
        Color := FPImage.Colors[X, Y];
        ResultPixels^[0] := Color.Red shr 8;
        ResultPixels^[1] := Color.Green shr 8;
        ResultPixels^[2] := Color.Blue shr 8;
        Inc(ResultPixels);
      end;
  except FreeAndNil(Result); raise end;
end;

class function TRGBAlphaImage.FromFpImage(const FPImage: TFPMemoryImage): TRGBAlphaImage;
var
  X, Y: Integer;
  ResultPixels: PVector4Byte;
  Color: TFPColor;
begin
  Result := TRGBAlphaImage.Create(FPImage.Width, FPImage.Height);
  try
    ResultPixels := Result.AlphaPixels;
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
  except FreeAndNil(Result); raise end;
end;

class function TGrayscaleImage.FromFpImage(const FPImage: TFPMemoryImage): TGrayscaleImage;
var
  X, Y: Integer;
  ResultPixels: PByte;
  Color: TFPColor;
begin
  Result := TGrayscaleImage.Create(FPImage.Width, FPImage.Height);
  try
    ResultPixels := Result.GrayscalePixels;
    for Y := FPImage.Height - 1 downto 0 do
      for X := 0 to FPImage.Width - 1 do
      begin
        Color := FPImage.Colors[X, Y];
        ResultPixels^ := GrayscaleValue(Vector3Byte(
          Color.Red shr 8,
          Color.Green shr 8,
          Color.Blue shr 8));
        Inc(ResultPixels);
      end;
  except FreeAndNil(Result); raise end;
end;

class function TGrayscaleAlphaImage.FromFpImage(const FPImage: TFPMemoryImage): TGrayscaleAlphaImage;
var
  X, Y: Integer;
  ResultPixels: PVector2Byte;
  Color: TFPColor;
begin
  Result := TGrayscaleAlphaImage.Create(FPImage.Width, FPImage.Height);
  try
    ResultPixels := Result.GrayscaleAlphaPixels;
    for Y := FPImage.Height - 1 downto 0 do
      for X := 0 to FPImage.Width - 1 do
      begin
        Color := FPImage.Colors[X, Y];
        ResultPixels^[0] := GrayscaleValue(Vector3Byte(
          Color.Red shr 8,
          Color.Green shr 8,
          Color.Blue shr 8));
        ResultPixels^[1] := Color.Alpha shr 8;
        Inc(ResultPixels);
      end;
  except FreeAndNil(Result); raise end;
end;

function FromFpImage(const FPImage: TFPMemoryImage;
  const AllowedImageClasses: array of TEncodedImageClass): TCastleImage;

  { FPImage doesn't provide us any information does the image contain useful
    alpha channel, and is it grayscale.
    So just iterate over pixels, and check it ourselves. }
  procedure Detect(out Alpha, Grayscale: boolean);
  var
    X, Y: Integer;
    Col: TFPColor;
  begin
    Alpha := false;
    Grayscale := true;
    for Y := FPImage.Height - 1 downto 0 do
      for X := 0 to FPImage.Width - 1 do
      begin
        Col := FPImage.Colors[X, Y];
        if (Col.Alpha shr 8) <> $FF then
        begin
          Alpha := true;
          if not Grayscale then Exit; // early exit for RGBA images
        end;
        if ((Col.Red shr 8) <> (Col.Green shr 8)) or
           ((Col.Red shr 8) <> (Col.Blue shr 8)) then
        begin
          Grayscale := false;
          if Alpha then Exit; // early exit for RGBA images
        end;
      end;
  end;

var
  Alpha, Grayscale: boolean;
  RGBA: TRGBAlphaImage;
begin
  Detect(Alpha, Grayscale);

  { first try to return the best class for given Alpha/Grayscale combination }

  if not Grayscale then
  begin
    if ClassAllowed(TRGBAlphaImage, AllowedImageClasses) and Alpha then
      Exit(TRGBAlphaImage.FromFpImage(FPImage)) else
    if ClassAllowed(TRGBImage, AllowedImageClasses) and not Alpha then
      Exit(TRGBImage.FromFpImage(FPImage));
  end else
  begin
    if ClassAllowed(TGrayscaleAlphaImage, AllowedImageClasses) and Alpha then
      Exit(TGrayscaleAlphaImage.FromFpImage(FPImage)) else
    if ClassAllowed(TGrayscaleImage, AllowedImageClasses) and not Alpha then
      Exit(TGrayscaleImage.FromFpImage(FPImage));
  end;

  { otherwise, load RGBA and eventually strip some information to satisfy
    ClassAllowed conditions }

  RGBA := TRGBAlphaImage.FromFpImage(FPImage);
  if ClassAllowed(TRGBAlphaImage, AllowedImageClasses) then
    Result := RGBA else
  if ClassAllowed(TRGBImage, AllowedImageClasses) then
  begin
    Result := RGBA.ToRGBImage;
    FreeAndNil(RGBA);
  end else
  if ClassAllowed(TGrayscaleImage, AllowedImageClasses) then
  begin
    Result := RGBA.ToGrayscaleImage;
    FreeAndNil(RGBA);
  end else
  if ClassAllowed(TGrayscaleAlphaImage, AllowedImageClasses) then
  begin
    Result := RGBA.ToGrayscaleAlphaImage;
    FreeAndNil(RGBA);
  end else
  begin
    FreeAndNil(RGBA);
    raise Exception.Create('Cannot convert FPImage to any of formats allowed by LoadImage AllowedImageClasses');
  end;
end;

function LoadFpImage(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass;
  Reader: TFPCustomImageReader): TEncodedImage;
var
  FPImage: TFPMemoryImage;
begin
  FPImage := TFPMemoryImage.Create(0, 0);
  try
    FPImage.UsePalette := false;
    try
      FPImage.LoadFromStream(Stream, Reader);
      { FPImage.LoadFromStream may raise any kind of Exception (for example,
        in case of invalid JPEG header it just raises generic Exception class).
        Catch it and convert to our EImageLoadError. }
    except on E: Exception do raise EImageLoadError.Create(E.Message) end;
    Result := FromFpImage(FPImage, AllowedImageClasses);
  finally
    FreeAndNil(FPImage);
    FreeAndNil(Reader);
  end;
end;

{ TEncodedImage -> TFPMemoryImage --------------------------------------------- }

function TEncodedImage.ToFpImage: TFPMemoryImage;
begin
  raise EImageSaveError.Create('Cannot convert to FPImage, and cannot use resize with TResizeNiceInterpolation mode, on images of class ' + ClassName);
  Result := nil; // avoid warning
end;

function TGrayscaleImage.ToFpImage: TFPMemoryImage;
var
  X, Y: Integer;
  P: PByte;
  Color: TFPColor;
begin
  Result := TFPMemoryImage.Create(Width, Height);
  try
    Result.UsePalette := false;
    P := GrayscalePixels;
    for Y := Result.Height - 1 downto 0 do
      for X := 0 to Result.Width - 1 do
      begin
        Color.Red   := Word(P^) shl 8;
        Color.Green := Color.Red;
        Color.Blue  := Color.Red;
        Color.Alpha := $FFFF;
        Result.Colors[X, Y] := Color;
        Inc(P);
      end;
  except FreeAndNil(Result); raise end;
end;

function TGrayscaleAlphaImage.ToFpImage: TFPMemoryImage;
var
  X, Y: Integer;
  P: PVector2Byte;
  Color: TFPColor;
begin
  Result := TFPMemoryImage.Create(Width, Height);
  try
    Result.UsePalette := false;
    P := GrayscaleAlphaPixels;
    for Y := Result.Height - 1 downto 0 do
      for X := 0 to Result.Width - 1 do
      begin
        Color.Red   := Word(P^[0]) shl 8;
        Color.Green := Color.Red;
        Color.Blue  := Color.Red;
        Color.Alpha := Word(P^[1]) shl 8;
        Result.Colors[X, Y] := Color;
        Inc(P);
      end;
  except FreeAndNil(Result); raise end;
end;

function TRGBImage.ToFpImage: TFPMemoryImage;
var
  X, Y: Integer;
  P: PVector3Byte;
  Color: TFPColor;
begin
  Result := TFPMemoryImage.Create(Width, Height);
  try
    Result.UsePalette := false;
    P := RGBPixels;
    for Y := Result.Height - 1 downto 0 do
      for X := 0 to Result.Width - 1 do
      begin
        { It doesn't matter what you put in least-significat
          byte of color component, FPWriteJPEG will do "shr 8" on these values
          anyway (JPEG can only save 8-bit RGB). }
        Color.Red   := Word(P^[0]) shl 8;
        Color.Green := Word(P^[1]) shl 8;
        Color.Blue  := Word(P^[2]) shl 8;
        Color.Alpha := $FFFF;
        Result.Colors[X, Y] := Color;
        Inc(P);
      end;
  except FreeAndNil(Result); raise end;
end;

function TRGBAlphaImage.ToFpImage: TFPMemoryImage;
var
  X, Y: Integer;
  P: PVector4Byte;
  Color: TFPColor;
begin
  Result := TFPMemoryImage.Create(Width, Height);
  try
    Result.UsePalette := false;
    P := AlphaPixels;
    for Y := Result.Height - 1 downto 0 do
      for X := 0 to Result.Width - 1 do
      begin
        { It doesn't matter what you put in least-significat
          byte of color component, FPWriteJPEG will do "shr 8" on these values
          anyway (JPEG can only save 8-bit RGB). }
        Color.Red   := Word(P^[0]) shl 8;
        Color.Green := Word(P^[1]) shl 8;
        Color.Blue  := Word(P^[2]) shl 8;
        Color.Alpha := Word(P^[3]) shl 8;
        Result.Colors[X, Y] := Color;
        Inc(P);
      end;
  except FreeAndNil(Result); raise end;
end;

procedure SaveFpImage(Img: TEncodedImage; Stream: TStream;
  Writer: TFPCustomImageWriter);
var
  FPImage: TFPMemoryImage;
begin
  FPImage := nil;
  try
    FPImage := Img.ToFpImage;
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
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
begin
  {$ifndef VER2_2}
  Result := LoadFpImage(Stream, AllowedImageClasses, TFPReaderGIF.Create);
  {$else}
  raise EImageLoadError.Create('Cannot load GIF when compiled with old FPC <= 2.2.x');
  {$endif}
end;

function LoadTGA(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
begin
  Result := LoadFpImage(Stream, AllowedImageClasses, TFPReaderTarga.Create);
end;

{
TIFF reader from FPReadTiff fails on ~/images/test_images/lena.tif
with "missing RowsPerStrip.."

function LoadTIFF(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
begin
  Result := LoadFpImage(Stream, AllowedImageClasses, TFPReaderTIFF.Create);
end;
}

function LoadXPM(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
begin
  Result := LoadFpImage(Stream, AllowedImageClasses, TFPReaderXPM.Create);
end;

function LoadPSD(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
begin
  {$ifndef VER2_2}
  Result := LoadFpImage(Stream, AllowedImageClasses, TFPReaderPSD.Create);
  {$else}
  raise EImageLoadError.Create('Cannot load PSD when compiled with old FPC <= 2.2.x');
  {$endif}
end;

function LoadPCX(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
begin
  Result := LoadFpImage(Stream, AllowedImageClasses, TFPReaderPCX.Create);
end;

function LoadJPEG(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
begin
  Result := LoadFpImage(Stream, AllowedImageClasses, TFPReaderJPEG.Create);
end;

{$ifdef CASTLE_PNG_USING_FCL_IMAGE}
function LoadPNG(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
begin
  Result := LoadFpImage(Stream, AllowedImageClasses, TFPReaderPNG.Create);
end;
{$endif}

function LoadPNM(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
begin
  Result := LoadFpImage(Stream, AllowedImageClasses, TFPReaderPNM.Create);
end;

procedure SaveJPEG(Img: TEncodedImage; Stream: TStream);
begin
  SaveFpImage(Img, Stream, TFPWriterJPEG.Create);
end;

{$ifdef CASTLE_PNG_USING_FCL_IMAGE}
procedure SavePNG(Img: TEncodedImage; Stream: TStream);
begin
  SaveFpImage(Img, Stream, TFPWriterPNG.Create);
end;
{$endif}

{ resizing ------------------------------------------------------------------- }

function TCastleImage.MakeResized(ResizeWidth, ResizeHeight: Cardinal;
  const Interpolation: TResizeNiceInterpolation): TCastleImage;

  function CreateInterpolation(const I: TResizeNiceInterpolation): TFPBaseInterpolation;
  begin
    case I of
      rniNearest        : Result := TBoxInterpolation.Create;
      rniBilinear       : Result := TBilineairInterpolation.Create;
      rniMitchel        : Result := TMitchelInterpolation.Create;
      rniBlackman       : Result := TBlackmanInterpolation.Create;
      rniBlackmanSinc   : Result := TBlackmanSincInterpolation.Create;
      rniBlackmanBessel : Result := TBlackmanBesselInterpolation.Create;
      rniGaussian       : Result := TGaussianInterpolation.Create;
      rniHermite        : Result := THermiteInterpolation.Create;
      rniLanczos        : Result := TLanczosInterpolation.Create;
      rniQuadratic      : Result := TQuadraticInterpolation.Create;
      rniCubic          : Result := TCubicInterpolation.Create;
      rniCatrom         : Result := TCatromInterpolation.Create;
      rniHanning        : Result := THanningInterpolation.Create;
      rniHamming        : Result := THammingInterpolation.Create;
      else EInternalError.Create('CreateInterpolation:I?');
    end;
  end;

var
  Interpolator: TFPBaseInterpolation;
  FPImageSource, FPImageDestination: TFPMemoryImage;
  CanvasDestination: TFPImageCanvas;
begin
  if ResizeWidth = 0 then ResizeWidth := Width;
  if ResizeHeight = 0 then ResizeHeight := Height;

  FPImageSource := ToFpImage;
  try
    Interpolator := CreateInterpolation(Interpolation);
    try
      FPImageDestination := TFPMemoryImage.Create(ResizeWidth, ResizeHeight);
      try
        {$warnings off} // contains some abstract methods (FCL bug?)
        CanvasDestination := TFPImageCanvas.Create(FPImageDestination);
        {$warnings on}
        try
          CanvasDestination.StretchDraw(0, 0, ResizeWidth, ResizeHeight, FPImageSource);
        finally FreeAndNil(CanvasDestination) end;
        Result := FromFpImage(FPImageDestination, [TCastleImageClass(ClassType)]);
      finally FreeAndNil(FPImageDestination) end;
    finally FreeAndNil(Interpolator) end;
  finally FreeAndNil(FPImageSource) end;
end;
