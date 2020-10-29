{
  Copyright 2001-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Image oparations using FPImage.

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

{ TInternalCastleFpImage ----------------------------------------------------- }

function CreateFpImage(const Width, Height: Integer): TInternalCastleFpImage;
begin
  Result := TInternalCastleFpImage.Create(Width, Height);
  //Result := TFPMemoryImage.Create(Width, Height);
  //Result := CreateFPCompactImg(GetFPCompactImgDesc(false, 8, true), Width, Height);
end;

function TInternalCastleFpImage.GetColors8Bit(const x, y: integer): TFPCompactImgRGBA8BitValue;
begin
  Result := FData[X + Y * Width];
end;

procedure TInternalCastleFpImage.SetColors8Bit(const x, y: integer; const Value: TFPCompactImgRGBA8BitValue);
begin
  FData[X + Y * Width] := Value;
end;

{ TInternalCastleFpImage -> TEncodedImage --------------------------------------------- }

procedure TEncodedImage.FromFpImage(const FPImage: TInternalCastleFpImage);
begin
  NotImplemented('FromFpImage');
end;

procedure TRGBImage.FromFpImage(const FPImage: TInternalCastleFpImage);
var
  X, Y: Integer;
  ResultPixels: PVector3Byte;
  Color: TFPCompactImgRGBA8BitValue;
begin
  SetSize(FPImage.Width, FPImage.Height);

  ResultPixels := Pixels;
  for Y := FPImage.Height - 1 downto 0 do
    for X := 0 to FPImage.Width - 1 do
    begin
      Color := FPImage.Colors8Bit[X, Y];
      ResultPixels^.Data[0] := Color.R;
      ResultPixels^.Data[1] := Color.G;
      ResultPixels^.Data[2] := Color.B;
      Inc(ResultPixels);
    end;
end;

procedure TRGBAlphaImage.FromFpImage(const FPImage: TInternalCastleFpImage);
var
  Y: Integer;
  RowSize: Integer;
  SourceRow: PByte;
  DestRow: PByte;
begin
  SetSize(FPImage.Width, FPImage.Height);

  RowSize := FPImage.Width * 4;
  { prepare pointers }
  SourceRow := PByte(FPImage.FData) + RowSize * FPImage.Height;
  DestRow := FRawPixels;

  { copy lines upside down }
  for Y := FPImage.Height - 1 downto 0 do
  begin
    Dec(SourceRow, RowSize);
    Move(SourceRow^, DestRow^, RowSize);
    Inc(DestRow, RowSize);
  end;
end;

procedure TGrayscaleImage.FromFpImage(const FPImage: TInternalCastleFpImage);
var
  X, Y: Integer;
  ResultPixels: PByte;
  Color: TFPCompactImgRGBA8BitValue;
begin
  SetSize(FPImage.Width, FPImage.Height);

  ResultPixels := Pixels;
  for Y := FPImage.Height - 1 downto 0 do
    for X := 0 to FPImage.Width - 1 do
    begin
      Color := FPImage.Colors8Bit[X, Y];
      ResultPixels^ := GrayscaleValue(Vector3Byte(
        Color.R,
        Color.G,
        Color.B));
      Inc(ResultPixels);
    end;
end;

procedure TGrayscaleAlphaImage.FromFpImage(const FPImage: TInternalCastleFpImage);
var
  X, Y: Integer;
  ResultPixels: PVector2Byte;
  Color: TFPCompactImgRGBA8BitValue;
begin
  SetSize(FPImage.Width, FPImage.Height);

  ResultPixels := Pixels;
  for Y := FPImage.Height - 1 downto 0 do
    for X := 0 to FPImage.Width - 1 do
    begin
      Color := FPImage.Colors8Bit[X, Y];
      ResultPixels^.Data[0] := GrayscaleValue(Vector3Byte(
        Color.R,
        Color.G,
        Color.B));
      ResultPixels^.Data[1] := Color.A;
      Inc(ResultPixels);
    end;
end;

{ Convert TInternalCastleFpImage to the best TCastleImage class. }
function CreateFromFpImage(const FPImage: TInternalCastleFpImage;
  const AllowedImageClasses: array of TEncodedImageClass): TCastleImage;

  { FPImage doesn't provide us any information does the image contain useful
    alpha channel, and is it grayscale.
    So just iterate over pixels, and check it ourselves. }
  procedure Detect(out Alpha, Grayscale: boolean);
  var
    X, Y: Integer;
    Col: TFPCompactImgRGBA8BitValue;
  begin
    Alpha := false;
    Grayscale := true;
    for Y := FPImage.Height - 1 downto 0 do
      for X := 0 to FPImage.Width - 1 do
      begin
        Col := FPImage.Colors8Bit[X, Y];
        if Col.A <> $FF then
        begin
          Alpha := true;
          if not Grayscale then Exit; // early exit for RGBA images
        end;
        if (Col.R <> Col.G) or
           (Col.R <> Col.B) then
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

  Result := nil;
  try
    { first try to return the best class for given Alpha/Grayscale combination }

    if not Grayscale then
    begin
      if ClassAllowed(TRGBAlphaImage, AllowedImageClasses) and Alpha then
      begin
        Result := TRGBAlphaImage.Create;
        Result.FromFpImage(FPImage);
        Exit;
      end else
      if ClassAllowed(TRGBImage, AllowedImageClasses) and not Alpha then
      begin
        Result := TRGBImage.Create;
        Result.FromFpImage(FPImage);
        Exit;
      end;
    end else
    begin
      if ClassAllowed(TGrayscaleAlphaImage, AllowedImageClasses) and Alpha then
      begin
        Result := TGrayscaleAlphaImage.Create;
        Result.FromFpImage(FPImage);
        Exit;
      end else
      if ClassAllowed(TGrayscaleImage, AllowedImageClasses) and not Alpha then
      begin
        Result := TGrayscaleImage.Create;
        Result.FromFpImage(FPImage);
        Exit;
      end;
    end;

    { otherwise, load RGBA and eventually strip some information to satisfy
      ClassAllowed conditions }

    RGBA := TRGBAlphaImage.Create;
    RGBA.FromFpImage(FPImage);
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

  except FreeAndNil(Result); raise end;
end;

function LoadFpImage(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass;
  Reader: TFPCustomImageReader): TEncodedImage;
var
  FPImage: TInternalCastleFpImage;
begin
  FPImage := CreateFpImage(0, 0);
  try
    FPImage.UsePalette := false;
    try
      FPImage.LoadFromStream(Stream, Reader);
      { FPImage.LoadFromStream may raise any kind of Exception (for example,
        in case of invalid JPEG header it just raises generic Exception class).
        Catch it and convert to our EImageLoadError. }
    except on E: Exception do raise EImageLoadError.Create(E.Message) end;
    Result := CreateFromFpImage(FPImage, AllowedImageClasses);
  finally
    FreeAndNil(FPImage);
    FreeAndNil(Reader);
  end;
end;

{ TEncodedImage -> TInternalCastleFpImage --------------------------------------------- }

function TEncodedImage.ToFpImage: TInternalCastleFpImage;
begin
  raise EImageSaveError.Create('Cannot convert to FPImage the images of class ' + ClassName);
  Result := nil; // avoid warning
end;

function TGrayscaleImage.ToFpImage: TInternalCastleFpImage;
var
  X, Y: Integer;
  P: PByte;
  Color: TFPCompactImgRGBA8BitValue;
begin
  Result := CreateFpImage(Width, Height);
  try
    Result.UsePalette := false;
    P := Pixels;
    for Y := Result.Height - 1 downto 0 do
      for X := 0 to Result.Width - 1 do
      begin
        Color.R := P^;
        Color.G := Color.R;
        Color.B := Color.R;
        Color.A := $FF;
        Result.Colors8Bit[X, Y] := Color;
        Inc(P);
      end;
  except FreeAndNil(Result); raise end;
end;

function TGrayscaleAlphaImage.ToFpImage: TInternalCastleFpImage;
var
  X, Y: Integer;
  P: PVector2Byte;
  Color: TFPCompactImgRGBA8BitValue;
begin
  Result := CreateFpImage(Width, Height);
  try
    Result.UsePalette := false;
    P := Pixels;
    for Y := Result.Height - 1 downto 0 do
      for X := 0 to Result.Width - 1 do
      begin
        Color.R := P^.Data[0];
        Color.G := Color.R;
        Color.B := Color.R;
        Color.A := P^.Data[1];
        Result.Colors8Bit[X, Y] := Color;
        Inc(P);
      end;
  except FreeAndNil(Result); raise end;
end;

function TRGBImage.ToFpImage: TInternalCastleFpImage;
var
  X, Y: Integer;
  P: PVector3Byte;
  Color: TFPCompactImgRGBA8BitValue;
begin
  Result := CreateFpImage(Width, Height);
  try
    Result.UsePalette := false;
    P := Pixels;
    for Y := Result.Height - 1 downto 0 do
      for X := 0 to Result.Width - 1 do
      begin
        Color.R := P^.Data[0];
        Color.G := P^.Data[1];
        Color.B := P^.Data[2];
        Color.A := $FF;
        Result.Colors8Bit[X, Y] := Color;
        Inc(P);
      end;
  except FreeAndNil(Result); raise end;
end;

function TRGBAlphaImage.ToFpImage: TInternalCastleFpImage;
var
  X, Y: Integer;
  P: PVector4Byte;
  Color: TFPCompactImgRGBA8BitValue;
begin
  Result := CreateFpImage(Width, Height);
  try
    Result.UsePalette := false;
    P := Pixels;
    for Y := Result.Height - 1 downto 0 do
      for X := 0 to Result.Width - 1 do
      begin
        Color.R := P^.Data[0];
        Color.G := P^.Data[1];
        Color.B := P^.Data[2];
        Color.A := P^.Data[3];
        Result.Colors8Bit[X, Y] := Color;
        Inc(P);
      end;
  except FreeAndNil(Result); raise end;
end;

procedure SaveFpImage(Img: TEncodedImage; Stream: TStream;
  Writer: TFPCustomImageWriter);
var
  FPImage: TInternalCastleFpImage;
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

{ LoadXxx, SaveXxx for particular image file formats ------------------------- }

function LoadGIF(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
begin
  Result := LoadFpImage(Stream, AllowedImageClasses, TFPReaderGIF.Create);
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
  Result := LoadFpImage(Stream, AllowedImageClasses, TFPReaderPSD.Create);
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

function LoadPNG_FPImage(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
begin
  Result := LoadFpImage(Stream, AllowedImageClasses, TFPReaderPNG.Create);
end;

function LoadPNM(Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;
begin
  Result := LoadFpImage(Stream, AllowedImageClasses, TFPReaderPNM.Create);
end;

procedure SaveJPEG(Img: TEncodedImage; Stream: TStream);
begin
  SaveFpImage(Img, Stream, TFPWriterJPEG.Create);
end;

procedure SavePNG_FPImage(Img: TEncodedImage; Stream: TStream);
var
  Writer: TFPWriterPNG;
begin
  Writer := TFPWriterPNG.Create;
  { without this, Writer.UseAlpha is always false and TFPWriterPNG never
    stores alpha channel of png image. }
  Writer.UseAlpha := Img.HasAlpha;
  { By default, we produce 16-bit PNG. Which usually is useless
    (as our inputs is 8-bit), and makes "nvcompress" on macOS fail
    on PNG files produced by our "downscaling". }
  Writer.WordSized := false;
  SaveFpImage(Img, Stream, Writer);
end;

{ resizing ------------------------------------------------------------------- }

function TCastleImage.MakeResizedToFpImage(ResizeWidth, ResizeHeight: Cardinal;
  const Interpolation: TResizeInterpolation): TInternalCastleFpImage;

  function CreateInterpolation(const I: TResizeInterpolation): TFPBaseInterpolation;
  begin
    case I of
      riNearest        : Result := TBoxInterpolation.Create;
      riBilinear       : Result := TBilineairInterpolation.Create;
      riMitchel        : Result := TMitchelInterpolation.Create;
      riBlackman       : Result := TBlackmanInterpolation.Create;
      riBlackmanSinc   : Result := TBlackmanSincInterpolation.Create;
      riBlackmanBessel : Result := TBlackmanBesselInterpolation.Create;
      riGaussian       : Result := TGaussianInterpolation.Create;
      riHermite        : Result := THermiteInterpolation.Create;
      riLanczos        : Result := TLanczosInterpolation.Create;
      riQuadratic      : Result := TQuadraticInterpolation.Create;
      riCubic          : Result := TCubicInterpolation.Create;
      riCatrom         : Result := TCatromInterpolation.Create;
      riHanning        : Result := THanningInterpolation.Create;
      riHamming        : Result := THammingInterpolation.Create;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('CreateInterpolation:I?');
      {$endif}
    end;
  end;

var
  Interpolator: TFPBaseInterpolation;
  FPImageSource: TInternalCastleFpImage;
  CanvasDestination: TFPImageCanvas;
begin
  if ResizeWidth = 0 then ResizeWidth := Width;
  if ResizeHeight = 0 then ResizeHeight := Height;

  FPImageSource := ToFpImage;
  try
    Interpolator := CreateInterpolation(Interpolation);
    try
      Result := CreateFpImage(ResizeWidth, ResizeHeight);
      try
        {$warnings off} // contains some abstract methods (FCL bug?)
        CanvasDestination := TFPImageCanvas.Create(Result);
        {$warnings on}
        try
          CanvasDestination.Interpolation := Interpolator;
          CanvasDestination.StretchDraw(0, 0, ResizeWidth, ResizeHeight, FPImageSource);
        finally FreeAndNil(CanvasDestination) end;
      except FreeAndNil(Result); raise end;
    finally FreeAndNil(Interpolator) end;
  finally FreeAndNil(FPImageSource) end;
end;
