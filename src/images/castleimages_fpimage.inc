{%MainUnit castleimages.pas}
{
  Copyright 2001-2022 Michalis Kamburelis.

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

{$ifdef FPC} // FpImage is only available in FPC

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
      ResultPixels^.X := Color.R;
      ResultPixels^.Y := Color.G;
      ResultPixels^.Z := Color.B;
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
      ResultPixels^.X := GrayscaleValue(Vector3Byte(
        Color.R,
        Color.G,
        Color.B));
      ResultPixels^.Y := Color.A;
      Inc(ResultPixels);
    end;
end;

procedure TRGBFloatImage.FromFpImage(const FPImage: TInternalCastleFpImage);
var
  X, Y: Integer;
  ResultPixels: PVector3;
  Color: TFPCompactImgRGBA8BitValue;
begin
  SetSize(FPImage.Width, FPImage.Height);

  ResultPixels := Pixels;
  for Y := FPImage.Height - 1 downto 0 do
    for X := 0 to FPImage.Width - 1 do
    begin
      Color := FPImage.Colors8Bit[X, Y];
      ResultPixels^.X := Color.R / 255;
      ResultPixels^.Y := Color.G / 255;
      ResultPixels^.Z := Color.B / 255;
      Inc(ResultPixels);
    end;
end;

{ Convert TInternalCastleFpImage to the best TCastleImage class. }
function CreateFromFpImage(const FPImage: TInternalCastleFpImage;
  const AllowedImageClasses: array of TEncodedImageClass): TEncodedImage;

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
  ImageClass: TEncodedImageClass;
begin
  if High(AllowedImageClasses) = 0 then
  begin
    { if AllowedImageClasses forces a specific class, then just create this image class }
    ImageClass := AllowedImageClasses[0];
  end else
  begin
    { detect Alpha and Grayscale looking at image contents, to determine ImageClass }
    Detect(Alpha, Grayscale);
    if not Grayscale then
    begin
      if Alpha then
        ImageClass := TRGBAlphaImage
      else
        ImageClass := TRGBImage;
    end else
    begin
      if Alpha then
        ImageClass := TGrayscaleAlphaImage
      else
        ImageClass := TGrayscaleImage;
    end;

    { if the optimal ImageClass (for these image contents) is not allowed,
      try to find another class that works, and looses least information. }
    if not ClassAllowed(ImageClass, AllowedImageClasses) then
    begin
      if ClassAllowed(TRGBAlphaImage, AllowedImageClasses) then
        ImageClass := TRGBAlphaImage
      else
      if ClassAllowed(TRGBImage, AllowedImageClasses) then
        ImageClass := TRGBImage
      else
      if ClassAllowed(TGrayscaleImage, AllowedImageClasses) then
        ImageClass := TGrayscaleImage
      else
      if ClassAllowed(TGrayscaleAlphaImage, AllowedImageClasses) then
        ImageClass := TGrayscaleAlphaImage
      else
        raise Exception.Create('Cannot load FPImage to any of formats allowed by LoadImage AllowedImageClasses');
    end;
  end;

  Result := ImageClass.Create;
  try
    Result.FromFpImage(FPImage);
  except FreeAndNil(Result); raise end;
end;

function Load_FpImage(const Stream: TStream;
  const AllowedImageClasses: array of TEncodedImageClass;
  const Reader: TFPCustomImageReader;
  const ImageFormat: TImageFormat): TEncodedImage;
var
  FPImage: TInternalCastleFpImage;
  DetectedClass: TEncodedImageClass;
begin
  { Unless exactly 1 image classes is allowed,
    try to detect best class using format-specific DetectClass function.
    This may avoid the need for time-consuming Detect(...) call in CreateFromFpImage.
    Note that we don't limit this detection only to AllowedImageClasses = [] case,
    as LoadImage is often called with TextureImageClasses or TextureImageClassesAll. }
  DetectedClass := nil;
  if (High(AllowedImageClasses) <> 0) and
     Assigned(ImageFormatInfos[ImageFormat].DetectClass) then
  begin
    DetectedClass := ImageFormatInfos[ImageFormat].DetectClass(Stream);
    if (DetectedClass <> nil) and
       (not ClassAllowed(DetectedClass, AllowedImageClasses)) then
    begin
      DetectedClass := nil; // do not use
    end;
    // for debug:
    // if DetectedClass <> nil then
    //   WritelnLog('Will use detected image class %s (format %s)', [
    //     DetectedClass.ClassName,
    //     ImageFormatInfos[ImageFormat].FormatName
    //   ]);
  end;

  FPImage := CreateFpImage(0, 0);
  try
    FPImage.UsePalette := false;
    try
      FPImage.LoadFromStream(Stream, Reader);
    except
      { FPImage.LoadFromStream may raise any kind of Exception (for example,
        in case of invalid JPEG header it just raises generic Exception class).
        Catch it and convert to our EImageLoadError. }
      on E: Exception do
        raise EImageLoadError.Create(E.Message)
    end;

    if DetectedClass <> nil then
      Result := CreateFromFpImage(FPImage, [DetectedClass])
    else
      Result := CreateFromFpImage(FPImage, AllowedImageClasses);
  finally
    FreeAndNil(FPImage);
    Reader.Free;
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
        Color.R := P^.X;
        Color.G := Color.R;
        Color.B := Color.R;
        Color.A := P^.Y;
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
        Color.R := P^.X;
        Color.G := P^.Y;
        Color.B := P^.Z;
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
        Color.R := P^.X;
        Color.G := P^.Y;
        Color.B := P^.Z;
        Color.A := P^.W;
        Result.Colors8Bit[X, Y] := Color;
        Inc(P);
      end;
  except FreeAndNil(Result); raise end;
end;

function TRGBFloatImage.ToFpImage: TInternalCastleFpImage;
var
  X, Y: Integer;
  P: PVector3;
  Col: TVector3Byte;
  Color: TFPCompactImgRGBA8BitValue;
begin
  Result := CreateFpImage(Width, Height);
  try
    Result.UsePalette := false;
    P := Pixels;
    for Y := Result.Height - 1 downto 0 do
      for X := 0 to Result.Width - 1 do
      begin
        Col := Vector3Byte(P^);
        Color.R := Col.X;
        Color.G := Col.Y;
        Color.B := Col.Z;
        Color.A := $FF;
        Result.Colors8Bit[X, Y] := Color;
        Inc(P);
      end;
  except FreeAndNil(Result); raise end;
end;

procedure Save_FpImage(Img: TEncodedImage; Stream: TStream;
  Writer: TFPCustomImageWriter);
var
  FPImage: TInternalCastleFpImage;
begin
  FPImage := nil;
  try
    {$warnings off} // using deprecated in deprecated
    FPImage := Img.ToFpImage;
    {$warnings on}
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

  {$warnings off} // using deprecated in deprecated
  FPImageSource := ToFpImage;
  {$warnings on}
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

{$endif FPC}
