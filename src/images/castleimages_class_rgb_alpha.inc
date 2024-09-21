{
  Copyright 2001-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { Image with each pixel stored as 4 bytes: RGB (color) and alpha (opacity). }
  TRGBAlphaImage = class(TCastleImage)
  private
    FPremultipliedAlpha: boolean;
    function GetPixels: PVector4Byte;
    function GetPixelsArray: PVector4ByteArray;
    {$ifdef FPC}
    procedure FromFpImage(const FPImage: TInternalCastleFpImage); override;
    {$endif}
  protected
    procedure DrawFromCore(Source: TCastleImage;
      X, Y, SourceX, SourceY, SourceWidth, SourceHeight: Integer;
      const Mode: TDrawMode); override;
    function GetColors(const X, Y, Z: Integer): TCastleColor; override;
    procedure SetColors(const X, Y, Z: Integer; const C: TCastleColor); override;
  public
    { Pointer to pixels. Same as RawPixels, only typecasted to PVector4Byte. }
    property Pixels: PVector4Byte read GetPixels;
    {$ifdef FPC}property AlphaPixels: PVector4Byte read GetPixels; deprecated 'use Pixels';{$endif}
    { Pointer to pixels. Same as RawPixels, only typecasted to PVector4ByteArray. }
    property PixelsArray: PVector4ByteArray read GetPixelsArray;

    class function PixelSize: Cardinal; override;
    class function ColorComponentsCount: Cardinal; override;

    function PixelPtr(const X, Y: Cardinal; const Z: Cardinal = 0): PVector4Byte;
    function RowPtr(const Y: Cardinal; const Z: Cardinal = 0): PVector4ByteArray;

    procedure InvertColors; override;

    procedure Clear(const Pixel: TVector4Byte); override;
    function IsClear(const Pixel: TVector4Byte): boolean; override;

    { Set alpha channel on every pixel to the same given value. }
    procedure ClearAlpha(const Alpha: Byte);

    procedure TransformRGB(const Matrix: TMatrix3); override;
    procedure ModulateRGB(const ColorModulator: TColorModulatorByteFunc); override;

    { Set alpha of every pixel to either AlphaOnColor
      (when color of pixel is equal to AlphaColor with Tolerance,
      see @link(EqualRGB)) or AlphaOnNoColor. }
    procedure AlphaDecide(const AlphaColor: TVector3Byte;
      Tolerance: Byte; AlphaOnColor: Byte; AlphaOnNoColor: Byte);

    { Copy RGB contents from one image, and alpha contents from the other.
      RGB channels are copied from the RGB image,
      alpha channel is copied from the Grayscale image. Given RGB and Grayscale
      images must have the same size, and this is the resulting
      size of this image after Compose call. }
    procedure Compose(RGB: TRGBImage; AGrayscale: TGrayscaleImage);

    function HasAlpha: boolean; override;

    function AlphaChannel(
      const AlphaTolerance: Byte): TAlphaChannel; override;

    procedure LerpWith(const Value: Single; SecondImage: TCastleImage); override;
    class procedure MixColors(const OutputColor: Pointer;
       const Weights: TVector4; const AColors: TVector4Pointer); override;

    procedure Assign(const Source: TCastleImage); override;

    { Remove alpha channel. }
    function ToRGBImage: TRGBImage; deprecated 'create TRGBImage and use TRGBImage.Assign';

    { Flatten to grayscale. }
    function ToGrayscaleAlphaImage: TGrayscaleAlphaImage; deprecated 'create TGrayscaleAlphaImage and use TGrayscaleAlphaImage.Assign';

    { Flatten to grayscale and remove alpha channel. }
    function ToGrayscaleImage: TGrayscaleImage; deprecated 'create TGrayscaleImage and use TGrayscaleImage.Assign';
    {$ifdef FPC}
    function ToFpImage: TInternalCastleFpImage; override;
    {$endif}

    { Premultiply the RGB channel with alpha, to make it faster
      to use this image as source for TCastleImage.DrawTo and
      TCastleImage.DrawFrom operations. Changes @link(PremultipliedAlpha)
      from @false to @true. Unless @link(PremultipliedAlpha) was
      already @true, in which case this method does nothing --- this way
      it is safe to call this many times, we will not repeat multiplying.

      @italic(The image with premultiplied alpha can only be used
      with a subset of image routines that actually support premultiplied alpha.)
      Right now, these are only TCastleImage.DrawTo and
      TCastleImage.DrawFrom. Image with PremultipliedAlpha can be used
      as a source for drawing, and the results will be the same as without
      premultiplying, but faster. }
    procedure PremultiplyAlpha;
    property PremultipliedAlpha: boolean read FPremultipliedAlpha;

    procedure AlphaBleed(const ProgressTitle: string = ''); override;
    function MakeAlphaBleed(const ProgressTitle: string = ''): TCastleImage; override;
  public
    procedure FillEllipse(const x, y: single; const aRadiusX, aRadiusY: single;
      const aColor: TCastleColor); override;
    procedure Ellipse(const x, y: single; const aRadiusX, aRadiusY: single;
      const aWidth: single; const aColor: TCastleColor); override;
    procedure FillRectangle(const x1, y1, x2, y2: single;
      const aColor: TCastleColor); override;
    procedure Rectangle(const x1, y1, x2, y2: single;
      const aWidth: single; const aColor: TCastleColor); override;
    procedure Line(const x1, y1, x2, y2: single;
      const aWidth: single; const aColor: TCastleColor); override;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TRGBAlphaImage ------------------------------------------------------------ }

function TRGBAlphaImage.GetPixels: PVector4Byte;
begin
  Result := PVector4Byte(RawPixels);
end;

function TRGBAlphaImage.GetPixelsArray: PVector4ByteArray;
begin
  Result := PVector4ByteArray(RawPixels);
end;

class function TRGBAlphaImage.PixelSize: Cardinal;
begin
  Result := 4;
end;

class function TRGBAlphaImage.ColorComponentsCount: Cardinal;
begin
  Result := 4;
end;

function TRGBAlphaImage.PixelPtr(const X, Y, Z: Cardinal): PVector4Byte;
begin
  Result := PVector4Byte(inherited PixelPtr(X, Y, Z));
end;

function TRGBAlphaImage.RowPtr(const Y, Z: Cardinal): PVector4ByteArray;
begin
  Result := PVector4ByteArray(inherited RowPtr(Y, Z));
end;

procedure TRGBAlphaImage.InvertColors;
var
  i: Cardinal;
  palpha: PVector4Byte;
begin
  palpha := Pixels;
  for i := 1 to Width * Height * Depth do
  begin
    palpha^.X := High(byte) - palpha^.X;
    palpha^.Y := High(byte) - palpha^.Y;
    palpha^.Z := High(byte) - palpha^.Z;
    Inc(palpha);
  end;
end;

function TRGBAlphaImage.GetColors(const X, Y, Z: Integer): TCastleColor;
var
  Pixel: PVector4Byte;
begin
  Pixel := PixelPtr(X, Y, Z);
  Result.X := Pixel^.X / 255;
  Result.Y := Pixel^.Y / 255;
  Result.Z := Pixel^.Z / 255;
  Result.W := Pixel^.W / 255;
end;

procedure TRGBAlphaImage.SetColors(const X, Y, Z: Integer; const C: TCastleColor);
var
  Pixel: PVector4Byte;
begin
  Pixel := PixelPtr(X, Y, Z);
  Pixel^.X := Clamped(Round(C.X * 255), Low(Byte), High(Byte));
  Pixel^.Y := Clamped(Round(C.Y * 255), Low(Byte), High(Byte));
  Pixel^.Z := Clamped(Round(C.Z * 255), Low(Byte), High(Byte));
  Pixel^.W := Clamped(Round(C.W * 255), Low(Byte), High(Byte));
end;

procedure TRGBAlphaImage.Clear(const Pixel: TVector4Byte);
begin
  FillDWord(RawPixels^, Width*Height, UInt32(Pixel));
end;

procedure TRGBAlphaImage.ClearAlpha(const Alpha: Byte);
var
  i: Cardinal;
  palpha: PVector4Byte;
begin
  palpha := Pixels;
  for i := 1 to Width * Height * Depth do
  begin
    palpha^.W := Alpha;
    Inc(palpha);
  end;
end;

function TRGBAlphaImage.IsClear(const Pixel: TVector4Byte): boolean;
begin
  Result := IsMemDWordFilled(RawPixels^, Width * Height * Depth, UInt32(Pixel));
end;

procedure TRGBAlphaImage.TransformRGB(const Matrix: TMatrix3);
type PPixel = PVector4Byte;
{$I castleimages_transformrgb_implement.inc}

procedure TRGBAlphaImage.ModulateRGB(const ColorModulator: TColorModulatorByteFunc);
type PPixel = PVector4Byte;
{$I castleimages_modulatergb_implement.inc}

procedure TRGBAlphaImage.AlphaDecide(const AlphaColor: TVector3Byte;
  Tolerance: Byte; AlphaOnColor: Byte; AlphaOnNoColor: Byte);
var
  pa: PVector4Byte;
  i: Cardinal;
begin
  pa := Pixels;
  for i := 1 to Width * Height * Depth do
  begin
    if EqualRGB(AlphaColor, PVector3Byte(pa)^, Tolerance) then
      pa^.W := AlphaOnColor else
      pa^.W := AlphaOnNoColor;
    Inc(pa);
  end;
end;

procedure TRGBAlphaImage.Compose(RGB: TRGBImage; AGrayscale: TGrayscaleImage);
var
  PtrAlpha: PVector4Byte;
  PtrRGB: PVector3Byte;
  PtrGrayscale: PByte;
  I: Cardinal;
begin
  Check( (RGB.Width = AGrayscale.Width) and
         (RGB.Height = AGrayscale.Height) and
         (RGB.Depth = AGrayscale.Depth),
    'For TRGBAlphaImage.Compose, RGB and alpha images must have the same sizes');

  SetSize(RGB);

  PtrAlpha := Pixels;
  PtrRGB := RGB.Pixels;
  PtrGrayscale := AGrayscale.Pixels;

  for I := 1 to Width * Height * Depth do
  begin
    System.Move(PtrRGB^, PtrAlpha^, SizeOf(TVector3Byte));
    PtrAlpha^.W := PtrGrayscale^;

    Inc(PtrAlpha);
    Inc(PtrRGB);
    Inc(PtrGrayscale);
  end;
end;

function TRGBAlphaImage.HasAlpha: boolean;
begin
  Result := true;
end;

function TRGBAlphaImage.AlphaChannel(
  const AlphaTolerance: Byte): TAlphaChannel;
var
  PtrAlpha: PVector4Byte;
  I: Cardinal;
begin
  PtrAlpha := Pixels;

  for I := 1 to Width * Height * Depth do
  begin
    if (PtrAlpha^.W > AlphaTolerance) and
       (PtrAlpha^.W < 255 - AlphaTolerance) then
      Exit(acBlending);
    Inc(PtrAlpha);
  end;

  Result := acTest;
end;

procedure TRGBAlphaImage.LerpWith(const Value: Single; SecondImage: TCastleImage);
var
  SelfPtr: PVector4Byte;
  SecondPtr: PVector4Byte;
  I: Cardinal;
begin
  LerpSimpleCheckConditions(SecondImage);

  SelfPtr := Pixels;
  SecondPtr := TRGBAlphaImage(SecondImage).Pixels;
  for I := 1 to Width * Height * Depth do
  begin
    SelfPtr^ := Lerp(Value, SelfPtr^, SecondPtr^);
    Inc(SelfPtr);
    Inc(SecondPtr);
  end;
end;

class procedure TRGBAlphaImage.MixColors(const OutputColor: Pointer;
  const Weights: TVector4; const AColors: TVector4Pointer);
var
  OutputCol: PVector4Byte absolute OutputColor;
  Cols: array [0..3] of PVector4Byte absolute AColors;
begin
  {$I norqcheckbegin.inc}
  OutputCol^.X := {$ifndef FAST_UNSAFE_MIX_COLORS} Clamped( {$endif} Round(
    Weights.X * Cols[0]^.X +
    Weights.Y * Cols[1]^.X +
    Weights.Z * Cols[2]^.X +
    Weights.W * Cols[3]^.X) {$ifndef FAST_UNSAFE_MIX_COLORS} , 0, High(Byte)) {$endif};
  OutputCol^.Y := {$ifndef FAST_UNSAFE_MIX_COLORS} Clamped( {$endif} Round(
    Weights.X * Cols[0]^.Y +
    Weights.Y * Cols[1]^.Y +
    Weights.Z * Cols[2]^.Y +
    Weights.W * Cols[3]^.Y) {$ifndef FAST_UNSAFE_MIX_COLORS} , 0, High(Byte)) {$endif};
  OutputCol^.Z := {$ifndef FAST_UNSAFE_MIX_COLORS} Clamped( {$endif} Round(
    Weights.X * Cols[0]^.Z +
    Weights.Y * Cols[1]^.Z +
    Weights.Z * Cols[2]^.Z +
    Weights.W * Cols[3]^.Z) {$ifndef FAST_UNSAFE_MIX_COLORS} , 0, High(Byte)) {$endif};
  OutputCol^.W := {$ifndef FAST_UNSAFE_MIX_COLORS} Clamped( {$endif} Round(
    Weights.X * Cols[0]^.W +
    Weights.Y * Cols[1]^.W +
    Weights.Z * Cols[2]^.W +
    Weights.W * Cols[3]^.W) {$ifndef FAST_UNSAFE_MIX_COLORS} , 0, High(Byte)) {$endif};
  {$I norqcheckend.inc}
end;

function TRGBAlphaImage.ToRGBImage: TRGBImage;
begin
  Result := TRGBImage.Create;
  Result.Assign(Self);
end;

function TRGBAlphaImage.ToGrayscaleImage: TGrayscaleImage;
begin
  Result := TGrayscaleImage.Create;
  Result.Assign(Self);
end;

function TRGBAlphaImage.ToGrayscaleAlphaImage: TGrayscaleAlphaImage;
begin
  Result := TGrayscaleAlphaImage.Create;
  Result.Assign(Self);
end;

procedure TRGBAlphaImage.PremultiplyAlpha;
var
  P: PVector4Byte;
  I: Integer;
begin
  if not FPremultipliedAlpha then
  begin
    FPremultipliedAlpha := true;
    P := Pixels;
    for I := 1 to Width * Height * Depth do
    begin
      P^.X := Clamped(Round(P^.X * P^.W / 255), 0, 255);
      P^.Y := Clamped(Round(P^.Y * P^.W / 255), 0, 255);
      P^.Z := Clamped(Round(P^.Z * P^.W / 255), 0, 255);
      Inc(P);
    end;
  end;
end;

procedure TRGBAlphaImage.AlphaBleed(const ProgressTitle: string);
var
  Copy: TCastleImage;
begin
  Copy := MakeAlphaBleed(ProgressTitle);
  try
    Assert(Size = Copy.Size);
    Move(Copy.RawPixels^, RawPixels^, Size);
  finally FreeAndNil(Copy) end;
end;

function TRGBAlphaImage.MakeAlphaBleed(const ProgressTitle: string): TCastleImage;

  function FindNearestNonTransparentPixel(X, Y, Z: Integer): PVector4Byte;

    function TryPixelOpaque(const DX, DY: Integer;
      var SomePixelWithinImage: boolean): PVector4Byte;
    var
      NX, NY, GX, GY: Integer;
    begin
      NX := X + DX;
      NY := Y + DY;
      GX := Clamped(NX, 0, Width - 1);
      GY := Clamped(NY, 0, Height - 1);
      { does not search in Z.
        This is faster for 2D images.
        Also this way we do not look if we're outside Z range below,
        which would stop the search too quickly for 2D images. }
      if (NX = GX) and (NY = GY) then
        SomePixelWithinImage := true;
      Result := PixelPtr(GX, GY, Z);
      if Result^.W <> High(Byte) then
        Result := nil; // nope, tried pixel is not opaque
    end;

  var
    Distance: Cardinal;
    SomePixelWithinImage: boolean;
  begin
    Distance := 1;
    Result := nil;
    repeat
      SomePixelWithinImage := false;
      Result := TryPixelOpaque(-Distance,         0, SomePixelWithinImage); if Result <> nil then Exit;
      Result := TryPixelOpaque( Distance,         0, SomePixelWithinImage); if Result <> nil then Exit;
      Result := TryPixelOpaque(        0, -Distance, SomePixelWithinImage); if Result <> nil then Exit;
      Result := TryPixelOpaque(        0,  Distance, SomePixelWithinImage); if Result <> nil then Exit;
      Result := TryPixelOpaque(-Distance, -Distance, SomePixelWithinImage); if Result <> nil then Exit;
      Result := TryPixelOpaque(-Distance,  Distance, SomePixelWithinImage); if Result <> nil then Exit;
      Result := TryPixelOpaque( Distance, -Distance, SomePixelWithinImage); if Result <> nil then Exit;
      Result := TryPixelOpaque( Distance,  Distance, SomePixelWithinImage); if Result <> nil then Exit;
      Inc(Distance);
    until not SomePixelWithinImage;
  end;

var
  P, NewP: PVector4Byte;
  X, Y, Z: Integer;
begin
  Result := MakeCopy;
  for X := 0 to Width - 1 do
    for Y := 0 to Height - 1 do
      for Z := 0 to Depth - 1 do
      begin
        P := Result.PixelPtr(X, Y, Z);
        if P^.W <> High(Byte) then
        begin
          NewP := FindNearestNonTransparentPixel(X, Y, Z);
          if NewP <> nil then
            Move(NewP^, P^, SizeOf(TVector3Byte));
        end;
      end;
end;

{$endif read_implementation}