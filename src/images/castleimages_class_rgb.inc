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

  { Image with pixel represented as a TVector3Byte (red, green, blue). }
  TRGBImage = class(TCastleImage)
  private
    function GetPixels: PVector3Byte;
    function GetPixelsArray: PVector3ByteArray;
    {$ifdef FPC}
    procedure FromFpImage(const FPImage: TInternalCastleFpImage); override;
    {$endif FPC}
  protected
    procedure DrawFromCore(Source: TCastleImage;
      X, Y, SourceX, SourceY, SourceWidth, SourceHeight: Integer;
      const Mode: TDrawMode); override;
    function GetColors(const X, Y, Z: Integer): TCastleColor; override;
    procedure SetColors(const X, Y, Z: Integer; const C: TCastleColor); override;
  public
    { Pointer to pixels. Same as RawPixels, only typecasted to PVector3Byte. }
    property Pixels: PVector3Byte read GetPixels;
    property RGBPixels: PVector3Byte read GetPixels; {$ifdef FPC} deprecated 'use Pixels'; {$endif}
    { Pointer to pixels. Same as RawPixels, only typecasted to PVector3ByteArray. }
    property PixelsArray: PVector3ByteArray read GetPixelsArray;

    class function PixelSize: Cardinal; override;
    class function ColorComponentsCount: Cardinal; override;

    function PixelPtr(const X, Y: Cardinal; const Z: Cardinal = 0): PVector3Byte;
    function RowPtr(const Y: Cardinal; const Z: Cardinal = 0): PVector3ByteArray;

    procedure InvertColors; override;

    procedure Clear(const Pixel: TVector4Byte); override;
    function IsClear(const Pixel: TVector4Byte): boolean; override;

    procedure TransformRGB(const Matrix: TMatrix3); override;
    procedure ModulateRGB(const ColorModulator: TColorModulatorByteFunc); override;

    { Create a new TRGBAlphaImage instance with RGB contents copied from this
      image, and alpha fully opaque. }
    function ToRGBAlphaImage: TRGBAlphaImage; deprecated 'create TRGBAlphaImage and use TRGBAlphaImage.Assign';

    { Convert image to an TRGBFloatImage format.

      Generally this conversion is safe, since Single can express
      more-or-less precisely (and definitely uniquely) all values that Byte can express.
      (Yeah, we have autotest for this statement.) }
    function ToRGBFloat: TRGBFloatImage; deprecated 'create TRGBFloatImage and use TRGBFloatImage.Assign';

    function ToGrayscale: TGrayscaleImage; deprecated 'create TGrayscaleImage and use TGrayscaleImage.Assign';

    {$ifdef FPC}
    function ToFpImage: TInternalCastleFpImage; override;
    {$endif FPC}

    { Draw horizontal line. Must be y1 <= y2, else it is NOOP. }
    procedure HorizontalLine(const x1, x2, y: Integer;
      const Color: TVector3Byte); overload;
    procedure HorizontalLine(const x1, x2, y: Integer;
      const Color: TCastleColor); overload;

    { Draw vertical line. Must be x1 <= x2, else it is NOOP. }
    procedure VerticalLine(const x, y1, y2: Integer;
      const Color: TVector3Byte); overload;
    procedure VerticalLine(const x, y1, y2: Integer;
      const Color: TCastleColor); overload;

    { Create image by merging two images according to a (third) mask image.
      This is a very special constructor.
      It creates image with the same size as MapImage.
      It also resizes ReplaceWhiteImage, ReplaceBlackImage
      to the size of MapImage.

      Then it inits color of each pixel of our image with
      combined colors of two pixels on the same coordinates from
      ReplaceWhiteImage, ReplaceBlackImage, something like

      @preformatted(
        Pixel[x, y] := ReplaceWhiteImage[x, y] * S +
                       ReplaceBlackImage[x, y] * (S-1);
      )

      where S = average of red, gree, blue of color MapImage[x, y].

      This means that final image will look like ReplaceWhiteImage
      in the areas where MapImage is white, and it will look like
      ReplaceBlackImage in the areas where MapImage is black. }
    constructor CreateCombined(const MapImage: TRGBImage;
      var ReplaceWhiteImage, ReplaceBlackImage: TRGBImage);

    procedure LerpWith(const Value: Single; SecondImage: TCastleImage); override;
    class procedure MixColors(const OutputColor: Pointer;
       const Weights: TVector4; const AColors: TVector4Pointer); override;

    procedure Assign(const Source: TCastleImage); override;

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

{ TRGBImage ------------------------------------------------------------ }

constructor TRGBImage.CreateCombined(const MapImage: TRGBImage;
  var ReplaceWhiteImage, ReplaceBlackImage: TRGBImage);
var
  Map, White, Black, Res: PVector3Byte;
  s: single;
  i: integer;
begin
  Create(MapImage.Width, MapImage.Height);

  ReplaceWhiteImage.Resize(MapImage.Width, MapImage.Height);
  ReplaceBlackImage.Resize(MapImage.Width, MapImage.Height);

  Map := MapImage.Pixels;
  White := ReplaceWhiteImage.Pixels;
  Black := ReplaceBlackImage.Pixels;
  Res := Pixels;

  for i := 1 to Width * Height * Depth do
  begin
    s := (Map^.X + Map^.Y + Map^.Z) / 255 / 3;
    Res^.X := Round(s * White^.X + (1-s) * Black^.X);
    Res^.Y := Round(s * White^.Y + (1-s) * Black^.Y);
    Res^.Z := Round(s * White^.Z + (1-s) * Black^.Z);
    Inc(Map);
    Inc(White);
    Inc(Black);
    Inc(Res);
  end;
end;

function TRGBImage.GetPixels: PVector3Byte;
begin
  Result := PVector3Byte(RawPixels);
end;

function TRGBImage.GetPixelsArray: PVector3ByteArray;
begin
  Result := PVector3ByteArray(RawPixels);
end;

class function TRGBImage.PixelSize: Cardinal;
begin
  Result := 3;
end;

class function TRGBImage.ColorComponentsCount: Cardinal;
begin
  Result := 3;
end;

function TRGBImage.PixelPtr(const X, Y, Z: Cardinal): PVector3Byte;
begin
  Result := PVector3Byte(inherited PixelPtr(X, Y, Z));
end;

function TRGBImage.RowPtr(const Y, Z: Cardinal): PVector3ByteArray;
begin
  Result := PVector3ByteArray(inherited RowPtr(Y, Z));
end;

procedure TRGBImage.InvertColors;
var
  i: Cardinal;
  prgb: PVector3Byte;
begin
  prgb := Pixels;
  for i := 1 to Width * Height * Depth do
  begin
    prgb^.X := High(byte) - prgb^.X;
    prgb^.Y := High(byte) - prgb^.Y;
    prgb^.Z := High(byte) - prgb^.Z;
    Inc(prgb);
  end;
end;

function TRGBImage.GetColors(const X, Y, Z: Integer): TCastleColor;
var
  Pixel: PVector3Byte;
begin
  Pixel := PixelPtr(X, Y, Z);
  Result.X := Pixel^.X / 255;
  Result.Y := Pixel^.Y / 255;
  Result.Z := Pixel^.Z / 255;
  Result.W := 1.0;
end;

procedure TRGBImage.SetColors(const X, Y, Z: Integer; const C: TCastleColor);
var
  Pixel: PVector3Byte;
begin
  Pixel := PixelPtr(X, Y, Z);
  Pixel^.X := Clamped(Round(C.X * 255), Low(Byte), High(Byte));
  Pixel^.Y := Clamped(Round(C.Y * 255), Low(Byte), High(Byte));
  Pixel^.Z := Clamped(Round(C.Z * 255), Low(Byte), High(Byte));
end;

procedure TRGBImage.Clear(const Pixel: TVector4Byte);
var
  P: PVector3Byte;
  I: Cardinal;
begin
  P := Pixels;
  for I := 1 to Width * Height * Depth do
  begin
    Move(Pixel, P^, SizeOf(TVector3Byte));
    Inc(P);
  end;
end;

function TRGBImage.IsClear(const Pixel: TVector4Byte): boolean;
var
  P: PVector3Byte;
  I: Cardinal;
begin
  P := Pixels;
  for I := 1 to Width * Height * Depth do
  begin
    if not CompareMem(@Pixel, P, SizeOf(TVector3Byte)) then
    begin
      Result := false;
      Exit;
    end;
    Inc(P);
  end;
  Result := true;
end;

procedure TRGBImage.TransformRGB(const Matrix: TMatrix3);
type PPixel = PVector3Byte;
{$I castleimages_transformrgb_implement.inc}

procedure TRGBImage.ModulateRGB(const ColorModulator: TColorModulatorByteFunc);
type PPixel = PVector3Byte;
{$I castleimages_modulatergb_implement.inc}

function TRGBImage.ToRGBAlphaImage: TRGBAlphaImage;
begin
  Result := TRGBAlphaImage.Create;
  Result.Assign(Self);
end;

function TRGBImage.ToRGBFloat: TRGBFloatImage;
begin
  Result := TRGBFloatImage.Create;
  Result.Assign(Self);
end;

function TRGBImage.ToGrayscale: TGrayscaleImage;
begin
  Result := TGrayscaleImage.Create;
  Result.Assign(Self);
end;

procedure TRGBImage.HorizontalLine(const x1, x2, y: Integer;
  const Color: TVector3Byte);
var
  P: PVector3Byte;
  i: Integer;
begin
  P := PixelPtr(x1, y);
  for i := 0 to x2 - x1 do begin P^ := Color; Inc(P) end;
end;

procedure TRGBImage.HorizontalLine(const X1, X2, Y: Integer;
  const Color: TCastleColor);
begin
  HorizontalLine(X1, X2, Y, Vector3Byte(Color.XYZ));
end;

procedure TRGBImage.VerticalLine(const x, y1, y2: Integer;
  const Color: TVector3Byte);
var P: PVector3Byte;
    i: Integer;
begin
 P := PixelPtr(x, y1);
 for i := 0 to y2 - y1 do
 begin
  P^ := Color;
  P := PointerAdd(P, SizeOf(TVector3Byte) * Width);
 end;
end;

procedure TRGBImage.VerticalLine(const x, y1, y2: Integer;
  const Color: TCastleColor);
begin
  VerticalLine(X, Y1, Y2, Vector3Byte(Color.XYZ));
end;

procedure TRGBImage.LerpWith(const Value: Single; SecondImage: TCastleImage);
var
  SelfPtr: PVector3Byte;
  SecondPtr: PVector3Byte;
  I: Cardinal;
begin
  LerpSimpleCheckConditions(SecondImage);

  SelfPtr := Pixels;
  SecondPtr := TRGBImage(SecondImage).Pixels;
  for I := 1 to Width * Height * Depth do
  begin
    SelfPtr^ := Lerp(Value, SelfPtr^, SecondPtr^);
    Inc(SelfPtr);
    Inc(SecondPtr);
  end;
end;

{ $define FAST_UNSAFE_MIX_COLORS}

class procedure TRGBImage.MixColors(const OutputColor: Pointer;
  const Weights: TVector4; const AColors: TVector4Pointer);
var
  OutputCol: PVector3Byte absolute OutputColor;
  Cols: array [0..3] of PVector3Byte absolute AColors;
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
  {$I norqcheckend.inc}
end;

{$endif read_implementation}