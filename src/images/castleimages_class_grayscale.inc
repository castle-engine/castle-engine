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

  { Grayscale image. Color is a simple Byte value. }
  TGrayscaleImage = class(TCastleImage)
  private
    FTreatAsAlpha: boolean;
    FColorWhenTreatedAsAlpha: TVector3Byte;
    FGrayscaleColorWhenTreatedAsAlpha: Byte;
    function GetPixels: PByte;
    function GetPixelsArray: PByteArray;
    procedure SetColorWhenTreatedAsAlpha(const Value: TVector3Byte);
    {$ifdef FPC}
    procedure FromFpImage(const FPImage: TInternalCastleFpImage); override;
    {$endif}
  protected
    procedure DrawFromCore(Source: TCastleImage;
      X, Y, SourceX, SourceY, SourceWidth, SourceHeight: Integer;
      const Mode: TDrawMode); override;
    function GetColors(const X, Y, Z: Integer): TCastleColor; override;
    procedure SetColors(const X, Y, Z: Integer; const C: TCastleColor); override;
    procedure AssignProperties(const Source: TCastleImage); override;
  public
    constructor Create; overload; override;

    { Pointer to pixels. Same as RawPixels, only typecasted to PByte. }
    property Pixels: PByte read GetPixels;
    property GrayscalePixels: PByte read GetPixels; {$ifdef FPC} deprecated 'use Pixels'; {$endif}
    { Pointer to pixels. Same as RawPixels, only typecasted to PByteArray. }
    property PixelsArray: PByteArray read GetPixelsArray;

    class function PixelSize: Cardinal; override;
    class function ColorComponentsCount: Cardinal; override;

    function PixelPtr(const X, Y: Cardinal; const Z: Cardinal = 0): PByte;
    function RowPtr(const Y: Cardinal; const Z: Cardinal = 0): PByteArray;

    procedure InvertColors; override;

    procedure Clear(const Pixel: TVector4Byte); override;
    function IsClear(const Pixel: TVector4Byte): boolean; override;

    procedure Clear(const Pixel: Byte); reintroduce; overload;
    function IsClear(const Pixel: Byte): boolean; reintroduce; overload;

    { Every pixels value is halved (divided by 2).
      This is done by simple bitshift, so you can be sure that all
      components are < 2^7 after this. }
    procedure HalfColors;

    { Add alpha channel.

      If TreatAsAlpha = @false: add constant opaque alpha.

      If TreatAsAlpha = @true: output alpha will be derived from source grayscale,
      output luminance will be derived from ColorWhenTreatedAsAlpha. }
    function ToGrayscaleAlphaImage: TGrayscaleAlphaImage; deprecated 'create TGrayscaleAlphaImage and use TGrayscaleAlphaImage.Assign';

    {$ifdef FPC}
    function ToFpImage: TInternalCastleFpImage; override;
    {$endif}

    procedure LerpWith(const Value: Single; SecondImage: TCastleImage); override;
    class procedure MixColors(const OutputColor: Pointer;
       const Weights: TVector4; const AColors: TVector4Pointer); override;

    { Should we treat grayscale image as pure alpha channel (without any color
      information) when using this as a texture.

      This property is meaningful for some operations:

      @orderedList(
        @item(
          When creating OpenGL texture from this image.
          If @true, then the grayscale pixel data will be loaded as alpha channel contents.
          When the texture is read by shaders, the RGB is (1,1,1) and alpha comes from the image.

          Note the we don't pass ColorWhenTreatedAsAlpha to OpenGL,
          as we don't have this functionality (e.g. https://www.khronos.org/opengl/wiki/Texture#Swizzle_mask
          cannot express an arbitrary but constant color on some channels.)
        )

        @item(
          When using @link(DrawFrom) / @link(DrawTo) methods or being assigned to something using @link(Assign).
          If @true, this image is drawn like an RGBA image,
          with constant RGB color ColorWhenTreatedAsAlpha, and alpha channel
          taken from contents of this image.)

        @item(
          It is also the only way for TGrayscaleImage to return AlphaChannel <> acNone.)

      )
    }
    property TreatAsAlpha: boolean
      read FTreatAsAlpha write FTreatAsAlpha default false;

    { Used for drawing/assigning when TreatAsAlpha is @true, and we need the base
      (not alpha) color for some equation.
      By default white (255, 255, 255). }
    property ColorWhenTreatedAsAlpha: TVector3Byte
      read FColorWhenTreatedAsAlpha write SetColorWhenTreatedAsAlpha;

    { Automatically derived from ColorWhenTreatedAsAlpha by averaging RGB components
      to calculate grayscale luminance.
      By default 255. }
    property GrayscaleColorWhenTreatedAsAlpha: Byte
      read FGrayscaleColorWhenTreatedAsAlpha;

    function AlphaChannel(
      const AlphaTolerance: Byte): TAlphaChannel; override;

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

{ TGrayscaleImage ------------------------------------------------------------ }

constructor TGrayscaleImage.Create;
begin
  inherited;
  FColorWhenTreatedAsAlpha := Vector3Byte(255, 255, 255);
  FGrayscaleColorWhenTreatedAsAlpha := GrayscaleValue(FColorWhenTreatedAsAlpha);
end;

function TGrayscaleImage.GetPixels: PByte;
begin
  Result := PByte(RawPixels);
end;

function TGrayscaleImage.GetPixelsArray: PByteArray;
begin
  Result := PByteArray(RawPixels);
end;

class function TGrayscaleImage.PixelSize: Cardinal;
begin
  Result := 1;
end;

class function TGrayscaleImage.ColorComponentsCount: Cardinal;
begin
  Result := 1;
end;

function TGrayscaleImage.PixelPtr(const X, Y, Z: Cardinal): PByte;
begin
  Result := PByte(inherited PixelPtr(X, Y, Z));
end;

function TGrayscaleImage.RowPtr(const Y, Z: Cardinal): PByteArray;
begin
  Result := PByteArray(inherited RowPtr(Y, Z));
end;

procedure TGrayscaleImage.Clear(const Pixel: TVector4Byte);
begin
  Clear(GrayscaleValue(Pixel));
end;

function TGrayscaleImage.IsClear(const Pixel: TVector4Byte): boolean;
begin
  Result := IsClear(GrayscaleValue(Pixel));
end;

procedure TGrayscaleImage.Clear(const Pixel: Byte);
begin
  FillChar(RawPixels^, Size, Pixel);
end;

function TGrayscaleImage.IsClear(const Pixel: Byte): boolean;
begin
  Result := IsMemCharFilled(RawPixels^, Size, AnsiChar(Pixel));
end;

procedure TGrayscaleImage.HalfColors;
var
  P: PByte;
  I: Cardinal;
begin
  P := Pixels;
  for I := 1 to Width * Height * Depth do
  begin
    P^ := P^ shr 1;
    Inc(P);
  end;
end;

procedure TGrayscaleImage.LerpWith(const Value: Single; SecondImage: TCastleImage);
var
  SelfPtr: PByte;
  SecondPtr: PByte;
  I: Cardinal;
begin
  LerpSimpleCheckConditions(SecondImage);

  SelfPtr := Pixels;
  SecondPtr := TGrayscaleImage(SecondImage).Pixels;
  for I := 1 to Width * Height * Depth do
  begin
    SelfPtr^ := Clamped(Round(Lerp(Value, SelfPtr^, SecondPtr^)), 0, High(Byte));
    Inc(SelfPtr);
    Inc(SecondPtr);
  end;
end;

class procedure TGrayscaleImage.MixColors(const OutputColor: Pointer;
  const Weights: TVector4; const AColors: TVector4Pointer);
var
  OutputCol: PByte absolute OutputColor;
  Cols: array [0..3] of PByte absolute AColors;
begin
  {$I norqcheckbegin.inc}
  OutputCol^ := {$ifndef FAST_UNSAFE_MIX_COLORS} Clamped( {$endif} Round(
    Weights.X * Cols[0]^ +
    Weights.Y * Cols[1]^ +
    Weights.Z * Cols[2]^ +
    Weights.W * Cols[3]^) {$ifndef FAST_UNSAFE_MIX_COLORS} , 0, High(Byte)) {$endif};
  {$I norqcheckend.inc}
end;

function TGrayscaleImage.ToGrayscaleAlphaImage: TGrayscaleAlphaImage;
begin
  Result := TGrayscaleAlphaImage.Create;
  Result.Assign(Self);
end;

function TGrayscaleImage.AlphaChannel(
  const AlphaTolerance: Byte): TAlphaChannel;
var
  PtrAlpha: PByte;
  I: Cardinal;
begin
  if not TreatAsAlpha then
    Exit(inherited AlphaChannel(AlphaTolerance));

  PtrAlpha := Pixels;

  for I := 1 to Width * Height * Depth do
  begin
    if (PtrAlpha^ > AlphaTolerance) and
       (PtrAlpha^ < 255 - AlphaTolerance) then
      Exit(acBlending);
    Inc(PtrAlpha);
  end;

  Result := acTest;
end;

procedure TGrayscaleImage.InvertColors;
var
  I: Cardinal;
  P: PByte;
begin
  P := Pixels;
  for I := 1 to Width * Height * Depth do
  begin
    P^ := High(Byte)-P^;
    Inc(P);
  end;
end;

function TGrayscaleImage.GetColors(const X, Y, Z: Integer): TCastleColor;
var
  Pixel: PByte;
begin
  Pixel := PixelPtr(X, Y, Z);
  Result.X := Pixel^ / 255;
  Result.Y := Pixel^ / 255;
  Result.Z := Pixel^ / 255;
  Result.W := 1.0;
end;

procedure TGrayscaleImage.SetColors(const X, Y, Z: Integer; const C: TCastleColor);
var
  Pixel: PByte;
begin
  Pixel := PixelPtr(X, Y, Z);
  Pixel^ := Clamped(Round(GrayscaleValue(C) * 255), Low(Byte), High(Byte));
end;

procedure TGrayscaleImage.SetColorWhenTreatedAsAlpha(const Value: TVector3Byte);
begin
  FColorWhenTreatedAsAlpha := Value;
  FGrayscaleColorWhenTreatedAsAlpha := GrayscaleValue(Value);
end;

procedure TGrayscaleImage.AssignProperties(const Source: TCastleImage);
begin
  inherited;

  { Copying these properties between TGrayscaleImage->TGrayscaleImage
    is important, e.g. when old OpenGLs need to resize font texture
    (to be power of 2) we have to copy the TreatAsAlpha value,
    to render font properly with alpha blending.
  }
  if Source is TGrayscaleImage then
  begin
    FTreatAsAlpha := TGrayscaleImage(Source).TreatAsAlpha;
    FColorWhenTreatedAsAlpha := TGrayscaleImage(Source).ColorWhenTreatedAsAlpha;
    FGrayscaleColorWhenTreatedAsAlpha := TGrayscaleImage(Source).GrayscaleColorWhenTreatedAsAlpha;
  end;
end;

{$endif read_implementation}