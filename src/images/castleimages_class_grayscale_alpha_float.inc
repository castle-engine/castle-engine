{%MainUnit castleimages.pas}
{
  Copyright 2024-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { Image with each pixel represented as 2 floating-point numbers (Single):
    grayscale (luminance) and alpha (opacity).

    Compared to TGrayscaleAlphaImage, each pixel is a floating-point number,
    which means it is more precise and can also be above 1.0 value. }
  TGrayscaleAlphaFloatImage = class(TCastleImage)
  private
    function GetPixels: PVector2;
    function GetPixelsArray: PVector2Array;
  protected
    function GetColors(const X, Y, Z: Integer): TCastleColor; override;
    procedure SetColors(const X, Y, Z: Integer; const C: TCastleColor); override;
  public
    class function PixelSize: Cardinal; override;
    class function ColorComponentsCount: Cardinal; override;
    procedure Assign(const Source: TCastleImage); override;
    procedure InvertColors; override;
    class procedure MixColors(const OutputColor: Pointer;
       const Weights: TVector4; const AColors: TVector4Pointer); override;

    { Pointer to pixels. Same as RawPixels, only typecasted to PVector2. }
    property Pixels: PVector2 read GetPixels;

    { Pointer to pixels. Same as RawPixels, only typecasted to PVector2Array. }
    property PixelsArray: PVector2Array read GetPixelsArray;

    { Pointer to given pixel. }
    function PixelPtr(const X, Y: Cardinal; const Z: Cardinal = 0): PVector2;

    { Pointer to given row. }
    function RowPtr(const Y: Cardinal; const Z: Cardinal = 0): PVector2Array;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TGrayscaleAlphaFloatImage ------------------------------------------------------------ }

function TGrayscaleAlphaFloatImage.GetPixels: PVector2;
begin
  Result := PVector2(RawPixels);
end;

function TGrayscaleAlphaFloatImage.GetPixelsArray: PVector2Array;
begin
  Result := PVector2Array(RawPixels);
end;

class function TGrayscaleAlphaFloatImage.PixelSize: Cardinal;
begin
  Result := SizeOf(TVector2);
end;

class function TGrayscaleAlphaFloatImage.ColorComponentsCount: Cardinal;
begin
  Result := 2;
end;

function TGrayscaleAlphaFloatImage.PixelPtr(const X, Y, Z: Cardinal): PVector2;
begin
  Result := PVector2(inherited PixelPtr(X, Y, Z));
end;

function TGrayscaleAlphaFloatImage.RowPtr(const Y, Z: Cardinal): PVector2Array;
begin
  Result := PVector2Array(inherited RowPtr(Y, Z));
end;

procedure TGrayscaleAlphaFloatImage.InvertColors;
var
  I: Cardinal;
  P: PVector2;
begin
  P := Pixels;
  for I := 1 to Width * Height * Depth do
  begin
    P^.X := Max(1.0 - P^.X, 0.0); // make sure result is >= 0, even when input > 1.0
    // do not touch alpha in P.Y
    Inc(P);
  end;
end;

function TGrayscaleAlphaFloatImage.GetColors(const X, Y, Z: Integer): TCastleColor;
var
  Pixel: PVector2;
begin
  Pixel := PixelPtr(X, Y, Z);
  Result.X := Pixel^.X;
  Result.Y := Pixel^.X;
  Result.Z := Pixel^.X;
  Result.W := Pixel^.Y;
end;

procedure TGrayscaleAlphaFloatImage.SetColors(const X, Y, Z: Integer; const C: TCastleColor);
var
  Pixel: PVector2;
begin
  Pixel := PixelPtr(X, Y, Z);
  Pixel^.X := GrayscaleValue(C);
  Pixel^.Y := C.W;
end;

class procedure TGrayscaleAlphaFloatImage.MixColors(const OutputColor: Pointer;
  const Weights: TVector4; const AColors: TVector4Pointer);
var
  OutputCol: PVector2 absolute OutputColor;
  Cols: array [0..3] of PVector2 absolute AColors;
begin
  OutputCol^.X :=
    Weights.X * Cols[0]^.X +
    Weights.Y * Cols[1]^.X +
    Weights.Z * Cols[2]^.X +
    Weights.W * Cols[3]^.X;
  OutputCol^.Y :=
    Weights.X * Cols[0]^.Y +
    Weights.Y * Cols[1]^.Y +
    Weights.Z * Cols[2]^.Y +
    Weights.W * Cols[3]^.Y;
end;

{$endif read_implementation}
