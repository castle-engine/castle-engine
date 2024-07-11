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

  { Image with RGBA colors using Single (4 floating-point numbers for each pixel).
    For high-precision RGBA images.
    Compared to TRGBAlphaImage, each pixel is a floating-point number,
    which means it is more precise and can also be above 1.0 value
    (though they are still in 0..1 range when reading from normal image formats). }
  TRGBAlphaFloatImage = class(TCastleImage)
  private
    function GetPixels: PVector4;
    function GetPixelsArray: PVector4Array;
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

    { Pointer to pixels. Same as RawPixels, only typecasted to PVector4. }
    property Pixels: PVector4 read GetPixels;

    { Pointer to pixels. Same as RawPixels, only typecasted to PVector4Array. }
    property PixelsArray: PVector4Array read GetPixelsArray;

    { Pointer to given pixel. }
    function PixelPtr(const X, Y: Cardinal; const Z: Cardinal = 0): PVector4;

    { Pointer to given row. }
    function RowPtr(const Y: Cardinal; const Z: Cardinal = 0): PVector4Array;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TRGBAlphaFloatImage ------------------------------------------------------------ }

function TRGBAlphaFloatImage.GetPixels: PVector4;
begin
  Result := PVector4(RawPixels);
end;

function TRGBAlphaFloatImage.GetPixelsArray: PVector4Array;
begin
  Result := PVector4Array(RawPixels);
end;

class function TRGBAlphaFloatImage.PixelSize: Cardinal;
begin
  Result := SizeOf(TVector4);
end;

class function TRGBAlphaFloatImage.ColorComponentsCount: Cardinal;
begin
  Result := 4;
end;

function TRGBAlphaFloatImage.PixelPtr(const X, Y, Z: Cardinal): PVector4;
begin
  Result := PVector4(inherited PixelPtr(X, Y, Z));
end;

function TRGBAlphaFloatImage.RowPtr(const Y, Z: Cardinal): PVector4Array;
begin
  Result := PVector4Array(inherited RowPtr(Y, Z));
end;

procedure TRGBAlphaFloatImage.InvertColors;
var
  I: Cardinal;
  P: PVector4;
begin
  P := Pixels;
  for I := 1 to Width * Height * Depth do
  begin
    P^.X := Max(1.0 - P^.X, 0.0); // make sure result is >= 0, even when input > 1.0
    P^.Y := Max(1.0 - P^.Y, 0.0);
    P^.Z := Max(1.0 - P^.Z, 0.0);
    // P^.W := Max(1.0 - P^.W, 0.0); // don't touch alpha
    Inc(P);
  end;
end;

function TRGBAlphaFloatImage.GetColors(const X, Y, Z: Integer): TCastleColor;
var
  Pixel: PVector4;
begin
  Pixel := PixelPtr(X, Y, Z);
  Result := Pixel^;
end;

procedure TRGBAlphaFloatImage.SetColors(const X, Y, Z: Integer; const C: TCastleColor);
var
  Pixel: PVector4;
begin
  Pixel := PixelPtr(X, Y, Z);
  Pixel^ := C;
end;

class procedure TRGBAlphaFloatImage.MixColors(const OutputColor: Pointer;
  const Weights: TVector4; const AColors: TVector4Pointer);
var
  OutputCol: PVector4 absolute OutputColor;
  Cols: array [0..3] of PVector4 absolute AColors;
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
  OutputCol^.Z :=
    Weights.X * Cols[0]^.Z +
    Weights.Y * Cols[1]^.Z +
    Weights.Z * Cols[2]^.Z +
    Weights.W * Cols[3]^.Z;
  OutputCol^.W :=
    Weights.X * Cols[0]^.W +
    Weights.Y * Cols[1]^.W +
    Weights.Z * Cols[2]^.W +
    Weights.W * Cols[3]^.W;
end;

{$endif read_implementation}
