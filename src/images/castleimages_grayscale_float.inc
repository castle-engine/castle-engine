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

  { Image with Single (floating-point number) for each pixel.
    Ideal for high-precision height maps.
    Compared to TGrayscaleImage, each pixel is a floating-point number,
    which means it is more precise and can also be above 1.0 value
    (though they are still in 0..1 range when reading from normal image formats). }
  TGrayscaleFloatImage = class(TCastleImage)
  private
    function GetPixels: PSingle;
    function GetPixelsArray: PSingleArray;
  protected
    function GetColors(const X, Y, Z: Integer): TCastleColor; override;
    procedure SetColors(const X, Y, Z: Integer; const C: TCastleColor); override;
  public
    class function PixelSize: Cardinal; override;
    class function ColorComponentsCount: Cardinal; override;
    procedure Assign(const Source: TCastleImage); override;
    procedure InvertColors; override;

    { Pointer to pixels. Same as RawPixels, only typecasted to PSingle. }
    property Pixels: PSingle read GetPixels;

    { Pointer to pixels. Same as RawPixels, only typecasted to PSingleArray. }
    property PixelsArray: PSingleArray read GetPixelsArray;

    { Pointer to given pixel. }
    function PixelPtr(const X, Y: Cardinal; const Z: Cardinal = 0): PSingle;

    { Pointer to given row. }
    function RowPtr(const Y: Cardinal; const Z: Cardinal = 0): PSingleArray;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TGrayscaleFloatImage ------------------------------------------------------------ }

function TGrayscaleFloatImage.GetPixels: PSingle;
begin
  Result := PSingle(RawPixels);
end;

function TGrayscaleFloatImage.GetPixelsArray: PSingleArray;
begin
  Result := PSingleArray(RawPixels);
end;

class function TGrayscaleFloatImage.PixelSize: Cardinal;
begin
  Result := SizeOf(Single);
end;

class function TGrayscaleFloatImage.ColorComponentsCount: Cardinal;
begin
  Result := 1;
end;

function TGrayscaleFloatImage.PixelPtr(const X, Y, Z: Cardinal): PSingle;
begin
  Result := PSingle(inherited PixelPtr(X, Y, Z));
end;

function TGrayscaleFloatImage.RowPtr(const Y, Z: Cardinal): PSingleArray;
begin
  Result := PSingleArray(inherited RowPtr(Y, Z));
end;

procedure TGrayscaleFloatImage.InvertColors;
var
  I: Cardinal;
  P: PSingle;
begin
  P := Pixels;
  for I := 1 to Width * Height * Depth do
  begin
    P^ := 1.0 - P^;
    Inc(P);
  end;
end;

function TGrayscaleFloatImage.GetColors(const X, Y, Z: Integer): TCastleColor;
var
  Pixel: PSingle;
begin
  Pixel := PixelPtr(X, Y, Z);
  Result.X := Pixel^;
  Result.Y := Pixel^;
  Result.Z := Pixel^;
  Result.W := 1.0;
end;

procedure TGrayscaleFloatImage.SetColors(const X, Y, Z: Integer; const C: TCastleColor);
var
  Pixel: PSingle;
begin
  Pixel := PixelPtr(X, Y, Z);
  Pixel^ := GrayscaleValue(C);
end;

{$endif read_implementation}
