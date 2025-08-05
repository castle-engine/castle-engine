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

  { Image with high-precision RGB colors encoded as 3 floating-point Single values. }
  TRGBFloatImage = class(TCastleImage)
  private
    function GetPixels: PVector3;
    function GetPixelsArray: PVector3Array;
    {$ifdef FPC}
    procedure FromFpImage(const FPImage: TInternalCastleFpImage); override;
    {$endif}
  protected
    function GetColors(const X, Y, Z: Integer): TCastleColor; override;
    procedure SetColors(const X, Y, Z: Integer; const C: TCastleColor); override;
  public
    { Pointer to pixels. Same as RawPixels, only typecasted to PVector3. }
    property Pixels: PVector3 read GetPixels;
    property RGBFloatPixels: PVector3 read GetPixels; {$ifdef FPC} deprecated 'use Pixels'; {$endif}
    { Pointer to pixels. Same as RawPixels, only typecasted to PVector3Array. }
    property PixelsArray: PVector3Array read GetPixelsArray;

    class function PixelSize: Cardinal; override;
    class function ColorComponentsCount: Cardinal; override;

    function PixelPtr(const X, Y: Cardinal; const Z: Cardinal = 0): PVector3;
    function RowPtr(const Y: Cardinal; const Z: Cardinal = 0): PVector3Array;

    procedure InvertColors; override;

    procedure Clear(const Pixel: TVector4Byte); overload; override;
    function IsClear(const Pixel: TVector4Byte): boolean; overload; override;

    procedure Clear(const Pixel: TVector3); reintroduce; overload;
    function IsClear(const Pixel: TVector3): boolean; reintroduce; overload;

    { Converts TRGBFloatImage to TRGBImage.
      Colors in pixels are simply rounded using @link(Vector3Byte).
      So such conversion not only kills the floating-point
      precision in float format but also clamps color components
      to 0..1. }
    function ToRGBImage: TRGBImage; deprecated 'create TRGBImage and use TRGBImage.Assign';
    {$ifdef FPC}
    function ToFpImage: TInternalCastleFpImage; override;
    {$endif}

    { Every component (red, green, blue) of every pixel
      is multiplied by Scale. }
    procedure ScaleColors(const Scale: Single);

    { Every component (red, green, blue) or every pixel
      is changed to Power(Value, Exp).
      So e.g. Exp = 1/2.2 gives commonly used gamma correction. }
    procedure ExpColors(const Exp: Single);

    procedure LerpWith(const Value: Single; SecondImage: TCastleImage); override;
    class procedure MixColors(const OutputColor: Pointer;
       const Weights: TVector4; const AColors: TVector4Pointer); override;
    procedure Assign(const Source: TCastleImage); override;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TRGBFloatImage ------------------------------------------------------------- }

function TRGBFloatImage.GetPixels: PVector3;
begin
  Result := PVector3(RawPixels);
end;

function TRGBFloatImage.GetPixelsArray: PVector3Array;
begin
  Result := PVector3Array(RawPixels);
end;

class function TRGBFloatImage.PixelSize: Cardinal;
begin
  Result := SizeOf(TVector3);
end;

class function TRGBFloatImage.ColorComponentsCount: Cardinal;
begin
  Result := 3;
end;

function TRGBFloatImage.PixelPtr(const X, Y, Z: Cardinal): PVector3;
begin
  Result := PVector3(inherited PixelPtr(X, Y, Z));
end;

function TRGBFloatImage.RowPtr(const Y, Z: Cardinal): PVector3Array;
begin
  Result := PVector3Array(inherited RowPtr(Y, Z));
end;

function TRGBFloatImage.GetColors(const X, Y, Z: Integer): TCastleColor;
var
  Pixel: PVector3;
begin
  Pixel := PixelPtr(X, Y, Z);
  Result.X := Pixel^.X;
  Result.Y := Pixel^.Y;
  Result.Z := Pixel^.Z;
  Result.W := 1.0;
end;

procedure TRGBFloatImage.SetColors(const X, Y, Z: Integer; const C: TCastleColor);
var
  Pixel: PVector3;
begin
  Pixel := PixelPtr(X, Y, Z);
  Pixel^.X := C.X;
  Pixel^.Y := C.Y;
  Pixel^.Z := C.Z;
end;

procedure TRGBFloatImage.Clear(const Pixel: TVector4Byte);
begin
  Clear(Vector3(
    Pixel.X * 255,
    Pixel.Y * 255,
    Pixel.Z * 255));
end;

function TRGBFloatImage.IsClear(const Pixel: TVector4Byte): boolean;
begin
  Result := IsClear(Vector3(
    Pixel.X * 255,
    Pixel.Y * 255,
    Pixel.Z * 255));
end;

procedure TRGBFloatImage.Clear(const Pixel: TVector3);
var
  P: PVector3;
  I: Cardinal;
begin
  P := Pixels;
  for I := 1 to Width * Height * Depth do
  begin
    Move(Pixel, P^, SizeOf(TVector3));
    Inc(P);
  end;
end;

function TRGBFloatImage.IsClear(const Pixel: TVector3): boolean;
var
  P: PVector3;
  I: Cardinal;
begin
  P := Pixels;
  for I := 1 to Width * Height * Depth do
  begin
    if not CompareMem(@Pixel, P, SizeOf(TVector3)) then
    begin
      Result := false;
      Exit;
    end;
    Inc(P);
  end;
  Result := true;
end;

function TRGBFloatImage.ToRGBImage: TRGBImage;
begin
  Result := TRGBImage.Create;
  Result.Assign(Self);
end;

procedure TRGBFloatImage.ScaleColors(const Scale: Single);
var
  pFloat: PVector3;
  i: Cardinal;
begin
  PFloat := Pixels;
  for i := 1 to Width * Height * Depth do
  begin
    PFloat^ := PFloat^ * Scale;
    Inc(PFloat);
  end;
end;

function VectorPowerComponents(const V: TVector3; const Exp: Single): TVector3;
begin
  Result.X := Power(V.X, Exp);
  Result.Y := Power(V.Y, Exp);
  Result.Z := Power(V.Z, Exp);
end;

procedure TRGBFloatImage.ExpColors(const Exp: Single);
var
  pFloat: PVector3;
  i: Cardinal;
begin
  PFloat := Pixels;
  for i := 1 to Width * Height * Depth do
  begin
    PFloat^ := VectorPowerComponents(PFloat^, Exp);
    Inc(PFloat);
  end;
end;

procedure TRGBFloatImage.LerpWith(const Value: Single; SecondImage: TCastleImage);
var
  SelfPtr: PVector3;
  SecondPtr: PVector3;
  I: Cardinal;
begin
  LerpSimpleCheckConditions(SecondImage);

  SelfPtr := Pixels;
  SecondPtr := TRGBFloatImage(SecondImage).Pixels;
  for I := 1 to Width * Height * Depth do
  begin
    SelfPtr^ := Lerp(Value, SelfPtr^, SecondPtr^);
    Inc(SelfPtr);
    Inc(SecondPtr);
  end;
end;

class procedure TRGBFloatImage.MixColors(const OutputColor: Pointer;
  const Weights: TVector4; const AColors: TVector4Pointer);
var
  OutputCol: PVector3 absolute OutputColor;
  Cols: array [0..3] of PVector3 absolute AColors;
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
end;

procedure TRGBFloatImage.InvertColors;
var
  I: Cardinal;
  P: PVector3;
begin
  P := Pixels;
  for I := 1 to Width * Height * Depth do
  begin
    P^.X := Max(1-P^.X, 0.0);
    P^.Y := Max(1-P^.Y, 0.0);
    P^.Z := Max(1-P^.Z, 0.0);
    Inc(P);
  end;
end;

{$endif read_implementation}