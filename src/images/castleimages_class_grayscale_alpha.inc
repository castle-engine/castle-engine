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

  { Grayscale image with an alpha channel.
    Each pixel is two bytes: grayscale + alpha. }
  TGrayscaleAlphaImage = class(TCastleImage)
  private
    function GetPixels: PVector2Byte;
    function GetPixelsArray: PVector2ByteArray;
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
    { Pointer to pixels. Same as RawPixels, only typecasted to PVector2Byte. }
    property Pixels: PVector2Byte read GetPixels;
    property GrayscaleAlphaPixels: PVector2Byte read GetPixels; {$ifdef FPC}deprecated 'use Pixels'; {$endif}
    { Pointer to pixels. Same as RawPixels, only typecasted to PVector2ByteArray. }
    property PixelsArray: PVector2ByteArray read GetPixelsArray;

    class function PixelSize: Cardinal; override;
    class function ColorComponentsCount: Cardinal; override;

    { Remove alpha channel. }
    function ToGrayscaleImage: TGrayscaleImage; deprecated 'create TGrayscaleImage and use TGrayscaleImage.Assign';

    function PixelPtr(const X, Y: Cardinal; const Z: Cardinal = 0): PVector2Byte;
    function RowPtr(const Y: Cardinal; const Z: Cardinal = 0): PVector2ByteArray;

    procedure InvertColors; override;

    procedure Clear(const Pixel: TVector4Byte); override;
    function IsClear(const Pixel: TVector4Byte): boolean; override;

    procedure Clear(const Pixel: TVector2Byte); reintroduce; overload;
    function IsClear(const Pixel: TVector2Byte): boolean; reintroduce; overload;

    function HasAlpha: boolean; override;

    function AlphaChannel(
      const AlphaTolerance: Byte): TAlphaChannel; override;

    {$ifdef FPC}
    function ToFpImage: TInternalCastleFpImage; override;
    {$endif}

    procedure LerpWith(const Value: Single; SecondImage: TCastleImage); override;
    class procedure MixColors(const OutputColor: Pointer;
       const Weights: TVector4; const AColors: TVector4Pointer); override;

    procedure Assign(const Source: TCastleImage); override;

    // TODO: this should be implemented, just like for TRGBAlphaImage
    //procedure AlphaBleed(const ProgressTitle: string = ''); override;
    //function MakeAlphaBleed(const ProgressTitle: string = ''): TCastleImage; override;
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

{ TGrayscaleAlphaImage ------------------------------------------------------------ }

function TGrayscaleAlphaImage.GetPixels: PVector2Byte;
begin
  Result := PVector2Byte(RawPixels);
end;

function TGrayscaleAlphaImage.GetPixelsArray: PVector2ByteArray;
begin
  Result := PVector2ByteArray(RawPixels);
end;

class function TGrayscaleAlphaImage.PixelSize: Cardinal;
begin
  Result := 2;
end;

class function TGrayscaleAlphaImage.ColorComponentsCount: Cardinal;
begin
  Result := 2;
end;

function TGrayscaleAlphaImage.PixelPtr(const X, Y, Z: Cardinal): PVector2Byte;
begin
  Result := PVector2Byte(inherited PixelPtr(X, Y, Z));
end;

function TGrayscaleAlphaImage.RowPtr(const Y, Z: Cardinal): PVector2ByteArray;
begin
  Result := PVector2ByteArray(inherited RowPtr(Y, Z));
end;

procedure TGrayscaleAlphaImage.Clear(const Pixel: TVector4Byte);
begin
  Clear(Vector2Byte(GrayscaleValue(Pixel), Pixel.W));
end;

function TGrayscaleAlphaImage.IsClear(const Pixel: TVector4Byte): boolean;
begin
  Result := IsClear(Vector2Byte(GrayscaleValue(Pixel), Pixel.W));
end;

procedure TGrayscaleAlphaImage.Clear(const Pixel: TVector2Byte);
var
  P: PVector2Byte;
  I: Cardinal;
begin
  P := Pixels;
  for I := 1 to Width * Height * Depth do
  begin
    Move(Pixel, P^, SizeOf(Pixel));
    Inc(P);
  end;
end;

function TGrayscaleAlphaImage.IsClear(const Pixel: TVector2Byte): boolean;
var
  P: PVector2Byte;
  I: Cardinal;
begin
  P := Pixels;
  for I := 1 to Width * Height * Depth do
  begin
    if not CompareMem(@Pixel, P, SizeOf(Pixel)) then
    begin
      Result := false;
      Exit;
    end;
    Inc(P);
  end;
  Result := true;
end;

function TGrayscaleAlphaImage.HasAlpha: boolean;
begin
  Result := true;
end;

function TGrayscaleAlphaImage.AlphaChannel(
  const AlphaTolerance: Byte): TAlphaChannel;
var
  PtrAlpha: PVector2Byte;
  I: Cardinal;
begin
  PtrAlpha := Pixels;

  for I := 1 to Width * Height * Depth do
  begin
    if (PtrAlpha^.Y > AlphaTolerance) and
       (PtrAlpha^.Y < 255 - AlphaTolerance) then
      Exit(acBlending);
    Inc(PtrAlpha);
  end;

  Result := acTest;
end;

procedure TGrayscaleAlphaImage.LerpWith(const Value: Single; SecondImage: TCastleImage);
var
  SelfPtr: PVector2Byte;
  SecondPtr: PVector2Byte;
  I: Cardinal;
begin
  LerpSimpleCheckConditions(SecondImage);

  SelfPtr := Pixels;
  SecondPtr := TGrayscaleAlphaImage(SecondImage).Pixels;
  for I := 1 to Width * Height * Depth do
  begin
    SelfPtr^ := Lerp(Value, SelfPtr^, SecondPtr^);
    Inc(SelfPtr);
    Inc(SecondPtr);
  end;
end;

class procedure TGrayscaleAlphaImage.MixColors(const OutputColor: Pointer;
  const Weights: TVector4; const AColors: TVector4Pointer);
var
  OutputCol: PVector2Byte absolute OutputColor;
  Cols: array [0..3] of PVector2Byte absolute AColors;
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
  {$I norqcheckend.inc}
end;

procedure TGrayscaleAlphaImage.InvertColors;
var
  I: Cardinal;
  P: PVector2Byte;
begin
  P := Pixels;
  for I := 1 to Width * Height * Depth do
  begin
    P^.X := High(Byte) - P^.X;
    Inc(P);
  end;
end;

function TGrayscaleAlphaImage.GetColors(const X, Y, Z: Integer): TCastleColor;
var
  Pixel: PVector2Byte;
begin
  Pixel := PixelPtr(X, Y, Z);
  Result.X := Pixel^.X / 255;
  Result.Y := Pixel^.X / 255;
  Result.Z := Pixel^.X / 255;
  Result.W := Pixel^.Y / 255;
end;

procedure TGrayscaleAlphaImage.SetColors(const X, Y, Z: Integer; const C: TCastleColor);
var
  Pixel: PVector2Byte;
begin
  Pixel := PixelPtr(X, Y, Z);
  Pixel^.X := Clamped(Round(GrayscaleValue(C) * 255), Low(Byte), High(Byte));
  Pixel^.Y := Clamped(Round(C.W               * 255), Low(Byte), High(Byte));
end;

function TGrayscaleAlphaImage.ToGrayscaleImage: TGrayscaleImage;
begin
  Result := TGrayscaleImage.Create;
  Result.Assign(Self);
end;

{$endif read_implementation}