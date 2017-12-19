{
  Copyright 2001-2017 Michalis Kamburelis, Yevhen Loza.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

(* Paint extension for CastleImages unit and must be used with it. *)

unit CastlePaint;

{$mode objfpc}{$H+}

interface

uses
  CastleImages, CastleColors, CastleVectors;

//type
  //TCastleColor = TVector4;
  //TCastleColorRGB = TVector3;
{  TCastleColorGrayscaleAlpha = TVector2;
  TCastleColorGrayscale = single;}


type
  { "native" colors (as in pointers) }
  TCastleColor4Byte = TVector4Byte;
  TCastleColor2Byte = TVector2Byte;


type
  { Contains "Sorter" procedures which calls a proper native
    implementation of each routine.
    Warning: the operation is destructive, it will overwrite the current
    image content, if you want to preserve the initial image
    you will need to create copies manually.
    Warning: Color is treated differently depending on the image it is painted over.
    They are not abstract (as helper currently doesn't allow for virtual methods)
    so they cause a negligible slow-down when called run-time }
  TCastleImageHelper = class helper for TCastleImage
  strict private
    function CastleColorToCastleColor4Byte(aColor: TCastleColor): TCastleColor4Byte;
    function CastleColorToCastleColor2Byte(aColor: TCastleColor): TCastleColor2Byte;
  public
    { draws a hollow circle at x,y with aRadius radius with antialiasing. }
    {procedure Circle(const x, y: single; const aRadius: single;
      const aColor: TCastleColor);}

    { draws a filled circle at x,y with aRadius radius with antialiasing. }
    procedure FillCircle(const x, y: single; const aRadius: single;
      const aColor: TCastleColor);

    { same as Circle but much faster, without antialiasing }
    {procedure QuickCircle(const x, y: integer; const aRadius: integer;
      const aColor: TCastleColor);}

    { same as FillCircle but much faster, without antialiasing }
    procedure QuickFillCircle(const x, y: integer; const aRadius: integer;
      const aColor: TCastleColor);

    { draws a hollow rectangle at x1,y1 - x2,y2 with antialiasing.}
    {...}
  end;

type
  TRGBAlphaImageHelper = class helper for TRGBAlphaImage
  public
    procedure FillCircle(const x, y: single; const aRadius: single;
      const aColor: TCastleColor4Byte);
    procedure QuickFillCircle(const x, y: integer; const aRadius: integer;
      const aColor: TCastleColor4Byte);
  end;

type
  TRGBImageHelper = class helper for TRGBImage
  public
    procedure FillCircle(const x, y: single; const aRadius: single;
      const aColor: TCastleColor4Byte);
    procedure QuickFillCircle(const x, y: integer; const aRadius: integer;
      const aColor: TCastleColor4Byte);
  end;

type
  TGrayscaleAlphaImageHelper = class helper for TGrayscaleAlphaImage
  public
    procedure FillCircle(const x, y: single; const aRadius: single;
      const aColor: TCastleColor2Byte);
    procedure QuickFillCircle(const x, y: integer; const aRadius: integer;
      const aColor: TCastleColor2Byte);
  end;

type
  TGrayscaleImageHelper = class helper for TGrayscaleImage
  public
    procedure FillCircle(const x, y: single; const aRadius: single;
      const aColor: TCastleColor2Byte);
    procedure QuickFillCircle(const x, y: integer; const aRadius: integer;
      const aColor: TCastleColor2Byte);
  end;

type
  TRGBFloatImageHelper = class helper for TRGBFloatImage
  public
    procedure FillCircle(const x, y: single; const aRadius: single;
      const aColor: TCastleColor);
    procedure QuickFillCircle(const x, y: integer; const aRadius: integer;
      const aColor: TCastleColor);
  end;

implementation

{-----= Tools =-----}

function TCastleImageHelper.CastleColorToCastleColor4Byte(aColor: TCastleColor): TCastleColor4Byte;
begin
  Result[0] := Trunc(aColor[0]*255);
  Result[1] := Trunc(aColor[1]*255);
  Result[2] := Trunc(aColor[2]*255);
  Result[3] := Trunc(aColor[3]*255);
end;

function TCastleImageHelper.CastleColorToCastleColor2Byte(aColor: TCastleColor): TCastleColor2Byte;
begin
  Result[0] := GrayscaleValue(Vector3Byte(Vector3(aColor[0], aColor[1], aColor[2])));
  Result[1] := Trunc(aColor[3]*255);
end;

{-----= "Sorting procedures =-----}

procedure TCastleImageHelper.FillCircle(const x, y: single; const aRadius: single;
  const aColor: TCastleColor);
begin
  if Self is TRGBAlphaImage then
    TRGBAlphaImage(Self).FillCircle(x, y, aRadius, CastleColorToCastleColor4Byte(aColor))
  else
  if Self is TRGBImage then
    TRGBImage(Self).FillCircle(x, y, aRadius, CastleColorToCastleColor4Byte(aColor))
  else
  if Self is TGrayscaleAlphaImage then
    TGrayscaleAlphaImage(Self).FillCircle(x, y, aRadius, CastleColorToCastleColor2Byte(aColor))
  else
  if Self is TGrayscaleImage then
    TGrayscaleImage(Self).FillCircle(x, y, aRadius, CastleColorToCastleColor2Byte(aColor))
  else
  if Self is TRGBFloatImage then
    TRGBFloatImage(Self).FillCircle(x, y, aRadius, aColor)
  else
  raise EImageDrawError.CreateFmt('Painting is not supported for %s',
        [ClassName]);
end;

procedure TCastleImageHelper.QuickFillCircle(const x, y: integer; const aRadius: integer;
      const aColor: TCastleColor);
begin
  if Self is TRGBAlphaImage then
    TRGBAlphaImage(Self).QuickFillCircle(x, y, aRadius, CastleColorToCastleColor4Byte(aColor))
  else
  if Self is TRGBImage then
    TRGBImage(Self).QuickFillCircle(x, y, aRadius, CastleColorToCastleColor4Byte(aColor))
  else
  if Self is TGrayscaleAlphaImage then
    TGrayscaleAlphaImage(Self).QuickFillCircle(x, y, aRadius, CastleColorToCastleColor2Byte(aColor))
  else
  if Self is TGrayscaleImage then
    TGrayscaleImage(Self).QuickFillCircle(x, y, aRadius, CastleColorToCastleColor2Byte(aColor))
  else
  if Self is TRGBFloatImage then
    TRGBFloatImage(Self).QuickFillCircle(x, y, aRadius, aColor)
  else
  raise EImageDrawError.CreateFmt('Painting is not supported for %s',
        [ClassName]);
end;

{-----= FILL CIRCLE =-----}

procedure TRGBAlphaImageHelper.FillCircle(const x, y: single; const aRadius: single;
  const aColor: TCastleColor4Byte);
var
  p: PVector4Byte;
  ix, iy: integer;
  d: single;
  Alpha1, Alpha1d, Alpha2, Alpha2d, AlphaSum: single;
  SqrRadius, SqrY: single;
begin
  SqrRadius := Sqr(aRadius);
  Alpha2 := aColor[3] / 255;
  for iy := Round(y - aRadius) - 1 to Round(y + aRadius) + 1 do
    if (iy >= 0) and (iy < Height) then
    begin
      SqrY := Sqr(iy - y);
      p := nil;
      for ix := Round(x - aRadius) - 1 to Round(x + aRadius) + 1 do
        if (ix >= 0) and (ix < Width) then
        begin
          d := SqrRadius - (Sqr(ix - x) + SqrY);
          if d > -1 then
          begin
            if p = nil then p := PixelPtr(ix, iy) else Inc(p);

            Alpha1 := p^.Data[3] / 255;

            {antialiasing}
            if d < 1 then
              Alpha2d := Alpha2 * (d + 1) / 2 // as of conditions above d changes from -1 to +1
            else
              Alpha2d := Alpha2;

            Alpha1d := Alpha1 * (1 - Alpha2d);
            AlphaSum := Alpha1 + (1 - Alpha1) * Alpha2d;
            {$HINT I wonder how "premultiplied alpha" will behave here?}
            p^[0] := Round((p^.Data[0] * Alpha1d + aColor.Data[0] * Alpha2d) / AlphaSum);
            p^[1] := Round((p^.Data[1] * Alpha1d + aColor.Data[1] * Alpha2d) / AlphaSum);
            p^[2] := Round((p^.Data[2] * Alpha1d + aColor.Data[2] * Alpha2d) / AlphaSum);
            p^[3] := Round(255 * AlphaSum);
          end;
        end;
    end;
end;

procedure TRGBImageHelper.FillCircle(const x, y: single; const aRadius: single;
  const aColor: TCastleColor4Byte);
var
  p: PVector3Byte;
begin

end;

procedure TGrayscaleAlphaImageHelper.FillCircle(const x, y: single; const aRadius: single;
  const aColor: TCastleColor2Byte);
var
  p: PVector2Byte;
begin

end;

procedure TGrayscaleImageHelper.FillCircle(const x, y: single; const aRadius: single;
  const aColor: TCastleColor2Byte);
var
  p: PByte;
begin

end;

procedure TRGBFloatImageHelper.FillCircle(const x, y: single; const aRadius: single;
  const aColor: TCastleColor);
var
  p: PVector3;
begin

end;

{-----= QUICK CIRCLE =-----}

procedure TRGBAlphaImageHelper.QuickFillCircle(const x, y: integer; const aRadius: integer;
  const aColor: TCastleColor4Byte);
var
  p: PVector4Byte;
  ix, iy: integer;
  d: integer;
  SqrRadius, SqrY: integer;
begin
  SqrRadius := Sqr(aRadius);
  for iy := y - aRadius to y + aRadius do
    if (iy >= 0) and (iy < Height) then
    begin
      p := nil;
      SqrY := Sqr(iy - y);
      for ix := x - aRadius to x + aRadius do
        if (ix >= 0) and (ix < Width) then
        begin
          d := SqrRadius - (Sqr(ix - x) + SqrY);
          if d >= 0 then
          begin
            if p = nil then p := PixelPtr(ix, iy) else Inc(p);
            p^[0] := aColor.Data[0];
            p^[1] := aColor.Data[1];
            p^[2] := aColor.Data[2];
            p^[3] := aColor.Data[3];
          end;
        end;
    end;
end;

procedure TRGBImageHelper.QuickFillCircle(const x, y: integer; const aRadius: integer;
  const aColor: TCastleColor4Byte);
var
  p: PVector3Byte;
begin

end;

procedure TGrayscaleAlphaImageHelper.QuickFillCircle(const x, y: integer; const aRadius: integer;
  const aColor: TCastleColor2Byte);
var
  p: PVector2Byte;
begin

end;

procedure TGrayscaleImageHelper.QuickFillCircle(const x, y: integer; const aRadius: integer;
  const aColor: TCastleColor2Byte);
var
  p: PByte;
begin

end;

procedure TRGBFloatImageHelper.QuickFillCircle(const x, y: integer; const aRadius: integer;
  const aColor: TCastleColor);
var
  p: PVector3;
begin

end;

end.

