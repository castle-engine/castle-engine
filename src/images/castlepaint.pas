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
{$R+}{$Q+}

interface

uses
  CastleImages, CastleColors, CastleVectors;

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
    { Draws a hollow circle at x,y with aRadius radius with antialiasing.
      Be aware, that aWidth is not accurately scaled in pixels due to optimizations. }
    procedure Circle(const x, y: single; const aRadius, aWidth: single;
      const aColor: TCastleColor);

    { Draws a filled circle at x,y with aRadius radius with antialiasing. }
    procedure FillCircle(const x, y: single; const aRadius: single;
      const aColor: TCastleColor);

    { Same as Circle but much faster, without antialiasing }
    procedure QuickCircle(const x, y: integer; const aRadius, aWidth: integer;
      const aColor: TCastleColor);

    { Same as FillCircle but much faster, without antialiasing }
    procedure QuickFillCircle(const x, y: integer; const aRadius: integer;
      const aColor: TCastleColor);

    { Draws a hollow rectangle at x1,y1 - x2,y2 with antialiasing.}
    {...}
  end;

type
  TRGBAlphaImageHelper = class helper for TRGBAlphaImage
  public
    procedure Circle(const x, y: single; const aRadius, aWidth: single;
      const aColor: TCastleColor4Byte);
    procedure FillCircle(const x, y: single; const aRadius: single;
      const aColor: TCastleColor4Byte);
    procedure QuickCircle(const x, y: integer; const aRadius, aWidth: integer;
      const aColor: TCastleColor4Byte);
    procedure QuickFillCircle(const x, y: integer; const aRadius: integer;
      const aColor: TCastleColor4Byte);
  end;

type
  TRGBImageHelper = class helper for TRGBImage
  public
    procedure Circle(const x, y: single; const aRadius, aWidth: single;
      const aColor: TCastleColor4Byte);
    procedure FillCircle(const x, y: single; const aRadius: single;
      const aColor: TCastleColor4Byte);
    procedure QuickCircle(const x, y: integer; const aRadius, aWidth: integer;
      const aColor: TCastleColor4Byte);
    procedure QuickFillCircle(const x, y: integer; const aRadius: integer;
      const aColor: TCastleColor4Byte);
  end;

type
  TGrayscaleAlphaImageHelper = class helper for TGrayscaleAlphaImage
  public
    procedure Circle(const x, y: single; const aRadius, aWidth: single;
      const aColor: TCastleColor2Byte);
    procedure FillCircle(const x, y: single; const aRadius: single;
      const aColor: TCastleColor2Byte);
    procedure QuickCircle(const x, y: integer; const aRadius, aWidth: integer;
      const aColor: TCastleColor2Byte);
    procedure QuickFillCircle(const x, y: integer; const aRadius: integer;
      const aColor: TCastleColor2Byte);
  end;

type
  TGrayscaleImageHelper = class helper for TGrayscaleImage
  public
    procedure Circle(const x, y: single; const aRadius, aWidth: single;
      const aColor: TCastleColor2Byte);
    procedure FillCircle(const x, y: single; const aRadius: single;
      const aColor: TCastleColor2Byte);
    procedure QuickCircle(const x, y: integer; const aRadius, aWidth: integer;
      const aColor: TCastleColor2Byte);
    procedure QuickFillCircle(const x, y: integer; const aRadius: integer;
      const aColor: TCastleColor2Byte);
  end;

type
  TRGBFloatImageHelper = class helper for TRGBFloatImage
  public
    procedure Circle(const x, y: single; const aRadius, aWidth: single;
      const aColor: TCastleColor);
    procedure FillCircle(const x, y: single; const aRadius: single;
      const aColor: TCastleColor);
    procedure QuickCircle(const x, y: integer; const aRadius, aWidth: integer;
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

procedure TCastleImageHelper.Circle(const x, y: single; const aRadius, aWidth: single;
  const aColor: TCastleColor);
begin
  if Self is TRGBAlphaImage then
    TRGBAlphaImage(Self).Circle(x, y, aRadius, aWidth, CastleColorToCastleColor4Byte(aColor))
  else
  if Self is TRGBImage then
    TRGBImage(Self).Circle(x, y, aRadius, aWidth, CastleColorToCastleColor4Byte(aColor))
  else
  if Self is TGrayscaleAlphaImage then
    TGrayscaleAlphaImage(Self).Circle(x, y, aRadius, aWidth, CastleColorToCastleColor2Byte(aColor))
  else
  if Self is TGrayscaleImage then
    TGrayscaleImage(Self).Circle(x, y, aRadius, aWidth, CastleColorToCastleColor2Byte(aColor))
  else
  if Self is TRGBFloatImage then
    TRGBFloatImage(Self).Circle(x, y, aRadius, aWidth, aColor)
  else
  raise EImageDrawError.CreateFmt('Painting is not supported for %s',
        [ClassName]);
end;

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

procedure TCastleImageHelper.QuickCircle(const x, y: integer; const aRadius, aWidth: integer;
      const aColor: TCastleColor);
begin
  if Self is TRGBAlphaImage then
    TRGBAlphaImage(Self).QuickCircle(x, y, aRadius, aWidth, CastleColorToCastleColor4Byte(aColor))
  else
  if Self is TRGBImage then
    TRGBImage(Self).QuickCircle(x, y, aRadius, aWidth, CastleColorToCastleColor4Byte(aColor))
  else
  if Self is TGrayscaleAlphaImage then
    TGrayscaleAlphaImage(Self).QuickCircle(x, y, aRadius, aWidth, CastleColorToCastleColor2Byte(aColor))
  else
  if Self is TGrayscaleImage then
    TGrayscaleImage(Self).QuickCircle(x, y, aRadius, aWidth, CastleColorToCastleColor2Byte(aColor))
  else
  if Self is TRGBFloatImage then
    TRGBFloatImage(Self).QuickCircle(x, y, aRadius, aWidth, aColor)
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

(* Note: Circles drawing can be significantly improved by drawing the circles
   in quadrants (as 4 parts of the image are equal) - most calculations
   (including antialiasing) will have to be made 4 times less frequent.
   However, that's for future optimizations - actually those will work only
   for integer coordinates (i.e. QuickXxxxx procedures) *)

(* Maybe, a good idea would be to make additional checks like aRadius>=1,
   aWidth >=1, etc. *)

(* Caution. Due to usage of a non-exact formula the aWidth is interpreted in
   a wrong way (the error is the larger, the larger aWidth is). I'm not sure
   if we need accuracy here, so I'll leave it this way for now. Maybe it will
   need to be rewritten later *)

(* I'm really unsure how premultiplied alpha will behave *)

{-----= CIRCLE =-----}

procedure TRGBAlphaImageHelper.Circle(const x, y: single; const aRadius, aWidth: single;
  const aColor: TCastleColor4Byte);
var
  p: PVector4Byte;
  ix, iy: integer;
  d1, d2: single;
  Alpha1, Alpha1d, Alpha2, Alpha2d, AlphaSum: single;
  SqrRadius1, SqrRadius2, DoubleRadius1, DoubleRadius2, SqrY1, SqrY2, SqrY, SqrX: single;
begin
  DoubleRadius1 := 2 * (aRadius);
  DoubleRadius2 := 2 * (aRadius - aWidth);
  SqrRadius1 := Sqr(aRadius);
  SqrRadius2 := Sqr(aRadius - aWidth);
  Alpha2 := aColor.Data[3] / 255;
  for iy := Round(y - aRadius) to Round(y + aRadius) do
    if (iy >= 0) and (iy < Height) then
    begin
      SqrY := Sqr(iy - y);
      SqrY1 := SqrRadius1 - SqrY;
      SqrY2 := SqrRadius2 - Sqr(iy - y);
      p := nil;
      for ix := Round(x - aRadius) to Round(x + aRadius) do
        if (ix >= 0) and (ix < Width) then
        begin
          SqrX := Sqr(ix - x);
          d1 := SqrY1 - SqrX;
          d2 := SqrY2 - SqrX;
          if p = nil then p := PixelPtr(ix, iy) else Inc(p);
          if (d1 >= 0) and (d2 <= DoubleRadius2) then
          begin

            Alpha1 := p^.Data[3] / 255;

            {antialiasing}
            { sqrt should be used here, however i like the result with square
              of distance better }
            if d1 < DoubleRadius1 then
              Alpha2d := Alpha2 * d1 / DoubleRadius1  // as of conditions above d / DoubleRadius changes from 0 to 1
            else
              Alpha2d := Alpha2;
            if d2 > 0 then
              Alpha2d := Alpha2d * (1 - d2 / DoubleRadius2);  // as of conditions above d / DoubleRadius changes from 0 to 1

            Alpha1d := Alpha1 * (1 - Alpha2d);
            AlphaSum := Alpha1 + (1 - Alpha1) * Alpha2d;
            if AlphaSum > 0 then begin
              p^.Data[0] := Round((p^.Data[0] * Alpha1d + aColor.Data[0] * Alpha2d) / AlphaSum);
              p^.Data[1] := Round((p^.Data[1] * Alpha1d + aColor.Data[1] * Alpha2d) / AlphaSum);
              p^.Data[2] := Round((p^.Data[2] * Alpha1d + aColor.Data[2] * Alpha2d) / AlphaSum);
            end;
            p^.Data[3] := Round(255 * AlphaSum);
          end;
        end;
    end;
end;

procedure TRGBImageHelper.Circle(const x, y: single; const aRadius, aWidth: single;
  const aColor: TCastleColor4Byte);
var
  p: PVector3Byte;
  ix, iy: integer;
  d1, d2: single;
  Alpha1d, Alpha2, Alpha2d: single;
  SqrRadius1, SqrRadius2, DoubleRadius1, DoubleRadius2, SqrY1, SqrY2, SqrY, SqrX: single;
begin
  DoubleRadius1 := 2 * (aRadius);
  DoubleRadius2 := 2 * (aRadius - aWidth);
  SqrRadius1 := Sqr(aRadius);
  SqrRadius2 := Sqr(aRadius - aWidth);
  Alpha2 := aColor.Data[3] / 255;
  for iy := Round(y - aRadius) to Round(y + aRadius) do
    if (iy >= 0) and (iy < Height) then
    begin
      SqrY := Sqr(iy - y);
      SqrY1 := SqrRadius1 - SqrY;
      SqrY2 := SqrRadius2 - Sqr(iy - y);
      p := nil;
      for ix := Round(x - aRadius) to Round(x + aRadius) do
        if (ix >= 0) and (ix < Width) then
        begin
          SqrX := Sqr(ix - x);
          d1 := SqrY1 - SqrX;
          d2 := SqrY2 - SqrX;
          if p = nil then p := PixelPtr(ix, iy) else Inc(p);
          if (d1 >= 0) and (d2 <= DoubleRadius2) then
          begin

            {antialiasing}
            { sqrt should be used here, however i like the result with square
              of distance better }
            if d1 < DoubleRadius1 then
              Alpha2d := Alpha2 * d1 / DoubleRadius1  // as of conditions above d / DoubleRadius changes from 0 to 1
            else
              Alpha2d := Alpha2;
            if d2 > 0 then
              Alpha2d := Alpha2d * (1 - d2 / DoubleRadius2);  // as of conditions above d / DoubleRadius changes from 0 to 1

            Alpha1d := (1 - Alpha2d);
            p^.Data[0] := Round(p^.Data[0] * Alpha1d + aColor.Data[0] * Alpha2d);
            p^.Data[1] := Round(p^.Data[1] * Alpha1d + aColor.Data[1] * Alpha2d);
            p^.Data[2] := Round(p^.Data[2] * Alpha1d + aColor.Data[2] * Alpha2d);
          end;
        end;
    end;
end;

procedure TGrayscaleAlphaImageHelper.Circle(const x, y: single; const aRadius, aWidth: single;
  const aColor: TCastleColor2Byte);
var
  p: PVector2Byte;
  ix, iy: integer;
  d1, d2: single;
  Alpha1, Alpha1d, Alpha2, Alpha2d, AlphaSum: single;
  SqrRadius1, SqrRadius2, DoubleRadius1, DoubleRadius2, SqrY1, SqrY2, SqrY, SqrX: single;
begin
  DoubleRadius1 := 2 * (aRadius);
  DoubleRadius2 := 2 * (aRadius - aWidth);
  SqrRadius1 := Sqr(aRadius);
  SqrRadius2 := Sqr(aRadius - aWidth);
  Alpha2 := aColor.Data[1] / 255;
  for iy := Round(y - aRadius) to Round(y + aRadius) do
    if (iy >= 0) and (iy < Height) then
    begin
      SqrY := Sqr(iy - y);
      SqrY1 := SqrRadius1 - SqrY;
      SqrY2 := SqrRadius2 - Sqr(iy - y);
      p := nil;
      for ix := Round(x - aRadius) to Round(x + aRadius) do
        if (ix >= 0) and (ix < Width) then
        begin
          SqrX := Sqr(ix - x);
          d1 := SqrY1 - SqrX;
          d2 := SqrY2 - SqrX;
          if p = nil then p := PixelPtr(ix, iy) else Inc(p);
          if (d1 >= 0) and (d2 <= DoubleRadius2) then
          begin

            Alpha1 := p^.Data[1] / 255;

            {antialiasing}
            { sqrt should be used here, however i like the result with square
              of distance better }
            if d1 < DoubleRadius1 then
              Alpha2d := Alpha2 * d1 / DoubleRadius1  // as of conditions above d / DoubleRadius changes from 0 to 1
            else
              Alpha2d := Alpha2;
            if d2 > 0 then
              Alpha2d := Alpha2d * (1 - d2 / DoubleRadius2);  // as of conditions above d / DoubleRadius changes from 0 to 1

            Alpha1d := Alpha1 * (1 - Alpha2d);
            AlphaSum := Alpha1 + (1 - Alpha1) * Alpha2d;
            if AlphaSum > 0 then
              p^.Data[0] := Round((p^.Data[0] * Alpha1d + aColor.Data[0] * Alpha2d) / AlphaSum);
            p^.Data[1] := Round(255 * AlphaSum);
          end;
        end;
    end;
end;

procedure TGrayscaleImageHelper.Circle(const x, y: single; const aRadius, aWidth: single;
  const aColor: TCastleColor2Byte);
var
  p: PByte;
  ix, iy: integer;
  d1, d2: single;
  Alpha1d, Alpha2, Alpha2d: single;
  SqrRadius1, SqrRadius2, DoubleRadius1, DoubleRadius2, SqrY1, SqrY2, SqrY, SqrX: single;
begin
  DoubleRadius1 := 2 * (aRadius);
  DoubleRadius2 := 2 * (aRadius - aWidth);
  SqrRadius1 := Sqr(aRadius);
  SqrRadius2 := Sqr(aRadius - aWidth);
  Alpha2 := aColor.Data[1] / 255;
  for iy := Round(y - aRadius) to Round(y + aRadius) do
    if (iy >= 0) and (iy < Height) then
    begin
      SqrY := Sqr(iy - y);
      SqrY1 := SqrRadius1 - SqrY;
      SqrY2 := SqrRadius2 - Sqr(iy - y);
      p := nil;
      for ix := Round(x - aRadius) to Round(x + aRadius) do
        if (ix >= 0) and (ix < Width) then
        begin
          SqrX := Sqr(ix - x);
          d1 := SqrY1 - SqrX;
          d2 := SqrY2 - SqrX;
          if p = nil then p := PixelPtr(ix, iy) else Inc(p);
          if (d1 >= 0) and (d2 <= DoubleRadius2) then
          begin

            {antialiasing}
            { sqrt should be used here, however i like the result with square
              of distance better }
            if d1 < DoubleRadius1 then
              Alpha2d := Alpha2 * d1 / DoubleRadius1  // as of conditions above d / DoubleRadius changes from 0 to 1
            else
              Alpha2d := Alpha2;
            if d2 > 0 then
              Alpha2d := Alpha2d * (1 - d2 / DoubleRadius2);  // as of conditions above d / DoubleRadius changes from 0 to 1

            Alpha1d := (1 - Alpha2d);
            p^ := Round(p^ * Alpha1d + aColor.Data[0] * Alpha2d);
          end;
        end;
    end;
end;

procedure TRGBFloatImageHelper.Circle(const x, y: single; const aRadius, aWidth: single;
  const aColor: TCastleColor);
var
  p: PVector3;
  ix, iy: integer;
  d1, d2: single;
  Alpha1d, Alpha2, Alpha2d: single;
  SqrRadius1, SqrRadius2, DoubleRadius1, DoubleRadius2, SqrY1, SqrY2, SqrY, SqrX: single;
begin
  DoubleRadius1 := 2 * (aRadius);
  DoubleRadius2 := 2 * (aRadius - aWidth);
  SqrRadius1 := Sqr(aRadius);
  SqrRadius2 := Sqr(aRadius - aWidth);
  Alpha2 := aColor.Data[3];
  for iy := Round(y - aRadius) to Round(y + aRadius) do
    if (iy >= 0) and (iy < Height) then
    begin
      SqrY := Sqr(iy - y);
      SqrY1 := SqrRadius1 - SqrY;
      SqrY2 := SqrRadius2 - Sqr(iy - y);
      p := nil;
      for ix := Round(x - aRadius) to Round(x + aRadius) do
        if (ix >= 0) and (ix < Width) then
        begin
          SqrX := Sqr(ix - x);
          d1 := SqrY1 - SqrX;
          d2 := SqrY2 - SqrX;
          if p = nil then p := PixelPtr(ix, iy) else Inc(p);
          if (d1 >= 0) and (d2 <= DoubleRadius2) then
          begin

            {antialiasing}
            { sqrt should be used here, however i like the result with square
              of distance better }
            if d1 < DoubleRadius1 then
              Alpha2d := Alpha2 * d1 / DoubleRadius1  // as of conditions above d / DoubleRadius changes from 0 to 1
            else
              Alpha2d := Alpha2;
            if d2 > 0 then
              Alpha2d := Alpha2d * (1 - d2 / DoubleRadius2);  // as of conditions above d / DoubleRadius changes from 0 to 1

            Alpha1d := (1 - Alpha2d);
            p^.Data[0] := p^.Data[0] * Alpha1d + aColor.Data[0] * Alpha2d;
            p^.Data[1] := p^.Data[1] * Alpha1d + aColor.Data[1] * Alpha2d;
            p^.Data[2] := p^.Data[2] * Alpha1d + aColor.Data[2] * Alpha2d;
          end;
        end;
    end;
end;

{-----= FILL CIRCLE =-----}

procedure TRGBAlphaImageHelper.FillCircle(const x, y: single; const aRadius: single;
  const aColor: TCastleColor4Byte);
var
  p: PVector4Byte;
  ix, iy: integer;
  d: single;
  Alpha1, Alpha1d, Alpha2, Alpha2d, AlphaSum: single;
  SqrRadius, DoubleRadius, SqrY: single;
begin
  SqrRadius := Sqr(aRadius);
  DoubleRadius := 2 * aRadius;
  Alpha2 := aColor[3] / 255;
  for iy := Round(y - aRadius) to Round(y + aRadius) do
    if (iy >= 0) and (iy < Height) then
    begin
      SqrY := SqrRadius - Sqr(iy - y);
      p := nil;
      for ix := Round(x - aRadius) to Round(x + aRadius) do
        if (ix >= 0) and (ix < Width) then
        begin
          d := SqrY - Sqr(ix - x);
          if d >= 0 then
          begin
            if p = nil then p := PixelPtr(ix, iy) else Inc(p);

            Alpha1 := p^.Data[3] / 255;

            {antialiasing}
            if d < DoubleRadius then
              { sqrt should be used here, however i like the result with square
                of distance better }
              Alpha2d := Alpha2 * d / DoubleRadius  // as of conditions above d / DoubleRadius changes from 0 to 1
            else
              Alpha2d := Alpha2;

            Alpha1d := Alpha1 * (1 - Alpha2d);
            AlphaSum := Alpha1 + (1 - Alpha1) * Alpha2d;
            if AlphaSum > 0 then begin
              p^.Data[0] := Round((p^.Data[0] * Alpha1d + aColor.Data[0] * Alpha2d) / AlphaSum);
              p^.Data[1] := Round((p^.Data[1] * Alpha1d + aColor.Data[1] * Alpha2d) / AlphaSum);
              p^.Data[2] := Round((p^.Data[2] * Alpha1d + aColor.Data[2] * Alpha2d) / AlphaSum);
            end;
            p^.Data[3] := Round(255 * AlphaSum);
          end;
        end;
    end;
end;

procedure TRGBImageHelper.FillCircle(const x, y: single; const aRadius: single;
  const aColor: TCastleColor4Byte);
var
  p: PVector3Byte;
  ix, iy: integer;
  d: single;
  Alpha1d, Alpha2, Alpha2d: single;
  SqrRadius, DoubleRadius, SqrY: single;
begin
  SqrRadius := Sqr(aRadius);
  DoubleRadius := 2 * aRadius;
  Alpha2 := aColor[3] / 255;
  for iy := Round(y - aRadius) to Round(y + aRadius) do
    if (iy >= 0) and (iy < Height) then
    begin
      SqrY := SqrRadius - Sqr(iy - y);
      p := nil;
      for ix := Round(x - aRadius) to Round(x + aRadius) do
        if (ix >= 0) and (ix < Width) then
        begin
          d := SqrY - Sqr(ix - x);
          if d >= 0 then
          begin
            if p = nil then p := PixelPtr(ix, iy) else Inc(p);

            {antialiasing}
            if d < DoubleRadius then
              { sqrt should be used here, however i like the result with square
                of distance better }
              Alpha2d := Alpha2 * d / DoubleRadius  // as of conditions above d / DoubleRadius changes from 0 to 1
            else
              Alpha2d := Alpha2;

            Alpha1d := 1 - Alpha2d;
            p^.Data[0] := Round((p^.Data[0] * Alpha1d + aColor.Data[0] * Alpha2d));
            p^.Data[1] := Round((p^.Data[1] * Alpha1d + aColor.Data[1] * Alpha2d));
            p^.Data[2] := Round((p^.Data[2] * Alpha1d + aColor.Data[2] * Alpha2d));
          end;
        end;
    end;
end;

procedure TGrayscaleAlphaImageHelper.FillCircle(const x, y: single; const aRadius: single;
  const aColor: TCastleColor2Byte);
var
  p: PVector2Byte;
  ix, iy: integer;
  d: single;
  Alpha1, Alpha1d, Alpha2, Alpha2d, AlphaSum: single;
  SqrRadius, DoubleRadius, SqrY: single;
begin
  SqrRadius := Sqr(aRadius);
  DoubleRadius := 2 * aRadius;
  Alpha2 := aColor[1] / 255;
  for iy := Round(y - aRadius) to Round(y + aRadius) do
    if (iy >= 0) and (iy < Height) then
    begin
      SqrY := SqrRadius - Sqr(iy - y);
      p := nil;
      for ix := Round(x - aRadius) to Round(x + aRadius) do
        if (ix >= 0) and (ix < Width) then
        begin
          d := SqrY - Sqr(ix - x);
          if d >= 0 then
          begin
            if p = nil then p := PixelPtr(ix, iy) else Inc(p);

            Alpha1 := p^.Data[1] / 255;

            {antialiasing}
            if d < DoubleRadius then
              { sqrt should be used here, however i like the result with square
                of distance better }
              Alpha2d := Alpha2 * d / DoubleRadius  // as of conditions above d / DoubleRadius changes from 0 to 1
            else
              Alpha2d := Alpha2;

            Alpha1d := Alpha1 * (1 - Alpha2d);
            AlphaSum := Alpha1 + (1 - Alpha1) * Alpha2d;
            if AlphaSum > 0 then
              p^.Data[0] := Round((p^.Data[0] * Alpha1d + aColor.Data[0] * Alpha2d) / AlphaSum);
            p^.Data[1] := Round(255 * AlphaSum);
          end;
        end;
    end;
end;

procedure TGrayscaleImageHelper.FillCircle(const x, y: single; const aRadius: single;
  const aColor: TCastleColor2Byte);
var
  p: PByte;
  ix, iy: integer;
  d: single;
  Alpha1d, Alpha2, Alpha2d: single;
  SqrRadius, DoubleRadius, SqrY: single;
begin
  SqrRadius := Sqr(aRadius);
  DoubleRadius := 2 * aRadius;
  Alpha2 := aColor[1] / 255;
  for iy := Round(y - aRadius) to Round(y + aRadius) do
    if (iy >= 0) and (iy < Height) then
    begin
      SqrY := SqrRadius - Sqr(iy - y);
      p := nil;
      for ix := Round(x - aRadius) to Round(x + aRadius) do
        if (ix >= 0) and (ix < Width) then
        begin
          d := SqrY - Sqr(ix - x);
          if d >= 0 then
          begin
            if p = nil then p := PixelPtr(ix, iy) else Inc(p);

            {antialiasing}
            if d < DoubleRadius then
              { sqrt should be used here, however i like the result with square
                of distance better }
              Alpha2d := Alpha2 * d / DoubleRadius  // as of conditions above d / DoubleRadius changes from 0 to 1
            else
              Alpha2d := Alpha2;

            Alpha1d := 1 - Alpha2d;
            p^ := Round((p^ * Alpha1d + aColor.Data[0] * Alpha2d));
          end;
        end;
    end;

end;

procedure TRGBFloatImageHelper.FillCircle(const x, y: single; const aRadius: single;
  const aColor: TCastleColor);
var
  p: PVector3;
  ix, iy: integer;
  d: single;
  Alpha1d, Alpha2, Alpha2d: single;
  SqrRadius, DoubleRadius, SqrY: single;
begin
  SqrRadius := Sqr(aRadius);
  DoubleRadius := 2 * aRadius;
  Alpha2 := aColor[3];
  for iy := Round(y - aRadius) to Round(y + aRadius) do
    if (iy >= 0) and (iy < Height) then
    begin
      SqrY := SqrRadius - Sqr(iy - y);
      p := nil;
      for ix := Round(x - aRadius) to Round(x + aRadius) do
        if (ix >= 0) and (ix < Width) then
        begin
          d := SqrY - Sqr(ix - x);
          if d >= 0 then
          begin
            if p = nil then p := PixelPtr(ix, iy) else Inc(p);

            {antialiasing}
            if d < DoubleRadius then
              { sqrt should be used here, however i like the result with square
                of distance better }
              Alpha2d := Alpha2 * d / DoubleRadius  // as of conditions above d / DoubleRadius changes from 0 to 1
            else
              Alpha2d := Alpha2;

            Alpha1d := 1 - Alpha2d;
            p^.Data[0] := (p^.Data[0] * Alpha1d + aColor.Data[0] * Alpha2d);
            p^.Data[1] := (p^.Data[1] * Alpha1d + aColor.Data[1] * Alpha2d);
            p^.Data[2] := (p^.Data[2] * Alpha1d + aColor.Data[2] * Alpha2d);
          end;
        end;
    end;

end;

{-----= QUICK FILL CIRCLE =-----}

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
      SqrY := SqrRadius - Sqr(iy - y);
      for ix := x - aRadius to x + aRadius do
        if (ix >= 0) and (ix < Width) then
        begin
          d := SqrY - Sqr(ix - x);
          if d >= 0 then
          begin
            if p = nil then p := PixelPtr(ix, iy) else Inc(p);
            p^.Data[0] := aColor.Data[0];
            p^.Data[1] := aColor.Data[1];
            p^.Data[2] := aColor.Data[2];
            p^.Data[3] := aColor.Data[3];
          end;
        end;
    end;
end;

procedure TRGBImageHelper.QuickFillCircle(const x, y: integer; const aRadius: integer;
  const aColor: TCastleColor4Byte);
var
  p: PVector3Byte;
  ix, iy: integer;
  d: integer;
  SqrRadius, SqrY: integer;
begin
  SqrRadius := Sqr(aRadius);
  for iy := y - aRadius to y + aRadius do
    if (iy >= 0) and (iy < Height) then
    begin
      p := nil;
      SqrY := SqrRadius - Sqr(iy - y);
      for ix := x - aRadius to x + aRadius do
        if (ix >= 0) and (ix < Width) then
        begin
          d := SqrY - Sqr(ix - x);
          if d >= 0 then
          begin
            if p = nil then p := PixelPtr(ix, iy) else Inc(p);
            p^.Data[0] := aColor.Data[0];
            p^.Data[1] := aColor.Data[1];
            p^.Data[2] := aColor.Data[2];
          end;
        end;
    end;

end;

procedure TGrayscaleAlphaImageHelper.QuickFillCircle(const x, y: integer; const aRadius: integer;
  const aColor: TCastleColor2Byte);
var
  p: PVector2Byte;
  ix, iy: integer;
  d: integer;
  SqrRadius, SqrY: integer;
begin
  SqrRadius := Sqr(aRadius);
  for iy := y - aRadius to y + aRadius do
    if (iy >= 0) and (iy < Height) then
    begin
      p := nil;
      SqrY := SqrRadius - Sqr(iy - y);
      for ix := x - aRadius to x + aRadius do
        if (ix >= 0) and (ix < Width) then
        begin
          d := SqrY - Sqr(ix - x);
          if d >= 0 then
          begin
            if p = nil then p := PixelPtr(ix, iy) else Inc(p);
            p^.Data[0] := aColor.Data[0];
            p^.Data[1] := aColor.Data[1];
          end;
        end;
    end;
end;

procedure TGrayscaleImageHelper.QuickFillCircle(const x, y: integer; const aRadius: integer;
  const aColor: TCastleColor2Byte);
var
  p: PByte;
  ix, iy: integer;
  d: integer;
  SqrRadius, SqrY: integer;
begin
  SqrRadius := Sqr(aRadius);
  for iy := y - aRadius to y + aRadius do
    if (iy >= 0) and (iy < Height) then
    begin
      p := nil;
      SqrY := SqrRadius - Sqr(iy - y);
      for ix := x - aRadius to x + aRadius do
        if (ix >= 0) and (ix < Width) then
        begin
          d := SqrY - Sqr(ix - x);
          if d >= 0 then
          begin
            if p = nil then p := PixelPtr(ix, iy) else Inc(p);
            p^ := aColor.Data[0];
          end;
        end;
    end;
end;

procedure TRGBFloatImageHelper.QuickFillCircle(const x, y: integer; const aRadius: integer;
  const aColor: TCastleColor);
var
  p: PVector3;
  ix, iy: integer;
  d: integer;
  SqrRadius, SqrY: integer;
begin
  SqrRadius := Sqr(aRadius);
  for iy := y - aRadius to y + aRadius do
    if (iy >= 0) and (iy < Height) then
    begin
      p := nil;
      SqrY := SqrRadius - Sqr(iy - y);
      for ix := x - aRadius to x + aRadius do
        if (ix >= 0) and (ix < Width) then
        begin
          d := SqrY - Sqr(ix - x);
          if d >= 0 then
          begin
            if p = nil then p := PixelPtr(ix, iy) else Inc(p);
            p^.Data[0] := aColor.Data[0];
            p^.Data[1] := aColor.Data[1];
            p^.Data[2] := aColor.Data[2];
          end;
        end;
    end;

end;

{-----= QUICK CIRCLE =-----}

procedure TRGBAlphaImageHelper.QuickCircle(const x, y: integer; const aRadius, aWidth: integer;
  const aColor: TCastleColor4Byte);
var
  p: PVector4Byte;
  ix, iy: integer;
  d1, d2: integer;
  SqrRadius1, SqrRadius2, SqrY1, SqrY2, SqrY, SqrX: integer;
begin
  SqrRadius1 := Sqr(aRadius);
  SqrRadius2 := Sqr(aRadius - aWidth);
  for iy := y - aRadius to y + aRadius do
    if (iy >= 0) and (iy < Height) then
    begin
      p := nil;
      SqrY := Sqr(iy - y);
      SqrY1 := SqrRadius1 - SqrY;
      SqrY2 := SqrRadius2 - SqrY;
      for ix := x - aRadius to x + aRadius do
        if (ix >= 0) and (ix < Width) then
        begin
          SqrX := Sqr(ix - x);
          d1 := SqrY1 - SqrX;
          d2 := SqrY2 - SqrX;
          if p = nil then p := PixelPtr(ix, iy) else Inc(p);
          if (d1 >= 0) and (d2 <= 0) then
          begin
            p^.Data[0] := aColor.Data[0];
            p^.Data[1] := aColor.Data[1];
            p^.Data[2] := aColor.Data[2];
            p^.Data[3] := aColor.Data[3];
          end;
        end;
    end;
end;

procedure TRGBImageHelper.QuickCircle(const x, y: integer; const aRadius, aWidth: integer;
  const aColor: TCastleColor4Byte);
var
  p: PVector3Byte;
  ix, iy: integer;
  d1, d2: integer;
  SqrRadius1, SqrRadius2, SqrY1, SqrY2, SqrY, SqrX: integer;
begin
  SqrRadius1 := Sqr(aRadius);
  SqrRadius2 := Sqr(aRadius - aWidth);
  for iy := y - aRadius to y + aRadius do
    if (iy >= 0) and (iy < Height) then
    begin
      p := nil;
      SqrY := Sqr(iy - y);
      SqrY1 := SqrRadius1 - SqrY;
      SqrY2 := SqrRadius2 - SqrY;
      for ix := x - aRadius to x + aRadius do
        if (ix >= 0) and (ix < Width) then
        begin
          SqrX := Sqr(ix - x);
          d1 := SqrY1 - SqrX;
          d2 := SqrY2 - SqrX;
          if p = nil then p := PixelPtr(ix, iy) else Inc(p);
          if (d1 >= 0) and (d2 <= 0) then
          begin
            p^.Data[0] := aColor.Data[0];
            p^.Data[1] := aColor.Data[1];
            p^.Data[2] := aColor.Data[2];
          end;
        end;
    end;

end;

procedure TGrayscaleAlphaImageHelper.QuickCircle(const x, y: integer; const aRadius, aWidth: integer;
  const aColor: TCastleColor2Byte);
var
  p: PVector2Byte;
  ix, iy: integer;
  d1, d2: integer;
  SqrRadius1, SqrRadius2, SqrY1, SqrY2, SqrY, SqrX: integer;
begin
  SqrRadius1 := Sqr(aRadius);
  SqrRadius2 := Sqr(aRadius - aWidth);
  for iy := y - aRadius to y + aRadius do
    if (iy >= 0) and (iy < Height) then
    begin
      p := nil;
      SqrY := Sqr(iy - y);
      SqrY1 := SqrRadius1 - SqrY;
      SqrY2 := SqrRadius2 - SqrY;
      for ix := x - aRadius to x + aRadius do
        if (ix >= 0) and (ix < Width) then
        begin
          SqrX := Sqr(ix - x);
          d1 := SqrY1 - SqrX;
          d2 := SqrY2 - SqrX;
          if p = nil then p := PixelPtr(ix, iy) else Inc(p);
          if (d1 >= 0) and (d2 <= 0) then
          begin
            p^.Data[0] := aColor.Data[0];
            p^.Data[1] := aColor.Data[1];
          end;
        end;
    end;
end;

procedure TGrayscaleImageHelper.QuickCircle(const x, y: integer; const aRadius, aWidth: integer;
  const aColor: TCastleColor2Byte);
var
  p: PByte;
  ix, iy: integer;
  d1, d2: integer;
  SqrRadius1, SqrRadius2, SqrY1, SqrY2, SqrY, SqrX: integer;
begin
  SqrRadius1 := Sqr(aRadius);
  SqrRadius2 := Sqr(aRadius - aWidth);
  for iy := y - aRadius to y + aRadius do
    if (iy >= 0) and (iy < Height) then
    begin
      p := nil;
      SqrY := Sqr(iy - y);
      SqrY1 := SqrRadius1 - SqrY;
      SqrY2 := SqrRadius2 - SqrY;
      for ix := x - aRadius to x + aRadius do
        if (ix >= 0) and (ix < Width) then
        begin
          SqrX := Sqr(ix - x);
          d1 := SqrY1 - SqrX;
          d2 := SqrY2 - SqrX;
          if p = nil then p := PixelPtr(ix, iy) else Inc(p);
          if (d1 >= 0) and (d2 <= 0) then
          begin
            p^ := aColor.Data[0];
          end;
        end;
    end;
end;

procedure TRGBFloatImageHelper.QuickCircle(const x, y: integer; const aRadius, aWidth: integer;
  const aColor: TCastleColor);
var
  p: PVector3;
  ix, iy: integer;
  d1, d2: integer;
  SqrRadius1, SqrRadius2, SqrY1, SqrY2, SqrY, SqrX: integer;
begin
  SqrRadius1 := Sqr(aRadius);
  SqrRadius2 := Sqr(aRadius - aWidth);
  for iy := y - aRadius to y + aRadius do
    if (iy >= 0) and (iy < Height) then
    begin
      p := nil;
      SqrY := Sqr(iy - y);
      SqrY1 := SqrRadius1 - SqrY;
      SqrY2 := SqrRadius2 - SqrY;
      for ix := x - aRadius to x + aRadius do
        if (ix >= 0) and (ix < Width) then
        begin
          SqrX := Sqr(ix - x);
          d1 := SqrY1 - SqrX;
          d2 := SqrY2 - SqrX;
          if p = nil then p := PixelPtr(ix, iy) else Inc(p);
          if (d1 >= 0) and (d2 <= 0) then
          begin
            p^.Data[0] := aColor.Data[0];
            p^.Data[1] := aColor.Data[1];
            p^.Data[2] := aColor.Data[2];
          end;
        end;
    end;

end;

end.

