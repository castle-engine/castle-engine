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

(* Note: Circles drawing can be significantly improved by drawing the circles
   in quadrants (as 4 parts of the image are equal) - most calculations
   (including antialiasing) will have to be made 4 times less frequent.
   However, that's for future optimizations - actually those will work only
   for integer coordinates (i.e. "Quick" procedures) *)

(* Maybe, a good idea would be to make additional checks like aRadius>=1,
   aWidth >=1, etc. *)

(* I'm really unsure how premultiplied alpha will behave *)

{-----= CIRCLE =-----}

procedure TRGBAlphaImage.Circle(const x, y: single; const aRadius, aWidth: single;
  const aColor: TCastleColor);
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

procedure TRGBImage.Circle(const x, y: single; const aRadius, aWidth: single;
  const aColor: TCastleColor);
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

procedure TGrayscaleAlphaImage.Circle(const x, y: single; const aRadius, aWidth: single;
  const aColor: TCastleColor);
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

procedure TGrayscaleImage.Circle(const x, y: single; const aRadius, aWidth: single;
  const aColor: TCastleColor);
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

procedure TCastleImage.Circle(const x, y: single; const aRadius, aWidth: single;
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

procedure TRGBAlphaImage.FillCircle(const x, y: single; const aRadius: single;
  const aColor: TCastleColor);
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

procedure TRGBImage.FillCircle(const x, y: single; const aRadius: single;
  const aColor: TCastleColor);
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

procedure TGrayscaleAlphaImage.FillCircle(const x, y: single; const aRadius: single;
  const aColor: TCastleColor);
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

procedure TGrayscaleImage.FillCircle(const x, y: single; const aRadius: single;
  const aColor: TCastleColor);
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

procedure TCastleImage.FillCircle(const x, y: single; const aRadius: single;
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

procedure TRGBAlphaImage.QuickFillCircle(const x, y: integer; const aRadius: integer;
  const aColor: TCastleColor);
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

procedure TRGBImage.QuickFillCircle(const x, y: integer; const aRadius: integer;
  const aColor: TCastleColor);
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

procedure TGrayscaleAlphaImage.QuickFillCircle(const x, y: integer; const aRadius: integer;
  const aColor: TCastleColor);
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

procedure TGrayscaleImage.QuickFillCircle(const x, y: integer; const aRadius: integer;
  const aColor: TCastleColor);
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

procedure TCastleImage.QuickFillCircle(const x, y: integer; const aRadius: integer;
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

procedure TRGBAlphaImage.QuickCircle(const x, y: integer; const aRadius, aWidth: integer;
  const aColor: TCastleColor);
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

procedure TRGBImage.QuickCircle(const x, y: integer; const aRadius, aWidth: integer;
  const aColor: TCastleColor);
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

procedure TGrayscaleAlphaImage.QuickCircle(const x, y: integer; const aRadius, aWidth: integer;
  const aColor: TCastleColor);
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

procedure TGrayscaleImage.QuickCircle(const x, y: integer; const aRadius, aWidth: integer;
  const aColor: TCastleColor);
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

procedure TCastleImage.QuickCircle(const x, y: integer; const aRadius, aWidth: integer;
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

{-----= FILL ELLIPSE =-----}

procedure TRGBAlphaImage.FillEllipse(const x, y: single; const aRadiusX, aRadiusY: single;
  const aColor: TCastleColor);
var
  p: PVector4Byte;
  ix, iy: integer;
  d: single;
  Alpha1, Alpha1d, Alpha2, Alpha2d, AlphaSum: single;
  MeanRadius, SqrY: single;
begin
  MeanRadius := 1 / aRadiusX + 1 / aRadiusY; //this is simple, but wrong. But let's leave it for now
  Alpha2 := aColor[3] / 255;
  for iy := Round(y - aRadiusY) to Round(y + aRadiusY) do
    if (iy >= 0) and (iy < Height) then
    begin
      SqrY := 1 - Sqr((iy - y)/aRadiusY);
      p := nil;
      for ix := Round(x - aRadiusX) to Round(x + aRadiusX) do
        if (ix >= 0) and (ix < Width) then
        begin
          d := SqrY - Sqr((ix - x)/aRadiusX);
          if d >= 0 then
          begin
            if p = nil then p := PixelPtr(ix, iy) else Inc(p);

            Alpha1 := p^.Data[3] / 255;

            {antialiasing}
            if d < MeanRadius then
              { sqrt should be used here, however i like the result with square
                of distance better }
              Alpha2d := Alpha2 * d / MeanRadius  // as of conditions above d / DoubleRadius changes from 0 to 1
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

procedure TRGBImage.FillEllipse(const x, y: single; const aRadiusX, aRadiusY: single;
  const aColor: TCastleColor);
var
  p: PVector3Byte;
begin

end;

procedure TGrayscaleAlphaImage.FillEllipse(const x, y: single; const aRadiusX, aRadiusY: single;
  const aColor: TCastleColor);
var
  p: PVector2Byte;
begin

end;

procedure TGrayscaleImage.FillEllipse(const x, y: single; const aRadiusX, aRadiusY: single;
  const aColor: TCastleColor);
var
  p: PByte;
begin

end;

procedure TCastleImage.FillEllipse(const x, y: single; const aRadiusX, aRadiusY: single;
  const aColor: TCastleColor);
var
  p: PVector3;
begin

end;

{
procedure TRGBAlphaImage.FillEllipse(const x, y: single; const aRadiusX, aRadiusY: single;
  const aColor: TCastleColor);
var
  p: PVector4Byte;
begin

end;

procedure TRGBImage.FillEllipse(const x, y: single; const aRadiusX, aRadiusY: single;
  const aColor: TCastleColor);
var
  p: PVector3Byte;
begin

end;

procedure TGrayscaleAlphaImage.FillEllipse(const x, y: single; const aRadiusX, aRadiusY: single;
  const aColor: TCastleColor);
var
  p: PVector2Byte;
begin

end;

procedure TGrayscaleImage.FillEllipse(const x, y: single; const aRadiusX, aRadiusY: single;
  const aColor: TCastleColor);
var
  p: PByte;
begin

end;

procedure TRGBFloatImage.FillEllipse(const x, y: single; const aRadiusX, aRadiusY: single;
  const aColor: TCastleColor);
var
  p: PVector3;
begin

end;
}

end.

