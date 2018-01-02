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

(* Paint simple geometric shapes for CastleImages. *)

(* Maybe, a good idea would be to make additional checks like aRadius>=1,
   aWidth >=1, etc. *)

(* I'm really unsure how premultiplied alpha will behave *)

{-----= FILL ELLIPSE =-----}

procedure TCastleImage.FillEllipse(const x, y: single; const aRadiusX, aRadiusY: single;
  const aColor: TCastleColor);
var
  NewColor, OldColor: TCastleColor;
  ix, iy: integer;
  Alpha1, Alpha1d, Alpha2, Alpha2d, AlphaSum: single;
  d, MeanRadius, SqrY: single;
begin
  MeanRadius := 1 / aRadiusX + 1 / aRadiusY; //this is simple, but wrong. But let's leave it for now
  Alpha2 := aColor.Data[3];
  for iy := Round(y - aRadiusY) to Round(y + aRadiusY) do
    if (iy >= 0) and (iy < Height) then
    begin
      SqrY := 1 - Sqr((iy - y) / aRadiusY);
      for ix := Round(x - aRadiusX) to Round(x + aRadiusX) do
        if (ix >= 0) and (ix < Width) then
        begin
          d := SqrY - Sqr((ix - x) / aRadiusX);
          if d >= 0 then
          begin
            OldColor := Colors[ix, iy, 0];
            Alpha1 := OldColor.Data[3];

            {antialiasing}
            if d < MeanRadius then
              { sqrt should be used here, however i like the result with square
                of distance better }
              Alpha2d := Alpha2 * d / MeanRadius  // as of conditions above d / DoubleRadius changes from 0 to 1
            else
              Alpha2d := Alpha2;

            Alpha1d := Alpha1 * (1 - Alpha2d);
            AlphaSum := Alpha1 + (1 - Alpha1) * Alpha2d;


            NewColor.Data[0] := (OldColor.Data[0] * Alpha1d + aColor.Data[0] * Alpha2d) / AlphaSum;
            NewColor.Data[1] := (OldColor.Data[1] * Alpha1d + aColor.Data[1] * Alpha2d) / AlphaSum;
            NewColor.Data[2] := (OldColor.Data[2] * Alpha1d + aColor.Data[2] * Alpha2d) / AlphaSum;
            NewColor.Data[3] := AlphaSum;

            Colors[ix, iy, 0] := NewColor;
          end;
        end;
    end;
end;

procedure TRGBAlphaImage.FillEllipse(const x, y: single; const aRadiusX, aRadiusY: single;
  const aColor: TCastleColor);
var
  p: PVector4Byte;
  ByteColor: TVector3Byte;
  ix, iy: integer;
  Alpha1, Alpha1d, Alpha2, Alpha2d, AlphaSum: single;
  d, MeanRadius, SqrY: single;
begin
  ByteColor.Data[0] := Trunc(255 * aColor.Data[0]);
  ByteColor.Data[1] := Trunc(255 * aColor.Data[1]);
  ByteColor.Data[2] := Trunc(255 * aColor.Data[2]);
  MeanRadius := 1 / aRadiusX + 1 / aRadiusY; //this is simple, but wrong. But let's leave it for now
  Alpha2 := aColor.Data[3];
  for iy := Round(y - aRadiusY) to Round(y + aRadiusY) do
    if (iy >= 0) and (iy < Height) then
    begin
      SqrY := 1 - Sqr((iy - y) / aRadiusY);
      p := nil;
      for ix := Round(x - aRadiusX) to Round(x + aRadiusX) do
        if (ix >= 0) and (ix < Width) then
        begin
          d := SqrY - Sqr((ix - x) / aRadiusX);
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
              p^.Data[0] := Round((p^.Data[0] * Alpha1d + ByteColor.Data[0] * Alpha2d) / AlphaSum);
              p^.Data[1] := Round((p^.Data[1] * Alpha1d + ByteColor.Data[1] * Alpha2d) / AlphaSum);
              p^.Data[2] := Round((p^.Data[2] * Alpha1d + ByteColor.Data[2] * Alpha2d) / AlphaSum);
            end;
            p^.Data[3] := Round(255 * AlphaSum);
          end;
        end;
    end;
end;

procedure TGrayscaleAlphaImage.FillEllipse(const x, y: single; const aRadiusX, aRadiusY: single;
  const aColor: TCastleColor);
var
  p: PVector2Byte;
  ByteColor: Byte;
  ix, iy: integer;
  Alpha1, Alpha1d, Alpha2, Alpha2d, AlphaSum: single;
  d, MeanRadius, SqrY: single;
begin
  ByteColor := GrayscaleValue(Vector3Byte(Vector3(aColor.Data[0], aColor.Data[1], aColor.Data[2])));
  MeanRadius := 1 / aRadiusX + 1 / aRadiusY; //this is simple, but wrong. But let's leave it for now
  Alpha2 := aColor.Data[3];
  for iy := Round(y - aRadiusY) to Round(y + aRadiusY) do
    if (iy >= 0) and (iy < Height) then
    begin
      SqrY := 1 - Sqr((iy - y) / aRadiusY);
      p := nil;
      for ix := Round(x - aRadiusX) to Round(x + aRadiusX) do
        if (ix >= 0) and (ix < Width) then
        begin
          d := SqrY - Sqr((ix - x) / aRadiusX);
          if d >= 0 then
          begin
            if p = nil then p := PixelPtr(ix, iy) else Inc(p);

            Alpha1 := p^.Data[1] / 255;

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
              p^.Data[0] := Round((p^.Data[0] * Alpha1d + ByteColor * Alpha2d) / AlphaSum);
            end;
            p^.Data[1] := Round(255 * AlphaSum);
          end;
        end;
    end;
end;

procedure TRGBImage.FillEllipse(const x, y: single; const aRadiusX, aRadiusY: single;
  const aColor: TCastleColor);
var
  p: PVector3Byte;
  ByteColor: TVector3Byte;
  ix, iy: integer;
  Alpha1d, Alpha2, Alpha2d: single;
  d, MeanRadius, SqrY: single;
begin
  ByteColor.Data[0] := Trunc(255 * aColor.Data[0]);
  ByteColor.Data[1] := Trunc(255 * aColor.Data[1]);
  ByteColor.Data[2] := Trunc(255 * aColor.Data[2]);
  MeanRadius := 1 / aRadiusX + 1 / aRadiusY; //this is simple, but wrong. But let's leave it for now
  Alpha2 := aColor.Data[3];
  for iy := Round(y - aRadiusY) to Round(y + aRadiusY) do
    if (iy >= 0) and (iy < Height) then
    begin
      SqrY := 1 - Sqr((iy - y) / aRadiusY);
      p := nil;
      for ix := Round(x - aRadiusX) to Round(x + aRadiusX) do
        if (ix >= 0) and (ix < Width) then
        begin
          d := SqrY - Sqr((ix - x) / aRadiusX);
          if d >= 0 then
          begin
            if p = nil then p := PixelPtr(ix, iy) else Inc(p);

            {antialiasing}
            if d < MeanRadius then
              { sqrt should be used here, however i like the result with square
                of distance better }
              Alpha2d := Alpha2 * d / MeanRadius  // as of conditions above d / DoubleRadius changes from 0 to 1
            else
              Alpha2d := Alpha2;

            Alpha1d := 1 - Alpha2d;
            p^.Data[0] := Round((p^.Data[0] * Alpha1d + ByteColor.Data[0] * Alpha2d));
            p^.Data[1] := Round((p^.Data[1] * Alpha1d + ByteColor.Data[1] * Alpha2d));
            p^.Data[2] := Round((p^.Data[2] * Alpha1d + ByteColor.Data[2] * Alpha2d));
          end;
        end;
    end;
end;

procedure TGrayscaleImage.FillEllipse(const x, y: single; const aRadiusX, aRadiusY: single;
  const aColor: TCastleColor);
var
  p: PByte;
  ByteColor: Byte;
  ix, iy: integer;
  Alpha1d, Alpha2, Alpha2d: single;
  d, MeanRadius, SqrY: single;
begin
  ByteColor := GrayscaleValue(Vector3Byte(Vector3(aColor.Data[0], aColor.Data[1], aColor.Data[2])));
  MeanRadius := 1 / aRadiusX + 1 / aRadiusY; //this is simple, but wrong. But let's leave it for now
  Alpha2 := aColor.Data[3];
  for iy := Round(y - aRadiusY) to Round(y + aRadiusY) do
    if (iy >= 0) and (iy < Height) then
    begin
      SqrY := 1 - Sqr((iy - y) / aRadiusY);
      p := nil;
      for ix := Round(x - aRadiusX) to Round(x + aRadiusX) do
        if (ix >= 0) and (ix < Width) then
        begin
          d := SqrY - Sqr((ix - x) / aRadiusX);
          if d >= 0 then
          begin
            if p = nil then p := PixelPtr(ix, iy) else Inc(p);

            {antialiasing}
            if d < MeanRadius then
              { sqrt should be used here, however i like the result with square
                of distance better }
              Alpha2d := Alpha2 * d / MeanRadius  // as of conditions above d / DoubleRadius changes from 0 to 1
            else
              Alpha2d := Alpha2;

            Alpha1d := 1 - Alpha2d;
            p^ := Round((p^ * Alpha1d + ByteColor * Alpha2d));
          end;
        end;
    end;
end;

{-----= HOLLOW ELLIPSE =-----}

procedure TCastleImage.Ellipse(const x, y: single; const aRadiusX, aRadiusY: single;
  const aWidth: single; const aColor: TCastleColor);
var
  NewColor, OldColor: TCastleColor;
  ix, iy: integer;
  Alpha1, Alpha1d, Alpha2, Alpha2d, AlphaSum: single;
  d1, d2, MeanRadius1, MeanRadius2, SqrY1, SqrY2: single;
begin
  MeanRadius1 := 1 / aRadiusX + 1 / aRadiusY;
  MeanRadius2 := 1 / (aRadiusX - aWidth) + 1 / (aRadiusY - aWidth);
  Alpha2 := aColor.Data[3];
  for iy := Round(y - aRadiusY) to Round(y + aRadiusY) do
    if (iy >= 0) and (iy < Height) then
    begin
      SqrY1 := 1 - Sqr((iy - y) / aRadiusY);
      SqrY2 := 1 - Sqr((iy - y) / (aRadiusY - aWidth));
      for ix := Round(x - aRadiusX) to Round(x + aRadiusX) do
        if (ix >= 0) and (ix < Width) then
        begin
          d1 := SqrY1 - Sqr((ix - x) / (aRadiusX));
          d2 := SqrY2 - Sqr((ix - x) / (aRadiusX - aWidth));
          if (d1 >= 0) and (d2 <= MeanRadius2) then
          begin
            OldColor := Colors[ix, iy, 0];
            Alpha1 := OldColor.Data[3];

            {antialiasing}
            if d1 < MeanRadius1 then
              Alpha2d := Alpha2 * d1 / MeanRadius1
            else
              Alpha2d := Alpha2;
            if d2 > 0 then
              Alpha2d := Alpha2d * (1 - d2 / MeanRadius2);

            Alpha1d := Alpha1 * (1 - Alpha2d);
            AlphaSum := Alpha1 + (1 - Alpha1) * Alpha2d;

            NewColor.Data[0] := (OldColor.Data[0] * Alpha1d + aColor.Data[0] * Alpha2d) / AlphaSum;
            NewColor.Data[1] := (OldColor.Data[1] * Alpha1d + aColor.Data[1] * Alpha2d) / AlphaSum;
            NewColor.Data[2] := (OldColor.Data[2] * Alpha1d + aColor.Data[2] * Alpha2d) / AlphaSum;
            NewColor.Data[3] := AlphaSum;

            Colors[ix, iy, 0] := NewColor;
          end;
        end;
    end;
end;

procedure TRGBAlphaImage.Ellipse(const x, y: single; const aRadiusX, aRadiusY: single;
  const aWidth: single; const aColor: TCastleColor);
var
  p: PVector4Byte;
  ByteColor: TVector3Byte;
  ix, iy: integer;
  Alpha1, Alpha1d, Alpha2, Alpha2d, AlphaSum: single;
  d1, d2, MeanRadius1, MeanRadius2, SqrY1, SqrY2: single;
begin
  ByteColor.Data[0] := Trunc(255 * aColor.Data[0]);
  ByteColor.Data[1] := Trunc(255 * aColor.Data[1]);
  ByteColor.Data[2] := Trunc(255 * aColor.Data[2]);
  MeanRadius1 := 1 / aRadiusX + 1 / aRadiusY;
  MeanRadius2 := 1 / (aRadiusX - aWidth) + 1 / (aRadiusY - aWidth);
  Alpha2 := aColor.Data[3];
{  for iy := Round(y - aRadiusY) to Round(y + aRadiusY) do
    if (iy >= 0) and (iy < Height) then
    begin
      SqrY := 1 - Sqr((iy - y) / aRadiusY);
      p := nil;
      for ix := Round(x - aRadiusX) to Round(x + aRadiusX) do
        if (ix >= 0) and (ix < Width) then
        begin
          d := SqrY - Sqr((ix - x) / aRadiusX);
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
              p^.Data[0] := Round((p^.Data[0] * Alpha1d + ByteColor.Data[0] * Alpha2d) / AlphaSum);
              p^.Data[1] := Round((p^.Data[1] * Alpha1d + ByteColor.Data[1] * Alpha2d) / AlphaSum);
              p^.Data[2] := Round((p^.Data[2] * Alpha1d + ByteColor.Data[2] * Alpha2d) / AlphaSum);
            end;
            p^.Data[3] := Round(255 * AlphaSum);
          end;
        end;
    end;}
end;

procedure TGrayscaleAlphaImage.Ellipse(const x, y: single; const aRadiusX, aRadiusY: single;
  const aWidth: single; const aColor: TCastleColor);
var
  p: PVector2Byte;
  ByteColor: Byte;
  ix, iy: integer;
  Alpha1, Alpha1d, Alpha2, Alpha2d, AlphaSum: single;
  d1, d2, MeanRadius1, MeanRadius2, SqrY1, SqrY2: single;
begin
  ByteColor := GrayscaleValue(Vector3Byte(Vector3(aColor.Data[0], aColor.Data[1], aColor.Data[2])));
  MeanRadius1 := 1 / aRadiusX + 1 / aRadiusY;
  MeanRadius2 := 1 / (aRadiusX - aWidth) + 1 / (aRadiusY - aWidth);
  Alpha2 := aColor.Data[3];
{  for iy := Round(y - aRadiusY) to Round(y + aRadiusY) do
    if (iy >= 0) and (iy < Height) then
    begin
      SqrY := 1 - Sqr((iy - y) / aRadiusY);
      p := nil;
      for ix := Round(x - aRadiusX) to Round(x + aRadiusX) do
        if (ix >= 0) and (ix < Width) then
        begin
          d := SqrY - Sqr((ix - x) / aRadiusX);
          if d >= 0 then
          begin
            if p = nil then p := PixelPtr(ix, iy) else Inc(p);

            Alpha1 := p^.Data[1] / 255;

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
              p^.Data[0] := Round((p^.Data[0] * Alpha1d + ByteColor * Alpha2d) / AlphaSum);
            end;
            p^.Data[1] := Round(255 * AlphaSum);
          end;
        end;
    end;}
end;

procedure TRGBImage.Ellipse(const x, y: single; const aRadiusX, aRadiusY: single;
  const aWidth: single; const aColor: TCastleColor);
var
  p: PVector3Byte;
  ByteColor: TVector3Byte;
  ix, iy: integer;
  Alpha1d, Alpha2, Alpha2d: single;
  d1, d2, MeanRadius1, MeanRadius2, SqrY1, SqrY2: single;
begin
  ByteColor.Data[0] := Trunc(255 * aColor.Data[0]);
  ByteColor.Data[1] := Trunc(255 * aColor.Data[1]);
  ByteColor.Data[2] := Trunc(255 * aColor.Data[2]);
  MeanRadius1 := 1 / aRadiusX + 1 / aRadiusY;
  MeanRadius2 := 1 / (aRadiusX - aWidth) + 1 / (aRadiusY - aWidth);
  Alpha2 := aColor.Data[3];
{  for iy := Round(y - aRadiusY) to Round(y + aRadiusY) do
    if (iy >= 0) and (iy < Height) then
    begin
      SqrY := 1 - Sqr((iy - y) / aRadiusY);
      p := nil;
      for ix := Round(x - aRadiusX) to Round(x + aRadiusX) do
        if (ix >= 0) and (ix < Width) then
        begin
          d := SqrY - Sqr((ix - x) / aRadiusX);
          if d >= 0 then
          begin
            if p = nil then p := PixelPtr(ix, iy) else Inc(p);

            {antialiasing}
            if d < MeanRadius then
              { sqrt should be used here, however i like the result with square
                of distance better }
              Alpha2d := Alpha2 * d / MeanRadius  // as of conditions above d / DoubleRadius changes from 0 to 1
            else
              Alpha2d := Alpha2;

            Alpha1d := 1 - Alpha2d;
            p^.Data[0] := Round((p^.Data[0] * Alpha1d + ByteColor.Data[0] * Alpha2d));
            p^.Data[1] := Round((p^.Data[1] * Alpha1d + ByteColor.Data[1] * Alpha2d));
            p^.Data[2] := Round((p^.Data[2] * Alpha1d + ByteColor.Data[2] * Alpha2d));
          end;
        end;
    end;}
end;

procedure TGrayscaleImage.Ellipse(const x, y: single; const aRadiusX, aRadiusY: single;
  const aWidth: single; const aColor: TCastleColor);
var
  p: PByte;
  ByteColor: Byte;
  ix, iy: integer;
  Alpha1d, Alpha2, Alpha2d: single;
  d1, d2, MeanRadius1, MeanRadius2, SqrY1, SqrY2: single;
begin
  ByteColor := GrayscaleValue(Vector3Byte(Vector3(aColor.Data[0], aColor.Data[1], aColor.Data[2])));
  MeanRadius1 := 1 / aRadiusX + 1 / aRadiusY;
  MeanRadius2 := 1 / (aRadiusX - aWidth) + 1 / (aRadiusY - aWidth);
  Alpha2 := aColor.Data[3];
{  for iy := Round(y - aRadiusY) to Round(y + aRadiusY) do
    if (iy >= 0) and (iy < Height) then
    begin
      SqrY := 1 - Sqr((iy - y) / aRadiusY);
      p := nil;
      for ix := Round(x - aRadiusX) to Round(x + aRadiusX) do
        if (ix >= 0) and (ix < Width) then
        begin
          d := SqrY - Sqr((ix - x) / aRadiusX);
          if d >= 0 then
          begin
            if p = nil then p := PixelPtr(ix, iy) else Inc(p);

            {antialiasing}
            if d < MeanRadius then
              { sqrt should be used here, however i like the result with square
                of distance better }
              Alpha2d := Alpha2 * d / MeanRadius  // as of conditions above d / DoubleRadius changes from 0 to 1
            else
              Alpha2d := Alpha2;

            Alpha1d := 1 - Alpha2d;
            p^ := Round((p^ * Alpha1d + ByteColor * Alpha2d));
          end;
        end;
    end; }
end;
