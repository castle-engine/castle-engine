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
  tmpColor: TCastleColor;
  ix, iy: integer;
  d: single;
  Alpha1, Alpha1d, Alpha2, Alpha2d, AlphaSum: single;
  MeanRadius, SqrY: single;
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
            Alpha1 := Colors[ix, iy, 0].Data[3];

            {antialiasing}
            if d < MeanRadius then
              { sqrt should be used here, however i like the result with square
                of distance better }
              Alpha2d := Alpha2 * d / MeanRadius  // as of conditions above d / DoubleRadius changes from 0 to 1
            else
              Alpha2d := Alpha2;

            Alpha1d := Alpha1 * (1 - Alpha2d);
            AlphaSum := Alpha1 + (1 - Alpha1) * Alpha2d;


            TmpColor.Data[0] := (Colors[ix, iy, 0].Data[0] * Alpha1d + aColor.Data[0] * Alpha2d) / AlphaSum;
            TmpColor.Data[1] := (Colors[ix, iy, 0].Data[1] * Alpha1d + aColor.Data[1] * Alpha2d) / AlphaSum;
            TmpColor.Data[2] := (Colors[ix, iy, 0].Data[2] * Alpha1d + aColor.Data[2] * Alpha2d) / AlphaSum;
            TmpColor.Data[3] := AlphaSum;

            Colors[ix, iy, 0] := TmpColor;
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
  d: single;
  Alpha1, Alpha1d, Alpha2, Alpha2d, AlphaSum: single;
  MeanRadius, SqrY: single;
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
  d: single;
  Alpha1, Alpha1d, Alpha2, Alpha2d, AlphaSum: single;
  MeanRadius, SqrY: single;
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
  d: single;
  Alpha1d, Alpha2, Alpha2d: single;
  MeanRadius, SqrY: single;
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
  d: single;
  Alpha1d, Alpha2, Alpha2d: single;
  MeanRadius, SqrY: single;
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

