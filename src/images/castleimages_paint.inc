{%MainUnit castleimages.pas}
{
  Copyright 2001-2022 Michalis Kamburelis, Yevhen Loza.

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
  Alpha2 := aColor.W;
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
            Alpha1 := OldColor.W;

            {antialiasing}
            if d < MeanRadius then
              { sqrt should be used here, however i like the result with square
                of distance better }
              Alpha2d := Alpha2 * d / MeanRadius  // as of conditions above d / DoubleRadius changes from 0 to 1
            else
              Alpha2d := Alpha2;

            Alpha1d := Alpha1 * (1 - Alpha2d);
            AlphaSum := Alpha1 + (1 - Alpha1) * Alpha2d;


            NewColor.X := (OldColor.X * Alpha1d + aColor.X * Alpha2d) / AlphaSum;
            NewColor.Y := (OldColor.Y * Alpha1d + aColor.Y * Alpha2d) / AlphaSum;
            NewColor.Z := (OldColor.Z * Alpha1d + aColor.Z * Alpha2d) / AlphaSum;
            NewColor.W := AlphaSum;

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
  ByteColor.X := Trunc(255 * aColor.X);
  ByteColor.Y := Trunc(255 * aColor.Y);
  ByteColor.Z := Trunc(255 * aColor.Z);
  MeanRadius := 1 / aRadiusX + 1 / aRadiusY; //this is simple, but wrong. But let's leave it for now
  Alpha2 := aColor.W;
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

            Alpha1 := p^.W / 255;

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
              p^.X := Round((p^.X * Alpha1d + ByteColor.X * Alpha2d) / AlphaSum);
              p^.Y := Round((p^.Y * Alpha1d + ByteColor.Y * Alpha2d) / AlphaSum);
              p^.Z := Round((p^.Z * Alpha1d + ByteColor.Z * Alpha2d) / AlphaSum);
            end;
            p^.W := Round(255 * AlphaSum);
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
  ByteColor := GrayscaleValue(Vector3Byte(Vector3(aColor.X, aColor.Y, aColor.Z)));
  MeanRadius := 1 / aRadiusX + 1 / aRadiusY; //this is simple, but wrong. But let's leave it for now
  Alpha2 := aColor.W;
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

            Alpha1 := p^.Y / 255;

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
              p^.X := Round((p^.X * Alpha1d + ByteColor * Alpha2d) / AlphaSum);
            end;
            p^.Y := Round(255 * AlphaSum);
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
  ByteColor.X := Trunc(255 * aColor.X);
  ByteColor.Y := Trunc(255 * aColor.Y);
  ByteColor.Z := Trunc(255 * aColor.Z);
  MeanRadius := 1 / aRadiusX + 1 / aRadiusY; //this is simple, but wrong. But let's leave it for now
  Alpha2 := aColor.W;
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
            p^.X := Round((p^.X * Alpha1d + ByteColor.X * Alpha2d));
            p^.Y := Round((p^.Y * Alpha1d + ByteColor.Y * Alpha2d));
            p^.Z := Round((p^.Z * Alpha1d + ByteColor.Z * Alpha2d));
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
  ByteColor := GrayscaleValue(Vector3Byte(Vector3(aColor.X, aColor.Y, aColor.Z)));
  MeanRadius := 1 / aRadiusX + 1 / aRadiusY; //this is simple, but wrong. But let's leave it for now
  Alpha2 := aColor.W;
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
  Alpha2 := aColor.W;
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
            Alpha1 := OldColor.W;

            {antialiasing}
            if d1 < MeanRadius1 then
              Alpha2d := Alpha2 * d1 / MeanRadius1
            else
              Alpha2d := Alpha2;
            if d2 > 0 then
              Alpha2d := Alpha2d * (1 - d2 / MeanRadius2);

            Alpha1d := Alpha1 * (1 - Alpha2d);
            AlphaSum := Alpha1 + (1 - Alpha1) * Alpha2d;

            NewColor.X := (OldColor.X * Alpha1d + aColor.X * Alpha2d) / AlphaSum;
            NewColor.Y := (OldColor.Y * Alpha1d + aColor.Y * Alpha2d) / AlphaSum;
            NewColor.Z := (OldColor.Z * Alpha1d + aColor.Z * Alpha2d) / AlphaSum;
            NewColor.W := AlphaSum;

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
  ByteColor.X := Trunc(255 * aColor.X);
  ByteColor.Y := Trunc(255 * aColor.Y);
  ByteColor.Z := Trunc(255 * aColor.Z);
  MeanRadius1 := 1 / aRadiusX + 1 / aRadiusY;
  MeanRadius2 := 1 / (aRadiusX - aWidth) + 1 / (aRadiusY - aWidth);
  Alpha2 := aColor.W;
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
            p := PixelPtr(ix, iy);

            Alpha1 := p^.W / 255;

            {antialiasing}
            if d1 < MeanRadius1 then
              Alpha2d := Alpha2 * d1 / MeanRadius1
            else
              Alpha2d := Alpha2;
            if d2 > 0 then
              Alpha2d := Alpha2d * (1 - d2 / MeanRadius2);

            Alpha1d := Alpha1 * (1 - Alpha2d);
            AlphaSum := Alpha1 + (1 - Alpha1) * Alpha2d;
            if AlphaSum > 0 then begin
              p^.X := Round((p^.X * Alpha1d + ByteColor.X * Alpha2d) / AlphaSum);
              p^.Y := Round((p^.Y * Alpha1d + ByteColor.Y * Alpha2d) / AlphaSum);
              p^.Z := Round((p^.Z * Alpha1d + ByteColor.Z * Alpha2d) / AlphaSum);
            end;
            p^.W := Round(255 * AlphaSum);
          end;
        end;
    end;
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
  ByteColor := GrayscaleValue(Vector3Byte(Vector3(aColor.X, aColor.Y, aColor.Z)));
  MeanRadius1 := 1 / aRadiusX + 1 / aRadiusY;
  MeanRadius2 := 1 / (aRadiusX - aWidth) + 1 / (aRadiusY - aWidth);
  Alpha2 := aColor.W;
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
            p := PixelPtr(ix, iy);

            Alpha1 := p^.Y / 255;

            {antialiasing}
            if d1 < MeanRadius1 then
              Alpha2d := Alpha2 * d1 / MeanRadius1
            else
              Alpha2d := Alpha2;
            if d2 > 0 then
              Alpha2d := Alpha2d * (1 - d2 / MeanRadius2);

            Alpha1d := Alpha1 * (1 - Alpha2d);
            AlphaSum := Alpha1 + (1 - Alpha1) * Alpha2d;
            if AlphaSum > 0 then begin
              p^.X := Round((p^.X * Alpha1d + ByteColor * Alpha2d) / AlphaSum);
            end;
            p^.Y := Round(255 * AlphaSum);
          end;
        end;
    end;
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
  ByteColor.X := Trunc(255 * aColor.X);
  ByteColor.Y := Trunc(255 * aColor.Y);
  ByteColor.Z := Trunc(255 * aColor.Z);
  MeanRadius1 := 1 / aRadiusX + 1 / aRadiusY;
  MeanRadius2 := 1 / (aRadiusX - aWidth) + 1 / (aRadiusY - aWidth);
  Alpha2 := aColor.W;
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
            p := PixelPtr(ix, iy);

            {antialiasing}
            if d1 < MeanRadius1 then
              Alpha2d := Alpha2 * d1 / MeanRadius1
            else
              Alpha2d := Alpha2;
            if d2 > 0 then
              Alpha2d := Alpha2d * (1 - d2 / MeanRadius2);

            Alpha1d := 1 - Alpha2d;
            p^.X := Round((p^.X * Alpha1d + ByteColor.X * Alpha2d));
            p^.Y := Round((p^.Y * Alpha1d + ByteColor.Y * Alpha2d));
            p^.Z := Round((p^.Z * Alpha1d + ByteColor.Z * Alpha2d));
          end;
        end;
    end;
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
  ByteColor := GrayscaleValue(Vector3Byte(Vector3(aColor.X, aColor.Y, aColor.Z)));
  MeanRadius1 := 1 / aRadiusX + 1 / aRadiusY;
  MeanRadius2 := 1 / (aRadiusX - aWidth) + 1 / (aRadiusY - aWidth);
  Alpha2 := aColor.W;
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
            p := PixelPtr(ix, iy);

            {antialiasing}
            if d1 < MeanRadius1 then
              Alpha2d := Alpha2 * d1 / MeanRadius1
            else
              Alpha2d := Alpha2;
            if d2 > 0 then
              Alpha2d := Alpha2d * (1 - d2 / MeanRadius2);

            Alpha1d := 1 - Alpha2d;
            p^ := Round((p^ * Alpha1d + ByteColor * Alpha2d));
          end;
        end;
    end;
end;

{-----= FILL RECTANGLE =-----}

procedure TCastleImage.FillRectangle(const x1, y1, x2, y2: single;
  const aColor: TCastleColor);
var
  NewColor, OldColor: TCastleColor;
  ix, iy: integer;
  Alpha1, Alpha1d, Alpha2, Alpha2d, AlphaSum: single;
  MinX, MaxX, MinY, MaxY: single;
  dx, dy: single;
  function Min(a, b: single): single; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
  begin
    if a < b then Result := a else Result := b;
  end;
begin
  if x1 < x2 then
  begin
    MinX := x1;
    MaxX := x2;
  end
  else
  begin
    MinX := x2;
    MaxX := x1;
  end;
  if y1 < y2 then
  begin
    MinY := y1;
    MaxY := y2;
  end
  else
  begin
    MinY := y2;
    MaxY := y1;
  end;
  if MinX < 0 then MinX := 0;
  if MinY < 0 then MinY := 0;
  if MaxX > Width then MaxX := Width;
  if MaxY > Height then MaxY := Height;

  Alpha2 := aColor.W;

  for iy := Round(MinY - 1) to Round(MaxY + 1) do
    if (iy >= 0) and (iy < Height) then
    begin
      for ix := Round(MinX - 1) to Round(MaxX + 1) do
        if (ix >= 0) and (ix < Width) then
        begin
          OldColor := Colors[ix, iy, 0];
          Alpha1 := OldColor.W;

          {antialiasing}
          if ix < MinX then
            dx := MinX - ix
          else
          if ix > MaxX then
            dx := ix - MaxX
          else
            dx := 1;
          if iy < MinY then
            dy := MinY - iy
          else
          if iy > MaxY then
            dy := iy - MaxY
          else
            dy := 1;
          Alpha2d := Alpha2 * Sqr(Min(dx, dy));

          Alpha1d := Alpha1 * (1 - Alpha2d);
          AlphaSum := Alpha1 + (1 - Alpha1) * Alpha2d;

          NewColor.X := (OldColor.X * Alpha1d + aColor.X * Alpha2d) / AlphaSum;
          NewColor.Y := (OldColor.Y * Alpha1d + aColor.Y * Alpha2d) / AlphaSum;
          NewColor.Z := (OldColor.Z * Alpha1d + aColor.Z * Alpha2d) / AlphaSum;
          NewColor.W := AlphaSum;

          Colors[ix, iy, 0] := NewColor;
        end;
    end;
end;

procedure TRGBAlphaImage.FillRectangle(const x1, y1, x2, y2: single;
const aColor: TCastleColor);
var
  p: PVector4Byte;
  ByteColor: TVector3Byte;
  ix, iy: integer;
  Alpha1, Alpha1d, Alpha2, Alpha2d, AlphaSum: single;
  MinX, MaxX, MinY, MaxY: single;
  dx, dy: single;
  function Min(a, b: single): single; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
  begin
    if a < b then Result := a else Result := b;
  end;
begin
  if x1 < x2 then
  begin
    MinX := x1;
    MaxX := x2;
  end
  else
  begin
    MinX := x2;
    MaxX := x1;
  end;
  if y1 < y2 then
  begin
    MinY := y1;
    MaxY := y2;
  end
  else
  begin
    MinY := y2;
    MaxY := y1;
  end;
  if MinX < 0 then MinX := 0;
  if MinY < 0 then MinY := 0;
  if MaxX > Width then MaxX := Width;
  if MaxY > Height then MaxY := Height;

  ByteColor.X := Trunc(255 * aColor.X);
  ByteColor.Y := Trunc(255 * aColor.Y);
  ByteColor.Z := Trunc(255 * aColor.Z);
  Alpha2 := aColor.W;


  for iy := Round(MinY - 1) to Round(MaxY + 1) do
    if (iy >= 0) and (iy < Height) then
    begin
      for ix := Round(MinX - 1) to Round(MaxX + 1) do
        if (ix >= 0) and (ix < Width) then
        begin
          p := PixelPtr(ix, iy);

          Alpha1 := p^.W / 255;

          {antialiasing}
          if ix < MinX then
            dx := MinX - ix
          else
          if ix > MaxX then
            dx := ix - MaxX
          else
            dx := 1;
          if iy < MinY then
            dy := MinY - iy
          else
          if iy > MaxY then
            dy := iy - MaxY
          else
            dy := 1;
          Alpha2d := Alpha2 * Sqr(Min(dx, dy));

          Alpha1d := Alpha1 * (1 - Alpha2d);
          AlphaSum := Alpha1 + (1 - Alpha1) * Alpha2d;
          if AlphaSum > 0 then begin
            p^.X := Round((p^.X * Alpha1d + ByteColor.X * Alpha2d) / AlphaSum);
            p^.Y := Round((p^.Y * Alpha1d + ByteColor.Y * Alpha2d) / AlphaSum);
            p^.Z := Round((p^.Z * Alpha1d + ByteColor.Z * Alpha2d) / AlphaSum);
          end;
          p^.W := Round(255 * AlphaSum);
        end;
    end;
end;

procedure TGrayscaleAlphaImage.FillRectangle(const x1, y1, x2, y2: single;
const aColor: TCastleColor);
var
  p: PVector2Byte;
  ByteColor: Byte;
  ix, iy: integer;
  Alpha1, Alpha1d, Alpha2, Alpha2d, AlphaSum: single;
  MinX, MaxX, MinY, MaxY: single;
  dx, dy: single;
  function Min(a, b: single): single; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
  begin
    if a < b then Result := a else Result := b;
  end;
begin
  if x1 < x2 then
  begin
    MinX := x1;
    MaxX := x2;
  end
  else
  begin
    MinX := x2;
    MaxX := x1;
  end;
  if y1 < y2 then
  begin
    MinY := y1;
    MaxY := y2;
  end
  else
  begin
    MinY := y2;
    MaxY := y1;
  end;
  if MinX < 0 then MinX := 0;
  if MinY < 0 then MinY := 0;
  if MaxX > Width then MaxX := Width;
  if MaxY > Height then MaxY := Height;

  ByteColor := GrayscaleValue(Vector3Byte(Vector3(aColor.X, aColor.Y, aColor.Z)));
  Alpha2 := aColor.W;

  for iy := Round(MinY - 1) to Round(MaxY + 1) do
    if (iy >= 0) and (iy < Height) then
    begin
      for ix := Round(MinX - 1) to Round(MaxX + 1) do
        if (ix >= 0) and (ix < Width) then
        begin
          p := PixelPtr(ix, iy);

          Alpha1 := p^.Y / 255;

          {antialiasing}
          if ix < MinX then
            dx := MinX - ix
          else
          if ix > MaxX then
            dx := ix - MaxX
          else
            dx := 1;
          if iy < MinY then
            dy := MinY - iy
          else
          if iy > MaxY then
            dy := iy - MaxY
          else
            dy := 1;
          Alpha2d := Alpha2 * Sqr(Min(dx, dy));

          Alpha1d := Alpha1 * (1 - Alpha2d);
          AlphaSum := Alpha1 + (1 - Alpha1) * Alpha2d;
          if AlphaSum > 0 then begin
            p^.X := Round((p^.X * Alpha1d + ByteColor * Alpha2d) / AlphaSum);
          end;
          p^.Y := Round(255 * AlphaSum);
        end;
    end;
end;

procedure TRGBImage.FillRectangle(const x1, y1, x2, y2: single;
const aColor: TCastleColor);
var
  p: PVector3Byte;
  ByteColor: TVector3Byte;
  ix, iy: integer;
  Alpha1d, Alpha2, Alpha2d: single;
  MinX, MaxX, MinY, MaxY: single;
  dx, dy: single;
  function Min(a, b: single): single; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
  begin
    if a < b then Result := a else Result := b;
  end;
begin
  if x1 < x2 then
  begin
    MinX := x1;
    MaxX := x2;
  end
  else
  begin
    MinX := x2;
    MaxX := x1;
  end;
  if y1 < y2 then
  begin
    MinY := y1;
    MaxY := y2;
  end
  else
  begin
    MinY := y2;
    MaxY := y1;
  end;
  if MinX < 0 then MinX := 0;
  if MinY < 0 then MinY := 0;
  if MaxX > Width then MaxX := Width;
  if MaxY > Height then MaxY := Height;

  ByteColor.X := Trunc(255 * aColor.X);
  ByteColor.Y := Trunc(255 * aColor.Y);
  ByteColor.Z := Trunc(255 * aColor.Z);
  Alpha2 := aColor.W;

  for iy := Round(MinY - 1) to Round(MaxY + 1) do
    if (iy >= 0) and (iy < Height) then
    begin
      for ix := Round(MinX - 1) to Round(MaxX + 1) do
        if (ix >= 0) and (ix < Width) then
        begin
          p := PixelPtr(ix, iy);

          {antialiasing}
          if ix < MinX then
            dx := MinX - ix
          else
          if ix > MaxX then
            dx := ix - MaxX
          else
            dx := 1;
          if iy < MinY then
            dy := MinY - iy
          else
          if iy > MaxY then
            dy := iy - MaxY
          else
            dy := 1;
          Alpha2d := Alpha2 * Sqr(Min(dx, dy));

          Alpha1d := 1 - Alpha2d;
          p^.X := Round((p^.X * Alpha1d + ByteColor.X * Alpha2d));
          p^.Y := Round((p^.Y * Alpha1d + ByteColor.Y * Alpha2d));
          p^.Z := Round((p^.Z * Alpha1d + ByteColor.Z * Alpha2d));
        end;
    end;
end;

procedure TGrayscaleImage.FillRectangle(const x1, y1, x2, y2: single;
const aColor: TCastleColor);
var
  p: PByte;
  ByteColor: Byte;
  ix, iy: integer;
  Alpha1d, Alpha2, Alpha2d: single;
  MinX, MaxX, MinY, MaxY: single;
  dx, dy: single;
  function Min(a, b: single): single; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
  begin
    if a < b then Result := a else Result := b;
  end;
begin
  if x1 < x2 then
  begin
    MinX := x1;
    MaxX := x2;
  end
  else
  begin
    MinX := x2;
    MaxX := x1;
  end;
  if y1 < y2 then
  begin
    MinY := y1;
    MaxY := y2;
  end
  else
  begin
    MinY := y2;
    MaxY := y1;
  end;
  if MinX < 0 then MinX := 0;
  if MinY < 0 then MinY := 0;
  if MaxX > Width then MaxX := Width;
  if MaxY > Height then MaxY := Height;

  ByteColor := GrayscaleValue(Vector3Byte(Vector3(aColor.X, aColor.Y, aColor.Z)));
  Alpha2 := aColor.W;

  for iy := Round(MinY - 1) to Round(MaxY + 1) do
    if (iy >= 0) and (iy < Height) then
    begin
      for ix := Round(MinX - 1) to Round(MaxX + 1) do
        if (ix >= 0) and (ix < Width) then
        begin
          p := PixelPtr(ix, iy);

          {antialiasing}
          if ix < MinX then
            dx := MinX - ix
          else
          if ix > MaxX then
            dx := ix - MaxX
          else
            dx := 1;
          if iy < MinY then
            dy := MinY - iy
          else
          if iy > MaxY then
            dy := iy - MaxY
          else
            dy := 1;
          Alpha2d := Alpha2 * Sqr(Min(dx, dy));

          Alpha1d := 1 - Alpha2d;
          p^ := Round((p^ * Alpha1d + ByteColor * Alpha2d));
        end;
    end;
end;

{-----= HOLLOW RECTANGLE =-----}

procedure TCastleImage.Rectangle(const x1, y1, x2, y2: single;
  const aWidth: single; const aColor: TCastleColor);
var
  NewColor, OldColor: TCastleColor;
  ix, iy: integer;
  Alpha1, Alpha1d, Alpha2, Alpha2d, AlphaSum: single;
  MinX, MaxX, MinY, MaxY: single;
  d: single;
begin
  if x1 < x2 then
  begin
    MinX := x1;
    MaxX := x2;
  end
  else
  begin
    MinX := x2;
    MaxX := x1;
  end;
  if y1 < y2 then
  begin
    MinY := y1;
    MaxY := y2;
  end
  else
  begin
    MinY := y2;
    MaxY := y1;
  end;
  if MinX < 0 then MinX := 0;
  if MinY < 0 then MinY := 0;
  if MaxX > Width then MaxX := Width;
  if MaxY > Height then MaxY := Height;

  Alpha2 := aColor.W;

  for iy := Round(MinY - 1) to Round(MaxY + 1) do
    if (iy >= 0) and (iy < Height) then
    begin
      for ix := Round(MinX - 1) to Round(MaxX + 1) do
        if (ix >= 0) and (ix < Width) then
        begin
          if ix < MinX then
            d := MinX - ix
          else
          if (iy < MinY) then
            d := MinY - iy
          else
          if ix > MaxX then
            d := ix - MaxX
          else
          if iy > MaxY then
            d := iy - MaxY
          else
          if ix < MinX + aWidth then
            d := 1
          else
          if iy < MinY + aWidth then
            d := 1
          else
          if ix > MaxX - aWidth then
            d := 1
          else
          if iy > MaxY - aWidth then
            d := 1
          else
          if ix < MinX + aWidth + 1 then
            d := MinX + aWidth + 1 - ix
          else
          if iy < MinY + aWidth + 1 then
            d := MinY + aWidth + 1 - iy
          else
          if ix > MaxX - aWidth - 1 then
            d := ix - (MaxX - aWidth - 1)
          else
          if iy > MaxY - aWidth - 1 then
            d := iy - (MaxY - aWidth - 1)
          else
            d := 0;

          if d > 0 then
          begin
            OldColor := Colors[ix, iy, 0];
            Alpha1 := OldColor.W;
            Alpha2d := Alpha2 * Sqr(d);

            Alpha1d := Alpha1 * (1 - Alpha2d);
            AlphaSum := Alpha1 + (1 - Alpha1) * Alpha2d;

            NewColor.X := (OldColor.X * Alpha1d + aColor.X * Alpha2d) / AlphaSum;
            NewColor.Y := (OldColor.Y * Alpha1d + aColor.Y * Alpha2d) / AlphaSum;
            NewColor.Z := (OldColor.Z * Alpha1d + aColor.Z * Alpha2d) / AlphaSum;
            NewColor.W := AlphaSum;

            Colors[ix, iy, 0] := NewColor;
          end;
        end;
    end;
end;

procedure TRGBAlphaImage.Rectangle(const x1, y1, x2, y2: single;
  const aWidth: single; const aColor: TCastleColor);
var
  p: PVector4Byte;
  ByteColor: TVector3Byte;
  ix, iy: integer;
  Alpha1, Alpha1d, Alpha2, Alpha2d, AlphaSum: single;
  MinX, MaxX, MinY, MaxY: single;
  d: single;
begin
  if x1 < x2 then
  begin
    MinX := x1;
    MaxX := x2;
  end
  else
  begin
    MinX := x2;
    MaxX := x1;
  end;
  if y1 < y2 then
  begin
    MinY := y1;
    MaxY := y2;
  end
  else
  begin
    MinY := y2;
    MaxY := y1;
  end;
  if MinX < 0 then MinX := 0;
  if MinY < 0 then MinY := 0;
  if MaxX > Width then MaxX := Width;
  if MaxY > Height then MaxY := Height;

  ByteColor.X := Trunc(255 * aColor.X);
  ByteColor.Y := Trunc(255 * aColor.Y);
  ByteColor.Z := Trunc(255 * aColor.Z);
  Alpha2 := aColor.W;


  for iy := Round(MinY - 1) to Round(MaxY + 1) do
    if (iy >= 0) and (iy < Height) then
    begin
      for ix := Round(MinX - 1) to Round(MaxX + 1) do
        if (ix >= 0) and (ix < Width) then
        begin
          if ix < MinX then
            d := MinX - ix
          else
          if (iy < MinY) then
            d := MinY - iy
          else
          if ix > MaxX then
            d := ix - MaxX
          else
          if iy > MaxY then
            d := iy - MaxY
          else
          if ix < MinX + aWidth then
            d := 1
          else
          if iy < MinY + aWidth then
            d := 1
          else
          if ix > MaxX - aWidth then
            d := 1
          else
          if iy > MaxY - aWidth then
            d := 1
          else
          if ix < MinX + aWidth + 1 then
            d := MinX + aWidth + 1 - ix
          else
          if iy < MinY + aWidth + 1 then
            d := MinY + aWidth + 1 - iy
          else
          if ix > MaxX - aWidth - 1 then
            d := ix - (MaxX - aWidth - 1)
          else
          if iy > MaxY - aWidth - 1 then
            d := iy - (MaxY - aWidth - 1)
          else
            d := 0;

          if d > 0 then
          begin
            p := PixelPtr(ix, iy);
            Alpha1 := p^.W / 255;
            Alpha2d := Alpha2 * Sqr(d);

            Alpha1d := Alpha1 * (1 - Alpha2d);
            AlphaSum := Alpha1 + (1 - Alpha1) * Alpha2d;
            if AlphaSum > 0 then begin
              p^.X := Round((p^.X * Alpha1d + ByteColor.X * Alpha2d) / AlphaSum);
              p^.Y := Round((p^.Y * Alpha1d + ByteColor.Y * Alpha2d) / AlphaSum);
              p^.Z := Round((p^.Z * Alpha1d + ByteColor.Z * Alpha2d) / AlphaSum);
            end;
            p^.W := Round(255 * AlphaSum);
          end;
        end;
    end;
end;

procedure TGrayscaleAlphaImage.Rectangle(const x1, y1, x2, y2: single;
  const aWidth: single; const aColor: TCastleColor);
var
  p: PVector2Byte;
  ByteColor: Byte;
  ix, iy: integer;
  Alpha1, Alpha1d, Alpha2, Alpha2d, AlphaSum: single;
  MinX, MaxX, MinY, MaxY: single;
  d: single;
begin
  if x1 < x2 then
  begin
    MinX := x1;
    MaxX := x2;
  end
  else
  begin
    MinX := x2;
    MaxX := x1;
  end;
  if y1 < y2 then
  begin
    MinY := y1;
    MaxY := y2;
  end
  else
  begin
    MinY := y2;
    MaxY := y1;
  end;
  if MinX < 0 then MinX := 0;
  if MinY < 0 then MinY := 0;
  if MaxX > Width then MaxX := Width;
  if MaxY > Height then MaxY := Height;

  ByteColor := GrayscaleValue(Vector3Byte(Vector3(aColor.X, aColor.Y, aColor.Z)));
  Alpha2 := aColor.W;

  for iy := Round(MinY - 1) to Round(MaxY + 1) do
    if (iy >= 0) and (iy < Height) then
    begin
      for ix := Round(MinX - 1) to Round(MaxX + 1) do
        if (ix >= 0) and (ix < Width) then
        begin
          if ix < MinX then
            d := MinX - ix
          else
          if (iy < MinY) then
            d := MinY - iy
          else
          if ix > MaxX then
            d := ix - MaxX
          else
          if iy > MaxY then
            d := iy - MaxY
          else
          if ix < MinX + aWidth then
            d := 1
          else
          if iy < MinY + aWidth then
            d := 1
          else
          if ix > MaxX - aWidth then
            d := 1
          else
          if iy > MaxY - aWidth then
            d := 1
          else
          if ix < MinX + aWidth + 1 then
            d := MinX + aWidth + 1 - ix
          else
          if iy < MinY + aWidth + 1 then
            d := MinY + aWidth + 1 - iy
          else
          if ix > MaxX - aWidth - 1 then
            d := ix - (MaxX - aWidth - 1)
          else
          if iy > MaxY - aWidth - 1 then
            d := iy - (MaxY - aWidth - 1)
          else
            d := 0;

          if d > 0 then
          begin
            p := PixelPtr(ix, iy);
            Alpha1 := p^.Y / 255;
            Alpha2d := Alpha2 * Sqr(d);

            Alpha1d := Alpha1 * (1 - Alpha2d);
            AlphaSum := Alpha1 + (1 - Alpha1) * Alpha2d;
            if AlphaSum > 0 then begin
              p^.X := Round((p^.X * Alpha1d + ByteColor * Alpha2d) / AlphaSum);
            end;
            p^.Y := Round(255 * AlphaSum);
          end;
        end;
    end;
end;

procedure TRGBImage.Rectangle(const x1, y1, x2, y2: single;
  const aWidth: single; const aColor: TCastleColor);
var
  p: PVector3Byte;
  ByteColor: TVector3Byte;
  ix, iy: integer;
  Alpha1d, Alpha2, Alpha2d: single;
  MinX, MaxX, MinY, MaxY: single;
  d: single;
begin
  if x1 < x2 then
  begin
    MinX := x1;
    MaxX := x2;
  end
  else
  begin
    MinX := x2;
    MaxX := x1;
  end;
  if y1 < y2 then
  begin
    MinY := y1;
    MaxY := y2;
  end
  else
  begin
    MinY := y2;
    MaxY := y1;
  end;
  if MinX < 0 then MinX := 0;
  if MinY < 0 then MinY := 0;
  if MaxX > Width then MaxX := Width;
  if MaxY > Height then MaxY := Height;

  ByteColor.X := Trunc(255 * aColor.X);
  ByteColor.Y := Trunc(255 * aColor.Y);
  ByteColor.Z := Trunc(255 * aColor.Z);
  Alpha2 := aColor.W;

  for iy := Round(MinY - 1) to Round(MaxY + 1) do
    if (iy >= 0) and (iy < Height) then
    begin
      for ix := Round(MinX - 1) to Round(MaxX + 1) do
        if (ix >= 0) and (ix < Width) then
        begin
          if ix < MinX then
            d := MinX - ix
          else
          if (iy < MinY) then
            d := MinY - iy
          else
          if ix > MaxX then
            d := ix - MaxX
          else
          if iy > MaxY then
            d := iy - MaxY
          else
          if ix < MinX + aWidth then
            d := 1
          else
          if iy < MinY + aWidth then
            d := 1
          else
          if ix > MaxX - aWidth then
            d := 1
          else
          if iy > MaxY - aWidth then
            d := 1
          else
          if ix < MinX + aWidth + 1 then
            d := MinX + aWidth + 1 - ix
          else
          if iy < MinY + aWidth + 1 then
            d := MinY + aWidth + 1 - iy
          else
          if ix > MaxX - aWidth - 1 then
            d := ix - (MaxX - aWidth - 1)
          else
          if iy > MaxY - aWidth - 1 then
            d := iy - (MaxY - aWidth - 1)
          else
            d := 0;

          if d > 0 then
          begin
            p := PixelPtr(ix, iy);
            Alpha2d := Alpha2 * Sqr(d);

            Alpha1d := 1 - Alpha2d;
            p^.X := Round((p^.X * Alpha1d + ByteColor.X * Alpha2d));
            p^.Y := Round((p^.Y * Alpha1d + ByteColor.Y * Alpha2d));
            p^.Z := Round((p^.Z * Alpha1d + ByteColor.Z * Alpha2d));
          end;
        end;
    end;
end;

procedure TGrayscaleImage.Rectangle(const x1, y1, x2, y2: single;
  const aWidth: single; const aColor: TCastleColor);
var
  p: PByte;
  ByteColor: Byte;
  ix, iy: integer;
  Alpha1d, Alpha2, Alpha2d: single;
  MinX, MaxX, MinY, MaxY: single;
  d: single;
begin
  if x1 < x2 then
  begin
    MinX := x1;
    MaxX := x2;
  end
  else
  begin
    MinX := x2;
    MaxX := x1;
  end;
  if y1 < y2 then
  begin
    MinY := y1;
    MaxY := y2;
  end
  else
  begin
    MinY := y2;
    MaxY := y1;
  end;
  if MinX < 0 then MinX := 0;
  if MinY < 0 then MinY := 0;
  if MaxX > Width then MaxX := Width;
  if MaxY > Height then MaxY := Height;

  ByteColor := GrayscaleValue(Vector3Byte(Vector3(aColor.X, aColor.Y, aColor.Z)));
  Alpha2 := aColor.W;

  for iy := Round(MinY - 1) to Round(MaxY + 1) do
    if (iy >= 0) and (iy < Height) then
    begin
      for ix := Round(MinX - 1) to Round(MaxX + 1) do
        if (ix >= 0) and (ix < Width) then
        begin
          if ix < MinX then
            d := MinX - ix
          else
          if (iy < MinY) then
            d := MinY - iy
          else
          if ix > MaxX then
            d := ix - MaxX
          else
          if iy > MaxY then
            d := iy - MaxY
          else
          if ix < MinX + aWidth then
            d := 1
          else
          if iy < MinY + aWidth then
            d := 1
          else
          if ix > MaxX - aWidth then
            d := 1
          else
          if iy > MaxY - aWidth then
            d := 1
          else
          if ix < MinX + aWidth + 1 then
            d := MinX + aWidth + 1 - ix
          else
          if iy < MinY + aWidth + 1 then
            d := MinY + aWidth + 1 - iy
          else
          if ix > MaxX - aWidth - 1 then
            d := ix - (MaxX - aWidth - 1)
          else
          if iy > MaxY - aWidth - 1 then
            d := iy - (MaxY - aWidth - 1)
          else
            d := 0;

          if d > 0 then
          begin
            p := PixelPtr(ix, iy);
            Alpha2d := Alpha2 * Sqr(d);

            Alpha1d := 1 - Alpha2d;
            p^ := Round((p^ * Alpha1d + ByteColor * Alpha2d));
          end;
        end;
    end;
end;

{-----= LINE =-----}

procedure TCastleImage.Line(const x1, y1, x2, y2: single;
  const aWidth: single; const aColor: TCastleColor);
var
  NewColor, OldColor: TCastleColor;
  ix, iy: integer;
  MinX, MaxX, MinY, MaxY: single;
  Alpha1, Alpha1d, Alpha2, Alpha2d, AlphaSum: single;
  a: single;
  d1x, d1y, d12, d2x, d2y, d22, nx, ny, nLength, t, t2, w2, dw2, da2: single;
begin
  if x1 < x2 then
  begin
    MinX := x1;
    MaxX := x2;
  end
  else
  begin
    MinX := x2;
    MaxX := x1;
  end;
  if y1 < y2 then
  begin
    MinY := y1;
    MaxY := y2;
  end
  else
  begin
    MinY := y2;
    MaxY := y1;
  end;
  if MinX < 0 then MinX := 0;
  if MinY < 0 then MinY := 0;
  if MaxX > Width then MaxX := Width;
  if MaxY > Height then MaxY := Height;

  Alpha2 := aColor.W;

  nx := x2 - x1; {vector along the line}
  ny := y2 - y1;
  nLength := Sqrt(Sqr(nx) + Sqr(ny));
  nx := nx / nLength;
  ny := ny / nLength;
  t2 := (x2 - x1) * nx + (y2 - y1) * ny; {parameter of the end point of the line, always >0, actually, equal to nLength}
  w2 := Sqr(aWidth / 2);
  dw2 := aWidth + 0.5; {double radius + 1}

  for iy := Round(MinY - aWidth / 2 - 1) to Round(MaxY + aWidth / 2 + 1) do
    if (iy >= 0) and (iy < Height) then
    begin
      d1y := iy - y1;
      d2y := iy - y2;
      for ix := Round(MinX - aWidth / 2 - 1) to Round(MaxX + aWidth / 2 + 1) do
        if (ix >= 0) and (ix < Width) then
        begin
          d1x := ix - x1;
          d12 := Sqr(d1x) + Sqr(d1y);
          t := d1x * nx + d1y * ny;

          if (t >= 0) and (t <= t2) then
          begin
            da2 := d12 - Sqr(t);
            if da2 < w2 then
              a := 1
            else
            if da2 < w2 + dw2 then
              a := (w2 + dw2 - da2) / dw2
            else
              a := 0;
          end
          else
          if d12 < w2 then
            a := 1
          else
          if d12 < w2 + dw2 then
            a := (w2 + dw2 - d12) / dw2
          else
          begin
            d2x := ix - x2;
            d22 := Sqr(d2x) + Sqr(d2y);
            if d22 < w2 then
              a := 1
            else
            if d22 < w2 + dw2 then
              a := (w2 + dw2 - d22) / dw2
            else
              a := 0;
          end;

          if a > 0 then
          begin
            OldColor := Colors[ix, iy, 0];
            Alpha1 := OldColor.W;
            Alpha2d := Alpha2 * a;

            Alpha1d := Alpha1 * (1 - Alpha2d);
            AlphaSum := Alpha1 + (1 - Alpha1) * Alpha2d;

            NewColor.X := (OldColor.X * Alpha1d + aColor.X * Alpha2d) / AlphaSum;
            NewColor.Y := (OldColor.Y * Alpha1d + aColor.Y * Alpha2d) / AlphaSum;
            NewColor.Z := (OldColor.Z * Alpha1d + aColor.Z * Alpha2d) / AlphaSum;
            NewColor.W := AlphaSum;

            Colors[ix, iy, 0] := NewColor;
          end;
        end;
    end;
end;

procedure TRGBAlphaImage.Line(const x1, y1, x2, y2: single;
  const aWidth: single; const aColor: TCastleColor);
var
  p: PVector4Byte;
  ByteColor: TVector3Byte;
  ix, iy: integer;
  Alpha1, Alpha1d, Alpha2, Alpha2d, AlphaSum: single;
  MinX, MaxX, MinY, MaxY: single;
  a: single;
  d1x, d1y, d12, d2x, d2y, d22, nx, ny, nLength, t, t2, w2, dw2, da2: single;
begin
  if x1 < x2 then
  begin
    MinX := x1;
    MaxX := x2;
  end
  else
  begin
    MinX := x2;
    MaxX := x1;
  end;
  if y1 < y2 then
  begin
    MinY := y1;
    MaxY := y2;
  end
  else
  begin
    MinY := y2;
    MaxY := y1;
  end;
  if MinX < 0 then MinX := 0;
  if MinY < 0 then MinY := 0;
  if MaxX > Width then MaxX := Width;
  if MaxY > Height then MaxY := Height;

  ByteColor.X := Trunc(255 * aColor.X);
  ByteColor.Y := Trunc(255 * aColor.Y);
  ByteColor.Z := Trunc(255 * aColor.Z);
  Alpha2 := aColor.W;

  nx := x2 - x1; {vector along the line}
  ny := y2 - y1;
  nLength := Sqrt(Sqr(nx) + Sqr(ny));
  nx := nx / nLength;
  ny := ny / nLength;
  t2 := (x2 - x1) * nx + (y2 - y1) * ny; {parameter of the end point of the line, always >0, actually, equal to nLength}
  w2 := Sqr(aWidth / 2);
  dw2 := aWidth + 0.5; {double radius + 1}

  for iy := Round(MinY - aWidth / 2 - 1) to Round(MaxY + aWidth / 2 + 1) do
    if (iy >= 0) and (iy < Height) then
    begin
      d1y := iy - y1;
      d2y := iy - y2;
      for ix := Round(MinX - aWidth / 2 - 1) to Round(MaxX + aWidth / 2 + 1) do
        if (ix >= 0) and (ix < Width) then
        begin
          d1x := ix - x1;
          d12 := Sqr(d1x) + Sqr(d1y);
          t := d1x * nx + d1y * ny;

          if (t >= 0) and (t <= t2) then
          begin
            da2 := d12 - Sqr(t);
            if da2 < w2 then
              a := 1
            else
            if da2 < w2 + dw2 then
              a := (w2 + dw2 - da2) / dw2
            else
              a := 0;
          end
          else
          if d12 < w2 then
            a := 1
          else
          if d12 < w2 + dw2 then
            a := (w2 + dw2 - d12) / dw2
          else
          begin
            d2x := ix - x2;
            d22 := Sqr(d2x) + Sqr(d2y);
            if d22 < w2 then
              a := 1
            else
            if d22 < w2 + dw2 then
              a := (w2 + dw2 - d22) / dw2
            else
              a := 0;
          end;

          if a > 0 then
          begin
            p := PixelPtr(ix, iy);
            Alpha1 := p^.W / 255;
            Alpha2d := Alpha2 * a;

            Alpha1d := Alpha1 * (1 - Alpha2d);
            AlphaSum := Alpha1 + (1 - Alpha1) * Alpha2d;
            if AlphaSum > 0 then begin
              p^.X := Round((p^.X * Alpha1d + ByteColor.X * Alpha2d) / AlphaSum);
              p^.Y := Round((p^.Y * Alpha1d + ByteColor.Y * Alpha2d) / AlphaSum);
              p^.Z := Round((p^.Z * Alpha1d + ByteColor.Z * Alpha2d) / AlphaSum);
            end;
            p^.W := Round(255 * AlphaSum);
          end;
        end;
    end;
end;

procedure TGrayscaleAlphaImage.Line(const x1, y1, x2, y2: single;
  const aWidth: single; const aColor: TCastleColor);
var
  p: PVector2Byte;
  ByteColor: Byte;
  ix, iy: integer;
  Alpha1, Alpha1d, Alpha2, Alpha2d, AlphaSum: single;
  MinX, MaxX, MinY, MaxY: single;
  a: single;
  d1x, d1y, d12, d2x, d2y, d22, nx, ny, nLength, t, t2, w2, dw2, da2: single;
begin
  if x1 < x2 then
  begin
    MinX := x1;
    MaxX := x2;
  end
  else
  begin
    MinX := x2;
    MaxX := x1;
  end;
  if y1 < y2 then
  begin
    MinY := y1;
    MaxY := y2;
  end
  else
  begin
    MinY := y2;
    MaxY := y1;
  end;
  if MinX < 0 then MinX := 0;
  if MinY < 0 then MinY := 0;
  if MaxX > Width then MaxX := Width;
  if MaxY > Height then MaxY := Height;

  ByteColor := GrayscaleValue(Vector3Byte(Vector3(aColor.X, aColor.Y, aColor.Z)));
  Alpha2 := aColor.W;

  nx := x2 - x1; {vector along the line}
  ny := y2 - y1;
  nLength := Sqrt(Sqr(nx) + Sqr(ny));
  nx := nx / nLength;
  ny := ny / nLength;
  t2 := (x2 - x1) * nx + (y2 - y1) * ny; {parameter of the end point of the line, always >0, actually, equal to nLength}
  w2 := Sqr(aWidth / 2);
  dw2 := aWidth + 0.5; {double radius + 1}

  for iy := Round(MinY - aWidth / 2 - 1) to Round(MaxY + aWidth / 2 + 1) do
    if (iy >= 0) and (iy < Height) then
    begin
      d1y := iy - y1;
      d2y := iy - y2;
      for ix := Round(MinX - aWidth / 2 - 1) to Round(MaxX + aWidth / 2 + 1) do
        if (ix >= 0) and (ix < Width) then
        begin
          d1x := ix - x1;
          d12 := Sqr(d1x) + Sqr(d1y);
          t := d1x * nx + d1y * ny;

          if (t >= 0) and (t <= t2) then
          begin
            da2 := d12 - Sqr(t);
            if da2 < w2 then
              a := 1
            else
            if da2 < w2 + dw2 then
              a := (w2 + dw2 - da2) / dw2
            else
              a := 0;
          end
          else
          if d12 < w2 then
            a := 1
          else
          if d12 < w2 + dw2 then
            a := (w2 + dw2 - d12) / dw2
          else
          begin
            d2x := ix - x2;
            d22 := Sqr(d2x) + Sqr(d2y);
            if d22 < w2 then
              a := 1
            else
            if d22 < w2 + dw2 then
              a := (w2 + dw2 - d22) / dw2
            else
              a := 0;
          end;

          if a > 0 then
          begin
            p := PixelPtr(ix, iy);
            Alpha1 := p^.Y / 255;
            Alpha2d := Alpha2 * a;

            Alpha1d := Alpha1 * (1 - Alpha2d);
            AlphaSum := Alpha1 + (1 - Alpha1) * Alpha2d;
            if AlphaSum > 0 then begin
              p^.X := Round((p^.X * Alpha1d + ByteColor * Alpha2d) / AlphaSum);
            end;
            p^.Y := Round(255 * AlphaSum);
          end;
        end;
    end;
end;

procedure TRGBImage.Line(const x1, y1, x2, y2: single;
  const aWidth: single; const aColor: TCastleColor);
var
  p: PVector3Byte;
  ByteColor: TVector3Byte;
  ix, iy: integer;
  Alpha1d, Alpha2, Alpha2d: single;
  MinX, MaxX, MinY, MaxY: single;
  a: single;
  d1x, d1y, d12, d2x, d2y, d22, nx, ny, nLength, t, t2, w2, dw2, da2: single;
begin
  if x1 < x2 then
  begin
    MinX := x1;
    MaxX := x2;
  end
  else
  begin
    MinX := x2;
    MaxX := x1;
  end;
  if y1 < y2 then
  begin
    MinY := y1;
    MaxY := y2;
  end
  else
  begin
    MinY := y2;
    MaxY := y1;
  end;
  if MinX < 0 then MinX := 0;
  if MinY < 0 then MinY := 0;
  if MaxX > Width then MaxX := Width;
  if MaxY > Height then MaxY := Height;

  ByteColor.X := Trunc(255 * aColor.X);
  ByteColor.Y := Trunc(255 * aColor.Y);
  ByteColor.Z := Trunc(255 * aColor.Z);
  Alpha2 := aColor.W;

  nx := x2 - x1; {vector along the line}
  ny := y2 - y1;
  nLength := Sqrt(Sqr(nx) + Sqr(ny));
  nx := nx / nLength;
  ny := ny / nLength;
  t2 := (x2 - x1) * nx + (y2 - y1) * ny; {parameter of the end point of the line, always >0, actually, equal to nLength}
  w2 := Sqr(aWidth / 2);
  dw2 := aWidth + 0.5; {double radius + 1}

  for iy := Round(MinY - aWidth / 2 - 1) to Round(MaxY + aWidth / 2 + 1) do
    if (iy >= 0) and (iy < Height) then
    begin
      d1y := iy - y1;
      d2y := iy - y2;
      for ix := Round(MinX - aWidth / 2 - 1) to Round(MaxX + aWidth / 2 + 1) do
        if (ix >= 0) and (ix < Width) then
        begin
          d1x := ix - x1;
          d12 := Sqr(d1x) + Sqr(d1y);
          t := d1x * nx + d1y * ny;

          if (t >= 0) and (t <= t2) then
          begin
            da2 := d12 - Sqr(t);
            if da2 < w2 then
              a := 1
            else
            if da2 < w2 + dw2 then
              a := (w2 + dw2 - da2) / dw2
            else
              a := 0;
          end
          else
          if d12 < w2 then
            a := 1
          else
          if d12 < w2 + dw2 then
            a := (w2 + dw2 - d12) / dw2
          else
          begin
            d2x := ix - x2;
            d22 := Sqr(d2x) + Sqr(d2y);
            if d22 < w2 then
              a := 1
            else
            if d22 < w2 + dw2 then
              a := (w2 + dw2 - d22) / dw2
            else
              a := 0;
          end;

          if a > 0 then
          begin
            p := PixelPtr(ix, iy);
            Alpha2d := Alpha2 * a;

            Alpha1d := 1 - Alpha2d;
            p^.X := Round((p^.X * Alpha1d + ByteColor.X * Alpha2d));
            p^.Y := Round((p^.Y * Alpha1d + ByteColor.Y * Alpha2d));
            p^.Z := Round((p^.Z * Alpha1d + ByteColor.Z * Alpha2d));
          end;
        end;
    end;
end;

procedure TGrayscaleImage.Line(const x1, y1, x2, y2: single;
  const aWidth: single; const aColor: TCastleColor);
var
  p: PByte;
  ByteColor: Byte;
  ix, iy: integer;
  Alpha1d, Alpha2, Alpha2d: single;
  MinX, MaxX, MinY, MaxY: single;
  a: single;
  d1x, d1y, d12, d2x, d2y, d22, nx, ny, nLength, t, t2, w2, dw2, da2: single;
begin
  if x1 < x2 then
  begin
    MinX := x1;
    MaxX := x2;
  end
  else
  begin
    MinX := x2;
    MaxX := x1;
  end;
  if y1 < y2 then
  begin
    MinY := y1;
    MaxY := y2;
  end
  else
  begin
    MinY := y2;
    MaxY := y1;
  end;
  if MinX < 0 then MinX := 0;
  if MinY < 0 then MinY := 0;
  if MaxX > Width then MaxX := Width;
  if MaxY > Height then MaxY := Height;

  ByteColor := GrayscaleValue(Vector3Byte(Vector3(aColor.X, aColor.Y, aColor.Z)));
  Alpha2 := aColor.W;
  nx := x2 - x1; {vector along the line}
  ny := y2 - y1;
  nLength := Sqrt(Sqr(nx) + Sqr(ny));
  nx := nx / nLength;
  ny := ny / nLength;
  t2 := (x2 - x1) * nx + (y2 - y1) * ny; {parameter of the end point of the line, always >0, actually, equal to nLength}
  w2 := Sqr(aWidth / 2);
  dw2 := aWidth + 0.5; {double radius + 1}

  for iy := Round(MinY - aWidth / 2 - 1) to Round(MaxY + aWidth / 2 + 1) do
    if (iy >= 0) and (iy < Height) then
    begin
      d1y := iy - y1;
      d2y := iy - y2;
      for ix := Round(MinX - aWidth / 2 - 1) to Round(MaxX + aWidth / 2 + 1) do
        if (ix >= 0) and (ix < Width) then
        begin
          d1x := ix - x1;
          d12 := Sqr(d1x) + Sqr(d1y);
          t := d1x * nx + d1y * ny;

          if (t >= 0) and (t <= t2) then
          begin
            da2 := d12 - Sqr(t);
            if da2 < w2 then
              a := 1
            else
            if da2 < w2 + dw2 then
              a := (w2 + dw2 - da2) / dw2
            else
              a := 0;
          end
          else
          if d12 < w2 then
            a := 1
          else
          if d12 < w2 + dw2 then
            a := (w2 + dw2 - d12) / dw2
          else
          begin
            d2x := ix - x2;
            d22 := Sqr(d2x) + Sqr(d2y);
            if d22 < w2 then
              a := 1
            else
            if d22 < w2 + dw2 then
              a := (w2 + dw2 - d22) / dw2
            else
              a := 0;
          end;

          if a > 0 then
          begin
            p := PixelPtr(ix, iy);
            Alpha2d := Alpha2 * a;

            Alpha1d := 1 - Alpha2d;
            p^ := Round((p^ * Alpha1d + ByteColor * Alpha2d));
          end;
        end;
    end;
end;

{ -----= FLOOD FILL =----- }

{ As this algorithm is extremely inefficient, there is only one variant at this
  moment. However, as far as it is an extremely rare use case, maybe, there is
  no actual need to improve it ever. }
procedure TCastleImage.FloodFill(const x, y: integer; const aColor: TCastleColor;
    const aThreshold: single = 0);
var
  OldColor: TCastleColor;
  ix, iy: integer;
  x1, y1, x2, y2: integer;
  aFinished: boolean;
  Flood: array of boolean;
  function isFloodColor(const ax, ay: integer): boolean; {$ifdef FPC}{$IFDEF SUPPORTS_INLINE}Inline;{$ENDIF}{$endif}
  begin
    if (ax >= 0) and (ay >=0) and (ax <= Width) and (ay <= Height) then
    begin
      if Flood[ax + ay * Height] then
        Result := true
      else
        Result := false;
    end
    else
      Result := false;
  end;
  function NearbyFloodColor: boolean;
  begin
    Result := isFloodColor(ix - 1, iy - 1) or isFloodColor(ix, iy - 1)
      or isFloodColor(ix + 1, iy - 1) or isFloodColor(ix - 1, iy)
      or isFloodColor(ix + 1, iy) or isFloodColor(ix - 1, iy + 1)
      or isFloodColor(ix, iy + 1) or isFloodColor(ix + 1, iy + 1);
  end;
begin
  x1 := x - 1;
  x2 := x + 1;
  y1 := y - 1;
  y2 := y + 1;
  SetLength(Flood, Width * Height);
  for ix := Low(Flood) to High(Flood) do Flood[ix] := false;
  OldColor := Colors[x, y, 0]; {grab the OldColor}
  Flood[x + y * Height] := true; {init the flood seed}
  repeat
    aFinished := true;
    if x1 < 0 then x1 := 0;
    if x1 >= Width then x1 := Width - 1;
    if y1 < 0 then y1 := 0;
    if y1 >= Height then y1 := Height - 1;
    for ix := x1 to x2 do
      for iy := y1 to y2 do
        if not Flood[ix + iy * Height] and
          ((Colors[ix, iy, 0] - OldColor).Length < aThreshold) and
          NearbyFloodColor then
        begin
          Flood[ix + iy * Height] := true;
          aFinished := false;
          if (ix = x1) then dec(x1);
          if (ix = x2) then inc(x2);
          if (iy = y1) then dec(y1);
          if (iy = y2) then inc(y2);
        end;
  until aFinished;
  //and paint the flood with the aColor
  for ix := x1 to x2 do
    for iy := y1 to y2 do
      if Flood[ix + iy * Height] then
        Colors[ix, iy, 0] := aColor;
end;

