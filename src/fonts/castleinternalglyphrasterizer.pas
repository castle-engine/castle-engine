{
  Copyright 2026-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Rasterize glyph outlines (TGlyphOutline) into bitmaps, in pure Pascal.

  The result follows FreeType conventions (bitmap rows go top-down,
  Left and Top specify the bitmap position relative to the glyph origin,
  monochrome bitmaps have 1 bit per pixel, most significant bit first).

  Anti-aliased rendering calculates exact pixel coverage by the outline
  (accumulating signed areas covered by the outline edges).
  Monochrome rendering sets pixels with centers inside the outline,
  with dropout control (thin strokes do not disappear), similar to FreeType.

  There is no hinting, so the glyphs are not aligned to the pixel grid.
  This makes them a bit softer than FreeType output at small sizes.

  -----------------------------------------------------------------------------

  Disclosure: This file is largely Claude-generated.
  It was reviewed and slightly edited by Michalis, but this is still a
  (unusually, for Castle Game Engine) large automatically generated file. }
unit CastleInternalGlyphRasterizer;

{$I castleconf.inc}

interface

uses SysUtils, CastleInternalOpenTypeFont;

type
  { Glyph rendered to a bitmap. }
  TRasterizedGlyph = record
    { Size of the bitmap, in pixels. }
    Width, Height: Integer;
    { Position of the bitmap left-top corner relative to the glyph origin,
      in pixels. Top is measured up from the baseline
      (so the bitmap spans Top - Height ... Top vertically). }
    Left, Top: Integer;
    { Bytes per row. }
    Pitch: Integer;
    { Monochrome (1 bit per pixel) or anti-aliased (1 byte per pixel,
      0..255) bitmap. }
    Mono: Boolean;
    { Bitmap data, rows go top-down. Empty if Width or Height are 0. }
    Data: TBytes;
  end;

{ Render glyph outline (in font units) scaled by Scale (pixels per font unit),
  with the glyph origin moved by (OriginX, OriginY) pixels. }
procedure RasterizeGlyph(const Outline: TGlyphOutline;
  const Scale, OriginX, OriginY: Double; const Mono: Boolean;
  out Glyph: TRasterizedGlyph);

implementation

uses Math;

const
  { Max distance of flattened lines from the curves, in pixels. }
  Flatness = 0.1;
  MaxCurveSegments = 100;
  { Protect from invalid fonts (and absurd sizes) causing huge allocations. }
  MaxBitmapSize = 4096;
  { Protect from invalid fonts with absurd coordinates (in pixels). }
  MaxCoordinate = 1000000;

type
  TDoubleArray = array of Double;

  { Polygons in bitmap coordinates (x to the right, y down).
    Each contour is closed implicitly. }
  TPolygons = record
    X, Y: TDoubleArray;
    Count: Integer;
    ContourStarts: array of Integer;
    ContourCount: Integer;
  end;

  { Scanline crossings, see ScanSpans. }
  TCrossings = record
    Pos: TDoubleArray;
    Winding: array of Integer;
    Count: Integer;
  end;

procedure AddPoint(var Polygons: TPolygons; const X, Y: Double);
begin
  if Polygons.Count >= Length(Polygons.X) then
  begin
    SetLength(Polygons.X, Max(64, Length(Polygons.X) * 2));
    SetLength(Polygons.Y, Length(Polygons.X));
  end;
  Polygons.X[Polygons.Count] := X;
  Polygons.Y[Polygons.Count] := Y;
  Inc(Polygons.Count);
end;

procedure StartContour(var Polygons: TPolygons);
begin
  if Polygons.ContourCount >= Length(Polygons.ContourStarts) then
    SetLength(Polygons.ContourStarts, Max(8, Length(Polygons.ContourStarts) * 2));
  Polygons.ContourStarts[Polygons.ContourCount] := Polygons.Count;
  Inc(Polygons.ContourCount);
end;

{ Index of the last point of given contour. }
function ContourEnd(const Polygons: TPolygons; const Contour: Integer): Integer;
begin
  if Contour + 1 < Polygons.ContourCount then
    Result := Polygons.ContourStarts[Contour + 1] - 1
  else
    Result := Polygons.Count - 1;
end;

{ Largest integer value <= V, as Double (safe for values outside of Integer range,
  unlike Math.Floor). }
function FloorDouble(const V: Double): Double;
begin
  Result := Int(V);
  if Result > V then
    Result := Result - 1;
end;

{ Quantize to 1/64 pixel, like FreeType 26.6 coordinates. }
function Round26(const V: Double): Double;
begin
  Result := FloorDouble(V * 64 + 0.5) / 64;
end;

procedure RasterizeGlyph(const Outline: TGlyphOutline;
  const Scale, OriginX, OriginY: Double; const Mono: Boolean;
  out Glyph: TRasterizedGlyph);
var
  { Bitmap placement for anti-aliased rendering (and the area we calculate). }
  BoxLeft, BoxTop, W, H: Integer;
  Polygons: TPolygons;

  { Transform point from font units to bitmap coordinates. }
  procedure TransformPoint(const X, Y: Double; out PX, PY: Double);
  begin
    PX := Round26(X * Scale) + OriginX - BoxLeft;
    PY := BoxTop - (Round26(Y * Scale) + OriginY);
  end;

  procedure Flatten;
  var
    I, J, N: Integer;
    P0X, P0Y, P1X, P1Y, P2X, P2Y, P3X, P3Y: Double;
    DDX, DDY, DD, DD2, T, MT: Double;
  begin
    Polygons.Count := 0;
    Polygons.ContourCount := 0;
    P0X := 0;
    P0Y := 0;
    for I := 0 to Outline.Count - 1 do
    begin
      case Outline.Commands[I].Kind of
        ocMove:
          begin
            StartContour(Polygons);
            TransformPoint(Outline.Commands[I].X[0], Outline.Commands[I].Y[0], P0X, P0Y);
            AddPoint(Polygons, P0X, P0Y);
          end;
        ocLine:
          begin
            if Polygons.ContourCount = 0 then
              StartContour(Polygons);
            TransformPoint(Outline.Commands[I].X[0], Outline.Commands[I].Y[0], P0X, P0Y);
            AddPoint(Polygons, P0X, P0Y);
          end;
        ocQuad:
          begin
            if Polygons.ContourCount = 0 then
              StartContour(Polygons);
            TransformPoint(Outline.Commands[I].X[0], Outline.Commands[I].Y[0], P1X, P1Y);
            TransformPoint(Outline.Commands[I].X[1], Outline.Commands[I].Y[1], P2X, P2Y);
            { max distance between curve and N uniform segments is |P0 - 2P1 + P2| / (4 N^2) }
            DDX := P0X - 2 * P1X + P2X;
            DDY := P0Y - 2 * P1Y + P2Y;
            DD := Sqrt(DDX * DDX + DDY * DDY);
            N := Ceil(Sqrt(DD / (4 * Flatness)));
            N := Max(1, Min(N, MaxCurveSegments));
            for J := 1 to N do
            begin
              T := J / N;
              MT := 1 - T;
              AddPoint(Polygons,
                MT * MT * P0X + 2 * MT * T * P1X + T * T * P2X,
                MT * MT * P0Y + 2 * MT * T * P1Y + T * T * P2Y);
            end;
            P0X := P2X;
            P0Y := P2Y;
          end;
        ocCubic:
          begin
            if Polygons.ContourCount = 0 then
              StartContour(Polygons);
            TransformPoint(Outline.Commands[I].X[0], Outline.Commands[I].Y[0], P1X, P1Y);
            TransformPoint(Outline.Commands[I].X[1], Outline.Commands[I].Y[1], P2X, P2Y);
            TransformPoint(Outline.Commands[I].X[2], Outline.Commands[I].Y[2], P3X, P3Y);
            { max distance between curve and N uniform segments
              is at most 3 * max(|P0 - 2P1 + P2|, |P1 - 2P2 + P3|) / (4 N^2) }
            DDX := P0X - 2 * P1X + P2X;
            DDY := P0Y - 2 * P1Y + P2Y;
            DD := Sqrt(DDX * DDX + DDY * DDY);
            DDX := P1X - 2 * P2X + P3X;
            DDY := P1Y - 2 * P2Y + P3Y;
            DD2 := Sqrt(DDX * DDX + DDY * DDY);
            if DD2 > DD then
              DD := DD2;
            N := Ceil(Sqrt(3 * DD / (4 * Flatness)));
            N := Max(1, Min(N, MaxCurveSegments));
            for J := 1 to N do
            begin
              T := J / N;
              MT := 1 - T;
              AddPoint(Polygons,
                MT * MT * MT * P0X + 3 * MT * MT * T * P1X + 3 * MT * T * T * P2X + T * T * T * P3X,
                MT * MT * MT * P0Y + 3 * MT * MT * T * P1Y + 3 * MT * T * T * P2Y + T * T * T * P3Y);
            end;
            P0X := P3X;
            P0Y := P3Y;
          end;
      end;
    end;
  end;

var
  { Anti-aliased rendering: accumulation buffer, with W * H + 1 items.
    Rows go top-down, one item for each pixel, and a "spill" item at the end.

    We add here signed coverage contributions of each outline edge,
    such that the running sum over the buffer (from the first item)
    is the signed area covered by the outline in each pixel.

    The signed area is positive or negative depending on the edge direction,
    so that (for correct fonts) the overlapping or nested contours sum
    correctly (with nonzero winding rule, we just clamp the absolute value
    to 1). }
  Acc: TDoubleArray;

  { Add to Acc a piece of an edge that is within the pixel row Row,
    going horizontally from XA to XB (in any order),
    and vertically covering height DY of the row (signed by edge direction).

    For a part of the piece within a single pixel column C,
    horizontally from A to B, the area covered to the right of it
    (inside this pixel) is DY * (1 - M), where M = (A + B) / 2 - C
    is the horizontal middle of the part relative to the pixel.
    All pixels further to the right are covered by the full DY.
    So we add DY * (1 - M) to the pixel C, and the remaining DY * M
    to the pixel C + 1 (from which the running sum carries it to the right).
    A piece crossing multiple pixel columns is split at the pixel boundaries,
    each part gets DY proportional to its horizontal length. }
  procedure AddRowPiece(const Row: Integer; XA, XB: Double; const DY: Double);
  var
    RowStart, Column: Integer;
    WidthF, PieceWidth, X, NextX, PartDY, M, T: Double;
  begin
    RowStart := Row * W;
    if XA > XB then
    begin
      T := XA;
      XA := XB;
      XB := T;
    end;

    { Clamp to the bitmap horizontally.
      Parts left of the bitmap cover the whole row, so they can be treated
      as being on the left edge. Parts right of the bitmap don't cover
      any pixel, they only go to the spill item (start of the next row),
      which is correct for the running sum. }
    WidthF := W;
    if XA < 0 then XA := 0;
    if XA > WidthF then XA := WidthF;
    if XB < 0 then XB := 0;
    if XB > WidthF then XB := WidthF;

    Column := Floor(XA);
    if Column > W - 1 then
      Column := W - 1;

    PieceWidth := XB - XA;
    if PieceWidth < 1e-12 then
    begin
      { vertical piece }
      M := XA - Column;
      Acc[RowStart + Column] := Acc[RowStart + Column] + DY * (1 - M);
      Acc[RowStart + Column + 1] := Acc[RowStart + Column + 1] + DY * M;
      Exit;
    end;

    X := XA;
    while X < XB do
    begin
      NextX := Column + 1;
      if NextX > XB then
        NextX := XB;
      PartDY := DY * (NextX - X) / PieceWidth;
      M := (X + NextX) / 2 - Column;
      Acc[RowStart + Column] := Acc[RowStart + Column] + PartDY * (1 - M);
      Acc[RowStart + Column + 1] := Acc[RowStart + Column + 1] + PartDY * M;
      X := NextX;
      Inc(Column);
    end;
  end;

  { Add to Acc an outline edge from (X0, Y0) to (X1, Y1),
    splitting it into pieces for each pixel row. }
  procedure AddLine(const X0, Y0, X1, Y1: Double);
  var
    Sign, TopX, TopY, BottomX, BottomY, YStart, YEnd, Slope,
      PieceTop, PieceBottom: Double;
    Row: Integer;
  begin
    if Y0 = Y1 then
      Exit; // horizontal edges don't cover anything
    if Y0 < Y1 then
    begin
      Sign := 1;
      TopX := X0;
      TopY := Y0;
      BottomX := X1;
      BottomY := Y1;
    end else
    begin
      Sign := -1;
      TopX := X1;
      TopY := Y1;
      BottomX := X0;
      BottomY := Y0;
    end;

    { clip vertically to the bitmap }
    YStart := TopY;
    if YStart < 0 then
      YStart := 0;
    YEnd := BottomY;
    if YEnd > H then
      YEnd := H;
    if YStart >= YEnd then
      Exit;

    Slope := (BottomX - TopX) / (BottomY - TopY);
    Row := Floor(YStart);
    while (Row < H) and (Row < YEnd) do
    begin
      PieceTop := Row;
      if PieceTop < YStart then
        PieceTop := YStart;
      PieceBottom := Row + 1;
      if PieceBottom > YEnd then
        PieceBottom := YEnd;
      if PieceBottom > PieceTop then
        AddRowPiece(Row,
          TopX + (PieceTop - TopY) * Slope,
          TopX + (PieceBottom - TopY) * Slope,
          Sign * (PieceBottom - PieceTop));
      Inc(Row);
    end;
  end;

  procedure RenderAntiAliased;
  var
    C, I, J, E, Next: Integer;
    Sum, V: Double;
  begin
    SetLength(Acc, W * H + 1);
    for I := 0 to Length(Acc) - 1 do
      Acc[I] := 0;
    for C := 0 to Polygons.ContourCount - 1 do
    begin
      E := ContourEnd(Polygons, C);
      for J := Polygons.ContourStarts[C] to E do
      begin
        if J = E then
          Next := Polygons.ContourStarts[C]
        else
          Next := J + 1;
        AddLine(Polygons.X[J], Polygons.Y[J], Polygons.X[Next], Polygons.Y[Next]);
      end;
    end;

    Glyph.Width := W;
    Glyph.Height := H;
    Glyph.Left := BoxLeft;
    Glyph.Top := BoxTop;
    Glyph.Pitch := W;
    Glyph.Mono := false;
    SetLength(Glyph.Data, W * H);
    Sum := 0;
    for I := 0 to W * H - 1 do
    begin
      Sum := Sum + Acc[I];
      V := Abs(Sum);
      if V > 1 then
        V := 1;
      Glyph.Data[I] := Trunc(V * 255 + 0.5);
    end;
  end;

var
  Crossings: TCrossings;

  { Intersect polygons with a scanline, collect crossings sorted by position.
    Horizontal scanline (Vertical = false) is the line y = Coord,
    crossings are positions in x.
    Vertical scanline (Vertical = true) is the line x = Coord,
    crossings are positions in y. }
  procedure ScanCrossings(const Coord: Double; const Vertical: Boolean);
  var
    C, J, E, Next, Wind, I, K: Integer;
    A0, B0, A1, B1, P, TPos: Double;
    TWind: Integer;
  begin
    Crossings.Count := 0;
    for C := 0 to Polygons.ContourCount - 1 do
    begin
      E := ContourEnd(Polygons, C);
      for J := Polygons.ContourStarts[C] to E do
      begin
        if J = E then
          Next := Polygons.ContourStarts[C]
        else
          Next := J + 1;
        { A is the coordinate along the scanline, B across it }
        if Vertical then
        begin
          A0 := Polygons.Y[J]; B0 := Polygons.X[J];
          A1 := Polygons.Y[Next]; B1 := Polygons.X[Next];
        end else
        begin
          A0 := Polygons.X[J]; B0 := Polygons.Y[J];
          A1 := Polygons.X[Next]; B1 := Polygons.Y[Next];
        end;
        if B0 = B1 then
          Continue;
        if B0 < B1 then
        begin
          if (Coord < B0) or (Coord >= B1) then
            Continue;
          Wind := 1;
        end else
        begin
          if (Coord < B1) or (Coord >= B0) then
            Continue;
          Wind := -1;
        end;
        P := A0 + (Coord - B0) * (A1 - A0) / (B1 - B0);
        if Crossings.Count >= Length(Crossings.Pos) then
        begin
          SetLength(Crossings.Pos, Max(16, Length(Crossings.Pos) * 2));
          SetLength(Crossings.Winding, Length(Crossings.Pos));
        end;
        Crossings.Pos[Crossings.Count] := P;
        Crossings.Winding[Crossings.Count] := Wind;
        Inc(Crossings.Count);
      end;
    end;

    { insertion sort, the number of crossings is small }
    for I := 1 to Crossings.Count - 1 do
    begin
      TPos := Crossings.Pos[I];
      TWind := Crossings.Winding[I];
      K := I - 1;
      while (K >= 0) and (Crossings.Pos[K] > TPos) do
      begin
        Crossings.Pos[K + 1] := Crossings.Pos[K];
        Crossings.Winding[K + 1] := Crossings.Winding[K];
        Dec(K);
      end;
      Crossings.Pos[K + 1] := TPos;
      Crossings.Winding[K + 1] := TWind;
    end;
  end;

var
  { Monochrome rendering: set pixels in the W * H area, rows top-down. }
  PixelsOn: array of Boolean;

  { Process all filled spans (nonzero winding rule) of the Crossings.
    For horizontal scanline, Line is the row, for vertical it's the column. }
  procedure ProcessSpans(const Line: Integer; const Vertical: Boolean);
  var
    I, Winding, Before, First, Last, Pixel, Limit: Integer;
    SpanStart, SpanEnd: Double;
  begin
    if Vertical then
      Limit := H
    else
      Limit := W;
    Winding := 0;
    SpanStart := 0;
    for I := 0 to Crossings.Count - 1 do
    begin
      Before := Winding;
      Winding := Winding + Crossings.Winding[I];
      if (Before = 0) and (Winding <> 0) then
        SpanStart := Crossings.Pos[I]
      else
      if (Before <> 0) and (Winding = 0) then
      begin
        SpanEnd := Crossings.Pos[I];
        { pixels with centers in [SpanStart, SpanEnd) }
        First := Ceil(SpanStart - 0.5);
        Last := Ceil(SpanEnd - 0.5) - 1;
        if First <= Last then
        begin
          { vertical scanlines are only used for dropout control }
          if not Vertical then
            for Pixel := Max(First, 0) to Min(Last, Limit - 1) do
              PixelsOn[Line * W + Pixel] := true;
        end else
        begin
          { dropout control: span contains no pixel center,
            set the pixel in the middle of the span }
          Pixel := Floor((SpanStart + SpanEnd) / 2);
          if (Pixel >= 0) and (Pixel < Limit) then
          begin
            if Vertical then
              PixelsOn[Pixel * W + Line] := true
            else
              PixelsOn[Line * W + Pixel] := true;
          end;
        end;
      end;
    end;
  end;

  procedure RenderMono(const XMin, YMin, XMax, YMax: Double);
  var
    I, GX, GY, PX, PY, MLeft, MRight, MBottom, MTop, MW, MH, Row, Col, Pitch: Integer;
  begin
    SetLength(PixelsOn, W * H);
    for I := 0 to W * H - 1 do
      PixelsOn[I] := false;
    { horizontal scanlines through pixel centers }
    for GY := 0 to H - 1 do
    begin
      ScanCrossings(GY + 0.5, false);
      ProcessSpans(GY, false);
    end;
    { vertical scanlines, for dropout control }
    for GX := 0 to W - 1 do
    begin
      ScanCrossings(GX + 0.5, true);
      ProcessSpans(GX, true);
    end;

    { Bitmap box with rounded coordinates (like FreeType does for mono). }
    MLeft := Floor(XMin + 0.5);
    MRight := Floor(XMax + 0.5);
    MBottom := Floor(YMin + 0.5);
    MTop := Floor(YMax + 0.5);
    if MRight <= MLeft then
    begin
      MLeft := Floor(XMin);
      MRight := MLeft + 1;
    end;
    if MTop <= MBottom then
    begin
      MBottom := Floor(YMin);
      MTop := MBottom + 1;
    end;
    { Extend the box to include all set pixels
      (dropout control may set pixels outside of the rounded box). }
    for GY := 0 to H - 1 do
      for GX := 0 to W - 1 do
        if PixelsOn[GY * W + GX] then
        begin
          PX := BoxLeft + GX;
          PY := BoxTop - GY; // pixel covers y range PY - 1 .. PY
          if PX < MLeft then MLeft := PX;
          if PX + 1 > MRight then MRight := PX + 1;
          if PY - 1 < MBottom then MBottom := PY - 1;
          if PY > MTop then MTop := PY;
        end;
    MW := MRight - MLeft;
    MH := MTop - MBottom;
    { pitch rounded to 2 bytes, like FreeType }
    Pitch := ((MW + 15) shr 4) shl 1;

    Glyph.Width := MW;
    Glyph.Height := MH;
    Glyph.Left := MLeft;
    Glyph.Top := MTop;
    Glyph.Pitch := Pitch;
    Glyph.Mono := true;
    SetLength(Glyph.Data, Pitch * MH);
    for I := 0 to Length(Glyph.Data) - 1 do
      Glyph.Data[I] := 0;
    for Row := 0 to MH - 1 do
    begin
      GY := (BoxTop - MTop) + Row;
      if (GY < 0) or (GY >= H) then
        Continue;
      for Col := 0 to MW - 1 do
      begin
        GX := (MLeft - BoxLeft) + Col;
        if (GX < 0) or (GX >= W) then
          Continue;
        if PixelsOn[GY * W + GX] then
          Glyph.Data[Row * Pitch + (Col shr 3)] :=
            Glyph.Data[Row * Pitch + (Col shr 3)] or (Byte($80) shr (Col and 7));
      end;
    end;
  end;

var
  I, J, NumPoints: Integer;
  X, Y, XMin, YMin, XMax, YMax: Double;
  HasPoints: Boolean;
begin
  Glyph.Width := 0;
  Glyph.Height := 0;
  Glyph.Left := 0;
  Glyph.Top := 0;
  Glyph.Pitch := 0;
  Glyph.Mono := Mono;
  Glyph.Data := nil;

  { control box of scaled points, y up }
  HasPoints := false;
  XMin := 0;
  YMin := 0;
  XMax := 0;
  YMax := 0;
  for I := 0 to Outline.Count - 1 do
  begin
    case Outline.Commands[I].Kind of
      ocQuad: NumPoints := 2;
      ocCubic: NumPoints := 3;
      else NumPoints := 1;
    end;
    for J := 0 to NumPoints - 1 do
    begin
      X := Round26(Outline.Commands[I].X[J] * Scale) + OriginX;
      Y := Round26(Outline.Commands[I].Y[J] * Scale) + OriginY;
      if not HasPoints then
      begin
        XMin := X;
        XMax := X;
        YMin := Y;
        YMax := Y;
        HasPoints := true;
      end else
      begin
        if X < XMin then XMin := X;
        if X > XMax then XMax := X;
        if Y < YMin then YMin := Y;
        if Y > YMax then YMax := Y;
      end;
    end;
  end;
  if not HasPoints then
    Exit;

  if (Abs(XMin) > MaxCoordinate) or (Abs(XMax) > MaxCoordinate) or
     (Abs(YMin) > MaxCoordinate) or (Abs(YMax) > MaxCoordinate) or
     (XMax - XMin > MaxBitmapSize) or (YMax - YMin > MaxBitmapSize) then
    raise EOpenTypeFontError.Create('Glyph is too large to render');

  BoxLeft := Floor(XMin);
  BoxTop := Ceil(YMax);
  W := Ceil(XMax) - BoxLeft;
  H := BoxTop - Floor(YMin);
  if W = 0 then
    W := 1;
  if H = 0 then
    H := 1;

  Flatten;
  if Mono then
    RenderMono(XMin, YMin, XMax, YMax)
  else
    RenderAntiAliased;
end;

end.
