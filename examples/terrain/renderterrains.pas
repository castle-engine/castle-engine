{
  Copyright 2009-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Specialized terrain rendering in OpenGL. }
unit RenderTerrains;

{$I castleconf.inc}

interface

uses CastleVectors, CastleTerrain;

{ Drawing of TTerrain (relies only on TTerrain.Height method).

  When LayersCount > 1, this draws terrain with layers
  (ultra-simplified geometry clipmaps).

  BaseSize * 2 is the size of the first (most detailed) square layer around
  the (MiddleX, MiddleY). Eeach successive layer has the same subdivision
  (although with middle square removed, as it's already done by
  previous layer) and 2 times larger size.

  Uses currently enabled GLSL program (or none), so be sure
  to assign CurrentProgram before calling this. }
procedure DrawTerrain(Terrain: TTerrain;
  const Subdivision: Cardinal;
  MiddleX, MiddleY: Single; BaseSize: Single;
  const LayersCount: Cardinal);

const
  { Scale grid coords to nicely fit in similar box like DrawTerrain produces.
    This should be set for TTerrainGrid, to affect TTerrainGrid.Height.
    It's also used by DrawGrid, to produce the same sized grid. }
  GridX1 = -1;
  GridY1 = -1;
  GridX2 = 1;
  GridY2 = 1;
  GridHeightScale = 0.0002;

{ Specialized drawing for TTerrainGrid, that displays only the
  precise grid points. }
procedure DrawGrid(Grid: TTerrainGrid);

procedure RenderTerrainsOpenGL;
procedure RenderTerrainsCloseGL;

{ Returns same colors as used by DrawTerrain. }
function ColorFromHeight(Terrain: TTerrain; Height: Single): TVector3Single;

implementation

uses CastleGL, CastleGLUtils, CastleUtils, SysUtils;

var
  TerrainVbo: TGLuint;
  TerrainIndexVbo: TGLuint;

procedure RenderTerrainsOpenGL;
begin
  if not GLFeatures.VertexBufferObject then
    raise Exception.Create('VBO support is required');
  glGenBuffers(1, @TerrainVbo);
  glGenBuffers(1, @TerrainIndexVbo);
end;

procedure RenderTerrainsCloseGL;
begin
  glFreeBuffer(TerrainVbo);
  glFreeBuffer(TerrainIndexVbo);
end;

function ColorFromHeightCore(const H: Single): TVector3Single;
begin
  { Colors strategy from http://www.ii.uni.wroc.pl/~anl/dyd/PGK/pracownia.html }
  if      (H < 0  )  then Result := Vector3Single(0,       0,         1) { blue }
  else if (H < 500)  then Result := Vector3Single(0,       H/500,     0) { green }
  else if (H < 1000) then Result := Vector3Single(H/500-1, 1,         0) { yellow }
  else if (H < 1500) then Result := Vector3Single(1,       H/500-2.0, 0) { red }
  else Result := Vector3Single(1, 1, 1);                                 { white }
end;

function ColorFromHeight(Terrain: TTerrain; Height: Single): TVector3Single;
begin
  if Terrain is TTerrainGrid then
  begin
    { For TTerrainGrid, Height is original GridHeight result. }
    Height /= GridHeightScale;
  end else
  begin
    { scale height down by Amplitude, to keep nice colors regardless of Amplitude }
    if Terrain is TTerrainNoise then
      Height /= TTerrainNoise(Terrain).Amplitude;
    { some hacks to hit interesting colors }
    Height := Height  * 2000 - 1000;
  end;

  Result := ColorFromHeightCore(Height);
end;

type
  TTerrainPoint = packed record
    Position, Normal, Color: TVector3Single;
  end;
  PTerrainPoint = ^TTerrainPoint;

var
  { Array for terrain points and indexes.

    Initially, when still using OpenGL immediate mode, this was useful to
    calculate all terrain points *once* before passing them to OpenGL
    (otherwise quad strips would calculate all twice).
    Then it was also useful to calculate normal vectors based on positions.

    Finally, now this is just send into OpenGL VBO. }
  Points: array of TTerrainPoint;
  PointsIndex: array of TGLuint;
  TrisIndex: array of TGLuint;

procedure DrawTerrainLayer(Terrain: TTerrain; const Subdivision: Cardinal;
  const X1, Y1, X2, Y2: Single; Hole, BorderTriangles: boolean);
var
  CountSteps, CountSteps1, CountStepsQ: Cardinal;

  procedure CalculatePositionColor(var P: TTerrainPoint; const I, J: Cardinal);
  begin
    { set XY to cover (X1, Y1) ... (X2, Y2) rectangle with our terrain }
    P.Position[0] := (X2 - X1) * I / (CountSteps-1) + X1;
    P.Position[1] := (Y2 - Y1) * J / (CountSteps-1) + Y1;

    P.Position[2] := Terrain.Height(P.Position[0], P.Position[1]);

    P.Color := ColorFromHeight(Terrain, P.Position[2]);
  end;

  procedure CalculateNormal(const I, J: Cardinal);
  var
    P, PX, PY: PTerrainPoint;
  begin
    P  := @(Points[ I      * CountSteps1 + J]);
    PX := @(Points[(I + 1) * CountSteps1 + J]);
    PY := @(Points[ I      * CountSteps1 + J + 1]);

    { TODO: this is actually normal vector of 1 of the four faces around this
      vertex. Optimally, we should calculate normals on all faces,
      and for vertex normal take average. }
    P^.Normal := (PX^.Position - P^.Position) ><
                 (PY^.Position - P^.Position);
  end;

var
  I, J: Cardinal;
  P: PTerrainPoint;
  Index: PGLuint;
begin
  { CountSteps-1 squares (edges) along the way,
    CountSteps points along the way.
    Calculate positions for CountSteps + 1 points
    (+ 1 additional for normal calculation).
    We want CountSteps-1 to be divisible by 4, for Hole rendering. }
  CountSteps := 1 shl Subdivision + 1;
  CountSteps1 := CountSteps + 1;
  { Quarter of CountSteps for sQuares }
  CountStepsQ := 1 shl (Subdivision - 2);

  { We will render CountSteps^2 points, but we want to calculate
    (CountSteps + 1)^2 points : to be able to calculate normal vectors.
    Normals for the last row and last column will not be calculated,
    and will not be used. }
  SetLength(Points, Sqr(CountSteps1));

  if Hole then
  begin
    { calculate Points and Colors }
    for I := 0 to CountStepsQ + 1 do
    begin
      P := @(Points[I * CountSteps1]);
      for J := 0 to CountSteps do
      begin
        { calculate P^, which is Points.List^[I * CountSteps1 + J] }
        CalculatePositionColor(P^, I, J);
        Inc(P);
      end;
    end;

    for I := CountStepsQ + 2 to CountStepsQ * 3 - 1 do
    begin
      P := @(Points[I * CountSteps1]);
      for J := 0 to CountStepsQ + 1 do
      begin
        { calculate P^, which is Points.List^[I * CountSteps1 + J] }
        CalculatePositionColor(P^, I, J);
        Inc(P);
      end;

      P := @(Points[I * CountSteps1 + CountStepsQ * 3]);
      for J := CountStepsQ * 3 to CountSteps do
      begin
        { calculate P^, which is Points.List^[I * CountSteps1 + J] }
        CalculatePositionColor(P^, I, J);
        Inc(P);
      end;
    end;

    for I := CountStepsQ * 3 to CountSteps do
    begin
      P := @(Points[I * CountSteps1]);
      for J := 0 to CountSteps do
      begin
        { calculate P^, which is Points.List^[I * CountSteps1 + J] }
        CalculatePositionColor(P^, I, J);
        Inc(P);
      end;
    end;

    { calculate Normals }
    for I := 0 to CountStepsQ do
      for J := 0 to CountSteps - 1 do
        CalculateNormal(I, J);

    for I := CountStepsQ + 1 to CountStepsQ * 3 - 1 do
    begin
      for J := 0 to CountStepsQ do
        CalculateNormal(I, J);
      for J := CountStepsQ * 3 to CountSteps - 1 do
        CalculateNormal(I, J);
    end;

    for I := CountStepsQ * 3 to CountSteps - 1 do
      for J := 0 to CountSteps - 1 do
        CalculateNormal(I, J);
  end else
  begin
    { calculate Points and Colors }
    P := PTerrainPoint(Points);
    for I := 0 to CountSteps do
      for J := 0 to CountSteps do
      begin
        { calculate P^, which is Points.List^[I * CountSteps1 + J] }
        CalculatePositionColor(P^, I, J);
        Inc(P);
      end;

    { calculate Normals }
    for I := 0 to CountSteps - 1 do
      for J := 0 to CountSteps - 1 do
        CalculateNormal(I, J);
  end;

  { calculate PointsIndex }
  SetLength(PointsIndex, (CountSteps - 1) * CountSteps * 2);
  Index := PGLuint(PointsIndex);
  for I := 1 to CountSteps - 1 do
    for J := 0 to CountSteps - 1 do
    begin
      Index^ := (I - 1) * CountSteps1 + J; Inc(Index);
      Index^ :=  I      * CountSteps1 + J; Inc(Index);
    end;

  { load Points into VBO, render }

  glBindBuffer(GL_ARRAY_BUFFER, TerrainVbo);
  glBufferData(GL_ARRAY_BUFFER, Length(Points) * SizeOf(TTerrainPoint),
    Pointer(Points), GL_STREAM_DRAW);

  glEnableClientState(GL_VERTEX_ARRAY);
  glVertexPointer(3, GL_FLOAT, SizeOf(TTerrainPoint), Offset(Points[0].Position, Points[0]));

  glEnableClientState(GL_NORMAL_ARRAY);
  glNormalPointer(GL_FLOAT, SizeOf(TTerrainPoint), Offset(Points[0].Normal, Points[0]));

  glEnableClientState(GL_COLOR_ARRAY);
  glColorPointer(3, GL_FLOAT, SizeOf(TTerrainPoint), Offset(Points[0].Color, Points[0]));

  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, TerrainIndexVbo);
  glBufferData(GL_ELEMENT_ARRAY_BUFFER, Length(PointsIndex) * SizeOf(TGLuint),
    Pointer(PointsIndex), GL_STREAM_DRAW);

  Assert(CountStepsQ * 4 - 1 = CountSteps - 2);

  if Hole then
  begin
    for I := 0 to CountStepsQ - 1 do
      glDrawElements(GL_QUAD_STRIP, CountSteps * 2, GL_UNSIGNED_INT,
        Pointer(CountSteps * 2 * I * SizeOf(TGLuint)));

    for I := CountStepsQ to CountStepsQ * 3 - 1  do
    begin
      glDrawElements(GL_QUAD_STRIP, (CountStepsQ+1) * 2, GL_UNSIGNED_INT,
        Pointer(CountSteps * 2 * I * SizeOf(TGLuint)));
      glDrawElements(GL_QUAD_STRIP, (CountStepsQ+1) * 2, GL_UNSIGNED_INT,
        Pointer((CountSteps * 2 * I + CountStepsQ*3*2) * SizeOf(TGLuint)));
    end;

    for I := CountStepsQ * 3 to CountStepsQ * 4 - 1 do
      glDrawElements(GL_QUAD_STRIP, CountSteps * 2, GL_UNSIGNED_INT,
        Pointer(CountSteps * 2 * I * SizeOf(TGLuint)));
  end else
  begin
    for I := 0 to CountSteps - 2 do
      glDrawElements(GL_QUAD_STRIP, CountSteps * 2, GL_UNSIGNED_INT,
        Pointer(CountSteps * 2 * I * SizeOf(TGLuint)));
  end;

  if BorderTriangles then
  begin
    SetLength(TrisIndex, ((CountSteps - 1) div 2) * 3 * 4);
    Index := PGLuint(TrisIndex);
    for I := 0 to (CountSteps - 1) div 2 - 1 do
    begin
      Index^ := I*2;     Inc(Index);
      Index^ := I*2 + 1; Inc(Index);
      Index^ := I*2 + 2; Inc(Index);

      Index^ := (CountSteps-1)*CountSteps1 + I*2;     Inc(Index);
      Index^ := (CountSteps-1)*CountSteps1 + I*2 + 1; Inc(Index);
      Index^ := (CountSteps-1)*CountSteps1 + I*2 + 2; Inc(Index);

      Index^ := (I*2    )*CountSteps1; Inc(Index);
      Index^ := (I*2 + 1)*CountSteps1; Inc(Index);
      Index^ := (I*2 + 2)*CountSteps1; Inc(Index);

      Index^ := CountSteps-1 + (I*2    )*CountSteps1; Inc(Index);
      Index^ := CountSteps-1 + (I*2 + 1)*CountSteps1; Inc(Index);
      Index^ := CountSteps-1 + (I*2 + 2)*CountSteps1; Inc(Index);
    end;

    glBufferData(GL_ELEMENT_ARRAY_BUFFER, Length(TrisIndex) * SizeOf(TGLuint),
      Pointer(TrisIndex), GL_STREAM_DRAW);
    glDrawElements(GL_TRIANGLES, Length(TrisIndex), GL_UNSIGNED_INT, nil);
  end;

  glDisableClientState(GL_VERTEX_ARRAY);
  glDisableClientState(GL_NORMAL_ARRAY);
  glDisableClientState(GL_COLOR_ARRAY);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
end;

procedure DrawTerrain(Terrain: TTerrain;
  const Subdivision: Cardinal;
  MiddleX, MiddleY: Single; BaseSize: Single;
  const LayersCount: Cardinal);
const
  RoundGridCell = 0.5;
var
  Layer: Cardinal;
  X1, Y1, X2, Y2: Single;
begin
  { to somewhat cure the effect of terrain "flowing" (because every small
    change of Middle point shifts all the points), round middle to
    some cell size. }
  MiddleX := Round(MiddleX / RoundGridCell) * RoundGridCell;
  MiddleY := Round(MiddleY / RoundGridCell) * RoundGridCell;
  X1 := MiddleX - BaseSize;
  Y1 := MiddleY - BaseSize;
  X2 := MiddleX + BaseSize;
  Y2 := MiddleY + BaseSize;
  for Layer := 0 to LayersCount - 1 do
  begin
    DrawTerrainLayer(Terrain, Subdivision, X1, Y1, X2, Y2,
      Layer <> 0, Layer < LayersCount - 1);
    X1 -= BaseSize;
    Y1 -= BaseSize;
    X2 += BaseSize;
    Y2 += BaseSize;
    BaseSize *= 2;
  end;
end;

procedure DrawGrid(Grid: TTerrainGrid);

  procedure Vertex(I, J: Cardinal);
  var
    HForColor: Single;
  begin
    HForColor := Grid.GridHeight(I, J);
    glColorv(ColorFromHeightCore(HForColor));

    glVertexv(Vector3Single(
      (GridX2 - GridX1) * I / Grid.GridSizeX + GridX1,
      (GridY2 - GridY1) * J / Grid.GridSizeY + GridY1,
      HForColor * GridHeightScale));
  end;

const
  Step = 10;
var
  I, J: Cardinal;
begin
  I := Step;
  while I < Grid.GridSizeX do
  begin
    glBegin(GL_QUAD_STRIP);
      J := 0;
      while J < Grid.GridSizeY do
      begin
        Vertex(I - Step, J);
        Vertex(I       , J);
        Inc(J, Step);
      end;
    glEnd();
    Inc(I, Step);
  end;
end;

end.
