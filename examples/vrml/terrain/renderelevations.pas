{
  Copyright 2009-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Rendering elevations (terrains) in OpenGL. }
unit RenderElevations;

interface

uses Elevations;

{ Generic drawing of any TElevation, relies only on TElevation.Height method }
procedure DrawElevation(Elevation: TElevation; const Subdivision: Cardinal);

{ Specialized drawing for TElevationGrid, that displays only the
  precise grid points. }
procedure DrawGrid(Grid: TElevationGrid);

procedure RenderElevationsInitGL;
procedure RenderElevationsCloseGL;

implementation

uses GL, GLU, GLExt, VectorMath, KambiGLUtils, KambiUtils, SysUtils;

var
  ElevationVbo: TGLuint;
  ElevationIndexVbo: TGLuint;

procedure RenderElevationsInitGL;
begin
  if not GL_ARB_vertex_buffer_object then
    raise Exception.Create('ARB_vertex_buffer_object is required');
  glGenBuffersARB(1, @ElevationVbo);
  glGenBuffersARB(1, @ElevationIndexVbo);
end;

procedure RenderElevationsCloseGL;
begin
  glDeleteBuffersARB(1, @ElevationVbo);
  glDeleteBuffersARB(1, @ElevationIndexVbo);
end;

function ColorFromHeight(const H: Single): TVector3Single;
begin
  { Colors strategy from http://www.ii.uni.wroc.pl/~anl/dyd/PGK/pracownia.html }
  if      (H < 0  )  then Result := Vector3Single(0,       0,         1) { blue }
  else if (H < 500)  then Result := Vector3Single(0,       H/500,     0) { green }
  else if (H < 1000) then Result := Vector3Single(H/500-1, 1,         0) { yellow }
  else if (H < 1500) then Result := Vector3Single(1,       H/500-2.0, 0) { red }
  else Result := Vector3Single(1, 1, 1);                                 { white }
end;

type
  TElevationPoint = packed record
    Position, Normal, Color: TVector3Single;
  end;
  PElevationPoint = ^TElevationPoint;

var
  { Array for elevation points and indexes.

    Initially, when still using OpenGL immediate mode, this was useful to
    calculate all elevation points *once* before passing them to OpenGL
    (otherwise quad strips would calculate all twice).
    Then it was also useful to calculate normal vectors based on positions.

    Finally, now this is just send into OpenGL VBO. }
  Points: array of TElevationPoint;
  PointsIndex: array of GLuint;

{ Calculate shift between A and B addresses (in bytes), and cast to Pointer.
  This is simply Result := A - B, except we do some typecasting. }
function Offset(var A, B): Pointer;
begin
  { additional PtrUInt typecast before Pointer, to avoid warning. }
  Result := Pointer(PtrUInt( PtrUInt(@A) - PtrUInt(@B) ));
end;

procedure DrawElevation(Elevation: TElevation; const Subdivision: Cardinal);
var
  CountSteps1: Cardinal;
  CountSteps: Cardinal;
  I, J: Cardinal;
  P, PX, PY: PElevationPoint;
  HForColor: Single;
  Index: PGLuint;
begin
  CountSteps := 1 shl Subdivision;
  CountSteps1 := CountSteps + 1;

  { We will render CountSteps^2 points, but we want to calculate
    (CountSteps + 1)^2 points : to be able to calculate normal vectors.
    Normals for the last row and last column will not be calculated,
    and will not be used. }
  SetLength(Points, Sqr(CountSteps1));

  { calculate Points and Colors }
  P := PElevationPoint(Points);
  for I := 0 to CountSteps do
    for J := 0 to CountSteps do
    begin
      { calculate P^, which is Points.Items[I * CountSteps1 + J] }

      { set XY to cover (-1, -1) ... (1, 1) rectangle with our elevation }
      P^.Position[0] := 2 * I / CountSteps - 1;
      P^.Position[1] := 2 * J / CountSteps - 1;

      P^.Position[2] := Elevation.Height(P^.Position[0], P^.Position[1]);

      HForColor := P^.Position[2];
      { scale height down by Amplitude, to keep nice colors regardless of Amplitude }
      if Elevation is TElevationNoise then
        HForColor /= TElevationNoise(Elevation).Amplitude;
      { some hacks to hit interesting colors }
      HForColor := HForColor  * 2000 - 1000;
      P^.Color := ColorFromHeight(HForColor);

      Inc(P);
    end;

  { calculate Normals }
  for I := 0 to CountSteps - 1 do
    for J := 0 to CountSteps - 1 do
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

  glBindBufferARB(GL_ARRAY_BUFFER_ARB, ElevationVbo);
  glBufferDataARB(GL_ARRAY_BUFFER_ARB, Length(Points) * SizeOf(TElevationPoint),
    Pointer(Points), GL_STREAM_DRAW_ARB);

  glEnableClientState(GL_VERTEX_ARRAY);
  glVertexPointer(3, GL_FLOAT, SizeOf(TElevationPoint), Offset(Points[0].Position, Points[0]));

  glEnableClientState(GL_NORMAL_ARRAY);
  glNormalPointer(GL_FLOAT, SizeOf(TElevationPoint), Offset(Points[0].Normal, Points[0]));

  glEnableClientState(GL_COLOR_ARRAY);
  glColorPointer(3, GL_FLOAT, SizeOf(TElevationPoint), Offset(Points[0].Color, Points[0]));

  glBindBufferARB(GL_ELEMENT_ARRAY_BUFFER_ARB, ElevationIndexVbo);
  glBufferDataARB(GL_ELEMENT_ARRAY_BUFFER_ARB, Length(PointsIndex) * SizeOf(TGLuint),
    Pointer(PointsIndex), GL_STREAM_DRAW_ARB);

  for I := 0 to CountSteps - 2 do
  begin
    glDrawElements(GL_QUAD_STRIP, CountSteps * 2, GL_UNSIGNED_INT,
      Pointer((CountSteps * 2) * SizeOf(TGLuint) * I));
  end;

  glDisableClientState(GL_VERTEX_ARRAY);
  glDisableClientState(GL_NORMAL_ARRAY);
  glDisableClientState(GL_COLOR_ARRAY);
  glBindBufferARB(GL_ARRAY_BUFFER_ARB, 0);
end;

procedure DrawGrid(Grid: TElevationGrid);
const
  { to scale coords to nicely fit in similar box like DrawElevation }
  ScaleSize = 100.0;
  ScaleHeight = 0.01;

  procedure Vertex(I, J: Cardinal);
  var
    HForColor: Single;
  begin
    HForColor := Grid.GridHeight(I, J);
    glColorv(ColorFromHeight(HForColor));
    glVertexv(Vector3Single(
      ScaleSize * (I / Grid.GridSizeX),
      ScaleSize * (J / Grid.GridSizeY),
      HForColor * ScaleHeight));
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
