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

var
  Subdivision: Cardinal = 6;

{ Generic drawing of any TElevation, relies only on TElevation.Height method }
procedure DrawElevation(Elevation: TElevation);

{ Specialized drawing for TElevationGrid, that displays only the
  precise grid points. }
procedure DrawGrid(Grid: TElevationGrid);

implementation

uses GL, GLU, GLExt, VectorMath, KambiGLUtils, KambiUtils, SysUtils;

procedure ColorFromHeight(const H: Single);
begin
  { Colors strategy from http://www.ii.uni.wroc.pl/~anl/dyd/PGK/pracownia.html }
  if      (H < 0  )  then glColor3f(0,       0,         1) { blue }
  else if (H < 500)  then glColor3f(0,       H/500,     0) { green }
  else if (H < 1000) then glColor3f(H/500-1, 1,         0) { yellow }
  else if (H < 1500) then glColor3f(1,       H/500-2.0, 0) { red }
  else glColor3f(1, 1, 1);                                 { white }
end;

var
  { Helper array for calculating all elevation points *once* before passing
    them to OpenGL. This way we avoid calculating most heights twice
    (which would be needed for simple implementation rendering with quad strips),
    also we'll have all the data to calculate normal vectors. }
  Points: TDynVector3SingleArray;

procedure DrawElevation(Elevation: TElevation);
var
  CountSteps1: Cardinal;

  procedure Vertex(const I, J: Cardinal);
  var
    P, PX, PY: PVector3Single;
    HForColor: Single;
  begin
    P  := Points.Pointers[ I      * CountSteps1 + J];
    PX := Points.Pointers[(I + 1) * CountSteps1 + J];
    PY := Points.Pointers[ I      * CountSteps1 + J + 1];
    { TODO: this is actually normal vector of 1 of the four faces around this
      vertex. Optimally, we should calculate normals on all faces,
      and for vertex normal take average. }
    glNormalv((PX^ - P^) >< (PY^ - P^));

    HForColor := P^[2];
    { scale height down by Amplitude, to keep nice colors regardless of Amplitude }
    if Elevation is TElevationNoise then
      HForColor /= TElevationNoise(Elevation).Amplitude;
    { some hacks to hit interesting colors }
    HForColor := HForColor  * 2000 - 1000;
    ColorFromHeight(HForColor);

    glVertexv(P^);
  end;

var
  CountSteps: Cardinal;
  I, J: Cardinal;
  P: PVector3Single;
begin
  CountSteps := 1 shl Subdivision;
  CountSteps1 := CountSteps + 1;

  { We will render CountSteps^2 points, but we want to calculate
    (CountSteps + 1)^2 points : to be able to calculate normal vectors. }
  Points.Length := Max(Points.Length, Sqr(CountSteps1));
  P := PVector3Single(Points.ItemsArray);

  for I := 0 to CountSteps do
    for J := 0 to CountSteps do
    begin
      { calculate P^, which is Points.Items[I * CountSteps1 + J] }

      { set XY to cover (-1, -1) ... (1, 1) rectangle with our elevation }
      P^[0] := 2 * I / CountSteps - 1;
      P^[1] := 2 * J / CountSteps - 1;

      P^[2] := Elevation.Height(P^[0], P^[1]);

      Inc(P);
    end;

  for I := 1 to CountSteps - 1 do
  begin
    glBegin(GL_QUAD_STRIP);
      for J := 0 to CountSteps - 1 do
      begin
        Vertex(I - 1, J);
        Vertex(I    , J);
      end;
    glEnd();
  end;
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
    ColorFromHeight(HForColor);
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

initialization
  Points := TDynVector3SingleArray.Create;
finalization
  FreeAndNil(Points);
end.
