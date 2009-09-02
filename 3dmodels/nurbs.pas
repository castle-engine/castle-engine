{
  Copyright 2009 Michalis Kamburelis.
  Parts based on white dune (also GPL >= 2):
  Stephen F. White, J. "MUFTI" Scheurich, others.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

{ Common utilities for NURBS curves and surfaces. }
unit NURBS;

interface

uses SysUtils, KambiUtils, VectorMath;

{ Calculate the actual tessellation, that is the number of tessellation
  points. This follows X3D spec for "an implementation subdividing
  the surface into an equal number of subdivision steps".
  Give value of tessellation field, and count of controlPoints.

  Returned value is for sure > 0 (never exactly 0). }
function ActualTessellation(const Tessellation: Integer;
  const Dimension: Cardinal): Cardinal;

{ Return point on NURBS curve.

  Requires:
  @unorderedList(
    @item T in [0; 1] range.
    @item PointsCount > 0 (not exactly 0).
  )

  @groupBegin }
function NurbsCurveValue(const Points: PVector3Single;
  const PointsCount: Cardinal; const T: Single): TVector3Single;
function NurbsCurveValue(const Points: TDynVector3SingleArray;
  const T: Single): TVector3Single;
{ @groupEnd }

{ Return point on NURBS surface.

  Requires:
  @unorderedList(
    @item U, V is in [0; 1] range.
    @item UDimension, VDimension > 0 (not exactly 0).
    @item Points.Count must match UDimension * VDimension.
  ) }
function NurbsSurfaceValue(const Points: TDynVector3SingleArray;
  const UDimension, VDimension: Cardinal;
  const U, V: Single): TVector3Single;

implementation

{ findSpan and basisFuns is rewritten from white dune's C source code.

  White dune:
  - http://vrml.cip.ica.uni-stuttgart.de/dune/
  - J. "MUFTI" Scheurich, Stephen F. White
  - GPL >= 2, so we're free to copy
  - there were methods in NodeNurbsCurve (src/NodeNurbsCurve.cpp) and
    NodeNurbsSurface. Exactly identical, except NodeNurbsSurface added:
      if ((right[r+1] + left[j-r]) == 0)
	  return;
}
function findSpan(const dimension, order: LongInt;
  const u: Single; knots: TDynSingleArray): LongInt;
var
  low, mid, high, oldLow, oldMid, oldHigh, n: LongInt;
begin
  n := dimension + order - 1;

  if u >= knots[n] then
  begin
    Result := n - order;
    Exit;
  end;

  low := order - 1;
  high := n - order + 1;

  mid := (low + high) div 2;

  oldLow := low;
  oldHigh := high;
  oldMid := mid;
  while (u < knots[mid]) or (u >= knots[mid+1]) do
  begin
    if u < knots[mid] then
      high := mid else
      low := mid;

    mid := (low+high) div 2;

    // emergency abort of loop, otherwise a endless loop can occure
    if (low = oldLow) and (high = oldHigh) and (mid = oldMid) then
      break;

    oldLow := low;
    oldHigh := high;
    oldMid := mid;
  end;
  Result := mid;
end;

procedure basisFuns(const span: LongInt; const u: Single; const order: LongInt;
  knots, basis, deriv: TDynSingleArray);
var
  left, right: TDynSingleArray;
  j, r: LongInt;
  saved, dsaved, temp: Single;
begin
  left := TDynSingleArray.Create(order);
  right := TDynSingleArray.Create(order);

  basis[0] := 1.0;
  for j := 1 to  order - 1 do
  begin
    left[j] := u - knots[span+1-j];
    right[j] := knots[span+j]-u;
    saved := 0.0;
    dsaved := 0.0;
    for r := 0 to j - 1 do
    begin
      if (right[r+1] + left[j-r]) = 0 then
        Exit;
      temp := basis[r] / (right[r+1] + left[j-r]);
      basis[r] := saved + right[r+1] * temp;
      deriv[r] := dsaved - j * temp;
      saved := left[j-r] * temp;
      dsaved := j * temp;
    end;
    basis[j] := saved;
    deriv[j] := dsaved;
  end;

  FreeAndNil(left);
  FreeAndNil(right);
end;

function ActualTessellation(const Tessellation: Integer;
  const Dimension: Cardinal): Cardinal;
begin
  if Tessellation > 0 then
    Result := Tessellation else
  if Tessellation = 0 then
    Result := 2 * Dimension else
    Result := Cardinal(-Tessellation) * Dimension;
  Inc(Result);
end;

function NurbsCurveValue(const Points: PVector3Single;
  const PointsCount: Cardinal; const T: Single): TVector3Single;
var
  DivResult: Int64;
  Remainder: Double;
begin
  Assert(PointsCount > 0);
  if PointsCount = 1 then
    Result := Points[0] else
  begin
    FloatDivMod(T, 1 / (PointsCount - 1), DivResult, Remainder);
    { floating point operations are never perfect, so prepare for various
      strange DivResult values. }
    if DivResult < 0 then
      Result := Points[0] else
    if DivResult >= PointsCount - 1 then
      Result := Points[PointsCount - 1] else
    begin
      { then DivResult between 0 .. PointsCount - 2 }
      Remainder := Clamped(Remainder / (1 / (PointsCount - 1)), 0, 1);
      Result := Lerp(Remainder,
        Points[DivResult],
        Points[DivResult + 1]);
    end;
  end;
end;

function NurbsCurveValue(const Points: TDynVector3SingleArray;
  const T: Single): TVector3Single;
begin
  Result := NurbsCurveValue(Points.Items, Points.Count, T);
end;

function NurbsSurfaceValue(const Points: TDynVector3SingleArray;
  const UDimension, VDimension: Cardinal;
  const U, V: Single): TVector3Single;
var
  VCurve: TDynVector3SingleArray;
  I: Cardinal;
begin
  VCurve := TDynVector3SingleArray.Create(VDimension);
  try
    for I := 0 to VDimension - 1 do
      VCurve.Items[I] := NurbsCurveValue(Points.Pointers[UDimension * I], UDimension, U);
    Result := NurbsCurveValue(VCurve, V);
  finally FreeAndNil(VCurve) end;
end;

end.