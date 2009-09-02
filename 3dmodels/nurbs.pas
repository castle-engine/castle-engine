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

uses SysUtils, KambiUtils, VectorMath, Matrix;

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
    @item U in [0; 1] range.
    @item PointsCount > 0 (not exactly 0).
    @item Order >= 2 (X3D and VRML 97 spec require this too).
    @item Knot must have exactly PointsCount + Order items.
  )

  Weight will be used only if it has the same length as PointsCount.
  Otherwise, weight = 1.0 (that is, defining non-rational curve) will be used
  (this follows X3D spec).

  @groupBegin }
function NurbsCurveValue(const Points: PVector3Single;
  const PointsCount: Cardinal; const U: Single;
  const Order: Cardinal;
  Knot, Weight: TDynDoubleArray): TVector3_Single;
function NurbsCurveValue(const Points: TDynVector3SingleArray;
  const U: Single;
  const Order: Cardinal;
  Knot, Weight: TDynDoubleArray): TVector3_Single;
{ @groupEnd }

{ Return point on NURBS surface.

  Requires:
  @unorderedList(
    @item U, V is in [0; 1] range.
    @item UDimension, VDimension > 0 (not exactly 0).
    @item Points.Count must match UDimension * VDimension.
    @item Order >= 2 (X3D and VRML 97 spec require this too).
    @item Each xKnot must have exactly xDimension + Order items.
  )

  Weight will be used only if it has the same length as PointsCount.
  Otherwise, weight = 1.0 (that is, defining non-rational curve) will be used
  (this follows X3D spec). }
function NurbsSurfaceValue(const Points: TDynVector3SingleArray;
  const UDimension, VDimension: Cardinal;
  const U, V: Single;
  const UOrder, VOrder: Cardinal;
  UKnot, VKnot, Weight: TDynDoubleArray;
  out Normal: TVector3_Single): TVector3_Single;

{ Calculate uniform knot, if Knot doesn't already have required number of items.
  After this, it's guaranteed that Knot.Count is Dimension + Order
  (just as required by NurbsCurveValue, NurbsSurfaceValue). }
procedure NurbsUniformKnotIfNeeded(Knot: TDynDoubleArray;
  const Dimension, Order: Cardinal);

implementation

{ findSpan and basisFuns is rewritten from white dune's C source code.
  Also NurbsCurveValue is based on NodeNurbsCurve::curvePoint.
  Also NurbsSurfaceValue is based on NodeNurbsSurface::surfacePoint.
  Also NurbsUniformKnotIfNeeded is based on NodeNurbsSurface::linearUknot.

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
  const u: Single; Knot: TDynDoubleArray): LongInt;
var
  low, mid, high, oldLow, oldMid, oldHigh, n: LongInt;
begin
  n := dimension + order - 1;

  if u >= Knot[n] then
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
  while (u < Knot[mid]) or (u >= Knot[mid+1]) do
  begin
    if u < Knot[mid] then
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
  Knot, basis, deriv: TDynDoubleArray);
var
  left, right: TDynDoubleArray;
  j, r: LongInt;
  saved, dsaved, temp: Single;
begin
  left := TDynDoubleArray.Create(order);
  right := TDynDoubleArray.Create(order);

  basis[0] := 1.0;
  for j := 1 to  order - 1 do
  begin
    left[j] := u - Knot[span+1-j];
    right[j] := Knot[span+j]-u;
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
  const PointsCount: Cardinal; const U: Single;
  const Order: Cardinal;
  Knot, Weight: TDynDoubleArray): TVector3_Single;
var
  i: Integer;
  w: Single;
  span: LongInt;
  basis, deriv: TDynDoubleArray;
  UseWeight: boolean;
begin
  UseWeight := Cardinal(Weight.Count) = PointsCount;

  basis := TDynDoubleArray.Create(order);
  deriv := TDynDoubleArray.Create(order);

  span := findSpan(PointsCount, order, u, Knot);

  basisFuns(span, u, order, Knot, basis, deriv);

  Result.Init_Zero;
  w := 0.0;
  for i :=0 to order-1 do
  begin
    Result += Points[span-order+1+i] * basis[i];
    if UseWeight then
      w += weight[span-order+1+i] * basis[i] else
      w += basis[i];
  end;

  Result /= w;

  FreeAndNil(basis);
  FreeAndNil(deriv);
end;

function NurbsCurveValue(const Points: TDynVector3SingleArray;
  const U: Single;
  const Order: Cardinal;
  Knot, Weight: TDynDoubleArray): TVector3_Single;
begin
  Result := NurbsCurveValue(Points.Items, Points.Count, U, Order, Knot, Weight);
end;

function NurbsSurfaceValue(const Points: TDynVector3SingleArray;
  const UDimension, VDimension: Cardinal;
  const U, V: Single;
  const UOrder, VOrder: Cardinal;
  UKnot, VKnot, Weight: TDynDoubleArray;
  out Normal: TVector3_Single): TVector3_Single;
var
  uBasis, vBasis, uDeriv, vDeriv: TDynDoubleArray;
  uSpan, vSpan: LongInt;
  I, J: LongInt;
  uBase, vBase, index: Cardinal;
  du, dv, un, vn: TVector3_Single;
  w, duw, dvw: Single;
  gain, dugain, dvgain: Single;
  P: TVector3_Single;
  UseWeight: boolean;
begin
  UseWeight := Weight.Count = Points.Count;

  uBasis := TDynDoubleArray.Create(UOrder);
  vBasis := TDynDoubleArray.Create(VOrder);
  uDeriv := TDynDoubleArray.Create(UOrder);
  vDeriv := TDynDoubleArray.Create(VOrder);

  uSpan := findSpan(uDimension, uOrder, u, uKnot);
  vSpan := findSpan(vDimension, vOrder, v, vKnot);

  basisFuns(uSpan, u, uOrder, uKnot, uBasis, uDeriv);
  basisFuns(vSpan, v, vOrder, vKnot, vBasis, vDeriv);

  uBase := uSpan-uOrder+1;
  vBase := vSpan-vOrder+1;

  index := vBase*uDimension + uBase;
  Result.Init_Zero;
  du.Init_Zero;
  dv.Init_Zero;

  w := 0.0;
  duw := 0.0;
  dvw := 0.0;

  for j := 0 to vOrder -1 do
  begin
    for i := 0 to uOrder - 1 do
    begin
      gain := uBasis[i] * vBasis[j];
      dugain := uDeriv[i] * vBasis[j];
      dvgain := uBasis[i] * vDeriv[j];

      P := Points.Items[index];

      Result += P * gain;

      du += P * dugain;
      dv += P * dvgain;
      if UseWeight then
      begin
        w += weight[index] * gain;
        duw += weight[index] * dugain;
        dvw += weight[index] * dvgain;
      end else
      begin
        w += gain;
        duw += dugain;
        dvw += dvgain;
      end;
      Inc(index);
    end;
    index += uDimension - uOrder;
  end;

  Result /= w;
  un := (du - Result * duw) / w;
  vn := (dv - Result * dvw) / w;
  normal := un >< vn;
  Vector_Normalize(normal);

  FreeAndNil(uBasis);
  FreeAndNil(vBasis);
  FreeAndNil(uDeriv);
  FreeAndNil(vDeriv);
end;

procedure NurbsUniformKnotIfNeeded(Knot: TDynDoubleArray;
  const Dimension, Order: Cardinal);
var
  I: Integer;
begin
  if Cardinal(Knot.Count) <> Dimension + Order then
  begin
    Knot.Count := Dimension + Order;

    for I := 0 to Order - 1 do
    begin
      Knot.Items[I] := 0;
      Knot.Items[Cardinal(I) + Dimension] := Dimension - Order + 1;
    end;
    for I := 0 to Dimension - Order - 1 do
      Knot.Items[Cardinal(I) + Order] := I + 1;
    for I := 0 to Order + Dimension - 1 do
      Knot.Items[I] /= Dimension - Order + 1;
  end;
end;

end.
