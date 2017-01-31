{
  Copyright 2009-2017 Michalis Kamburelis.
  Parts based on white dune (GPL >= 2):
  Stephen F. White, J. "MUFTI" Scheurich, others.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software.

  Although most of the "Castle Game Engine" is available on terms of LGPL
  (see COPYING.txt in this distribution for detailed info), parts of this unit
  are an exception: they use white dune strict GPL >= 2 code.
  You can redistribute and/or modify *this unit, CastleNURBS.pas, as a whole*
  only under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  If the engine is compiled with CASTLE_ENGINE_LGPL symbol
  (see ../base/castleconf.inc), an alternative "dummy" implementation of
  this unit will be used, that doesn't depend on any GPL code.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Common utilities for NURBS curves and surfaces. }
unit CastleNURBS;

{$I castleconf.inc}

interface

uses SysUtils, CastleUtils, CastleVectors, CastleBoxes;

{ Calculate the tessellation (number of NURBS points generated).
  This follows X3D spec for "an implementation subdividing
  the surface into an equal number of subdivision steps".
  Give value of tessellation field, and count of controlPoints.

  Returned value is for sure > 0 (never exactly 0). }
function ActualTessellation(const Tessellation: Integer;
  const Dimension: Cardinal): Cardinal;

{ Return point on NURBS curve.

  Requires:
  @unorderedList(
    @item PointsCount > 0 (not exactly 0).
    @item Order >= 2 (X3D and VRML 97 spec require this too).
    @item Knot must have exactly PointsCount + Order items.
  )

  Weight will be used only if it has the same length as PointsCount.
  Otherwise, weight = 1.0 (that is, defining non-rational curve) will be used
  (this follows X3D spec).

  Tangent, if non-nil, will be set to the direction at given point of the
  curve, pointing from the smaller to larger knot values.
  It will be normalized. This can be directly useful to generate
  orientations by X3D NurbsOrientationInterpolator node.

  @groupBegin }
function NurbsCurvePoint(const Points: PVector3Single;
  const PointsCount: Cardinal; const U: Single;
  const Order: Cardinal;
  Knot, Weight: TDoubleList;
  Tangent: PVector3Single): TVector3Single;
function NurbsCurvePoint(const Points: TVector3SingleList;
  const U: Single;
  const Order: Cardinal;
  Knot, Weight: TDoubleList;
  Tangent: PVector3Single): TVector3Single;
{ @groupEnd }

{ Return point on NURBS surface.

  Requires:
  @unorderedList(
    @item UDimension, VDimension > 0 (not exactly 0).
    @item Points.Count must match UDimension * VDimension.
    @item Order >= 2 (X3D and VRML 97 spec require this too).
    @item Each xKnot must have exactly xDimension + Order items.
  )

  Weight will be used only if it has the same length as PointsCount.
  Otherwise, weight = 1.0 (that is, defining non-rational curve) will be used
  (this follows X3D spec).

  Normal, if non-nil, will be set to the normal at given point of the
  surface. It will be normalized. You can use this to pass these normals
  to rendering. Or to generate normals for X3D NurbsSurfaceInterpolator node. }
function NurbsSurfacePoint(const Points: TVector3SingleList;
  const UDimension, VDimension: Cardinal;
  const U, V: Single;
  const UOrder, VOrder: Cardinal;
  UKnot, VKnot, Weight: TDoubleList;
  Normal: PVector3Single): TVector3Single;

type
  { Naming notes: what precisely is called a "uniform" knot vector seems
    to differ in literature / software.
    Blender calls nkPeriodicUniform as "Uniform",
    and nkEndpointUniform as "Endpoint".
    http://en.wiki.mcneel.com/default.aspx/McNeel/NURBSDoc.html
    calls nkEndpointUniform as "Uniform".
    "An introduction to NURBS: with historical perspective"
    (by David F. Rogers) calls nkEndpointUniform "open uniform" and
    nkPeriodicUniform "periodic uniform". }

  { Type of NURBS knot vector to generate. }
  TNurbsKnotKind = (
    { All knot values are evenly spaced, all knots are single.
      This is good for periodic curves. }
    nkPeriodicUniform,
    { Starting and ending knots have Order multiplicity, rest is evenly spaced.
      The curve hits endpoints. }
    nkEndpointUniform);

{ Calculate a default knot, if Knot doesn't already have required number of items.
  After this, it's guaranteed that Knot.Count is Dimension + Order
  (just as required by NurbsCurvePoint, NurbsSurfacePoint). }
procedure NurbsKnotIfNeeded(Knot: TDoubleList;
  const Dimension, Order: Cardinal; const Kind: TNurbsKnotKind);

function NurbsBoundingBox(Point: TVector3SingleList;
  Weight: TDoubleList): TBox3D;
function NurbsBoundingBox(Point: TVector3SingleList;
  Weight: TSingleList): TBox3D;

function NurbsBoundingBox(Point: TVector3SingleList;
  Weight: TDoubleList; const Transform: TMatrix4Single): TBox3D;
function NurbsBoundingBox(Point: TVector3SingleList;
  Weight: TSingleList; const Transform: TMatrix4Single): TBox3D;

implementation

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

function NurbsCurvePoint(const Points: TVector3SingleList;
  const U: Single;
  const Order: Cardinal;
  Knot, Weight: TDoubleList;
  Tangent: PVector3Single): TVector3Single;
begin
  Result := NurbsCurvePoint(PVector3Single(Points.List), Points.Count,
    U, Order, Knot, Weight, Tangent);
end;

{$ifdef CASTLE_ENGINE_LGPL}

{ Dummy implementations }

function NurbsCurvePoint(const Points: PVector3Single;
  const PointsCount: Cardinal; const U: Single;
  const Order: Cardinal;
  Knot, Weight: TDoubleList;
  Tangent: PVector3Single): TVector3Single;
begin
  Result := ZeroVector3Single;
end;

function NurbsSurfacePoint(const Points: TVector3SingleList;
  const UDimension, VDimension: Cardinal;
  const U, V: Single;
  const UOrder, VOrder: Cardinal;
  UKnot, VKnot, Weight: TDoubleList;
  Normal: PVector3Single): TVector3Single;
begin
  Result := ZeroVector3Single;
end;

{$else CASTLE_ENGINE_LGPL}

{ findSpan and basisFuns is rewritten from white dune's C source code
  (almost identical methods of NodeNurbsCurve and NodeNurbsSurface).
  Also NurbsCurvePoint is based on NodeNurbsCurve::curvePoint.
  Also NurbsSurfacePoint is based on NodeNurbsSurface::surfacePoint.
  Also NurbsUniformKnotIfNeeded is based on NodeNurbsSurface::linearUknot.

  White dune:
  - http://wdune.ourproject.org/
  - J. "MUFTI" Scheurich, Stephen F. White
  - GPL >= 2, so we're free to copy
  - findSpan and basisFuns were methods in NodeNurbsCurve
    (src/NodeNurbsCurve.cpp) and NodeNurbsSurface.
    *Almost* exactly identical, the only difference: NodeNurbsSurface
    had these two additional lines (safety check, included in my version):
      if ((right[r+1] + left[j-r]) == 0)
          return;
}
function findSpan(const dimension, order: LongInt;
  const u: Single; Knot: TDoubleList): LongInt;
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
  Knot, basis, deriv: TDoubleList);
var
  left, right: TDoubleList;
  j, r: LongInt;
  saved, dsaved, temp: Single;
begin
  left  := TDoubleList.Create; left .Count := order;
  right := TDoubleList.Create; right.Count := order;

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
      begin
        { Or we could use try..finally, at a (very very small) speed penalty. }
        FreeAndNil(left);
        FreeAndNil(right);
        Exit;
      end;
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

function NurbsCurvePoint(const Points: PVector3Single;
  const PointsCount: Cardinal; const U: Single;
  const Order: Cardinal;
  Knot, Weight: TDoubleList;
  Tangent: PVector3Single): TVector3Single;
var
  i: Integer;
  w, duw: Single;
  span: LongInt;
  basis, deriv: TDoubleList;
  UseWeight: boolean;
  du: TVector3Single;
  index: Cardinal;
begin
  UseWeight := Cardinal(Weight.Count) = PointsCount;

  basis := TDoubleList.Create; basis.Count := order;
  deriv := TDoubleList.Create; deriv.Count := order;

  span := findSpan(PointsCount, order, u, Knot);

  basisFuns(span, u, order, Knot, basis, deriv);

  Result := ZeroVector3Single;
  du := ZeroVector3Single;

  w := 0.0;
  duw := 0.0;

  for i := 0 to order-1 do
  begin
    index := span-order+1+i;
    Result += Points[index] * basis[i];
    du += Points[index] * deriv[i];
    if UseWeight then
    begin
      w += weight[index] * basis[i];
      duw += weight[index] * deriv[i];
    end else
    begin
      w += basis[i];
      duw += deriv[i];
    end;
  end;

  Result /= w;

  if Tangent <> nil then
  begin
    Tangent^ := (du - Result * duw) / w;
    NormalizeVar(Tangent^);
  end;

  FreeAndNil(basis);
  FreeAndNil(deriv);
end;

function NurbsSurfacePoint(const Points: TVector3SingleList;
  const UDimension, VDimension: Cardinal;
  const U, V: Single;
  const UOrder, VOrder: Cardinal;
  UKnot, VKnot, Weight: TDoubleList;
  Normal: PVector3Single): TVector3Single;
var
  uBasis, vBasis, uDeriv, vDeriv: TDoubleList;
  uSpan, vSpan: LongInt;
  I, J: LongInt;
  uBase, vBase, index: Cardinal;
  du, dv, un, vn: TVector3Single;
  w, duw, dvw: Single;
  gain, dugain, dvgain: Single;
  P: TVector3Single;
  UseWeight: boolean;
begin
  UseWeight := Weight.Count = Points.Count;

  uBasis := TDoubleList.Create; uBasis.Count := UOrder;
  vBasis := TDoubleList.Create; vBasis.Count := VOrder;
  uDeriv := TDoubleList.Create; uDeriv.Count := UOrder;
  vDeriv := TDoubleList.Create; vDeriv.Count := VOrder;

  uSpan := findSpan(uDimension, uOrder, u, uKnot);
  vSpan := findSpan(vDimension, vOrder, v, vKnot);

  basisFuns(uSpan, u, uOrder, uKnot, uBasis, uDeriv);
  basisFuns(vSpan, v, vOrder, vKnot, vBasis, vDeriv);

  uBase := uSpan-uOrder+1;
  vBase := vSpan-vOrder+1;

  index := vBase*uDimension + uBase;
  Result := ZeroVector3Single;
  du := ZeroVector3Single;
  dv := ZeroVector3Single;

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

      P := Points.L[index];

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

  if Normal <> nil then
  begin
    un := (du - Result * duw) / w;
    vn := (dv - Result * dvw) / w;
    normal^ := un >< vn;
    NormalizeVar(normal^);
  end;

  FreeAndNil(uBasis);
  FreeAndNil(vBasis);
  FreeAndNil(uDeriv);
  FreeAndNil(vDeriv);
end;

{$endif CASTLE_ENGINE_LGPL}

procedure NurbsKnotIfNeeded(Knot: TDoubleList;
  const Dimension, Order: Cardinal; const Kind: TNurbsKnotKind);
var
  I: Integer;
begin
  if Cardinal(Knot.Count) <> Dimension + Order then
  begin
    Knot.Count := Dimension + Order;

    case Kind of
      nkPeriodicUniform:
        begin
          for I := 0 to Knot.Count - 1 do
            Knot.L[I] := I;
        end;
      nkEndpointUniform:
        begin
          for I := 0 to Order - 1 do
          begin
            Knot.L[I] := 0;
            Knot.L[Cardinal(I) + Dimension] := Dimension - Order + 1;
          end;
          for I := 0 to Dimension - Order - 1 do
            Knot.L[Cardinal(I) + Order] := I + 1;
          for I := 0 to Order + Dimension - 1 do
            Knot.L[I] /= Dimension - Order + 1;
        end;
      else raise EInternalError.Create('NurbsKnotIfNeeded 594');
    end;
  end;
end;

function NurbsBoundingBox(Point: TVector3SingleList;
  Weight: TDoubleList): TBox3D;
var
  V: PVector3Single;
  W: Single;
  I: Integer;
begin
  if Weight.Count = Point.Count then
  begin
    if Point.Count = 0 then
      Result := EmptyBox3D else
    begin
      W := Weight.L[0];
      if W = 0 then W := 1;

      Result.Data[0] := Point.L[0] / W;
      Result.Data[1] := Result.Data[0];

      for I := 1 to Point.Count - 1 do
      begin
        V := Point.Ptr(I);
        W := Weight.L[I];
        if W = 0 then W := 1;

        MinVar(Result.Data[0][0], V^[0] / W);
        MinVar(Result.Data[0][1], V^[1] / W);
        MinVar(Result.Data[0][2], V^[2] / W);

        MaxVar(Result.Data[1][0], V^[0] / W);
        MaxVar(Result.Data[1][1], V^[1] / W);
        MaxVar(Result.Data[1][2], V^[2] / W);
      end;
    end;
  end else
  { Otherwise, all the weights are assumed 1.0 }
    Result := CalculateBoundingBox(Point);
end;

function NurbsBoundingBox(Point: TVector3SingleList;
  Weight: TSingleList): TBox3D;
var
  WeightDouble: TDoubleList;
begin
  { Direct implementation using single would be much faster...
    But not important, this is only for old VRML 2.0, not for X3D. }
  WeightDouble := Weight.ToDouble;
  try
    Result := NurbsBoundingBox(Point, WeightDouble);
  finally FreeAndNil(WeightDouble) end;
end;

function NurbsBoundingBox(Point: TVector3SingleList;
  Weight: TDoubleList; const Transform: TMatrix4Single): TBox3D;
var
  V: TVector3Single;
  W: Single;
  I: Integer;
begin
  if Weight.Count = Point.Count then
  begin
    if Point.Count = 0 then
      Result := EmptyBox3D else
    begin
      W := Weight.L[0];
      if W = 0 then W := 1;

      Result.Data[0] := MatrixMultPoint(Transform, Point.L[0] / W);
      Result.Data[1] := Result.Data[0];

      for I := 1 to Point.Count - 1 do
      begin
        W := Weight.L[I];
        if W = 0 then W := 1;

        V := MatrixMultPoint(Transform, Point.L[I] / W);

        MinVar(Result.Data[0][0], V[0]);
        MinVar(Result.Data[0][1], V[1]);
        MinVar(Result.Data[0][2], V[2]);

        MaxVar(Result.Data[1][0], V[0]);
        MaxVar(Result.Data[1][1], V[1]);
        MaxVar(Result.Data[1][2], V[2]);
      end;
    end;
  end else
  { Otherwise, all the weights are assumed 1.0 }
    Result := CalculateBoundingBox(Point, Transform);
end;

function NurbsBoundingBox(Point: TVector3SingleList;
  Weight: TSingleList; const Transform: TMatrix4Single): TBox3D;
var
  WeightDouble: TDoubleList;
begin
  { Direct implementation using single would be much faster...
    But not important, this is only for old VRML 2.0, not for X3D. }
  WeightDouble := Weight.ToDouble;
  try
    Result := NurbsBoundingBox(Point, WeightDouble, Transform);
  finally FreeAndNil(WeightDouble) end;
end;

end.
