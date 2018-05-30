{
  Copyright 2009-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Common utilities for NURBS curves and surfaces. }
unit CastleNURBS
  deprecated 'soon this unit will be renamed to CastleInternalNurbs; you should use NURBS only through the X3D nodes, like TNurbsCurveNode or TNurbsPatchSurfaceNode';

{$I castleconf.inc}

interface

uses SysUtils, CastleUtils, CastleVectors, CastleBoxes;

type
  TDoubleArray = array of Double;

  { Calculate NURBS basis functions. }
  TNurbsBasisCalculator = class
  strict private
    { Remember the curve parameters passed to constructor. }
    ControlPointCount: Integer;
    Order: Cardinal;
    Knot: TDoubleList;
    { Ready allocated structures, to speedup repeated calls to GetBasis.
      Their contents are overridden by each GetBasis call. }
    Basis, Left, Right: TDoubleArray;
    { Speedup FindKnotSpan by first trying out last result.
      This optimizes a very often case, when we're repeatedly calling
      GetBasis with a very similar U value (that falls within the same interval).
      This typically happens when calculating a series of points on the NURBS
      curve or surface. }
    LastFindKnotSpanResult: Integer;

    { In which knot interval is the given point.
      The result is guaranteed to be in [Order - 1...ControlPointCount - 1] range. }
    function FindKnotSpan(const U: Double): Integer;
  public
    constructor Create(const AControlPointCount: Integer;
      const AOrder: Cardinal; const AKnot: TDoubleList);

    { Calculate the basis functions (N) values for U.
      By the way, calculate also the interval in Knot in which U lies. }
    function GetBasis(const U: Double; out KnotInterval: Integer): TDoubleArray;
  end;

  { Calculate point on a NURBS curve. }
  TNurbsCurveCalculator = class
  strict private
    { Remember the curve parameters passed to constructor. }
    Points: TVector3List;
    Order: Cardinal;
    Knot, Weight: TDoubleList;

    BasisCalculator: TNurbsBasisCalculator;
  public
    constructor Create(const APoints: TVector3List;
      const AOrder: Cardinal; const AKnot, AWeight: TDoubleList);
    destructor Destroy; override;

    { Calculate point on a NURBS curve. Optimized for repeated calls,
      to calculate points on the same curve.
      See NurbsCurvePoint for detailed documentation. }
    function GetPoint(const U:Single; const Tangent: PVector3): TVector3;
  end;

  { Calculate point on a NURBS surface. }
  TNurbsSurfaceCalculator = class
  strict private
    { Remember the curve parameters passed to constructor. }
    Points: TVector3List;
    UDimension, VDimension: Cardinal;
    UOrder, VOrder: Cardinal;
    UKnot, VKnot, Weight: TDoubleList;

    UBasisCalculator, VBasisCalculator: TNurbsBasisCalculator;
  public
    constructor Create(const APoints: TVector3List;
      const AUDimension, AVDimension: Cardinal;
      const AUOrder, AVOrder: Cardinal;
      const AUKnot, AVKnot, AWeight: TDoubleList);
    destructor Destroy; override;

    { Calculate point on a NURBS surface. Optimized for repeated calls,
      to calculate points on the same surface.
      See NurbsSurfacePoint for detailed documentation. }
    function GetPoint(const U, V: Single; const Normal: PVector3): TVector3;
  end;

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
    @item Points.Count > 0 (not exactly 0).
    @item Order >= 2 (X3D and VRML 97 spec require this too).
    @item Knot must have exactly PointsCount + Order items.
    @item U is between Knot.First and Knot.Last.
  )

  Weight will be used only if it has the same length as Points.Count.
  Otherwise, weight = 1.0 (that is, defining non-rational curve) will be used
  (this follows X3D spec).

  Tangent, if non-nil, will be set to the direction at given point of the
  curve, pointing from the smaller to larger knot values.
  It will be normalized. This can be directly useful to generate
  orientations by X3D NurbsOrientationInterpolator node. }
function NurbsCurvePoint(const Points: TVector3List;
  const U: Single;
  const Order: Cardinal;
  const Knot, Weight: TDoubleList;
  const Tangent: PVector3): TVector3;

{ Return point on NURBS surface.

  Requires:
  @unorderedList(
    @item UDimension, VDimension > 0 (not exactly 0).
    @item Points.Count must match UDimension * VDimension.
    @item Order >= 2 (X3D and VRML 97 spec require this too).
    @item Each xKnot must have exactly xDimension + Order items.
  )

  Weight will be used only if it has the same length as UDimension * VDimension.
  Otherwise, weight = 1.0 (that is, defining non-rational curve) will be used
  (this follows X3D spec).

  Normal, if non-nil, will be set to the normal at given point of the
  surface. It will be normalized. You can use this to pass these normals
  to rendering. Or to generate normals for X3D NurbsSurfaceInterpolator node. }
function NurbsSurfacePoint(const Points: TVector3List;
  const UDimension, VDimension: Cardinal;
  const U, V: Single;
  const UOrder, VOrder: Cardinal;
  const UKnot, VKnot, Weight: TDoubleList;
  const Normal: PVector3): TVector3;

type
  EInvalidPiecewiseBezierCount = class(Exception);

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
    nkEndpointUniform,

    { NURBS curve will effectively become a piecewise Bezier curve.
      The order of NURBS curve will determine the order of Bezier curve,
      for example NURBS curve with order = 4 results in a cubic Bezier curve. }
    nkPiecewiseBezier);

{ Calculate a default knot, if Knot doesn't already have required number of items.
  After this, it's guaranteed that Knot.Count is Dimension + Order
  (just as required by NurbsCurvePoint, NurbsSurfacePoint).
  @raises(EInvalidPiecewiseBezierCount When you use nkPiecewiseBezier
    with invalid control points count (Dimension) and Order.) }
procedure NurbsKnotIfNeeded(Knot: TDoubleList;
  const Dimension, Order: Cardinal; const Kind: TNurbsKnotKind);

function NurbsBoundingBox(Point: TVector3List;
  Weight: TDoubleList): TBox3D; overload;
function NurbsBoundingBox(Point: TVector3List;
  Weight: TSingleList): TBox3D; overload;

function NurbsBoundingBox(Point: TVector3List;
  Weight: TDoubleList; const Transform: TMatrix4): TBox3D; overload;
function NurbsBoundingBox(Point: TVector3List;
  Weight: TSingleList; const Transform: TMatrix4): TBox3D; overload;

implementation

uses Math;

{ TNurbsBasisCalculator ------------------------------------------------------ }

constructor TNurbsBasisCalculator.Create(const AControlPointCount: Integer;
  const AOrder: Cardinal; const AKnot: TDoubleList);
begin
  inherited Create;
  ControlPointCount := AControlPointCount;
  Order := AOrder;
  Knot := AKnot;

  SetLength(Basis, Order);
  SetLength(Left, Order);
  SetLength(Right, Order);
  LastFindKnotSpanResult := -1;
end;

function TNurbsBasisCalculator.FindKnotSpan(const U: Double): Integer;
var
  Low, High: Integer;
begin
  { Use the last cached result, if still matching.
    This eliminates the need for binary-search inside FindKnotSpan
    in majority of cases. }
  if (LastFindKnotSpanResult <> -1) and
     Between(U, Knot[LastFindKnotSpanResult], Knot[LastFindKnotSpanResult + 1]) then
    Exit(LastFindKnotSpanResult);

  if U >= Knot.Last then
    Result := ControlPointCount - 1
  else
  begin
    Low := Order - 1;
    High := ControlPointCount;
    Result := (Low + High) div 2;
    while (U < Knot[Result]) or (U > Knot[Result + 1]) do
    begin
      if U < Knot[Result] then
        High := Result
      else
        Low := Result;

      if High - Low <= 1 then
      begin
        if U <= Knot[High] then
          Result := Low
        else
          Result := High; // Does this ever occur? It seems not?
        Break;
      end else
      begin
        Assert(Result <> (Low + High) div 2, 'FindKnotSpan would enter infinite loop');
        Result := (Low + High) div 2;
      end;
    end;
  end;

  Assert(Result < ControlPointCount);

  Assert(Between(U, Knot[Result], Knot[Result + 1]));
  LastFindKnotSpanResult := Result;
end;

function TNurbsBasisCalculator.GetBasis(const U: Double;
  out KnotInterval: Integer): TDoubleArray;

{ The algorithm follows the "Inverted Triangular Scheme" described in
  http://isd.ktu.lt/it2010/material/Proceedings/1_AI_7.pdf ,
  called NurbsBasis in the pseudo-code there.
  Terminology note: Degree (denoted "p" in the above paper)
  is our Order ‑ 1. }

var
  J, R: Integer;
  Saved, Tmp: Double;
begin
  KnotInterval := FindKnotSpan(U);

  Result := Basis;

  Result[0] := 1;
  // Note that Left[0] and Right[0] are never accessed, their values don't matter
  for J := 1 to Order - 1 do
  begin
    Saved := 0;
    Left[J] := U - Knot[KnotInterval + 1 - J];
    Right[J] := Knot[KnotInterval + J] - U;

    { Note that our Knots are not normalized now (not in 0..1 range),
      we could fix it here by doing

        Left[J] := MapRange01(U - Knot[KnotInterval + 1 - J], Knot.First, Knot.Last);
        Right[J] := MapRange01(Knot[KnotInterval + J] - U, Knot.First, Knot.Last);

      but it doesn't seem necessary. }

    for R := 0 to J - 1 do
    begin
      { Assertion fails on demo-models/nurbs/nurbs_dune_tests.wrl,
        although it works OK -- the number is just very small? }
      // Assert(not IsZero(Right[R + 1] + Left[J - R]));
      Tmp := Result[R] / (Right[R + 1] + Left[J - R]);
      Result[R] := Saved + Right[R + 1] * Tmp;
      Saved := Left[J - R] * Tmp;
    end;
    Result[J] := Saved;
  end;
end;

{ TNurbsCurveCalculator ------------------------------------------------------ }

constructor TNurbsCurveCalculator.Create(
  const APoints: TVector3List;
  const AOrder: Cardinal; const AKnot, AWeight: TDoubleList);
begin
  inherited Create;
  Points      := APoints;
  Order       := AOrder;
  Knot        := AKnot;
  Weight      := AWeight;

  BasisCalculator := TNurbsBasisCalculator.Create(Points.Count, Order, Knot);
end;

destructor TNurbsCurveCalculator.Destroy;
begin
  FreeAndNil(BasisCalculator);
  inherited;
end;

function TNurbsCurveCalculator.GetPoint(const U: Single; const Tangent: PVector3): TVector3;

  { Get a single point on NURBS curve, having calculated the Basis values.
    The algorithm follows the GetPoint0 described in
    http://isd.ktu.lt/it2010/material/Proceedings/1_AI_7.pdf . }
  function GetPointCore(const Basis: TDoubleArray; const KnotInterval: Integer): TVector3;
  var
    WeightSum: Double;
    I, Index: Integer;
  begin
    WeightSum := 0;
    Result := TVector3.Zero;

    if Weight.Count <> Points.Count then
    begin
      { Optimized version in case all weights = 1.
        In this case, WeightSum would always be 1, so we can avoid calculating it completely. }
      for I := 0 to Order - 1 do
      begin
        Index := KnotInterval - (Order - 1) + I;
        Result := Result + Basis[I] * Points.List^[Index];
      end;
    end else
    begin
      for I := 0 to Order - 1 do
      begin
        Index := KnotInterval - (Order - 1) + I;
        WeightSum := WeightSum + Basis[I] * Weight.List^[Index];
        { Note that Points are already "pre-multiplied" by weights,
          see https://castle-engine.io/x3d_implementation_nurbs.php#section_homogeneous_coordinates }
        Result := Result + Basis[I] * Points.List^[Index];
      end;
      Assert(not IsZero(WeightSum));
      Result := Result / WeightSum;
    end;
  end;

  { Calculate Tangent by simply sampling an adjacent point. }
  function GetTangent(const Here: TVector3): TVector3;
  var
    TangentShift: Single;
    ShiftedHere: TVector3;
  begin
    TangentShift := (Knot.Last - Knot.First) * 0.01;
    if U < (Knot.First + Knot.Last) / 2 then
    begin
      ShiftedHere := GetPoint(U + TangentShift, nil);
      Result := ShiftedHere - Here;
    end else
    begin
      ShiftedHere := GetPoint(U - TangentShift, nil);
      Result := Here - ShiftedHere;
    end;
  end;

var
  KnotInterval: Integer;
  Basis: TDoubleArray;
begin
  Assert(Knot.Count = Points.Count + Order);

  Basis := BasisCalculator.GetBasis(U, KnotInterval);
  Result := GetPointCore(Basis, KnotInterval);

  if Tangent <> nil then
    Tangent^ := GetTangent(Result).Normalize;
end;

{ TNurbsSurfaceCalculator ---------------------------------------------------- }

constructor TNurbsSurfaceCalculator.Create(const APoints: TVector3List;
  const AUDimension, AVDimension: Cardinal;
  const AUOrder, AVOrder: Cardinal;
  const AUKnot, AVKnot, AWeight: TDoubleList);
begin
  inherited Create;
  Points     := APoints;
  UDimension := AUDimension;
  VDimension := AVDimension;
  UOrder     := AUOrder;
  VOrder     := AVOrder;
  UKnot      := AUKnot;
  VKnot      := AVKnot;
  Weight     := AWeight;

  UBasisCalculator := TNurbsBasisCalculator.Create(UDimension, UOrder, UKnot);
  VBasisCalculator := TNurbsBasisCalculator.Create(VDimension, VOrder, VKnot);
end;

destructor TNurbsSurfaceCalculator.Destroy;
begin
  FreeAndNil(UBasisCalculator);
  FreeAndNil(VBasisCalculator);
  inherited;
end;

function TNurbsSurfaceCalculator.GetPoint(const U, V: Single; const Normal: PVector3): TVector3;

  { Get a single point on NURBS surface.
    The algorithm is a slightly extended version of the GetPointCore
    inside TNurbsCurveCalculator.GetPoint.
    For a difference in equations between curve and surface,
    see https://en.wikipedia.org/wiki/Non-uniform_rational_B-spline . }
  function GetPointCore(const UBasis, VBasis: TDoubleArray;
    const UKnotInterval, VKnotInterval: Integer): TVector3;
  var
    WeightSum: Double;
    I, J, UIndex, VIndex: Integer;
  begin
    WeightSum := 0;
    Result := TVector3.Zero;

    if Weight.Count <> UDimension * VDimension then
    begin
      { Optimized version in case all weights = 1.
        In this case, WeightSum would always be 1, so we can avoid calculating it completely. }
      for I := 0 to UOrder - 1 do
        for J := 0 to VOrder - 1 do
        begin
          UIndex := UKnotInterval - (UOrder - 1) + I;
          VIndex := VKnotInterval - (VOrder - 1) + J;
          Result := Result + UBasis[I] * VBasis[J] * Points.List^[UIndex + VIndex * UDimension];
        end;
    end else
    begin
      for I := 0 to UOrder - 1 do
        for J := 0 to VOrder - 1 do
        begin
          UIndex := UKnotInterval - (UOrder - 1) + I;
          VIndex := VKnotInterval - (VOrder - 1) + J;
          WeightSum := WeightSum + UBasis[I] * VBasis[J] * Weight.List^[UIndex + VIndex * UDimension];
          Result := Result + UBasis[I] * VBasis[J] * Points.List^[UIndex + VIndex * UDimension];
        end;
      Assert(not IsZero(WeightSum));
      Result := Result / WeightSum;
    end;
  end;

  { Calculate UTangent by simply sampling an adjacent point. }
  function GetUTangent(const Here: TVector3): TVector3;
  var
    UTangentShift: Single;
    UShiftedHere: TVector3;
  begin
    UTangentShift := (UKnot.Last - UKnot.First) * 0.01;
    if U < (UKnot.First + UKnot.Last) / 2 then
    begin
      UShiftedHere := GetPoint(U + UTangentShift, V, nil);
      Result := UShiftedHere - Here;
    end else
    begin
      UShiftedHere := GetPoint(U - UTangentShift, V, nil);
      Result := Here - UShiftedHere;
    end;
  end;

  function GetVTangent(const Here: TVector3): TVector3;
  var
    VTangentShift: Single;
    VShiftedHere: TVector3;
  begin
    VTangentShift := (VKnot.Last - VKnot.First) * 0.01;
    if V < (VKnot.First + VKnot.Last) / 2 then
    begin
      VShiftedHere := GetPoint(U, V + VTangentShift, nil);
      Result := VShiftedHere - Here;
    end else
    begin
      VShiftedHere := GetPoint(U, V - VTangentShift, nil);
      Result := Here - VShiftedHere;
    end;
  end;

var
  UKnotInterval, VKnotInterval: Integer;
  UBasis, VBasis: TDoubleArray;
begin
  Assert(UKnot.Count = UDimension + UOrder);
  Assert(VKnot.Count = VDimension + VOrder);
  Assert(Points.Count = UDimension * VDimension);

  UBasis := UBasisCalculator.GetBasis(U, UKnotInterval);
  VBasis := VBasisCalculator.GetBasis(V, VKnotInterval);
  Result := GetPointCore(UBasis, VBasis, UKnotInterval, VKnotInterval);

  if Normal <> nil then
    Normal^ := TVector3.CrossProduct(GetUTangent(Result), GetVTangent(Result)).Normalize;
end;

{ globals -------------------------------------------------------------------- }

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

function NurbsCurvePoint(const Points: TVector3List;
  const U: Single;
  const Order: Cardinal;
  const Knot, Weight: TDoubleList;
  const Tangent: PVector3): TVector3;
var
  Calculator: TNurbsCurveCalculator;
begin
  Calculator := TNurbsCurveCalculator.Create(Points, Order, Knot, Weight);
  try
    Result := Calculator.GetPoint(U, Tangent);
  finally FreeAndNil(Calculator) end;
end;

function NurbsSurfacePoint(const Points: TVector3List;
  const UDimension, VDimension: Cardinal;
  const U, V: Single;
  const UOrder, VOrder: Cardinal;
  const UKnot, VKnot, Weight: TDoubleList;
  const Normal: PVector3): TVector3;
var
  Calculator: TNurbsSurfaceCalculator;
begin
  Calculator := TNurbsSurfaceCalculator.Create(Points,
    UDimension, VDimension, UOrder, VOrder, UKnot, VKnot, Weight);
  try
    Result := Calculator.GetPoint(U, V, Normal);
  finally FreeAndNil(Calculator) end;
end;

procedure NurbsKnotIfNeeded(Knot: TDoubleList;
  const Dimension, Order: Cardinal; const Kind: TNurbsKnotKind);
var
  I, Segments: Integer;
begin
  if Cardinal(Knot.Count) <> Dimension + Order then
  begin
    Knot.Count := Dimension + Order;

    case Kind of
      nkPeriodicUniform:
        begin
          for I := 0 to Knot.Count - 1 do
            Knot.List^[I] := I;
        end;
      nkEndpointUniform:
        begin
          for I := 0 to Order - 1 do
          begin
            Knot.List^[I] := 0;
            Knot.List^[Cardinal(I) + Dimension] := Dimension - Order + 1;
          end;
          for I := Order to Dimension - 1 do
            Knot.List^[I] := I - Order + 1;
          for I := 0 to Dimension + Order - 1 do
            Knot.List^[I] := Knot.List^[I] / (Dimension - Order + 1);
        end;
      nkPiecewiseBezier:
        begin
          { For useful notes on knots, see
            http://www-evasion.imag.fr/~Francois.Faure/doc/inventorMentor/sgi_html/ch08.html
            http://saccade.com/writing/graphics/KnotVectors.pdf

            For Order = 4 (cubic Bezier curve) and 3 segments you want to get
            14 knot values:
              0 0 0 0
                1 1 1
                2 2 2
              3 3 3 3

            Control points count (Dimension) must be "Segments * (Order - 1)  + 1".
            In the example above, we have Dimension = 10,
            and it matches knot count that has Dimension + Order = 14.
          }

          Segments := (Dimension - 1) div (Order - 1);
          if (Dimension - 1) mod (Order - 1) <> 0 then
            raise EInvalidPiecewiseBezierCount.CreateFmt('Invalid NURBS curve control points count (%d) for a piecewise Bezier curve with order %d',
              [Dimension, Order]);

          for I := 0 to Order - 1 do
          begin
            Knot.List^[I] := 0;
            Knot.List^[Cardinal(I) + Dimension] := Segments;
          end;
          for I := Order to Dimension - 1 do
            Knot.List^[I] := (I - Order) div (Order - 1) + 1;
        end;
      else raise EInternalError.Create('NurbsKnotIfNeeded 594');
    end;

    // Debug:
    // Writeln('Recalculated NURBS knot:');
    // for I := 0 to Knot.Count - 1 do
    //   Writeln(I:4, ' = ', Knot[I]:1:2);
  end;
end;

function NurbsBoundingBox(Point: TVector3List;
  Weight: TDoubleList): TBox3D;
var
  V: PVector3;
  W: Single;
  I: Integer;
begin
  if Weight.Count = Point.Count then
  begin
    if Point.Count = 0 then
      Result := TBox3D.Empty else
    begin
      W := Weight.List^[0];
      if W = 0 then W := 1;

      Result.Data[0] := Point.List^[0] / W;
      Result.Data[1] := Result.Data[0];

      for I := 1 to Point.Count - 1 do
      begin
        V := Point.Ptr(I);
        W := Weight.List^[I];
        if W = 0 then W := 1;

        MinVar(Result.Data[0].Data[0], V^.Data[0] / W);
        MinVar(Result.Data[0].Data[1], V^.Data[1] / W);
        MinVar(Result.Data[0].Data[2], V^.Data[2] / W);

        MaxVar(Result.Data[1].Data[0], V^.Data[0] / W);
        MaxVar(Result.Data[1].Data[1], V^.Data[1] / W);
        MaxVar(Result.Data[1].Data[2], V^.Data[2] / W);
      end;
    end;
  end else
  { Otherwise, all the weights are assumed 1.0 }
    Result := CalculateBoundingBox(Point);
end;

function NurbsBoundingBox(Point: TVector3List;
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

function NurbsBoundingBox(Point: TVector3List;
  Weight: TDoubleList; const Transform: TMatrix4): TBox3D;
var
  V: TVector3;
  W: Single;
  I: Integer;
begin
  if Weight.Count = Point.Count then
  begin
    if Point.Count = 0 then
      Result := TBox3D.Empty else
    begin
      W := Weight.List^[0];
      if W = 0 then W := 1;

      Result.Data[0] := Transform.MultPoint(Point.List^[0] / W);
      Result.Data[1] := Result.Data[0];

      for I := 1 to Point.Count - 1 do
      begin
        W := Weight.List^[I];
        if W = 0 then W := 1;

        V := Transform.MultPoint(Point.List^[I] / W);

        MinVar(Result.Data[0].Data[0], V[0]);
        MinVar(Result.Data[0].Data[1], V[1]);
        MinVar(Result.Data[0].Data[2], V[2]);

        MaxVar(Result.Data[1].Data[0], V[0]);
        MaxVar(Result.Data[1].Data[1], V[1]);
        MaxVar(Result.Data[1].Data[2], V[2]);
      end;
    end;
  end else
  { Otherwise, all the weights are assumed 1.0 }
    Result := CalculateBoundingBox(Point, Transform);
end;

function NurbsBoundingBox(Point: TVector3List;
  Weight: TSingleList; const Transform: TMatrix4): TBox3D;
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
