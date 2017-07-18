{
  Copyright 2016-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleCurves unit. }
unit TestCastleCurves;

interface

uses fpcunit, testutils, testregistry, CastleBaseTestCase;

type
  TTestCastleCurves = class(TCastleBaseTestCase)
  published
    procedure TestSlowFastPiecewiseBezier;
  end;

implementation

uses SysUtils, Classes, Math,
  CastleVectors, CastleUtils, CastleCurves, CastleInterpolatedCurves;

{ TSlowestPiecewiseCubicBezier ---------------------------------------------- }

type
  { Slowest version of PiecewiseCubicBezier.
    Does not use CubicBezier3D in THalfFastPiecewiseCubicBezier.Point.
    Keeps a list of BezierCurves calculated in UpdateControlPoints.
    Useful to test correctness of CubicBezier3D function vs TRationalBezierCurve. }
  TSlowestPiecewiseCubicBezier = class(TInterpolatedCurve)
  private
    BezierCurves: TRationalBezierCurveList;
    ConvexHullPoints: TVector3List;
  protected
    function CreateConvexHullPoints: TVector3List; override;
    procedure DestroyConvexHullPoints(Points: TVector3List); override;
  public
    function Point(const t: Float): TVector3; override;

    { Convert to a list of @link(TRationalBezierCurve) instances.

      From each line segment ControlPoint[i] ... ControlPoint[i+1]
      you get one TRationalBezierCurve with 4 control points,
      where ControlPoint[0] and ControlPoint[3] are taken from
      ours ControlPoint[i] ... ControlPoint[i+1] and the middle
      ControlPoint[1], ControlPoint[2] are calculated so that all those
      bezier curves join smoothly.

      All Weights are set to 1.0 (so actually these are all normal
      Bezier curves; but I'm treating normal Bezier curves as Rational
      Bezier curves everywhere here) }
    function ToRationalBezierCurves(ResultOwnsCurves: boolean): TRationalBezierCurveList;

    procedure UpdateControlPoints; override;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TSlowestPiecewiseCubicBezier --------------------------------------------------- }

function TSlowestPiecewiseCubicBezier.CreateConvexHullPoints: TVector3List;
begin
  Result := ConvexHullPoints;
end;

procedure TSlowestPiecewiseCubicBezier.DestroyConvexHullPoints(Points: TVector3List);
begin
end;

function TSlowestPiecewiseCubicBezier.Point(const t: Float): TVector3;
var
  i: Integer;
begin
  if ControlPoints.Count = 1 then
    Exit(ControlPoints.L[0]);

  for i := 0 to BezierCurves.Count-1 do
    if t <= BezierCurves[i].TEnd then Break;

  // writeln('TSlowestPiecewiseCubicBezier got ', i, ' ',
  //   MapRange(t, BezierCurves[i].TBegin, BezierCurves[i].TEnd, 0, 1):1:2);

  Result := BezierCurves[i].Point(t);
end;

function TSlowestPiecewiseCubicBezier.ToRationalBezierCurves(ResultOwnsCurves: boolean): TRationalBezierCurveList;
var
  S: TVector3List;

  function MiddlePoint(i, Sign: Integer): TVector3;
  begin
    Result := ControlPoints.L[i];
    Result := Result +
      S.L[i] * (Sign * (ControlPointT(i) - ControlPointT(i-1)) / 3);
  end;

var
  C: TVector3List;
  i: Integer;
  {$warnings off} { Consciously using deprecated stuff. }
  NewCurve: TRationalBezierCurve;
  {$warnings on}
begin
  Result := TRationalBezierCurveList.Create(ResultOwnsCurves);
  try
    if ControlPoints.Count <= 1 then Exit;

    if ControlPoints.Count = 2 then
    begin
      { Normal calcualtions (based on SLE mmgk notes) cannot be done when
        ControlPoints.Count = 2:
        C.Count would be 1, S.Count would be 2,
        S[0] would be calculated based on C[0] and S[1],
        S[1] would be calculated based on C[0] and S[0].
        So I can't calculate S[0] and S[1] using given equations when
        ControlPoints.Count = 2. So I must implement a special case for
        ControlPoints.Count = 2. }
      {$warnings off} { Consciously using deprecated stuff. }
      NewCurve := TRationalBezierCurve.Create(nil);
      {$warnings on}
      NewCurve.TBegin := ControlPointT(0);
      NewCurve.TEnd := ControlPointT(1);
      NewCurve.ControlPoints.Add(ControlPoints.L[0]);
      NewCurve.ControlPoints.Add(Lerp(1/3, ControlPoints.L[0], ControlPoints.L[1]));
      NewCurve.ControlPoints.Add(Lerp(2/3, ControlPoints.L[0], ControlPoints.L[1]));
      NewCurve.ControlPoints.Add(ControlPoints.L[1]);
      NewCurve.Weights.AddRange([1.0, 1.0, 1.0, 1.0]);
      NewCurve.UpdateControlPoints;
      Result.Add(NewCurve);

      Exit;
    end;

    { based on SLE mmgk notes, "Krzywe Beziera" page 4 }
    C := nil;
    S := nil;
    try
      C := TVector3List.Create;
      C.Count := ControlPoints.Count-1;
      { calculate C values }
      for i := 0 to C.Count-1 do
      begin
        C.L[i] := ControlPoints.L[i+1] - ControlPoints.L[i];
        C.L[i] := C.L[i] * (1/(ControlPointT(i+1) - ControlPointT(i)));
      end;

      S := TVector3List.Create;
      S.Count := ControlPoints.Count;
      { calculate S values }
      for i := 1 to S.Count-2 do
        S.L[i] := Lerp( (ControlPointT(i+1) - ControlPointT(i))/
                            (ControlPointT(i+1) - ControlPointT(i-1)),
                            C.L[i-1], C.L[i]);
      S.L[0        ] := C.L[0        ] * 2 - S.L[1        ];
      S.L[S.Count-1] := C.L[S.Count-2] * 2 - S.L[S.Count-2];

      for i := 1 to ControlPoints.Count-1 do
      begin
        {$warnings off} { Consciously using deprecated stuff. }
        NewCurve := TRationalBezierCurve.Create(nil);
        {$warnings on}
        NewCurve.TBegin := ControlPointT(i-1);
        NewCurve.TEnd := ControlPointT(i);
        NewCurve.ControlPoints.Add(ControlPoints.L[i-1]);
        NewCurve.ControlPoints.Add(MiddlePoint(i-1, +1));
        NewCurve.ControlPoints.Add(MiddlePoint(i  , -1));
        NewCurve.ControlPoints.Add(ControlPoints.L[i]);
        NewCurve.Weights.AddRange([1.0, 1.0, 1.0, 1.0]);
        NewCurve.UpdateControlPoints;
        Result.Add(NewCurve);
      end;
    finally
      C.Free;
      S.Free;
    end;
  except Result.Free; raise end;
end;

procedure TSlowestPiecewiseCubicBezier.UpdateControlPoints;
var
  i: Integer;
begin
  inherited;
  FreeAndNil(BezierCurves);

  BezierCurves := ToRationalBezierCurves(true);

  ConvexHullPoints.Clear;
  ConvexHullPoints.AddRange(ControlPoints);
  for i := 0 to BezierCurves.Count-1 do
  begin
    ConvexHullPoints.Add(BezierCurves[i].ControlPoints.L[1]);
    ConvexHullPoints.Add(BezierCurves[i].ControlPoints.L[2]);
  end;
end;

constructor TSlowestPiecewiseCubicBezier.Create(AOwner: TComponent);
begin
  inherited;
  ConvexHullPoints := TVector3List.Create;
end;

destructor TSlowestPiecewiseCubicBezier.Destroy;
begin
  FreeAndNil(BezierCurves);
  FreeAndNil(ConvexHullPoints);
  inherited;
end;

{ TOnlinePiecewiseCubicBezier interface -------------------------------------- }

type
  { Like TPiecewiseCubicBezier, but does not need a call to UpdateControlPoints.
    And doesn't implement convex hull calculation.
    Use if you may very often change control points. }
  TOnlinePiecewiseCubicBezier = class(TInterpolatedCurve)
  public
    function Point(const t: Float): TVector3; override;
  end;

{ TOnlinePiecewiseCubicBezier implementation --------------------------------- }

function TOnlinePiecewiseCubicBezier.Point(const T: Float): TVector3;

  { This asssumes that I = [0..ControlPoints.Count - 2]. }
  function C(const I: Integer): TVector3;
  begin
    Result := ControlPoints.L[I + 1] - ControlPoints.L[I];
    //Result := Result * (1 / (ControlPointT(I + 1) - ControlPointT(I)));
  end;

  { This asssumes that I = [0..ControlPoints.Count - 1],
    not outside of this range, and that ControlPoints.Count > 2. }
  function S(const I: Integer): TVector3;
  begin
    if I = 0 then
      Exit(C(0) * 2 - S(1)) else
    if I = ControlPoints.Count - 1 then
      Exit(C(ControlPoints.Count - 2) * 2 - S(ControlPoints.Count - 2)) else
      Exit((C(I - 1) + C(I)) / 2);
      // Exit(Lerp( (ControlPointT(I + 1) - ControlPointT(I))/
      //            (ControlPointT(I + 1) - ControlPointT(I-1)),
      //            C(I - 1), C(I)));
  end;

var
  T01: Single;
  TInsidePiece: Double;
  IndexBefore: Int64;
  IndexAfter, IndexBeforeChange: Integer;
  PieceControlPoints: TCubicBezier3DPoints;
begin
  Assert(ControlPoints.Count >= 1);
  if ControlPoints.Count = 1 then
    Exit(ControlPoints.Items[0]);

  T01 := MapRange(T, TBegin, TEnd, 0, 1);
  if ControlPoints.Count = 2 then
    Exit(Lerp(T01, ControlPoints.Items[0], ControlPoints.Items[1]));

  FloatDivMod(T01, 1 / (ControlPoints.Count - 1), IndexBefore, TInsidePiece);
  TInsidePiece *= ControlPoints.Count - 1; // make TInsidePiece in 0..1 range

  { fix IndexBefore (together with TInsidePiece, synchronized)
    to be within [0, ControlPoints.Count - 2] range.
    Necessary so that both IndexBefore and IndexAfter are later in valid
    control points [0, ControlPoints.Count - 1] range. }
  IndexBeforeChange := 0;
  if IndexBefore > ControlPoints.Count - 2 then
    IndexBeforeChange := -(IndexBefore - (ControlPoints.Count - 2)) else
  if IndexBefore < 0 then
    IndexBeforeChange := -IndexBefore;
  if IndexBeforeChange <> 0 then
  begin
    IndexBefore += IndexBeforeChange;
    TInsidePiece -= IndexBeforeChange;
  end;
  Assert(IndexBefore >= 0);
  Assert(IndexBefore <= ControlPoints.Count - 2);

  { calculate IndexAfter }
  IndexAfter := IndexBefore + 1;

  PieceControlPoints[0] := ControlPoints[IndexBefore];
  PieceControlPoints[3] := ControlPoints[IndexAfter];

  PieceControlPoints[1] := PieceControlPoints[0] + S(IndexBefore) / 3;
  PieceControlPoints[2] := PieceControlPoints[3] - S(IndexAfter) / 3;

  Result := CubicBezier3D(TInsidePiece, PieceControlPoints);
end;

{ TTestCastleCurves ---------------------------------------------------------- }

procedure TTestCastleCurves.TestSlowFastPiecewiseBezier;

  procedure InitCurve1(const C: TControlPointsCurve);
  begin
    C.ControlPoints.Clear;
    C.ControlPoints.Add(Vector3(0, 0, 0));
    C.ControlPoints.Add(Vector3(1, 1, 0));
    C.ControlPoints.Add(Vector3(2, 0, 0));
    C.UpdateControlPoints;
  end;

  procedure InitCurve2(const C: TControlPointsCurve);
  begin
    C.ControlPoints.Clear;
    C.ControlPoints.Add(Vector3(0, -1, 0));
    C.ControlPoints.Add(Vector3(1, 1, 0));
    C.ControlPoints.Add(Vector3(2, 0, 3));
    C.ControlPoints.Add(Vector3(5, 10, 6));
    C.ControlPoints.Add(Vector3(6, 0, 4));
    C.UpdateControlPoints;
  end;

  procedure InitCurve3(const C: TControlPointsCurve);
  begin
    C.ControlPoints.Clear;
    { for 2 items, Point implementation can do just Lerp in optimized
      implementations, test it }
    C.ControlPoints.Add(Vector3(5, 6, 7));
    C.ControlPoints.Add(Vector3(55, 66, 77));
    C.UpdateControlPoints;
  end;

const
  TestCount = 10;
var
  C1: TOnlinePiecewiseCubicBezier;
  C2: TPiecewiseCubicBezier;
  C3: TSlowestPiecewiseCubicBezier;
  I: Integer;
begin
  C1 := TOnlinePiecewiseCubicBezier.Create(nil);
  C2 := TPiecewiseCubicBezier.Create(nil);
  C3 := TSlowestPiecewiseCubicBezier.Create(nil);
  try
    InitCurve1(C1);
    InitCurve1(C2);
    InitCurve1(C3);
    for I := 0 to TestCount do
    begin
      // Writeln(i);
      AssertVectorsEqual(C1.Point(I / TestCount), C2.Point(I / TestCount), 0.01);
      AssertVectorsEqual(C2.Point(I / TestCount), C3.Point(I / TestCount), 0.01);
    end;

    InitCurve2(C1);
    InitCurve2(C2);
    InitCurve2(C3);
    for I := 0 to TestCount do
    begin
      // Writeln(i);
      AssertVectorsEqual(C1.Point(I / TestCount), C2.Point(I / TestCount), 0.01);
      AssertVectorsEqual(C2.Point(I / TestCount), C3.Point(I / TestCount), 0.01);
    end;

    InitCurve3(C1);
    InitCurve3(C2);
    InitCurve3(C3);
    for I := 0 to TestCount do
    begin
      // Writeln(i);
      AssertVectorsEqual(C1.Point(I / TestCount), C2.Point(I / TestCount), 0.01);
      AssertVectorsEqual(C2.Point(I / TestCount), C3.Point(I / TestCount), 0.01);
    end;
  finally
    FreeAndNil(C1);
    FreeAndNil(C2);
    FreeAndNil(C3);
  end;
end;

initialization
  RegisterTest(TTestCastleCurves);
end.
