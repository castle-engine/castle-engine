{
  Copyright 2004-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Rational (weighted) Bezier curves (TRationalBezierCurve)
  and smooth curves (TSmoothInterpolatedCurve, each segment is a Bezier curve).

  Implemented first for bezier_curves program
  [http://castle-engine.sourceforge.net/bezier_curves.php],
  may be generally useful. }
unit BezierCurve;

interface

uses SysUtils, CastleUtils, CastleClassUtils, Classes, Math, CastleVectors, Curve,
  FGL;

type
  { Rational Bezier curve (Bezier curve with weights).
    Note: for Bezier Curve ControlPoints.Count MAY be 1.
    (For TControlPointsCurve it must be >= 2) }
  TRationalBezierCurve = class(TControlPointsCurve)
  public
    { Splits this curve using Casteljau algorithm.

      Under B1 and B2 returns two new, freshly created, bezier curves,
      such that if you concatenate them - they will create this curve.
      Proportion is something from (0; 1).
      B1 will be equal to Self for T in TBegin .. TMiddle,
      B2 will be equal to Self for T in TMiddle .. TEnd,
      where TMiddle = TBegin + Proportion * (TEnd - TBegin).

      B1.ControlPoints.Count = B2.ControlPoints.Count =
        Self.ControlPoints.Count. }
    procedure Split(const Proportion: Float; var B1, B2: TRationalBezierCurve);

    function Point(const t: Float): TVector3Single; override;
    class function NiceClassName: string; override;
  public
    { Curve weights.
      Must always be Weights.Count = ControlPoints.Count.
      After changing Weights you also have to call UpdateControlPoints.}
    Weights: TFloatList;

    procedure UpdateControlPoints; override;

    constructor Create(const ATBegin, ATEnd: Float); override;
    destructor Destroy; override;
  end;

  TRationalBezierCurveList = specialize TFPGObjectList<TRationalBezierCurve>;

  { Smooth interpolated curve, each ControlPoints[i]..ControlPoints[i+1]
    segment is converted to a rational Bezier curve (with 4 control points)
    when rendering.

    You can also explicitly convert it to a list of bezier curves using
    ToRationalBezierCurves.

    Here too ControlPoints.Count MAY be 1.
    (For TControlPointsCurve it must be >= 2) }
  TSmoothInterpolatedCurve = class(TInterpolatedCurve)
  private
    BezierCurves: TRationalBezierCurveList;
    ConvexHullPoints: TVector3SingleList;
  protected
    function CreateConvexHullPoints: TVector3SingleList; override;
    procedure DestroyConvexHullPoints(Points: TVector3SingleList); override;
  public
    function Point(const t: Float): TVector3Single; override;

    { convert this to a list of TRationalBezierCurve.

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

    class function NiceClassName: string; override;

    constructor Create(const ATBegin, ATEnd: Float); override;
    destructor Destroy; override;
  end;

implementation

uses GL, GLU, CastleGLUtils;

{ TRationalBezierCurve ----------------------------------------------- }

{$define DE_CASTELJAU_DECLARE:=
var
  W: TVector3SingleList;
  Wgh: TFloatList;
  i, k, n, j: Integer;}

{ This initializes W and Wgh (0-th step of de Casteljau algorithm).
  It uses ControlPoints, Weights. }
{$define DE_CASTELJAU_BEGIN:=
  n := ControlPoints.Count - 1;

  W := nil;
  Wgh := nil;
  try
    // using nice FPC memory manager should make this memory allocating
    // (in each call to Point) painless. So I don't care about optimizing
    // this by moving W to private class-scope.
    W := TVector3SingleList.Create;
    W.Assign(ControlPoints);
    Wgh := TFloatList.Create;
    Wgh.Assign(Weights);
}

{ This caculates in W and Wgh k-th step of de Casteljau algorithm.
  This assumes that W and Wgh already contain (k-1)-th step.
  Uses u as the target point position (in [0; 1]) }
{$define DE_CASTELJAU_STEP:=
begin
  for i := 0 to n - k do
  begin
    for j := 0 to 2 do
      W.L[i][j]:=(1-u) * Wgh[i  ] * W.L[i  ][j] +
                         u * Wgh[i+1] * W.L[i+1][j];
    Wgh.L[i]:=(1-u) * Wgh[i] + u * Wgh[i+1];
    for j := 0 to 2 do
      W.L[i][j] /= Wgh.L[i];
  end;
end;}

{ This frees W and Wgh. }
{$define DE_CASTELJAU_END:=
  finally
    Wgh.Free;
    W.Free;
  end;}

procedure TRationalBezierCurve.Split(const Proportion: Float; var B1, B2: TRationalBezierCurve);
var TMiddle, u: Float;
DE_CASTELJAU_DECLARE
begin
  TMiddle := TBegin + Proportion * (TEnd - TBegin);
  B1 := TRationalBezierCurve.Create(TBegin, TMiddle);
  B2 := TRationalBezierCurve.Create(TMiddle, TEnd);
  B1.ControlPoints.Count := ControlPoints.Count;
  B2.ControlPoints.Count := ControlPoints.Count;
  B1.Weights.Count := Weights.Count;
  B2.Weights.Count := Weights.Count;

  { now we do Casteljau algorithm, similiar to what we do in Point.
    But this time our purpose is to update B1.ControlPoints/Weights and
    B2.ControlPoints/Weights. }

  u := Proportion;

  DE_CASTELJAU_BEGIN
    B1.ControlPoints.L[0] := ControlPoints.L[0];
    B1.Weights      .L[0] := Wgh          .L[0];
    B2.ControlPoints.L[n] := ControlPoints.L[n];
    B2.Weights      .L[n] := Wgh          .L[n];

    for k := 1 to n do
    begin
      DE_CASTELJAU_STEP

      B1.ControlPoints.L[k]   := W  .L[0];
      B1.Weights      .L[k]   := Wgh.L[0];
      B2.ControlPoints.L[n-k] := W  .L[n-k];
      B2.Weights      .L[n-k] := Wgh.L[n-k];
    end;
  DE_CASTELJAU_END
end;

function TRationalBezierCurve.Point(const t: Float): TVector3Single;
var
  u: Float;
DE_CASTELJAU_DECLARE
begin
  { u := t normalized to [0; 1] }
  u := (t - TBegin) / (TEnd - TBegin);

  DE_CASTELJAU_BEGIN
    for k := 1 to n do DE_CASTELJAU_STEP
    Result := W.L[0];
  DE_CASTELJAU_END
end;

class function TRationalBezierCurve.NiceClassName: string;
begin
  Result := 'Rational Bezier curve';
end;

procedure TRationalBezierCurve.UpdateControlPoints;
begin
  inherited;
  Assert(Weights.Count = ControlPoints.Count);
end;

constructor TRationalBezierCurve.Create(const ATBegin, ATEnd: Float);
begin
  inherited;
  Weights := TFloatList.Create;
  Weights.Count := ControlPoints.Count;
end;

destructor TRationalBezierCurve.Destroy;
begin
  Weights.Free;
  inherited;
end;

{ TSmoothInterpolatedCurve ------------------------------------------------------------ }

function TSmoothInterpolatedCurve.CreateConvexHullPoints: TVector3SingleList;
begin
  Result := ConvexHullPoints;
end;

procedure TSmoothInterpolatedCurve.DestroyConvexHullPoints(Points: TVector3SingleList);
begin
end;

function TSmoothInterpolatedCurve.Point(const t: Float): TVector3Single;
var
  i: Integer;
begin
  if ControlPoints.Count = 1 then
    Exit(ControlPoints.L[0]);

  for i := 0 to BezierCurves.Count-1 do
    if t <= BezierCurves[i].TEnd then Break;

  Result := BezierCurves[i].Point(t);
end;

function TSmoothInterpolatedCurve.ToRationalBezierCurves(ResultOwnsCurves: boolean): TRationalBezierCurveList;
var
  S: TVector3SingleList;

  function MiddlePoint(i, Sign: Integer): TVector3Single;
  begin
    Result := ControlPoints.L[i];
    VectorAddTo1st(Result,
      VectorScale(S.L[i], Sign * (ControlPointT(i) - ControlPointT(i-1)) / 3));
  end;

var
  C: TVector3SingleList;
  i: Integer;
  NewCurve: TRationalBezierCurve;
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
      NewCurve := TRationalBezierCurve.Create(ControlPointT(0), ControlPointT(1));
      NewCurve.ControlPoints.Add(ControlPoints.L[0]);
      NewCurve.ControlPoints.Add(Lerp(1/3, ControlPoints.L[0], ControlPoints.L[1]));
      NewCurve.ControlPoints.Add(Lerp(2/3, ControlPoints.L[0], ControlPoints.L[1]));
      NewCurve.ControlPoints.Add(ControlPoints.L[1]);
      NewCurve.Weights.AddArray([1.0, 1.0, 1.0, 1.0]);
      NewCurve.UpdateControlPoints;
      Result.Add(NewCurve);

      Exit;
    end;

    { based on SLE mmgk notes, "Krzywe Beziera" page 4 }
    C := nil;
    S := nil;
    try
      C := TVector3SingleList.Create;
      C.Count := ControlPoints.Count-1;
      { calculate C values }
      for i := 0 to C.Count-1 do
      begin
        C.L[i] := VectorSubtract(ControlPoints.L[i+1], ControlPoints.L[i]);
        VectorScaleTo1st(C.L[i],
          1/(ControlPointT(i+1) - ControlPointT(i)));
      end;

      S := TVector3SingleList.Create;
      S.Count := ControlPoints.Count;
      { calculate S values }
      for i := 1 to S.Count-2 do
        S.L[i] := Lerp( (ControlPointT(i+1) - ControlPointT(i))/
                            (ControlPointT(i+1) - ControlPointT(i-1)),
                            C.L[i-1], C.L[i]);
      S.L[0        ] := VectorSubtract(VectorScale(C.L[0        ], 2), S.L[1        ]);
      S.L[S.Count-1] := VectorSubtract(VectorScale(C.L[S.Count-2], 2), S.L[S.Count-2]);

      for i := 1 to ControlPoints.Count-1 do
      begin
        NewCurve := TRationalBezierCurve.Create(ControlPointT(i-1), ControlPointT(i));
        NewCurve.ControlPoints.Add(ControlPoints.L[i-1]);
        NewCurve.ControlPoints.Add(MiddlePoint(i-1, +1));
        NewCurve.ControlPoints.Add(MiddlePoint(i  , -1));
        NewCurve.ControlPoints.Add(ControlPoints.L[i]);
        NewCurve.Weights.AddArray([1.0, 1.0, 1.0, 1.0]);
        NewCurve.UpdateControlPoints;
        Result.Add(NewCurve);
      end;
    finally
      C.Free;
      S.Free;
    end;
  except Result.Free; raise end;
end;

class function TSmoothInterpolatedCurve.NiceClassName: string;
begin
  Result := 'Smooth Interpolated curve';
end;

procedure TSmoothInterpolatedCurve.UpdateControlPoints;
var
  i: Integer;
begin
  inherited;
  FreeAndNil(BezierCurves);

  BezierCurves := ToRationalBezierCurves(true);

  ConvexHullPoints.Clear;
  ConvexHullPoints.AddList(ControlPoints);
  for i := 0 to BezierCurves.Count-1 do
  begin
    ConvexHullPoints.Add(BezierCurves[i].ControlPoints.L[1]);
    ConvexHullPoints.Add(BezierCurves[i].ControlPoints.L[2]);
  end;
end;

constructor TSmoothInterpolatedCurve.Create(const ATBegin, ATEnd: Float);
begin
  inherited;
  ConvexHullPoints := TVector3SingleList.Create;
end;

destructor TSmoothInterpolatedCurve.Destroy;
begin
  FreeAndNil(BezierCurves);
  FreeAndNil(ConvexHullPoints);
  inherited;
end;

end.