{
  Copyright 2004-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Rational (weighted) Bezier curves (TRationalBezierCurve)
  and smooth curves (TSmoothInterpolatedCurve, each segment is a Bezier curve).

  Implemented first for bezier_curves program
  [http://vrmlengine.sourceforge.net/bezier_curves.php],
  may be generally useful. }
unit BezierCurve;

interface

uses SysUtils, KambiUtils, KambiClassUtils, Classes, Math, VectorMath, Curve,
  FGL {$ifdef VER2_2}, FGLObjectList22 {$endif};

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
      Must always be Weights.Length = ControlPoints.Length.
      After changing Weights you also have to call UpdateControlPoints.}
    Weights: TDynFloatArray;

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
    ConvexHullPoints: TDynVector3SingleArray;
  protected
    function CreateConvexHullPoints: TDynVector3SingleArray; override;
    procedure DestroyConvexHullPoints(Points: TDynVector3SingleArray); override;
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

uses GL, GLU, KambiGLUtils;

{ TRationalBezierCurve ----------------------------------------------- }

{$define DE_CASTELJAU_DECLARE:=
var
  W: TDynVector3SingleArray;
  Wgh: TDynFloatArray;
  i, k, n, j: Integer;}

{ This initializes W and Wgh (0-th step of de Casteljau algorithm).
  It uses ControlPoints, Weights. }
{$define DE_CASTELJAU_BEGIN:=
  n := ControlPoints.High;

  W := nil;
  Wgh := nil;
  try
    // using nice FPC memory manager should make this memory allocating
    // (in each call to Point) painless. So I don't care about optimizing
    // this by moving W to private class-scope.
    W := TDynVector3SingleArray.Create(ControlPoints.Count);
    Wgh := TDynFloatArray.Create(Weights.Count);

    Move(ControlPoints.Items[0], W.Items[0],   W.Count   * SizeOf(TVector3Single));
    Move(Weights.Items[0],       Wgh.Items[0], Wgh.Count * SizeOf(Float));
}

{ This caculates in W and Wgh k-th step of de Casteljau algorithm.
  This assumes that W and Wgh already contain (k-1)-th step.
  Uses u as the target point position (in [0; 1]) }
{$define DE_CASTELJAU_STEP:=
begin
  for i := 0 to n - k do
  begin
    for j := 0 to 2 do
      W.Items[i][j]:=(1-u) * Wgh[i  ] * W.Items[i  ][j] +
                         u * Wgh[i+1] * W.Items[i+1][j];
    Wgh.Items[i]:=(1-u) * Wgh[i] + u * Wgh[i+1];
    for j := 0 to 2 do
      W.Items[i][j] /= Wgh.Items[i];
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
    B1.ControlPoints.Items[0] := ControlPoints.Items[0];
    B1.Weights      .Items[0] := Wgh          .Items[0];
    B2.ControlPoints.Items[n] := ControlPoints.Items[n];
    B2.Weights      .Items[n] := Wgh          .Items[n];

    for k := 1 to n do
    begin
      DE_CASTELJAU_STEP

      B1.ControlPoints.Items[k]   := W  .Items[0];
      B1.Weights      .Items[k]   := Wgh.Items[0];
      B2.ControlPoints.Items[n-k] := W  .Items[n-k];
      B2.Weights      .Items[n-k] := Wgh.Items[n-k];
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
    Result := W.Items[0];
  DE_CASTELJAU_END
end;

class function TRationalBezierCurve.NiceClassName: string;
begin
  Result := 'Rational Bezier curve';
end;

procedure TRationalBezierCurve.UpdateControlPoints;
begin
  inherited;
  Assert(Weights.Length = ControlPoints.Length);
end;

constructor TRationalBezierCurve.Create(const ATBegin, ATEnd: Float);
begin
  inherited;
  Weights := TDynFloatArray.Create(ControlPoints.Count);
end;

destructor TRationalBezierCurve.Destroy;
begin
  Weights.Free;
  inherited;
end;

{ TSmoothInterpolatedCurve ------------------------------------------------------------ }

function TSmoothInterpolatedCurve.CreateConvexHullPoints: TDynVector3SingleArray;
begin
  Result := ConvexHullPoints;
end;

procedure TSmoothInterpolatedCurve.DestroyConvexHullPoints(Points: TDynVector3SingleArray);
begin
end;

function TSmoothInterpolatedCurve.Point(const t: Float): TVector3Single;
var
  i: Integer;
begin
  if ControlPoints.Count = 1 then
    Exit(ControlPoints.Items[0]);

  for i := 0 to BezierCurves.Count-1 do
    if t <= BezierCurves[i].TEnd then Break;

  Result := BezierCurves[i].Point(t);
end;

function TSmoothInterpolatedCurve.ToRationalBezierCurves(ResultOwnsCurves: boolean): TRationalBezierCurveList;
var
  S: TDynVector3SingleArray;

  function MiddlePoint(i, Sign: Integer): TVector3Single;
  begin
    Result := ControlPoints.Items[i];
    VectorAddTo1st(Result,
      VectorScale(S.Items[i], Sign * (ControlPointT(i) - ControlPointT(i-1)) / 3));
  end;

var
  C: TDynVector3SingleArray;
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
      NewCurve.ControlPoints.Add(ControlPoints.Items[0]);
      NewCurve.ControlPoints.Add(Lerp(1/3, ControlPoints.Items[0], ControlPoints.Items[1]));
      NewCurve.ControlPoints.Add(Lerp(2/3, ControlPoints.Items[0], ControlPoints.Items[1]));
      NewCurve.ControlPoints.Add(ControlPoints.Items[1]);
      NewCurve.Weights.AppendArray([1.0, 1.0, 1.0, 1.0]);
      NewCurve.UpdateControlPoints;
      Result.Add(NewCurve);

      Exit;
    end;

    { based on SLE mmgk notes, "Krzywe Beziera" page 4 }
    C := nil;
    S := nil;
    try
      C := TDynVector3SingleArray.Create(ControlPoints.Count-1);
      { calculate C values }
      for i := 0 to C.Count-1 do
      begin
        C.Items[i] := VectorSubtract(ControlPoints.Items[i+1], ControlPoints.Items[i]);
        VectorScaleTo1st(C.Items[i],
          1/(ControlPointT(i+1) - ControlPointT(i)));
      end;

      S := TDynVector3SingleArray.Create(ControlPoints.Count);
      { calculate S values }
      for i := 1 to S.Count-2 do
        S.Items[i] := Lerp( (ControlPointT(i+1) - ControlPointT(i))/
                            (ControlPointT(i+1) - ControlPointT(i-1)),
                            C.Items[i-1], C.Items[i]);
      S.Items[0        ] := VectorSubtract(VectorScale(C.Items[0        ], 2), S.Items[1        ]);
      S.Items[S.Count-1] := VectorSubtract(VectorScale(C.Items[S.Count-2], 2), S.Items[S.Count-2]);

      for i := 1 to ControlPoints.Count-1 do
      begin
        NewCurve := TRationalBezierCurve.Create(ControlPointT(i-1), ControlPointT(i));
        NewCurve.ControlPoints.Add(ControlPoints.Items[i-1]);
        NewCurve.ControlPoints.Add(MiddlePoint(i-1, +1));
        NewCurve.ControlPoints.Add(MiddlePoint(i  , -1));
        NewCurve.ControlPoints.Add(ControlPoints.Items[i]);
        NewCurve.Weights.AppendArray([1.0, 1.0, 1.0, 1.0]);
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

  ConvexHullPoints.SetLength(0);
  ConvexHullPoints.AppendDynArray(ControlPoints);
  for i := 0 to BezierCurves.Count-1 do
  begin
    ConvexHullPoints.Add(BezierCurves[i].ControlPoints.Items[1]);
    ConvexHullPoints.Add(BezierCurves[i].ControlPoints.Items[2]);
  end;
end;

constructor TSmoothInterpolatedCurve.Create(const ATBegin, ATEnd: Float);
begin
  inherited;
  ConvexHullPoints := TDynVector3SingleArray.Create;
end;

destructor TSmoothInterpolatedCurve.Destroy;
begin
  FreeAndNil(BezierCurves);
  FreeAndNil(ConvexHullPoints);
  inherited;
end;

end.