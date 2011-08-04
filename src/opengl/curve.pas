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

{ 3D curves (TCurve and basic descendants). }

unit Curve;

interface

uses VectorMath, Boxes3D, KambiUtils, KambiScript,
  KambiClassUtils, Classes, Base3D, Frustum,
  FGL {$ifdef VER2_2}, FGLObjectList22 {$endif};

type
  { "Curve" is, in the sense of this class, some 3d object that
    @unorderedList(
      @itemSpacing compact
      @item can be seen as a set of points Point(t) for t in [TBegin, TEnd]
      @item more or less "fits" inside his BoundingBox
    )

      Note for BoundingBox method: Curve should fit inside this BoundingBox.
      For now, this does not have to be a "perfect fit", it may be smaller than
      it should be and it may be larger than it could be.
      This should be treated as something like a "good hint".
      (Maybe at some time I'll make this conditions more rigorous).
  }
  TCurve = class(T3D)
  private
    FTBegin, FTEnd: Float;
    FDefaultSegments: Cardinal;
  public
    { TBegin/End determine the valid range of t.
      Must be TBegin <= TEnd.
      @groupBegin }
    property TBegin: Float read FTBegin;
    property TEnd: Float read FTEnd;
    { @groupEnd }

    { This is the most important method that must be defined in subclasses.
      This determines the exact shape of the curve. }
    function Point(const t: Float): TVector3Single; virtual; abstract;

    { Here t is specified as @code(TBegin + i/Segments* (TEnd-TBegin)) }
    function PointOfSegment(i, Segments: Cardinal): TVector3Single;

    { This function renders a curve by dividing it to Segments line segments.
      So actually @italic(every) curve will be rendered as a set of straight lines.
      You should just give some large number for Segments to have something
      that will be really smooth.

      OpenGL commands:
      @orderedList(
        @item(This method calls glBegin(GL_LINE_STRIP);)
        @item(Then it calls glVertexv(PointOfSegment(i, Segments))
          for i in [0; Segments]
          (yes, this means that it calls glVertex Segments+1 times).)
        @item(Then this method calls glEnd.)
      ) }
    procedure Render(Segments: Cardinal);

    { Default number of segments, used when rendering by T3D interface
      (that is, @code(Render(Frustum, TransparentGroup...)) method.) }
    property DefaultSegments: Cardinal
      read FDefaultSegments write FDefaultSegments default 10;

    procedure Render(const Frustum: TFrustum;
      const Params: TRenderParams); override;

    constructor Create(const ATBegin, ATEnd: Float); reintroduce;
  end;

  TCurveList = specialize TFPGObjectList<TCurve>;

  { @abstract(This is a curve defined by explicitly giving functions for
    Point(t) = x(t), y(t), z(t) as KambiScript expressions.) }
  TKamScriptCurve = class(TCurve)
  protected
    FTVariable: TKamScriptFloat;
    FXFunction, FYFunction, FZFunction: TKamScriptExpression;
    FBoundingBox: TBox3D;
  public
    function Point(const t: Float): TVector3Single; override;

    { XFunction, YFunction, ZFunction are functions based on variable 't'.
      @groupBegin }
    property XFunction: TKamScriptExpression read FXFunction;
    property YFunction: TKamScriptExpression read FYFunction;
    property ZFunction: TKamScriptExpression read FZFunction;
    { @groupEnd }

    { This is the variable controlling 't' value, embedded also in
      XFunction, YFunction, ZFunction. }
    property TVariable: TKamScriptFloat read FTVariable;

    { This class provides simple implementation for BoundingBox: it is simply
      a BoundingBox of Point(i, SegmentsForBoundingBox)
      for i in [0 .. SegmentsForBoundingBox].
      Subclasses may override this to calculate something more accurate. }
    function BoundingBox: TBox3D; override;

    { XFunction, YFunction, ZFunction references are copied here,
      and will be freed in destructor (so don't Free them yourself). }
    constructor Create(const ATBegin, ATEnd: Float;
      AXFunction, AYFunction, AZFunction: TKamScriptExpression;
      ATVariable: TKamScriptFloat;
      ASegmentsForBoundingBox: Cardinal = 100);

    destructor Destroy; override;
  end;

  { @abstract(This is a basic abstract class for curves determined my some
    set of ControlPoints.)
    Note: it is @italic(not) defined in this class any correspondence between
    values of T (argument for Point function) and ControlPoints. }
  TControlPointsCurve = class(TCurve)
  private
    FBoundingBox: TBox3D;
  protected
    { Using these function you can control how Convex Hull (for RenderConvexHull)
      is calculated: CreateConvexHullPoints should return points that must be
      in convex hull (we will run ConvexHull function on those points),
      DestroyConvexHullPoints should finalize them.

      This way you can create new object in CreateConvexHullPoints and free it in
      DestroyConvexHullPoints, but you can also give in CreateConvexHullPoints
      reference to some already existing object and do nothing in
      DestroyConvexHullPoints. (we will not modify object given as
      CreateConvexHullPoints in any way)

      Default implementation in this class returns ControlPoints as
      CreateConvexHullPoints. (and does nothing in DestroyConvexHullPoints) }
    function CreateConvexHullPoints: TDynVector3SingleArray; virtual;
    procedure DestroyConvexHullPoints(Points: TDynVector3SingleArray); virtual;
  public
    ControlPoints: TDynVector3SingleArray;

    { glBegin(GL_POINTS) + glVertex fo each ControlPoints[i] + glEnd. }
    procedure RenderControlPoints;

    { This class provides implementation for BoundingBox: it is simply
      a BoundingBox of ControlPoints. Subclasses may (but don't have to)
      override this to calculate better (more accurate) BoundingBox. }
    function BoundingBox: TBox3D; override;

    { Always after changing ControlPoints and before calling Point,
      BoundingBox (and anything that calls them, e.g. Render calls Point)
      call this method. It recalculates necessary things.
      ControlPoints.Count must be >= 2.

      When overriding: always call inherited first. }
    procedure UpdateControlPoints; virtual;

    { Nice class name, with spaces, starts with a capital letter.
      Much better than ClassName. Especially under FPC 1.0.x where
      ClassName is always uppercased. }
    class function NiceClassName: string; virtual; abstract;

    { do glBegin(GL_POLYGON), glVertex(v)..., glEnd,
      where glVertex are points of Convex Hull of ControlPoints
      (ignoring Z-coord of ControlPoints). }
    procedure RenderConvexHull;

    { Constructor.
      This is virtual because it's called by CreateDivideKamScriptCurve.
      It's also useful in many places in curves.lpr. }
    constructor Create(const ATBegin, ATEnd: Float); virtual;

    { Calculates ControlPoints taking Point(i, ControlPointsCount-1)
      for i in [0 .. ControlPointsCount-1] from KamScriptCurve.
      TBegin and TEnd are copied from KamScriptCurve. }
    constructor CreateDivideKamScriptCurve(KamScriptCurve: TKamScriptCurve;
      ControlPointsCount: Cardinal);

    destructor Destroy; override;
  end;

  TControlPointsCurveClass = class of TControlPointsCurve;

  TControlPointsCurveList = specialize TFPGObjectList<TControlPointsCurve>;

  { @abstract(This is a class for curves that pass exactly through
    ControlPoints.)
    I.e. for each ControlPoint[i] there exists some value Ti
    that Point(Ti) = ControlPoint[i] and
    TBegin = T0 <= .. Ti-1 <= Ti <= Ti+1 ... <= Tn = TEnd
    (i.e. Point(TBegin) = ControlPoints[0],
          Point(TEnd) = ControlPoints[n]
     and all Ti are ordered,
     n = ControlPoints.High) }
  TInterpolatedCurve = class(TControlPointsCurve)
    { This can be overriden in subclasses.
      In this class this provides the most common implementation:
      equally (uniformly) spaced Ti values. }
    function ControlPointT(i: Integer): Float; virtual;
  end;

  { This is a curve defined as [Lx(t), Ly(t), Lz(t)] where
    L?(t) are Lagrange's interpolation polynomials.
    Lx(t) crosses points (ti, xi) (i = 0..ControlPoints.Count-1)
    where ti = TBegin + i/(ControlPoints.Count-1) * (TEnd-TBegin)
    and xi = ControlPoints[i, 0].
    Similarly for Ly and Lz.

    Later note: in fact, you can override ControlPointT to define
    function "ti" as you like. }
  TLagrangeInterpolatedCurve = class(TInterpolatedCurve)
  private
    { Values for Newton's polynomial form of Lx, Ly and Lz.
      Will be calculated in UpdateControlPoints. }
    Newton: array[0..2]of TDynFloatArray;
  public
    procedure UpdateControlPoints; override;
    function Point(const t: Float): TVector3Single; override;

    class function NiceClassName: string; override;

    constructor Create(const ATBegin, ATEnd: Float); override;
    destructor Destroy; override;
  end;

  { Natural cubic spline (1D).
    May be periodic or not. }
  TNaturalCubicSpline = class
  private
    FMinX, FMaxX: Float;
    FOwnsX, FOwnsY: boolean;
    FPeriodic: boolean;
    FX, FY: TDynFloatArray;
    M: TDynFloatArray;
  public
    property MinX: Float read FMinX;
    property MaxX: Float read FMaxX;
    property Periodic: boolean read FPeriodic;

    { Constructs natural cubic spline such that for every i in [0; X.Count-1]
      s(X[i]) = Y[i]. Must be X.Count = Y.Count.
      X must be already sorted.
      MinX = X[0], MaxX = X[X.Count-1].

      Warning: we will copy references to X and Y ! So make sure that these
      objects are available for the life of this object.
      We will free in destructor X if OwnsX and free Y if OwnsY. }
    constructor Create(X, Y: TDynFloatArray; AOwnsX, AOwnsY, APeriodic: boolean);
    destructor Destroy; override;

    { Evaluate value of natural cubic spline at x.
      Must be MinX <= x <= MaxX. }
    function Evaluate(x: Float): Float;
  end;

  { 3D curve defined by three 1D natural cubic splines.
    Works just like TLagrangeInterpolatedCurve, only the interpolation
    is different now. }
  TNaturalCubicSplineCurve_Abstract = class(TInterpolatedCurve)
  protected
    { Is the curve closed. May depend on ControlPoints,
      it will be recalculated in UpdateControlPoints. }
    function Closed: boolean; virtual; abstract;
  private
    { Created/Freed in UpdateControlPoints, Freed in Destroy }
    Spline: array[0..2]of TNaturalCubicSpline;
  public
    procedure UpdateControlPoints; override;
    function Point(const t: Float): TVector3Single; override;

    constructor Create(const ATBegin, ATEnd: Float); override;
    destructor Destroy; override;
  end;

  { 3D curve defined by three 1D natural cubic splines, automatically
    closed if first and last points match. This is the most often suitable
    non-abstract implementation of TNaturalCubicSplineCurve_Abstract. }
  TNaturalCubicSplineCurve = class(TNaturalCubicSplineCurve_Abstract)
  protected
    function Closed: boolean; override;
  public
    class function NiceClassName: string; override;
  end;

  { 3D curve defined by three 1D natural cubic splines, always treated as closed. }
  TNaturalCubicSplineCurveAlwaysClosed = class(TNaturalCubicSplineCurve_Abstract)
  protected
    function Closed: boolean; override;
  public
    class function NiceClassName: string; override;
  end;

  { 3D curve defined by three 1D natural cubic splines, never treated as closed. }
  TNaturalCubicSplineCurveNeverClosed = class(TNaturalCubicSplineCurve_Abstract)
  protected
    function Closed: boolean; override;
  public
    class function NiceClassName: string; override;
  end;

implementation

uses SysUtils, GL, GLU, ConvexHullUnit, KambiGLUtils;

{ TCurve ------------------------------------------------------------ }

function TCurve.PointOfSegment(i, Segments: Cardinal): TVector3Single;
begin
 Result := Point(TBegin + (i/Segments) * (TEnd-TBegin));
end;

procedure TCurve.Render(Segments: Cardinal);
var i: Integer;
begin
 glBegin(GL_LINE_STRIP);
 for i := 0 to Segments do glVertexv(PointOfSegment(i, Segments));
 glEnd;
end;

procedure TCurve.Render(const Frustum: TFrustum;
  const Params: TRenderParams);
begin
  if Params.TransparentGroup in [tgAll, tgOpaque] then
    Render(DefaultSegments);
end;

constructor TCurve.Create(const ATBegin, ATEnd: Float);
begin
 inherited Create(nil);
 FTBegin := ATBegin;
 FTEnd := ATEnd;
 FDefaultSegments := 10;
end;

{ TKamScriptCurve ------------------------------------------------------------ }

function TKamScriptCurve.Point(const t: Float): TVector3Single;
begin
  TVariable.Value := t;
  Result[0] := (XFunction.Execute as TKamScriptFloat).Value;
  Result[1] := (YFunction.Execute as TKamScriptFloat).Value;
  Result[2] := (ZFunction.Execute as TKamScriptFloat).Value;

 {test: Writeln('Point at t = ',FloatToNiceStr(Single(t)), ' is (',
   VectorToNiceStr(Result), ')');}
end;

function TKamScriptCurve.BoundingBox: TBox3D;
begin
 Result := FBoundingBox;
end;

constructor TKamScriptCurve.Create(const ATBegin, ATEnd: Float;
  AXFunction, AYFunction, AZFunction: TKamScriptExpression;
  ATVariable: TKamScriptFloat;
  ASegmentsForBoundingBox: Cardinal);
var i, k: Integer;
    P: TVector3Single;
begin
 inherited Create(ATBegin, ATEnd);
 FXFunction := AXFunction;
 FYFunction := AYFunction;
 FZFunction := AZFunction;
 FTVariable := ATVariable;

 { calculate FBoundingBox }
 P := PointOfSegment(0, ASegmentsForBoundingBox); { = Point(TBegin) }
 FBoundingBox[0] := P;
 FBoundingBox[1] := P;
 for i := 1 to ASegmentsForBoundingBox do
 begin
  P := PointOfSegment(i, ASegmentsForBoundingBox);
  for k := 0 to 2 do
  begin
   FBoundingBox[0, k] := Min(FBoundingBox[0, k], P[k]);
   FBoundingBox[1, k] := Max(FBoundingBox[1, k], P[k]);
  end;
 end;
end;

destructor TKamScriptCurve.Destroy;
begin
 FXFunction.FreeByParentExpression;
 FYFunction.FreeByParentExpression;
 FZFunction.FreeByParentExpression;
 inherited;
end;

{ TControlPointsCurve ------------------------------------------------ }

procedure TControlPointsCurve.RenderControlPoints;
var i: Integer;
begin
 glBegin(GL_POINTS);
 for i := 0 to ControlPoints.Count-1 do glVertexv(ControlPoints.Items[i]);
 glEnd;
end;

function TControlPointsCurve.BoundingBox: TBox3D;
begin
 Result := FBoundingBox;
end;

procedure TControlPointsCurve.UpdateControlPoints;
begin
 FBoundingBox := CalculateBoundingBox(PVector3Single(ControlPoints.Items),
   ControlPoints.Count, 0);
end;

function TControlPointsCurve.CreateConvexHullPoints: TDynVector3SingleArray;
begin
 Result := ControlPoints;
end;

procedure TControlPointsCurve.DestroyConvexHullPoints(Points: TDynVector3SingleArray);
begin
end;

procedure TControlPointsCurve.RenderConvexHull;
var CHPoints: TDynVector3SingleArray;
    CH: TDynIntegerArray;
    i: Integer;
begin
 CHPoints := CreateConvexHullPoints;
 try
  CH := ConvexHull(CHPoints);
  try
   glBegin(GL_POLYGON);
   try
    for i := 0 to CH.Count-1 do
     glVertexv(CHPoints.Items[CH[i]]);
   finally glEnd end;
  finally CH.Free end;
 finally DestroyConvexHullPoints(CHPoints) end;
end;

constructor TControlPointsCurve.Create(const ATBegin, ATEnd: Float);
begin
 inherited Create(ATBegin, ATEnd);
 ControlPoints := TDynVector3SingleArray.Create;
 { DON'T call UpdateControlPoints from here - UpdateControlPoints is virtual !
   So we set FBoundingBox by hand. }
 FBoundingBox := EmptyBox3D;
end;

constructor TControlPointsCurve.CreateDivideKamScriptCurve(
  KamScriptCurve: TKamScriptCurve; ControlPointsCount: Cardinal);
var i: Integer;
begin
 Create(KamScriptCurve.TBegin, KamScriptCurve.TEnd);
 ControlPoints.Count := ControlPointsCount;
 for i := 0 to ControlPointsCount-1 do
  ControlPoints.Items[i] := KamScriptCurve.PointOfSegment(i, ControlPointsCount-1);
 UpdateControlPoints;
end;

destructor TControlPointsCurve.Destroy;
begin
 FreeAndNil(ControlPoints);
 inherited;
end;

{ TInterpolatedCurve ----------------------------------------------- }

function TInterpolatedCurve.ControlPointT(i: Integer): Float;
begin
 Result := TBegin + (i/(ControlPoints.Count-1)) * (TEnd-TBegin);
end;

{ TLagrangeInterpolatedCurve ----------------------------------------------- }

procedure TLagrangeInterpolatedCurve.UpdateControlPoints;
var i, j, k, l: Integer;
begin
 inherited;

 for i := 0 to 2 do
 begin
  Newton[i].Count := ControlPoints.Count;
  for j := 0 to ControlPoints.Count-1 do
   Newton[i].Items[j] := ControlPoints.Items[j, i];

  { licz kolumny tablicy ilorazow roznicowych in place, overriding Newton[i] }
  for k := 1 to ControlPoints.Count-1 do
   { licz k-ta kolumne }
   for l := ControlPoints.Count-1 downto k do
    { licz l-ty iloraz roznicowy w k-tej kolumnie }
    Newton[i].Items[l]:=
      (Newton[i].Items[l] - Newton[i].Items[l-1]) /
      (ControlPointT(l) - ControlPointT(l-k));
 end;
end;

function TLagrangeInterpolatedCurve.Point(const t: Float): TVector3Single;
var i, k: Integer;
    f: Float;
begin
 for i := 0 to 2 do
 begin
  { Oblicz F przy pomocy uogolnionego schematu Hornera z Li(t).
    Wspolczynniki b_k sa w tablicy Newton[i].Items[k],
    wartosci t_k sa w ControlPointT(k). }
  F := Newton[i].Items[ControlPoints.Count-1];
  for k := ControlPoints.Count-2 downto 0 do
   F := F*(t-ControlPointT(k)) + Newton[i].Items[k];
  { Dopiero teraz przepisz F do Result[i]. Dzieki temu obliczenia wykonujemy
    na Floatach. Tak, to naprawde pomaga -- widac ze kiedy uzywamy tego to
    musimy miec wiecej ControlPoints zeby dostac Floating Point Overflow. }
  Result[i] := F;
 end;
end;

class function TLagrangeInterpolatedCurve.NiceClassName: string;
begin
 Result := 'Lagrange interpolated curve';
end;

constructor TLagrangeInterpolatedCurve.Create(const ATBegin, ATEnd: Float);
var i: Integer;
begin
 inherited Create(ATBegin, ATEnd);
 for i := 0 to 2 do Newton[i] := TDynFloatArray.Create;
end;

destructor TLagrangeInterpolatedCurve.Destroy;
var i: Integer;
begin
 for i := 0 to 2 do FreeAndNil(Newton[i]);
 inherited;
end;

{ TNaturalCubicSpline -------------------------------------------------------- }

constructor TNaturalCubicSpline.Create(X, Y: TDynFloatArray;
  AOwnsX, AOwnsY, APeriodic: boolean);

{ Based on SLE (== Stanislaw Lewanowicz) notes on ii.uni.wroc.pl lecture. }

var
  { n = X.High. Integer, not Cardinal, to avoid some overflows in n-2. }
  n: Integer;

  { [Not]PeriodicDK licza wspolczynnik d_k. k jest na pewno w [1; n-1] }
  function PeriodicDK(k: Integer): Float;
  var h_k: Float;
      h_k1: Float;
  begin
   h_k  := X[k] - X[k-1];
   h_k1 := X[k+1] - X[k];
   Result := ( 6 / (h_k + h_k1) ) *
             ( (Y[k+1] - Y[k]) / h_k1 -
               (Y[k] - Y[k-1]) / h_k
             );
  end;

  { special version, works like PeriodicDK(n) should work
    (but does not, PeriodicDK works only for k < n.) }
  function PeriodicDN: Float;
  var h_n: Float;
      h_n1: Float;
  begin
   h_n  := X[n] - X[n-1];
   h_n1 := X[1] - X[0];
   Result := ( 6 / (h_n + h_n1) ) *
             ( (Y[1] - Y[n]) / h_n1 -
               (Y[n] - Y[n-1]) / h_n
             );
  end;

  function IlorazRoznicowy(const Start, Koniec: Cardinal): Float;
  { liczenie pojedynczego ilorazu roznicowego wzorem rekurencyjnym.
    Poniewaz do algorytmu bedziemy potrzebowali tylko ilorazow stopnia 3
    (lub mniej) i to tylko na chwile - wiec taka implementacja
    (zamiast zabawa w tablice) bedzie prostsza i wystarczajaca. }
  begin
   if Start = Koniec then
    Result := Y[Start] else
    Result := (IlorazRoznicowy(Start + 1, Koniec) -
               IlorazRoznicowy(Start, Koniec - 1))
               / (X[Koniec] - X[Start]);
  end;

  function NotPeriodicDK(k: Integer): Float;
  begin
   Result := 6 * IlorazRoznicowy(k-1, k+1);
  end;

var u, q, s, t, v: TDynFloatArray;
    hk, dk, pk, delta_k, delta_n, h_n, h_n1: Float;
    k: Integer;
begin
 inherited Create;
 Assert(X.Count = Y.Count);
 FMinX := X[0];
 FMaxX := X[X.High];
 FOwnsX := AOwnsX;
 FOwnsY := AOwnsY;
 FX := X;
 FY := Y;
 FPeriodic := APeriodic;

 { prepare to calculate M }
 n := X.High;
 M := TDynFloatArray.Create(n+1);

 { Algorytm obliczania wartosci M[0..n] z notatek SLE, te same oznaczenia.
   Sa tutaj polaczone algorytmy na Periodic i not Perdiodic, zeby mozliwie
   nie duplikowac kodu (i uniknac pomylek z copy&paste).
   Tracimy przez to troche czasu (wielokrotne testy "if Periodic ..."),
   ale kod jest prostszy i bardziej elegancki.

   Notka: W notatkach SLE dla Periodic = true w jednym miejscu uzyto
   M[n+1] ale tak naprawde nie musimy go liczyc ani uzywac. }

 u := nil;
 q := nil;
 s := nil;
 try
  u := TDynFloatArray.Create(n);
  q := TDynFloatArray.Create(n);
  if Periodic then s := TDynFloatArray.Create(n);

  { calculate u[0], q[0], s[0] }
  u[0] := 0;
  q[0] := 0;
  if Periodic then s[0] := 1;

  for k := 1 to n - 1 do
  begin
   { calculate u[k], q[k], s[k] }

   hk := X[k] - X[k-1];
   { delta[k] = h[k] / (h[k] + h[k+1])
              = h[k] / (X[k] - X[k-1] + X[k+1] - X[k])
              = h[k] / (X[k+1] - X[k-1])
   }
   delta_k := hk / (X[k+1] - X[k-1]);
   pk := delta_k * q[k-1] + 2;
   q[k]:=(delta_k - 1) / pk;
   if Periodic then s[k] := - delta_k * s[k-1] / pk;
   if Periodic then
    dk := PeriodicDK(k) else
    dk := NotPeriodicDK(k);
   u[k]:=(dk - delta_k * u[k-1]) / pk;
  end;

  { teraz wyliczamy wartosci M[0..n] }
  if Periodic then
  begin
   t := nil;
   v := nil;
   try
    t := TDynFloatArray.Create(n+1);
    v := TDynFloatArray.Create(n+1);

    t[n] := 1;
    v[n] := 0;

    { z notatek SLE wynika ze t[0], v[0] nie sa potrzebne (i k moze robic
      "downto 1" zamiast "downto 0") ale t[0], v[0] MOGA byc potrzebne:
      przy obliczaniu M[n] dla n = 1. }
    for k := n-1 downto 0 do
    begin
     t[k] := q[k] * t[k+1] + s[k];
     v[k] := q[k] * v[k+1] + u[k];
    end;

    h_n  := X[n] - X[n-1];
    h_n1 := X[1] - X[0];
    delta_n := h_n / (h_n + h_n1);
    M[n] := (PeriodicDN - (1-delta_n) * v[1] - delta_n * v[n-1]) /
            (2          + (1-delta_n) * t[1] + delta_n * t[n-1]);
    M[0] := M[n];
    for k := n-1 downto 1 do  M[k] := v[k] + t[k] * M[n];
   finally
    t.Free;
    v.Free;
   end;
  end else
  begin
   { zawsze M[0] = M[n] = 0, zeby latwo bylo zapisac obliczenia w Evaluate }
   M[0] := 0;
   M[n] := 0;
   M[n-1] := u[n-1];
   for k := n-2 downto 1 do M[k] := u[k] + q[k] * M[k+1];
  end;
 finally
  u.Free;
  q.Free;
  s.Free;
 end;
end;

destructor TNaturalCubicSpline.Destroy;
begin
 if FOwnsX then FX.Free;
 if FOwnsY then FY.Free;
 M.Free;
 inherited;
end;

function TNaturalCubicSpline.Evaluate(x: Float): Float;

  function Power3rd(const a: Float): Float;
  begin
   Result := a * a * a;
  end;

var k, KMin, KMax, KMiddle: Cardinal;
    hk: Float;
begin
 Clamp(x, MinX, MaxX);

 { calculate k: W ktorym przedziale x[k-1]..x[k] jest argument ?
   TODO: nalezoloby pomyslec o wykorzystaniu faktu
   ze czesto wiadomo iz wezly x[i] sa rownoodlegle. }
 KMin := 1;
 KMax := FX.High;
 repeat
  KMiddle:=(KMin + KMax) div 2;
  { jak jest ulozony x w stosunku do przedzialu FX[KMiddle-1]..FX[KMiddle] ? }
  if x < FX[KMiddle-1] then KMax := KMiddle-1 else
   if x > FX[KMiddle] then KMin := KMiddle+1 else
    begin
     KMin := KMiddle; { set only KMin, KMax is meaningless from now }
     break;
    end;
 until KMin = KMax;

 k := KMin;

 Assert(Between(x, FX[k-1], FX[k]));

 { obliczenia uzywaja tych samych symboli co w notatkach SLE }
 { teraz obliczam wartosc s(x) gdzie s to postac funkcji sklejanej
   na przedziale FX[k-1]..FX[k] }
 hk := FX[k] - FX[k-1];
 Result := ( ( M[k-1] * Power3rd(FX[k] - x) + M[k] * Power3rd(x - FX[k-1]) )/6 +
             ( FY[k-1] - M[k-1]*Sqr(hk)/6 )*(FX[k] - x)                        +
             ( FY[k]   - M[k  ]*Sqr(hk)/6 )*(x - FX[k-1])
           ) / hk;
end;

{ TNaturalCubicSplineCurve_Abstract ------------------------------------------- }

procedure TNaturalCubicSplineCurve_Abstract.UpdateControlPoints;
var i, j: Integer;
    SplineX, SplineY: TDynFloatArray;
begin
 inherited;

 { calculate SplineX.
   Spline[0] and Spline[1] and Spline[2] will share the same reference to X.
   Only Spline[2] will own SplineX. (Spline[2] will be always Freed as the
   last one, so it's safest to set OwnsX in Spline[2]) }
 SplineX := TDynFloatArray.Create(ControlPoints.Count);
 for i := 0 to ControlPoints.Count-1 do SplineX[i] := ControlPointT(i);

 for i := 0 to 2 do
 begin
  FreeAndNil(Spline[i]);

  { calculate SplineY }
  SplineY := TDynFloatArray.Create(ControlPoints.Count);
  for j := 0 to ControlPoints.Count-1 do SplineY[j] := ControlPoints.Items[j, i];

  Spline[i] := TNaturalCubicSpline.Create(SplineX, SplineY, i = 2, true,
    Closed);
 end;
end;

function TNaturalCubicSplineCurve_Abstract.Point(const t: Float): TVector3Single;
var i: Integer;
begin
 for i := 0 to 2 do Result[i] := Spline[i].Evaluate(t);
end;

constructor TNaturalCubicSplineCurve_Abstract.Create(const ATBegin, ATEnd: Float);
begin
 inherited Create(ATBegin, ATEnd);
end;

destructor TNaturalCubicSplineCurve_Abstract.Destroy;
var i: Integer;
begin
 for i := 0 to 2 do FreeAndNil(Spline[i]);
 inherited;
end;

{ TNaturalCubicSplineCurve -------------------------------------------------- }

class function TNaturalCubicSplineCurve.NiceClassName: string;
begin
 Result := 'Natural cubic spline curve';
end;

function TNaturalCubicSplineCurve.Closed: boolean;
begin
 Result := VectorsEqual(ControlPoints.Items[0],
                      ControlPoints.Items[ControlPoints.Count-1]);
end;

{ TNaturalCubicSplineCurveAlwaysClosed -------------------------------------- }

class function TNaturalCubicSplineCurveAlwaysClosed.NiceClassName: string;
begin
 Result := 'Natural cubic spline curve (closed)';
end;

function TNaturalCubicSplineCurveAlwaysClosed.Closed: boolean;
begin
 Result := true;
end;

{ TNaturalCubicSplineCurveNeverClosed ---------------------------------------- }

class function TNaturalCubicSplineCurveNeverClosed.NiceClassName: string;
begin
 Result := 'Natural cubic spline curve (not closed)';
end;

function TNaturalCubicSplineCurveNeverClosed.Closed: boolean;
begin
 Result := false;
end;

end.