{
  Copyright 2004-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ 3D curves (TCurve and basic descendants). }
unit CastleCurves;

{$I castleconf.inc}

{$modeswitch nestedprocvars}{$H+}

interface

uses Classes, FGL, CastleVectors, CastleBoxes, CastleUtils, CastleScript,
  CastleClassUtils, Castle3D, CastleFrustum, CastleColors;

type
  { 3D curve, a set of points defined by a continous function @link(Point)
    for arguments within [TBegin, TEnd].

    Note that some descendants return only an approximate BoundingBox result,
    it may be too small or too large sometimes.
    (Maybe at some time I'll make this more rigorous, as some code may require
    that it's a proper bounding box, maybe too large but never too small.) }
  TCurve = class(T3D)
  private
    FColor: TCastleColor;
    FLineWidth: Single;
    FTBegin, FTEnd: Single;
    FDefaultSegments: Cardinal;
  public
    { The valid range of curve function argument. Must be TBegin <= TEnd.
      @groupBegin }
    property TBegin: Single read FTBegin write FTBegin default 0;
    property TEnd: Single read FTEnd write FTEnd default 1;
    { @groupEnd }

    { Curve function, for each parameter value determine the 3D point.
      This determines the actual shape of the curve. }
    function Point(const t: Float): TVector3Single; virtual; abstract;

    { Curve function to work with rendered line segments begin/end points.
      This is simply a more specialized version of @link(Point),
      it scales the argument such that you get Point(TBegin) for I = 0
      and you get Point(TEnd) for I = Segments. }
    function PointOfSegment(i, Segments: Cardinal): TVector3Single;

    { Render curve by dividing it into a given number of line segments.
      So actually @italic(every) curve will be rendered as a set of straight lines.
      You should just give some large number for Segments to have something
      that will be really smooth.

      This does direct OpenGL rendering right now, setting GL color
      and then rendering a line strip. }
    procedure Render(Segments: Cardinal); deprecated 'Do not render curve directly by this method, instead add the curve to SceneManager.Items to have it rendered automatically.';

    { Curve rendering color. White by default. }
    property Color: TCastleColor read FColor write FColor;

    property LineWidth: Single read FLineWidth write FLineWidth default 1;

    { Default number of segments, used when rendering by T3D interface
      (that is, @code(Render(Frustum, TransparentGroup...)) method.) }
    property DefaultSegments: Cardinal
      read FDefaultSegments write FDefaultSegments default 10;

    procedure Render(const Frustum: TFrustum;
      const Params: TRenderParams); override;

    constructor Create(AOwner: TComponent); override;
  end;

  TCurveList = specialize TFPGObjectList<TCurve>;

  { Curve defined by explicitly giving functions for
    Point(t) = x(t), y(t), z(t) as CastleScript expressions. }
  TCasScriptCurve = class(TCurve)
  private
    FSegmentsForBoundingBox: Cardinal;
    procedure SetSegmentsForBoundingBox(AValue: Cardinal);
    procedure SetTVariable(AValue: TCasScriptFloat);
  protected
    FTVariable: TCasScriptFloat;
    FFunction: array [0..2] of TCasScriptExpression;
    FBoundingBox: TBox3D;
    function GetFunction(const Index: Integer): TCasScriptExpression;
    procedure SetFunction(const Index: Integer; const Value: TCasScriptExpression);
    procedure UpdateBoundingBox;
  public
    function Point(const t: Float): TVector3Single; override;

    { XFunction, YFunction, ZFunction are functions based on variable 't'.
      Once set, these instances become owned by this class, do not free
      them yourself!
      @groupBegin }
    property XFunction: TCasScriptExpression index 0 read GetFunction write SetFunction;
    property YFunction: TCasScriptExpression index 1 read GetFunction write SetFunction;
    property ZFunction: TCasScriptExpression index 2 read GetFunction write SetFunction;
    { @groupEnd }

    { This is the variable controlling 't' value, embedded also in
      XFunction, YFunction, ZFunction. This is NOT owned by this class,
      make sure to free it yourself! }
    property TVariable: TCasScriptFloat read FTVariable write SetTVariable;

    property SegmentsForBoundingBox: Cardinal
      read FSegmentsForBoundingBox write SetSegmentsForBoundingBox default 100;

    { Simple bounding box. It is simply
      a BoundingBox of Point(i, SegmentsForBoundingBox)
      for i in [0 .. SegmentsForBoundingBox].
      Subclasses may override this to calculate something more accurate. }
    function BoundingBox: TBox3D; override;

    constructor Create(AOwner: TComponent); override;

    destructor Destroy; override;
  end;

  { A basic abstract class for curves determined my some set of ControlPoints.
    Note: it is @italic(not) defined in this class any correspondence between
    values of T (argument for Point function) and ControlPoints. }
  TControlPointsCurve = class(TCurve)
  private
    FBoundingBox: TBox3D;
    FControlPointsColor: TCastleColor;
    FConvexHullColor: TCastleColor;
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
    function CreateConvexHullPoints: TVector3SingleList; virtual;
    procedure DestroyConvexHullPoints(Points: TVector3SingleList); virtual;
  public
    ControlPoints: TVector3SingleList;

    property ControlPointsColor: TCastleColor read FControlPointsColor write FControlPointsColor;

    { Render control points, using ControlPointsColor. }
    procedure RenderControlPoints;

    { This class provides implementation for BoundingBox: it is simply
      a BoundingBox of ControlPoints. Subclasses may (but don't have to)
      override this to calculate better (more accurate) BoundingBox. }
    function BoundingBox: TBox3D; override;

    { Always after changing ControlPoints or TBegin or TEnd and before calling Point,
      BoundingBox (and anything that calls them, e.g. Render calls Point)
      call this method. It recalculates necessary things.
      ControlPoints.Count must be >= 2.

      When overriding: always call inherited first. }
    procedure UpdateControlPoints; virtual;

    { Nice class name, with spaces, starts with a capital letter.
      Much better than ClassName. Especially under FPC 1.0.x where
      ClassName is always uppercased. }
    class function NiceClassName: string; virtual; abstract;

    property ConvexHullColor: TCastleColor read FConvexHullColor write FConvexHullColor;

    { Render convex hull polygon, using ConvexHullColor.
      Ignores Z-coord of ControlPoints. }
    procedure RenderConvexHull;

    { Constructor.
      It has to be virtual because it's called by CreateDivideCasScriptCurve. }
    constructor Create(AOwner: TComponent); override;

    { Calculates ControlPoints taking Point(i, ControlPointsCount-1)
      for i in [0 .. ControlPointsCount-1] from CasScriptCurve.
      TBegin and TEnd are copied from CasScriptCurve. }
    constructor CreateDivideCasScriptCurve(CasScriptCurve: TCasScriptCurve;
      ControlPointsCount: Cardinal);

    destructor Destroy; override;
  end;

  TControlPointsCurveClass = class of TControlPointsCurve;

  TControlPointsCurveList = specialize TFPGObjectList<TControlPointsCurve>;

  { Curve that passes exactly through it's ControlPoints.x
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

  { Curve defined as [Lx(t), Ly(t), Lz(t)] where
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
    Newton: array[0..2]of TFloatList;
  public
    procedure UpdateControlPoints; override;
    function Point(const t: Float): TVector3Single; override;

    class function NiceClassName: string; override;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end deprecated 'Rendering of this is not portable to OpenGLES, and this is not really a useful curve for most practical game uses. For portable and fast curves consider using X3D NURBS nodes (wrapped in a TCastleScene) instead.';

  { Natural cubic spline (1D).
    May be periodic or not. }
  TNaturalCubicSpline = class
  private
    FMinX, FMaxX: Float;
    FOwnsX, FOwnsY: boolean;
    FPeriodic: boolean;
    FX, FY: TFloatList;
    M: TFloatList;
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
    constructor Create(X, Y: TFloatList; AOwnsX, AOwnsY, APeriodic: boolean);
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

    constructor Create(AOwner: TComponent); override;
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
  end deprecated 'Rendering of this is not portable to OpenGLES, and this is not really a useful curve for most practical game uses. For portable and fast curves consider using X3D NURBS nodes (wrapped in a TCastleScene) instead.';

  { 3D curve defined by three 1D natural cubic splines, always treated as closed. }
  TNaturalCubicSplineCurveAlwaysClosed = class(TNaturalCubicSplineCurve_Abstract)
  protected
    function Closed: boolean; override;
  public
    class function NiceClassName: string; override;
  end deprecated 'Rendering of this is not portable to OpenGLES, and this is not really a useful curve for most practical game uses. For portable and fast curves consider using X3D NURBS nodes (wrapped in a TCastleScene) instead.';

  { 3D curve defined by three 1D natural cubic splines, never treated as closed. }
  TNaturalCubicSplineCurveNeverClosed = class(TNaturalCubicSplineCurve_Abstract)
  protected
    function Closed: boolean; override;
  public
    class function NiceClassName: string; override;
  end deprecated 'Rendering of this is not portable to OpenGLES, and this is not really a useful curve for most practical game uses. For portable and fast curves consider using X3D NURBS nodes (wrapped in a TCastleScene) instead.';

  { Rational Bezier curve (Bezier curve with weights).
    Note: for Bezier Curve ControlPoints.Count MAY be 1.
    (For TControlPointsCurve it must be >= 2) }
  TRationalBezierCurve = class(TControlPointsCurve)
  public
    function Point(const t: Float): TVector3Single; override;
    class function NiceClassName: string; override;
  public
    { Curve weights.
      Must always be Weights.Count = ControlPoints.Count.
      After changing Weights you also have to call UpdateControlPoints.}
    Weights: TFloatList;

    procedure UpdateControlPoints; override;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end deprecated 'Rendering of TRationalBezierCurve is not portable to OpenGLES (that is: Android and iOS) and not very efficient. For portable and fast curves consider using X3D NURBS nodes (wrapped in a TCastleScene) instead.';

  {$warnings off} { Consciously using deprecated stuff. }
  TRationalBezierCurveList = specialize TFPGObjectList<TRationalBezierCurve>;
  {$warnings on}

  TCubicBezier2DPoints = array [0..3] of TVector2Single;
  TCubicBezier3DPoints = array [0..3] of TVector3Single;

  { Piecewise (composite) cubic Bezier curve.
    Each segment (ControlPoints[i]..ControlPoints[i+1])
    is a cubic Bezier curve (Bezier with 4 control points,
    2 points in the middle are auto-calculated for max smoothness).

    This is a cubic B-spline. Which is equivalent to C2 continuous
    composite Bézier curves. See
    https://en.wikipedia.org/wiki/Spline_%28mathematics%29 .

    You can also explicitly convert it to a list of bezier curves using
    ToRationalBezierCurves.

    ControlPoints.Count may be 1 (in general,
    for TControlPointsCurve, it must be >= 2).

    Note that, while using this to calculate points on curve is OK,
    rendering this, and placing it on SceneManager.Items list, is deprecated.
    It is not portable to OpenGLES (that is: Android and iOS) and
    not very efficient. For portable and fast curves consider using
    X3D NURBS nodes (wrapped in a TCastleScene) instead.
    Or convert this curve to a TLineSetNode X3D node.
  }
  TPiecewiseCubicBezier = class(TInterpolatedCurve)
  private
    BezierCurves: array of TCubicBezier3DPoints;
    ConvexHullPoints: TVector3SingleList;
  protected
    function CreateConvexHullPoints: TVector3SingleList; override;
    procedure DestroyConvexHullPoints(Points: TVector3SingleList); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateControlPoints; override;
    function Point(const t: Float): TVector3Single; override;
    class function NiceClassName: string; override;
  end;

  {$warnings off} { Consciously using deprecated stuff. }
  TSmoothInterpolatedCurve = TPiecewiseCubicBezier;
  {$warnings on}

{ Cubic (4 control points) Bezier curve (with all weights equal) in 1D. }
function CubicBezier1D(T: Single; const Points: TVector4Single): Single;

{ Cubic (4 control points) Bezier curve (with all weights equal) in 2D. }
function CubicBezier2D(T: Single; const Points: TCubicBezier2DPoints): TVector2Single;

{ Cubic (4 control points) Bezier curve (with all weights equal) in 3D. }
function CubicBezier3D(T: Single; const Points: TCubicBezier3DPoints): TVector3Single;

{ Catmull-Rom spline. Nice way to have a function that for certain arguments
  reaches certain values, and between interpolates smoothly.

  Catmull-Rom splines are a special case of cubic Hermite splines, see
  https://en.wikipedia.org/wiki/Cubic_Hermite_spline . }
function CatmullRomSpline(const X: Single; const Loop: boolean;
  const Arguments: TSingleList;
  const Values: TSingleList): Single;

{ Catmull-Rom spline low-level function.
  For X in [0..1], the curve values change from V1 to V2.
  V0 and V3 are curve values outside the [0..1] range, used to calculate tangents.

  See http://www.mvps.org/directx/articles/catmull/.

  @seealso CatmullRomSpline }
function CatmullRom(const V0, V1, V2, V3, X: Single): Single;

{ Hermite spline. Nice way to have a function that for certain arguments
  reaches certain values, and between interpolates smoothly.
  Requires specifying tangent values (use @link(CatmullRomSpline)
  or @link(HermiteTenseSpline) to use automatic tangents). }
function HermiteSpline(const X: Single; const Loop: boolean;
  const Arguments, Values, Tangents: TSingleList): Single;

{ Hermite spline with tangents zero (it will be horizontal at control points).
  Nice way to have a function that for certain arguments
  reaches certain values, and between interpolates smoothly.

  This is equivalent (for faster) to using @link(HermiteSpline) with all
  tangents equal to zero.

  This is called a "cardinal spline", a special case of
  Hermite spline, with all tangents calculated with "tension" parameter equal
  to 1 (maximum), which means that all tangents are simply zero (horizontal).
  See https://en.wikipedia.org/wiki/Cubic_Hermite_spline for math behind this. }
function HermiteTenseSpline(const X: Single; const Loop: boolean;
  const Arguments, Values: TSingleList): Single;

implementation

uses SysUtils, Math, CastleGL, CastleConvexHull, CastleGLUtils;

{ TCurve ------------------------------------------------------------ }

function TCurve.PointOfSegment(i, Segments: Cardinal): TVector3Single;
begin
  Result := Point(TBegin + (i/Segments) * (TEnd-TBegin));
end;

procedure TCurve.Render(Segments: Cardinal);
var i: Integer;
begin
  {$ifndef OpenGLES} //TODO-es
  glColorv(Color);
  glLineWidth(LineWidth);
  glBegin(GL_LINE_STRIP);
  for i := 0 to Segments do glVertexv(PointOfSegment(i, Segments));
  glEnd;
  {$endif}
end;

procedure TCurve.Render(const Frustum: TFrustum;
  const Params: TRenderParams);
begin
  if GetExists and (not Params.Transparent) and Params.ShadowVolumesReceivers then
  begin
    {$ifndef OpenGLES} //TODO-es
    if not Params.RenderTransformIdentity then
    begin
      glPushMatrix;
      glMultMatrix(Params.RenderTransform);
    end;
    {$endif}

    {$warnings off}
    Render(DefaultSegments);
    {$warnings on}

    {$ifndef OpenGLES}
    if not Params.RenderTransformIdentity then
      glPopMatrix;
    {$endif}
  end;
end;

constructor TCurve.Create(AOwner: TComponent);
begin
  inherited;
  FTBegin := 0;
  FTEnd := 1;
  FDefaultSegments := 10;
  FLineWidth := 1;
  FColor := White;
end;

{ TCasScriptCurve ------------------------------------------------------------ }

procedure TCasScriptCurve.SetTVariable(AValue: TCasScriptFloat);
begin
  if FTVariable = AValue then Exit;
  FTVariable := AValue;
  UpdateBoundingBox;
end;

procedure TCasScriptCurve.SetSegmentsForBoundingBox(AValue: Cardinal);
begin
  if FSegmentsForBoundingBox = AValue then Exit;
  FSegmentsForBoundingBox := AValue;
  UpdateBoundingBox;
end;

function TCasScriptCurve.GetFunction(const Index: Integer): TCasScriptExpression;
begin
  Result := FFunction[Index];
end;

procedure TCasScriptCurve.SetFunction(const Index: Integer;
  const Value: TCasScriptExpression);
begin
  if FFunction[Index] = Value then Exit;

  if FFunction[Index] <> nil then
    FFunction[Index].FreeByParentExpression;

  FFunction[Index] := Value;
  UpdateBoundingBox;
end;

procedure TCasScriptCurve.UpdateBoundingBox;
var
  i, k: Integer;
  P: TVector3Single;
begin
  if (XFunction = nil) or
     (YFunction = nil) or
     (ZFunction = nil) or
     (TVariable = nil) then
    FBoundingBox := EmptyBox3D else
  begin
    { calculate FBoundingBox }
    P := PointOfSegment(0, SegmentsForBoundingBox); { = Point(TBegin) }
    FBoundingBox.Data[0] := P;
    FBoundingBox.Data[1] := P;
    for i := 1 to SegmentsForBoundingBox do
    begin
      P := PointOfSegment(i, SegmentsForBoundingBox);
      for k := 0 to 2 do
      begin
        FBoundingBox.Data[0, k] := Min(FBoundingBox.Data[0, k], P[k]);
        FBoundingBox.Data[1, k] := Max(FBoundingBox.Data[1, k], P[k]);
      end;
    end;
  end;
end;

function TCasScriptCurve.Point(const t: Float): TVector3Single;
var
  I: Integer;
begin
  TVariable.Value := T;
  for I := 0 to 2 do
    Result[I] := (FFunction[I].Execute as TCasScriptFloat).Value;

  {test: Writeln('Point at t = ',FloatToNiceStr(Single(t)), ' is (',
    VectorToNiceStr(Result), ')');}
end;

function TCasScriptCurve.BoundingBox: TBox3D;
begin
  Result := FBoundingBox;
end;

constructor TCasScriptCurve.Create(AOwner: TComponent);
begin
  inherited;
  FSegmentsForBoundingBox := 100;
end;

destructor TCasScriptCurve.Destroy;
var
  I: Integer;
begin
  for I := 0 to 2 do
    if FFunction[I] <> nil then
    begin
      FFunction[I].FreeByParentExpression;
      FFunction[I] := nil;
    end;
  inherited;
end;

{ TControlPointsCurve ------------------------------------------------ }

procedure TControlPointsCurve.RenderControlPoints;
var
  i: Integer;
begin
  {$ifndef OpenGLES} //TODO-es
  glColorv(ControlPointsColor);
  glBegin(GL_POINTS);
  for i := 0 to ControlPoints.Count-1 do glVertexv(ControlPoints.L[i]);
  glEnd;
  {$endif}
end;

function TControlPointsCurve.BoundingBox: TBox3D;
begin
  Result := FBoundingBox;
end;

procedure TControlPointsCurve.UpdateControlPoints;
begin
  FBoundingBox := CalculateBoundingBox(PVector3Single(ControlPoints.List),
    ControlPoints.Count, 0);
end;

function TControlPointsCurve.CreateConvexHullPoints: TVector3SingleList;
begin
  Result := ControlPoints;
end;

procedure TControlPointsCurve.DestroyConvexHullPoints(Points: TVector3SingleList);
begin
end;

procedure TControlPointsCurve.RenderConvexHull;
var
  CHPoints: TVector3SingleList;
  CH: TIntegerList;
  i: Integer;
begin
  CHPoints := CreateConvexHullPoints;
  try
    CH := ConvexHull(CHPoints);
    try
      {$ifndef OpenGLES} //TODO-es
      glColorv(ConvexHullColor);
      glBegin(GL_POLYGON);
      try
        for i := 0 to CH.Count-1 do
          glVertexv(CHPoints.L[CH[i]]);
      finally glEnd end;
      {$endif}
    finally CH.Free end;
  finally DestroyConvexHullPoints(CHPoints) end;
end;

constructor TControlPointsCurve.Create(AOwner: TComponent);
begin
  inherited;
  ControlPoints := TVector3SingleList.Create;
  { DON'T call UpdateControlPoints from here - UpdateControlPoints is virtual !
    So we set FBoundingBox by hand. }
  FBoundingBox := EmptyBox3D;
  FControlPointsColor := White;
  FConvexHullColor := White;
end;

constructor TControlPointsCurve.CreateDivideCasScriptCurve(
  CasScriptCurve: TCasScriptCurve; ControlPointsCount: Cardinal);
var
  i: Integer;
begin
  Create(nil);
  TBegin := CasScriptCurve.TBegin;
  TEnd := CasScriptCurve.TEnd;
  ControlPoints.Count := ControlPointsCount;
  for i := 0 to ControlPointsCount-1 do
    ControlPoints.L[i] := CasScriptCurve.PointOfSegment(i, ControlPointsCount-1);
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
var
  i, j, k, l: Integer;
begin
  inherited;

  for i := 0 to 2 do
  begin
    Newton[i].Count := ControlPoints.Count;
    for j := 0 to ControlPoints.Count-1 do
      Newton[i].L[j] := ControlPoints.L[j, i];

    { licz kolumny tablicy ilorazow roznicowych in place, overriding Newton[i] }
    for k := 1 to ControlPoints.Count-1 do
      { licz k-ta kolumne }
      for l := ControlPoints.Count-1 downto k do
        { licz l-ty iloraz roznicowy w k-tej kolumnie }
        Newton[i].L[l]:=
          (Newton[i].L[l] - Newton[i].L[l-1]) /
          (ControlPointT(l) - ControlPointT(l-k));
  end;
end;

function TLagrangeInterpolatedCurve.Point(const t: Float): TVector3Single;
var
  i, k: Integer;
  f: Float;
begin
  for i := 0 to 2 do
  begin
    { Oblicz F przy pomocy uogolnionego schematu Hornera z Li(t).
      Wspolczynniki b_k sa w tablicy Newton[i].L[k],
      wartosci t_k sa w ControlPointT(k). }
    F := Newton[i].L[ControlPoints.Count-1];
    for k := ControlPoints.Count-2 downto 0 do
      F := F*(t-ControlPointT(k)) + Newton[i].L[k];
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

constructor TLagrangeInterpolatedCurve.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited;
  for i := 0 to 2 do Newton[i] := TFloatList.Create;
end;

destructor TLagrangeInterpolatedCurve.Destroy;
var
  i: Integer;
begin
  for i := 0 to 2 do FreeAndNil(Newton[i]);
  inherited;
end;

{ TNaturalCubicSpline -------------------------------------------------------- }

constructor TNaturalCubicSpline.Create(X, Y: TFloatList;
  AOwnsX, AOwnsY, APeriodic: boolean);

{ Based on SLE (== Stanislaw Lewanowicz) notes on ii.uni.wroc.pl lecture. }

var
  { n = X.High. Integer, not Cardinal, to avoid some overflows in n-2. }
  n: Integer;

  { [Not]PeriodicDK licza wspolczynnik d_k. k jest na pewno w [1; n-1] }
  function PeriodicDK(k: Integer): Float;
  var
    h_k: Float;
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
  var
    h_n: Float;
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

var
  u, q, s, t, v: TFloatList;
  hk, dk, pk, delta_k, delta_n, h_n, h_n1: Float;
  k: Integer;
begin
  inherited Create;
  Assert(X.Count = Y.Count);
  FMinX := X.First;
  FMaxX := X.Last;
  FOwnsX := AOwnsX;
  FOwnsY := AOwnsY;
  FX := X;
  FY := Y;
  FPeriodic := APeriodic;

  { prepare to calculate M }
  n := X.Count - 1;
  M := TFloatList.Create;
  M.Count := n+1;

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
    u := TFloatList.Create; U.Count := N;
    q := TFloatList.Create; Q.Count := N;
    if Periodic then begin s := TFloatList.Create; S.Count := N; end;

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
        t := TFloatList.Create; T.Count := N + 1;
        v := TFloatList.Create; V.Count := N + 1;

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

var
  k, KMin, KMax, KMiddle: Cardinal;
  hk: Float;
begin
  ClampVar(x, MinX, MaxX);

  { calculate k: W ktorym przedziale x[k-1]..x[k] jest argument ?
    TODO: nalezoloby pomyslec o wykorzystaniu faktu
    ze czesto wiadomo iz wezly x[i] sa rownoodlegle. }
  KMin := 1;
  KMax := FX.Count - 1;
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
var
  i, j: Integer;
  SplineX, SplineY: TFloatList;
begin
  inherited;

  { calculate SplineX.
    Spline[0] and Spline[1] and Spline[2] will share the same reference to X.
    Only Spline[2] will own SplineX. (Spline[2] will be always Freed as the
    last one, so it's safest to set OwnsX in Spline[2]) }
  SplineX := TFloatList.Create;
  SplineX.Count := ControlPoints.Count;
  for i := 0 to ControlPoints.Count-1 do SplineX[i] := ControlPointT(i);

  for i := 0 to 2 do
  begin
    FreeAndNil(Spline[i]);

    { calculate SplineY }
    SplineY := TFloatList.Create;
    SplineY.Count := ControlPoints.Count;
    for j := 0 to ControlPoints.Count-1 do SplineY[j] := ControlPoints.L[j, i];

    Spline[i] := TNaturalCubicSpline.Create(SplineX, SplineY, i = 2, true, Closed);
  end;
end;

function TNaturalCubicSplineCurve_Abstract.Point(const t: Float): TVector3Single;
var
  i: Integer;
begin
  for i := 0 to 2 do Result[i] := Spline[i].Evaluate(t);
end;

constructor TNaturalCubicSplineCurve_Abstract.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TNaturalCubicSplineCurve_Abstract.Destroy;
var
  i: Integer;
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
  Result := VectorsEqual(ControlPoints.First,
                         ControlPoints.Last);
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

{ TRationalBezierCurve ----------------------------------------------- }

function TRationalBezierCurve.Point(const t: Float): TVector3Single;
var
  u: Float;
  W: TVector3SingleList;
  Wgh: TFloatList;
  i, k, n, j: Integer;
begin
  { u := t normalized to [0; 1] }
  u := (t - TBegin) / (TEnd - TBegin);

  { Initializes W and Wgh (0-th step of de Casteljau algorithm).
    It uses ControlPoints, Weights. }
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

    for k := 1 to n do
    begin
      { This caculates in W and Wgh k-th step of de Casteljau algorithm.
        This assumes that W and Wgh already contain (k-1)-th step.
        Uses u as the target point position (in [0; 1]) }
      for i := 0 to n - k do
      begin
        for j := 0 to 2 do
          W.L[i][j]:=(1-u) * Wgh[i  ] * W.L[i  ][j] +
                             u * Wgh[i+1] * W.L[i+1][j];
        Wgh.L[i]:=(1-u) * Wgh[i] + u * Wgh[i+1];
        for j := 0 to 2 do
          W.L[i][j] /= Wgh.L[i];
      end;
    end;

    Result := W.L[0];
  finally
    Wgh.Free;
    W.Free;
  end;
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

constructor TRationalBezierCurve.Create(AOwner: TComponent);
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

{ TPiecewiseCubicBezier --------------------------------------------------- }

function TPiecewiseCubicBezier.CreateConvexHullPoints: TVector3SingleList;
begin
  Result := ConvexHullPoints;
end;

procedure TPiecewiseCubicBezier.DestroyConvexHullPoints(Points: TVector3SingleList);
begin
end;

function TPiecewiseCubicBezier.Point(const t: Float): TVector3Single;
var
  T01: Single;
  TInsidePiece: Double;
  IndexBefore: Int64;
  IndexBeforeChange: Integer;
begin
  Assert(ControlPoints.Count >= 1);
  if ControlPoints.Count = 1 then
    Exit(ControlPoints.Items[0]);

  T01 := MapRange(T, TBegin, TEnd, 0, 1);
  if ControlPoints.Count = 2 then
    // super-fast case
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

  // writeln('TPiecewiseCubicBezier got ', IndexBefore, ' ', TInsidePiece:1:2);

  Result := CubicBezier3D(TInsidePiece, BezierCurves[IndexBefore]);
end;

class function TPiecewiseCubicBezier.NiceClassName: string;
begin
  Result := 'Cubic B-Spline (piecewise C2-Smooth Cubic Bezier)';
end;

procedure TPiecewiseCubicBezier.UpdateControlPoints;

  procedure UpdateBezierCurves;
  var
    S: TVector3SingleList;
    C: TVector3SingleList;
    I: Integer;
    PointBegin, PointEnd: TVector3Single;
  begin
    { Normal calculations cannot be done when
      ControlPoints.Count = 2:
      C.Count would be 1, S.Count would be 2,
      S[0] would be calculated based on C[0] and S[1],
      S[1] would be calculated based on C[0] and S[0].
      So we can't calculate S[0] and S[1] using given equations when
      ControlPoints.Count = 2.

      Point() method implements a special case for ControlPoints.Count = 2,
      it just does Lerp then. }
    if ControlPoints.Count <= 2 then
      Exit;

    { based on SLE mmgk notes, "Krzywe Beziera" page 4 }
    C := nil;
    S := nil;
    try
      C := TVector3SingleList.Create;
      C.Count := ControlPoints.Count - 1;
      { calculate C values }
      for I := 0 to C.Count - 1 do
        C[I] := ControlPoints[I + 1] - ControlPoints[I];

      S := TVector3SingleList.Create;
      S.Count := ControlPoints.Count;
      { calculate S values }
      for I := 1 to S.Count - 2 do
        S[I] := (C[I-1] + C[I]) / 2;
      S[0        ] := C[0        ] * 2 - S[1        ];
      S[S.Count-1] := C[S.Count-2] * 2 - S[S.Count-2];

      SetLength(BezierCurves, ControlPoints.Count - 1);

      for I := 1 to ControlPoints.Count - 1 do
      begin
        PointBegin := ControlPoints.L[I - 1];
        PointEnd   := ControlPoints.L[I];
        BezierCurves[I - 1][0] := PointBegin;
        BezierCurves[I - 1][1] := PointBegin + S[I -1] / 3;
        BezierCurves[I - 1][2] := PointEnd   - S[I   ] / 3;
        BezierCurves[I - 1][3] := PointEnd;
      end;
    finally
      C.Free;
      S.Free;
    end;
  end;

  procedure UpdateConvexHullPoints;
  var
    I: Integer;
  begin
    ConvexHullPoints.Clear;
    ConvexHullPoints.AddList(ControlPoints);
    for I := 0 to Length(BezierCurves) - 1 do
    begin
      { add also intermediate control points }
      ConvexHullPoints.Add(BezierCurves[I][1]);
      ConvexHullPoints.Add(BezierCurves[I][2]);
    end;
  end;

begin
  inherited;
  UpdateBezierCurves;
  UpdateConvexHullPoints;
end;

constructor TPiecewiseCubicBezier.Create(AOwner: TComponent);
begin
  inherited;
  ConvexHullPoints := TVector3SingleList.Create;
end;

destructor TPiecewiseCubicBezier.Destroy;
begin
  FreeAndNil(ConvexHullPoints);
  inherited;
end;

{ global routines ------------------------------------------------------------ }

function CubicBezier1D(T: Single; const Points: TVector4Single): Single;
var
  T1: Single;
begin
  T := Clamped(T, 0, 1);
  T1 := 1 - T;
  Result := Points[0] *     Sqr(T1) * T1 +
            Points[1] * 3 * Sqr(T1) * T +
            Points[2] * 3 * Sqr(T) * T1 +
            Points[3] *     Sqr(T) * T;
end;

function CubicBezier2D(T: Single; const Points: TCubicBezier2DPoints): TVector2Single;
var
  T1: Single;
begin
  T := Clamped(T, 0, 1);
  T1 := 1 - T;
  Result := Points[0] * (    Sqr(T1) * T1) +
            Points[1] * (3 * Sqr(T1) * T) +
            Points[2] * (3 * Sqr(T) * T1) +
            Points[3] * (    Sqr(T) * T);
end;

function CubicBezier3D(T: Single; const Points: TCubicBezier3DPoints): TVector3Single;
var
  T1: Single;
begin
  T := Clamped(T, 0, 1);
  T1 := 1 - T;
  Result := Points[0] * (    Sqr(T1) * T1) +
            Points[1] * (3 * Sqr(T1) * T) +
            Points[2] * (3 * Sqr(T) * T1) +
            Points[3] * (    Sqr(T) * T);
end;

type
  { Calculate curve segment value, knowing that X is between
    Arguments[IndexOfRightValue - 1] and
    Arguments[IndexOfRightValue] and that count > 1 and IndexOfRightValue > 0.
    XInSegment is X already transformed from
    Arguments[IndexOfRightValue - 1] and
    Arguments[IndexOfRightValue] to the [0..1] range.
    IOW, this is the curve-specific equation, with all boring special cases
    eliminated. }
  TCurveSegmentFunction = function (const IndexOfRightValue: Integer;
    const XInSegment: Single): Single is nested;

{ General spline calculation, using SegmentFunction for a curve-specific equation. }
function CalculateSpline(const X: Single; const Loop: boolean;
  const Arguments, Values: TSingleList;
  const SegmentFunction: TCurveSegmentFunction): Single;

  { Calculate assuming that X is between [First..Last], and Count > 1. }
  function CalculateInRange(const X: Single): Single;
  var
    I, C: Integer;
  begin
    C := Arguments.Count;

    // TODO: make binary search
    I := 1;
    while (I + 1 < C) and (X > Arguments.L[I]) do Inc(I);

    Result := SegmentFunction(I,
      (X - Arguments.L[I - 1]) / (Arguments.L[I] - Arguments.L[I - 1]));
  end;

var
  C: Integer;
  FirstArg, LastArg, Len: Single;
begin
  C := Arguments.Count;

  if C = 0 then
    Result := 0 else
  begin
    FirstArg := Arguments.L[0];
    if C = 1 then
      Result := FirstArg else
    begin
      LastArg := Arguments.L[C - 1];
      Len := LastArg - FirstArg;
      if X < FirstArg then
      begin
        if Loop then
          Result := CalculateInRange(X + Ceil((FirstArg - X) / Len) * Len) else
          Result := Values.L[0];
      end else
      if X > LastArg then
      begin
        if Loop then
          Result := CalculateInRange(X - Ceil((X - LastArg) / Len) * Len) else
          Result := Values.L[C - 1];
      end else
        Result := CalculateInRange(X);
    end;
  end;
end;

function CatmullRom(const V0, V1, V2, V3, X: Single): Single;
var
  X2, X3: Single;
begin
  X2 := Sqr(X);
  X3 := X2 * X;
  Result := 0.5 * (
    (2 * V1) +
    (-V0 + V2) * X +
    (2*V0 - 5*V1 + 4*V2 - V3) * X2 +
    (-V0 + 3*V1- 3*V2 + V3) * X3
  );
end;

function CatmullRomSpline(const X: Single; const Loop: boolean;
  const Arguments: TSingleList;
  const Values: TSingleList): Single;

  function CatmullRomSegment(const I: Integer; const XInSegment: Single): Single;
  var
    C: Integer;
    V0, V1, V2, V3: Single;
  begin
    C := Arguments.Count;

    V1 := Values.L[I - 1];
    V2 := Values.L[I];

    if I - 2 = -1 then
    begin
      if Loop then
        V0 := Values.L[C - 2] else // not Values.L[C - 1], as first and last values are usually equal
        V0 := Values.L[0];
    end else
      V0 := Values.L[I - 2];

    if I + 1 = C then
    begin
      if Loop then
        V3 := Values.L[1] else // not Values.L[C - 1], as first and last values are usually equal
        V3 := Values.L[C - 1];
    end else
      V3 := Values.L[I + 1];

    Result := CatmullRom(V0, V1, V2, V3, XInSegment);
  end;

begin
  if Arguments.Count <> Values.Count then
    raise Exception.Create('CatmullRomSpline: Arguments and Values lists must have equal count');
  Result := CalculateSpline(X, Loop, Arguments, Values, @CatmullRomSegment);
end;

function Hermite(const V0, V1, Tangent0, Tangent1, X: Single): Single;
var
  X2, X3: Single;
begin
  X2 := Sqr(X);
  X3 := X2 * X;
  { equation from https://en.wikipedia.org/wiki/Cubic_Hermite_spline }
  Result :=
    (2 * X3 - 3 * X2 + 1) * V0 +
    (X3 - 2 * X2 + X) * Tangent0 +
    (-2 * X3 + 3 *X2) * V1 +
    (X3 - X2) * Tangent1;
end;

function HermiteSpline(const X: Single; const Loop: boolean;
  const Arguments, Values, Tangents: TSingleList): Single;

  function HermiteSegment(const I: Integer; const XInSegment: Single): Single;
  begin
    Result := Hermite(
      Values  .L[I - 1], Values  .L[I],
      Tangents.L[I - 1], Tangents.L[I], XInSegment);
  end;

begin
  if (Arguments.Count <> Values.Count) or
     (Arguments.Count <> Tangents.Count) then
    raise Exception.Create('HermiteSpline: Arguments and Values and Tangents lists must have equal count');
  Result := CalculateSpline(X, Loop, Arguments, Values, @HermiteSegment);
end;

function HermiteTense(const V0, V1, X: Single): Single;
var
  X2, X3: Single;
begin
  X2 := Sqr(X);
  X3 := X2 * X;
  Result :=
    (2 * X3 - 3 * X2 + 1) * V0 +
    (-2 * X3 + 3 *X2) * V1;
end;

function HermiteTenseSpline(const X: Single; const Loop: boolean;
  const Arguments, Values: TSingleList): Single;

  function HermiteTenseSegment(const I: Integer; const XInSegment: Single): Single;
  begin
    Result := HermiteTense(
      Values.L[I - 1], Values.L[I], XInSegment);
  end;

begin
  if Arguments.Count <> Values.Count then
    raise Exception.Create('HermiteTenseSpline: Arguments and Values lists must have equal count');
  Result := CalculateSpline(X, Loop, Arguments, Values, @HermiteTenseSegment);
end;

end.
