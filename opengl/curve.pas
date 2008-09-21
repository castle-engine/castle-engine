{
  Copyright 2004-2005 Michalis Kamburelis.

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
}

{ @abstract(3d curve class TCurve and many basic descendants.) }

unit Curve;

interface

uses VectorMath, Boxes3d, KambiUtils, KambiScript, Polynomials,
  KambiClassUtils;

{$define read_interface}

type
  { "Curve" is, in the sense of this class, some 3d object that
    @unorderedList(
      @itemSpacing compact
      @item can be seen as a set of points Point(t) for t in [TBegin, TEnd]
      @item more or less "fits" inside his BoundingBox
    ) }
  TCurve = class
  private
    FTBegin, FTEnd: Float;
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

    { Curve should fit inside this BoundingBox.
      For now, this does not have to be a "perfect fit", it may be smaller than
      it should be and it may be larger than it could be.
      This should be treated as something like a "good hint".
      (Maybe at some time I'll make this conditions more rigorous) }
    function BoundingBox: TBox3d; virtual; abstract;

    constructor Create(const ATBegin, ATEnd: Float);
  end;

  TObjectsListItem_2 = TCurve;
  {$I objectslist_2.inc}
  TCurvesList = TObjectsList_2;

  { @abstract(This is a curve defined by explicitly giving functions for
    Point(t) = x(t), y(t), z(t) as KambiScript expressions.) }
  TKamScriptCurve = class(TCurve)
  protected
    FTVariable: TKamScriptFloat;
    FXFunction, FYFunction, FZFunction: TKamScriptExpression;
    FBoundingBox: TBox3d;
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
    function BoundingBox: TBox3d; override;

    { XFunction, YFunction, ZFunction references are copied here,
      and will be freed in destructor (so don't Free them yourself). }
    constructor Create(const ATBegin, ATEnd: Float;
      AXFunction, AYFunction, AZFunction: TKamScriptExpression;
      ASegmentsForBoundingBox: Cardinal); overload;
    constructor Create(const ATBegin, ATEnd: Float;
      AXFunction, AYFunction, AZFunction: TKamScriptExpression
      { ASegmentsForBoundingBox = 100 } ); overload;
    { }
    destructor Destroy; override;
  end;

  { @abstract(This is a basic abstract class for curves determined my some
    set of ControlPoints.)
    Note: it is @italic(not) defined in this class any correspondence between
    values of T (argument for Point function) and ControlPoints. }
  TControlPointsCurve = class(TCurve)
  private
    FBoundingBox: TBox3d;
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
    function BoundingBox: TBox3d; override;

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
      It's also useful in many places in curves.pasprogram. }
    constructor Create(const ATBegin, ATEnd: Float); virtual;

    { Calculates ControlPoints taking Point(i, ControlPointsCount-1)
      for i in [0 .. ControlPointsCount-1] from KamScriptCurve.
      TBegin and TEnd are copied from KamScriptCurve. }
    constructor CreateDivideKamScriptCurve(KamScriptCurve: TKamScriptCurve;
      ControlPointsCount: Cardinal);

    destructor Destroy; override;
  end;

  TControlPointsCurveClass = class of TControlPointsCurve;

  TObjectsListItem_1 = TControlPointsCurve;
  {$I objectslist_1.inc}
  TControlPointsCurvesList = TObjectsList_1;

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

  { This is a curve defined as [Sx(t), Sy(t), Sz(t)] where
    S?(t) are natural cubic splines. Interface works the same as for
    TLagrangeInterpolatedCurve, only this time interpolation is done
    differently.

    Whether it's Closed depends on the value returned by Closed
    (it may depend on ControlPoints, i.e. it will be reevaluated in
    UpdateControlPoints). }
  TNaturalCubicSplineCurve_Abstract = class(TInterpolatedCurve)
  protected
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

  { @abstract(Same as TNaturalCubicSplineCurve_Abstract, only it's
    Closed when ControlPoints first and last are the same
    (i.e. VectorsEqual(ControlPoints[0], ControlPoints[ControlPoints.Count-1])).)

    This is the most sensible non-abstract implementation of
    NaturalCubicSplineCurve. }
  TNaturalCubicSplineCurve = class(TNaturalCubicSplineCurve_Abstract)
  protected
    function Closed: boolean; override;
  public
    class function NiceClassName: string; override;
  end;

  { TNaturalCubicSplineCurve_Abstract that is always Closed. }
  TNaturalCubicSplineCurveAlwaysClosed = class(TNaturalCubicSplineCurve_Abstract)
  protected
    function Closed: boolean; override;
  public
    class function NiceClassName: string; override;
  end;

  { TNaturalCubicSplineCurve_Abstract that is never Closed. }
  TNaturalCubicSplineCurveNeverClosed = class(TNaturalCubicSplineCurve_Abstract)
  protected
    function Closed: boolean; override;
  public
    class function NiceClassName: string; override;
  end;

{$undef read_interface}

implementation

uses SysUtils, GL, GLU, GLExt, ConvexHullUnit, KambiGLUtils;

{$define read_implementation}
{$I objectslist_1.inc}
{$I objectslist_2.inc}

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

constructor TCurve.Create(const ATBegin, ATEnd: Float);
begin
 inherited Create;
 FTBegin := ATBegin;
 FTEnd := ATEnd;
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

function TKamScriptCurve.BoundingBox: TBox3d;
begin
 Result := FBoundingBox;
end;

constructor TKamScriptCurve.Create(const ATBegin, ATEnd: Float;
  AXFunction, AYFunction, AZFunction: TKamScriptExpression;
  ASegmentsForBoundingBox: Cardinal);
var i, k: Integer;
    P: TVector3Single;
begin
 inherited Create(ATBegin, ATEnd);
 FXFunction := AXFunction;
 FYFunction := AYFunction;
 FZFunction := AZFunction;

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

constructor TKamScriptCurve.Create(const ATBegin, ATEnd: Float;
  AXFunction, AYFunction, AZFunction: TKamScriptExpression);
begin
 Create(ATBegin, ATEnd, AXFunction, AYFunction, AZFunction, 100);
end;

destructor TKamScriptCurve.Destroy;
begin
 FreeAndNil(FXFunction);
 FreeAndNil(FYFunction);
 FreeAndNil(FZFunction);
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

function TControlPointsCurve.BoundingBox: TBox3d;
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
 FBoundingBox := EmptyBox3d;
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
  Newton[i].SetLength(ControlPoints.Count);
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

{ TNaturalCubicSplineCurve_Abstract ------------------------------------------- }

procedure TNaturalCubicSplineCurve_Abstract.UpdateControlPoints;
var i, j: Integer;
    SplineX, SplineY: TDynFloatArray;
begin
 inherited;

 { evaluate SplineX.
   Spline[0] and Spline[1] and Spline[2] will share the same reference to X.
   Only Spline[2] will own SplineX. (Spline[2] will be always Freed as the
   last one, so it's safest to set OwnsX in Spline[2]) }
 SplineX := TDynFloatArray.Create(ControlPoints.Count);
 for i := 0 to ControlPoints.Count-1 do SplineX[i] := ControlPointT(i);

 for i := 0 to 2 do
 begin
  FreeAndNil(Spline[i]);

  { evaluate SplineY }
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