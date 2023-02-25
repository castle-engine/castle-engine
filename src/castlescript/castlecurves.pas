{
  Copyright 2004-2023 Michalis Kamburelis.

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

interface

uses SysUtils, Classes, Generics.Collections, DOM,
  CastleVectors, CastleBoxes, CastleUtils, CastleScript,
  CastleClassUtils, CastleFrustum, X3DNodes;

type
  ECurveFileInvalid = class(Exception);

  { 3D curve, a set of points defined by a continuous function @link(Point)
    for arguments within [TBegin, TEnd]. }
  TCurve = class
  private
    FTBegin, FTEnd: Single;
  protected
    procedure LoadFromElement(const E: TDOMElement); virtual;
    procedure SaveToStream(const Stream: TStream); virtual;
  public
    const
      DefaultSegments = 32;

    constructor Create;

    { The valid range of curve function argument. Must be TBegin <= TEnd.
      @groupBegin }
    property TBegin: Single read FTBegin write FTBegin {$ifdef FPC}default 0{$endif};
    property TEnd: Single read FTEnd write FTEnd {$ifdef FPC}default 1{$endif};
    { @groupEnd }

    { Curve function, for each parameter value determine the 3D point.
      This determines the actual shape of the curve.
      This is the simplest approach to calculate points on a curve. }
    function Point(const t: Float): TVector3; virtual; abstract;
    function Point2D(const t: Float): TVector2;

    { Curve function to work with rendered line segments begin/end points.
      This is simply a more specialized version of @link(Point),
      it scales the argument such that you get Point(TBegin) for I = 0
      and you get Point(TEnd) for I = Segments. }
    function PointOfSegment(const i, Segments: Cardinal): TVector3;

    { Load the first curve defined in given XML file.
      Hint: use https://castle-engine.io/curves_tool to design curves
      visually. }
    class function LoadFromFile(const URL: string): TCurve;

    function BoundingBox: TBox3D; virtual; abstract;

    { Represent this curve as an X3D geometry node,
      that you can use to visualize this. }
    function GeometryNode(const Segments: Cardinal = DefaultSegments): TAbstractGeometryNode;
  end;

  TCurveList = class({$ifdef FPC}specialize{$endif} TObjectList<TCurve>)
  public
    { Load curves definitions from a simple XML file.
      Hint: use https://castle-engine.io/curves_tool to design curves
      visually. }
    procedure LoadFromFile(const URL: string);

    { Save curve definitions to a simple XML file.
      Hint: use https://castle-engine.io/curves_tool to design curves
      visually. }
    procedure SaveToFile(const URL: string);
  end;

  { Curve defined by explicitly giving functions for
    Point(t) = x(t), y(t), z(t) as CastleScript expressions. }
  TCasScriptCurve = class(TCurve)
  private
    FSegmentsForBoundingBox: Cardinal;
    FTVariable: TCasScriptFloat;
    FFunction: array [0..2] of TCasScriptExpression;
    FBoundingBox: TBox3D;
    procedure SetSegmentsForBoundingBox(AValue: Cardinal);
    procedure SetTVariable(AValue: TCasScriptFloat);
    function GetFunction(const Index: Integer): TCasScriptExpression;
    procedure SetFunction(const Index: Integer; const Value: TCasScriptExpression);
    procedure UpdateBoundingBox;
  protected
    procedure LoadFromElement(const E: TDOMElement); override;
    procedure SaveToStream(const Stream: TStream); override;
  public
    function Point(const t: Float): TVector3; override;

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

    constructor Create;

    destructor Destroy; override;
  end;

  { A basic abstract class for curves determined my some set of ControlPoints.
    Note: it is @italic(not) defined in this class any correspondence between
    values of T (argument for Point function) and ControlPoints. }
  TControlPointsCurve = class(TCurve)
  strict private
    FBoundingBox: TBox3D;
  strict protected
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
    function CreateConvexHullPoints: TVector3List; virtual;
    procedure DestroyConvexHullPoints(Points: TVector3List); virtual;
  protected
    procedure LoadFromElement(const E: TDOMElement); override;
    procedure SaveToStream(const Stream: TStream); override;
  public
    ControlPoints: TVector3List;

    { Bounding box of the curve.
      In this class, it is simply a BoundingBox of ControlPoints. }
    function BoundingBox: TBox3D; override;

    { Always after changing ControlPoints or TBegin or TEnd and before calling
      @link(Point) (or anything that uses @link(Point), like @link(BoundingBox))
      call this method. It recalculates necessary things.
      ControlPoints.Count must be >= 2.

      When overriding: always call inherited first. }
    procedure UpdateControlPoints; virtual;

    { Constructor. }
    constructor Create;

    { Calculate initial control points by sampling given TCasScriptCurve,
      with analytical curve equation.
      TBegin and TEnd are copied from CasScriptCurve. }
    constructor CreateFromEquation(CasScriptCurve: TCasScriptCurve;
      ControlPointsCount: Cardinal);

    destructor Destroy; override;

    { Calculate the convex hull. Caller is responsible for freeing the result. }
    function ConvexHull: TVector3List;
  end;

  TControlPointsCurveClass = class of TControlPointsCurve;

  TControlPointsCurveList = {$ifdef FPC}specialize{$endif} TObjectList<TControlPointsCurve>;

  TCubicBezier2DPoints = array [0..3] of TVector2;
  TCubicBezier3DPoints = array [0..3] of TVector3;
  TCubicBezier3DPointsArray = array of TCubicBezier3DPoints;
  { Piecewise (composite) cubic Bezier curve.
    Each segment (ControlPoints[i]..ControlPoints[i+1])
    is a cubic Bezier curve (Bezier with 4 control points,
    2 points in the middle are auto-calculated for max smoothness).

    This is a cubic B-spline. Which is equivalent to C2 continuous
    composite Bézier curves. See
    https://en.wikipedia.org/wiki/Spline_%28mathematics%29 .
    Aka Cubic B-Spline (piecewise C2-Smooth Cubic Bezier).

    ControlPoints.Count may be 1 (in general,
    for TControlPointsCurve, it must be >= 2).
  }
  TPiecewiseCubicBezier = class(TControlPointsCurve)
  strict private
    BezierCurves: TCubicBezier3DPointsArray;
    ConvexHullPoints: TVector3List;
    FBoundingBox: TBox3D;
  strict protected
    function CreateConvexHullPoints: TVector3List; override;
    procedure DestroyConvexHullPoints(Points: TVector3List); override;
    { Calculating additional points. }
    procedure UpdateBezierCurves(var ABezierCurves: TCubicBezier3DPointsArray); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure UpdateControlPoints; override;
    function Point(const t: Float): TVector3; override;
    function BoundingBox: TBox3D; override;
  end;

{ Cubic (4 control points) Bezier curve (with all weights equal) in 1D. }
function CubicBezier1D(T: Single; const Points: TVector4): Single;

{ Cubic (4 control points) Bezier curve (with all weights equal) in 2D. }
function CubicBezier2D(T: Single; const Points: TCubicBezier2DPoints): TVector2;

{ Cubic (4 control points) Bezier curve (with all weights equal) in 3D. }
function CubicBezier3D(T: Single; const Points: TCubicBezier3DPoints): TVector3;

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

uses Math,
  CastleXMLUtils, CastleDownload;

function ConvexHullIndexes(Points: TVector3List): TIntegerList; forward;

{ TCurve ------------------------------------------------------------ }

function TCurve.PointOfSegment(const i, Segments: Cardinal): TVector3;
begin
  Result := Point(TBegin + (i/Segments) * (TEnd-TBegin));
end;

constructor TCurve.Create;
begin
  inherited;
  FTBegin := 0;
  FTEnd := 1;
end;

procedure TCurve.LoadFromElement(const E: TDOMElement);
var
  ETime: TDOMElement;
begin
  ETime := E.ChildElement('time');
  TBegin := ETime.AttributeSingle('begin');
  TEnd := ETime.AttributeSingle('end');
end;

procedure TCurve.SaveToStream(const Stream: TStream);
begin
  WritelnStr(Stream, Format('    <time begin="%f" end="%f" />', [TBegin, TEnd]));
end;

function TCurve.Point2D(const T: Float): TVector2;
var
  V: TVector3;
begin
  V := Point(T);
  Result.X := V.X;
  Result.Y := V.Y;
end;

class function TCurve.LoadFromFile(const URL: string): TCurve;
var
  List: TCurveList;
begin
  List := TCurveList.Create(true);
  try
    List.LoadFromFile(URL);
    if List.Count = 0 then
      raise ECurveFileInvalid.Create('Empty curve XML file, cannot get first curve');
    Result := List.Extract(List.First);
  finally FreeAndNil(List) end;
end;

function TCurve.GeometryNode(const Segments: Cardinal = DefaultSegments): TAbstractGeometryNode;
var
  Coord: TCoordinateNode;
  LineSet: TLineSetNode;
  I: Integer;
begin
  Coord := TCoordinateNode.Create;
  Coord.FdPoint.Items.Clear;
  for I := 0 to Segments do
    Coord.FdPoint.Items.Add(Point(I / Segments));

  LineSet := TLineSetNode.Create;
  LineSet.SetVertexCount([Segments + 1]);
  LineSet.Coord := Coord;

  Result := LineSet;
end;

{ TCurveList ---------------------------------------------------- }

procedure TCurveList.LoadFromFile(const URL: string);
var
  Document: TXMLDocument;
  I: TXMLElementIterator;
  CurveTypeStr: string;
  Curve: TCurve;
begin
  Clear;

  Document := URLReadXML(URL);
  try
    Check(Document.DocumentElement.TagName = 'curves',
      'Root node of curves file must be <curves>');

    I := Document.DocumentElement.ChildrenIterator('curve');
    try
      while I.GetNext do
      begin
        CurveTypeStr := I.Current.AttributeString('type');
        if SameText(CurveTypeStr, TPiecewiseCubicBezier.ClassName) then
          Curve := TPiecewiseCubicBezier.Create else
        if SameText(CurveTypeStr, TCasScriptCurve.ClassName) then
          Curve := TCasScriptCurve.Create else
          raise ECurveFileInvalid.CreateFmt('Curve type "%s" unknown', [CurveTypeStr]);
        Curve.LoadFromElement(I.Current);
        if Curve is TControlPointsCurve then
          TControlPointsCurve(Curve).UpdateControlPoints;
        Add(Curve);
      end;
    finally FreeAndNil(I); end;
  finally FreeAndNil(Document) end;
end;

procedure TCurveList.SaveToFile(const URL: string);
var
  Stream: TStream;
  I: Integer;
begin
  Stream := URLSaveStream(URL);
  try
    WritelnStr(Stream, '<?xml version="1.0"?>');
    WritelnStr(Stream, '<curves>');
    for I := 0 to Count - 1 do
    begin
      WritelnStr(Stream, '  <curve type="' + Items[I].ClassName + '">');
      Items[I].SaveToStream(Stream);
      WritelnStr(Stream, '  </curve>');
    end;
    WritelnStr(Stream, '</curves>');
  finally FreeAndNil(Stream) end;
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
  P: TVector3;
begin
  if (XFunction = nil) or
     (YFunction = nil) or
     (ZFunction = nil) or
     (TVariable = nil) then
    FBoundingBox := TBox3D.Empty else
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
        FBoundingBox.Data[0].InternalData[k] := Min(FBoundingBox.Data[0].InternalData[k], P[k]);
        FBoundingBox.Data[1].InternalData[k] := Max(FBoundingBox.Data[1].InternalData[k], P[k]);
      end;
    end;
  end;
end;

function TCasScriptCurve.Point(const t: Float): TVector3;
var
  I: Integer;
begin
  TVariable.Value := T;
  for I := 0 to 2 do
    Result.InternalData[I] := (FFunction[I].Execute as TCasScriptFloat).Value;

  {test: Writeln('Point at t = ',FloatToNiceStr(Single(t)), ' is (',
    Result.ToString, ')');}
end;

function TCasScriptCurve.BoundingBox: TBox3D;
begin
  Result := FBoundingBox;
end;

constructor TCasScriptCurve.Create;
begin
  inherited;
  FSegmentsForBoundingBox := 100;
  FBoundingBox := TBox3D.Empty;
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

procedure TCasScriptCurve.LoadFromElement(const E: TDOMElement);
begin
  inherited LoadFromElement(E);
  // TODO: load TCasScriptCurve specifics
end;

procedure TCasScriptCurve.SaveToStream(const Stream: TStream);
begin
  inherited SaveToStream(Stream);
  // TODO: save TCasScriptCurve specifics
end;

{ TControlPointsCurve ------------------------------------------------ }

function TControlPointsCurve.BoundingBox: TBox3D;
begin
  Result := FBoundingBox;
end;

procedure TControlPointsCurve.UpdateControlPoints;
begin
  FBoundingBox := CalculateBoundingBox(PVector3(ControlPoints.L),
    ControlPoints.Count, 0);
end;

function TControlPointsCurve.CreateConvexHullPoints: TVector3List;
begin
  Result := ControlPoints;
end;

procedure TControlPointsCurve.DestroyConvexHullPoints(Points: TVector3List);
begin
end;

function TControlPointsCurve.ConvexHull: TVector3List;
var
  PotentialConvexHullPoints: TVector3List;
  Indexes: TIntegerList;
  I: Integer;
begin
  Result := TVector3List.Create;

  PotentialConvexHullPoints := CreateConvexHullPoints;
  try
    if PotentialConvexHullPoints.Count <> 0 then
    begin
      Indexes := ConvexHullIndexes(PotentialConvexHullPoints);
      try
        for I := 0 to Indexes.Count - 1 do
          Result.Add(PotentialConvexHullPoints.List^[Indexes.List^[I]]);
      finally FreeAndNil(Indexes) end;
    end;
  finally DestroyConvexHullPoints(PotentialConvexHullPoints) end;
end;

constructor TControlPointsCurve.Create;
begin
  inherited;
  ControlPoints := TVector3List.Create;
  { DON'T call UpdateControlPoints from here - UpdateControlPoints is virtual !
    So we set FBoundingBox by hand. }
  FBoundingBox := TBox3D.Empty;
end;

constructor TControlPointsCurve.CreateFromEquation(
  CasScriptCurve: TCasScriptCurve; ControlPointsCount: Cardinal);
var
  i: Integer;
begin
  Create;
  TBegin := CasScriptCurve.TBegin;
  TEnd := CasScriptCurve.TEnd;
  ControlPoints.Count := ControlPointsCount;
  for i := 0 to ControlPointsCount-1 do
    ControlPoints.List^[i] := CasScriptCurve.PointOfSegment(i, ControlPointsCount-1);
  UpdateControlPoints;
end;

destructor TControlPointsCurve.Destroy;
begin
  FreeAndNil(ControlPoints);
  inherited;
end;

procedure TControlPointsCurve.LoadFromElement(const E: TDOMElement);
var
  I: TXMLElementIterator;
  EControlPoints: TDOMElement;
begin
  inherited LoadFromElement(E);

  EControlPoints := E.ChildElement('control_points');
  I := EControlPoints.ChildrenIterator('control_point');
  try
    while I.GetNext do
      ControlPoints.Add(I.Current.AttributeVector3('value'));
  finally FreeAndNil(I); end;
end;

procedure TControlPointsCurve.SaveToStream(const Stream: TStream);
var
  I: Integer;
  VectorStr: string;
begin
  inherited SaveToStream(Stream);

  WritelnStr(Stream, '    <control_points>');
  for I := 0 to ControlPoints.Count - 1 do
  begin
    VectorStr := ControlPoints[I].ToRawString;
    WritelnStr(Stream, '      <control_point value="' + VectorStr + '"/>');
  end;
  WritelnStr(Stream, '    </control_points>');
end;

{ TPiecewiseCubicBezier --------------------------------------------------- }

function TPiecewiseCubicBezier.CreateConvexHullPoints: TVector3List;
begin
  Result := ConvexHullPoints;
end;

procedure TPiecewiseCubicBezier.DestroyConvexHullPoints(Points: TVector3List);
begin
end;

function TPiecewiseCubicBezier.Point(const t: Float): TVector3;
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
  TInsidePiece := TInsidePiece * (ControlPoints.Count - 1); // make TInsidePiece in 0..1 range

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
    IndexBefore := IndexBefore + IndexBeforeChange;
    TInsidePiece := TInsidePiece - IndexBeforeChange;
  end;
  Assert(IndexBefore >= 0);
  Assert(IndexBefore <= ControlPoints.Count - 2);

  // writeln('TPiecewiseCubicBezier got ', IndexBefore, ' ', TInsidePiece:1:2);

  if IndexBefore >= Length(BezierCurves) then
    raise Exception.Create('Curves data inside PiecewiseCubicBezier not initialized, probably you forgot to call UpdateControlPoints after changing the ControlPoints');
  Result := CubicBezier3D(TInsidePiece, BezierCurves[IndexBefore]);
end;

procedure TPiecewiseCubicBezier.UpdateBezierCurves(var ABezierCurves: TCubicBezier3DPointsArray);
var
  S: TVector3List;
  C: TVector3List;
  I: Integer;
  PointBegin, PointEnd: TVector3;
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
    C := TVector3List.Create;
    C.Count := ControlPoints.Count - 1;
    { calculate C values }
    for I := 0 to C.Count - 1 do
      C[I] := ControlPoints[I + 1] - ControlPoints[I];

    S := TVector3List.Create;
    S.Count := ControlPoints.Count;
    { calculate S values }
    for I := 1 to S.Count - 2 do
      S[I] := (C[I-1] + C[I]) / 2;
    S[0        ] := C[0        ] * 2 - S[1        ];
    S[S.Count-1] := C[S.Count-2] * 2 - S[S.Count-2];

    SetLength(ABezierCurves, ControlPoints.Count - 1);

    for I := 1 to ControlPoints.Count - 1 do
    begin
      PointBegin := ControlPoints.List^[I - 1];
      PointEnd   := ControlPoints.List^[I];
      ABezierCurves[I - 1][0] := PointBegin;
      ABezierCurves[I - 1][1] := PointBegin + S[I -1] / 3;
      ABezierCurves[I - 1][2] := PointEnd   - S[I   ] / 3;
      ABezierCurves[I - 1][3] := PointEnd;
    end;
  finally
    C.Free;
    S.Free;
  end;
end;

procedure TPiecewiseCubicBezier.UpdateControlPoints;

  procedure UpdateConvexHullPoints;
  var
    I: Integer;
  begin
    ConvexHullPoints.Clear;
    ConvexHullPoints.AddRange(ControlPoints);
    for I := 0 to Length(BezierCurves) - 1 do
    begin
      { add also intermediate control points }
      ConvexHullPoints.Add(BezierCurves[I][1]);
      ConvexHullPoints.Add(BezierCurves[I][2]);
    end;
  end;

  procedure UpdateBoundingBox;
  begin
    FBoundingBox := CalculateBoundingBox(PVector3(ConvexHullPoints.L),
      ConvexHullPoints.Count, 0);
  end;

begin
  inherited;
  UpdateBezierCurves(BezierCurves);
  UpdateConvexHullPoints;
  UpdateBoundingBox;
end;

constructor TPiecewiseCubicBezier.Create;
begin
  inherited;
  ConvexHullPoints := TVector3List.Create;
  FBoundingBox := TBox3D.Empty;
end;

destructor TPiecewiseCubicBezier.Destroy;
begin
  FreeAndNil(ConvexHullPoints);
  inherited;
end;

function TPiecewiseCubicBezier.BoundingBox: TBox3D;
begin
  Result := FBoundingBox;
end;

{ global routines ------------------------------------------------------------ }

function CubicBezier1D(T: Single; const Points: TVector4): Single;
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

function CubicBezier2D(T: Single; const Points: TCubicBezier2DPoints): TVector2;
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

function CubicBezier3D(T: Single; const Points: TCubicBezier3DPoints): TVector3;
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

{ General spline calculation.

  Sets Inside to @true if the point is between 2 arguments on Arguments list,
  and sets IndexOfRightValue, XInSegment in this case.
  It guarantees that X is between
  Arguments[IndexOfRightValue - 1] and
  Arguments[IndexOfRightValue] and that count > 1 and IndexOfRightValue > 0.
  XInSegment is X already transformed from
  Arguments[IndexOfRightValue - 1] and
  Arguments[IndexOfRightValue] to the [0..1] range.
  It is expected the caller will calculate
  CurveResult using IndexOfRightValue, XInSegment, and curve-specific equation.

  Sets Inside to @false if the point is outside of Arguments,
  and sets CurveResult in this case. }
procedure CalculateSpline(const X: Single; const Loop: boolean;
  const Arguments, Values: TSingleList;
  out Inside: Boolean; out CurveResult: Single;
  out IndexOfRightValue: Integer; out XInSegment: Single);

  { Calculate assuming that X is between [First..Last], and Count > 1. }
  procedure CalculateInRange(const X: Single);
  var
    I, C: Integer;
  begin
    Assert(Inside);

    C := Arguments.Count;

    // TODO: make binary search
    I := 1;
    while (I + 1 < C) and (X > Arguments.List^[I]) do Inc(I);

    IndexOfRightValue := I;
    XInSegment := (X - Arguments.List^[I - 1]) / (Arguments.List^[I] - Arguments.List^[I - 1]);
  end;

var
  C: Integer;
  FirstArg, LastArg, Len: Single;
begin
  C := Arguments.Count;

  if C = 0 then
  begin
    Inside := false;
    CurveResult := 0;
  end else
  begin
    FirstArg := Arguments.List^[0];
    if C = 1 then
    begin
      Inside := false;
      CurveResult := FirstArg;
    end else
    begin
      LastArg := Arguments.List^[C - 1];
      Len := LastArg - FirstArg;
      if X < FirstArg then
      begin
        if Loop then
        begin
          Inside := true;
          CalculateInRange(X + Ceil((FirstArg - X) / Len) * Len)
        end else
        begin
          Inside := false;
          CurveResult := Values.List^[0];
        end;
      end else
      if X > LastArg then
      begin
        if Loop then
        begin
          Inside := true;
          CalculateInRange(X - Ceil((X - LastArg) / Len) * Len);
        end else
        begin
          Inside := false;
          CurveResult := Values.List^[C - 1];
        end;
      end else
      begin
        Inside := true;
        CalculateInRange(X);
      end;
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

    V1 := Values.List^[I - 1];
    V2 := Values.List^[I];

    if I - 2 = -1 then
    begin
      if Loop then
        V0 := Values.List^[C - 2] else // not Values.List^[C - 1], as first and last values are usually equal
        V0 := Values.List^[0];
    end else
      V0 := Values.List^[I - 2];

    if I + 1 = C then
    begin
      if Loop then
        V3 := Values.List^[1] else // not Values.List^[C - 1], as first and last values are usually equal
        V3 := Values.List^[C - 1];
    end else
      V3 := Values.List^[I + 1];

    Result := CatmullRom(V0, V1, V2, V3, XInSegment);
  end;

var
  Inside: Boolean;
  IndexOfRightValue: Integer;
  XInSegment: Single;
begin
  if Arguments.Count <> Values.Count then
    raise Exception.Create('CatmullRomSpline: Arguments and Values lists must have equal count');
  CalculateSpline(X, Loop, Arguments, Values, Inside, Result, IndexOfRightValue, XInSegment);
  if Inside then
    Result := CatmullRomSegment(IndexOfRightValue, XInSegment);
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
      Values  .List^[I - 1], Values  .List^[I],
      Tangents.List^[I - 1], Tangents.List^[I], XInSegment);
  end;

var
  Inside: Boolean;
  IndexOfRightValue: Integer;
  XInSegment: Single;
begin
  if (Arguments.Count <> Values.Count) or
     (Arguments.Count <> Tangents.Count) then
    raise Exception.Create('HermiteSpline: Arguments and Values and Tangents lists must have equal count');

  CalculateSpline(X, Loop, Arguments, Values, Inside, Result, IndexOfRightValue, XInSegment);
  if Inside then
    Result := HermiteSegment(IndexOfRightValue, XInSegment);
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
      Values.List^[I - 1], Values.List^[I], XInSegment);
  end;

var
  Inside: Boolean;
  IndexOfRightValue: Integer;
  XInSegment: Single;
begin
  if Arguments.Count <> Values.Count then
    raise Exception.Create('HermiteTenseSpline: Arguments and Values lists must have equal count');
  CalculateSpline(X, Loop, Arguments, Values, Inside, Result, IndexOfRightValue, XInSegment);
  if Inside then
    Result := HermiteTenseSegment(IndexOfRightValue, XInSegment);
end;

{ Calculate the convex hull ignoring Z coordinates of pixels.
  That is, all Points[*][2] are ignored.
  Returns newly created array with the indices to Points.
  If you want to draw an edge of convex hull,
  you want to iterate over these points like (for each i) Points[Result[i]]).

  Points.Count must be >= 1. }
function ConvexHullIndexes(Points: TVector3List): TIntegerList;

{ this is the Jarvis algorithm, based on description in Cormen's
  "Introduction to alg." }

var InResult: TBooleanList;

  function FindNext(Start: Integer; var NextI: Integer; RightSide: boolean): boolean;
  { Starting from Points[Start], knowing that InResult[Start],
    find next vertex on convex hull. If RightSide then we're moving from
    lowest vertex to highest, walking over the right edge of the convex hull.
    Else we're moving from highest to lowest, walking over the left edge
    of hull.

    Return false if RightSide and Start is the highest vertex,
    or (not RightSide) and Start is the lowest vertex.
    Else sets Next as appropriate and returns true.

    Returned Next for SURE has InResult[Next] = false. }
  var MaxCotanAngle, ThisCotan: Single;
      MaxCotanAngleI, i: Integer;
  begin
   MaxCotanAngle := -MaxSingle;
   MaxCotanAngleI := -1;
   for i := 0 to Points.Count-1 do
    if not InResult[i] then
    begin
     if SameValue(Points.List^[i][1], Points.List^[Start][1]) then
     begin
      if RightSide = (Points.List^[i][0] > Points.List^[Start][0]) then
      begin
       MaxCotanAngle := MaxSingle;
       MaxCotanAngleI := i;
      end;
     end else
     if RightSide = (Points.List^[i][1] > Points.List^[Start][1]) then
     begin
      ThisCotan:=(Points.List^[i][0] - Points.List^[Start][0]) /
                 (Points.List^[i][1] - Points.List^[Start][1]);
      if ThisCotan > MaxCotanAngle then
      begin
       MaxCotanAngle := ThisCotan;
       MaxCotanAngleI := i;
      end;
     end;
    end;

   Result := MaxCotanAngleI <> -1;
   if Result then NextI := MaxCotanAngleI;
  end;

  procedure MarkNext(i: Integer);
  begin
   InResult[i] := true;
   Result.Add(i);
  end;

var MinY: Single;
    i0, i, NextI: Integer;
begin
 Assert(Points.Count >= 1);

 { find i0, index of lowest point in Points }
 MinY := Points.List^[0][1];
 i0 := 0;
 for i := 1 to Points.Count-1 do
  if Points.List^[i][1] < MinY then
  begin
   MinY := Points.List^[i][1];
   i0 := i;
  end;

 InResult := TBooleanList.Create;
 try
  InResult.Count := Points.Count; { TFPGList already initializes all to false }
  Result := TIntegerList.Create;
  try
   MarkNext(i0);

   i := i0;
   while FindNext(i, NextI, true ) do begin i := NextI; MarkNext(i); end;
   while FindNext(i, NextI, false) do begin i := NextI; MarkNext(i); end;

  except Result.Free; raise end;
 finally InResult.Free end;
end;

end.
