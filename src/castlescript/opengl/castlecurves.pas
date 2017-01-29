{
  Copyright 2004-2017 Michalis Kamburelis.

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

uses SysUtils, Classes, FGL, DOM,
  CastleVectors, CastleBoxes, CastleUtils, CastleScript,
  CastleClassUtils, Castle3D, CastleFrustum, CastleColors;

type
  ECurveFileInvalid = class(Exception);

  { 3D curve, a set of points defined by a continous function @link(Point)
    for arguments within [TBegin, TEnd]. }
  TCurve = class(T3D)
  private
    FColor: TCastleColor;
    FLineWidth: Single;
    FTBegin, FTEnd: Single;
    FDefaultSegments: Cardinal;
  protected
    procedure LoadFromElement(const E: TDOMElement); virtual;
    procedure SaveToStream(const Stream: TStream); virtual;
  public
    { The valid range of curve function argument. Must be TBegin <= TEnd.
      @groupBegin }
    property TBegin: Single read FTBegin write FTBegin default 0;
    property TEnd: Single read FTEnd write FTEnd default 1;
    { @groupEnd }

    { Curve function, for each parameter value determine the 3D point.
      This determines the actual shape of the curve. }
    function Point(const t: Float): TVector3Single; virtual; abstract;
    function Point2D(const t: Float): TVector2Single;

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
      deprecated 'Do not use this, as you should not use Render method.';

    property LineWidth: Single read FLineWidth write FLineWidth default 1;
      deprecated 'Do not use this, as you should not use Render method.';

    { Default number of segments, used when rendering by T3D interface
      (that is, @code(Render(Frustum, TransparentGroup...)) method.) }
    property DefaultSegments: Cardinal
      read FDefaultSegments write FDefaultSegments default 10;

    procedure Render(const Frustum: TFrustum;
      const Params: TRenderParams); override;

    constructor Create(AOwner: TComponent); override;

    { Load the first curve defined in given XML file.
      Hint: use https://github.com/castle-engine/castle-engine/wiki/Curves-tool to design curves
      visually. }
    class function LoadFromFile(const URL: string): TCurve;
  end;

  TCurveList = class(specialize TFPGObjectList<TCurve>)
  public
    { Load curves definitions from a simple XML file.
      Hint: use https://github.com/castle-engine/castle-engine/wiki/Curves-tool to design curves
      visually. }
    procedure LoadFromFile(const URL: string);

    { Save curve definitions to a simple XML file.
      Hint: use https://github.com/castle-engine/castle-engine/wiki/Curves-tool to design curves
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
  strict private
    FBoundingBox: TBox3D;
    FControlPointsColor: TCastleColor;
    FConvexHullColor: TCastleColor;
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
    function CreateConvexHullPoints: TVector3SingleList; virtual;
    procedure DestroyConvexHullPoints(Points: TVector3SingleList); virtual;
  protected
    procedure LoadFromElement(const E: TDOMElement); override;
    procedure SaveToStream(const Stream: TStream); override;
  public
    ControlPoints: TVector3SingleList;

    property ControlPointsColor: TCastleColor read FControlPointsColor write FControlPointsColor;
      deprecated 'Do not use this, as you should not use RenderControlPoints method.';

    { Render control points, using ControlPointsColor. }
    procedure RenderControlPoints;
      deprecated 'Do not render stuff directly by this method. Instead create TCastleScene, initialize it''s X3D graph based on ControlPoints, and add it to the SceneManager.Items.';

    { Bounding box of the curve.
      In this class, it is simply a BoundingBox of ControlPoints. }
    function BoundingBox: TBox3D; override;

    { Always after changing ControlPoints or TBegin or TEnd and before calling Point,
      BoundingBox (and anything that calls them, e.g. Render calls Point)
      call this method. It recalculates necessary things.
      ControlPoints.Count must be >= 2.

      When overriding: always call inherited first. }
    procedure UpdateControlPoints; virtual;

    property ConvexHullColor: TCastleColor read FConvexHullColor write FConvexHullColor;
      deprecated 'Do not use this, as you should not use RenderConvexHull method.';

    { Render convex hull polygon, using ConvexHullColor.
      Ignores Z-coord of ControlPoints. }
    procedure RenderConvexHull;
      deprecated 'Do not render stuff directly by this method. Instead create TCastleScene, initialize it''s X3D graph based on ConvexHullPoints, and add it to the SceneManager.Items.';

    { Constructor. }
    constructor Create(AOwner: TComponent); override;

    { Calculate initial control points by sampling given TCasScriptCurve,
      with analytical curve equation.
      TBegin and TEnd are copied from CasScriptCurve. }
    constructor CreateFromEquation(CasScriptCurve: TCasScriptCurve;
      ControlPointsCount: Cardinal);

    destructor Destroy; override;
  end;

  TControlPointsCurveClass = class of TControlPointsCurve;

  TControlPointsCurveList = specialize TFPGObjectList<TControlPointsCurve>;

  { Rational Bezier curve (Bezier curve with weights).
    Note: for Bezier Curve ControlPoints.Count MAY be 1.
    (For TControlPointsCurve it must be >= 2) }
  TRationalBezierCurve = class(TControlPointsCurve)
  protected
    procedure LoadFromElement(const E: TDOMElement); override;
    procedure SaveToStream(const Stream: TStream); override;
  public
    { Curve weights.
      Must always be Weights.Count = ControlPoints.Count.
      After changing Weights you also have to call UpdateControlPoints.}
    Weights: TFloatList;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateControlPoints; override;
    function Point(const t: Float): TVector3Single; override;
  end deprecated 'Rendering of TRationalBezierCurve is not portable to OpenGLES (that is: Android and iOS) and not very efficient. Also, this is usually not very useful curve for game purposes, you usually want to use cubic Bezier (CubicBezier3D) or piecewise cubic Bezier (TPiecewiseCubicBezier) instead. For portable and fast general curves use X3D NURBS nodes (wrapped in a TCastleScene) instead.';

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
    Aka Cubic B-Spline (piecewise C2-Smooth Cubic Bezier).

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
  TPiecewiseCubicBezier = class(TControlPointsCurve)
  strict private
    BezierCurves: array of TCubicBezier3DPoints;
    ConvexHullPoints: TVector3SingleList;
    FBoundingBox: TBox3D;
  strict protected
    function CreateConvexHullPoints: TVector3SingleList; override;
    procedure DestroyConvexHullPoints(Points: TVector3SingleList); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateControlPoints; override;
    function Point(const t: Float): TVector3Single; override;
    function BoundingBox: TBox3D; override;
  end;

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

uses Math,
  CastleGL, CastleConvexHull, CastleGLUtils, CastleXMLUtils, CastleDownload;

{ TCurve ------------------------------------------------------------ }

function TCurve.PointOfSegment(i, Segments: Cardinal): TVector3Single;
begin
  Result := Point(TBegin + (i/Segments) * (TEnd-TBegin));
end;

procedure TCurve.Render(Segments: Cardinal);
{$ifndef OpenGLES} //TODO-es
{ Using deprecated stuff within deprecated stuff. }
{$warnings off}
var
  i: Integer;
begin
  glColorv(Color);
  RenderContext.LineWidth := LineWidth;
  glBegin(GL_LINE_STRIP);
  for i := 0 to Segments do glVertexv(PointOfSegment(i, Segments));
  glEnd;
{$warnings on}
{$else}
begin
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

function TCurve.Point2D(const T: Float): TVector2Single;
var
  V: TVector3Single;
begin
  V := Point(T);
  Result[0] := V[0];
  Result[1] := V[1];
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
        {$warnings off}
        { consciously using deprecated }
        if SameText(CurveTypeStr, TRationalBezierCurve.ClassName) then
          Curve := TRationalBezierCurve.Create(nil) else
        {$warnings on}
        if SameText(CurveTypeStr, TPiecewiseCubicBezier.ClassName) then
          Curve := TPiecewiseCubicBezier.Create(nil) else
        if SameText(CurveTypeStr, TCasScriptCurve.ClassName) then
          Curve := TCasScriptCurve.Create(nil) else
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

procedure TControlPointsCurve.RenderControlPoints;
{$ifndef OpenGLES} //TODO-es
var
  i: Integer;
begin
  {$warnings off}
  glColorv(ControlPointsColor);
  {$warnings on}
  glBegin(GL_POINTS);
  for i := 0 to ControlPoints.Count-1 do glVertexv(ControlPoints.L[i]);
  glEnd;
{$else}
begin
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
{$ifndef OpenGLES} //TODO-es
var
  CHPoints: TVector3SingleList;
  CH: TIntegerList;
  i: Integer;
begin
  CHPoints := CreateConvexHullPoints;
  try
    CH := ConvexHull(CHPoints);
    try
      {$warnings off}
      glColorv(ConvexHullColor);
      {$warnings on}
      glBegin(GL_POLYGON);
      try
        for i := 0 to CH.Count-1 do
          glVertexv(CHPoints.L[CH[i]]);
      finally glEnd end;
    finally CH.Free end;
  finally DestroyConvexHullPoints(CHPoints) end;
{$else}
begin
{$endif}
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

constructor TControlPointsCurve.CreateFromEquation(
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
    VectorStr := VectorToRawStr(ControlPoints[I]);
    WritelnStr(Stream, '      <control_point value="' + VectorStr + '"/>');
  end;
  WritelnStr(Stream, '    </control_points>');
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

procedure TRationalBezierCurve.LoadFromElement(const E: TDOMElement);
var
  I: TXMLElementIterator;
  EControlPoints: TDOMElement;
begin
  inherited LoadFromElement(E);

  EControlPoints := E.ChildElement('weights');
  I := EControlPoints.ChildrenIterator('weight');
  try
    while I.GetNext do
      Weights.Add(I.Current.AttributeSingle('value'));
  finally FreeAndNil(I); end;
end;

procedure TRationalBezierCurve.SaveToStream(const Stream: TStream);
var
  I: Integer;
begin
  inherited SaveToStream(Stream);

  WritelnStr(Stream, '    <weights>');
  for I := 0 to Weights.Count - 1 do
    WritelnStr(Stream, '      <weight value="' + FloatToRawStr(Weights[I]) + '"/>');
  WritelnStr(Stream, '    </weights>');
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

  if IndexBefore >= Length(BezierCurves) then
    raise Exception.Create('Curves data inside PiecewiseCubicBezier not initialized, probably you forgot to call UpdateControlPoints after changing the ControlPoints');
  Result := CubicBezier3D(TInsidePiece, BezierCurves[IndexBefore]);
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

  procedure UpdateBoundingBox;
  begin
    FBoundingBox := CalculateBoundingBox(PVector3Single(ConvexHullPoints.List),
      ConvexHullPoints.Count, 0);
  end;

begin
  inherited;
  UpdateBezierCurves;
  UpdateConvexHullPoints;
  UpdateBoundingBox;
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

function TPiecewiseCubicBezier.BoundingBox: TBox3D;
begin
  Result := FBoundingBox;
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
