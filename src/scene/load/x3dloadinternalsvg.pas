{
  Copyright 2026-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Loading SVG files.

  Registers SVG as a model format (in the initialization section of this unit),
  so you can load SVG files into TCastleScene like any other model,
  e.g. @code(Scene.Url := 'castle-data:/my.svg').

  This is only possible when compiling with FPC that includes fcl-svg package
  (latest FPC from "main" branch, from 2026-10-07 or newer).
  With older FPC, or with Delphi, the SVG format is not registered.

  The SVG is converted to X3D nodes (vector graphics, no rasterization).

  Most important notes on how the SVG content is interpreted by the engine
  are on https://castle-engine.io/model_formats#svg .

  How it works:

  @unorderedList(
    @item(fcl-svg (from FPC) parses SVG and resolves everything
      (CSS, use, symbol, markers, text layout, arcs, transformations...).
      It then "renders" the document by calling
      methods of a TSVGRenderBackend descendant.)

    @item(Our backend turns these calls into X3D nodes.
      Each fill, stroke and text run becomes one X3D TShapeNode
      with TIndexedFaceSetNode and TUnlitMaterialNode.)
  )

  Simplifications (SVG features not fully supported):

  @unorderedList(
    @item(Each SVG subpath becomes a separate X3D face, triangulated by CGE.
      So SVG fill-rule (evenodd / nonzero) and holes are not honored,
      e.g. inside of "o" and "e" letters is filled.)

    @item(Gradients are approximated by per-vertex colors.)

    @item(Clipping (clip-path, overflow) clips to the convex hull of the clip
      shape. This is exact for convex clip shapes, like circle or rectangle.)

    @item(Masks, filters, patterns, group opacity, images are not supported.
      fcl-svg then draws the content without these effects.)

    @item(SVG text (using fonts) is not rendered.
      We don't use the system fonts provided by fcl-svg (SVGPlatformFontProvider),
      because on most platforms it links to the FreeType library statically,
      which would make every application require FreeType at startup.
      Text already converted to paths (curves) in SVG is rendered OK.
      TODO: Implement fcl-svg font provider using Castle Game Engine fonts.)
  ) }
unit X3DLoadInternalSvg;

{$I castleconf.inc}

interface

{ Define CASTLE_FCL_SVG when we can use fcl-svg package.

  - It requires FPC 3.3.1 from "main" branch, from 2026-10-07 or newer.

    fcl-svg was added to FPC "main" in late September 2026.
    In order to work also with older FPC 3.3.1 versions,
    we cannot just assume that FPC_FULLVERSION >= 30301 implies SVG support.

    We considered using "$if declared(FpSvg.Types)" but it doesn't work to detect
    units.

    So we detect a new System identifier, DynArrayRefCount,
    added to FPC "main" on 2026-10-07
    ( https://gitlab.com/freepascal.org/fpc/source/-/commit/05be34c09a ).
    When it is declared, we also have fcl-svg
    (on platforms where fcl-svg is built, see below).

    This hack has a small "blind spot":
    if you have FPC 3.3.1 from revisions between 2026-09-20 and 2026-10-07,
    then it has SVG but we will not define CASTLE_FCL_SVG.
    Just upgrade to latest FPC 3.3.1 snapshot to get SVG support.

    Note: This check must be after the "interface" keyword,
    as System unit is not yet loaded before it.

  - fcl-svg is only built for some OSes, see P.OSes in fcl-svg/fpmake.pp.

  Note: Check "defined(FPC)" first in the same $if expression,
  not only in a surrounding "$ifdef FPC".
  Delphi evaluates $if expressions even inside an inactive "$ifdef FPC",
  and then fails with "E2026 Constant expression expected"
  on unknown FPC_FULLVERSION. }
{$if defined(FPC) and
     (FPC_FULLVERSION >= 30301) and
     declared(DynArrayRefCount) and
     ( defined(MSWINDOWS) or
       defined(LINUX) or
       defined(DARWIN) or
       defined(BSD) or
       defined(SOLARIS)
     )}
  {$define CASTLE_FCL_SVG}
{$endif}

{ Apply CASTLE_SVG_SUPPORT_DISABLE,
  although for most cases it is already eliminated by excluding
  from X3DLoad uses. }
{$ifdef CASTLE_SVG_SUPPORT_DISABLE}
  {$undef CASTLE_FCL_SVG}
{$endif}

type
  { Information about the last SVG loading.
    Only for informational / debug purposes. }
  TSvgToX3DStats = record
    { Size of the SVG document, in SVG pixels. }
    Width, Height: Integer;
    { Drawing operations that fcl-svg asked us to perform. }
    Fills, Strokes, GlyphRuns: Integer;
    { Drawing operations we ignored. }
    IgnoredImages: Integer;
    { X3D content we created. }
    Shapes, Faces, Points: Integer;
  end;

var
  { Information about the last loaded SVG.
    Only for informational / debug purposes. }
  InternalLastSvgStats: TSvgToX3DStats;

  { Distance in Z between the consecutive shapes (layers) created from SVG.

    The drawing order of SVG is preserved by giving each
    shape a larger Z than the previous one.
    So the thickness in Z of the loaded SVG is
    (number of shapes - 1) * SvgLayersZDistance.

    Changing this affects the SVG files loaded later. }
  SvgLayersZDistance: Single = 1.0;

implementation

{$ifdef CASTLE_FCL_SVG}

uses SysUtils, Classes, Math, Generics.Collections,
  FpSvg.Types, FpSvg.Backend, FpSvg.Dom, FpSvg.Read, FpSvg.Geom, FpSvg.Render,
  X3DNodes, X3DLoad, CastleUtils, CastleVectors, CastleColors, CastleCurves;

{ 2D polygon helpers ---------------------------------------------------------- }

type
  { Polygon in 2D, in SVG device space. }
  TPolygon = array of TVector2;
  TPolygonArray = array of TPolygon;

{ Cross product of (B - A) and (P - A).
  Positive when P is on the left side of the line from A to B. }
function Cross(const A, B, P: TVector2): Single;
begin
  Result := (B.X - A.X) * (P.Y - A.Y) - (B.Y - A.Y) * (P.X - A.X);
end;

{ Convex hull of all points of all polygons, in counter-clockwise order
  (in the sense of Cross being positive inside).
  Returns @nil if the hull has less than 3 points. }
function ConvexHull(const Polygons: TPolygonArray): TPolygon;
var
  Points: TVector3List;
  Indexes: TIntegerList;
  I, J: Integer;
begin
  Result := nil;
  Points := TVector3List.Create;
  try
    for I := 0 to High(Polygons) do
      for J := 0 to High(Polygons[I]) do
        Points.Add(Vector3(Polygons[I][J], 0));
    if Points.Count < 3 then
      Exit;
    Indexes := ConvexHullIndexes(Points);
    try
      if Indexes.Count < 3 then
        Exit;
      SetLength(Result, Indexes.Count);
      for I := 0 to Indexes.Count - 1 do
        Result[I] := Points.L[Indexes.L[I]].XY;
    finally FreeAndNil(Indexes) end;
  finally FreeAndNil(Points) end;
end;

{ Clip Subject polygon to the Clip polygon, that must be convex and
  in counter-clockwise order (like ConvexHull result).
  Uses Sutherland-Hodgman algorithm.
  Subject may be concave (the result is then correct, but may contain
  degenerate edges along the clip boundary, which is harmless for rendering).
  Returns @nil if nothing remains after clipping. }
function ClipPolygon(const Subject, Clip: TPolygon): TPolygon;
var
  Input: TPolygon;
  Count, I, J: Integer;
  EdgeStart, EdgeEnd, Current, Previous: TVector2;

  function Inside(const P: TVector2): Boolean;
  begin
    Result := Cross(EdgeStart, EdgeEnd, P) >= 0;
  end;

  procedure Add(const P: TVector2);
  begin
    Result[Count] := P;
    Inc(Count);
  end;

  { Intersection of line Previous->Current with the edge line.
    Called only when Previous and Current are on different sides of the edge line,
    so the lines are not parallel.
    Also EdgeStart <> EdgeEnd then (otherwise all points would be inside). }
  function Intersection: TVector2;
  begin
    try
      Result := Lines2DIntersection(
        Line2DFrom2Points(Previous, Current),
        Line2DFrom2Points(EdgeStart, EdgeEnd));
    except
      { Possible only due to floating-point inaccuracy,
        when Previous and Current are both very close to the edge line. }
      on ELinesParallel do
        Result := Previous;
    end;
  end;

begin
  if Length(Clip) < 3 then
    Exit(nil); // empty clip keeps nothing
  Result := Subject;
  for I := 0 to High(Clip) do
  begin
    if Length(Result) = 0 then
      Exit;
    EdgeStart := Clip[I];
    EdgeEnd := Clip[(I + 1) mod Length(Clip)];
    Input := Result;
    { Each input vertex adds at most 2 output vertexes. }
    Result := nil;
    SetLength(Result, 2 * Length(Input));
    Count := 0;
    Previous := Input[High(Input)];
    for J := 0 to High(Input) do
    begin
      Current := Input[J];
      if Inside(Current) then
      begin
        if not Inside(Previous) then
          Add(Intersection);
        Add(Current);
      end else
      if Inside(Previous) then
        Add(Intersection);
      Previous := Current;
    end;
    SetLength(Result, Count);
  end;
  if Length(Result) < 3 then
    Result := nil;
end;

{ Pending shapes ------------------------------------------------------------- }

type
  { Shape to be converted to X3D, with polygons in SVG device space.
    We keep shapes in this form (not as X3D nodes) until the end of SVG
    rendering, as they may still be clipped (see TX3DBackend.PopLayerAsMask). }
  TPendingShape = class
    Polygons: TPolygonArray;
    Opacity: Double;
    { When UseGradient, Gradient and GradientInverse determine the color.
      Otherwise Color determines the color. }
    UseGradient: Boolean;
    Color: TSVGColor;
    Gradient: TSVGGradient;
    { Maps SVG device space to gradient space. }
    GradientInverse: TSVGMatrix;
    { Clip all polygons to the given convex polygon (like ConvexHull result). }
    procedure Clip(const ClipHull: TPolygon);
  end;

  TPendingShapeList = specialize TObjectList<TPendingShape>;
  TPendingShapeListStack = specialize TObjectList<TPendingShapeList>;
  TPolygonStack = specialize TList<TPolygon>;

procedure TPendingShape.Clip(const ClipHull: TPolygon);
var
  I, Count: Integer;
  Clipped: TPolygon;
begin
  Count := 0;
  for I := 0 to High(Polygons) do
  begin
    Clipped := ClipPolygon(Polygons[I], ClipHull);
    if Clipped <> nil then
    begin
      Polygons[Count] := Clipped;
      Inc(Count);
    end;
  end;
  SetLength(Polygons, Count);
end;

{ TX3DBackend ---------------------------------------------------------------- }

type
  { Backend for fcl-svg renderer that creates X3D nodes. }
  TX3DBackend = class(TSVGRenderBackend)
  strict private
    { Temporary paths, to avoid allocating them for each shape. }
    FSource, FDashed, FPoly: TSVGPolyPath;
    FGlyphPath: TSVGPath;
    { Stack of layers. Each layer holds shapes drawn into it.
      The bottom layer holds the final shapes. }
    FLayers: TPendingShapeListStack;
    { Stack of clip polygons (each is a convex hull of the clip path). }
    FClips: TPolygonStack;
    function CurrentLayer: TPendingShapeList;
    { Copy FPoly (already in device space) to an array of polygons. }
    function PolyToPolygons: TPolygonArray;
    { Add FPoly (already in device space) as a new shape. }
    procedure AddShape(const Paint: TSVGPaint; const Opacity: Double;
      const CTM: TSVGMatrix; const UserBounds: TSVGRect);
    { Add shape filling given path (in user space). }
    procedure AddFill(const Path: TSVGPath; const CTM: TSVGMatrix;
      const Paint: TSVGPaint; const Opacity: Double);
    procedure PushClipPoly;
    { Convert final shapes to X3D. }
    procedure CreateNodes(const Shapes: TPendingShapeList);
  public
    { Place created X3D nodes here. Set before rendering. }
    Parent: TAbstractGroupingNode;
    Stats: TSvgToX3DStats;
    constructor Create; override;
    destructor Destroy; override;
    class function BackendName: String; override;
    class function Capabilities: TSVGBackendCapabilities; override;
    procedure BeginFrame(aWidth, aHeight: Integer); override;
    procedure EndFrame; override;
    procedure FillPath(aPath: TSVGPath; const aCTM: TSVGMatrix;
      const aPaint: TSVGPaint; aRule: TSVGFillRule;
      aOpacity: Double); override;
    procedure StrokePath(aPath: TSVGPath; const aCTM: TSVGMatrix;
      const aPaint: TSVGPaint; const aPen: TSVGPen;
      aOpacity: Double); override;
    procedure PushClip(aPath: TSVGPath; const aCTM: TSVGMatrix;
      aRule: TSVGFillRule); override;
    procedure PushStrokeClip(aPath: TSVGPath; const aCTM: TSVGMatrix;
      const aPen: TSVGPen); override;
    procedure PopClip; override;
    procedure SetColorInterpolation(
      aSpace: TSVGColorInterpolation); override;
    procedure PushLayer(const aBounds: TSVGRect; aOpacity: Double;
      aIsolate: Boolean); override;
    procedure PopLayer; override;
    procedure PopLayerAsMask(aMode: TSVGMaskMode); override;
    procedure DrawGlyphRun(aFont: TSVGFontHandle;
      const aGlyphs: TSVGGlyphArray; const aCTM: TSVGMatrix;
      const aPaint: TSVGPaint; aOpacity: Double); override;
    procedure DrawImage(aImage: ISVGImageSource; const aRect: TSVGRect;
      const aCTM: TSVGMatrix; aOpacity: Double); override;
  end;

const
  { How precisely to convert curves to line segments, in SVG pixels.
    Same as TSVGSoftBackend uses. }
  Flatness = SVGDefaultFlatness;

  { Multiply SVG Y coordinates by this, to make SVG content go up in CGE.
    In SVG, Y goes down; in CGE (and X3D), Y goes up.

    We flip the Y of the final points, instead of using a TTransformNode
    with negative scale, because negative scale interacts badly with
    physics colliders and lighting (e.g. if the user changes the material,
    or adds a mesh collider).

    All SVG transformations (transform attribute, viewBox, use, symbol,
    markers...) are already applied (by fcl-svg and our backend)
    when we do this, so this is simply the final conversion of coordinates. }
  ConvertYCoords = -1;

constructor TX3DBackend.Create;
begin
  inherited Create;
  FSource := TSVGPolyPath.Create;
  FDashed := TSVGPolyPath.Create;
  FPoly := TSVGPolyPath.Create;
  FGlyphPath := TSVGPath.Create;
  FLayers := TPendingShapeListStack.Create(true);
  FClips := TPolygonStack.Create;
end;

destructor TX3DBackend.Destroy;
begin
  FreeAndNil(FSource);
  FreeAndNil(FDashed);
  FreeAndNil(FPoly);
  FreeAndNil(FGlyphPath);
  FreeAndNil(FLayers);
  FreeAndNil(FClips);
  inherited;
end;

class function TX3DBackend.BackendName: String;
begin
  Result := 'castle-engine-x3d';
end;

class function TX3DBackend.Capabilities: TSVGBackendCapabilities;
begin
  { We handle clipping (approximately, by convex hull).
    For all other capabilities, fcl-svg renderer does a fallback
    (draws the content without the effect). }
  Result := [bcClipPath];
end;

procedure TX3DBackend.BeginFrame(aWidth, aHeight: Integer);
begin
  Stats.Width := aWidth;
  Stats.Height := aHeight;
  FLayers.Clear;
  FLayers.Add(TPendingShapeList.Create(true));
  FClips.Clear;
end;

procedure TX3DBackend.EndFrame;
begin
  Assert(FLayers.Count = 1, 'Unbalanced PushLayer / PopLayer');
  CreateNodes(CurrentLayer);
  FLayers.Clear;
end;

function TX3DBackend.CurrentLayer: TPendingShapeList;
begin
  Result := FLayers.Last;
end;

function TX3DBackend.PolyToPolygons: TPolygonArray;
var
  SubPath, I, PointCount, Count: Integer;
begin
  SetLength(Result, FPoly.SubPathCount);
  Count := 0;
  for SubPath := 0 to FPoly.SubPathCount - 1 do
  begin
    PointCount := FPoly.PointCount[SubPath];
    { Flattened closed subpath may repeat the 1st point at the end. }
    if (PointCount > 1) and
       (FPoly.Points[SubPath, 0].X = FPoly.Points[SubPath, PointCount - 1].X) and
       (FPoly.Points[SubPath, 0].Y = FPoly.Points[SubPath, PointCount - 1].Y) then
      Dec(PointCount);
    if PointCount < 3 then
      Continue;
    SetLength(Result[Count], PointCount);
    for I := 0 to PointCount - 1 do
      Result[Count][I] := Vector2(
        FPoly.Points[SubPath, I].X,
        FPoly.Points[SubPath, I].Y);
    Inc(Count);
  end;
  SetLength(Result, Count);
end;

procedure TX3DBackend.AddShape(const Paint: TSVGPaint; const Opacity: Double;
  const CTM: TSVGMatrix; const UserBounds: TSVGRect);
var
  ResolvedPaint: TSVGPaint;
  Shape: TPendingShape;
  ToGradient: TSVGMatrix;
  I: Integer;
begin
  if Opacity <= 0 then
    Exit;

  Shape := TPendingShape.Create;
  try
    Shape.Opacity := Opacity;
    ResolvedPaint := Paint.Resolved;
    case ResolvedPaint.Kind of
      spNone: Exit;
      spColor:
        Shape.Color := ResolvedPaint.Color;
      spServer:
        begin
          { Only gradients are handled, patterns are not. }
          if (not ResolvedPaint.Server.GetGradient(Shape.Gradient)) or
             (not Shape.Gradient.HasStops) then
            Exit;
          { Gradient space maps to the device by: gradient transform,
            then (if necessary) bounding box, then CTM.
            Just like in TSVGSoftBackend.PrepareGradient. }
          ToGradient := Shape.Gradient.Transform;
          if Shape.Gradient.Units = guObjectBoundingBox then
          begin
            if UserBounds.IsEmpty then
              Exit;
            ToGradient := ToGradient.Compose(TSVGGradient.BoxTransform(UserBounds));
          end;
          ToGradient := ToGradient.Compose(CTM);
          if not ToGradient.Invert(Shape.GradientInverse) then
            Exit;
          Shape.UseGradient := true;
        end;
    end;

    Shape.Polygons := PolyToPolygons;
    for I := 0 to FClips.Count - 1 do
      Shape.Clip(FClips[I]);
    if Length(Shape.Polygons) = 0 then
      Exit;

    CurrentLayer.Add(Shape);
    Shape := nil; // now owned by CurrentLayer
  finally
    FreeAndNil(Shape);
  end;
end;

procedure TX3DBackend.CreateNodes(const Shapes: TPendingShapeList);

  function SvgColorToRGB(const C: TSVGColor): TCastleColorRGB;
  begin
    Result := Vector3(
      C.Red / $FFFF,
      C.Green / $FFFF,
      C.Blue / $FFFF
    );
  end;

  function SvgColorToRGBA(const C: TSVGColor; const Opacity: Double): TCastleColor;
  begin
    Result := Vector4(
      C.Red / $FFFF,
      C.Green / $FFFF,
      C.Blue / $FFFF,
      C.Alpha / $FFFF * Opacity
    );
  end;

var
  ShapeIndex, PolygonIndex, I: Integer;
  PendingShape: TPendingShape;
  P: TVector2;
  Coord: TCoordinateNode;
  ColorNode: TColorRGBANode;
  Geometry: TIndexedFaceSetNode;
  Material: TUnlitMaterialNode;
  Appearance: TAppearanceNode;
  Shape: TShapeNode;
begin
  for ShapeIndex := 0 to Shapes.Count - 1 do
  begin
    PendingShape := Shapes[ShapeIndex];

    Coord := TCoordinateNode.Create;

    Geometry := TIndexedFaceSetNode.Create;
    Geometry.Coord := Coord;
    { Faces are visible from both sides. SVG doesn't specify orientation. }
    Geometry.Solid := false;
    { SVG subpaths are often concave. }
    Geometry.Convex := false;

    ColorNode := nil;
    if PendingShape.UseGradient then
    begin
      ColorNode := TColorRGBANode.Create;
      Geometry.Color := ColorNode;
    end;

    for PolygonIndex := 0 to High(PendingShape.Polygons) do
    begin
      for I := 0 to High(PendingShape.Polygons[PolygonIndex]) do
      begin
        P := PendingShape.Polygons[PolygonIndex][I];
        Geometry.FdCoordIndex.Items.Add(Coord.FdPoint.Items.Count);
        { Z grows with the index of the shape, to preserve SVG drawing order. }
        Coord.FdPoint.Items.Add(Vector3(
          P.X,
          P.Y * ConvertYCoords,
          ShapeIndex * SvgLayersZDistance));
        { Gradient is calculated using P in SVG device space (not flipped by ConvertYCoords). }
        if ColorNode <> nil then
          ColorNode.FdColor.Items.Add(SvgColorToRGBA(
            PendingShape.Gradient.ColorAt(PendingShape.Gradient.OffsetAt(
              PendingShape.GradientInverse.Transform(TSVGPoint.Create(P.X, P.Y)))),
            PendingShape.Opacity));
      end;
      Geometry.FdCoordIndex.Items.Add(-1);
      Inc(Stats.Faces);
    end;
    Inc(Stats.Points, Coord.FdPoint.Items.Count);

    Material := TUnlitMaterialNode.Create;
    if PendingShape.UseGradient then
    begin
      { Colors (including alpha) come from ColorNode. }
      Material.EmissiveColor := WhiteRGB;
    end else
    begin
      Material.EmissiveColor := SvgColorToRGB(PendingShape.Color);
      Material.Transparency := 1 -
        PendingShape.Color.Alpha / $FFFF * PendingShape.Opacity;
    end;

    Appearance := TAppearanceNode.Create;
    Appearance.Material := Material;

    Shape := TShapeNode.Create;
    Shape.Appearance := Appearance;
    Shape.Geometry := Geometry;
    Parent.AddChildren(Shape);
    Inc(Stats.Shapes);
  end;
end;

procedure TX3DBackend.FillPath(aPath: TSVGPath; const aCTM: TSVGMatrix;
  const aPaint: TSVGPaint; aRule: TSVGFillRule; aOpacity: Double);
begin
  Inc(Stats.Fills);
  { We ignore aRule, see the unit comments about fill-rule. }
  AddFill(aPath, aCTM, aPaint, aOpacity);
end;

procedure TX3DBackend.AddFill(const Path: TSVGPath; const CTM: TSVGMatrix;
  const Paint: TSVGPaint; const Opacity: Double);
var
  Scale: Double;
begin
  Scale := CTM.MaxScale;
  if Scale <= 0 then
    Exit;
  { Flatten in user space (keeping the bounding box for gradients),
    then transform to device space. Like TSVGSoftBackend does. }
  FSource.Flatten(Path, TSVGMatrix.Identity, Flatness / Scale);
  FPoly.Assign(FSource);
  FPoly.Transform(CTM);
  AddShape(Paint, Opacity, CTM, FSource.Bounds);
end;

procedure TX3DBackend.StrokePath(aPath: TSVGPath; const aCTM: TSVGMatrix;
  const aPaint: TSVGPaint; const aPen: TSVGPen; aOpacity: Double);
var
  Scale, Tolerance: Double;
begin
  Inc(Stats.Strokes);
  if aPen.Width <= 0 then
    Exit;
  Scale := aCTM.MaxScale;
  if Scale <= 0 then
    Exit;
  Tolerance := Flatness / Scale;
  FSource.Flatten(aPath, TSVGMatrix.Identity, Tolerance);
  { fcl-svg calculates the outline of the stroke (with dashes, caps, joins)
    for us. The resulting pieces overlap, which is not a problem
    for opaque strokes. }
  if aPen.IsDashed then
  begin
    FDashed.BuildDashes(FSource, aPen.Dashes, aPen.DashOffset);
    FPoly.BuildStroke(FDashed, aPen, Tolerance);
  end else
    FPoly.BuildStroke(FSource, aPen, Tolerance);
  FPoly.Transform(aCTM);
  AddShape(aPaint, aOpacity, aCTM, FSource.Bounds);
end;

procedure TX3DBackend.PushClipPoly;
begin
  FClips.Add(ConvexHull(PolyToPolygons));
end;

procedure TX3DBackend.PushClip(aPath: TSVGPath; const aCTM: TSVGMatrix;
  aRule: TSVGFillRule);
begin
  FPoly.Flatten(aPath, aCTM, Flatness);
  PushClipPoly;
end;

procedure TX3DBackend.PushStrokeClip(aPath: TSVGPath; const aCTM: TSVGMatrix;
  const aPen: TSVGPen);
var
  Scale, Tolerance: Double;
begin
  Scale := aCTM.MaxScale;
  if Scale <= 0 then
  begin
    FClips.Add(nil); // clips everything
    Exit;
  end;
  Tolerance := Flatness / Scale;
  FSource.Flatten(aPath, TSVGMatrix.Identity, Tolerance);
  FPoly.BuildStroke(FSource, aPen, Tolerance);
  FPoly.Transform(aCTM);
  PushClipPoly;
end;

procedure TX3DBackend.PopClip;
begin
  FClips.Delete(FClips.Count - 1);
end;

procedure TX3DBackend.SetColorInterpolation(aSpace: TSVGColorInterpolation);
begin
  { Ignored, we only blend in sRGB. }
end;

procedure TX3DBackend.PushLayer(const aBounds: TSVGRect; aOpacity: Double;
  aIsolate: Boolean);
begin
  FLayers.Add(TPendingShapeList.Create(true));
end;

procedure TX3DBackend.PopLayer;
var
  Layer: TPendingShapeList;
begin
  { Move shapes from the layer to the parent layer. }
  Layer := FLayers.ExtractIndex(FLayers.Count - 1);
  try
    Layer.OwnsObjects := false;
    CurrentLayer.AddRange(Layer);
  finally FreeAndNil(Layer) end;
end;

procedure TX3DBackend.PopLayerAsMask(aMode: TSVGMaskMode);
var
  Mask: TPendingShapeList;
  MaskPolygons: TPolygonArray;
  MaskHull: TPolygon;
  Shape: TPendingShape;
  I: Integer;
begin
  { As we don't declare bcMask capability, this is only used by fcl-svg
    to clip by a clip-path that is not a single shape
    (e.g. it contains "use" element, like the CGE logo).
    The shapes drawn into the mask layer define the clip area.
    We clip shapes in the parent layer by their convex hull. }
  Mask := FLayers.ExtractIndex(FLayers.Count - 1);
  try
    MaskPolygons := nil;
    for Shape in Mask do
      for I := 0 to High(Shape.Polygons) do
      begin
        SetLength(MaskPolygons, Length(MaskPolygons) + 1);
        MaskPolygons[High(MaskPolygons)] := Shape.Polygons[I];
      end;
    MaskHull := ConvexHull(MaskPolygons);

    for I := CurrentLayer.Count - 1 downto 0 do
    begin
      CurrentLayer[I].Clip(MaskHull);
      if Length(CurrentLayer[I].Polygons) = 0 then
        CurrentLayer.Delete(I);
    end;
  finally FreeAndNil(Mask) end;
end;

procedure TX3DBackend.DrawGlyphRun(aFont: TSVGFontHandle;
  const aGlyphs: TSVGGlyphArray; const aCTM: TSVGMatrix;
  const aPaint: TSVGPaint; aOpacity: Double);
begin
  Inc(Stats.GlyphRuns);
  { Convert text to outline paths, and fill them. }
  FGlyphPath.Clear;
  if SVGAppendGlyphRun(aFont, aGlyphs, FGlyphPath) then
    AddFill(FGlyphPath, aCTM, aPaint, aOpacity);
end;

procedure TX3DBackend.DrawImage(aImage: ISVGImageSource; const aRect: TSVGRect;
  const aCTM: TSVGMatrix; aOpacity: Double);
begin
  { Not implemented. This could create a quad with PixelTexture. }
  Inc(Stats.IgnoredImages);
end;

{ LoadSvgModel --------------------------------------------------------------- }

{ Load SVG, convert it to X3D nodes. Registered as TModelFormat.OnLoad. }
function LoadSvgModel(const Stream: TStream; const BaseUrl: String;
  const LoadOptions: TCastleSceneLoadOptions): TX3DRootNode;
var
  Document: TSVGDocument;
  Renderer: TSVGRenderer;
  Backend: TX3DBackend;
  Transform: TTransformNode;
begin
  Result := TX3DRootNode.Create;
  try
    Transform := TTransformNode.Create;
    Result.AddChildren(Transform);

    Document := nil;
    Renderer := nil;
    Backend := nil;
    try
      Document := ReadSVGStream(Stream);
      Renderer := TSVGRenderer.Create;
      { We leave Renderer.Fonts = nil, so SVG text is not rendered.
        See the unit comments why we don't use SVGPlatformFontProvider. }
      Backend := TX3DBackend.Create;
      Backend.Parent := Transform;
      Renderer.Render(Document, Backend);
      InternalLastSvgStats := Backend.Stats;
    finally
      FreeAndNil(Backend);
      FreeAndNil(Renderer);
      FreeAndNil(Document);
    end;

    { Center the drawing at (0,0).
      Y coordinates were already multiplied by ConvertYCoords. }
    Transform.Translation := Vector3(
      -InternalLastSvgStats.Width / 2,
      -ConvertYCoords * InternalLastSvgStats.Height / 2, 0);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

var
  ModelFormat: TModelFormat;
initialization
  ModelFormat := TModelFormat.Create;
  ModelFormat.OnLoad := {$ifdef FPC}@{$endif} LoadSvgModel;
  ModelFormat.OnLoadForceMemoryStream := true;
  ModelFormat.MimeTypes.Add('image/svg+xml');
  ModelFormat.FileFilterName := 'SVG (*.svg)';
  ModelFormat.Extensions.Add('.svg');
  RegisterModelFormat(ModelFormat);

{$endif CASTLE_FCL_SVG}

end.
