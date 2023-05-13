{%MainUnit x3dnodes.pas}
{
  Copyright 2008-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { Base type for nodes that provide control curve information in 2D space. }
  TAbstractNurbsControlCurveNode = class(TAbstractNode)
  {$I auto_generated_node_helpers/x3dnodes_x3dnurbscontrolcurvenode.inc}
  end;

  { Base type for all geometry node types that are created parametrically
    and use control points to describe the final shape of the surface. }
  TAbstractParametricGeometryNode = class(TAbstractGeometryNode)
  {$I auto_generated_node_helpers/x3dnodes_x3dparametricgeometrynode.inc}
  end;

  { Abstract geometry type for all types of NURBS surfaces. }
  TAbstractNurbsSurfaceGeometryNode = class(TAbstractParametricGeometryNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    function Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode; override;
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function BoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function InternalCoord(State: TX3DGraphTraverseState;
      out ACoord: TMFVec3f): boolean; override;
    function CoordField: TSFNode; override;
    function SolidField: TSFBool; override;

    { Get the position of a point on the surface.

      The returned position is in the local transformation space of this shape.
      This method is guaranteed to work the same,
      regardless if this node is part of any TX3DRootNode and TCastleSceneCore
      or not.

      @param U First parameter of the parametric surface, in [0..1] range.
      @param V Second parameter of the parametric surface, in [0..1] range.
      @param(OutputNormal Optional. If non-nil, will be set to the normal,
        that is, normalized direction in 3D that is orthogonal to the surface
        at this point.) }
    function Point(const U, V: Single; const OutputNormal: PVector3 = nil): TVector3;

  {$I auto_generated_node_helpers/x3dnodes_x3dnurbssurfacegeometrynode.inc}
  end;

  { Groups a set of curve segments for a composite contour, for X3D.

    X3D cannot share implementation with VRML 2.0 version (TContour2DNode_2),
    since for VRML 2.0 this is a valid geometry node (TAbstractGeometryNode
    descendant). }
  TContour2DNode = class(TAbstractNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    class function ForVRMLVersion(const Version: TX3DVersion): boolean; override;
  {$I auto_generated_node_helpers/x3dnodes_contour2d.inc}
  end;

  { Piecewise linear curve segment as a part
    of a trimming contour in the u, v domain of a surface. }
  TContourPolyline2DNode = class(TAbstractNurbsControlCurveNode)
  {$I auto_generated_node_helpers/x3dnodes_contourpolyline2d.inc}
  end;

  { 3D coordinates defines using double precision floating point values. }
  TCoordinateDoubleNode = class(TAbstractCoordinateNode)
  public
    function CoordCount: Cardinal; override;
  {$I auto_generated_node_helpers/x3dnodes_coordinatedouble.inc}
  end;

  { Visible NURBS curve in 3D. }
  TNurbsCurveNode = class(TAbstractParametricGeometryNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    class function ForVRMLVersion(const Version: TX3DVersion): boolean; override;
    function Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode; override;
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function BoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function InternalCoord(State: TX3DGraphTraverseState;
      out ACoord: TMFVec3f): boolean; override;
    function CoordField: TSFNode; override;

    { Make this NURBS curve equal to a piecewise Bezier curve by setting appropriate
      "knot". This looks at @link(ControlPoint) count and @link(Order)
      (set Order = 4 for a typical cubic Bezier curve). }
    procedure PiecewiseBezier;

    { Get the position of a point on the curve.

      The returned position is in the local transformation space of this shape.
      This method is guaranteed to work the same,
      regardless if this node is part of any TX3DRootNode and TCastleSceneCore
      or not.

      @param U The place on a curve, in [0..1] range.
      @param(Tangent Optional. If non-nil, will be set to the tangent,
        that is, direction in 3D in which the curve is going.) }
    function Point(const U: Single; const Tangent: PVector3 = nil): TVector3;

    {$I auto_generated_node_helpers/x3dnodes_nurbscurve.inc}
  end;
  TNurbsCurveNode_3 = TNurbsCurveNode;

  { Trimming segment that is expressed a NURBS curve and is
    part of a trimming contour in the u,v domain of the surface. }
  TNurbsCurve2DNode = class(TAbstractNurbsControlCurveNode)
  {$I auto_generated_node_helpers/x3dnodes_nurbscurve2d.inc}
  end;

  { Interpolate (animate) orientations as tangent vectors of the 3D NURBS curve. }
  TNurbsOrientationInterpolatorNode = class(TAbstractChildNode)
  strict private
    procedure EventSet_FractionReceive(const Event: TX3DEvent; const Value: TX3DField;
      const Time: TX3DTime);
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
  {$I auto_generated_node_helpers/x3dnodes_nurbsorientationinterpolator.inc}
  end;

  { Visible NURBS 3D surface. }
  TNurbsPatchSurfaceNode = class(TAbstractNurbsSurfaceGeometryNode)
  {$I auto_generated_node_helpers/x3dnodes_nurbspatchsurface.inc}
  end;

  { Interpolate (animate) positions along the 3D NURBS curve. }
  TNurbsPositionInterpolatorNode = class(TAbstractChildNode)
  strict private
    procedure EventSet_FractionReceive(const Event: TX3DEvent; const Value: TX3DField;
      const Time: TX3DTime);
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
    class function ForVRMLVersion(const Version: TX3DVersion): boolean; override;

    {$I auto_generated_node_helpers/x3dnodes_nurbspositioninterpolator.inc}
  end;
  TNurbsPositionInterpolatorNode_3 = TNurbsPositionInterpolatorNode;

  { Groups a set of NURBS surface nodes to a common group
    for rendering purposes, to ensure a common tesselation within the group. }
  TNurbsSetNode = class(TAbstractChildNode)
  public
    { Implementation note: Fdgeometry is not enumerated in DirectEnumerateActive,
      as it's not actually rendered from NurbsSet node.
      Children here have to be placed elsewhere, in some Shape,
      to actually get enumerated as "active". }
    { }

  {$I auto_generated_node_helpers/x3dnodes_nurbsset.inc}
  end;

  { Interpolate (animate) by sampling a position and normal at 3D NURBS surface
    from an input 2D surface parameters. }
  TNurbsSurfaceInterpolatorNode = class(TAbstractChildNode)
  strict private
    procedure EventSet_FractionReceive(const Event: TX3DEvent; const Value: TX3DField;
      const Time: TX3DTime);
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
  {$I auto_generated_node_helpers/x3dnodes_nurbssurfaceinterpolator.inc}
  end;

  { Path in 2D space (that can be constructed from NURBS curves,
    or straight segments) extruded along a 3D NURBS curve.
    @bold(Rendering of this node is not implemented yet.) }
  TNurbsSweptSurfaceNode = class(TAbstractParametricGeometryNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    function SolidField: TSFBool; override;
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

  {$I auto_generated_node_helpers/x3dnodes_nurbssweptsurface.inc}
  end;

  { Path in 2D space (that can be constructed from NURBS curves,
    or straight segments) extruded along a 2D NURBS curve.
    @bold(Rendering of this node is not implemented yet.) }
  TNurbsSwungSurfaceNode = class(TAbstractParametricGeometryNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    function SolidField: TSFBool; override;
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

    {$I auto_generated_node_helpers/x3dnodes_nurbsswungsurface.inc}
  end;

  { NURBS surface existing in the parametric domain of its surface host
    specifying the mapping of the texture onto the surface.
    @bold(Not implemented yet.) }
  TNurbsTextureCoordinateNode = class(TAbstractNode)
  {$I auto_generated_node_helpers/x3dnodes_nurbstexturecoordinate.inc}
  end;

  { Visible 3D NURBS surface (like a @link(TNurbsPatchSurfaceNode))
    that is trimmed by a set of trimming loops.

    @bold(The trimming is not implemented yet. This is rendered exactly
    like the normal @link(TNurbsPatchSurfaceNode).) }
  TNurbsTrimmedSurfaceNode = class(TAbstractNurbsSurfaceGeometryNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    function Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode; override;
  {$I auto_generated_node_helpers/x3dnodes_nurbstrimmedsurface.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

function TAbstractNurbsSurfaceGeometryNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := FdcontrolPoint.Enumerate(Func);
  if Result <> nil then Exit;

  Result := FdtexCoord.Enumerate(Func);
  if Result <> nil then Exit;
end;

class function TContour2DNode.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major >= 3;
end;

function TContour2DNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := Fdchildren.Enumerate(Func);
  if Result <> nil then Exit;
end;

function TCoordinateDoubleNode.CoordCount: Cardinal;
begin
  Result := FdPoint.Items.Count;
end;

class function TNurbsCurveNode.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major >= 3;
end;

function TNurbsCurveNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := FdcontrolPoint.Enumerate(Func);
  if Result <> nil then Exit;
end;

{ Convert X3D or VRML 97 NurbsCurve to LineSet. }
procedure NurbsCurveProxy(
  const LS: TLineSetNode;
  const ControlPoint: TVector3List;
  const Tessellation, Order: LongInt;
  const FieldKnot, Weight: TDoubleList);
var
  ResultCoord: TCoordinateNode;
  Tess: Cardinal;
  I: Integer;
  Increase: Double;
  Knot: TDoubleList;
  NurbsCalculator: TNurbsCurveCalculator;
begin
  if ControlPoint.Count = 0 then Exit;

  if Order < 2 then
  begin
    WritelnWarning('VRML/X3D', 'NURBS order must be >= 2');
    Exit;
  end;

  { We can be sure now that
    - we have ControlPoint, non-nil, with at least 1 point.
    - we have Order >= 2 }

  { calculate correct Knot vector }
  Knot := TDoubleList.Create;
  Knot.Assign(FieldKnot);
  NurbsKnotIfNeeded(Knot, ControlPoint.Count, Order, nkEndpointUniform);

  { calculate tesselation: Tess, Increase }
  Tess := ActualTessellation(Tessellation, ControlPoint.Count);
  Increase := (Knot.Last - Knot.First) / (Tess - 1);

  { make resulting Coordinate node }
  ResultCoord := TCoordinateNode.Create('', LS.BaseUrl);
  LS.FdCoord.Value := ResultCoord;

  { calculate result Coordinate.point field }
  ResultCoord.FdPoint.Items.Count := Tess;
  NurbsCalculator := TNurbsCurveCalculator.Create(ControlPoint, Order, Knot, Weight);
  try
    for I := 0 to Tess - 1 do
      ResultCoord.FdPoint.Items.L[I] := NurbsCalculator.GetPoint(
        Knot.First + Increase * I, nil);
  finally FreeAndNil(NurbsCalculator) end;

  { set LineSet.vertexCount (otherwise it's coord will be ignored) }
  LS.FdVertexCount.Items.Add(Tess);

  FreeAndNil(Knot);
end;

function TNurbsCurveNode.Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode;
var
  ControlPointList: TVector3List;
begin
  Result := TLineSetNode.Create(X3DName, BaseUrl);
  try
    { TODO: we should handle here all TAbstractCoordinateNode }
    if ControlPoint is TCoordinateNode then
      ControlPointList := TCoordinateNode(ControlPoint).FdPoint.Items
    else
      Exit;

    NurbsCurveProxy(TLineSetNode(Result), ControlPointList, FdTessellation.Value,
      FdOrder.Value, FdKnot.Items, FdWeight.Items);
  except FreeAndNil(Result); raise end;
end;

function TNurbsCurveNode.CoordField: TSFNode;
begin
  Result := FdControlPoint;
end;

{ Although our BoundingBox and LocalBoundingBox do not rely on InternalCoord()
  override, it is still necessary to react to animation of Coordinate node
  points, see
  http://www.web3d.org/x3d/content/examples/Basic/NURBS/_pages/page01.html }
function TNurbsCurveNode.InternalCoord(State: TX3DGraphTraverseState;
  out ACoord: TMFVec3f): boolean;
begin
  Result := true;
  if FdControlPoint.Value is TCoordinateNode then // also checks that FdControlPoint.Value <> nil
    ACoord := TCoordinateNode(FdControlPoint.Value).FdPoint
  else
    ACoord := nil;
end;

{ We cannot simply override InternalCoord() and let bounding box be calculated
  based on it. It would fail for curves with weigths. }
function TNurbsCurveNode.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  if FdControlPoint.Value is TCoordinateNode then // also checks that FdControlPoint.Value <> nil
    Result := NurbsBoundingBox(TCoordinateNode(FdControlPoint.Value).FdPoint.Items, FdWeight.Items)
  else
    Result := TBox3D.Empty;
end;

{ We cannot simply override InternalCoord() and let bounding box be calculated
  based on it. It would fail for curves with weigths. }
function TNurbsCurveNode.BoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  if FdControlPoint.Value is TCoordinateNode then // also checks that FdControlPoint.Value <> nil
    Result := NurbsBoundingBox(TCoordinateNode(FdControlPoint.Value).FdPoint.Items, FdWeight.Items, State.Transformation.Transform)
  else
    Result := TBox3D.Empty;
end;

function TNurbsCurveNode.TrianglesCount(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  Result := 0;
end;

procedure TNurbsCurveNode.PiecewiseBezier;
var
  ControlPointCount: Integer;
begin
  FdKnot.Items.Clear;

  if ControlPoint is TCoordinateNode then
  begin
    ControlPointCount := TCoordinateNode(ControlPoint).FdPoint.Count;
    NurbsKnotIfNeeded(FdKnot.Items, ControlPointCount, Order, nkPiecewiseBezier);
  end;
end;

function TNurbsCurveNode.Point(const U: Single; const Tangent: PVector3): TVector3;
var
  ControlPointList: TVector3List;
  CurrentKnot: TDoubleList;
begin
  if ControlPoint is TCoordinateNode then
    ControlPointList := TCoordinateNode(ControlPoint).FdPoint.Items
  else
    raise EInvalidNurbs.Create('Only NURBS with Coordinate node are supported');

  if ControlPointList.Count = 0 then
    raise EInvalidNurbs.Create('Only NURBS with at least 1 controlPoint are supported');

  if Order < 2 then
    raise EInvalidNurbs.Create('NURBS order must be >= 2');

  { calculate correct CurrentKnot vector }
  CurrentKnot := TDoubleList.Create;
  CurrentKnot.Assign(FdKnot.Items);
  NurbsKnotIfNeeded(CurrentKnot, ControlPointList.Count, Order, nkEndpointUniform);

  Result := NurbsCurvePoint(ControlPointList,
    MapRange(U, 0, 1, CurrentKnot.First, CurrentKnot.Last),
    Order, CurrentKnot, FdWeight.Items, Tangent);

  FreeAndNil(CurrentKnot);
end;

{ TNurbsOrientationInterpolatorNode ------------------------------------------ }

constructor TNurbsOrientationInterpolatorNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  EventSet_Fraction.AddNotification({$ifdef FPC}@{$endif} EventSet_FractionReceive);
end;

function TNurbsOrientationInterpolatorNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := FdcontrolPoint.Enumerate(Func);
  if Result <> nil then Exit;
end;

procedure TNurbsOrientationInterpolatorNode.EventSet_FractionReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
var
  ControlPointList: TVector3List;
  Knot: TDoubleList;
  Tangent: TVector3;
  Orientation: TVector4;
begin
  if not EventValue_Changed.SendNeeded then Exit;

  { TODO: we should handle here all TAbstractCoordinateNode }
  if ControlPoint is TCoordinateNode then
    ControlPointList := TCoordinateNode(ControlPoint).FdPoint.Items
  else
    Exit;

  if ControlPointList.Count = 0 then Exit;

  if FdOrder.Value < 2 then
  begin
    WritelnWarning('VRML/X3D', 'NURBS order must be >= 2');
    Exit;
  end;

  { We can be sure now that
    - we have ControlPointList, non-nil, with at least 1 point.
    - we have Order >= 2 }

  { calculate correct Knot vector }
  Knot := TDoubleList.Create;
  Knot.Assign(FdKnot.Items);
  NurbsKnotIfNeeded(Knot, ControlPointList.Count, FdOrder.Value, nkEndpointUniform);

  NurbsCurvePoint(ControlPointList,
    (Value as TSFFloat).Value,
    FdOrder.Value, Knot, FdWeight.Items, @Tangent);

  FreeAndNil(Knot);

  { calculate Orientation from Tangent.
    For this, we treat Tangent like "camera direction", and we have to set
    "camera up" as anything orthogonal to direction. }
  Orientation := OrientationFromDirectionUp(Tangent, AnyOrthogonalVector(Tangent));

  EventValue_Changed.Send(Orientation, Time);
end;

{ Converting X3D NurbsPatchSurface and VRML 97 NurbsSurface to
  IndexedQuadSet. This is for Proxy methods implementation. }
procedure NurbsPatchSurfaceProxy(
  const QS: TIndexedQuadSetNode;
  const ControlPoint: TVector3List;
  const UTessellation, VTessellation, UDimension, VDimension, UOrder, VOrder: LongInt;
  const FieldUKnot, FieldVKnot, Weight: TDoubleList;
  const PossiblyUClosed, PossiblyVClosed, Solid, Ccw: boolean;
  const TexCoord: TX3DNode);

const
  { This has to be slightly larger than normal epsilon
    (test nurbs_dune_primitives.x3dv). }
  ClosedCheckEpsilon = 0.000001;

  procedure LogClosed(Value: boolean);
  begin
    if Value then
      WritelnLog('NURBS', 'Checking if NURBS surface is really closed (because it''s VRML 97 NurbsSurface or NurbsPatchSurface with .xClosed is TRUE): no, ignoring (otherwise would cause invalid geometry/normals)')
    else
      WritelnLog('NURBS', 'Checking if NURBS surface is really closed: yes');
  end;

  function CalculateUClosed: boolean;
  var
    J: Integer;
  begin
    Result := PossiblyUClosed;
    if Result then
    begin
      for J := 0 to VDimension - 1 do
        if not TVector3.Equals(
          ControlPoint.L[                 J * UDimension],
          ControlPoint.L[UDimension - 1 + J * UDimension], ClosedCheckEpsilon) then
        begin
          Result := false;
          Break;
        end;
      LogClosed(Result);
    end;
  end;

  function CalculateVClosed: boolean;
  var
    I: Integer;
  begin
    Result := PossiblyVClosed;
    if Result then
    begin
      for I := 0 to UDimension - 1 do
        if not TVector3.Equals(
          ControlPoint.L[I                                ],
          ControlPoint.L[I + (VDimension - 1) * UDimension], ClosedCheckEpsilon) then
        begin
          Result := false;
          Break;
        end;
      LogClosed(Result);
    end;
  end;

var
  UTess, VTess: Cardinal;
  UClosed, VClosed: boolean;

  function MakeIndex(I, J: Cardinal): Cardinal;
  begin
    if (I = UTess - 1) and UClosed then I := 0;
    if (J = VTess - 1) and VClosed then J := 0;
    Result := I + J * UTess;
  end;

var
  ResultCoord, ResultNormal: TVector3List;
  ResultIndex: TInt32List;
  I, J, NextIndex: Cardinal;
  UIncrease, VIncrease: Double;
  UKnot, VKnot: TDoubleList;
  ResultTexCoord: TVector2List;
  Normal: TVector3;
  NurbsCalculator: TNurbsSurfaceCalculator;
begin
  if ControlPoint.Count = 0 then Exit;

  if UDimension * VDimension <>
     Integer(ControlPoint.Count) then
  begin
    WritelnWarning('VRML/X3D', Format('Number of coordinates in Nurbs[Patch]Surface.controlPoint (%d) doesn''t match uDimension * vDimension (%d * %d = %d)',
      [ ControlPoint.Count,
        UDimension,  VDimension,
        UDimension * VDimension ]));
    Exit;
  end;

  if (UDimension < 0) or
     (VDimension < 0) then
  begin
    WritelnWarning('VRML/X3D', 'Nurbs[Patch]Surface.u/vDimension must be >= 0');
    Exit;
  end;

  if (UOrder < 2) or
     (VOrder < 2) then
  begin
    WritelnWarning('VRML/X3D', 'NURBS order must be >= 2');
    Exit;
  end;

  { We can be sure now that we have
    - correct ControlPoint, non-nil, with at least 1 point.
    - uDimension, vDimension match ControlPoint count, and are > 0.
    - we have Order >= 2.
  }

  { calculate actual XClosed values, using PossiblyXClosed values.

    For X3D:

    - The PossiblyXClosed come from xClosed fields. Still, we have to check
      them.

      Since we use xClosed fields not only to calculate normals,
      but to actually set the geometry indexes, we have to check them
      carefully and avoid using if look bad. Otherwise not only the normals,
      but the whole geometry would look bad when xClosed fields are wrongly
      set to TRUE.

      Moreover, X3D spec says "If the last control point is not identical
      with the first control point, the field is ignored."
      for X3DNurbsSurfaceGeometryNode. So it seems the implementation
      isn't supposed to "trust" xClosed = TRUE value anyway (that's also
      why no VRML warning is done about it, although we log it).
      Examples of such wrong xClosed = TRUE settings may be found
      even on web3d.org examples, see
      http://www.web3d.org/x3d/content/examples/NURBS/
      e.g. "Fred The Bunny" in X3d.)

    For VRML 97 NurbsSurface, these are always true, we have to always
    check this for VRML 97 NurbsSurface. }

  UClosed := CalculateUClosed;
  VClosed := CalculateVClosed;

  { calculate correct UKnot, VKnot vectors }
  UKnot := TDoubleList.Create;
  UKnot.Assign(FieldUKnot);
  NurbsKnotIfNeeded(UKnot, UDimension, UOrder, nkEndpointUniform);
  VKnot := TDoubleList.Create;
  VKnot.Assign(FieldVKnot);
  NurbsKnotIfNeeded(VKnot, VDimension, VOrder, nkEndpointUniform);

  { calculate tesselation: xTess, xIncrease }
  UTess := ActualTessellation(UTessellation, UDimension);
  VTess := ActualTessellation(VTessellation, VDimension);
  UIncrease := (UKnot.Last - UKnot.First) / (UTess - 1);
  VIncrease := (VKnot.Last - VKnot.First) / (VTess - 1);

  { make resulting Coordinate and Normal nodes }
  QS.FdCoord.Value := TCoordinateNode.Create('', QS.BaseUrl);
  ResultCoord := (QS.FdCoord.Value as TCoordinateNode).FdPoint.Items;
  ResultCoord.Count := UTess * VTess;

  QS.FdNormal.Value := TNormalNode.Create('', QS.BaseUrl);
  ResultNormal := (QS.FdNormal.Value as TNormalNode).FdVector.Items;
  ResultNormal.Count := UTess * VTess;

  NurbsCalculator := TNurbsSurfaceCalculator.Create(ControlPoint,
    UDimension, VDimension,
    UOrder, VOrder,
    UKnot, VKnot,
    Weight);
  try
    { calculate result Coordinate.point and Normal.vector field }
    for I := 0 to UTess - 1 do
      for J := 0 to VTess - 1 do
      begin
        ResultCoord.L[I + J * UTess] :=
          NurbsCalculator.GetPoint(
            UKnot.First + UIncrease * I,
            VKnot.First + VIncrease * J,
            @Normal);
        if Ccw then
          ResultNormal.L[I + J * UTess] := Normal else
          ResultNormal.L[I + J * UTess] := -Normal;
      end;
  finally FreeAndNil(NurbsCalculator) end;

  { For now, don't use normal values --- they are nonsense at the edges
    of the surface? See nurbs_curve_interpolators.x3dv for demo. }
  QS.FdNormal.Value := nil;

  { calculate index field }
  ResultIndex := QS.FdIndex.Items;
  ResultIndex.Count := 4 * (UTess - 1) * (VTess - 1);
  NextIndex := 0;
  for I := 1 to UTess - 1 do
    for J := 1 to VTess - 1 do
    begin
      { This order (important for solid = TRUE values) is compatible
        with white dune and octagaplayer. }
      ResultIndex.L[NextIndex] := MakeIndex(I  , J  ); Inc(NextIndex);
      ResultIndex.L[NextIndex] := MakeIndex(I-1, J  ); Inc(NextIndex);
      ResultIndex.L[NextIndex] := MakeIndex(I-1, J-1); Inc(NextIndex);
      ResultIndex.L[NextIndex] := MakeIndex(I  , J-1); Inc(NextIndex);
    end;
  Assert(NextIndex = Cardinal(ResultIndex.Count));

  QS.FdSolid.Value := Solid;
  QS.FdCcw.Value := Ccw;

  { calculate texCoord field }
  QS.FdTexCoord.Value := TexCoord;
  if QS.FdTexCoord.Value = nil then
  begin
    { X3D spec says "By default, texture coordinates in the unit square
      (or cube for 3D coordinates) are generated automatically
      from the parametric subdivision.". I think this means that tex coords
      S/T = 0..1 range from starting to ending knot. }
    QS.FdTexCoord.Value := TTextureCoordinateNode.Create('', QS.BaseUrl);
    ResultTexCoord := (QS.FdTexCoord.Value as TTextureCoordinateNode).FdPoint.Items;
    ResultTexCoord.Count := UTess * VTess;
    for I := 0 to UTess - 1 do
      for J := 0 to VTess - 1 do
        ResultTexCoord.L[I + J * UTess] := Vector2(
          I / (UTess - 1),
          J / (VTess - 1));
  end;

  FreeAndNil(UKnot);
  FreeAndNil(VKnot);
end;

function TAbstractNurbsSurfaceGeometryNode.Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode;
var
  ControlPointList: TVector3List;
begin
  Result := TIndexedQuadSetNode.Create(X3DName, BaseUrl);
  try
    { TODO: we should handle here all TAbstractCoordinateNode }
    if ControlPoint <> nil then
    begin
      ControlPointList := TCoordinateNode(ControlPoint).FdPoint.Items;
      NurbsPatchSurfaceProxy(TIndexedQuadSetNode(Result),
        ControlPointList,
        FdUTessellation.Value,
        FdVTessellation.Value,
        FdUDimension.Value,
        FdVDimension.Value,
        FdUOrder.Value,
        FdVOrder.Value,
        FdUKnot.Items,
        FdVKnot.Items,
        FdWeight.Items,
        FdUClosed.Value,
        FdVClosed.Value,
        FdSolid.Value,
        true { ccw always true for X3D NurbsPatchSurface },
        FdTexCoord.Value);
    end;

    { Note: In case of null / invalid / unhandled ControlPoint,
      we still return valid (just empty) IndexedQuadSet node. }
  except FreeAndNil(Result); raise end;
end;

function TAbstractNurbsSurfaceGeometryNode.CoordField: TSFNode;
begin
  Result := FdControlPoint;
end;

{ Although our BoundingBox and LocalBoundingBox do not rely on InternalCoord()
  override, it is still necessary to react to animation of Coordinate node
  points, see
  http://www.web3d.org/x3d/content/examples/Basic/NURBS/_pages/page01.html }
function TAbstractNurbsSurfaceGeometryNode.InternalCoord(State: TX3DGraphTraverseState;
  out ACoord: TMFVec3f): boolean;
begin
  Result := true;
  if FdControlPoint.Value is TCoordinateNode then // also checks that FdControlPoint.Value <> nil
    ACoord := TCoordinateNode(FdControlPoint.Value).FdPoint
  else
    ACoord := nil;
end;

{ We cannot simply override InternalCoord() and let bounding box be calculated
  based on it. It would fail for curves with weigths. }
function TAbstractNurbsSurfaceGeometryNode.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  if FdControlPoint.Value is TCoordinateNode then // also checks that FdControlPoint.Value <> nil
    Result := NurbsBoundingBox(TCoordinateNode(FdControlPoint.Value).FdPoint.Items, FdWeight.Items)
  else
    Result := TBox3D.Empty;
end;

{ We cannot simply override InternalCoord() and let bounding box be calculated
  based on it. It would fail for curves with weigths. }
function TAbstractNurbsSurfaceGeometryNode.BoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  if FdControlPoint.Value is TCoordinateNode then // also checks that FdControlPoint.Value <> nil
    Result := NurbsBoundingBox(TCoordinateNode(FdControlPoint.Value).FdPoint.Items, FdWeight.Items, State.Transformation.Transform)
  else
    Result := TBox3D.Empty;
end;

function TAbstractNurbsSurfaceGeometryNode.TrianglesCount(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
var
  UTess, VTess: Cardinal;
begin
  if (FdUDimension.Value > 0) and
     (FdVDimension.Value > 0) then
  begin
    UTess := ActualTessellation(FdUTessellation.Value, FdUDimension.Value);
    VTess := ActualTessellation(FdVTessellation.Value, FdVDimension.Value);
    Result := (UTess - 1) * (VTess - 1) * 2;
  end else
    Result := 0;
end;

function TAbstractNurbsSurfaceGeometryNode.SolidField: TSFBool;
begin
  Result := FdSolid;
end;

function TAbstractNurbsSurfaceGeometryNode.Point(const U, V: Single; const OutputNormal: PVector3 = nil): TVector3;
var
  ControlPointList: TVector3List;
  UKnot, VKnot: TDoubleList;
begin
  if ControlPoint is TCoordinateNode then
    ControlPointList := TCoordinateNode(ControlPoint).FdPoint.Items
  else
    raise EInvalidNurbs.Create('Only NURBS with Coordinate node are supported');

  if ControlPointList.Count = 0 then
    raise EInvalidNurbs.Create('Only NURBS with at least 1 controlPoint are supported');

  if FdUDimension.Value * FdVDimension.Value <>
     Integer(ControlPointList.Count) then
    raise EInvalidNurbs.CreateFmt('Number of coordinates in NURBS surface controlPoint (%d) doesn''t match uDimension * vDimension (%d * %d = %d)', [
      ControlPointList.Count,
      FdUDimension.Value,
      FdVDimension.Value,
      FdUDimension.Value * FdVDimension.Value
    ]);

  if (FdUDimension.Value < 0) or
     (FdVDimension.Value < 0) then
    raise EInvalidNurbs.Create('NURBS surface u/vDimension must be >= 0');

  if (FdUOrder.Value < 2) or
     (FdVOrder.Value < 2) then
    raise EInvalidNurbs.Create('NURBS surface u/vOrder must be >= 2');

  { We can be sure now that we have
    - correct ControlPointList, non-nil, with at least 1 point.
    - uDimension, vDimension match ControlPointList count, and are > 0.
    - we have Order >= 2.
  }

  { calculate correct UKnot, VKnot vectors }
  UKnot := TDoubleList.Create;
  UKnot.Assign(FdUKnot.Items);
  NurbsKnotIfNeeded(UKnot, FdUDimension.Value, FdUOrder.Value, nkEndpointUniform);
  VKnot := TDoubleList.Create;
  VKnot.Assign(FdVKnot.Items);
  NurbsKnotIfNeeded(VKnot, FdVDimension.Value, FdVOrder.Value, nkEndpointUniform);

  Result :=
    NurbsSurfacePoint(ControlPointList,
      FdUDimension.Value,
      FdVDimension.Value,
      U,
      V,
      FdUOrder.Value,
      FdVOrder.Value,
      UKnot,
      VKnot,
      FdWeight.Items,
      OutputNormal);

  FreeAndNil(UKnot);
  FreeAndNil(VKnot);
end;

constructor TNurbsPositionInterpolatorNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  EventSet_Fraction.AddNotification({$ifdef FPC}@{$endif} EventSet_FractionReceive);
end;

class function TNurbsPositionInterpolatorNode.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major >= 3;
end;

function TNurbsPositionInterpolatorNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := FdcontrolPoint.Enumerate(Func);
  if Result <> nil then Exit;
end;

procedure TNurbsPositionInterpolatorNode.EventSet_FractionReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
var
  ControlPointList: TVector3List;
  Knot: TDoubleList;
  OutputValue: TVector3;
begin
  if not EventValue_Changed.SendNeeded then Exit;

  if ControlPoint is TCoordinateNode then
    ControlPointList := TCoordinateNode(ControlPoint).FdPoint.Items
  else
    Exit;

  if ControlPointList.Count = 0 then Exit;

  if FdOrder.Value < 2 then
  begin
    WritelnWarning('VRML/X3D', 'NURBS order must be >= 2');
    Exit;
  end;

  { We can be sure now that
    - we have ControlPointList, non-nil, with at least 1 point.
    - we have Order >= 2 }

  { calculate correct Knot vector }
  Knot := TDoubleList.Create;
  Knot.Assign(FdKnot.Items);
  NurbsKnotIfNeeded(Knot, ControlPointList.Count, FdOrder.Value, nkEndpointUniform);

  OutputValue := NurbsCurvePoint(ControlPointList, (Value as TSFFloat).Value,
    FdOrder.Value, Knot, FdWeight.Items, nil);

  FreeAndNil(Knot);

  EventValue_Changed.Send(OutputValue, Time);
end;

{ TNurbsSurfaceInterpolatorNode ---------------------------------------------- }

constructor TNurbsSurfaceInterpolatorNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  EventSet_Fraction.AddNotification({$ifdef FPC}@{$endif} EventSet_FractionReceive);
end;

function TNurbsSurfaceInterpolatorNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := FdcontrolPoint.Enumerate(Func);
  if Result <> nil then Exit;
end;

procedure TNurbsSurfaceInterpolatorNode.EventSet_FractionReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
var
  ControlPointList: TVector3List;
  OutputNormal: TVector3;
  OutputPosition: TVector3;
  UKnot, VKnot: TDoubleList;
begin
  if not (EventPosition_Changed.SendNeeded or
          EventNormal_Changed.SendNeeded) then Exit;

  { TODO: we should handle here all TAbstractCoordinateNode }
  if ControlPoint is TCoordinateNode then
    ControlPointList := TCoordinateNode(ControlPoint).FdPoint.Items
  else
    Exit;

  if ControlPointList.Count = 0 then Exit;

  if FdUDimension.Value * FdVDimension.Value <>
     Integer(ControlPointList.Count) then
  begin
    WritelnWarning('VRML/X3D', Format('Number of coordinates in NurbsSurfaceInterpolator.controlPoint (%d) doesn''t match uDimension * vDimension (%d * %d = %d)',
      [ ControlPointList.Count,
        FdUDimension.Value,  FdVDimension.Value,
        FdUDimension.Value * FdVDimension.Value ]));
    Exit;
  end;

  if (FdUDimension.Value < 0) or
     (FdVDimension.Value < 0) then
  begin
    WritelnWarning('VRML/X3D', 'NurbsSurfaceInterpolator.u/vDimension must be >= 0');
    Exit;
  end;

  if (FdUOrder.Value < 2) or
     (FdVOrder.Value < 2) then
  begin
    WritelnWarning('VRML/X3D', 'NURBS order must be >= 2');
    Exit;
  end;

  { We can be sure now that we have
    - correct ControlPointList, non-nil, with at least 1 point.
    - uDimension, vDimension match ControlPointList count, and are > 0.
    - we have Order >= 2.
  }

  { calculate correct UKnot, VKnot vectors }
  UKnot := TDoubleList.Create;
  UKnot.Assign(FdUKnot.Items);
  NurbsKnotIfNeeded(UKnot, FdUDimension.Value, FdUOrder.Value, nkEndpointUniform);
  VKnot := TDoubleList.Create;
  VKnot.Assign(FdVKnot.Items);
  NurbsKnotIfNeeded(VKnot, FdVDimension.Value, FdVOrder.Value, nkEndpointUniform);

  OutputPosition :=
    NurbsSurfacePoint(ControlPointList,
      FdUDimension.Value,
      FdVDimension.Value,
      (Value as TSFVec2f).Value[0],
      (Value as TSFVec2f).Value[1],
      FdUOrder.Value,
      FdVOrder.Value,
      UKnot,
      VKnot,
      FdWeight.Items,
      @OutputNormal);

  FreeAndNil(UKnot);
  FreeAndNil(VKnot);

  EventPosition_Changed.Send(OutputPosition, Time);
  EventNormal_Changed.Send(OutputNormal, Time);
end;

function TNurbsSweptSurfaceNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := FdcrossSectionCurve.Enumerate(Func);
  if Result <> nil then Exit;

  Result := FdtrajectoryCurve.Enumerate(Func);
  if Result <> nil then Exit;
end;

function TNurbsSweptSurfaceNode.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  { Rendering of TNurbsSweptSurfaceNode not implemented. }
  Result := TBox3D.Empty;
end;

function TNurbsSweptSurfaceNode.VerticesCount(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  { Rendering of TNurbsSweptSurfaceNode not implemented. }
  Result := 0;
end;

function TNurbsSweptSurfaceNode.TrianglesCount(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  { Rendering of TNurbsSweptSurfaceNode not implemented. }
  Result := 0;
end;

function TNurbsSweptSurfaceNode.SolidField: TSFBool;
begin
  Result := FdSolid;
end;

function TNurbsSwungSurfaceNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := FdprofileCurve.Enumerate(Func);
  if Result <> nil then Exit;

  Result := FdtrajectoryCurve.Enumerate(Func);
  if Result <> nil then Exit;
end;

function TNurbsSwungSurfaceNode.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  { Rendering of TNurbsSwungSurfaceNode not implemented. }
  Result := TBox3D.Empty;
end;

function TNurbsSwungSurfaceNode.VerticesCount(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  { Rendering of TNurbsSwungSurfaceNode not implemented. }
  Result := 0;
end;

function TNurbsSwungSurfaceNode.TrianglesCount(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  { Rendering of TNurbsSwungSurfaceNode not implemented. }
  Result := 0;
end;

function TNurbsSwungSurfaceNode.SolidField: TSFBool;
begin
  Result := FdSolid;
end;

function TNurbsTrimmedSurfaceNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := FdtrimmingContour.Enumerate(Func);
  if Result <> nil then Exit;
end;

function TNurbsTrimmedSurfaceNode.Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode;
begin
  Result := inherited;
  if FdTrimmingContour.Count <> 0 then
    WritelnWarning('VRML/X3D', 'NurbsTrimmedSurface.trimmingContour is not implemented yet (that is, NurbsTrimmedSurface is rendered just like NurbsPatchSurface)');
end;

procedure RegisterNURBSNodes;
begin
  NodesManager.RegisterNodeClasses([
    TContour2DNode,
    TContourPolyline2DNode,
    TCoordinateDoubleNode,
    TNurbsCurveNode,
    TNurbsCurve2DNode,
    TNurbsOrientationInterpolatorNode,
    TNurbsPatchSurfaceNode,
    TNurbsPositionInterpolatorNode,
    TNurbsSetNode,
    TNurbsSurfaceInterpolatorNode,
    TNurbsSweptSurfaceNode,
    TNurbsSwungSurfaceNode,
    TNurbsTextureCoordinateNode,
    TNurbsTrimmedSurfaceNode
  ]);
end;

{$endif read_implementation}
