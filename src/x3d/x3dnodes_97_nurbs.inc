{
  Copyright 2002-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ NURBS nodes from VRML 97 Amendment 1 specification.

  Note: Some of VRML 97 NURBS nodes are the same (or at least
  backward compatible) as X3D NURBS nodes, these are defined and handled
  only in x3d_nurbs.inc. } { }
{$ifdef read_interface}

  { Free form deformation on a set of 3D coordinates by using a NURBS volume.
    @bold(Not implemented.) }
  TCoordinateDeformerNode = class(TAbstractGroupingNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    { Event in } { }
    private FEventaddChildren: TMFNodeEvent;
    public property EventaddChildren: TMFNodeEvent read FEventaddChildren;

    { Event in } { }
    private FEventremoveChildren: TMFNodeEvent;
    public property EventremoveChildren: TMFNodeEvent read FEventremoveChildren;

    private FFdchildren: TMFNode;
    public property Fdchildren: TMFNode read FFdchildren;

    private FFdcontrolPoint: TMFVec3f;
    public property FdcontrolPoint: TMFVec3f read FFdcontrolPoint;

    private FFdinputCoord: TMFNode;
    public property FdinputCoord: TMFNode read FFdinputCoord;

    private FFdinputTransform: TMFNode;
    public property FdinputTransform: TMFNode read FFdinputTransform;

    private FFdoutputCoord: TMFNode;
    public property FdoutputCoord: TMFNode read FFdoutputCoord;

    private FFdweight: TMFFloat;
    public property Fdweight: TMFFloat read FFdweight;

    private FFdbboxCenter: TSFVec3f;
    public property FdbboxCenter: TSFVec3f read FFdbboxCenter;

    private FFdbboxSize: TSFVec3f;
    public property FdbboxSize: TSFVec3f read FFdbboxSize;

    private FFduDimension: TSFInt32;
    public property FduDimension: TSFInt32 read FFduDimension;

    private FFduKnot: TMFFloat;
    public property FduKnot: TMFFloat read FFduKnot;

    private FFduOrder: TSFInt32;
    public property FduOrder: TSFInt32 read FFduOrder;

    private FFdvDimension: TSFInt32;
    public property FdvDimension: TSFInt32 read FFdvDimension;

    private FFdvKnot: TMFFloat;
    public property FdvKnot: TMFFloat read FFdvKnot;

    private FFdvOrder: TSFInt32;
    public property FdvOrder: TSFInt32 read FFdvOrder;

    private FFdwDimension: TSFInt32;
    public property FdwDimension: TSFInt32 read FFdwDimension;

    private FFdwKnot: TMFFloat;
    public property FdwKnot: TMFFloat read FFdwKnot;

    private FFdwOrder: TSFInt32;
    public property FdwOrder: TSFInt32 read FFdwOrder;

    class function URNMatching(const URN: string): boolean; override;
  end;

  { Visible NURBS curve in 3D, in VRML 2.0.
    @bold(Not implemented.) }
  TNurbsCurveNode_2 = class(TAbstractX3DGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    private FFdcontrolPoint: TMFVec3f;
    public property FdcontrolPoint: TMFVec3f read FFdcontrolPoint;

    private FFdweight: TMFFloat;
    public property Fdweight: TMFFloat read FFdweight;

    private FFdtessellation: TSFInt32;
    public property Fdtessellation: TSFInt32 read FFdtessellation;

    private FFdknot: TMFFloat;
    public property Fdknot: TMFFloat read FFdknot;

    private FFdorder: TSFInt32;
    public property Fdorder: TSFInt32 read FFdorder;

    class function URNMatching(const URN: string): boolean; override;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean; override;

    function Proxy(var State: TX3DGraphTraverseState;
      const OverTriangulate: boolean): TAbstractGeometryNode; override;
    function ProxyUsesOverTriangulate: boolean; override;
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function BoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
  end;

  { Groups a set of NURBS surface nodes to a common group
    for rendering purposes, to ensure a common tesselation within the group,
    for VRML 2.0. }
  TNurbsGroupNode = class(TAbstractX3DGroupingNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    private FFdtessellationScale: TSFFloat;
    public property FdtessellationScale: TSFFloat read FFdtessellationScale;

    class function URNMatching(const URN: string): boolean; override;
  end;

  { Interpolate (animate) positions along the 3D NURBS curve. }
  TNurbsPositionInterpolatorNode_2 = class(TAbstractChildNode)
  private
    procedure EventSet_FractionReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    { Event in } { }
    private FEventset_fraction: TSFFloatEvent;
    public property Eventset_fraction: TSFFloatEvent read FEventset_fraction;

    private FFddimension: TSFInt32;
    public property Fddimension: TSFInt32 read FFddimension;

    private FFdkeyValue: TMFVec3f;
    public property FdkeyValue: TMFVec3f read FFdkeyValue;

    private FFdkeyWeight: TMFFloat;
    public property FdkeyWeight: TMFFloat read FFdkeyWeight;

    private FFdknot: TMFFloat;
    public property Fdknot: TMFFloat read FFdknot;

    private FFdorder: TSFInt32;
    public property Fdorder: TSFInt32 read FFdorder;

    { Event out } { }
    private FEventvalue_changed: TSFVec3fEvent;
    public property Eventvalue_changed: TSFVec3fEvent read FEventvalue_changed;

    class function URNMatching(const URN: string): boolean; override;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean; override;
  end;

  { Visible NURBS 3D surface, for VRML 2.0. }
  TNurbsSurfaceNode = class(TAbstractX3DGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    private FFdcontrolPoint: TMFVec3f;
    public property FdcontrolPoint: TMFVec3f read FFdcontrolPoint;

    private FFdtexCoord: TSFNode;
    public property FdtexCoord: TSFNode read FFdtexCoord;

    private FFduTessellation: TSFInt32;
    public property FduTessellation: TSFInt32 read FFduTessellation;

    private FFdvTessellation: TSFInt32;
    public property FdvTessellation: TSFInt32 read FFdvTessellation;

    private FFdweight: TMFFloat;
    public property Fdweight: TMFFloat read FFdweight;

    private FFdccw: TSFBool;
    public property Fdccw: TSFBool read FFdccw;

    private FFdsolid: TSFBool;
    public property Fdsolid: TSFBool read FFdsolid;

    private FFduDimension: TSFInt32;
    public property FduDimension: TSFInt32 read FFduDimension;

    private FFduKnot: TMFFloat;
    public property FduKnot: TMFFloat read FFduKnot;

    private FFduOrder: TSFInt32;
    public property FduOrder: TSFInt32 read FFduOrder;

    private FFdvDimension: TSFInt32;
    public property FdvDimension: TSFInt32 read FFdvDimension;

    private FFdvKnot: TMFFloat;
    public property FdvKnot: TMFFloat read FFdvKnot;

    private FFdvOrder: TSFInt32;
    public property FdvOrder: TSFInt32 read FFdvOrder;

    class function URNMatching(const URN: string): boolean; override;

    function Proxy(var State: TX3DGraphTraverseState;
      const OverTriangulate: boolean): TAbstractGeometryNode; override;
    function ProxyUsesOverTriangulate: boolean; override;
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function BoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function SolidField: TSFBool; override;
  end;

  { NURBS surface existing in the parametric domain of its surface host specifying
    the mapping of the texture onto the surface, for VRML 2.0.
    @bold(Not implemented.) }
  TNurbsTextureSurfaceNode = class(TX3DNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    private FFdcontrolPoint: TMFVec2f;
    public property FdcontrolPoint: TMFVec2f read FFdcontrolPoint;

    private FFdweight: TMFFloat;
    public property Fdweight: TMFFloat read FFdweight;

    private FFduDimension: TSFInt32;
    public property FduDimension: TSFInt32 read FFduDimension;

    private FFduKnot: TMFFloat;
    public property FduKnot: TMFFloat read FFduKnot;

    private FFduOrder: TSFInt32;
    public property FduOrder: TSFInt32 read FFduOrder;

    private FFdvDimension: TSFInt32;
    public property FdvDimension: TSFInt32 read FFdvDimension;

    private FFdvKnot: TMFFloat;
    public property FdvKnot: TMFFloat read FFdvKnot;

    private FFdvOrder: TSFInt32;
    public property FdvOrder: TSFInt32 read FFdvOrder;

    class function URNMatching(const URN: string): boolean; override;
  end;

  { NURBS surface that is trimmed by a set of trimming loops.
    @bold(Rendering of this node is not implemented yet.) }
  TTrimmedSurfaceNode = class(TAbstractX3DGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    { Event in } { }
    private FEventaddTrimmingContour: TMFNodeEvent;
    public property EventaddTrimmingContour: TMFNodeEvent read FEventaddTrimmingContour;

    { Event in } { }
    private FEventremoveTrimmingContour: TMFNodeEvent;
    public property EventremoveTrimmingContour: TMFNodeEvent read FEventremoveTrimmingContour;

    private FFdtrimmingContour: TMFNode;
    public property FdtrimmingContour: TMFNode read FFdtrimmingContour;

    private FFdsurface: TSFNode;
    public property Fdsurface: TSFNode read FFdsurface;

    class function URNMatching(const URN: string): boolean; override;

    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
  end;

  { 2D curve composed from NURBS curves and straight segments, for VRML 2.0.
    @bold(Not implemented yet.) }
  TContour2DNode_2 = class(TAbstractX3DGeometryNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;
    class function ForVRMLVersion(const Version: TX3DVersion): boolean; override;

    { Event in } { }
    private FEventAddChildren: TMFNodeEvent;
    public property EventAddChildren: TMFNodeEvent read FEventAddChildren;

    { Event in } { }
    private FEventRemoveChildren: TMFNodeEvent;
    public property EventRemoveChildren: TMFNodeEvent read FEventRemoveChildren;

    private FFdChildren: TMFNode;
    public property FdChildren: TMFNode read FFdChildren;

    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
  end;
{$endif read_interface}

{$ifdef read_implementation}
procedure TCoordinateDeformerNode.CreateNode;
begin
  inherited;

  FEventaddChildren := TMFNodeEvent.Create(Self, 'addChildren', true);
  AddEvent(FEventaddChildren);

  FEventremoveChildren := TMFNodeEvent.Create(Self, 'removeChildren', true);
  AddEvent(FEventremoveChildren);

  FFdchildren := TMFNode.Create(Self, 'children', IAbstractChildNode);
  AddField(FFdchildren);

  FFdcontrolPoint := TMFVec3f.Create(Self, 'controlPoint', []);
  AddField(FFdcontrolPoint);

  FFdinputCoord := TMFNode.Create(Self, 'inputCoord', [TCoordinateNode]);
  AddField(FFdinputCoord);

  FFdinputTransform := TMFNode.Create(Self, 'inputTransform', [TTransformNode]);
  AddField(FFdinputTransform);

  FFdoutputCoord := TMFNode.Create(Self, 'outputCoord', [TCoordinateNode]);
  AddField(FFdoutputCoord);

  FFdweight := TMFFloat.Create(Self, 'weight', []);
  AddField(FFdweight);

  FFdbboxCenter := TSFVec3f.Create(Self, 'bboxCenter', ZeroVector3Single);
  FFdbboxCenter.Exposed := false;
  AddField(FFdbboxCenter);

  FFdbboxSize := TSFVec3f.Create(Self, 'bboxSize', Vector3Single(-1, -1, -1));
  FFdbboxSize.Exposed := false;
  AddField(FFdbboxSize);

  FFduDimension := TSFInt32.Create(Self, 'uDimension', 0);
  FFduDimension.Exposed := false;
  AddField(FFduDimension);

  FFduKnot := TMFFloat.Create(Self, 'uKnot', []);
  FFduKnot.Exposed := false;
  AddField(FFduKnot);

  FFduOrder := TSFInt32.Create(Self, 'uOrder', 2);
  FFduOrder.Exposed := false;
  AddField(FFduOrder);

  FFdvDimension := TSFInt32.Create(Self, 'vDimension', 0);
  FFdvDimension.Exposed := false;
  AddField(FFdvDimension);

  FFdvKnot := TMFFloat.Create(Self, 'vKnot', []);
  FFdvKnot.Exposed := false;
  AddField(FFdvKnot);

  FFdvOrder := TSFInt32.Create(Self, 'vOrder', 2);
  FFdvOrder.Exposed := false;
  AddField(FFdvOrder);

  FFdwDimension := TSFInt32.Create(Self, 'wDimension', 0);
  FFdwDimension.Exposed := false;
  AddField(FFdwDimension);

  FFdwKnot := TMFFloat.Create(Self, 'wKnot', []);
  FFdwKnot.Exposed := false;
  AddField(FFdwKnot);

  FFdwOrder := TSFInt32.Create(Self, 'wOrder', 2);
  FFdwOrder.Exposed := false;
  AddField(FFdwOrder);

  DefaultContainerField := 'children';
end;

class function TCoordinateDeformerNode.ClassX3DType: string;
begin
  Result := 'CoordinateDeformer';
end;

class function TCoordinateDeformerNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType);
end;

procedure TNurbsCurveNode_2.CreateNode;
begin
  inherited;

  FFdcontrolPoint := TMFVec3f.Create(Self, 'controlPoint', []);
   FdcontrolPoint.ChangesAlways := [chGeometry];
  AddField(FFdcontrolPoint);

  FFdweight := TMFFloat.Create(Self, 'weight', []);
   Fdweight.ChangesAlways := [chGeometry];
  AddField(FFdweight);

  FFdtessellation := TSFInt32.Create(Self, 'tessellation', 0);
   Fdtessellation.ChangesAlways := [chGeometry];
  AddField(FFdtessellation);

  FFdknot := TMFFloat.Create(Self, 'knot', []);
   Fdknot.Exposed := false;
   Fdknot.ChangesAlways := [chGeometry];
  AddField(FFdknot);

  FFdorder := TSFInt32.Create(Self, 'order', 3);
   Fdorder.Exposed := false;
   Fdorder.ChangesAlways := [chGeometry];
  AddField(FFdorder);
end;

class function TNurbsCurveNode_2.ClassX3DType: string;
begin
  Result := 'NurbsCurve';
end;

class function TNurbsCurveNode_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType);
end;

class function TNurbsCurveNode_2.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major = 2;
end;

function TNurbsCurveNode_2.Proxy(var State: TX3DGraphTraverseState;
  const OverTriangulate: boolean): TAbstractGeometryNode;
var
  KnotDouble, WeightDouble: TDoubleList;
begin
  Result := TLineSetNode.Create(X3DName, BaseUrl);
  try
    KnotDouble := FdKnot.Items.ToDouble;
    WeightDouble := FdWeight.Items.ToDouble;
    try
      NurbsCurveProxy(TLineSetNode(Result), FdControlPoint.Items,
        FdTessellation.Value, FdOrder.Value, KnotDouble, WeightDouble);
    finally
      FreeAndNil(KnotDouble);
      FreeAndNil(WeightDouble);
    end;
  except FreeAndNil(Result); raise end;
end;

function TNurbsCurveNode_2.ProxyUsesOverTriangulate: boolean;
begin
  Result := false;
end;

{ We cannot simply override Coord() and let bounding box be calculated
  based on it. It would fail for curves with weigths. }
function TNurbsCurveNode_2.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  Result := NurbsBoundingBox(FdControlPoint.Items, FdWeight.Items);
end;

{ We cannot simply override Coord() and let bounding box be calculated
  based on it. It would fail for curves with weigths. }
function TNurbsCurveNode_2.BoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  Result := NurbsBoundingBox(FdControlPoint.Items, FdWeight.Items, State.Transform);
end;

function TNurbsCurveNode_2.TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  Result := 0;
end;

procedure TNurbsGroupNode.CreateNode;
begin
  inherited;

  FFdtessellationScale := TSFFloat.Create(Self, 'tessellationScale', 1.0);
  AddField(FFdtessellationScale);
end;

class function TNurbsGroupNode.ClassX3DType: string;
begin
  Result := 'NurbsGroup';
end;

class function TNurbsGroupNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType);
end;

procedure TNurbsPositionInterpolatorNode_2.CreateNode;
begin
  inherited;

  FEventset_fraction := TSFFloatEvent.Create(Self, 'set_fraction', true);
  AddEvent(FEventset_fraction);

  FFddimension := TSFInt32.Create(Self, 'dimension', 0);
  AddField(FFddimension);

  FFdkeyValue := TMFVec3f.Create(Self, 'keyValue', []);
  AddField(FFdkeyValue);

  FFdkeyWeight := TMFFloat.Create(Self, 'keyWeight', []);
  AddField(FFdkeyWeight);

  FFdknot := TMFFloat.Create(Self, 'knot', []);
  AddField(FFdknot);

  FFdorder := TSFInt32.Create(Self, 'order', 4);
  AddField(FFdorder);

  FEventvalue_changed := TSFVec3fEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventvalue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.OnReceive.Add(@EventSet_FractionReceive);
end;

class function TNurbsPositionInterpolatorNode_2.ClassX3DType: string;
begin
  Result := 'NurbsPositionInterpolator';
end;

class function TNurbsPositionInterpolatorNode_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType);
end;

class function TNurbsPositionInterpolatorNode_2.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major = 2;
end;

procedure TNurbsPositionInterpolatorNode_2.EventSet_FractionReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
var
  Knot, KeyWeight: TDoubleList;
  OutputValue: TVector3Single;
begin
  if not EventValue_Changed.SendNeeded then Exit;

  if FdKeyValue.Count = 0 then Exit;

  if FdOrder.Value < 2 then
  begin
    WritelnWarning('VRML/X3D', 'NURBS order must be >= 2');
    Exit;
  end;

  { We can be sure now that
    - we have ControlPoint (in KeyValue), non-nil, with at least 1 point.
    - we have Order >= 2 }

  { calculate correct Knot vector }
  Knot := TDoubleList.Create;
  Knot.Assign(FdKnot.Items);
  NurbsKnotIfNeeded(Knot, FdKeyValue.Count, FdOrder.Value, nkEndpointUniform);

  KeyWeight := FdKeyWeight.Items.ToDouble;

  OutputValue := NurbsCurvePoint(FdKeyValue.Items, (Value as TSFFloat).Value,
    FdOrder.Value, Knot, KeyWeight, nil);

  FreeAndNil(KeyWeight);
  FreeAndNil(Knot);

  EventValue_Changed.Send(OutputValue, Time);
end;

procedure TNurbsSurfaceNode.CreateNode;
begin
  inherited;

  FFdcontrolPoint := TMFVec3f.Create(Self, 'controlPoint', []);
   FdcontrolPoint.ChangesAlways := [chGeometry];
  AddField(FFdcontrolPoint);

  FFdtexCoord := TSFNode.Create(Self, 'texCoord', [TTextureCoordinateNode, TNurbsTextureSurfaceNode]);
   FdtexCoord.ChangesAlways := [chGeometry];
  AddField(FFdtexCoord);

  FFduTessellation := TSFInt32.Create(Self, 'uTessellation', 0);
   FduTessellation.ChangesAlways := [chGeometry];
  AddField(FFduTessellation);

  FFdvTessellation := TSFInt32.Create(Self, 'vTessellation', 0);
   FdvTessellation.ChangesAlways := [chGeometry];
  AddField(FFdvTessellation);

  FFdweight := TMFFloat.Create(Self, 'weight', []);
   Fdweight.ChangesAlways := [chGeometry];
  AddField(FFdweight);

  FFdccw := TSFBool.Create(Self, 'ccw', TRUE);
   Fdccw.Exposed := false;
   Fdccw.ChangesAlways := [chGeometry];
  AddField(FFdccw);

  FFdsolid := TSFBool.Create(Self, 'solid', TRUE);
   Fdsolid.Exposed := false;
   Fdsolid.ChangesAlways := [chGeometry];
  AddField(FFdsolid);

  FFduDimension := TSFInt32.Create(Self, 'uDimension', 0);
   FduDimension.Exposed := false;
   FduDimension.ChangesAlways := [chGeometry];
  AddField(FFduDimension);

  FFduKnot := TMFFloat.Create(Self, 'uKnot', []);
   FduKnot.Exposed := false;
   FduKnot.ChangesAlways := [chGeometry];
  AddField(FFduKnot);

  FFduOrder := TSFInt32.Create(Self, 'uOrder', 3);
   FduOrder.Exposed := false;
   FduOrder.ChangesAlways := [chGeometry];
  AddField(FFduOrder);

  FFdvDimension := TSFInt32.Create(Self, 'vDimension', 0);
   FdvDimension.Exposed := false;
   FdvDimension.ChangesAlways := [chGeometry];
  AddField(FFdvDimension);

  FFdvKnot := TMFFloat.Create(Self, 'vKnot', []);
   FdvKnot.Exposed := false;
   FdvKnot.ChangesAlways := [chGeometry];
  AddField(FFdvKnot);

  FFdvOrder := TSFInt32.Create(Self, 'vOrder', 3);
   FdvOrder.Exposed := false;
   FdvOrder.ChangesAlways := [chGeometry];
  AddField(FFdvOrder);
end;

class function TNurbsSurfaceNode.ClassX3DType: string;
begin
  Result := 'NurbsSurface';
end;

class function TNurbsSurfaceNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType);
end;

function TNurbsSurfaceNode.Proxy(var State: TX3DGraphTraverseState;
  const OverTriangulate: boolean): TAbstractGeometryNode;
var
  UKnotDouble, VKnotDouble, WeightDouble: TDoubleList;
begin
  Result := TIndexedQuadSetNode.Create(X3DName, BaseUrl);
  try
    { For NurbsPatchSurfaceProxy (and underlying NurbsSurfacePoint)
      we want knots and weights to be double-precision.
      Implementing another version of
      NurbsPatchSurfaceProxy (and underlying NurbsSurfacePoint)
      based on Single would make a faster implementation, but it's not worth
      the trouble --- VRML 97 NurbsSurface is only for compatibility,
      newer models should use X3D version anyway. }
    UKnotDouble := FdUKnot.Items.ToDouble;
    VKnotDouble := FdVKnot.Items.ToDouble;
    WeightDouble := FdWeight.Items.ToDouble;
    try
      NurbsPatchSurfaceProxy(TIndexedQuadSetNode(Result),
        FdControlPoint.Items,
        FdUTessellation.Value,
        FdVTessellation.Value,
        FdUDimension.Value,
        FdVDimension.Value,
        FdUOrder.Value,
        FdVOrder.Value,
        UKnotDouble,
        VKnotDouble,
        WeightDouble,
        { Both UClosed, VClosed are true for VRML 97 NurbsSurface.
          NurbsPatchSurfaceProxy will always check whether it should be
          closed based on whether limiting controlPoints match. }
        true, true,
        FdSolid.Value,
        FdCcw.Value,
        FdTexCoord.Value);
    finally
      FreeAndNil(UKnotDouble);
      FreeAndNil(VKnotDouble);
      FreeAndNil(WeightDouble);
    end;
  except FreeAndNil(Result); raise end;
end;

function TNurbsSurfaceNode.ProxyUsesOverTriangulate: boolean;
begin
  Result := false;
end;

{ We cannot simply override Coord() and let bounding box be calculated
  based on it. It would fail for curves with weigths. }
function TNurbsSurfaceNode.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  Result := NurbsBoundingBox(FdControlPoint.Items, FdWeight.Items);
end;

{ We cannot simply override Coord() and let bounding box be calculated
  based on it. It would fail for curves with weigths. }
function TNurbsSurfaceNode.BoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  Result := NurbsBoundingBox(FdControlPoint.Items, FdWeight.Items, State.Transform);
end;

function TNurbsSurfaceNode.TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
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

function TNurbsSurfaceNode.SolidField: TSFBool;
begin
  Result := FdSolid;
end;

procedure TNurbsTextureSurfaceNode.CreateNode;
begin
  inherited;

  FFdcontrolPoint := TMFVec2f.Create(Self, 'controlPoint', []);
  AddField(FFdcontrolPoint);

  FFdweight := TMFFloat.Create(Self, 'weight', []);
  AddField(FFdweight);

  FFduDimension := TSFInt32.Create(Self, 'uDimension', 0);
  FFduDimension.Exposed := false;
  AddField(FFduDimension);

  FFduKnot := TMFFloat.Create(Self, 'uKnot', []);
  FFduKnot.Exposed := false;
  AddField(FFduKnot);

  FFduOrder := TSFInt32.Create(Self, 'uOrder', 3);
  FFduOrder.Exposed := false;
  AddField(FFduOrder);

  FFdvDimension := TSFInt32.Create(Self, 'vDimension', 0);
  FFdvDimension.Exposed := false;
  AddField(FFdvDimension);

  FFdvKnot := TMFFloat.Create(Self, 'vKnot', []);
  FFdvKnot.Exposed := false;
  AddField(FFdvKnot);

  FFdvOrder := TSFInt32.Create(Self, 'vOrder', 3);
  FFdvOrder.Exposed := false;
  AddField(FFdvOrder);
end;

class function TNurbsTextureSurfaceNode.ClassX3DType: string;
begin
  Result := 'NurbsTextureSurface';
end;

class function TNurbsTextureSurfaceNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType);
end;

procedure TTrimmedSurfaceNode.CreateNode;
begin
  inherited;

  FEventaddTrimmingContour := TMFNodeEvent.Create(Self, 'addTrimmingContour', true);
  AddEvent(FEventaddTrimmingContour);

  FEventremoveTrimmingContour := TMFNodeEvent.Create(Self, 'removeTrimmingContour', true);
  AddEvent(FEventremoveTrimmingContour);

  FFdtrimmingContour := TMFNode.Create(Self, 'trimmingContour', [TContour2DNode_2]);
   FdtrimmingContour.ChangesAlways := [chGeometry];
  AddField(FFdtrimmingContour);

  FFdsurface := TSFNode.Create(Self, 'surface', [TNurbsSurfaceNode]);
   Fdsurface.ChangesAlways := [chGeometry];
  AddField(FFdsurface);
end;

class function TTrimmedSurfaceNode.ClassX3DType: string;
begin
  Result := 'TrimmedSurface';
end;

class function TTrimmedSurfaceNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType);
end;

{$define TGeometryNotImplemented := TTrimmedSurfaceNode}
GeometryNotImplemented

procedure TContour2DNode_2.CreateNode;
begin
  inherited;

  FEventAddChildren := TMFNodeEvent.Create(Self, 'addChildren', true);
  AddEvent(FEventAddChildren);

  FEventRemoveChildren := TMFNodeEvent.Create(Self, 'removeChildren', true);
  AddEvent(FEventRemoveChildren);

  FFdChildren := TMFNode.Create(Self, 'children', [TNurbsCurve2DNode, TContourPolyline2DNode,
    { In VRML 97 these are allowed children too: }
    TPolyline2DNode, TContour2DNode_2 ]);
   FdChildren.ChangesAlways := [chGeometry];
  AddField(FFdChildren);

  DefaultContainerField := 'trimmingContour';
end;

class function TContour2DNode_2.ClassX3DType: string;
begin
  Result := 'Contour2D';
end;

class function TContour2DNode_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType);
end;

class function TContour2DNode_2.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major = 2;
end;

function TContour2DNode_2.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := Fdchildren.Enumerate(Func);
  if Result <> nil then Exit;
end;

{$define TGeometryNotImplemented := TContour2DNode_2}
GeometryNotImplemented

procedure RegisterVRML97NodesNurbs;
begin
  NodesManager.RegisterNodeClasses([
    TCoordinateDeformerNode,
    TNurbsCurveNode_2,
    TNurbsGroupNode,
    TNurbsPositionInterpolatorNode_2,
    TNurbsSurfaceNode,
    TNurbsTextureSurfaceNode,
    TTrimmedSurfaceNode,
    TContour2DNode_2
  ]);
end;
{$endif read_implementation}
