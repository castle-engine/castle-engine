{
  Copyright 2002-2022 Michalis Kamburelis.

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
    strict private FEventaddChildren: TMFNodeEvent;
    public property EventaddChildren: TMFNodeEvent read FEventaddChildren;

    { Event in } { }
    strict private FEventremoveChildren: TMFNodeEvent;
    public property EventremoveChildren: TMFNodeEvent read FEventremoveChildren;

    strict private FFdchildren: TMFNode;
    public property Fdchildren: TMFNode read FFdchildren;

    strict private FFdcontrolPoint: TMFVec3f;
    public property FdcontrolPoint: TMFVec3f read FFdcontrolPoint;

    strict private FFdinputCoord: TMFNode;
    public property FdinputCoord: TMFNode read FFdinputCoord;

    strict private FFdinputTransform: TMFNode;
    public property FdinputTransform: TMFNode read FFdinputTransform;

    strict private FFdoutputCoord: TMFNode;
    public property FdoutputCoord: TMFNode read FFdoutputCoord;

    strict private FFdweight: TMFFloat;
    public property Fdweight: TMFFloat read FFdweight;

    strict private FFdbboxCenter: TSFVec3f;
    public property FdbboxCenter: TSFVec3f read FFdbboxCenter;

    strict private FFdbboxSize: TSFVec3f;
    public property FdbboxSize: TSFVec3f read FFdbboxSize;

    strict private FFduDimension: TSFInt32;
    public property FduDimension: TSFInt32 read FFduDimension;

    strict private FFduKnot: TMFFloat;
    public property FduKnot: TMFFloat read FFduKnot;

    strict private FFduOrder: TSFInt32;
    public property FduOrder: TSFInt32 read FFduOrder;

    strict private FFdvDimension: TSFInt32;
    public property FdvDimension: TSFInt32 read FFdvDimension;

    strict private FFdvKnot: TMFFloat;
    public property FdvKnot: TMFFloat read FFdvKnot;

    strict private FFdvOrder: TSFInt32;
    public property FdvOrder: TSFInt32 read FFdvOrder;

    strict private FFdwDimension: TSFInt32;
    public property FdwDimension: TSFInt32 read FFdwDimension;

    strict private FFdwKnot: TMFFloat;
    public property FdwKnot: TMFFloat read FFdwKnot;

    strict private FFdwOrder: TSFInt32;
    public property FdwOrder: TSFInt32 read FFdwOrder;
  end;

  { Visible NURBS curve in 3D, in VRML 2.0.
    @bold(Not implemented.) }
  TNurbsCurveNode_2 = class(TAbstractGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdcontrolPoint: TMFVec3f;
    public property FdcontrolPoint: TMFVec3f read FFdcontrolPoint;

    strict private FFdweight: TMFFloat;
    public property Fdweight: TMFFloat read FFdweight;

    strict private FFdtessellation: TSFInt32;
    public property Fdtessellation: TSFInt32 read FFdtessellation;

    strict private FFdknot: TMFFloat;
    public property Fdknot: TMFFloat read FFdknot;

    strict private FFdorder: TSFInt32;
    public property Fdorder: TSFInt32 read FFdorder;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean; override;

    function Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode; override;
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function BoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
  end deprecated 'use TNurbsCurveNode';

  { Groups a set of NURBS surface nodes to a common group
    for rendering purposes, to ensure a common tesselation within the group,
    for VRML 2.0. }
  TNurbsGroupNode = class(TAbstractX3DGroupingNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdtessellationScale: TSFFloat;
    public property FdtessellationScale: TSFFloat read FFdtessellationScale;
  end;

  { Interpolate (animate) positions along the 3D NURBS curve. }
  TNurbsPositionInterpolatorNode_2 = class(TAbstractChildNode)
  strict private
    procedure EventSet_FractionReceive(const Event: TX3DEvent; const Value: TX3DField;
      const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    { Event in } { }
    strict private FEventset_fraction: TSFFloatEvent;
    public property Eventset_fraction: TSFFloatEvent read FEventset_fraction;

    strict private FFddimension: TSFInt32;
    public property Fddimension: TSFInt32 read FFddimension;

    strict private FFdkeyValue: TMFVec3f;
    public property FdkeyValue: TMFVec3f read FFdkeyValue;

    strict private FFdkeyWeight: TMFFloat;
    public property FdkeyWeight: TMFFloat read FFdkeyWeight;

    strict private FFdknot: TMFFloat;
    public property Fdknot: TMFFloat read FFdknot;

    strict private FFdorder: TSFInt32;
    public property Fdorder: TSFInt32 read FFdorder;

    { Event out } { }
    strict private FEventvalue_changed: TSFVec3fEvent;
    public property Eventvalue_changed: TSFVec3fEvent read FEventvalue_changed;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean; override;
  end deprecated 'use TNurbsPositionInterpolatorNode';

  { Visible NURBS 3D surface, for VRML 2.0. }
  TNurbsSurfaceNode = class(TAbstractGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdcontrolPoint: TMFVec3f;
    public property FdcontrolPoint: TMFVec3f read FFdcontrolPoint;

    strict private FFdtexCoord: TSFNode;
    public property FdtexCoord: TSFNode read FFdtexCoord;

    strict private FFduTessellation: TSFInt32;
    public property FduTessellation: TSFInt32 read FFduTessellation;

    strict private FFdvTessellation: TSFInt32;
    public property FdvTessellation: TSFInt32 read FFdvTessellation;

    strict private FFdweight: TMFFloat;
    public property Fdweight: TMFFloat read FFdweight;

    strict private FFdccw: TSFBool;
    public property Fdccw: TSFBool read FFdccw;

    strict private FFdsolid: TSFBool;
    public property Fdsolid: TSFBool read FFdsolid;

    strict private FFduDimension: TSFInt32;
    public property FduDimension: TSFInt32 read FFduDimension;

    strict private FFduKnot: TMFFloat;
    public property FduKnot: TMFFloat read FFduKnot;

    strict private FFduOrder: TSFInt32;
    public property FduOrder: TSFInt32 read FFduOrder;

    strict private FFdvDimension: TSFInt32;
    public property FdvDimension: TSFInt32 read FFdvDimension;

    strict private FFdvKnot: TMFFloat;
    public property FdvKnot: TMFFloat read FFdvKnot;

    strict private FFdvOrder: TSFInt32;
    public property FdvOrder: TSFInt32 read FFdvOrder;

    function Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode; override;
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function BoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function SolidField: TSFBool; override;
  end deprecated 'use TNurbsPatchSurfaceNode; we define TNurbsSurfaceNode only for compatibility with VRML 2.0';

  { NURBS surface existing in the parametric domain of its surface host specifying
    the mapping of the texture onto the surface, for VRML 2.0.
    @bold(Not implemented.) }
  TNurbsTextureSurfaceNode = class(TX3DNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdcontrolPoint: TMFVec2f;
    public property FdcontrolPoint: TMFVec2f read FFdcontrolPoint;

    strict private FFdweight: TMFFloat;
    public property Fdweight: TMFFloat read FFdweight;

    strict private FFduDimension: TSFInt32;
    public property FduDimension: TSFInt32 read FFduDimension;

    strict private FFduKnot: TMFFloat;
    public property FduKnot: TMFFloat read FFduKnot;

    strict private FFduOrder: TSFInt32;
    public property FduOrder: TSFInt32 read FFduOrder;

    strict private FFdvDimension: TSFInt32;
    public property FdvDimension: TSFInt32 read FFdvDimension;

    strict private FFdvKnot: TMFFloat;
    public property FdvKnot: TMFFloat read FFdvKnot;

    strict private FFdvOrder: TSFInt32;
    public property FdvOrder: TSFInt32 read FFdvOrder;

  end;

  { NURBS surface that is trimmed by a set of trimming loops.
    @bold(Rendering of this node is not implemented yet.) }
  TTrimmedSurfaceNode = class(TAbstractGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    { Event in } { }
    strict private FEventaddTrimmingContour: TMFNodeEvent;
    public property EventaddTrimmingContour: TMFNodeEvent read FEventaddTrimmingContour;

    { Event in } { }
    strict private FEventremoveTrimmingContour: TMFNodeEvent;
    public property EventremoveTrimmingContour: TMFNodeEvent read FEventremoveTrimmingContour;

    strict private FFdtrimmingContour: TMFNode;
    public property FdtrimmingContour: TMFNode read FFdtrimmingContour;

    strict private FFdsurface: TSFNode;
    public property Fdsurface: TSFNode read FFdsurface;

    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
  end deprecated 'use TNurbsTrimmedSurfaceNode';

  { 2D curve composed from NURBS curves and straight segments, for VRML 2.0.
    @bold(Not implemented yet.) }
  TContour2DNode_2 = class(TAbstractGeometryNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function ForVRMLVersion(const Version: TX3DVersion): boolean; override;

    { Event in } { }
    strict private FEventAddChildren: TMFNodeEvent;
    public property EventAddChildren: TMFNodeEvent read FEventAddChildren;

    { Event in } { }
    strict private FEventRemoveChildren: TMFNodeEvent;
    public property EventRemoveChildren: TMFNodeEvent read FEventRemoveChildren;

    strict private FFdChildren: TMFNode;
    public property FdChildren: TMFNode read FFdChildren;

    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TX3DGraphTraverseState;
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

  FFdchildren := TMFNode.Create(Self, true, 'children', [TAbstractChildNode]);
  AddField(FFdchildren);

  FFdcontrolPoint := TMFVec3f.Create(Self, true, 'controlPoint', []);
  AddField(FFdcontrolPoint);

  FFdinputCoord := TMFNode.Create(Self, true, 'inputCoord', [TCoordinateNode]);
  AddField(FFdinputCoord);

  FFdinputTransform := TMFNode.Create(Self, true, 'inputTransform', [TTransformNode]);
  AddField(FFdinputTransform);

  FFdoutputCoord := TMFNode.Create(Self, true, 'outputCoord', [TCoordinateNode]);
  AddField(FFdoutputCoord);

  FFdweight := TMFFloat.Create(Self, true, 'weight', []);
  AddField(FFdweight);

  FFdbboxCenter := TSFVec3f.Create(Self, false, 'bboxCenter', TVector3.Zero);
  AddField(FFdbboxCenter);

  FFdbboxSize := TSFVec3f.Create(Self, false, 'bboxSize', Vector3(-1, -1, -1));
  AddField(FFdbboxSize);

  FFduDimension := TSFInt32.Create(Self, false, 'uDimension', 0);
  AddField(FFduDimension);

  FFduKnot := TMFFloat.Create(Self, false, 'uKnot', []);
  AddField(FFduKnot);

  FFduOrder := TSFInt32.Create(Self, false, 'uOrder', 2);
  AddField(FFduOrder);

  FFdvDimension := TSFInt32.Create(Self, false, 'vDimension', 0);
  AddField(FFdvDimension);

  FFdvKnot := TMFFloat.Create(Self, false, 'vKnot', []);
  AddField(FFdvKnot);

  FFdvOrder := TSFInt32.Create(Self, false, 'vOrder', 2);
  AddField(FFdvOrder);

  FFdwDimension := TSFInt32.Create(Self, false, 'wDimension', 0);
  AddField(FFdwDimension);

  FFdwKnot := TMFFloat.Create(Self, false, 'wKnot', []);
  AddField(FFdwKnot);

  FFdwOrder := TSFInt32.Create(Self, false, 'wOrder', 2);
  AddField(FFdwOrder);

  DefaultContainerField := 'children';
end;

class function TCoordinateDeformerNode.ClassX3DType: string;
begin
  Result := 'CoordinateDeformer';
end;

procedure TNurbsCurveNode_2.CreateNode;
begin
  inherited;

  FFdcontrolPoint := TMFVec3f.Create(Self, true, 'controlPoint', []);
   FdcontrolPoint.ChangeAlways := chGeometry;
  AddField(FFdcontrolPoint);

  FFdweight := TMFFloat.Create(Self, true, 'weight', []);
   Fdweight.ChangeAlways := chGeometry;
  AddField(FFdweight);

  FFdtessellation := TSFInt32.Create(Self, true, 'tessellation', 0);
   Fdtessellation.ChangeAlways := chGeometry;
  AddField(FFdtessellation);

  FFdknot := TMFFloat.Create(Self, false, 'knot', []);
   Fdknot.ChangeAlways := chGeometry;
  AddField(FFdknot);

  FFdorder := TSFInt32.Create(Self, false, 'order', 3);
   Fdorder.ChangeAlways := chGeometry;
  AddField(FFdorder);
end;

class function TNurbsCurveNode_2.ClassX3DType: string;
begin
  Result := 'NurbsCurve';
end;

class function TNurbsCurveNode_2.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major = 2;
end;

function TNurbsCurveNode_2.Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode;
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

{ We cannot simply override InternalCoord() and let bounding box be calculated
  based on it. It would fail for curves with weigths. }
function TNurbsCurveNode_2.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  Result := NurbsBoundingBox(FdControlPoint.Items, FdWeight.Items);
end;

{ We cannot simply override InternalCoord() and let bounding box be calculated
  based on it. It would fail for curves with weigths. }
function TNurbsCurveNode_2.BoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  Result := NurbsBoundingBox(FdControlPoint.Items, FdWeight.Items, State.Transformation.Transform);
end;

function TNurbsCurveNode_2.TrianglesCount(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  Result := 0;
end;

procedure TNurbsGroupNode.CreateNode;
begin
  inherited;

  FFdtessellationScale := TSFFloat.Create(Self, true, 'tessellationScale', 1.0);
  AddField(FFdtessellationScale);
end;

class function TNurbsGroupNode.ClassX3DType: string;
begin
  Result := 'NurbsGroup';
end;

procedure TNurbsPositionInterpolatorNode_2.CreateNode;
begin
  inherited;

  FEventset_fraction := TSFFloatEvent.Create(Self, 'set_fraction', true);
  AddEvent(FEventset_fraction);

  FFddimension := TSFInt32.Create(Self, true, 'dimension', 0);
  AddField(FFddimension);

  FFdkeyValue := TMFVec3f.Create(Self, true, 'keyValue', []);
  AddField(FFdkeyValue);

  FFdkeyWeight := TMFFloat.Create(Self, true, 'keyWeight', []);
  AddField(FFdkeyWeight);

  FFdknot := TMFFloat.Create(Self, true, 'knot', []);
  AddField(FFdknot);

  FFdorder := TSFInt32.Create(Self, true, 'order', 4);
  AddField(FFdorder);

  FEventvalue_changed := TSFVec3fEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventvalue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.AddNotification({$ifdef FPC}@{$endif} EventSet_FractionReceive);
end;

class function TNurbsPositionInterpolatorNode_2.ClassX3DType: string;
begin
  Result := 'NurbsPositionInterpolator';
end;

class function TNurbsPositionInterpolatorNode_2.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major = 2;
end;

procedure TNurbsPositionInterpolatorNode_2.EventSet_FractionReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
var
  Knot, KeyWeight: TDoubleList;
  OutputValue: TVector3;
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

  FFdcontrolPoint := TMFVec3f.Create(Self, true, 'controlPoint', []);
   FdcontrolPoint.ChangeAlways := chGeometry;
  AddField(FFdcontrolPoint);

  FFdtexCoord := TSFNode.Create(Self, true, 'texCoord', [TTextureCoordinateNode, TNurbsTextureSurfaceNode]);
   FdtexCoord.ChangeAlways := chGeometry;
  AddField(FFdtexCoord);

  FFduTessellation := TSFInt32.Create(Self, true, 'uTessellation', 0);
   FduTessellation.ChangeAlways := chGeometry;
  AddField(FFduTessellation);

  FFdvTessellation := TSFInt32.Create(Self, true, 'vTessellation', 0);
   FdvTessellation.ChangeAlways := chGeometry;
  AddField(FFdvTessellation);

  FFdweight := TMFFloat.Create(Self, true, 'weight', []);
   Fdweight.ChangeAlways := chGeometry;
  AddField(FFdweight);

  FFdccw := TSFBool.Create(Self, false, 'ccw', TRUE);
   Fdccw.ChangeAlways := chGeometry;
  AddField(FFdccw);

  FFdsolid := TSFBool.Create(Self, false, 'solid', TRUE);
   Fdsolid.ChangeAlways := chGeometry;
  AddField(FFdsolid);

  FFduDimension := TSFInt32.Create(Self, false, 'uDimension', 0);
   FduDimension.ChangeAlways := chGeometry;
  AddField(FFduDimension);

  FFduKnot := TMFFloat.Create(Self, false, 'uKnot', []);
   FduKnot.ChangeAlways := chGeometry;
  AddField(FFduKnot);

  FFduOrder := TSFInt32.Create(Self, false, 'uOrder', 3);
   FduOrder.ChangeAlways := chGeometry;
  AddField(FFduOrder);

  FFdvDimension := TSFInt32.Create(Self, false, 'vDimension', 0);
   FdvDimension.ChangeAlways := chGeometry;
  AddField(FFdvDimension);

  FFdvKnot := TMFFloat.Create(Self, false, 'vKnot', []);
   FdvKnot.ChangeAlways := chGeometry;
  AddField(FFdvKnot);

  FFdvOrder := TSFInt32.Create(Self, false, 'vOrder', 3);
   FdvOrder.ChangeAlways := chGeometry;
  AddField(FFdvOrder);
end;

class function TNurbsSurfaceNode.ClassX3DType: string;
begin
  Result := 'NurbsSurface';
end;

function TNurbsSurfaceNode.Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode;
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

{ We cannot simply override InternalCoord() and let bounding box be calculated
  based on it. It would fail for curves with weigths. }
function TNurbsSurfaceNode.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  Result := NurbsBoundingBox(FdControlPoint.Items, FdWeight.Items);
end;

{ We cannot simply override InternalCoord() and let bounding box be calculated
  based on it. It would fail for curves with weigths. }
function TNurbsSurfaceNode.BoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  Result := NurbsBoundingBox(FdControlPoint.Items, FdWeight.Items, State.Transformation.Transform);
end;

function TNurbsSurfaceNode.TrianglesCount(State: TX3DGraphTraverseState;
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

  FFdcontrolPoint := TMFVec2f.Create(Self, true, 'controlPoint', []);
  AddField(FFdcontrolPoint);

  FFdweight := TMFFloat.Create(Self, true, 'weight', []);
  AddField(FFdweight);

  FFduDimension := TSFInt32.Create(Self, false, 'uDimension', 0);
  AddField(FFduDimension);

  FFduKnot := TMFFloat.Create(Self, false, 'uKnot', []);
  AddField(FFduKnot);

  FFduOrder := TSFInt32.Create(Self, false, 'uOrder', 3);
  AddField(FFduOrder);

  FFdvDimension := TSFInt32.Create(Self, false, 'vDimension', 0);
  AddField(FFdvDimension);

  FFdvKnot := TMFFloat.Create(Self, false, 'vKnot', []);
  AddField(FFdvKnot);

  FFdvOrder := TSFInt32.Create(Self, false, 'vOrder', 3);
  AddField(FFdvOrder);
end;

class function TNurbsTextureSurfaceNode.ClassX3DType: string;
begin
  Result := 'NurbsTextureSurface';
end;

procedure TTrimmedSurfaceNode.CreateNode;
begin
  inherited;

  FEventaddTrimmingContour := TMFNodeEvent.Create(Self, 'addTrimmingContour', true);
  AddEvent(FEventaddTrimmingContour);

  FEventremoveTrimmingContour := TMFNodeEvent.Create(Self, 'removeTrimmingContour', true);
  AddEvent(FEventremoveTrimmingContour);

  FFdtrimmingContour := TMFNode.Create(Self, true, 'trimmingContour', [TContour2DNode_2]);
   FdtrimmingContour.ChangeAlways := chGeometry;
  AddField(FFdtrimmingContour);

  {$warnings off} // using deprecated nodes in deprecated nodes
  FFdsurface := TSFNode.Create(Self, true, 'surface', [TNurbsSurfaceNode]);
   Fdsurface.ChangeAlways := chGeometry;
  AddField(FFdsurface);
  {$warnings on}
end;

class function TTrimmedSurfaceNode.ClassX3DType: string;
begin
  Result := 'TrimmedSurface';
end;

function TTrimmedSurfaceNode.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  { Rendering of TTrimmedSurfaceNode not implemented. }
  Result := TBox3D.Empty;
end;

function TTrimmedSurfaceNode.VerticesCount(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  { Rendering of TTrimmedSurfaceNode not implemented. }
  Result := 0;
end;

function TTrimmedSurfaceNode.TrianglesCount(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  { Rendering of TTrimmedSurfaceNode not implemented. }
  Result := 0;
end;

{ TContour2DNode_2 ----------------------------------------------------------- }

procedure TContour2DNode_2.CreateNode;
begin
  inherited;

  FEventAddChildren := TMFNodeEvent.Create(Self, 'addChildren', true);
  AddEvent(FEventAddChildren);

  FEventRemoveChildren := TMFNodeEvent.Create(Self, 'removeChildren', true);
  AddEvent(FEventRemoveChildren);

  FFdChildren := TMFNode.Create(Self, true, 'children', [TNurbsCurve2DNode, TContourPolyline2DNode,
    { In VRML 97 these are allowed children too: }
    TPolyline2DNode, TContour2DNode_2 ]);
   FdChildren.ChangeAlways := chGeometry;
  AddField(FFdChildren);

  DefaultContainerField := 'trimmingContour';
end;

class function TContour2DNode_2.ClassX3DType: string;
begin
  Result := 'Contour2D';
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

function TContour2DNode_2.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  { Rendering of TContour2DNode_2 not implemented. }
  Result := TBox3D.Empty;
end;

function TContour2DNode_2.VerticesCount(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  { Rendering of TContour2DNode_2 not implemented. }
  Result := 0;
end;

function TContour2DNode_2.TrianglesCount(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  { Rendering of TContour2DNode_2 not implemented. }
  Result := 0;
end;

procedure RegisterVRML97NodesNurbs;
begin
  {$warnings off} // register deprecated nodes
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
  {$warnings on}
end;
{$endif read_implementation}
