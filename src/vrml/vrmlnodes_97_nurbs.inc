{
  Copyright 2002-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ NURBS nodes from VRML 97 Amendment 1 specification.

  Note: Some of VRML 97 NURBS nodes are the same (or at least
  backward compatible) as X3D NURBS nodes, these are defined and handled
  only in x3d_nurbs.inc. } { }
{$ifdef read_interface}
  TNodeCoordinateDeformer = class(TVRMLGroupingNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;

    { Event: MFNode, in } { }
    private FEventaddChildren: TVRMLEvent;
    public property EventaddChildren: TVRMLEvent read FEventaddChildren;

    { Event: MFNode, in } { }
    private FEventremoveChildren: TVRMLEvent;
    public property EventremoveChildren: TVRMLEvent read FEventremoveChildren;

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

  TNodeNurbsCurve_2 = class(TNodeX3DGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;

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

    class function ForVRMLVersion(const Version: TVRMLVersion): boolean; override;

    function Proxy(var State: TVRMLGraphTraverseState;
      const OverTriangulate: boolean): TVRMLGeometryNode; override;
    function ProxyUsesOverTriangulate: boolean; override;
    function LocalBoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function BoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
  end;

  TNodeNurbsGroup = class(TNodeX3DGroupingNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;

    private FFdtessellationScale: TSFFloat;
    public property FdtessellationScale: TSFFloat read FFdtessellationScale;

    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeNurbsPositionInterpolator_2 = class(TNodeX3DChildNode)
  private
    procedure EventSet_FractionReceive(Event: TVRMLEvent; Value: TVRMLField;
      const Time: TVRMLTime);
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;

    { Event: SFFloat, in } { }
    private FEventset_fraction: TVRMLEvent;
    public property Eventset_fraction: TVRMLEvent read FEventset_fraction;

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

    { Event: SFVec3f, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;

    class function URNMatching(const URN: string): boolean; override;

    class function ForVRMLVersion(const Version: TVRMLVersion): boolean; override;
  end;

  TNodeNurbsSurface = class(TNodeX3DGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;

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

    function Proxy(var State: TVRMLGraphTraverseState;
      const OverTriangulate: boolean): TVRMLGeometryNode; override;
    function ProxyUsesOverTriangulate: boolean; override;
    function LocalBoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function BoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
  end;

  TNodeNurbsTextureSurface = class(TVRMLNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;

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

  TNodeTrimmedSurface = class(TNodeX3DGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;

    { Event: MFNode, in } { }
    private FEventaddTrimmingContour: TVRMLEvent;
    public property EventaddTrimmingContour: TVRMLEvent read FEventaddTrimmingContour;

    { Event: MFNode, in } { }
    private FEventremoveTrimmingContour: TVRMLEvent;
    public property EventremoveTrimmingContour: TVRMLEvent read FEventremoveTrimmingContour;

    private FFdtrimmingContour: TMFNode;
    public property FdtrimmingContour: TMFNode read FFdtrimmingContour;

    private FFdsurface: TSFNode;
    public property Fdsurface: TSFNode read FFdsurface;

    class function URNMatching(const URN: string): boolean; override;

    { Geometry node not implemented } { }
    function LocalBoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
  end;
{$endif read_interface}

{$ifdef read_implementation}
procedure TNodeCoordinateDeformer.CreateNode;
begin
  inherited;

  FEventaddChildren := TVRMLEvent.Create(Self, 'addChildren', TMFNode, true);
  Events.Add(FEventaddChildren);

  FEventremoveChildren := TVRMLEvent.Create(Self, 'removeChildren', TMFNode, true);
  Events.Add(FEventremoveChildren);

  FFdchildren := TMFNode.Create(Self, 'children', INodeX3DChildNode);
  Fields.Add(FFdchildren);

  FFdcontrolPoint := TMFVec3f.Create(Self, 'controlPoint', []);
  Fields.Add(FFdcontrolPoint);

  FFdinputCoord := TMFNode.Create(Self, 'inputCoord', [TNodeCoordinate]);
  Fields.Add(FFdinputCoord);

  FFdinputTransform := TMFNode.Create(Self, 'inputTransform', [TNodeTransform]);
  Fields.Add(FFdinputTransform);

  FFdoutputCoord := TMFNode.Create(Self, 'outputCoord', [TNodeCoordinate]);
  Fields.Add(FFdoutputCoord);

  FFdweight := TMFFloat.Create(Self, 'weight', []);
  Fields.Add(FFdweight);

  FFdbboxCenter := TSFVec3f.Create(Self, 'bboxCenter', ZeroVector3Single);
  FFdbboxCenter.Exposed := false;
  Fields.Add(FFdbboxCenter);

  FFdbboxSize := TSFVec3f.Create(Self, 'bboxSize', Vector3Single(-1, -1, -1));
  FFdbboxSize.Exposed := false;
  Fields.Add(FFdbboxSize);

  FFduDimension := TSFInt32.Create(Self, 'uDimension', 0);
  FFduDimension.Exposed := false;
  Fields.Add(FFduDimension);

  FFduKnot := TMFFloat.Create(Self, 'uKnot', []);
  FFduKnot.Exposed := false;
  Fields.Add(FFduKnot);

  FFduOrder := TSFInt32.Create(Self, 'uOrder', 2);
  FFduOrder.Exposed := false;
  Fields.Add(FFduOrder);

  FFdvDimension := TSFInt32.Create(Self, 'vDimension', 0);
  FFdvDimension.Exposed := false;
  Fields.Add(FFdvDimension);

  FFdvKnot := TMFFloat.Create(Self, 'vKnot', []);
  FFdvKnot.Exposed := false;
  Fields.Add(FFdvKnot);

  FFdvOrder := TSFInt32.Create(Self, 'vOrder', 2);
  FFdvOrder.Exposed := false;
  Fields.Add(FFdvOrder);

  FFdwDimension := TSFInt32.Create(Self, 'wDimension', 0);
  FFdwDimension.Exposed := false;
  Fields.Add(FFdwDimension);

  FFdwKnot := TMFFloat.Create(Self, 'wKnot', []);
  FFdwKnot.Exposed := false;
  Fields.Add(FFdwKnot);

  FFdwOrder := TSFInt32.Create(Self, 'wOrder', 2);
  FFdwOrder.Exposed := false;
  Fields.Add(FFdwOrder);

  DefaultContainerField := 'children';
end;

class function TNodeCoordinateDeformer.ClassNodeTypeName: string;
begin
  Result := 'CoordinateDeformer';
end;

class function TNodeCoordinateDeformer.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

procedure TNodeNurbsCurve_2.CreateNode;
begin
  inherited;

  FFdcontrolPoint := TMFVec3f.Create(Self, 'controlPoint', []);
   FdcontrolPoint.ChangesAlways := [chGeometry];
  Fields.Add(FFdcontrolPoint);

  FFdweight := TMFFloat.Create(Self, 'weight', []);
   Fdweight.ChangesAlways := [chGeometry];
  Fields.Add(FFdweight);

  FFdtessellation := TSFInt32.Create(Self, 'tessellation', 0);
   Fdtessellation.ChangesAlways := [chGeometry];
  Fields.Add(FFdtessellation);

  FFdknot := TMFFloat.Create(Self, 'knot', []);
   Fdknot.Exposed := false;
   Fdknot.ChangesAlways := [chGeometry];
  Fields.Add(FFdknot);

  FFdorder := TSFInt32.Create(Self, 'order', 3);
   Fdorder.Exposed := false;
   Fdorder.ChangesAlways := [chGeometry];
  Fields.Add(FFdorder);
end;

class function TNodeNurbsCurve_2.ClassNodeTypeName: string;
begin
  Result := 'NurbsCurve';
end;

class function TNodeNurbsCurve_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

class function TNodeNurbsCurve_2.ForVRMLVersion(const Version: TVRMLVersion): boolean;
begin
  Result := Version.Major = 2;
end;

function TNodeNurbsCurve_2.Proxy(var State: TVRMLGraphTraverseState;
  const OverTriangulate: boolean): TVRMLGeometryNode;
var
  KnotDouble, WeightDouble: TDynDoubleArray;
begin
  Result := TNodeLineSet.Create(NodeName, WWWBasePath);
  try
    KnotDouble := FdKnot.Items.ToDouble;
    WeightDouble := FdWeight.Items.ToDouble;
    try
      NurbsCurveProxy(TNodeLineSet(Result), FdControlPoint.Items,
        FdTessellation.Value, FdOrder.Value, KnotDouble, WeightDouble);
    finally
      FreeAndNil(KnotDouble);
      FreeAndNil(WeightDouble);
    end;
  except FreeAndNil(Result); raise end;
end;

function TNodeNurbsCurve_2.ProxyUsesOverTriangulate: boolean;
begin
  Result := false;
end;

{ We cannot simply override Coord() and let bounding box be calculated
  based on it. It would fail for curves with weigths. }
function TNodeNurbsCurve_2.LocalBoundingBox(State: TVRMLGraphTraverseState;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D;
begin
  Result := NurbsBoundingBox(FdControlPoint.Items, FdWeight.Items);
end;

{ We cannot simply override Coord() and let bounding box be calculated
  based on it. It would fail for curves with weigths. }
function TNodeNurbsCurve_2.BoundingBox(State: TVRMLGraphTraverseState;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D;
begin
  Result := NurbsBoundingBox(FdControlPoint.Items, FdWeight.Items, State.Transform);
end;

function TNodeNurbsCurve_2.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
begin
  Result := 0;
end;

procedure TNodeNurbsGroup.CreateNode;
begin
  inherited;

  FFdtessellationScale := TSFFloat.Create(Self, 'tessellationScale', 1.0);
  Fields.Add(FFdtessellationScale);
end;

class function TNodeNurbsGroup.ClassNodeTypeName: string;
begin
  Result := 'NurbsGroup';
end;

class function TNodeNurbsGroup.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

procedure TNodeNurbsPositionInterpolator_2.CreateNode;
begin
  inherited;

  FEventset_fraction := TVRMLEvent.Create(Self, 'set_fraction', TSFFloat, true);
  Events.Add(FEventset_fraction);

  FFddimension := TSFInt32.Create(Self, 'dimension', 0);
  Fields.Add(FFddimension);

  FFdkeyValue := TMFVec3f.Create(Self, 'keyValue', []);
  Fields.Add(FFdkeyValue);

  FFdkeyWeight := TMFFloat.Create(Self, 'keyWeight', []);
  Fields.Add(FFdkeyWeight);

  FFdknot := TMFFloat.Create(Self, 'knot', []);
  Fields.Add(FFdknot);

  FFdorder := TSFInt32.Create(Self, 'order', 4);
  Fields.Add(FFdorder);

  FEventvalue_changed := TVRMLEvent.Create(Self, 'value_changed', TSFVec3f, false);
  Events.Add(FEventvalue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.OnReceive.Add(@EventSet_FractionReceive);
end;

class function TNodeNurbsPositionInterpolator_2.ClassNodeTypeName: string;
begin
  Result := 'NurbsPositionInterpolator';
end;

class function TNodeNurbsPositionInterpolator_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

class function TNodeNurbsPositionInterpolator_2.ForVRMLVersion(const Version: TVRMLVersion): boolean;
begin
  Result := Version.Major = 2;
end;

procedure TNodeNurbsPositionInterpolator_2.EventSet_FractionReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
var
  Knot, KeyWeight: TDynDoubleArray;
  OutputValue: TVector3Single;
begin
  if not EventValue_Changed.SendNeeded then Exit;

  if FdKeyValue.Count = 0 then Exit;

  if FdOrder.Value < 2 then
  begin
    OnWarning(wtMajor, 'VRML/X3D', 'NURBS order must be >= 2');
    Exit;
  end;

  { We can be sure now that
    - we have ControlPoint (in KeyValue), non-nil, with at least 1 point.
    - we have Order >= 2 }

  { calculate correct Knot vector }
  Knot := TDynDoubleArray.Create;
  Knot.Assign(FdKnot.Items);
  NurbsKnotIfNeeded(Knot, FdKeyValue.Count, FdOrder.Value, nkEndpointUniform);

  KeyWeight := FdKeyWeight.Items.ToDouble;

  OutputValue := NurbsCurvePoint(FdKeyValue.Items, (Value as TSFFloat).Value,
    FdOrder.Value, Knot, KeyWeight, nil);

  FreeAndNil(KeyWeight);
  FreeAndNil(Knot);

  EventValue_Changed.Send(OutputValue, Time);
end;

procedure TNodeNurbsSurface.CreateNode;
begin
  inherited;

  FFdcontrolPoint := TMFVec3f.Create(Self, 'controlPoint', []);
   FdcontrolPoint.ChangesAlways := [chGeometry];
  Fields.Add(FFdcontrolPoint);

  FFdtexCoord := TSFNode.Create(Self, 'texCoord', [TNodeTextureCoordinate, TNodeNurbsTextureSurface]);
   FdtexCoord.ChangesAlways := [chGeometry];
  Fields.Add(FFdtexCoord);

  FFduTessellation := TSFInt32.Create(Self, 'uTessellation', 0);
   FduTessellation.ChangesAlways := [chGeometry];
  Fields.Add(FFduTessellation);

  FFdvTessellation := TSFInt32.Create(Self, 'vTessellation', 0);
   FdvTessellation.ChangesAlways := [chGeometry];
  Fields.Add(FFdvTessellation);

  FFdweight := TMFFloat.Create(Self, 'weight', []);
   Fdweight.ChangesAlways := [chGeometry];
  Fields.Add(FFdweight);

  FFdccw := TSFBool.Create(Self, 'ccw', TRUE);
   Fdccw.Exposed := false;
   Fdccw.ChangesAlways := [chGeometry];
  Fields.Add(FFdccw);

  FFdsolid := TSFBool.Create(Self, 'solid', TRUE);
   Fdsolid.Exposed := false;
   Fdsolid.ChangesAlways := [chGeometry];
  Fields.Add(FFdsolid);

  FFduDimension := TSFInt32.Create(Self, 'uDimension', 0);
   FduDimension.Exposed := false;
   FduDimension.ChangesAlways := [chGeometry];
  Fields.Add(FFduDimension);

  FFduKnot := TMFFloat.Create(Self, 'uKnot', []);
   FduKnot.Exposed := false;
   FduKnot.ChangesAlways := [chGeometry];
  Fields.Add(FFduKnot);

  FFduOrder := TSFInt32.Create(Self, 'uOrder', 3);
   FduOrder.Exposed := false;
   FduOrder.ChangesAlways := [chGeometry];
  Fields.Add(FFduOrder);

  FFdvDimension := TSFInt32.Create(Self, 'vDimension', 0);
   FdvDimension.Exposed := false;
   FdvDimension.ChangesAlways := [chGeometry];
  Fields.Add(FFdvDimension);

  FFdvKnot := TMFFloat.Create(Self, 'vKnot', []);
   FdvKnot.Exposed := false;
   FdvKnot.ChangesAlways := [chGeometry];
  Fields.Add(FFdvKnot);

  FFdvOrder := TSFInt32.Create(Self, 'vOrder', 3);
   FdvOrder.Exposed := false;
   FdvOrder.ChangesAlways := [chGeometry];
  Fields.Add(FFdvOrder);
end;

class function TNodeNurbsSurface.ClassNodeTypeName: string;
begin
  Result := 'NurbsSurface';
end;

class function TNodeNurbsSurface.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

function TNodeNurbsSurface.Proxy(var State: TVRMLGraphTraverseState;
  const OverTriangulate: boolean): TVRMLGeometryNode;
var
  UKnotDouble, VKnotDouble, WeightDouble: TDynDoubleArray;
begin
  Result := TNodeIndexedQuadSet.Create(NodeName, WWWBasePath);
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
      NurbsPatchSurfaceProxy(TNodeIndexedQuadSet(Result),
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

function TNodeNurbsSurface.ProxyUsesOverTriangulate: boolean;
begin
  Result := false;
end;

{ We cannot simply override Coord() and let bounding box be calculated
  based on it. It would fail for curves with weigths. }
function TNodeNurbsSurface.LocalBoundingBox(State: TVRMLGraphTraverseState;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D;
begin
  Result := NurbsBoundingBox(FdControlPoint.Items, FdWeight.Items);
end;

{ We cannot simply override Coord() and let bounding box be calculated
  based on it. It would fail for curves with weigths. }
function TNodeNurbsSurface.BoundingBox(State: TVRMLGraphTraverseState;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D;
begin
  Result := NurbsBoundingBox(FdControlPoint.Items, FdWeight.Items, State.Transform);
end;

function TNodeNurbsSurface.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
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

procedure TNodeNurbsTextureSurface.CreateNode;
begin
  inherited;

  FFdcontrolPoint := TMFVec2f.Create(Self, 'controlPoint', []);
  Fields.Add(FFdcontrolPoint);

  FFdweight := TMFFloat.Create(Self, 'weight', []);
  Fields.Add(FFdweight);

  FFduDimension := TSFInt32.Create(Self, 'uDimension', 0);
  FFduDimension.Exposed := false;
  Fields.Add(FFduDimension);

  FFduKnot := TMFFloat.Create(Self, 'uKnot', []);
  FFduKnot.Exposed := false;
  Fields.Add(FFduKnot);

  FFduOrder := TSFInt32.Create(Self, 'uOrder', 3);
  FFduOrder.Exposed := false;
  Fields.Add(FFduOrder);

  FFdvDimension := TSFInt32.Create(Self, 'vDimension', 0);
  FFdvDimension.Exposed := false;
  Fields.Add(FFdvDimension);

  FFdvKnot := TMFFloat.Create(Self, 'vKnot', []);
  FFdvKnot.Exposed := false;
  Fields.Add(FFdvKnot);

  FFdvOrder := TSFInt32.Create(Self, 'vOrder', 3);
  FFdvOrder.Exposed := false;
  Fields.Add(FFdvOrder);
end;

class function TNodeNurbsTextureSurface.ClassNodeTypeName: string;
begin
  Result := 'NurbsTextureSurface';
end;

class function TNodeNurbsTextureSurface.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

procedure TNodeTrimmedSurface.CreateNode;
begin
  inherited;

  FEventaddTrimmingContour := TVRMLEvent.Create(Self, 'addTrimmingContour', TMFNode, true);
  Events.Add(FEventaddTrimmingContour);

  FEventremoveTrimmingContour := TVRMLEvent.Create(Self, 'removeTrimmingContour', TMFNode, true);
  Events.Add(FEventremoveTrimmingContour);

  FFdtrimmingContour := TMFNode.Create(Self, 'trimmingContour', [TNodeContour2D]);
   FdtrimmingContour.ChangesAlways := [chGeometry];
  Fields.Add(FFdtrimmingContour);

  FFdsurface := TSFNode.Create(Self, 'surface', [TNodeNurbsSurface]);
   Fdsurface.ChangesAlways := [chGeometry];
  Fields.Add(FFdsurface);
end;

class function TNodeTrimmedSurface.ClassNodeTypeName: string;
begin
  Result := 'TrimmedSurface';
end;

class function TNodeTrimmedSurface.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName);
end;

{$define TGeometryNotImplemented := TNodeTrimmedSurface}
GeometryNotImplemented

procedure RegisterVRML97NodesNurbs;
begin
  NodesManager.RegisterNodeClasses([
    TNodeCoordinateDeformer,
    TNodeNurbsCurve_2,
    TNodeNurbsGroup,
    TNodeNurbsPositionInterpolator_2,
    TNodeNurbsSurface,
    TNodeNurbsTextureSurface,
    TNodeTrimmedSurface
  ]);
end;
{$endif read_implementation}
