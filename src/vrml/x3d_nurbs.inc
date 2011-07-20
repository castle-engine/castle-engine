{
  Copyright 2008-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ NurbsCurve2D and Contour2D are shared with VRML 97 NURBS definition.
  The rest is, for now, completely separate from VRML 97 NURBS definition
  in vrmlnodes_97_nurbs.inc. }

{$ifdef KAMBI_HAS_NURBS}

{$ifdef read_interface}
  { }
  TNodeX3DNurbsControlCurveNode = class(TNodeX3DNode)
  public
    procedure CreateNode; override;

    private FFdcontrolPoint: TMFVec2d;
    public property FdcontrolPoint: TMFVec2d read FFdcontrolPoint;
  end;

  TNodeX3DParametricGeometryNode = class(TNodeX3DGeometryNode)
  end;

  TNodeX3DNurbsSurfaceGeometryNode = class(TNodeX3DParametricGeometryNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    procedure CreateNode; override;

    private FFdcontrolPoint: TSFNode;
    public property FdcontrolPoint: TSFNode read FFdcontrolPoint;

    private FFdtexCoord: TSFNode;
    public property FdtexCoord: TSFNode read FFdtexCoord;

    private FFduTessellation: TSFInt32;
    public property FduTessellation: TSFInt32 read FFduTessellation;

    private FFdvTessellation: TSFInt32;
    public property FdvTessellation: TSFInt32 read FFdvTessellation;

    private FFdweight: TMFDouble;
    public property Fdweight: TMFDouble read FFdweight;

    private FFdsolid: TSFBool;
    public property Fdsolid: TSFBool read FFdsolid;

    private FFduClosed: TSFBool;
    public property FduClosed: TSFBool read FFduClosed;

    private FFduDimension: TSFInt32;
    public property FduDimension: TSFInt32 read FFduDimension;

    private FFduKnot: TMFDouble;
    public property FduKnot: TMFDouble read FFduKnot;

    private FFduOrder: TSFInt32;
    public property FduOrder: TSFInt32 read FFduOrder;

    private FFdvClosed: TSFBool;
    public property FdvClosed: TSFBool read FFdvClosed;

    private FFdvDimension: TSFInt32;
    public property FdvDimension: TSFInt32 read FFdvDimension;

    private FFdvKnot: TMFDouble;
    public property FdvKnot: TMFDouble read FFdvKnot;

    private FFdvOrder: TSFInt32;
    public property FdvOrder: TSFInt32 read FFdvOrder;
  end;

  { Contour2D node.

    For VRML 2.0, this is TNodeX3DGeometryNode, since VRML 2.0 specification
    explicitly says it's valid geometry node. For X3D specification, this was only
    TNodeX3DNode. When actually implementing this, I will have to either handle
    both VRML 2.0 and X3D cases at once, or just split this class into
    two separate TNodeContour2D_2 and TNodeContour2D. }
  TNodeContour2D = class(TNodeX3DGeometryNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFNode, in } { }
    private FEventaddChildren: TVRMLEvent;
    public property EventaddChildren: TVRMLEvent read FEventaddChildren;

    { Event: MFNode, in } { }
    private FEventremoveChildren: TVRMLEvent;
    public property EventremoveChildren: TVRMLEvent read FEventremoveChildren;

    private FFdchildren: TMFNode;
    public property Fdchildren: TMFNode read FFdchildren;

    { Geometry node not implemented } { }
    function LocalBoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
  end;

  TNodeContourPolyline2D = class(TNodeX3DNurbsControlCurveNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeCoordinateDouble = class(TNodeX3DCoordinateNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdpoint: TMFVec3d;
    public property Fdpoint: TMFVec3d read FFdpoint;

    function CoordCount: Cardinal; override;
  end;

  TNodeNurbsCurve = class(TNodeX3DParametricGeometryNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdcontrolPoint: TSFNode;
    public property FdcontrolPoint: TSFNode read FFdcontrolPoint;

    private FFdtessellation: TSFInt32;
    public property Fdtessellation: TSFInt32 read FFdtessellation;

    private FFdweight: TMFDouble;
    public property Fdweight: TMFDouble read FFdweight;

    private FFdclosed: TSFBool;
    public property Fdclosed: TSFBool read FFdclosed;

    private FFdknot: TMFDouble;
    public property Fdknot: TMFDouble read FFdknot;

    private FFdorder: TSFInt32;
    public property Fdorder: TSFInt32 read FFdorder;

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
  TNodeNurbsCurve_3 = TNodeNurbsCurve;

  TNodeNurbsCurve2D = class(TNodeX3DNurbsControlCurveNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdtessellation: TSFInt32;
    public property Fdtessellation: TSFInt32 read FFdtessellation;

    private FFdweight: TMFDouble;
    public property Fdweight: TMFDouble read FFdweight;

    private FFdclosed: TSFBool;
    public property Fdclosed: TSFBool read FFdclosed;

    private FFdknot: TMFDouble;
    public property Fdknot: TMFDouble read FFdknot;

    private FFdorder: TSFInt32;
    public property Fdorder: TSFInt32 read FFdorder;
  end;

  TNodeNurbsOrientationInterpolator = class(TNodeX3DChildNode)
  private
    procedure EventSet_FractionReceive(Event: TVRMLEvent; Value: TVRMLField;
      const Time: TVRMLTime);
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFFloat, in } { }
    private FEventset_fraction: TVRMLEvent;
    public property Eventset_fraction: TVRMLEvent read FEventset_fraction;

    private FFdcontrolPoint: TSFNode;
    public property FdcontrolPoint: TSFNode read FFdcontrolPoint;

    private FFdknot: TMFDouble;
    public property Fdknot: TMFDouble read FFdknot;

    private FFdorder: TSFInt32;
    public property Fdorder: TSFInt32 read FFdorder;

    private FFdweight: TMFDouble;
    public property Fdweight: TMFDouble read FFdweight;

    { Event: SFRotation, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;
  end;

  TNodeNurbsPatchSurface = class(TNodeX3DNurbsSurfaceGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
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

  TNodeNurbsPositionInterpolator = class(TNodeX3DChildNode)
  private
    procedure EventSet_FractionReceive(Event: TVRMLEvent; Value: TVRMLField;
      const Time: TVRMLTime);
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFFloat, in } { }
    private FEventset_fraction: TVRMLEvent;
    public property Eventset_fraction: TVRMLEvent read FEventset_fraction;

    private FFdcontrolPoint: TSFNode;
    public property FdcontrolPoint: TSFNode read FFdcontrolPoint;

    private FFdknot: TMFDouble;
    public property Fdknot: TMFDouble read FFdknot;

    private FFdorder: TSFInt32;
    public property Fdorder: TSFInt32 read FFdorder;

    private FFdweight: TMFDouble;
    public property Fdweight: TMFDouble read FFdweight;

    { Event: SFVec3f, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;

    class function ForVRMLVersion(const Version: TVRMLVersion): boolean; override;
  end;
  TNodeNurbsPositionInterpolator_3 = TNodeNurbsPositionInterpolator;

  TNodeNurbsSet = class(TNodeX3DChildNode, INodeX3DBoundedObject)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFNode, in } { }
    private FEventaddGeometry: TVRMLEvent;
    public property EventaddGeometry: TVRMLEvent read FEventaddGeometry;

    { Event: MFNode, in } { }
    private FEventremoveGeometry: TVRMLEvent;
    public property EventremoveGeometry: TVRMLEvent read FEventremoveGeometry;

    { Implementation note: Fdgeometry is not enumerated in DirectEnumerateActive,
      as it's not actually rendered from NurbsSet node.
      Children here have to be placed elsewhere, in some Shape,
      to actually get enumerated as "active". }
    { }
    private FFdgeometry: TMFNode;
    public property Fdgeometry: TMFNode read FFdgeometry;

    private FFdtessellationScale: TSFFloat;
    public property FdtessellationScale: TSFFloat read FFdtessellationScale;

    private FFdbboxCenter: TSFVec3f;
    public property FdbboxCenter: TSFVec3f read FFdbboxCenter;

    private FFdbboxSize: TSFVec3f;
    public property FdbboxSize: TSFVec3f read FFdbboxSize;
  end;

  TNodeNurbsSurfaceInterpolator = class(TNodeX3DChildNode)
  private
    procedure EventSet_FractionReceive(Event: TVRMLEvent; Value: TVRMLField;
      const Time: TVRMLTime);
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFVec2f, in } { }
    private FEventset_fraction: TVRMLEvent;
    public property Eventset_fraction: TVRMLEvent read FEventset_fraction;

    private FFdcontrolPoint: TSFNode;
    public property FdcontrolPoint: TSFNode read FFdcontrolPoint;

    private FFdweight: TMFDouble;
    public property Fdweight: TMFDouble read FFdweight;

    { Event: SFVec3f, out } { }
    private FEventposition_changed: TVRMLEvent;
    public property Eventposition_changed: TVRMLEvent read FEventposition_changed;

    { Event: SFVec3f, out } { }
    private FEventnormal_changed: TVRMLEvent;
    public property Eventnormal_changed: TVRMLEvent read FEventnormal_changed;

    private FFduDimension: TSFInt32;
    public property FduDimension: TSFInt32 read FFduDimension;

    private FFduKnot: TMFDouble;
    public property FduKnot: TMFDouble read FFduKnot;

    private FFduOrder: TSFInt32;
    public property FduOrder: TSFInt32 read FFduOrder;

    private FFdvDimension: TSFInt32;
    public property FdvDimension: TSFInt32 read FFdvDimension;

    private FFdvKnot: TMFDouble;
    public property FdvKnot: TMFDouble read FFdvKnot;

    private FFdvOrder: TSFInt32;
    public property FdvOrder: TSFInt32 read FFdvOrder;
  end;

  TNodeNurbsSweptSurface = class(TNodeX3DParametricGeometryNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdcrossSectionCurve: TSFNode;
    public property FdcrossSectionCurve: TSFNode read FFdcrossSectionCurve;

    private FFdtrajectoryCurve: TSFNode;
    public property FdtrajectoryCurve: TSFNode read FFdtrajectoryCurve;

    private FFdccw: TSFBool;
    public property Fdccw: TSFBool read FFdccw;

    private FFdsolid: TSFBool;
    public property Fdsolid: TSFBool read FFdsolid;

    { Geometry node not implemented } { }
    function LocalBoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
  end;

  TNodeNurbsSwungSurface = class(TNodeX3DParametricGeometryNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdprofileCurve: TSFNode;
    public property FdprofileCurve: TSFNode read FFdprofileCurve;

    private FFdtrajectoryCurve: TSFNode;
    public property FdtrajectoryCurve: TSFNode read FFdtrajectoryCurve;

    private FFdccw: TSFBool;
    public property Fdccw: TSFBool read FFdccw;

    private FFdsolid: TSFBool;
    public property Fdsolid: TSFBool read FFdsolid;

    { Geometry node not implemented } { }
    function LocalBoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
  end;

  TNodeNurbsTextureCoordinate = class(TNodeX3DNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdcontrolPoint: TMFVec2f;
    public property FdcontrolPoint: TMFVec2f read FFdcontrolPoint;

    private FFdweight: TMFFloat;
    public property Fdweight: TMFFloat read FFdweight;

    private FFduDimension: TSFInt32;
    public property FduDimension: TSFInt32 read FFduDimension;

    private FFduKnot: TMFDouble;
    public property FduKnot: TMFDouble read FFduKnot;

    private FFduOrder: TSFInt32;
    public property FduOrder: TSFInt32 read FFduOrder;

    private FFdvDimension: TSFInt32;
    public property FdvDimension: TSFInt32 read FFdvDimension;

    private FFdvKnot: TMFDouble;
    public property FdvKnot: TMFDouble read FFdvKnot;

    private FFdvOrder: TSFInt32;
    public property FdvOrder: TSFInt32 read FFdvOrder;
  end;

  TNodeNurbsTrimmedSurface = class(TNodeX3DNurbsSurfaceGeometryNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFNode, in } { }
    private FEventaddTrimmingContour: TVRMLEvent;
    public property EventaddTrimmingContour: TVRMLEvent read FEventaddTrimmingContour;

    { Event: MFNode, in } { }
    private FEventremoveTrimmingContour: TVRMLEvent;
    public property EventremoveTrimmingContour: TVRMLEvent read FEventremoveTrimmingContour;

    private FFdtrimmingContour: TMFNode;
    public property FdtrimmingContour: TMFNode read FFdtrimmingContour;

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
procedure TNodeX3DNurbsControlCurveNode.CreateNode;
begin
  inherited;

  FFdcontrolPoint := TMFVec2d.Create(Self, 'controlPoint', []);
  Fields.Add(FFdcontrolPoint);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

procedure TNodeX3DNurbsSurfaceGeometryNode.CreateNode;
begin
  inherited;

  FFdcontrolPoint := TSFNode.Create(Self, 'controlPoint', [TNodeX3DCoordinateNode]);
   FdcontrolPoint.ChangesAlways := [chGeometry];
  Fields.Add(FFdcontrolPoint);

  FFdtexCoord := TSFNode.Create(Self, 'texCoord', [TNodeX3DTextureCoordinateNode, TNodeNurbsTextureCoordinate]);
   FdtexCoord.ChangesAlways := [chGeometry];
  Fields.Add(FFdtexCoord);

  FFduTessellation := TSFInt32.Create(Self, 'uTessellation', 0);
   FduTessellation.ChangesAlways := [chGeometry];
  Fields.Add(FFduTessellation);
  { X3D specification comment: (-Inf,Inf) }

  FFdvTessellation := TSFInt32.Create(Self, 'vTessellation', 0);
   FdvTessellation.ChangesAlways := [chGeometry];
  Fields.Add(FFdvTessellation);
  { X3D specification comment: (-Inf,Inf) }

  FFdweight := TMFDouble.Create(Self, 'weight', []);
   Fdweight.ChangesAlways := [chGeometry];
  Fields.Add(FFdweight);
  { X3D specification comment: (0,Inf) }

  FFdsolid := TSFBool.Create(Self, 'solid', true);
   Fdsolid.Exposed := false;
   Fdsolid.ChangesAlways := [chGeometry];
  Fields.Add(FFdsolid);

  FFduClosed := TSFBool.Create(Self, 'uClosed', false);
   FduClosed.Exposed := false;
   FduClosed.ChangesAlways := [chGeometry];
  Fields.Add(FFduClosed);

  FFduDimension := TSFInt32.Create(Self, 'uDimension', 0);
   FduDimension.Exposed := false;
   FduDimension.ChangesAlways := [chGeometry];
  Fields.Add(FFduDimension);
  { X3D specification comment: [0,Inf) }

  FFduKnot := TMFDouble.Create(Self, 'uKnot', []);
   FduKnot.Exposed := false;
   FduKnot.ChangesAlways := [chGeometry];
  Fields.Add(FFduKnot);
  { X3D specification comment: (-Inf,Inf) }

  FFduOrder := TSFInt32.Create(Self, 'uOrder', 3);
   FduOrder.Exposed := false;
   FduOrder.ChangesAlways := [chGeometry];
  Fields.Add(FFduOrder);
  { X3D specification comment: [2,Inf) }

  FFdvClosed := TSFBool.Create(Self, 'vClosed', false);
   FdvClosed.Exposed := false;
   FdvClosed.ChangesAlways := [chGeometry];
  Fields.Add(FFdvClosed);

  FFdvDimension := TSFInt32.Create(Self, 'vDimension', 0);
   FdvDimension.Exposed := false;
   FdvDimension.ChangesAlways := [chGeometry];
  Fields.Add(FFdvDimension);
  { X3D specification comment: [0,Inf) }

  FFdvKnot := TMFDouble.Create(Self, 'vKnot', []);
   FdvKnot.Exposed := false;
   FdvKnot.ChangesAlways := [chGeometry];
  Fields.Add(FFdvKnot);
  { X3D specification comment: (-Inf,Inf) }

  FFdvOrder := TSFInt32.Create(Self, 'vOrder', 3);
   FdvOrder.Exposed := false;
   FdvOrder.ChangesAlways := [chGeometry];
  Fields.Add(FFdvOrder);
  { X3D specification comment: [2,Inf) }
end;

procedure TNodeX3DNurbsSurfaceGeometryNode.DirectEnumerateActive(
  Func: TEnumerateChildrenFunction);
begin
  FdcontrolPoint.EnumerateValid(Func);
  FdtexCoord.EnumerateValid(Func);
end;

procedure TNodeContour2D.CreateNode;
begin
  inherited;

  FEventaddChildren := TVRMLEvent.Create(Self, 'addChildren', TMFNode, true);
  Events.Add(FEventaddChildren);

  FEventremoveChildren := TVRMLEvent.Create(Self, 'removeChildren', TMFNode, true);
  Events.Add(FEventremoveChildren);

  FFdchildren := TMFNode.Create(Self, 'children', [TNodeNurbsCurve2D, TNodeContourPolyline2D,
    { In VRML 97 these are allowed children too: }
    TNodePolyline2D, TNodeContour2D ]);
   Fdchildren.ChangesAlways := [chGeometry];
  Fields.Add(FFdchildren);

  DefaultContainerField := 'trimmingContour';
end;

class function TNodeContour2D.ClassNodeTypeName: string;
begin
  Result := 'Contour2D';
end;

function TNodeContour2D.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, VRML2Version, 1000);
end;

class function TNodeContour2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeContour2D.DirectEnumerateActive(
  Func: TEnumerateChildrenFunction);
begin
  Fdchildren.EnumerateValid(Func);
end;

{$define TGeometryNotImplemented := TNodeContour2D}
GeometryNotImplemented

procedure TNodeContourPolyline2D.CreateNode;
begin
  inherited;

  DefaultContainerField := 'geometry';
end;

class function TNodeContourPolyline2D.ClassNodeTypeName: string;
begin
  Result := 'ContourPolyline2D';
end;

function TNodeContourPolyline2D.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
end;

class function TNodeContourPolyline2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeCoordinateDouble.CreateNode;
begin
  inherited;

  FFdpoint := TMFVec3d.Create(Self, 'point', []);
   { Not really handled for now }
   Fdpoint.ChangesAlways := [chCoordinate];
  Fields.Add(FFdpoint);
  { X3D specification comment: (-Inf,Inf) }
end;

class function TNodeCoordinateDouble.ClassNodeTypeName: string;
begin
  Result := 'CoordinateDouble';
end;

function TNodeCoordinateDouble.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
end;

class function TNodeCoordinateDouble.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TNodeCoordinateDouble.CoordCount: Cardinal;
begin
  Result := FdPoint.Items.Count;
end;

procedure TNodeNurbsCurve.CreateNode;
begin
  inherited;

  FFdcontrolPoint := TSFNode.Create(Self, 'controlPoint', [TNodeX3DCoordinateNode]);
   FdcontrolPoint.ChangesAlways := [chGeometry];
  Fields.Add(FFdcontrolPoint);

  FFdtessellation := TSFInt32.Create(Self, 'tessellation', 0);
   Fdtessellation.ChangesAlways := [chGeometry];
  Fields.Add(FFdtessellation);
  { X3D specification comment: (-Inf,Inf) }

  FFdweight := TMFDouble.Create(Self, 'weight', []);
   Fdweight.ChangesAlways := [chGeometry];
  Fields.Add(FFdweight);
  { X3D specification comment: (0,Inf) }

  FFdclosed := TSFBool.Create(Self, 'closed', false);
   Fdclosed.Exposed := false;
   Fdclosed.ChangesAlways := [chGeometry];
  Fields.Add(FFdclosed);

  FFdknot := TMFDouble.Create(Self, 'knot', []);
   Fdknot.Exposed := false;
   Fdknot.ChangesAlways := [chGeometry];
  Fields.Add(FFdknot);
  { X3D specification comment: (-Inf,Inf) }

  FFdorder := TSFInt32.Create(Self, 'order', 3);
   Fdorder.Exposed := false;
   Fdorder.ChangesAlways := [chGeometry];
  Fields.Add(FFdorder);
  { X3D specification comment: [2,Inf) }
end;

class function TNodeNurbsCurve.ClassNodeTypeName: string;
begin
  Result := 'NurbsCurve';
end;

function TNodeNurbsCurve.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
end;

class function TNodeNurbsCurve.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

class function TNodeNurbsCurve.ForVRMLVersion(const Version: TVRMLVersion): boolean;
begin
  Result := Version.Major >= 3;
end;

procedure TNodeNurbsCurve.DirectEnumerateActive(
  Func: TEnumerateChildrenFunction);
begin
  FdcontrolPoint.EnumerateValid(Func);
end;

{ Convert X3D or VRML 97 NurbsCurve to LineSet. }
procedure NurbsCurveProxy(
  const LS: TNodeLineSet;
  const ControlPoint: TDynVector3SingleArray;
  const Tessellation, Order: LongInt;
  const FieldKnot, Weight: TDynDoubleArray);
var
  ResultCoord: TNodeCoordinate;
  Tess: Cardinal;
  I: Integer;
  Increase: Double;
  Knot: TDynDoubleArray;
begin
  if ControlPoint.Count = 0 then Exit;

  if Order < 2 then
  begin
    VRMLWarning(vwSerious, 'NURBS order must be >= 2');
    Exit;
  end;

  { We can be sure now that
    - we have ControlPoint, non-nil, with at least 1 point.
    - we have Order >= 2 }

  { calculate correct Knot vector }
  Knot := TDynDoubleArray.Create;
  Knot.Assign(FieldKnot);
  NurbsKnotIfNeeded(Knot, ControlPoint.Count, Order, nkEndpointUniform);

  { calculate tesselation: Tess, Increase }
  Tess := ActualTessellation(Tessellation, ControlPoint.Count);
  Increase := (Knot.Last - Knot.First) / (Tess - 1);

  { make resulting Coordinate node }
  ResultCoord := TNodeCoordinate.Create('', LS.WWWBasePath);
  LS.FdCoord.Value := ResultCoord;

  { calculate result Coordinate.point field }
  ResultCoord.FdPoint.Items.Count := Tess;
  for I := 0 to Tess - 1 do
    ResultCoord.FdPoint.Items.Items[I] :=
      NurbsCurvePoint(ControlPoint,
        Knot.First + Increase * I,
        Order, Knot, Weight, nil);

  { set LineSet.vertexCount (otherwise it's coord will be ignored) }
  LS.FdVertexCount.Items.Add(Tess);

  FreeAndNil(Knot);
end;

function TNodeNurbsCurve.Proxy(var State: TVRMLGraphTraverseState;
  const OverTriangulate: boolean): TVRMLGeometryNode;
var
  ControlPoint: TDynVector3SingleArray;
begin
  Result := TNodeLineSet.Create(NodeName, WWWBasePath);
  try
    { TODO: we should handle here all TNodeX3DCoordinateNode }
    if (FdControlPoint.Value <> nil) and
       (FdControlPoint.Value is TNodeCoordinate) then
      ControlPoint := TNodeCoordinate(FdControlPoint.Value).FdPoint.Items else
      Exit;

    NurbsCurveProxy(TNodeLineSet(Result), ControlPoint, FdTessellation.Value,
      FdOrder.Value, FdKnot.Items, FdWeight.Items);
  except FreeAndNil(Result); raise end;
end;

function TNodeNurbsCurve.ProxyUsesOverTriangulate: boolean;
begin
  Result := false;
end;

{ We cannot simply override Coord() and let bounding box be calculated
  based on it. It would fail for curves with weigths. }
function TNodeNurbsCurve.LocalBoundingBox(State: TVRMLGraphTraverseState;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D;
begin
  if (FdControlPoint.Value <> nil) and
     (FdControlPoint.Value is TNodeCoordinate) then
    Result := NurbsBoundingBox(TNodeCoordinate(FdControlPoint.Value).FdPoint.Items, FdWeight.Items) else
    Result := EmptyBox3D;
end;

{ We cannot simply override Coord() and let bounding box be calculated
  based on it. It would fail for curves with weigths. }
function TNodeNurbsCurve.BoundingBox(State: TVRMLGraphTraverseState;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D;
begin
  if (FdControlPoint.Value <> nil) and
     (FdControlPoint.Value is TNodeCoordinate) then
    Result := NurbsBoundingBox(TNodeCoordinate(FdControlPoint.Value).FdPoint.Items, FdWeight.Items, State.Transform) else
    Result := EmptyBox3D;
end;

function TNodeNurbsCurve.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal;
begin
  Result := 0;
end;

procedure TNodeNurbsCurve2D.CreateNode;
begin
  inherited;

  FFdtessellation := TSFInt32.Create(Self, 'tessellation', 0);
  Fields.Add(FFdtessellation);
  { X3D specification comment: (-Inf,Inf) }

  FFdweight := TMFDouble.Create(Self, 'weight', []);
  Fields.Add(FFdweight);
  { X3D specification comment: (0,Inf) }

  FFdclosed := TSFBool.Create(Self, 'closed', false);
   Fdclosed.Exposed := false;
  Fields.Add(FFdclosed);

  FFdknot := TMFDouble.Create(Self, 'knot', []);
   Fdknot.Exposed := false;
  Fields.Add(FFdknot);
  { X3D specification comment: (-Inf,Inf) }

  FFdorder := TSFInt32.Create(Self, 'order', 3);
   Fdorder.Exposed := false;
  Fields.Add(FFdorder);
  { X3D specification comment: [2,Inf) }

  DefaultContainerField := 'geometry';
end;

class function TNodeNurbsCurve2D.ClassNodeTypeName: string;
begin
  Result := 'NurbsCurve2D';
end;

function TNodeNurbsCurve2D.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, VRML2Version, 1000);
end;

class function TNodeNurbsCurve2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeNurbsOrientationInterpolator.CreateNode;
begin
  inherited;

  FEventset_fraction := TVRMLEvent.Create(Self, 'set_fraction', TSFFloat, true);
  Events.Add(FEventset_fraction);

  FFdcontrolPoint := TSFNode.Create(Self, 'controlPoint', [TNodeX3DCoordinateNode]);
  Fields.Add(FFdcontrolPoint);

  FFdknot := TMFDouble.Create(Self, 'knot', []);
  Fields.Add(FFdknot);
  { X3D specification comment: (-Inf,Inf) }

  FFdorder := TSFInt32.Create(Self, 'order', 3);
  Fields.Add(FFdorder);
  { X3D specification comment: (2,Inf) }

  FFdweight := TMFDouble.Create(Self, 'weight', []);
  Fields.Add(FFdweight);
  { X3D specification comment: (-Inf,Inf) }

  FEventvalue_changed := TVRMLEvent.Create(Self, 'value_changed', TSFRotation, false);
  Events.Add(FEventvalue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.OnReceive.Add(@EventSet_FractionReceive);
end;

class function TNodeNurbsOrientationInterpolator.ClassNodeTypeName: string;
begin
  Result := 'NurbsOrientationInterpolator';
end;

function TNodeNurbsOrientationInterpolator.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
end;

class function TNodeNurbsOrientationInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeNurbsOrientationInterpolator.DirectEnumerateActive(
  Func: TEnumerateChildrenFunction);
begin
  FdcontrolPoint.EnumerateValid(Func);
end;

procedure TNodeNurbsOrientationInterpolator.EventSet_FractionReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
var
  ControlPoint: TDynVector3SingleArray;
  Knot: TDynDoubleArray;
  Tangent: TVector3Single;
  Orientation: TVector4Single;
begin
  if not EventValue_Changed.SendNeeded then Exit;

  { TODO: we should handle here all TNodeX3DCoordinateNode }
  if (FdControlPoint.Value <> nil) and
     (FdControlPoint.Value is TNodeCoordinate) then
    ControlPoint := TNodeCoordinate(FdControlPoint.Value).FdPoint.Items else
    Exit;

  if ControlPoint.Count = 0 then Exit;

  if FdOrder.Value < 2 then
  begin
    VRMLWarning(vwSerious, 'NURBS order must be >= 2');
    Exit;
  end;

  { We can be sure now that
    - we have ControlPoint, non-nil, with at least 1 point.
    - we have Order >= 2 }

  { calculate correct Knot vector }
  Knot := TDynDoubleArray.Create;
  Knot.Assign(FdKnot.Items);
  NurbsKnotIfNeeded(Knot, ControlPoint.Count, FdOrder.Value, nkEndpointUniform);

  NurbsCurvePoint(ControlPoint,
    (Value as TSFFloat).Value,
    FdOrder.Value, Knot, FdWeight.Items, @Tangent);

  FreeAndNil(Knot);

  { calculate Orientation from Tangent.
    For this, we treat Tangent like "camera direction", and we have to set
    "camera up" as anything orthogonal to direction. }
  Orientation := CamDirUp2Orient(Tangent, AnyOrthogonalVector(Tangent));

  EventValue_Changed.Send(Orientation, Time);
end;

procedure TNodeNurbsPatchSurface.CreateNode;
begin
  inherited;
end;

class function TNodeNurbsPatchSurface.ClassNodeTypeName: string;
begin
  Result := 'NurbsPatchSurface';
end;

function TNodeNurbsPatchSurface.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
end;

class function TNodeNurbsPatchSurface.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

{ Converting X3D NurbsPatchSurface and VRML 97 NurbsSurface to
  IndexedQuadSet. This is for Proxy methods implementation. }
procedure NurbsPatchSurfaceProxy(
  const QS: TNodeIndexedQuadSet;
  const ControlPoint: TDynVector3SingleArray;
  const UTessellation, VTessellation, UDimension, VDimension, UOrder, VOrder: LongInt;
  const FieldUKnot, FieldVKnot, Weight: TDynDoubleArray;
  const PossiblyUClosed, PossiblyVClosed, Solid, Ccw: boolean;
  const TexCoord: TVRMLNode);

const
  { This has to be slightly larger than normal epsilon
    (test nurbs_dune_primitives.x3dv). }
  ClosedCheckEpsilon = 0.000001;

  procedure LogClosed(Value: boolean);
  begin
    if Log then
    begin
      if Value then
        WritelnLog('NURBS', 'Checking if NURBS surface is really closed (because it''s VRML 97 NurbsSurface or NurbsPatchSurface with .xClosed is TRUE): no, ignoring (otherwise would cause invalid geometry/normals)') else
        WritelnLog('NURBS', 'Checking if NURBS surface is really closed: yes');
    end;
  end;

  function CalculateUClosed: boolean;
  var
    J: Integer;
  begin
    Result := PossiblyUClosed;
    if Result then
    begin
      for J := 0 to VDimension - 1 do
        if not VectorsEqual(
          ControlPoint.Items[                 J * UDimension],
          ControlPoint.Items[UDimension - 1 + J * UDimension], ClosedCheckEpsilon) then
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
        if not VectorsEqual(
          ControlPoint.Items[I                                ],
          ControlPoint.Items[I + (VDimension - 1) * UDimension], ClosedCheckEpsilon) then
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
  ResultCoord, ResultNormal: TDynVector3SingleArray;
  ResultIndex: TDynLongIntArray;
  I, J, NextIndex: Cardinal;
  UIncrease, VIncrease: Double;
  UKnot, VKnot: TDynDoubleArray;
  ResultTexCoord: TDynVector2SingleArray;
  Normal: TVector3Single;
begin
  if ControlPoint.Count = 0 then Exit;

  if UDimension * VDimension <>
     Integer(ControlPoint.Count) then
  begin
    VRMLWarning(vwSerious, Format('Number of coordinates in Nurbs[Patch]Surface.controlPoint (%d) doesn''t match uDimension * vDimension (%d * %d = %d)',
      [ ControlPoint.Count,
        UDimension,  VDimension,
        UDimension * VDimension ]));
    Exit;
  end;

  if (UDimension < 0) or
     (VDimension < 0) then
  begin
    VRMLWarning(vwSerious, 'Nurbs[Patch]Surface.u/vDimension is < 0');
    Exit;
  end;

  if (UOrder < 2) or
     (VOrder < 2) then
  begin
    VRMLWarning(vwSerious, 'NURBS order must be >= 2');
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
  UKnot := TDynDoubleArray.Create;
  UKnot.Assign(FieldUKnot);
  NurbsKnotIfNeeded(UKnot, UDimension, UOrder, nkEndpointUniform);
  VKnot := TDynDoubleArray.Create;
  VKnot.Assign(FieldVKnot);
  NurbsKnotIfNeeded(VKnot, VDimension, VOrder, nkEndpointUniform);

  { calculate tesselation: xTess, xIncrease }
  UTess := ActualTessellation(UTessellation, UDimension);
  VTess := ActualTessellation(VTessellation, VDimension);
  UIncrease := (UKnot.Last - UKnot.First) / (UTess - 1);
  VIncrease := (VKnot.Last - VKnot.First) / (VTess - 1);

  { make resulting Coordinate and Normal nodes }
  QS.FdCoord.Value := TNodeCoordinate.Create('', QS.WWWBasePath);
  ResultCoord := (QS.FdCoord.Value as TNodeCoordinate).FdPoint.Items;
  ResultCoord.Count := UTess * VTess;

  QS.FdNormal.Value := TNodeNormal.Create('', QS.WWWBasePath);
  ResultNormal := (QS.FdNormal.Value as TNodeNormal).FdVector.Items;
  ResultNormal.Count := UTess * VTess;

  { calculate result Coordinate.point and Normal.vector field }
  for I := 0 to UTess - 1 do
    for J := 0 to VTess - 1 do
    begin
      ResultCoord.Items[I + J * UTess] :=
        NurbsSurfacePoint(ControlPoint,
          UDimension,
          VDimension,
          UKnot.First + UIncrease * I,
          VKnot.First + VIncrease * J,
          UOrder,
          VOrder,
          UKnot,
          VKnot,
          Weight,
          @Normal);
      if Ccw then
        ResultNormal.Items[I + J * UTess] := Normal else
        ResultNormal.Items[I + J * UTess] := -Normal;
    end;

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
      ResultIndex.Items[NextIndex] := MakeIndex(I  , J  ); Inc(NextIndex);
      ResultIndex.Items[NextIndex] := MakeIndex(I-1, J  ); Inc(NextIndex);
      ResultIndex.Items[NextIndex] := MakeIndex(I-1, J-1); Inc(NextIndex);
      ResultIndex.Items[NextIndex] := MakeIndex(I  , J-1); Inc(NextIndex);
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
    QS.FdTexCoord.Value := TNodeTextureCoordinate.Create('', QS.WWWBasePath);
    ResultTexCoord := (QS.FdTexCoord.Value as TNodeTextureCoordinate).FdPoint.Items;
    ResultTexCoord.Count := UTess * VTess;
    for I := 0 to UTess - 1 do
      for J := 0 to VTess - 1 do
        ResultTexCoord.Items[I + J * UTess] := Vector2Single(
          I / (UTess - 1),
          J / (VTess - 1));
  end;

  FreeAndNil(UKnot);
  FreeAndNil(VKnot);
end;

function TNodeNurbsPatchSurface.Proxy(var State: TVRMLGraphTraverseState;
  const OverTriangulate: boolean): TVRMLGeometryNode;
var
  ControlPoint: TDynVector3SingleArray;
begin
  Result := TNodeIndexedQuadSet.Create(NodeName, WWWBasePath);
  try
    { TODO: we should handle here all TNodeX3DCoordinateNode }
    if (FdControlPoint.Value <> nil) and
       (FdControlPoint.Value is TNodeCoordinate) then
    begin
      ControlPoint := TNodeCoordinate(FdControlPoint.Value).FdPoint.Items;
      NurbsPatchSurfaceProxy(TNodeIndexedQuadSet(Result),
        ControlPoint,
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

    { Note: In case of null / invalid / unhandled FdControlPoint.Value,
      we still return valid (just empty) IndexedQuadSet node. }
  except FreeAndNil(Result); raise end;
end;

function TNodeNurbsPatchSurface.ProxyUsesOverTriangulate: boolean;
begin
  Result := false;
end;

{ We cannot simply override Coord() and let bounding box be calculated
  based on it. It would fail for curves with weigths. }
function TNodeNurbsPatchSurface.LocalBoundingBox(State: TVRMLGraphTraverseState;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D;
begin
  if (FdControlPoint.Value <> nil) and
     (FdControlPoint.Value is TNodeCoordinate) then
    Result := NurbsBoundingBox(TNodeCoordinate(FdControlPoint.Value).FdPoint.Items, FdWeight.Items) else
    Result := EmptyBox3D;
end;

{ We cannot simply override Coord() and let bounding box be calculated
  based on it. It would fail for curves with weigths. }
function TNodeNurbsPatchSurface.BoundingBox(State: TVRMLGraphTraverseState;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D;
begin
  if (FdControlPoint.Value <> nil) and
     (FdControlPoint.Value is TNodeCoordinate) then
    Result := NurbsBoundingBox(TNodeCoordinate(FdControlPoint.Value).FdPoint.Items, FdWeight.Items, State.Transform) else
    Result := EmptyBox3D;
end;

function TNodeNurbsPatchSurface.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
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

procedure TNodeNurbsPositionInterpolator.CreateNode;
begin
  inherited;

  FEventset_fraction := TVRMLEvent.Create(Self, 'set_fraction', TSFFloat, true);
  Events.Add(FEventset_fraction);

  FFdcontrolPoint := TSFNode.Create(Self, 'controlPoint', [TNodeX3DCoordinateNode]);
  Fields.Add(FFdcontrolPoint);

  FFdknot := TMFDouble.Create(Self, 'knot', []);
  Fields.Add(FFdknot);
  { X3D specification comment: (-Inf,Inf) }

  FFdorder := TSFInt32.Create(Self, 'order', 3);
  Fields.Add(FFdorder);
  { X3D specification comment: (2,Inf) }

  FFdweight := TMFDouble.Create(Self, 'weight', []);
  Fields.Add(FFdweight);
  { X3D specification comment: (-Inf,Inf) }

  FEventvalue_changed := TVRMLEvent.Create(Self, 'value_changed', TSFVec3f, false);
  Events.Add(FEventvalue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.OnReceive.Add(@EventSet_FractionReceive);
end;

class function TNodeNurbsPositionInterpolator.ClassNodeTypeName: string;
begin
  Result := 'NurbsPositionInterpolator';
end;

function TNodeNurbsPositionInterpolator.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
end;

class function TNodeNurbsPositionInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

class function TNodeNurbsPositionInterpolator.ForVRMLVersion(const Version: TVRMLVersion): boolean;
begin
  Result := Version.Major >= 3;
end;

procedure TNodeNurbsPositionInterpolator.DirectEnumerateActive(
  Func: TEnumerateChildrenFunction);
begin
  FdcontrolPoint.EnumerateValid(Func);
end;

procedure TNodeNurbsPositionInterpolator.EventSet_FractionReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
var
  ControlPoint: TDynVector3SingleArray;
  Knot: TDynDoubleArray;
  OutputValue: TVector3Single;
begin
  if not EventValue_Changed.SendNeeded then Exit;

  if (FdControlPoint.Value <> nil) and
     (FdControlPoint.Value is TNodeCoordinate) then
    ControlPoint := TNodeCoordinate(FdControlPoint.Value).FdPoint.Items else
    Exit;

  if ControlPoint.Count = 0 then Exit;

  if FdOrder.Value < 2 then
  begin
    VRMLWarning(vwSerious, 'NURBS order must be >= 2');
    Exit;
  end;

  { We can be sure now that
    - we have ControlPoint, non-nil, with at least 1 point.
    - we have Order >= 2 }

  { calculate correct Knot vector }
  Knot := TDynDoubleArray.Create;
  Knot.Assign(FdKnot.Items);
  NurbsKnotIfNeeded(Knot, ControlPoint.Count, FdOrder.Value, nkEndpointUniform);

  OutputValue := NurbsCurvePoint(ControlPoint, (Value as TSFFloat).Value,
    FdOrder.Value, Knot, FdWeight.Items, nil);

  FreeAndNil(Knot);

  EventValue_Changed.Send(OutputValue, Time);
end;

procedure TNodeNurbsSet.CreateNode;
begin
  inherited;

  FEventaddGeometry := TVRMLEvent.Create(Self, 'addGeometry', TMFNode, true);
  Events.Add(FEventaddGeometry);

  FEventremoveGeometry := TVRMLEvent.Create(Self, 'removeGeometry', TMFNode, true);
  Events.Add(FEventremoveGeometry);

  FFdgeometry := TMFNode.Create(Self, 'geometry', [TNodeX3DParametricGeometryNode]);
  Fields.Add(FFdgeometry);

  FFdtessellationScale := TSFFloat.Create(Self, 'tessellationScale', 1.0);
  Fields.Add(FFdtessellationScale);
  { X3D specification comment: (0,Inf) }

  FFdbboxCenter := TSFVec3f.Create(Self, 'bboxCenter', Vector3Single(0, 0, 0));
  FFdbboxCenter.Exposed := false;
  Fields.Add(FFdbboxCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdbboxSize := TSFVec3f.Create(Self, 'bboxSize', Vector3Single(-1, -1, -1));
  FFdbboxSize.Exposed := false;
  Fields.Add(FFdbboxSize);
  { X3D specification comment: [0,Inf) or -1 -1 -1 }

  DefaultContainerField := 'children';
end;

class function TNodeNurbsSet.ClassNodeTypeName: string;
begin
  Result := 'NurbsSet';
end;

function TNodeNurbsSet.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
end;

class function TNodeNurbsSet.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeNurbsSurfaceInterpolator.CreateNode;
begin
  inherited;

  FEventset_fraction := TVRMLEvent.Create(Self, 'set_fraction', TSFVec2f, true);
  Events.Add(FEventset_fraction);

  FFdcontrolPoint := TSFNode.Create(Self, 'controlPoint', [TNodeX3DCoordinateNode]);
  Fields.Add(FFdcontrolPoint);

  FFdweight := TMFDouble.Create(Self, 'weight', []);
  Fields.Add(FFdweight);
  { X3D specification comment: (-Inf,Inf) }

  FEventposition_changed := TVRMLEvent.Create(Self, 'position_changed', TSFVec3f, false);
  Events.Add(FEventposition_changed);

  FEventnormal_changed := TVRMLEvent.Create(Self, 'normal_changed', TSFVec3f, false);
  Events.Add(FEventnormal_changed);

  FFduDimension := TSFInt32.Create(Self, 'uDimension', 0);
  FFduDimension.Exposed := false;
  Fields.Add(FFduDimension);
  { X3D specification comment: [0,Inf) }

  FFduKnot := TMFDouble.Create(Self, 'uKnot', []);
  FFduKnot.Exposed := false;
  Fields.Add(FFduKnot);
  { X3D specification comment: (-Inf,Inf) }

  FFduOrder := TSFInt32.Create(Self, 'uOrder', 3);
  FFduOrder.Exposed := false;
  Fields.Add(FFduOrder);
  { X3D specification comment: [2,Inf) }

  FFdvDimension := TSFInt32.Create(Self, 'vDimension', 0);
  FFdvDimension.Exposed := false;
  Fields.Add(FFdvDimension);
  { X3D specification comment: [0,Inf) }

  FFdvKnot := TMFDouble.Create(Self, 'vKnot', []);
  FFdvKnot.Exposed := false;
  Fields.Add(FFdvKnot);
  { X3D specification comment: (-Inf,Inf) }

  FFdvOrder := TSFInt32.Create(Self, 'vOrder', 3);
  FFdvOrder.Exposed := false;
  Fields.Add(FFdvOrder);
  { X3D specification comment: [2,Inf) }

  DefaultContainerField := 'children';

  EventSet_Fraction.OnReceive.Add(@EventSet_FractionReceive);
end;

class function TNodeNurbsSurfaceInterpolator.ClassNodeTypeName: string;
begin
  Result := 'NurbsSurfaceInterpolator';
end;

function TNodeNurbsSurfaceInterpolator.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
end;

class function TNodeNurbsSurfaceInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeNurbsSurfaceInterpolator.DirectEnumerateActive(
  Func: TEnumerateChildrenFunction);
begin
  FdcontrolPoint.EnumerateValid(Func);
end;

procedure TNodeNurbsSurfaceInterpolator.EventSet_FractionReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
var
  ControlPoint: TDynVector3SingleArray;
  OutputNormal: TVector3Single;
  OutputPosition: TVector3Single;
  UKnot, VKnot: TDynDoubleArray;
begin
  if not (EventPosition_Changed.SendNeeded or
          EventNormal_Changed.SendNeeded) then Exit;

  { TODO: we should handle here all TNodeX3DCoordinateNode }
  if (FdControlPoint.Value <> nil) and
     (FdControlPoint.Value is TNodeCoordinate) then
    ControlPoint := TNodeCoordinate(FdControlPoint.Value).FdPoint.Items else
    Exit;

  if ControlPoint.Count = 0 then Exit;

  if FdUDimension.Value * FdVDimension.Value <>
     Integer(ControlPoint.Count) then
  begin
    VRMLWarning(vwSerious, Format('Number of coordinates in NurbsSurfaceInterpolator.controlPoint (%d) doesn''t match uDimension * vDimension (%d * %d = %d)',
      [ ControlPoint.Count,
        FdUDimension.Value,  FdVDimension.Value,
        FdUDimension.Value * FdVDimension.Value ]));
    Exit;
  end;

  if (FdUDimension.Value < 0) or
     (FdVDimension.Value < 0) then
  begin
    VRMLWarning(vwSerious, 'NurbsSurfaceInterpolator.u/vDimension is < 0');
    Exit;
  end;

  if (FdUOrder.Value < 2) or
     (FdVOrder.Value < 2) then
  begin
    VRMLWarning(vwSerious, 'NURBS order must be >= 2');
    Exit;
  end;

  { We can be sure now that we have
    - correct ControlPoint, non-nil, with at least 1 point.
    - uDimension, vDimension match ControlPoint count, and are > 0.
    - we have Order >= 2.
  }

  { calculate correct UKnot, VKnot vectors }
  UKnot := TDynDoubleArray.Create;
  UKnot.Assign(FdUKnot.Items);
  NurbsKnotIfNeeded(UKnot, FdUDimension.Value, FdUOrder.Value, nkEndpointUniform);
  VKnot := TDynDoubleArray.Create;
  VKnot.Assign(FdVKnot.Items);
  NurbsKnotIfNeeded(VKnot, FdVDimension.Value, FdVOrder.Value, nkEndpointUniform);

  OutputPosition :=
    NurbsSurfacePoint(ControlPoint,
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

procedure TNodeNurbsSweptSurface.CreateNode;
begin
  inherited;

  FFdcrossSectionCurve := TSFNode.Create(Self, 'crossSectionCurve', [TNodeX3DNurbsControlCurveNode]);
   FdcrossSectionCurve.ChangesAlways := [chGeometry];
  Fields.Add(FFdcrossSectionCurve);

  FFdtrajectoryCurve := TSFNode.Create(Self, 'trajectoryCurve', [TNodeNurbsCurve]);
   FdtrajectoryCurve.ChangesAlways := [chGeometry];
  Fields.Add(FFdtrajectoryCurve);

  FFdccw := TSFBool.Create(Self, 'ccw', true);
   Fdccw.Exposed := false;
   Fdccw.ChangesAlways := [chGeometry];
  Fields.Add(FFdccw);

  FFdsolid := TSFBool.Create(Self, 'solid', true);
   Fdsolid.Exposed := false;
   Fdsolid.ChangesAlways := [chGeometry];
  Fields.Add(FFdsolid);
end;

class function TNodeNurbsSweptSurface.ClassNodeTypeName: string;
begin
  Result := 'NurbsSweptSurface';
end;

function TNodeNurbsSweptSurface.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
end;

class function TNodeNurbsSweptSurface.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeNurbsSweptSurface.DirectEnumerateActive(
  Func: TEnumerateChildrenFunction);
begin
  FdcrossSectionCurve.EnumerateValid(Func);
  FdtrajectoryCurve.EnumerateValid(Func);
end;

{$define TGeometryNotImplemented := TNodeNurbsSweptSurface}
GeometryNotImplemented

procedure TNodeNurbsSwungSurface.CreateNode;
begin
  inherited;

  FFdprofileCurve := TSFNode.Create(Self, 'profileCurve', [TNodeX3DNurbsControlCurveNode]);
   FdprofileCurve.ChangesAlways := [chGeometry];
  Fields.Add(FFdprofileCurve);

  FFdtrajectoryCurve := TSFNode.Create(Self, 'trajectoryCurve', [TNodeX3DNurbsControlCurveNode]);
   FdtrajectoryCurve.ChangesAlways := [chGeometry];
  Fields.Add(FFdtrajectoryCurve);

  FFdccw := TSFBool.Create(Self, 'ccw', true);
   Fdccw.Exposed := false;
   Fdccw.ChangesAlways := [chGeometry];
  Fields.Add(FFdccw);

  FFdsolid := TSFBool.Create(Self, 'solid', true);
   Fdsolid.Exposed := false;
   Fdsolid.ChangesAlways := [chGeometry];
  Fields.Add(FFdsolid);
end;

class function TNodeNurbsSwungSurface.ClassNodeTypeName: string;
begin
  Result := 'NurbsSwungSurface';
end;

function TNodeNurbsSwungSurface.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
end;

class function TNodeNurbsSwungSurface.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeNurbsSwungSurface.DirectEnumerateActive(
  Func: TEnumerateChildrenFunction);
begin
  FdprofileCurve.EnumerateValid(Func);
  FdtrajectoryCurve.EnumerateValid(Func);
end;

{$define TGeometryNotImplemented := TNodeNurbsSwungSurface}
GeometryNotImplemented

procedure TNodeNurbsTextureCoordinate.CreateNode;
begin
  inherited;

  FFdcontrolPoint := TMFVec2f.Create(Self, 'controlPoint', []);
  Fields.Add(FFdcontrolPoint);
  { X3D specification comment: (-Inf,Inf) }

  FFdweight := TMFFloat.Create(Self, 'weight', []);
  Fields.Add(FFdweight);
  { X3D specification comment: (0,Inf) }

  FFduDimension := TSFInt32.Create(Self, 'uDimension', 0);
  FFduDimension.Exposed := false;
  Fields.Add(FFduDimension);
  { X3D specification comment: [0,Inf) }

  FFduKnot := TMFDouble.Create(Self, 'uKnot', []);
  FFduKnot.Exposed := false;
  Fields.Add(FFduKnot);
  { X3D specification comment: (-Inf,Inf) }

  FFduOrder := TSFInt32.Create(Self, 'uOrder', 3);
  FFduOrder.Exposed := false;
  Fields.Add(FFduOrder);
  { X3D specification comment: [2,Inf) }

  FFdvDimension := TSFInt32.Create(Self, 'vDimension', 0);
  FFdvDimension.Exposed := false;
  Fields.Add(FFdvDimension);
  { X3D specification comment: [0,Inf) }

  FFdvKnot := TMFDouble.Create(Self, 'vKnot', []);
  FFdvKnot.Exposed := false;
  Fields.Add(FFdvKnot);
  { X3D specification comment: (-Inf,Inf) }

  FFdvOrder := TSFInt32.Create(Self, 'vOrder', 3);
  FFdvOrder.Exposed := false;
  Fields.Add(FFdvOrder);
  { X3D specification comment: [2,Inf) }

  DefaultContainerField := 'texCoord';
end;

class function TNodeNurbsTextureCoordinate.ClassNodeTypeName: string;
begin
  Result := 'NurbsTextureCoordinate';
end;

function TNodeNurbsTextureCoordinate.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
end;

class function TNodeNurbsTextureCoordinate.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeNurbsTrimmedSurface.CreateNode;
begin
  inherited;

  FEventaddTrimmingContour := TVRMLEvent.Create(Self, 'addTrimmingContour', TMFNode, true);
  Events.Add(FEventaddTrimmingContour);

  FEventremoveTrimmingContour := TVRMLEvent.Create(Self, 'removeTrimmingContour', TMFNode, true);
  Events.Add(FEventremoveTrimmingContour);

  FFdtrimmingContour := TMFNode.Create(Self, 'trimmingContour', [TNodeContour2D]);
   FdtrimmingContour.ChangesAlways := [chGeometry];
  Fields.Add(FFdtrimmingContour);
end;

class function TNodeNurbsTrimmedSurface.ClassNodeTypeName: string;
begin
  Result := 'NurbsTrimmedSurface';
end;

function TNodeNurbsTrimmedSurface.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
end;

class function TNodeNurbsTrimmedSurface.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeNurbsTrimmedSurface.DirectEnumerateActive(
  Func: TEnumerateChildrenFunction);
begin
  FdtrimmingContour.EnumerateValid(Func);
end;

{$define TGeometryNotImplemented := TNodeNurbsTrimmedSurface}
GeometryNotImplemented

procedure RegisterNURBSNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeContour2D,
    TNodeContourPolyline2D,
    TNodeCoordinateDouble,
    TNodeNurbsCurve,
    TNodeNurbsCurve2D,
    TNodeNurbsOrientationInterpolator,
    TNodeNurbsPatchSurface,
    TNodeNurbsPositionInterpolator,
    TNodeNurbsSet,
    TNodeNurbsSurfaceInterpolator,
    TNodeNurbsSweptSurface,
    TNodeNurbsSwungSurface,
    TNodeNurbsTextureCoordinate,
    TNodeNurbsTrimmedSurface
  ]);
end;

{$endif read_implementation}

{$endif KAMBI_HAS_NURBS}
