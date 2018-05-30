{
  Copyright 2008-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ NurbsCurve2D is shared with VRML 97 NURBS definition.
  The rest is, for now, completely separate from VRML 97 NURBS definition
  in x3dnodes_97_nurbs.inc. }

{$ifdef read_interface}
  TNurbsSurfaceNode = class;

  { Base type for nodes that provide control curve information in 2D space. }
  TAbstractNurbsControlCurveNode = class(TAbstractNode)
  public
    procedure CreateNode; override;

    strict private FFdControlPoint: TMFVec2d;
    public property FdControlPoint: TMFVec2d read FFdControlPoint;

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
    procedure CreateNode; override;

    function Proxy(var State: TX3DGraphTraverseState;
      const OverTriangulate: boolean): TAbstractGeometryNode; override;
    function ProxyUsesOverTriangulate: boolean; override;
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function BoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function InternalCoord(State: TX3DGraphTraverseState;
      out ACoord: TMFVec3f): boolean; override;
    function SolidField: TSFBool; override;

    strict private FFdControlPoint: TSFNode;
    public property FdControlPoint: TSFNode read FFdControlPoint;

    strict private FFdTexCoord: TSFNode;
    public property FdTexCoord: TSFNode read FFdTexCoord;

    strict private FFdUTessellation: TSFInt32;
    public property FdUTessellation: TSFInt32 read FFdUTessellation;

    strict private FFdVTessellation: TSFInt32;
    public property FdVTessellation: TSFInt32 read FFdVTessellation;

    strict private FFdWeight: TMFDouble;
    public property FdWeight: TMFDouble read FFdWeight;

    strict private FFdSolid: TSFBool;
    public property FdSolid: TSFBool read FFdSolid;

    strict private FFdUClosed: TSFBool;
    public property FdUClosed: TSFBool read FFdUClosed;

    strict private FFdUDimension: TSFInt32;
    public property FdUDimension: TSFInt32 read FFdUDimension;

    strict private FFdUKnot: TMFDouble;
    public property FdUKnot: TMFDouble read FFdUKnot;

    strict private FFdUOrder: TSFInt32;
    public property FdUOrder: TSFInt32 read FFdUOrder;

    strict private FFdVClosed: TSFBool;
    public property FdVClosed: TSFBool read FFdVClosed;

    strict private FFdVDimension: TSFInt32;
    public property FdVDimension: TSFInt32 read FFdVDimension;

    strict private FFdVKnot: TMFDouble;
    public property FdVKnot: TMFDouble read FFdVKnot;

    strict private FFdVOrder: TSFInt32;
    public property FdVOrder: TSFInt32 read FFdVOrder;

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
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;
    class function ForVRMLVersion(const Version: TX3DVersion): boolean; override;

    { Event in } { }
    strict private FEventAddChildren: TMFNodeEvent;
    public property EventAddChildren: TMFNodeEvent read FEventAddChildren;

    { Event in } { }
    strict private FEventRemoveChildren: TMFNodeEvent;
    public property EventRemoveChildren: TMFNodeEvent read FEventRemoveChildren;

    strict private FFdChildren: TMFNode;
    public property FdChildren: TMFNode read FFdChildren;

    {$I auto_generated_node_helpers/x3dnodes_contour2d.inc}
  end;

  { Piecewise linear curve segment as a part
    of a trimming contour in the u, v domain of a surface. }
  TContourPolyline2DNode = class(TAbstractNurbsControlCurveNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    {$I auto_generated_node_helpers/x3dnodes_contourpolyline2d.inc}
  end;

  { 3D coordinates defines using double precision floating point values. }
  TCoordinateDoubleNode = class(TAbstractCoordinateNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    strict private FFdPoint: TMFVec3d;
    public property FdPoint: TMFVec3d read FFdPoint;

    function CoordCount: Cardinal; override;

    {$I auto_generated_node_helpers/x3dnodes_coordinatedouble.inc}
  end;

  { Visible NURBS curve in 3D. }
  TNurbsCurveNode = class(TAbstractParametricGeometryNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    strict private FFdControlPoint: TSFNode;
    public property FdControlPoint: TSFNode read FFdControlPoint;

    strict private FFdTessellation: TSFInt32;
    public property FdTessellation: TSFInt32 read FFdTessellation;

    strict private FFdWeight: TMFDouble;
    public property FdWeight: TMFDouble read FFdWeight;

    strict private FFdClosed: TSFBool;
    public property FdClosed: TSFBool read FFdClosed;

    strict private FFdKnot: TMFDouble;
    public property FdKnot: TMFDouble read FFdKnot;

    strict private FFdOrder: TSFInt32;
    public property FdOrder: TSFInt32 read FFdOrder;

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
    function InternalCoord(State: TX3DGraphTraverseState;
      out ACoord: TMFVec3f): boolean; override;

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
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    strict private FFdTessellation: TSFInt32;
    public property FdTessellation: TSFInt32 read FFdTessellation;

    strict private FFdWeight: TMFDouble;
    public property FdWeight: TMFDouble read FFdWeight;

    strict private FFdClosed: TSFBool;
    public property FdClosed: TSFBool read FFdClosed;

    strict private FFdKnot: TMFDouble;
    public property FdKnot: TMFDouble read FFdKnot;

    strict private FFdOrder: TSFInt32;
    public property FdOrder: TSFInt32 read FFdOrder;

    {$I auto_generated_node_helpers/x3dnodes_nurbscurve2d.inc}
  end;

  { Interpolate (animate) orientations as tangent vectors of the 3D NURBS curve. }
  TNurbsOrientationInterpolatorNode = class(TAbstractChildNode)
  strict private
    procedure EventSet_FractionReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event in } { }
    strict private FEventSet_fraction: TSFFloatEvent;
    public property EventSet_fraction: TSFFloatEvent read FEventSet_fraction;

    strict private FFdControlPoint: TSFNode;
    public property FdControlPoint: TSFNode read FFdControlPoint;

    strict private FFdKnot: TMFDouble;
    public property FdKnot: TMFDouble read FFdKnot;

    strict private FFdOrder: TSFInt32;
    public property FdOrder: TSFInt32 read FFdOrder;

    strict private FFdWeight: TMFDouble;
    public property FdWeight: TMFDouble read FFdWeight;

    { Event out } { }
    strict private FEventValue_changed: TSFRotationEvent;
    public property EventValue_changed: TSFRotationEvent read FEventValue_changed;

    {$I auto_generated_node_helpers/x3dnodes_nurbsorientationinterpolator.inc}
  end;

  { Visible NURBS 3D surface. }
  TNurbsPatchSurfaceNode = class(TAbstractNurbsSurfaceGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    {$I auto_generated_node_helpers/x3dnodes_nurbspatchsurface.inc}
  end;

  { Interpolate (animate) positions along the 3D NURBS curve. }
  TNurbsPositionInterpolatorNode = class(TAbstractChildNode)
  strict private
    procedure EventSet_FractionReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event in } { }
    strict private FEventSet_fraction: TSFFloatEvent;
    public property EventSet_fraction: TSFFloatEvent read FEventSet_fraction;

    strict private FFdControlPoint: TSFNode;
    public property FdControlPoint: TSFNode read FFdControlPoint;

    strict private FFdKnot: TMFDouble;
    public property FdKnot: TMFDouble read FFdKnot;

    strict private FFdOrder: TSFInt32;
    public property FdOrder: TSFInt32 read FFdOrder;

    strict private FFdWeight: TMFDouble;
    public property FdWeight: TMFDouble read FFdWeight;

    { Event out } { }
    strict private FEventValue_changed: TSFVec3fEvent;
    public property EventValue_changed: TSFVec3fEvent read FEventValue_changed;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean; override;

    {$I auto_generated_node_helpers/x3dnodes_nurbspositioninterpolator.inc}
  end;
  TNurbsPositionInterpolatorNode_3 = TNurbsPositionInterpolatorNode;

  { Groups a set of NURBS surface nodes to a common group
    for rendering purposes, to ensure a common tesselation within the group. }
  TNurbsSetNode = class(TAbstractChildNode, IAbstractBoundedObject)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event in } { }
    strict private FEventAddGeometry: TMFNodeEvent;
    public property EventAddGeometry: TMFNodeEvent read FEventAddGeometry;

    { Event in } { }
    strict private FEventRemoveGeometry: TMFNodeEvent;
    public property EventRemoveGeometry: TMFNodeEvent read FEventRemoveGeometry;

    { Implementation note: Fdgeometry is not enumerated in DirectEnumerateActive,
      as it's not actually rendered from NurbsSet node.
      Children here have to be placed elsewhere, in some Shape,
      to actually get enumerated as "active". }
    { }
    strict private FFdGeometry: TMFNode;
    public property FdGeometry: TMFNode read FFdGeometry;

    strict private FFdTessellationScale: TSFFloat;
    public property FdTessellationScale: TSFFloat read FFdTessellationScale;

    strict private FFdBboxCenter: TSFVec3f;
    public property FdBboxCenter: TSFVec3f read FFdBboxCenter;

    strict private FFdBboxSize: TSFVec3f;
    public property FdBboxSize: TSFVec3f read FFdBboxSize;

    {$I auto_generated_node_helpers/x3dnodes_nurbsset.inc}
  end;

  { Interpolate (animate) by sampling a position and normal at 3D NURBS surface
    from an input 2D surface parameters. }
  TNurbsSurfaceInterpolatorNode = class(TAbstractChildNode)
  strict private
    procedure EventSet_FractionReceive(Event: TX3DEvent; Value: TX3DField;
      const Time: TX3DTime);
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event in } { }
    strict private FEventSet_fraction: TSFVec2fEvent;
    public property EventSet_fraction: TSFVec2fEvent read FEventSet_fraction;

    strict private FFdControlPoint: TSFNode;
    public property FdControlPoint: TSFNode read FFdControlPoint;

    strict private FFdWeight: TMFDouble;
    public property FdWeight: TMFDouble read FFdWeight;

    { Event out } { }
    strict private FEventPosition_changed: TSFVec3fEvent;
    public property EventPosition_changed: TSFVec3fEvent read FEventPosition_changed;

    { Event out } { }
    strict private FEventNormal_changed: TSFVec3fEvent;
    public property EventNormal_changed: TSFVec3fEvent read FEventNormal_changed;

    strict private FFdUDimension: TSFInt32;
    public property FdUDimension: TSFInt32 read FFdUDimension;

    strict private FFdUKnot: TMFDouble;
    public property FdUKnot: TMFDouble read FFdUKnot;

    strict private FFdUOrder: TSFInt32;
    public property FdUOrder: TSFInt32 read FFdUOrder;

    strict private FFdVDimension: TSFInt32;
    public property FdVDimension: TSFInt32 read FFdVDimension;

    strict private FFdVKnot: TMFDouble;
    public property FdVKnot: TMFDouble read FFdVKnot;

    strict private FFdVOrder: TSFInt32;
    public property FdVOrder: TSFInt32 read FFdVOrder;

    {$I auto_generated_node_helpers/x3dnodes_nurbssurfaceinterpolator.inc}
  end;

  { Path in 2D space (that can be constructed from NURBS curves,
    or straight segments) extruded along a 3D NURBS curve.
    @bold(Rendering of this node is not implemented yet.) }
  TNurbsSweptSurfaceNode = class(TAbstractParametricGeometryNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;
    function SolidField: TSFBool; override;

    strict private FFdCrossSectionCurve: TSFNode;
    public property FdCrossSectionCurve: TSFNode read FFdCrossSectionCurve;

    strict private FFdTrajectoryCurve: TSFNode;
    public property FdTrajectoryCurve: TSFNode read FFdTrajectoryCurve;

    strict private FFdCcw: TSFBool;
    public property FdCcw: TSFBool read FFdCcw;

    strict private FFdSolid: TSFBool;
    public property FdSolid: TSFBool read FFdSolid;

    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
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
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;
    function SolidField: TSFBool; override;

    strict private FFdProfileCurve: TSFNode;
    public property FdProfileCurve: TSFNode read FFdProfileCurve;

    strict private FFdTrajectoryCurve: TSFNode;
    public property FdTrajectoryCurve: TSFNode read FFdTrajectoryCurve;

    strict private FFdCcw: TSFBool;
    public property FdCcw: TSFBool read FFdCcw;

    strict private FFdSolid: TSFBool;
    public property FdSolid: TSFBool read FFdSolid;

    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

    {$I auto_generated_node_helpers/x3dnodes_nurbsswungsurface.inc}
  end;

  { NURBS surface existing in the parametric domain of its surface host
    specifying the mapping of the texture onto the surface.
    @bold(Not implemented yet.) }
  TNurbsTextureCoordinateNode = class(TAbstractNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    strict private FFdControlPoint: TMFVec2f;
    public property FdControlPoint: TMFVec2f read FFdControlPoint;

    strict private FFdWeight: TMFFloat;
    public property FdWeight: TMFFloat read FFdWeight;

    strict private FFdUDimension: TSFInt32;
    public property FdUDimension: TSFInt32 read FFdUDimension;

    strict private FFdUKnot: TMFDouble;
    public property FdUKnot: TMFDouble read FFdUKnot;

    strict private FFdUOrder: TSFInt32;
    public property FdUOrder: TSFInt32 read FFdUOrder;

    strict private FFdVDimension: TSFInt32;
    public property FdVDimension: TSFInt32 read FFdVDimension;

    strict private FFdVKnot: TMFDouble;
    public property FdVKnot: TMFDouble read FFdVKnot;

    strict private FFdVOrder: TSFInt32;
    public property FdVOrder: TSFInt32 read FFdVOrder;

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
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event in } { }
    strict private FEventAddTrimmingContour: TMFNodeEvent;
    public property EventAddTrimmingContour: TMFNodeEvent read FEventAddTrimmingContour;

    { Event in } { }
    strict private FEventRemoveTrimmingContour: TMFNodeEvent;
    public property EventRemoveTrimmingContour: TMFNodeEvent read FEventRemoveTrimmingContour;

    strict private FFdTrimmingContour: TMFNode;
    public property FdTrimmingContour: TMFNode read FFdTrimmingContour;

    function Proxy(var State: TX3DGraphTraverseState;
      const OverTriangulate: boolean): TAbstractGeometryNode; override;

    {$I auto_generated_node_helpers/x3dnodes_nurbstrimmedsurface.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TAbstractNurbsControlCurveNode.CreateNode;
begin
  inherited;

  FFdControlPoint := TMFVec2d.Create(Self, true, 'controlPoint', []);
  AddField(FFdControlPoint);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

procedure TAbstractNurbsSurfaceGeometryNode.CreateNode;
begin
  inherited;

  FFdControlPoint := TSFNode.Create(Self, true, 'controlPoint', [TAbstractCoordinateNode]);
   FdControlPoint.ChangesAlways := [chGeometry];
  AddField(FFdControlPoint);

  FFdTexCoord := TSFNode.Create(Self, true, 'texCoord', [TAbstractTextureCoordinateNode, TNurbsTextureCoordinateNode]);
   FdTexCoord.ChangesAlways := [chGeometry];
  AddField(FFdTexCoord);

  FFdUTessellation := TSFInt32.Create(Self, true, 'uTessellation', 0);
   FdUTessellation.ChangesAlways := [chGeometry];
  AddField(FFdUTessellation);
  { X3D specification comment: (-Inf,Inf) }

  FFdVTessellation := TSFInt32.Create(Self, true, 'vTessellation', 0);
   FdVTessellation.ChangesAlways := [chGeometry];
  AddField(FFdVTessellation);
  { X3D specification comment: (-Inf,Inf) }

  FFdWeight := TMFDouble.Create(Self, true, 'weight', []);
   FdWeight.ChangesAlways := [chGeometry];
  AddField(FFdWeight);
  { X3D specification comment: (0,Inf) }

  FFdSolid := TSFBool.Create(Self, false, 'solid', true);
   FdSolid.ChangesAlways := [chGeometry];
  AddField(FFdSolid);

  FFdUClosed := TSFBool.Create(Self, false, 'uClosed', false);
   FdUClosed.ChangesAlways := [chGeometry];
  AddField(FFdUClosed);

  FFdUDimension := TSFInt32.Create(Self, false, 'uDimension', 0);
   FdUDimension.ChangesAlways := [chGeometry];
  AddField(FFdUDimension);
  { X3D specification comment: [0,Inf) }

  FFdUKnot := TMFDouble.Create(Self, false, 'uKnot', []);
   FdUKnot.ChangesAlways := [chGeometry];
  AddField(FFdUKnot);
  { X3D specification comment: (-Inf,Inf) }

  FFdUOrder := TSFInt32.Create(Self, false, 'uOrder', 3);
   FdUOrder.ChangesAlways := [chGeometry];
  AddField(FFdUOrder);
  { X3D specification comment: [2,Inf) }

  FFdVClosed := TSFBool.Create(Self, false, 'vClosed', false);
   FdVClosed.ChangesAlways := [chGeometry];
  AddField(FFdVClosed);

  FFdVDimension := TSFInt32.Create(Self, false, 'vDimension', 0);
   FdVDimension.ChangesAlways := [chGeometry];
  AddField(FFdVDimension);
  { X3D specification comment: [0,Inf) }

  FFdVKnot := TMFDouble.Create(Self, false, 'vKnot', []);
   FdVKnot.ChangesAlways := [chGeometry];
  AddField(FFdVKnot);
  { X3D specification comment: (-Inf,Inf) }

  FFdVOrder := TSFInt32.Create(Self, false, 'vOrder', 3);
   FdVOrder.ChangesAlways := [chGeometry];
  AddField(FFdVOrder);
  { X3D specification comment: [2,Inf) }
end;

function TAbstractNurbsSurfaceGeometryNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := FdcontrolPoint.Enumerate(Func);
  if Result <> nil then Exit;

  Result := FdtexCoord.Enumerate(Func);
  if Result <> nil then Exit;
end;

procedure TContour2DNode.CreateNode;
begin
  inherited;

  FEventAddChildren := TMFNodeEvent.Create(Self, 'addChildren', true);
  AddEvent(FEventAddChildren);

  FEventRemoveChildren := TMFNodeEvent.Create(Self, 'removeChildren', true);
  AddEvent(FEventRemoveChildren);

  FFdChildren := TMFNode.Create(Self, true, 'children', [TNurbsCurve2DNode, TContourPolyline2DNode]);
   FdChildren.ChangesAlways := [chEverything];
  AddField(FFdChildren);

  DefaultContainerField := 'trimmingContour';
end;

class function TContour2DNode.ClassX3DType: string;
begin
  Result := 'Contour2D';
end;

class function TContour2DNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
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

procedure TContourPolyline2DNode.CreateNode;
begin
  inherited;

  DefaultContainerField := 'geometry';
end;

class function TContourPolyline2DNode.ClassX3DType: string;
begin
  Result := 'ContourPolyline2D';
end;

class function TContourPolyline2DNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TCoordinateDoubleNode.CreateNode;
begin
  inherited;

  FFdPoint := TMFVec3d.Create(Self, true, 'point', []);
   { Not really handled for now }
   FdPoint.ChangesAlways := [chCoordinate];
  AddField(FFdPoint);
  { X3D specification comment: (-Inf,Inf) }
end;

class function TCoordinateDoubleNode.ClassX3DType: string;
begin
  Result := 'CoordinateDouble';
end;

class function TCoordinateDoubleNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

function TCoordinateDoubleNode.CoordCount: Cardinal;
begin
  Result := FdPoint.Items.Count;
end;

procedure TNurbsCurveNode.CreateNode;
begin
  inherited;

  FFdControlPoint := TSFNode.Create(Self, true, 'controlPoint', [TAbstractCoordinateNode]);
   FdControlPoint.ChangesAlways := [chGeometry];
  AddField(FFdControlPoint);

  FFdTessellation := TSFInt32.Create(Self, true, 'tessellation', 0);
   FdTessellation.ChangesAlways := [chGeometry];
  AddField(FFdTessellation);
  { X3D specification comment: (-Inf,Inf) }

  FFdWeight := TMFDouble.Create(Self, true, 'weight', []);
   FdWeight.ChangesAlways := [chGeometry];
  AddField(FFdWeight);
  { X3D specification comment: (0,Inf) }

  FFdClosed := TSFBool.Create(Self, false, 'closed', false);
   FdClosed.ChangesAlways := [chGeometry];
  AddField(FFdClosed);

  FFdKnot := TMFDouble.Create(Self, false, 'knot', []);
   FdKnot.ChangesAlways := [chGeometry];
  AddField(FFdKnot);
  { X3D specification comment: (-Inf,Inf) }

  FFdOrder := TSFInt32.Create(Self, false, 'order', 3);
   FdOrder.ChangesAlways := [chGeometry];
  AddField(FFdOrder);
  { X3D specification comment: [2,Inf) }
end;

class function TNurbsCurveNode.ClassX3DType: string;
begin
  Result := 'NurbsCurve';
end;

class function TNurbsCurveNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
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
      ResultCoord.FdPoint.Items.List^[I] := NurbsCalculator.GetPoint(
        Knot.First + Increase * I, nil);
  finally FreeAndNil(NurbsCalculator) end;

  { set LineSet.vertexCount (otherwise it's coord will be ignored) }
  LS.FdVertexCount.Items.Add(Tess);

  FreeAndNil(Knot);
end;

function TNurbsCurveNode.Proxy(var State: TX3DGraphTraverseState;
  const OverTriangulate: boolean): TAbstractGeometryNode;
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

function TNurbsCurveNode.ProxyUsesOverTriangulate: boolean;
begin
  Result := false;
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
    Result := NurbsBoundingBox(TCoordinateNode(FdControlPoint.Value).FdPoint.Items, FdWeight.Items, State.Transform)
  else
    Result := TBox3D.Empty;
end;

function TNurbsCurveNode.TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
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

procedure TNurbsCurve2DNode.CreateNode;
begin
  inherited;

  FFdTessellation := TSFInt32.Create(Self, true, 'tessellation', 0);
  AddField(FFdTessellation);
  { X3D specification comment: (-Inf,Inf) }

  FFdWeight := TMFDouble.Create(Self, true, 'weight', []);
  AddField(FFdWeight);
  { X3D specification comment: (0,Inf) }

  FFdClosed := TSFBool.Create(Self, false, 'closed', false);
  AddField(FFdClosed);

  FFdKnot := TMFDouble.Create(Self, false, 'knot', []);
  AddField(FFdKnot);
  { X3D specification comment: (-Inf,Inf) }

  FFdOrder := TSFInt32.Create(Self, false, 'order', 3);
  AddField(FFdOrder);
  { X3D specification comment: [2,Inf) }

  DefaultContainerField := 'children';
end;

class function TNurbsCurve2DNode.ClassX3DType: string;
begin
  Result := 'NurbsCurve2D';
end;

class function TNurbsCurve2DNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TNurbsOrientationInterpolatorNode.CreateNode;
begin
  inherited;

  FEventSet_fraction := TSFFloatEvent.Create(Self, 'set_fraction', true);
  AddEvent(FEventSet_fraction);

  FFdControlPoint := TSFNode.Create(Self, true, 'controlPoint', [TAbstractCoordinateNode]);
  AddField(FFdControlPoint);

  FFdKnot := TMFDouble.Create(Self, true, 'knot', []);
  AddField(FFdKnot);
  { X3D specification comment: (-Inf,Inf) }

  FFdOrder := TSFInt32.Create(Self, true, 'order', 3);
  AddField(FFdOrder);
  { X3D specification comment: (2,Inf) }

  FFdWeight := TMFDouble.Create(Self, true, 'weight', []);
  AddField(FFdWeight);
  { X3D specification comment: (-Inf,Inf) }

  FEventValue_changed := TSFRotationEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.AddNotification({$ifdef CASTLE_OBJFPC}@{$endif} EventSet_FractionReceive);
end;

class function TNurbsOrientationInterpolatorNode.ClassX3DType: string;
begin
  Result := 'NurbsOrientationInterpolator';
end;

class function TNurbsOrientationInterpolatorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

function TNurbsOrientationInterpolatorNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := FdcontrolPoint.Enumerate(Func);
  if Result <> nil then Exit;
end;

procedure TNurbsOrientationInterpolatorNode.EventSet_FractionReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
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

procedure TNurbsPatchSurfaceNode.CreateNode;
begin
  inherited;
end;

class function TNurbsPatchSurfaceNode.ClassX3DType: string;
begin
  Result := 'NurbsPatchSurface';
end;

class function TNurbsPatchSurfaceNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
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
        if not TVector3.Equals(
          ControlPoint.List^[                 J * UDimension],
          ControlPoint.List^[UDimension - 1 + J * UDimension], ClosedCheckEpsilon) then
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
          ControlPoint.List^[I                                ],
          ControlPoint.List^[I + (VDimension - 1) * UDimension], ClosedCheckEpsilon) then
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
  ResultIndex: TLongIntList;
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
    WritelnWarning('VRML/X3D', 'Nurbs[Patch]Surface.u/vDimension is < 0');
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
        ResultCoord.List^[I + J * UTess] :=
          NurbsCalculator.GetPoint(
            UKnot.First + UIncrease * I,
            VKnot.First + VIncrease * J,
            @Normal);
        if Ccw then
          ResultNormal.List^[I + J * UTess] := Normal else
          ResultNormal.List^[I + J * UTess] := -Normal;
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
      ResultIndex.List^[NextIndex] := MakeIndex(I  , J  ); Inc(NextIndex);
      ResultIndex.List^[NextIndex] := MakeIndex(I-1, J  ); Inc(NextIndex);
      ResultIndex.List^[NextIndex] := MakeIndex(I-1, J-1); Inc(NextIndex);
      ResultIndex.List^[NextIndex] := MakeIndex(I  , J-1); Inc(NextIndex);
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
        ResultTexCoord.List^[I + J * UTess] := Vector2(
          I / (UTess - 1),
          J / (VTess - 1));
  end;

  FreeAndNil(UKnot);
  FreeAndNil(VKnot);
end;

function TAbstractNurbsSurfaceGeometryNode.Proxy(var State: TX3DGraphTraverseState;
  const OverTriangulate: boolean): TAbstractGeometryNode;
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

function TAbstractNurbsSurfaceGeometryNode.ProxyUsesOverTriangulate: boolean;
begin
  Result := false;
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
    Result := NurbsBoundingBox(TCoordinateNode(FdControlPoint.Value).FdPoint.Items, FdWeight.Items, State.Transform)
  else
    Result := TBox3D.Empty;
end;

function TAbstractNurbsSurfaceGeometryNode.TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
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

procedure TNurbsPositionInterpolatorNode.CreateNode;
begin
  inherited;

  FEventSet_fraction := TSFFloatEvent.Create(Self, 'set_fraction', true);
  AddEvent(FEventSet_fraction);

  FFdControlPoint := TSFNode.Create(Self, true, 'controlPoint', [TAbstractCoordinateNode]);
  AddField(FFdControlPoint);

  FFdKnot := TMFDouble.Create(Self, true, 'knot', []);
  AddField(FFdKnot);
  { X3D specification comment: (-Inf,Inf) }

  FFdOrder := TSFInt32.Create(Self, true, 'order', 3);
  AddField(FFdOrder);
  { X3D specification comment: (2,Inf) }

  FFdWeight := TMFDouble.Create(Self, true, 'weight', []);
  AddField(FFdWeight);
  { X3D specification comment: (-Inf,Inf) }

  FEventValue_changed := TSFVec3fEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.AddNotification({$ifdef CASTLE_OBJFPC}@{$endif} EventSet_FractionReceive);
end;

class function TNurbsPositionInterpolatorNode.ClassX3DType: string;
begin
  Result := 'NurbsPositionInterpolator';
end;

class function TNurbsPositionInterpolatorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
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
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
var
  ControlPointList: TVector3List;
  Knot: TDoubleList;
  OutputValue: TVector3;
begin
  if not EventValue_Changed.SendNeeded then Exit;

  if ControlPoint <> nil then
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

procedure TNurbsSetNode.CreateNode;
begin
  inherited;

  FEventAddGeometry := TMFNodeEvent.Create(Self, 'addGeometry', true);
  AddEvent(FEventAddGeometry);

  FEventRemoveGeometry := TMFNodeEvent.Create(Self, 'removeGeometry', true);
  AddEvent(FEventRemoveGeometry);

  FFdGeometry := TMFNode.Create(Self, true, 'geometry', [TAbstractParametricGeometryNode]);
  AddField(FFdGeometry);

  FFdTessellationScale := TSFFloat.Create(Self, true, 'tessellationScale', 1.0);
  AddField(FFdTessellationScale);
  { X3D specification comment: (0,Inf) }

  FFdBboxCenter := TSFVec3f.Create(Self, false, 'bboxCenter', Vector3(0, 0, 0));
  AddField(FFdBboxCenter);
  { X3D specification comment: (-Inf,Inf) }

  FFdBboxSize := TSFVec3f.Create(Self, false, 'bboxSize', Vector3(-1, -1, -1));
  AddField(FFdBboxSize);
  { X3D specification comment: [0,Inf) or -1 -1 -1 }

  DefaultContainerField := 'children';
end;

class function TNurbsSetNode.ClassX3DType: string;
begin
  Result := 'NurbsSet';
end;

class function TNurbsSetNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TNurbsSurfaceInterpolatorNode.CreateNode;
begin
  inherited;

  FEventSet_fraction := TSFVec2fEvent.Create(Self, 'set_fraction', true);
  AddEvent(FEventSet_fraction);

  FFdControlPoint := TSFNode.Create(Self, true, 'controlPoint', [TAbstractCoordinateNode]);
  AddField(FFdControlPoint);

  FFdWeight := TMFDouble.Create(Self, true, 'weight', []);
  AddField(FFdWeight);
  { X3D specification comment: (-Inf,Inf) }

  FEventPosition_changed := TSFVec3fEvent.Create(Self, 'position_changed', false);
  AddEvent(FEventPosition_changed);

  FEventNormal_changed := TSFVec3fEvent.Create(Self, 'normal_changed', false);
  AddEvent(FEventNormal_changed);

  FFdUDimension := TSFInt32.Create(Self, false, 'uDimension', 0);
  AddField(FFdUDimension);
  { X3D specification comment: [0,Inf) }

  FFdUKnot := TMFDouble.Create(Self, false, 'uKnot', []);
  AddField(FFdUKnot);
  { X3D specification comment: (-Inf,Inf) }

  FFdUOrder := TSFInt32.Create(Self, false, 'uOrder', 3);
  AddField(FFdUOrder);
  { X3D specification comment: [2,Inf) }

  FFdVDimension := TSFInt32.Create(Self, false, 'vDimension', 0);
  AddField(FFdVDimension);
  { X3D specification comment: [0,Inf) }

  FFdVKnot := TMFDouble.Create(Self, false, 'vKnot', []);
  AddField(FFdVKnot);
  { X3D specification comment: (-Inf,Inf) }

  FFdVOrder := TSFInt32.Create(Self, false, 'vOrder', 3);
  AddField(FFdVOrder);
  { X3D specification comment: [2,Inf) }

  DefaultContainerField := 'children';

  EventSet_Fraction.AddNotification({$ifdef CASTLE_OBJFPC}@{$endif} EventSet_FractionReceive);
end;

class function TNurbsSurfaceInterpolatorNode.ClassX3DType: string;
begin
  Result := 'NurbsSurfaceInterpolator';
end;

class function TNurbsSurfaceInterpolatorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

function TNurbsSurfaceInterpolatorNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := FdcontrolPoint.Enumerate(Func);
  if Result <> nil then Exit;
end;

procedure TNurbsSurfaceInterpolatorNode.EventSet_FractionReceive(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
var
  ControlPointList: TVector3List;
  OutputNormal: TVector3;
  OutputPosition: TVector3;
  UKnot, VKnot: TDoubleList;
begin
  if not (EventPosition_Changed.SendNeeded or
          EventNormal_Changed.SendNeeded) then Exit;

  { TODO: we should handle here all TAbstractCoordinateNode }
  if ControlPoint <> nil then
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
    WritelnWarning('VRML/X3D', 'NurbsSurfaceInterpolator.u/vDimension is < 0');
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

procedure TNurbsSweptSurfaceNode.CreateNode;
begin
  inherited;

  FFdCrossSectionCurve := TSFNode.Create(Self, true, 'crossSectionCurve', [TAbstractNurbsControlCurveNode]);
   FdCrossSectionCurve.ChangesAlways := [chGeometry];
  AddField(FFdCrossSectionCurve);

  FFdTrajectoryCurve := TSFNode.Create(Self, true, 'trajectoryCurve', [TNurbsCurveNode]);
   FdTrajectoryCurve.ChangesAlways := [chGeometry];
  AddField(FFdTrajectoryCurve);

  FFdCcw := TSFBool.Create(Self, false, 'ccw', true);
   FdCcw.ChangesAlways := [chGeometry];
  AddField(FFdCcw);

  FFdSolid := TSFBool.Create(Self, false, 'solid', true);
   FdSolid.ChangesAlways := [chGeometry];
  AddField(FFdSolid);
end;

class function TNurbsSweptSurfaceNode.ClassX3DType: string;
begin
  Result := 'NurbsSweptSurface';
end;

class function TNurbsSweptSurfaceNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
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

function TNurbsSweptSurfaceNode.VerticesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  { Rendering of TNurbsSweptSurfaceNode not implemented. }
  Result := 0;
end;

function TNurbsSweptSurfaceNode.TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  { Rendering of TNurbsSweptSurfaceNode not implemented. }
  Result := 0;
end;

function TNurbsSweptSurfaceNode.SolidField: TSFBool;
begin
  Result := FdSolid;
end;

procedure TNurbsSwungSurfaceNode.CreateNode;
begin
  inherited;

  FFdProfileCurve := TSFNode.Create(Self, true, 'profileCurve', [TAbstractNurbsControlCurveNode]);
   FdProfileCurve.ChangesAlways := [chGeometry];
  AddField(FFdProfileCurve);

  FFdTrajectoryCurve := TSFNode.Create(Self, true, 'trajectoryCurve', [TAbstractNurbsControlCurveNode]);
   FdTrajectoryCurve.ChangesAlways := [chGeometry];
  AddField(FFdTrajectoryCurve);

  FFdCcw := TSFBool.Create(Self, false, 'ccw', true);
   FdCcw.ChangesAlways := [chGeometry];
  AddField(FFdCcw);

  FFdSolid := TSFBool.Create(Self, false, 'solid', true);
   FdSolid.ChangesAlways := [chGeometry];
  AddField(FFdSolid);
end;

class function TNurbsSwungSurfaceNode.ClassX3DType: string;
begin
  Result := 'NurbsSwungSurface';
end;

class function TNurbsSwungSurfaceNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
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

function TNurbsSwungSurfaceNode.VerticesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  { Rendering of TNurbsSwungSurfaceNode not implemented. }
  Result := 0;
end;

function TNurbsSwungSurfaceNode.TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  { Rendering of TNurbsSwungSurfaceNode not implemented. }
  Result := 0;
end;

function TNurbsSwungSurfaceNode.SolidField: TSFBool;
begin
  Result := FdSolid;
end;

procedure TNurbsTextureCoordinateNode.CreateNode;
begin
  inherited;

  FFdControlPoint := TMFVec2f.Create(Self, true, 'controlPoint', []);
  AddField(FFdControlPoint);
  { X3D specification comment: (-Inf,Inf) }

  FFdWeight := TMFFloat.Create(Self, true, 'weight', []);
  AddField(FFdWeight);
  { X3D specification comment: (0,Inf) }

  FFdUDimension := TSFInt32.Create(Self, false, 'uDimension', 0);
  AddField(FFdUDimension);
  { X3D specification comment: [0,Inf) }

  FFdUKnot := TMFDouble.Create(Self, false, 'uKnot', []);
  AddField(FFdUKnot);
  { X3D specification comment: (-Inf,Inf) }

  FFdUOrder := TSFInt32.Create(Self, false, 'uOrder', 3);
  AddField(FFdUOrder);
  { X3D specification comment: [2,Inf) }

  FFdVDimension := TSFInt32.Create(Self, false, 'vDimension', 0);
  AddField(FFdVDimension);
  { X3D specification comment: [0,Inf) }

  FFdVKnot := TMFDouble.Create(Self, false, 'vKnot', []);
  AddField(FFdVKnot);
  { X3D specification comment: (-Inf,Inf) }

  FFdVOrder := TSFInt32.Create(Self, false, 'vOrder', 3);
  AddField(FFdVOrder);
  { X3D specification comment: [2,Inf) }

  DefaultContainerField := 'texCoord';
end;

class function TNurbsTextureCoordinateNode.ClassX3DType: string;
begin
  Result := 'NurbsTextureCoordinate';
end;

class function TNurbsTextureCoordinateNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TNurbsTrimmedSurfaceNode.CreateNode;
begin
  inherited;

  FEventAddTrimmingContour := TMFNodeEvent.Create(Self, 'addTrimmingContour', true);
  AddEvent(FEventAddTrimmingContour);

  FEventRemoveTrimmingContour := TMFNodeEvent.Create(Self, 'removeTrimmingContour', true);
  AddEvent(FEventRemoveTrimmingContour);

  FFdTrimmingContour := TMFNode.Create(Self, true, 'trimmingContour', [TContour2DNode]);
   FdTrimmingContour.ChangesAlways := [chGeometry];
  AddField(FFdTrimmingContour);
end;

class function TNurbsTrimmedSurfaceNode.ClassX3DType: string;
begin
  Result := 'NurbsTrimmedSurface';
end;

class function TNurbsTrimmedSurfaceNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

function TNurbsTrimmedSurfaceNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := FdtrimmingContour.Enumerate(Func);
  if Result <> nil then Exit;
end;

function TNurbsTrimmedSurfaceNode.Proxy(var State: TX3DGraphTraverseState;
  const OverTriangulate: boolean): TAbstractGeometryNode;
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
