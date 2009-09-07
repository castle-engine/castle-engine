{
  Copyright 2008-2009 Michalis Kamburelis.

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

{ NurbsCurve2D and Contour2D are shared with VRML 97 NURBS definition.
  The rest is, for now, completely separate from VRML 97 NURBS definition
  in vrml97nodes.inc. }

{$ifdef read_interface}
  { }
  TNodeX3DNurbsControlCurveNode = class(TNodeX3DNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

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
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

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

  { Contour2D.

    For VRML 2.0, this is TNodeX3DGeometryNode, since VRML 2.0 specification
    explicitly says it's valid geometry node. For X3D specification, this was only
    TNodeX3DNode. When implementing NURBS, I will have to either handle
    both VRML 2.0 and X3D cases at once, or just split this class into
    two separate TNodeContour2D_2 and TNodeContour2D_3. }
  TNodeContour2D = class(TNodeX3DGeometryNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
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
    function LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  TNodeContourPolyline2D = class(TNodeX3DNurbsControlCurveNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeCoordinateDouble = class(TNodeX3DCoordinateNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdpoint: TMFVec3d;
    public property Fdpoint: TMFVec3d read FFdpoint;

    function CoordCount: Cardinal; override;
  end;

  TNodeNurbsCurve_3 = class(TNodeX3DParametricGeometryNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
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

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean; override;

    function Proxy: TVRMLGeometryNode; override;
    function Coord(State: TVRMLGraphTraverseState; out ACoord: TMFVec3f): boolean; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
  end;

  TNodeNurbsCurve2D = class(TNodeX3DNurbsControlCurveNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
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
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
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
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    function Proxy: TVRMLGeometryNode; override;
    function Coord(State: TVRMLGraphTraverseState; out ACoord: TMFVec3f): boolean; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
  end;

  TNodeNurbsPositionInterpolator_3 = class(TNodeX3DChildNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
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

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean; override;
  end;

  TNodeNurbsSet = class(TNodeX3DChildNode, INodeX3DBoundedObject)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
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
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
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
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
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
    function LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  TNodeNurbsSwungSurface = class(TNodeX3DParametricGeometryNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
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
    function LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

  TNodeNurbsTextureCoordinate = class(TNodeX3DNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
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
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
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
    function LocalBoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;
  end;

{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeX3DNurbsControlCurveNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcontrolPoint := TMFVec2d.Create(Self, 'controlPoint', []);
  Fields.Add(FFdcontrolPoint);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

constructor TNodeX3DNurbsSurfaceGeometryNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcontrolPoint := TSFNode.Create(Self, 'controlPoint', [TNodeX3DCoordinateNode]);
  Fields.Add(FFdcontrolPoint);

  FFdtexCoord := TSFNode.Create(Self, 'texCoord', [TNodeX3DTextureCoordinateNode, TNodeNurbsTextureCoordinate]);
  Fields.Add(FFdtexCoord);

  FFduTessellation := TSFInt32.Create(Self, 'uTessellation', 0);
  Fields.Add(FFduTessellation);
  { X3D specification comment: (-Inf,Inf) }

  FFdvTessellation := TSFInt32.Create(Self, 'vTessellation', 0);
  Fields.Add(FFdvTessellation);
  { X3D specification comment: (-Inf,Inf) }

  FFdweight := TMFDouble.Create(Self, 'weight', []);
  Fields.Add(FFdweight);
  { X3D specification comment: (0,Inf) }

  FFdsolid := TSFBool.Create(Self, 'solid', true);
  FFdsolid.Exposed := false;
  Fields.Add(FFdsolid);

  FFduClosed := TSFBool.Create(Self, 'uClosed', false);
  FFduClosed.Exposed := false;
  Fields.Add(FFduClosed);

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

  FFdvClosed := TSFBool.Create(Self, 'vClosed', false);
  FFdvClosed.Exposed := false;
  Fields.Add(FFdvClosed);

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
end;

procedure TNodeX3DNurbsSurfaceGeometryNode.DirectEnumerateActive(
  Func: TEnumerateChildrenFunction);
begin
  FdcontrolPoint.EnumerateValid(Func);
  FdtexCoord.EnumerateValid(Func);
end;

constructor TNodeContour2D.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventaddChildren := TVRMLEvent.Create(Self, 'addChildren', TMFNode, true);
  Events.Add(FEventaddChildren);

  FEventremoveChildren := TVRMLEvent.Create(Self, 'removeChildren', TMFNode, true);
  Events.Add(FEventremoveChildren);

  FFdchildren := TMFNode.Create(Self, 'children', [TNodeNurbsCurve2D, TNodeContourPolyline2D,
    { In VRML 97 these are allowed children too: }
    TNodePolyline2D, TNodeContour2D ]);
  Fields.Add(FFdchildren);

  DefaultContainerField := 'trimmingContour';
end;

class function TNodeContour2D.ClassNodeTypeName: string;
begin
  Result := 'Contour2D';
end;

function TNodeContour2D.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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

constructor TNodeContourPolyline2D.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  DefaultContainerField := 'geometry';
end;

class function TNodeContourPolyline2D.ClassNodeTypeName: string;
begin
  Result := 'ContourPolyline2D';
end;

function TNodeContourPolyline2D.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeContourPolyline2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeCoordinateDouble.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdpoint := TMFVec3d.Create(Self, 'point', []);
  Fields.Add(FFdpoint);
  { X3D specification comment: (-Inf,Inf) }
end;

class function TNodeCoordinateDouble.ClassNodeTypeName: string;
begin
  Result := 'CoordinateDouble';
end;

function TNodeCoordinateDouble.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
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

constructor TNodeNurbsCurve_3.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcontrolPoint := TSFNode.Create(Self, 'controlPoint', [TNodeX3DCoordinateNode]);
  Fields.Add(FFdcontrolPoint);

  FFdtessellation := TSFInt32.Create(Self, 'tessellation', 0);
  Fields.Add(FFdtessellation);
  { X3D specification comment: (-Inf,Inf) }

  FFdweight := TMFDouble.Create(Self, 'weight', []);
  Fields.Add(FFdweight);
  { X3D specification comment: (0,Inf) }

  FFdclosed := TSFBool.Create(Self, 'closed', false);
  FFdclosed.Exposed := false;
  Fields.Add(FFdclosed);

  FFdknot := TMFDouble.Create(Self, 'knot', []);
  FFdknot.Exposed := false;
  Fields.Add(FFdknot);
  { X3D specification comment: (-Inf,Inf) }

  FFdorder := TSFInt32.Create(Self, 'order', 3);
  FFdorder.Exposed := false;
  Fields.Add(FFdorder);
  { X3D specification comment: [2,Inf) }
end;

class function TNodeNurbsCurve_3.ClassNodeTypeName: string;
begin
  Result := 'NurbsCurve';
end;

function TNodeNurbsCurve_3.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeNurbsCurve_3.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

class function TNodeNurbsCurve_3.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 3;
end;

procedure TNodeNurbsCurve_3.DirectEnumerateActive(
  Func: TEnumerateChildrenFunction);
begin
  FdcontrolPoint.EnumerateValid(Func);
end;

function TNodeNurbsCurve_3.Proxy: TVRMLGeometryNode;
var
  LS: TNodeLineSet absolute Result;
  ControlPoint: TNodeCoordinate;
  ResultCoord: TNodeCoordinate;
  Tess: Cardinal;
  I: Integer;
  Increase: Double;
  Knot: TDynDoubleArray;
begin
  LS := TNodeLineSet.Create(NodeName, WWWBasePath);
  try
    { TODO: we should handle here all TNodeX3DCoordinateNode }
    if (FdControlPoint.Value <> nil) and
       (FdControlPoint.Value is TNodeCoordinate) then
      ControlPoint := TNodeCoordinate(FdControlPoint.Value) else
      Exit;

    if ControlPoint.FdPoint.Count = 0 then Exit;

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
    NurbsKnotIfNeeded(Knot, ControlPoint.CoordCount, FdOrder.Value, nkEndpointUniform);

    { calculate tesselation: Tess, Increase }
    Tess := ActualTessellation(FdTessellation.Value, ControlPoint.CoordCount);
    Increase := (Knot.Last - Knot.First) / (Tess - 1);

    { make resulting Coordinate node }
    ResultCoord := (TVRMLNodeClass(ControlPoint.ClassType).Create('', WWWBasePath)) as TNodeCoordinate;
    LS.FdCoord.Value := ResultCoord;

    { calculate result Coordinate.point field }
    ResultCoord.FdPoint.Items.Count := Tess;
    for I := 0 to Tess - 1 do
      ResultCoord.FdPoint.Items.Items[I] :=
        NurbsCurvePoint(ControlPoint.FdPoint.Items,
          Knot.First + Increase * I,
          FdOrder.Value, Knot, FdWeight.Items);

    { set LineSet.vertexCount (otherwise it's coord will be ignored) }
    LS.FdVertexCount.Items.AppendItem(Tess);

    FreeAndNil(Knot);
  except FreeAndNil(Result); raise end;
end;

{ Coord implementation makes automatic, optimal and fast
  (without the need for Proxy call) LocalBoundingBox, BoundingBox
  implementation. }
function TNodeNurbsCurve_3.Coord(State: TVRMLGraphTraverseState;
  out ACoord: TMFVec3f): boolean;
begin
  Result := true;
  if (FdControlPoint.Value <> nil) and
     (FdControlPoint.Value is TNodeCoordinate) then
    ACoord := TNodeCoordinate(FdControlPoint.Value).FdPoint else
    ACoord := nil;
end;

function TNodeNurbsCurve_3.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
begin
  Result := 0;
end;

function TNodeNurbsCurve_3.VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
var
  ControlPoint: TNodeCoordinate;
begin
  if (FdControlPoint.Value <> nil) and
     (FdControlPoint.Value is TNodeCoordinate) then
    ControlPoint := TNodeCoordinate(FdControlPoint.Value) else
    Exit(0);

  if ControlPoint.CoordCount > 0 then
    Result := ActualTessellation(FdTessellation.Value, ControlPoint.CoordCount) else
    Result := 0;
end;

constructor TNodeNurbsCurve2D.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdtessellation := TSFInt32.Create(Self, 'tessellation', 0);
  Fields.Add(FFdtessellation);
  { X3D specification comment: (-Inf,Inf) }

  FFdweight := TMFDouble.Create(Self, 'weight', []);
  Fields.Add(FFdweight);
  { X3D specification comment: (0,Inf) }

  FFdclosed := TSFBool.Create(Self, 'closed', false);
  FFdclosed.Exposed := false;
  Fields.Add(FFdclosed);

  FFdknot := TMFDouble.Create(Self, 'knot', []);
  FFdknot.Exposed := false;
  Fields.Add(FFdknot);
  { X3D specification comment: (-Inf,Inf) }

  FFdorder := TSFInt32.Create(Self, 'order', 3);
  FFdorder.Exposed := false;
  Fields.Add(FFdorder);
  { X3D specification comment: [2,Inf) }

  DefaultContainerField := 'geometry';
end;

class function TNodeNurbsCurve2D.ClassNodeTypeName: string;
begin
  Result := 'NurbsCurve2D';
end;

function TNodeNurbsCurve2D.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeNurbsCurve2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeNurbsOrientationInterpolator.Create(const ANodeName: string;
  const AWWWBasePath: string);
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
end;

class function TNodeNurbsOrientationInterpolator.ClassNodeTypeName: string;
begin
  Result := 'NurbsOrientationInterpolator';
end;

function TNodeNurbsOrientationInterpolator.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
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

constructor TNodeNurbsPatchSurface.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;
end;

class function TNodeNurbsPatchSurface.ClassNodeTypeName: string;
begin
  Result := 'NurbsPatchSurface';
end;

function TNodeNurbsPatchSurface.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeNurbsPatchSurface.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TNodeNurbsPatchSurface.Proxy: TVRMLGeometryNode;
var
  ControlPoint: TNodeCoordinate;

const
  { This has to be slightly larger than normal epsilon
    (test nurbs_dune_primitives.x3dv). }
  ClosedCheckEpsilon = 0.000001;

  function CalculateUClosed: boolean;
  var
    Points: PVector3Single;
    J: Integer;
  begin
    Result := FdUClosed.Value;
    if Result then
    begin
      Points := ControlPoint.FdPoint.Items.Items;
      for J := 0 to FdVDimension.Value - 1 do
        if not VectorsEqual(
          Points[                         J * FdUDimension.Value],
          Points[FdUDimension.Value - 1 + J * FdUDimension.Value], ClosedCheckEpsilon) then
        begin
          if Log then
            WritelnLog('NURBS', 'NurbsPatchSurface.uClosed is TRUE, but looking at controlPoints it should be FALSE, ignoring (otherwise would cause invalid geometry/normals)');
          Result := false;
          Exit;
        end;
      if Log then
        WritelnLog('NURBS', 'NurbsPatchSurface.uClosed confirmed Ok');
    end;
  end;

  function CalculateVClosed: boolean;
  var
    Points: PVector3Single;
    I: Integer;
  begin
    Result := FdUClosed.Value;
    if Result then
    begin
      Points := ControlPoint.FdPoint.Items.Items;
      for I := 0 to FdUDimension.Value - 1 do
        if not VectorsEqual(
          Points[I                                                ],
          Points[I + (FdVDimension.Value - 1) * FdUDimension.Value], ClosedCheckEpsilon) then
        begin
          if Log then
            WritelnLog('NURBS', 'NurbsPatchSurface.vClosed is TRUE, but looking at controlPoints it should be FALSE, ignoring (otherwise would cause invalid geometry/normals)');
          Result := false;
          Exit;
        end;
      if Log then
        WritelnLog('NURBS', 'NurbsPatchSurface.vClosed confirmed Ok');
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
  QS: TNodeIndexedQuadSet absolute Result;
  ResultCoord: TDynVector3SingleArray;
  ResultIndex: TDynLongIntArray;
  I, J, NextIndex: Cardinal;
  Normal: TVector3_Single;
  UIncrease, VIncrease: Double;
  UKnot, VKnot: TDynDoubleArray;
  ResultTexCoord: TDynVector2SingleArray;
begin
  QS := TNodeIndexedQuadSet.Create(NodeName, WWWBasePath);
  try
    { TODO: we should handle here all TNodeX3DCoordinateNode }
    if (FdControlPoint.Value <> nil) and
       (FdControlPoint.Value is TNodeCoordinate) then
      ControlPoint := TNodeCoordinate(FdControlPoint.Value) else
      Exit;

    if ControlPoint.CoordCount = 0 then Exit;

    if FdUDimension.Value * FdVDimension.Value <>
       Integer(ControlPoint.CoordCount) then
    begin
      VRMLWarning(vwSerious, Format('Number of coordinates in NurbsPatchSurface.controlPoint (%d) doesn''t match uDimension * vDimension (%d * %d = %d)',
        [ ControlPoint.CoordCount,
          FdUDimension.Value,  FdVDimension.Value,
          FdUDimension.Value * FdVDimension.Value ]));
      Exit;
    end;

    if (FdUDimension.Value < 0) or
       (FdVDimension.Value < 0) then
    begin
      VRMLWarning(vwSerious, 'NurbsPatchSurface.u/vDimension is < 0');
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

    { calculate actual UClosed, VClosed values.

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
      e.g. "Fred The Bunny" in X3d.) }
    UClosed := CalculateUClosed;
    VClosed := CalculateVClosed;

    { calculate correct UKnot, VKnot vectors }
    UKnot := TDynDoubleArray.Create;
    UKnot.Assign(FdUKnot.Items);
    NurbsKnotIfNeeded(UKnot, FdUDimension.Value, FdUOrder.Value, nkEndpointUniform);
    VKnot := TDynDoubleArray.Create;
    VKnot.Assign(FdVKnot.Items);
    NurbsKnotIfNeeded(VKnot, FdVDimension.Value, FdVOrder.Value, nkEndpointUniform);

    { calculate tesselation: xTess, xIncrease }
    UTess := ActualTessellation(FdUTessellation.Value, FdUDimension.Value);
    VTess := ActualTessellation(FdVTessellation.Value, FdVDimension.Value);
    UIncrease := (UKnot.Last - UKnot.First) / (UTess - 1);
    VIncrease := (VKnot.Last - VKnot.First) / (VTess - 1);

    { make resulting Coordinate node }
    QS.FdCoord.Value := TVRMLNodeClass(ControlPoint.ClassType).Create('', WWWBasePath);
    ResultCoord := (QS.FdCoord.Value as TNodeCoordinate).FdPoint.Items;

    { calculate result Coordinate.point field }
    ResultCoord.Count := UTess * VTess;
    for I := 0 to UTess - 1 do
      for J := 0 to VTess - 1 do
        ResultCoord.Items[I + J * UTess] :=
          NurbsSurfacePoint(ControlPoint.FdPoint.Items,
            FdUDimension.Value,
            FdVDimension.Value,
            UKnot.First + UIncrease * I,
            VKnot.First + VIncrease * J,
            FdUOrder.Value,
            FdVOrder.Value,
            UKnot,
            VKnot,
            FdWeight.Items,
            { unused for now }
            Normal);

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

    QS.FdSolid.Value := FdSolid.Value;

    { calculate texCoord field }
    QS.FdTexCoord.Value := FdTexCoord.Value;
    if QS.FdTexCoord.Value = nil then
    begin
      { X3D spec says "By default, texture coordinates in the unit square
        (or cube for 3D coordinates) are generated automatically
        from the parametric subdivision.". I think this means that tex coords
        S/T = 0..1 range from starting to ending knot. }
      QS.FdTexCoord.Value := TNodeTextureCoordinate.Create('', WWWBasePath);
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
  except FreeAndNil(Result); raise end;
end;

{ Coord implementation makes automatic, optimal and fast
  (without the need for Proxy call) LocalBoundingBox, BoundingBox
  implementation. }
function TNodeNurbsPatchSurface.Coord(State: TVRMLGraphTraverseState;
  out ACoord: TMFVec3f): boolean;
begin
  Result := true;
  if (FdControlPoint.Value <> nil) and
     (FdControlPoint.Value is TNodeCoordinate) then
    ACoord := TNodeCoordinate(FdControlPoint.Value).FdPoint else
    ACoord := nil;
end;

function TNodeNurbsPatchSurface.TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
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

function TNodeNurbsPatchSurface.VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal;
var
  UTess, VTess: Cardinal;
begin
  if (FdUDimension.Value > 0) and
     (FdVDimension.Value > 0) then
  begin
    UTess := ActualTessellation(FdUTessellation.Value, FdUDimension.Value);
    VTess := ActualTessellation(FdVTessellation.Value, FdVDimension.Value);
    Result := UTess * VTess;
  end else
    Result := 0;
end;

constructor TNodeNurbsPositionInterpolator_3.Create(const ANodeName: string;
  const AWWWBasePath: string);
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
end;

class function TNodeNurbsPositionInterpolator_3.ClassNodeTypeName: string;
begin
  Result := 'NurbsPositionInterpolator';
end;

function TNodeNurbsPositionInterpolator_3.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeNurbsPositionInterpolator_3.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

class function TNodeNurbsPositionInterpolator_3.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 3;
end;

procedure TNodeNurbsPositionInterpolator_3.DirectEnumerateActive(
  Func: TEnumerateChildrenFunction);
begin
  FdcontrolPoint.EnumerateValid(Func);
end;

constructor TNodeNurbsSet.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventaddGeometry := TVRMLEvent.Create(Self, 'addGeometry', TMFNode, true);
  Events.Add(FEventaddGeometry);

  FEventremoveGeometry := TVRMLEvent.Create(Self, 'removeGeometry', TMFNode, true);
  Events.Add(FEventremoveGeometry);

  FFdgeometry := TMFNode.Create(Self, 'geometry', [TNodeNurbsSurface]);
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
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeNurbsSet.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeNurbsSurfaceInterpolator.Create(const ANodeName: string;
  const AWWWBasePath: string);
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
end;

class function TNodeNurbsSurfaceInterpolator.ClassNodeTypeName: string;
begin
  Result := 'NurbsSurfaceInterpolator';
end;

function TNodeNurbsSurfaceInterpolator.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
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

constructor TNodeNurbsSweptSurface.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcrossSectionCurve := TSFNode.Create(Self, 'crossSectionCurve', [TNodeX3DNurbsControlCurveNode]);
  Fields.Add(FFdcrossSectionCurve);

  FFdtrajectoryCurve := TSFNode.Create(Self, 'trajectoryCurve', [TNodeNurbsCurve_3]);
  Fields.Add(FFdtrajectoryCurve);

  FFdccw := TSFBool.Create(Self, 'ccw', true);
  FFdccw.Exposed := false;
  Fields.Add(FFdccw);

  FFdsolid := TSFBool.Create(Self, 'solid', true);
  FFdsolid.Exposed := false;
  Fields.Add(FFdsolid);
end;

class function TNodeNurbsSweptSurface.ClassNodeTypeName: string;
begin
  Result := 'NurbsSweptSurface';
end;

function TNodeNurbsSweptSurface.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
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

constructor TNodeNurbsSwungSurface.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdprofileCurve := TSFNode.Create(Self, 'profileCurve', [TNodeX3DNurbsControlCurveNode]);
  Fields.Add(FFdprofileCurve);

  FFdtrajectoryCurve := TSFNode.Create(Self, 'trajectoryCurve', [TNodeX3DNurbsControlCurveNode]);
  Fields.Add(FFdtrajectoryCurve);

  FFdccw := TSFBool.Create(Self, 'ccw', true);
  FFdccw.Exposed := false;
  Fields.Add(FFdccw);

  FFdsolid := TSFBool.Create(Self, 'solid', true);
  FFdsolid.Exposed := false;
  Fields.Add(FFdsolid);
end;

class function TNodeNurbsSwungSurface.ClassNodeTypeName: string;
begin
  Result := 'NurbsSwungSurface';
end;

function TNodeNurbsSwungSurface.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
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

constructor TNodeNurbsTextureCoordinate.Create(const ANodeName: string;
  const AWWWBasePath: string);
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
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeNurbsTextureCoordinate.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeNurbsTrimmedSurface.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventaddTrimmingContour := TVRMLEvent.Create(Self, 'addTrimmingContour', TMFNode, true);
  Events.Add(FEventaddTrimmingContour);

  FEventremoveTrimmingContour := TVRMLEvent.Create(Self, 'removeTrimmingContour', TMFNode, true);
  Events.Add(FEventremoveTrimmingContour);

  FFdtrimmingContour := TMFNode.Create(Self, 'trimmingContour', [TNodeContour2D]);
  Fields.Add(FFdtrimmingContour);
end;

class function TNodeNurbsTrimmedSurface.ClassNodeTypeName: string;
begin
  Result := 'NurbsTrimmedSurface';
end;

function TNodeNurbsTrimmedSurface.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
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
    TNodeNurbsCurve_3,
    TNodeNurbsCurve2D,
    TNodeNurbsOrientationInterpolator,
    TNodeNurbsPatchSurface,
    TNodeNurbsPositionInterpolator_3,
    TNodeNurbsSet,
    TNodeNurbsSurfaceInterpolator,
    TNodeNurbsSweptSurface,
    TNodeNurbsSwungSurface,
    TNodeNurbsTextureCoordinate,
    TNodeNurbsTrimmedSurface
  ]);
end;

{$endif read_implementation}
