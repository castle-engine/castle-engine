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

{$ifdef read_interface}
  { }
  TNodeArc2D = class(TNodeX3DGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdEndAngle: TSFFloat;
    public property FdEndAngle: TSFFloat read FFdEndAngle;

    private FFdRadius: TSFFloat;
    public property FdRadius: TSFFloat read FFdRadius;

    private FFdStartAngle: TSFFloat;
    public property FdStartAngle: TSFFloat read FFdStartAngle;

    { Geometry node not implemented } { }
    function LocalBoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
  end;

  TNodeArcClose2D = class(TNodeX3DGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdClosureType: TSFString;
    public property FdClosureType: TSFString read FFdClosureType;

    private FFdEndAngle: TSFFloat;
    public property FdEndAngle: TSFFloat read FFdEndAngle;

    private FFdRadius: TSFFloat;
    public property FdRadius: TSFFloat read FFdRadius;

    private FFdSolid: TSFBool;
    public property FdSolid: TSFBool read FFdSolid;

    private FFdStartAngle: TSFFloat;
    public property FdStartAngle: TSFFloat read FFdStartAngle;

    { Geometry node not implemented } { }
    function LocalBoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
  end;

  TNodeCircle2D = class(TNodeX3DGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdRadius: TSFFloat;
    public property FdRadius: TSFFloat read FFdRadius;

    function Proxy(var State: TVRMLGraphTraverseState;
      const OverTriangulate: boolean): TVRMLGeometryNode; override;
    function ProxyUsesOverTriangulate: boolean; override;
  end;

  TNodeDisk2D = class(TNodeX3DGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdInnerRadius: TSFFloat;
    public property FdInnerRadius: TSFFloat read FFdInnerRadius;

    private FFdOuterRadius: TSFFloat;
    public property FdOuterRadius: TSFFloat read FFdOuterRadius;

    private FFdSolid: TSFBool;
    public property FdSolid: TSFBool read FFdSolid;

    { Geometry node not implemented } { }
    function LocalBoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
  end;

  { Polyline2D node.

    The "lineSegments" field uses our TVRMLField.AddAlternativeName mechanism,
    and when reading/saving VRML 97 file it will be named "point"
    (as it was in VRML 97 amendment 1 specification).
    In X3D normal name, "lineSegments", will be used. }
  TNodePolyline2D = class(TNodeX3DGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdLineSegments: TMFVec2f;
    public property FdLineSegments: TMFVec2f read FFdLineSegments;

    { Geometry node not implemented } { }
    function LocalBoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
  end;

  TNodePolypoint2D = class(TNodeX3DGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdPoint: TMFVec2f;
    public property FdPoint: TMFVec2f read FFdPoint;

    { Geometry node not implemented } { }
    function LocalBoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
  end;

  TNodeRectangle2D = class(TNodeX3DGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdSize: TSFVec2f;
    public property FdSize: TSFVec2f read FFdSize;

    private FFdSolid: TSFBool;
    public property FdSolid: TSFBool read FFdSolid;

    function Proxy(var State: TVRMLGraphTraverseState;
      const OverTriangulate: boolean): TVRMLGeometryNode; override;
    function ProxyUsesOverTriangulate: boolean; override;
  end;

  TNodeTriangleSet2D = class(TNodeX3DGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdVertices: TMFVec2f;
    public property FdVertices: TMFVec2f read FFdVertices;

    private FFdSolid: TSFBool;
    public property FdSolid: TSFBool read FFdSolid;

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
procedure TNodeArc2D.CreateNode;
begin
  inherited;

  FFdEndAngle := TSFFloat.Create(Self, 'endAngle', Pi/2);
   FdEndAngle.Exposed := false;
   FdEndAngle.ChangesAlways := [chGeometry];
  Fields.Add(FFdEndAngle);
  { X3D specification comment: [-2Pi,2Pi] }

  FFdRadius := TSFFloat.Create(Self, 'radius', 1);
   FdRadius.Exposed := false;
   FdRadius.ChangesAlways := [chGeometry];
  Fields.Add(FFdRadius);
  { X3D specification comment: (0,Inf) }

  FFdStartAngle := TSFFloat.Create(Self, 'startAngle', 0);
   FdStartAngle.Exposed := false;
   FdStartAngle.ChangesAlways := [chGeometry];
  Fields.Add(FFdStartAngle);
  { X3D specification comment: [-2Pi,2Pi] }
end;

class function TNodeArc2D.ClassNodeTypeName: string;
begin
  Result := 'Arc2D';
end;

class function TNodeArc2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

{$define TGeometryNotImplemented := TNodeArc2D}
GeometryNotImplemented

procedure TNodeArcClose2D.CreateNode;
begin
  inherited;

  FFdClosureType := TSFString.Create(Self, 'closureType', 'PIE');
   FdClosureType.Exposed := false;
   FdClosureType.ChangesAlways := [chGeometry];
  Fields.Add(FFdClosureType);
  { X3D specification comment: ["PIE"|"CHORD"] }

  FFdEndAngle := TSFFloat.Create(Self, 'endAngle', Pi/2);
   FdEndAngle.Exposed := false;
   FdEndAngle.ChangesAlways := [chGeometry];
  Fields.Add(FFdEndAngle);
  { X3D specification comment: [-2Pi,2Pi] }

  FFdRadius := TSFFloat.Create(Self, 'radius', 1);
   FdRadius.Exposed := false;
   FdRadius.ChangesAlways := [chGeometry];
  Fields.Add(FFdRadius);
  { X3D specification comment: (0,Inf) }

  FFdSolid := TSFBool.Create(Self, 'solid', false);
   FdSolid.Exposed := false;
   FdSolid.ChangesAlways := [chGeometry];
  Fields.Add(FFdSolid);

  FFdStartAngle := TSFFloat.Create(Self, 'startAngle', 0);
   FdStartAngle.Exposed := false;
   FdStartAngle.ChangesAlways := [chGeometry];
  Fields.Add(FFdStartAngle);
  { X3D specification comment: [-2Pi,2Pi] }
end;

class function TNodeArcClose2D.ClassNodeTypeName: string;
begin
  Result := 'ArcClose2D';
end;

class function TNodeArcClose2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

{$define TGeometryNotImplemented := TNodeArcClose2D}
GeometryNotImplemented

procedure TNodeCircle2D.CreateNode;
begin
  inherited;

  FFdRadius := TSFFloat.Create(Self, 'radius', 1);
   FdRadius.Exposed := false;
   FdRadius.ChangesAlways := [chGeometry];
  Fields.Add(FFdRadius);
  { X3D specification comment: (0,Inf) }
end;

class function TNodeCircle2D.ClassNodeTypeName: string;
begin
  Result := 'Circle2D';
end;

class function TNodeCircle2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TNodeCircle2D.Proxy(var State: TVRMLGraphTraverseState;
  const OverTriangulate: boolean): TVRMLGeometryNode;
var
  Slices: Integer;
  C: TNodeCoordinate;
  LineSet: TNodeLineSet absolute Result;
  I: Integer;
  AngleSin, AngleCos: Float;
begin
  LineSet := TNodeLineSet.Create(NodeName, WWWBasePath);
  try
    Slices := State.LastNodes.KambiTriangulation.QuadricSlices;

    C := TNodeCoordinate.Create('', WWWBasePath);
    C.FdPoint.Items.Count := Slices + 1;
    LineSet.FdCoord.Value := C;

    { calculate C contents }
    for I := 0 to Slices - 1 do
    begin
      SinCos((I / Slices) * 2 * Pi, AngleSin, AngleCos);

      C.FdPoint.Items.L[I] := Vector3Single(
        AngleSin * FdRadius.Value,
        AngleCos * FdRadius.Value, 0);
    end;
    { close the circle }
    C.FdPoint.Items.L[Slices] := C.FdPoint.Items.L[0];

    LineSet.FdVertexCount.Items.Add(Slices + 1);
  except FreeAndNil(Result); raise end;
end;

function TNodeCircle2D.ProxyUsesOverTriangulate: boolean;
begin
  Result := false;
end;

procedure TNodeDisk2D.CreateNode;
begin
  inherited;

  FFdInnerRadius := TSFFloat.Create(Self, 'innerRadius', 0);
   FdInnerRadius.Exposed := false;
   FdInnerRadius.ChangesAlways := [chGeometry];
  Fields.Add(FFdInnerRadius);
  { X3D specification comment: [0,Inf) }

  FFdOuterRadius := TSFFloat.Create(Self, 'outerRadius', 1);
   FdOuterRadius.Exposed := false;
   FdOuterRadius.ChangesAlways := [chGeometry];
  Fields.Add(FFdOuterRadius);
  { X3D specification comment: (0,Inf) }

  FFdSolid := TSFBool.Create(Self, 'solid', false);
   FdSolid.Exposed := false;
   FdSolid.ChangesAlways := [chGeometry];
  Fields.Add(FFdSolid);
end;

class function TNodeDisk2D.ClassNodeTypeName: string;
begin
  Result := 'Disk2D';
end;

class function TNodeDisk2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

{$define TGeometryNotImplemented := TNodeDisk2D}
GeometryNotImplemented

procedure TNodePolyline2D.CreateNode;
begin
  inherited;

  FFdLineSegments := TMFVec2f.Create(Self, 'lineSegments', []);
   FdLineSegments.Exposed := false;
   FdLineSegments.ChangesAlways := [chGeometry];
  Fields.Add(FFdLineSegments);
  { X3D specification comment: (-Inf,Inf) }

  FFdLineSegments.AddAlternativeName('point', 2);
end;

class function TNodePolyline2D.ClassNodeTypeName: string;
begin
  Result := 'Polyline2D';
end;

class function TNodePolyline2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

{$define TGeometryNotImplemented := TNodePolyline2D}
GeometryNotImplemented

procedure TNodePolypoint2D.CreateNode;
begin
  inherited;

  FFdPoint := TMFVec2f.Create(Self, 'point', []);
   FdPoint.ChangesAlways := [chGeometry];
  Fields.Add(FFdPoint);
  { X3D specification comment: (-Inf,Inf) }
end;

class function TNodePolypoint2D.ClassNodeTypeName: string;
begin
  Result := 'Polypoint2D';
end;

class function TNodePolypoint2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

{$define TGeometryNotImplemented := TNodePolypoint2D}
GeometryNotImplemented

procedure TNodeRectangle2D.CreateNode;
begin
  inherited;

  FFdSize := TSFVec2f.Create(Self, 'size', Vector2Single(2, 2));
   FdSize.Exposed := false;
   FdSize.ChangesAlways := [chGeometry];
  Fields.Add(FFdSize);
  { X3D specification comment: (0,Inf) }

  FFdSolid := TSFBool.Create(Self, 'solid', false);
   FdSolid.Exposed := false;
   FdSolid.ChangesAlways := [chGeometry];
  Fields.Add(FFdSolid);
end;

class function TNodeRectangle2D.ClassNodeTypeName: string;
begin
  Result := 'Rectangle2D';
end;

class function TNodeRectangle2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function Rectangle2DProxy(Original: TVRMLGeometryNode;
  const Size: TVector2Single; const Solid: boolean): TVRMLGeometryNode;
var
  Coord: TNodeCoordinate;
  TexCoord: TNodeTextureCoordinate;
  QuadSet: TNodeQuadSet absolute Result;
begin
  QuadSet := TNodeQuadSet.Create(Original.NodeName, Original.WWWBasePath);
  try
    Coord := TNodeCoordinate.Create('', Original.WWWBasePath);
    Coord.FdPoint.Items.Count := 4;
    QuadSet.FdCoord.Value := Coord;

    TexCoord := TNodeTextureCoordinate.Create('', Original.WWWBasePath);
    TexCoord.FdPoint.Items.Count := 4;
    QuadSet.FdTexCoord.Value := TexCoord;

    { calculate Coord, TexCoord contents }

    Coord.FdPoint.Items.L[0] := Vector3Single(-Size[0] / 2, -Size[1] / 2, 0);
    TexCoord.FdPoint.Items.L[0] := Vector2Single(0, 0);

    Coord.FdPoint.Items.L[1] := Vector3Single( Size[0] / 2, -Size[1] / 2, 0);
    TexCoord.FdPoint.Items.L[1] := Vector2Single(1, 0);

    Coord.FdPoint.Items.L[2] := Vector3Single( Size[0] / 2,  Size[1] / 2, 0);
    TexCoord.FdPoint.Items.L[2] := Vector2Single(1, 1);

    Coord.FdPoint.Items.L[3] := Vector3Single(-Size[0] / 2,  Size[1] / 2, 0);
    TexCoord.FdPoint.Items.L[3] := Vector2Single(0, 1);

    QuadSet.FdSolid.Value := Solid;
  except FreeAndNil(Result); raise end;
end;

function TNodeRectangle2D.Proxy(var State: TVRMLGraphTraverseState;
  const OverTriangulate: boolean): TVRMLGeometryNode;
begin
  Result := Rectangle2DProxy(Self, FdSize.Value, FdSolid.Value);
end;

function TNodeRectangle2D.ProxyUsesOverTriangulate: boolean;
begin
  Result := false;
end;

procedure TNodeTriangleSet2D.CreateNode;
begin
  inherited;

  FFdVertices := TMFVec2f.Create(Self, 'vertices', []);
   FdVertices.ChangesAlways := [chGeometry];
  Fields.Add(FFdVertices);
  { X3D specification comment: (-Inf,Inf) }

  FFdSolid := TSFBool.Create(Self, 'solid', false);
   FdSolid.Exposed := false;
   FdSolid.ChangesAlways := [chGeometry];
  Fields.Add(FFdSolid);
end;

class function TNodeTriangleSet2D.ClassNodeTypeName: string;
begin
  Result := 'TriangleSet2D';
end;

class function TNodeTriangleSet2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

{$define TGeometryNotImplemented := TNodeTriangleSet2D}
GeometryNotImplemented

procedure RegisterGeometry2DNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeArc2D,
    TNodeArcClose2D,
    TNodeCircle2D,
    TNodeDisk2D,
    TNodePolyline2D,
    TNodePolypoint2D,
    TNodeRectangle2D,
    TNodeTriangleSet2D
  ]);
end;

{$endif read_implementation}
