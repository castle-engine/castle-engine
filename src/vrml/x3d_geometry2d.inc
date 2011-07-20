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

    private FFdendAngle: TSFFloat;
    public property FdendAngle: TSFFloat read FFdendAngle;

    private FFdradius: TSFFloat;
    public property Fdradius: TSFFloat read FFdradius;

    private FFdstartAngle: TSFFloat;
    public property FdstartAngle: TSFFloat read FFdstartAngle;

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

    private FFdclosureType: TSFString;
    public property FdclosureType: TSFString read FFdclosureType;

    private FFdendAngle: TSFFloat;
    public property FdendAngle: TSFFloat read FFdendAngle;

    private FFdradius: TSFFloat;
    public property Fdradius: TSFFloat read FFdradius;

    private FFdsolid: TSFBool;
    public property Fdsolid: TSFBool read FFdsolid;

    private FFdstartAngle: TSFFloat;
    public property FdstartAngle: TSFFloat read FFdstartAngle;

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

    private FFdradius: TSFFloat;
    public property Fdradius: TSFFloat read FFdradius;

    function Proxy(var State: TVRMLGraphTraverseState;
      const OverTriangulate: boolean): TVRMLGeometryNode; override;
    function ProxyUsesOverTriangulate: boolean; override;
  end;

  TNodeDisk2D = class(TNodeX3DGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdinnerRadius: TSFFloat;
    public property FdinnerRadius: TSFFloat read FFdinnerRadius;

    private FFdouterRadius: TSFFloat;
    public property FdouterRadius: TSFFloat read FFdouterRadius;

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

    private FFdlineSegments: TMFVec2f;
    public property FdlineSegments: TMFVec2f read FFdlineSegments;

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

    private FFdpoint: TMFVec2f;
    public property Fdpoint: TMFVec2f read FFdpoint;

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

    private FFdsize: TSFVec2f;
    public property Fdsize: TSFVec2f read FFdsize;

    private FFdsolid: TSFBool;
    public property Fdsolid: TSFBool read FFdsolid;

    function Proxy(var State: TVRMLGraphTraverseState;
      const OverTriangulate: boolean): TVRMLGeometryNode; override;
    function ProxyUsesOverTriangulate: boolean; override;
  end;

  TNodeTriangleSet2D = class(TNodeX3DGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdvertices: TMFVec2f;
    public property Fdvertices: TMFVec2f read FFdvertices;

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

{$endif read_interface}

{$ifdef read_implementation}
procedure TNodeArc2D.CreateNode;
begin
  inherited;

  FFdendAngle := TSFFloat.Create(Self, 'endAngle', Pi/2);
   FdendAngle.Exposed := false;
   FdendAngle.ChangesAlways := [chGeometry];
  Fields.Add(FFdendAngle);
  { X3D specification comment: [-2Pi,2Pi] }

  FFdradius := TSFFloat.Create(Self, 'radius', 1);
   Fdradius.Exposed := false;
   Fdradius.ChangesAlways := [chGeometry];
  Fields.Add(FFdradius);
  { X3D specification comment: (0,Inf) }

  FFdstartAngle := TSFFloat.Create(Self, 'startAngle', 0);
   FdstartAngle.Exposed := false;
   FdstartAngle.ChangesAlways := [chGeometry];
  Fields.Add(FFdstartAngle);
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

  FFdclosureType := TSFString.Create(Self, 'closureType', 'PIE');
   FdclosureType.Exposed := false;
   FdclosureType.ChangesAlways := [chGeometry];
  Fields.Add(FFdclosureType);
  { X3D specification comment: ["PIE"|"CHORD"] }

  FFdendAngle := TSFFloat.Create(Self, 'endAngle', Pi/2);
   FdendAngle.Exposed := false;
   FdendAngle.ChangesAlways := [chGeometry];
  Fields.Add(FFdendAngle);
  { X3D specification comment: [-2Pi,2Pi] }

  FFdradius := TSFFloat.Create(Self, 'radius', 1);
   Fdradius.Exposed := false;
   Fdradius.ChangesAlways := [chGeometry];
  Fields.Add(FFdradius);
  { X3D specification comment: (0,Inf) }

  FFdsolid := TSFBool.Create(Self, 'solid', false);
   Fdsolid.Exposed := false;
   Fdsolid.ChangesAlways := [chGeometry];
  Fields.Add(FFdsolid);

  FFdstartAngle := TSFFloat.Create(Self, 'startAngle', 0);
   FdstartAngle.Exposed := false;
   FdstartAngle.ChangesAlways := [chGeometry];
  Fields.Add(FFdstartAngle);
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

  FFdradius := TSFFloat.Create(Self, 'radius', 1);
   Fdradius.Exposed := false;
   Fdradius.ChangesAlways := [chGeometry];
  Fields.Add(FFdradius);
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

      C.FdPoint.Items.Items[I] := Vector3Single(
        AngleSin * FdRadius.Value,
        AngleCos * FdRadius.Value, 0);
    end;
    { close the circle }
    C.FdPoint.Items.Items[Slices] := C.FdPoint.Items.Items[0];

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

  FFdinnerRadius := TSFFloat.Create(Self, 'innerRadius', 0);
   FdinnerRadius.Exposed := false;
   FdinnerRadius.ChangesAlways := [chGeometry];
  Fields.Add(FFdinnerRadius);
  { X3D specification comment: [0,Inf) }

  FFdouterRadius := TSFFloat.Create(Self, 'outerRadius', 1);
   FdouterRadius.Exposed := false;
   FdouterRadius.ChangesAlways := [chGeometry];
  Fields.Add(FFdouterRadius);
  { X3D specification comment: (0,Inf) }

  FFdsolid := TSFBool.Create(Self, 'solid', false);
   Fdsolid.Exposed := false;
   Fdsolid.ChangesAlways := [chGeometry];
  Fields.Add(FFdsolid);
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

  FFdlineSegments := TMFVec2f.Create(Self, 'lineSegments', []);
   FdlineSegments.Exposed := false;
   FdlineSegments.ChangesAlways := [chGeometry];
  Fields.Add(FFdlineSegments);
  { X3D specification comment: (-Inf,Inf) }

  FFdlineSegments.AddAlternativeName('point', 2);
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

  FFdpoint := TMFVec2f.Create(Self, 'point', []);
   Fdpoint.ChangesAlways := [chGeometry];
  Fields.Add(FFdpoint);
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

  FFdsize := TSFVec2f.Create(Self, 'size', Vector2Single(2, 2));
   Fdsize.Exposed := false;
   Fdsize.ChangesAlways := [chGeometry];
  Fields.Add(FFdsize);
  { X3D specification comment: (0,Inf) }

  FFdsolid := TSFBool.Create(Self, 'solid', false);
   Fdsolid.Exposed := false;
   Fdsolid.ChangesAlways := [chGeometry];
  Fields.Add(FFdsolid);
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

    Coord.FdPoint.Items.Items[0] := Vector3Single(-Size[0] / 2, -Size[1] / 2, 0);
    TexCoord.FdPoint.Items.Items[0] := Vector2Single(0, 0);

    Coord.FdPoint.Items.Items[1] := Vector3Single( Size[0] / 2, -Size[1] / 2, 0);
    TexCoord.FdPoint.Items.Items[1] := Vector2Single(1, 0);

    Coord.FdPoint.Items.Items[2] := Vector3Single( Size[0] / 2,  Size[1] / 2, 0);
    TexCoord.FdPoint.Items.Items[2] := Vector2Single(1, 1);

    Coord.FdPoint.Items.Items[3] := Vector3Single(-Size[0] / 2,  Size[1] / 2, 0);
    TexCoord.FdPoint.Items.Items[3] := Vector2Single(0, 1);

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

  FFdvertices := TMFVec2f.Create(Self, 'vertices', []);
   Fdvertices.ChangesAlways := [chGeometry];
  Fields.Add(FFdvertices);
  { X3D specification comment: (-Inf,Inf) }

  FFdsolid := TSFBool.Create(Self, 'solid', false);
   Fdsolid.Exposed := false;
   Fdsolid.ChangesAlways := [chGeometry];
  Fields.Add(FFdsolid);
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
