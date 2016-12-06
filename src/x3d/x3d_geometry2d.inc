{
  Copyright 2002-2016 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { }
  TArc2DNode = class(TAbstractX3DGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdEndAngle: TSFFloat;
    public property FdEndAngle: TSFFloat read FFdEndAngle;

    private FFdRadius: TSFFloat;
    public property FdRadius: TSFFloat read FFdRadius;

    private FFdStartAngle: TSFFloat;
    public property FdStartAngle: TSFFloat read FFdStartAngle;

    { Geometry node not implemented } { }
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
  end;

  TArcClose2DNode = class(TAbstractX3DGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
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
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
  end;

  TCircle2DNode = class(TAbstractX3DGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdRadius: TSFFloat;
    public property FdRadius: TSFFloat read FFdRadius;

    private FFdSlices: TSFInt32;
    public property FdSlices: TSFInt32 read FFdSlices;
    function CalculateSlices: Cardinal;

    function Proxy(var State: TX3DGraphTraverseState;
      const OverTriangulate: boolean): TAbstractGeometryNode; override;
    function ProxyUsesOverTriangulate: boolean; override;
  end;

  TDisk2DNode = class(TAbstractX3DGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdInnerRadius: TSFFloat;
    public property FdInnerRadius: TSFFloat read FFdInnerRadius;

    private FFdOuterRadius: TSFFloat;
    public property FdOuterRadius: TSFFloat read FFdOuterRadius;

    private FFdSolid: TSFBool;
    public property FdSolid: TSFBool read FFdSolid;

    { Geometry node not implemented } { }
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
  end;

  { Polyline2D node.

    The "lineSegments" field uses our TX3DField.AddAlternativeName mechanism,
    and when reading/saving VRML 97 file it will be named "point"
    (as it was in VRML 97 amendment 1 specification).
    In X3D normal name, "lineSegments", will be used. }
  TPolyline2DNode = class(TAbstractX3DGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdLineSegments: TMFVec2f;
    public property FdLineSegments: TMFVec2f read FFdLineSegments;

    { Geometry node not implemented } { }
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
  end;

  TPolypoint2DNode = class(TAbstractX3DGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdPoint: TMFVec2f;
    public property FdPoint: TMFVec2f read FFdPoint;

    { Geometry node not implemented } { }
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
  end;

  TRectangle2DNode = class(TAbstractX3DGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdSize: TSFVec2f;
    public property FdSize: TSFVec2f read FFdSize;

    private FFdSolid: TSFBool;
    public property FdSolid: TSFBool read FFdSolid;

    function Proxy(var State: TX3DGraphTraverseState;
      const OverTriangulate: boolean): TAbstractGeometryNode; override;
    function ProxyUsesOverTriangulate: boolean; override;
  end;

  TTriangleSet2DNode = class(TAbstractX3DGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdVertices: TMFVec2f;
    public property FdVertices: TMFVec2f read FFdVertices;

    private FFdSolid: TSFBool;
    public property FdSolid: TSFBool read FFdSolid;

    { Geometry node not implemented } { }
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TArc2DNode.CreateNode;
begin
  inherited;

  FFdEndAngle := TSFFloat.Create(Self, 'endAngle', Pi/2);
   FdEndAngle.Angle := true;
   FdEndAngle.Exposed := false;
   FdEndAngle.ChangesAlways := [chGeometry];
  AddField(FFdEndAngle);
  { X3D specification comment: [-2Pi,2Pi] }

  FFdRadius := TSFFloat.Create(Self, 'radius', 1);
   FdRadius.Exposed := false;
   FdRadius.ChangesAlways := [chGeometry];
  AddField(FFdRadius);
  { X3D specification comment: (0,Inf) }

  FFdStartAngle := TSFFloat.Create(Self, 'startAngle', 0);
   FdStartAngle.Angle := true;
   FdStartAngle.Exposed := false;
   FdStartAngle.ChangesAlways := [chGeometry];
  AddField(FFdStartAngle);
  { X3D specification comment: [-2Pi,2Pi] }
end;

class function TArc2DNode.ClassX3DType: string;
begin
  Result := 'Arc2D';
end;

class function TArc2DNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

{$define TGeometryNotImplemented := TArc2DNode}
GeometryNotImplemented

procedure TArcClose2DNode.CreateNode;
begin
  inherited;

  FFdClosureType := TSFString.Create(Self, 'closureType', 'PIE');
   FdClosureType.Exposed := false;
   FdClosureType.ChangesAlways := [chGeometry];
  AddField(FFdClosureType);
  { X3D specification comment: ["PIE"|"CHORD"] }

  FFdEndAngle := TSFFloat.Create(Self, 'endAngle', Pi/2);
   FdEndAngle.Angle := true;
   FdEndAngle.Exposed := false;
   FdEndAngle.ChangesAlways := [chGeometry];
  AddField(FFdEndAngle);
  { X3D specification comment: [-2Pi,2Pi] }

  FFdRadius := TSFFloat.Create(Self, 'radius', 1);
   FdRadius.Exposed := false;
   FdRadius.ChangesAlways := [chGeometry];
  AddField(FFdRadius);
  { X3D specification comment: (0,Inf) }

  FFdSolid := TSFBool.Create(Self, 'solid', false);
   FdSolid.Exposed := false;
   FdSolid.ChangesAlways := [chGeometry];
  AddField(FFdSolid);

  FFdStartAngle := TSFFloat.Create(Self, 'startAngle', 0);
   FdStartAngle.Angle := true;
   FdStartAngle.Exposed := false;
   FdStartAngle.ChangesAlways := [chGeometry];
  AddField(FFdStartAngle);
  { X3D specification comment: [-2Pi,2Pi] }
end;

class function TArcClose2DNode.ClassX3DType: string;
begin
  Result := 'ArcClose2D';
end;

class function TArcClose2DNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

{$define TGeometryNotImplemented := TArcClose2DNode}
GeometryNotImplemented

procedure TCircle2DNode.CreateNode;
begin
  inherited;

  FFdRadius := TSFFloat.Create(Self, 'radius', 1);
   FdRadius.Exposed := false;
   FdRadius.ChangesAlways := [chGeometry];
  AddField(FFdRadius);
  { X3D specification comment: (0,Inf) }

  FFdSlices := TSFInt32.Create(Self, 'slices', -1);
   FdSlices.ChangesAlways := [chGeometry];
  AddField(FFdSlices);
end;

class function TCircle2DNode.ClassX3DType: string;
begin
  Result := 'Circle2D';
end;

class function TCircle2DNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

function TCircle2DNode.CalculateSlices: Cardinal;
begin
  // use default in case of -1 or invalid value
  if FdSlices.Value < MinTriangulationSlices then
    Result := DefaultTriangulationSlices
  else
    Result := FdSlices.Value;
end;

function TCircle2DNode.Proxy(var State: TX3DGraphTraverseState;
  const OverTriangulate: boolean): TAbstractGeometryNode;
var
  SlicesNow: Integer;
  C: TCoordinateNode;
  LineSet: TLineSetNode absolute Result;
  I: Integer;
  AngleSin, AngleCos: Float;
begin
  LineSet := TLineSetNode.Create(Name, BaseUrl);
  try
    SlicesNow := CalculateSlices;

    C := TCoordinateNode.Create('', BaseUrl);
    C.FdPoint.Items.Count := SlicesNow + 1;
    LineSet.FdCoord.Value := C;

    { calculate C contents }
    for I := 0 to SlicesNow - 1 do
    begin
      SinCos((I / SlicesNow) * 2 * Pi, AngleSin, AngleCos);

      C.FdPoint.Items.L[I] := Vector3Single(
        AngleSin * FdRadius.Value,
        AngleCos * FdRadius.Value, 0);
    end;
    { close the circle }
    C.FdPoint.Items.L[SlicesNow] := C.FdPoint.Items.L[0];

    LineSet.FdVertexCount.Items.Add(SlicesNow + 1);
  except FreeAndNil(Result); raise end;
end;

function TCircle2DNode.ProxyUsesOverTriangulate: boolean;
begin
  Result := false;
end;

procedure TDisk2DNode.CreateNode;
begin
  inherited;

  FFdInnerRadius := TSFFloat.Create(Self, 'innerRadius', 0);
   FdInnerRadius.Exposed := false;
   FdInnerRadius.ChangesAlways := [chGeometry];
  AddField(FFdInnerRadius);
  { X3D specification comment: [0,Inf) }

  FFdOuterRadius := TSFFloat.Create(Self, 'outerRadius', 1);
   FdOuterRadius.Exposed := false;
   FdOuterRadius.ChangesAlways := [chGeometry];
  AddField(FFdOuterRadius);
  { X3D specification comment: (0,Inf) }

  FFdSolid := TSFBool.Create(Self, 'solid', false);
   FdSolid.Exposed := false;
   FdSolid.ChangesAlways := [chGeometry];
  AddField(FFdSolid);
end;

class function TDisk2DNode.ClassX3DType: string;
begin
  Result := 'Disk2D';
end;

class function TDisk2DNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

{$define TGeometryNotImplemented := TDisk2DNode}
GeometryNotImplemented

procedure TPolyline2DNode.CreateNode;
begin
  inherited;

  FFdLineSegments := TMFVec2f.Create(Self, 'lineSegments', []);
   FdLineSegments.Exposed := false;
   FdLineSegments.ChangesAlways := [chGeometry];
  AddField(FFdLineSegments);
  { X3D specification comment: (-Inf,Inf) }

  FFdLineSegments.AddAlternativeName('point', 2);
end;

class function TPolyline2DNode.ClassX3DType: string;
begin
  Result := 'Polyline2D';
end;

class function TPolyline2DNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
end;

{$define TGeometryNotImplemented := TPolyline2DNode}
GeometryNotImplemented

procedure TPolypoint2DNode.CreateNode;
begin
  inherited;

  FFdPoint := TMFVec2f.Create(Self, 'point', []);
   FdPoint.ChangesAlways := [chGeometry];
  AddField(FFdPoint);
  { X3D specification comment: (-Inf,Inf) }
end;

class function TPolypoint2DNode.ClassX3DType: string;
begin
  Result := 'Polypoint2D';
end;

class function TPolypoint2DNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

{$define TGeometryNotImplemented := TPolypoint2DNode}
GeometryNotImplemented

procedure TRectangle2DNode.CreateNode;
begin
  inherited;

  FFdSize := TSFVec2f.Create(Self, 'size', Vector2Single(2, 2));
   FdSize.Exposed := false;
   FdSize.ChangesAlways := [chGeometry];
  AddField(FFdSize);
  { X3D specification comment: (0,Inf) }

  FFdSolid := TSFBool.Create(Self, 'solid', false);
   FdSolid.Exposed := false;
   FdSolid.ChangesAlways := [chGeometry];
  AddField(FFdSolid);
end;

class function TRectangle2DNode.ClassX3DType: string;
begin
  Result := 'Rectangle2D';
end;

class function TRectangle2DNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

function Rectangle2DProxy(Original: TAbstractGeometryNode;
  const Size: TVector2Single; const Solid: boolean): TAbstractGeometryNode;
var
  Coord: TCoordinateNode;
  TexCoord: TTextureCoordinateNode;
  QuadSet: TQuadSetNode absolute Result;
begin
  QuadSet := TQuadSetNode.Create(Original.Name, Original.BaseUrl);
  try
    Coord := TCoordinateNode.Create('', Original.BaseUrl);
    Coord.FdPoint.Items.Count := 4;
    QuadSet.FdCoord.Value := Coord;

    TexCoord := TTextureCoordinateNode.Create('', Original.BaseUrl);
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

function TRectangle2DNode.Proxy(var State: TX3DGraphTraverseState;
  const OverTriangulate: boolean): TAbstractGeometryNode;
begin
  Result := Rectangle2DProxy(Self, FdSize.Value, FdSolid.Value);
end;

function TRectangle2DNode.ProxyUsesOverTriangulate: boolean;
begin
  Result := false;
end;

procedure TTriangleSet2DNode.CreateNode;
begin
  inherited;

  FFdVertices := TMFVec2f.Create(Self, 'vertices', []);
   FdVertices.ChangesAlways := [chGeometry];
  AddField(FFdVertices);
  { X3D specification comment: (-Inf,Inf) }

  FFdSolid := TSFBool.Create(Self, 'solid', false);
   FdSolid.Exposed := false;
   FdSolid.ChangesAlways := [chGeometry];
  AddField(FFdSolid);
end;

class function TTriangleSet2DNode.ClassX3DType: string;
begin
  Result := 'TriangleSet2D';
end;

class function TTriangleSet2DNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

{$define TGeometryNotImplemented := TTriangleSet2DNode}
GeometryNotImplemented

procedure RegisterGeometry2DNodes;
begin
  NodesManager.RegisterNodeClasses([
    TArc2DNode,
    TArcClose2DNode,
    TCircle2DNode,
    TDisk2DNode,
    TPolyline2DNode,
    TPolypoint2DNode,
    TRectangle2DNode,
    TTriangleSet2DNode
  ]);
end;

{$endif read_implementation}
