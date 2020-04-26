{
  Copyright 2002-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { Circular arc in 2D.
    @bold(Rendering of this node is not implemented yet.) }
  TArc2DNode = class(TAbstractGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdEndAngle: TSFFloat;
    public property FdEndAngle: TSFFloat read FFdEndAngle;

    strict private FFdRadius: TSFFloat;
    public property FdRadius: TSFFloat read FFdRadius;

    strict private FFdStartAngle: TSFFloat;
    public property FdStartAngle: TSFFloat read FFdStartAngle;

    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

    {$I auto_generated_node_helpers/x3dnodes_arc2d.inc}
  end;

  { Closed circular arc in 2D.
    @bold(Rendering of this node is not implemented yet.) }
  TArcClose2DNode = class(TAbstractGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdClosureType: TSFString;
    public property FdClosureType: TSFString read FFdClosureType;

    strict private FFdEndAngle: TSFFloat;
    public property FdEndAngle: TSFFloat read FFdEndAngle;

    strict private FFdRadius: TSFFloat;
    public property FdRadius: TSFFloat read FFdRadius;

    strict private FFdSolid: TSFBool;
    public property FdSolid: TSFBool read FFdSolid;

    strict private FFdStartAngle: TSFFloat;
    public property FdStartAngle: TSFFloat read FFdStartAngle;

    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

    {$I auto_generated_node_helpers/x3dnodes_arcclose2d.inc}
  end;

  { Circle in 2D. }
  TCircle2DNode = class(TAbstractGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdRadius: TSFFloat;
    public property FdRadius: TSFFloat read FFdRadius;

    strict private FFdSlices: TSFInt32;
    public property FdSlices: TSFInt32 read FFdSlices;
    function CalculateSlices: Cardinal;

    function Proxy(var State: TX3DGraphTraverseState;
      const OverTriangulate: boolean): TAbstractGeometryNode; override;
    function ProxyUsesOverTriangulate: boolean; override;

    {$I auto_generated_node_helpers/x3dnodes_circle2d.inc}
  end;

  { Disc (filled circle) in 2D.
    @bold(Rendering of this node is not implemented yet.) }
  TDisk2DNode = class(TAbstractGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdInnerRadius: TSFFloat;
    public property FdInnerRadius: TSFFloat read FFdInnerRadius;

    strict private FFdOuterRadius: TSFFloat;
    public property FdOuterRadius: TSFFloat read FFdOuterRadius;

    strict private FFdSolid: TSFBool;
    public property FdSolid: TSFBool read FFdSolid;

    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

    {$I auto_generated_node_helpers/x3dnodes_disk2d.inc}
  end;

  { Series of contiguous line segments in 2D.
    @bold(Rendering of this node is not implemented yet.) }
  TPolyline2DNode = class(TAbstractGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdLineSegments: TMFVec2f;
    { Polyline segments.
      In VRML 97 it is called "point", in X3D it is "lineSegments". }
    public property FdLineSegments: TMFVec2f read FFdLineSegments;

    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

    {$I auto_generated_node_helpers/x3dnodes_polyline2d.inc}
  end;

  { Set of vertices in 2D.
    @bold(Rendering of this node is not implemented yet.) }
  TPolypoint2DNode = class(TAbstractGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdPoint: TMFVec2f;
    public property FdPoint: TMFVec2f read FFdPoint;

    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

    {$I auto_generated_node_helpers/x3dnodes_polypoint2d.inc}
  end;

  { Rectangle in 2D. }
  TRectangle2DNode = class(TAbstractGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdSize: TSFVec2f;
    public property FdSize: TSFVec2f read FFdSize;

    strict private FFdSolid: TSFBool;
    public property FdSolid: TSFBool read FFdSolid;

    function Proxy(var State: TX3DGraphTraverseState;
      const OverTriangulate: boolean): TAbstractGeometryNode; override;
    function ProxyUsesOverTriangulate: boolean; override;

    {$I auto_generated_node_helpers/x3dnodes_rectangle2d.inc}
  end;

  { Set of triangles in 2D.
    @bold(Rendering of this node is not implemented yet.) }
  TTriangleSet2DNode = class(TAbstractGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdVertices: TMFVec2f;
    public property FdVertices: TMFVec2f read FFdVertices;

    strict private FFdSolid: TSFBool;
    public property FdSolid: TSFBool read FFdSolid;

    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

    {$I auto_generated_node_helpers/x3dnodes_triangleset2d.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TArc2DNode.CreateNode;
begin
  inherited;

  FFdEndAngle := TSFFloat.Create(Self, false, 'endAngle', Pi/2);
   FdEndAngle.Angle := true;
   FdEndAngle.ChangeAlways := chGeometry;
  AddField(FFdEndAngle);
  { X3D specification comment: [-2Pi,2Pi] }

  FFdRadius := TSFFloat.Create(Self, false, 'radius', 1);
   FdRadius.ChangeAlways := chGeometry;
  AddField(FFdRadius);
  { X3D specification comment: (0,Inf) }

  FFdStartAngle := TSFFloat.Create(Self, false, 'startAngle', 0);
   FdStartAngle.Angle := true;
   FdStartAngle.ChangeAlways := chGeometry;
  AddField(FFdStartAngle);
  { X3D specification comment: [-2Pi,2Pi] }
end;

class function TArc2DNode.ClassX3DType: string;
begin
  Result := 'Arc2D';
end;

function TArc2DNode.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  { Rendering of TArc2DNode not implemented. }
  Result := TBox3D.Empty;
end;

function TArc2DNode.VerticesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  { Rendering of TArc2DNode not implemented. }
  Result := 0;
end;

function TArc2DNode.TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  { Rendering of TArc2DNode not implemented. }
  Result := 0;
end;

procedure TArcClose2DNode.CreateNode;
begin
  inherited;

  FFdClosureType := TSFString.Create(Self, false, 'closureType', 'PIE');
   FdClosureType.ChangeAlways := chGeometry;
  AddField(FFdClosureType);
  { X3D specification comment: ["PIE"|"CHORD"] }

  FFdEndAngle := TSFFloat.Create(Self, false, 'endAngle', Pi/2);
   FdEndAngle.Angle := true;
   FdEndAngle.ChangeAlways := chGeometry;
  AddField(FFdEndAngle);
  { X3D specification comment: [-2Pi,2Pi] }

  FFdRadius := TSFFloat.Create(Self, false, 'radius', 1);
   FdRadius.ChangeAlways := chGeometry;
  AddField(FFdRadius);
  { X3D specification comment: (0,Inf) }

  FFdSolid := TSFBool.Create(Self, false, 'solid', false);
   FdSolid.ChangeAlways := chGeometry;
  AddField(FFdSolid);

  FFdStartAngle := TSFFloat.Create(Self, false, 'startAngle', 0);
   FdStartAngle.Angle := true;
   FdStartAngle.ChangeAlways := chGeometry;
  AddField(FFdStartAngle);
  { X3D specification comment: [-2Pi,2Pi] }
end;

class function TArcClose2DNode.ClassX3DType: string;
begin
  Result := 'ArcClose2D';
end;

function TArcClose2DNode.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  { Rendering of TArcClose2DNode not implemented. }
  Result := TBox3D.Empty;
end;

function TArcClose2DNode.VerticesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  { Rendering of TArcClose2DNode not implemented. }
  Result := 0;
end;

function TArcClose2DNode.TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  { Rendering of TArcClose2DNode not implemented. }
  Result := 0;
end;

procedure TCircle2DNode.CreateNode;
begin
  inherited;

  FFdRadius := TSFFloat.Create(Self, false, 'radius', 1);
   FdRadius.ChangeAlways := chGeometry;
  AddField(FFdRadius);
  { X3D specification comment: (0,Inf) }

  FFdSlices := TSFInt32.Create(Self, true, 'slices', -1);
   FdSlices.ChangeAlways := chGeometry;
  AddField(FFdSlices);
end;

class function TCircle2DNode.ClassX3DType: string;
begin
  Result := 'Circle2D';
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
  LineSet := TLineSetNode.Create(X3DName, BaseUrl);
  try
    SlicesNow := CalculateSlices;

    C := TCoordinateNode.Create('', BaseUrl);
    C.FdPoint.Items.Count := SlicesNow + 1;
    LineSet.FdCoord.Value := C;

    { calculate C contents }
    for I := 0 to SlicesNow - 1 do
    begin
      SinCos((I / SlicesNow) * 2 * Pi, AngleSin, AngleCos);

      C.FdPoint.Items.List^[I] := Vector3(
        AngleSin * FdRadius.Value,
        AngleCos * FdRadius.Value, 0);
    end;
    { close the circle }
    C.FdPoint.Items.List^[SlicesNow] := C.FdPoint.Items.List^[0];

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

  FFdInnerRadius := TSFFloat.Create(Self, false, 'innerRadius', 0);
   FdInnerRadius.ChangeAlways := chGeometry;
  AddField(FFdInnerRadius);
  { X3D specification comment: [0,Inf) }

  FFdOuterRadius := TSFFloat.Create(Self, false, 'outerRadius', 1);
   FdOuterRadius.ChangeAlways := chGeometry;
  AddField(FFdOuterRadius);
  { X3D specification comment: (0,Inf) }

  FFdSolid := TSFBool.Create(Self, false, 'solid', false);
   FdSolid.ChangeAlways := chGeometry;
  AddField(FFdSolid);
end;

class function TDisk2DNode.ClassX3DType: string;
begin
  Result := 'Disk2D';
end;

function TDisk2DNode.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  { Rendering of TDisk2DNode not implemented. }
  Result := TBox3D.Empty;
end;

function TDisk2DNode.VerticesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  { Rendering of TDisk2DNode not implemented. }
  Result := 0;
end;

function TDisk2DNode.TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  { Rendering of TDisk2DNode not implemented. }
  Result := 0;
end;

procedure TPolyline2DNode.CreateNode;
begin
  inherited;

  FFdLineSegments := TMFVec2f.Create(Self, false, 'lineSegments', []);
   FdLineSegments.ChangeAlways := chGeometry;
  AddField(FFdLineSegments);
  { X3D specification comment: (-Inf,Inf) }

  FFdLineSegments.AddAlternativeName('point', 2);
end;

class function TPolyline2DNode.ClassX3DType: string;
begin
  Result := 'Polyline2D';
end;

function TPolyline2DNode.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  { Rendering of TPolyline2DNode not implemented. }
  Result := TBox3D.Empty;
end;

function TPolyline2DNode.VerticesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  { Rendering of TPolyline2DNode not implemented. }
  Result := 0;
end;

function TPolyline2DNode.TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  { Rendering of TPolyline2DNode not implemented. }
  Result := 0;
end;

procedure TPolypoint2DNode.CreateNode;
begin
  inherited;

  FFdPoint := TMFVec2f.Create(Self, true, 'point', []);
   FdPoint.ChangeAlways := chGeometry;
  AddField(FFdPoint);
  { X3D specification comment: (-Inf,Inf) }
end;

class function TPolypoint2DNode.ClassX3DType: string;
begin
  Result := 'Polypoint2D';
end;

function TPolypoint2DNode.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  { Rendering of TPolypoint2DNode not implemented. }
  Result := TBox3D.Empty;
end;

function TPolypoint2DNode.VerticesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  { Rendering of TPolypoint2DNode not implemented. }
  Result := 0;
end;

function TPolypoint2DNode.TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  { Rendering of TPolypoint2DNode not implemented. }
  Result := 0;
end;

procedure TRectangle2DNode.CreateNode;
begin
  inherited;

  FFdSize := TSFVec2f.Create(Self, false, 'size', Vector2(2, 2));
   FdSize.ChangeAlways := chGeometry;
  AddField(FFdSize);
  { X3D specification comment: (0,Inf) }

  FFdSolid := TSFBool.Create(Self, false, 'solid', false);
   FdSolid.ChangeAlways := chGeometry;
  AddField(FFdSolid);
end;

class function TRectangle2DNode.ClassX3DType: string;
begin
  Result := 'Rectangle2D';
end;

function Rectangle2DProxy(Original: TAbstractGeometryNode;
  const Size: TVector2; const Solid: boolean): TAbstractGeometryNode;
var
  Coord: TCoordinateNode;
  TexCoord: TTextureCoordinateNode;
  QuadSet: TQuadSetNode absolute Result;
begin
  QuadSet := TQuadSetNode.Create(Original.X3DName, Original.BaseUrl);
  try
    Coord := TCoordinateNode.Create('', Original.BaseUrl);
    Coord.FdPoint.Items.Count := 4;
    QuadSet.FdCoord.Value := Coord;

    TexCoord := TTextureCoordinateNode.Create('', Original.BaseUrl);
    TexCoord.FdPoint.Items.Count := 4;
    QuadSet.FdTexCoord.Value := TexCoord;

    { calculate Coord, TexCoord contents }

    Coord.FdPoint.Items.List^[0] := Vector3(-Size[0] / 2, -Size[1] / 2, 0);
    TexCoord.FdPoint.Items.List^[0] := Vector2(0, 0);

    Coord.FdPoint.Items.List^[1] := Vector3( Size[0] / 2, -Size[1] / 2, 0);
    TexCoord.FdPoint.Items.List^[1] := Vector2(1, 0);

    Coord.FdPoint.Items.List^[2] := Vector3( Size[0] / 2,  Size[1] / 2, 0);
    TexCoord.FdPoint.Items.List^[2] := Vector2(1, 1);

    Coord.FdPoint.Items.List^[3] := Vector3(-Size[0] / 2,  Size[1] / 2, 0);
    TexCoord.FdPoint.Items.List^[3] := Vector2(0, 1);

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

  FFdVertices := TMFVec2f.Create(Self, true, 'vertices', []);
   FdVertices.ChangeAlways := chGeometry;
  AddField(FFdVertices);
  { X3D specification comment: (-Inf,Inf) }

  FFdSolid := TSFBool.Create(Self, false, 'solid', false);
   FdSolid.ChangeAlways := chGeometry;
  AddField(FFdSolid);
end;

class function TTriangleSet2DNode.ClassX3DType: string;
begin
  Result := 'TriangleSet2D';
end;

function TTriangleSet2DNode.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  { Rendering of TTriangleSet2DNode not implemented. }
  Result := TBox3D.Empty;
end;

function TTriangleSet2DNode.VerticesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  { Rendering of TTriangleSet2DNode not implemented. }
  Result := 0;
end;

function TTriangleSet2DNode.TrianglesCount(State: TX3DGraphTraverseState; OverTriangulate: boolean;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  { Rendering of TTriangleSet2DNode not implemented. }
  Result := 0;
end;

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
