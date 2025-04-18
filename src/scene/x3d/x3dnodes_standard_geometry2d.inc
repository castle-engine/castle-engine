{%MainUnit x3dnodes.pas}
{
  Copyright 2002-2023 Michalis Kamburelis.

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
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;

  {$I auto_generated_node_helpers/x3dnodes_arc2d.inc}
  end;

  { Type of @link(TArcClose2DNode.ClosureType).
    See X3D specification about the possible arc closure types:
    https://www.web3d.org/documents/specifications/19775-1/V3.3/Part01/components/geometry2D.html#ArcClose2D }
  TArcClosureType = (acPie, acChord);

  { Closed circular arc in 2D.
    @bold(Rendering of this node is not implemented yet.) }
  TArcClose2DNode = class(TAbstractGeometryNode)
  public
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal; override;
    function SolidField: TSFBool; override;

  {$I auto_generated_node_helpers/x3dnodes_arcclose2d.inc}
  end;

  { Circle in 2D. }
  TCircle2DNode = class(TAbstractGeometryNode)
  strict private
    function CalculateSlices: Cardinal;
  public
    function Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode; override;

  {$I auto_generated_node_helpers/x3dnodes_circle2d.inc}
  end;

  { Disc (filled circle) in 2D. }
  TDisk2DNode = class(TAbstractGeometryNode)
  strict private
    function CalculateSlices: Cardinal;
  public
    function Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode; override;
    function SolidField: TSFBool; override;

    {$I auto_generated_node_helpers/x3dnodes_disk2d.inc}
  end;

  { Series of line segments in 2D. }
  TPolyline2DNode = class(TAbstractGeometryNode)
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: String = ''); override;
    function Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode; override;

  {$I auto_generated_node_helpers/x3dnodes_polyline2d.inc}
  end;

  { Set of vertices in 2D. }
  TPolypoint2DNode = class(TAbstractGeometryNode)
  public
    function Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode; override;

  {$I auto_generated_node_helpers/x3dnodes_polypoint2d.inc}
  end;

  { Rectangle in 2D. }
  TRectangle2DNode = class(TAbstractGeometryNode)
  public
    function Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode; override;
    function SolidField: TSFBool; override;

  {$I auto_generated_node_helpers/x3dnodes_rectangle2d.inc}
  end;

  { Set of triangles in 2D. }
  TTriangleSet2DNode = class(TAbstractGeometryNode)
  public
    function Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode; override;
    function SolidField: TSFBool; override;

  {$I auto_generated_node_helpers/x3dnodes_triangleset2d.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

const
  ArcClosureTypeNames: array [TArcClosureType] of string =
  ('PIE', 'CHORD');

{ TArc2DNode ----------------------------------------------------------------- }

function TArc2DNode.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  { Rendering of TArc2DNode not implemented. }
  Result := TBox3D.Empty;
end;

function TArc2DNode.VerticesCount(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  { Rendering of TArc2DNode not implemented. }
  Result := 0;
end;

function TArc2DNode.TrianglesCount(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  { Rendering of TArc2DNode not implemented. }
  Result := 0;
end;

{ TArcClose2DNode ----------------------------------------------------------------- }

function TArcClose2DNode.LocalBoundingBox(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D;
begin
  { Rendering of TArcClose2DNode not implemented. }
  Result := TBox3D.Empty;
end;

function TArcClose2DNode.VerticesCount(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  { Rendering of TArcClose2DNode not implemented. }
  Result := 0;
end;

function TArcClose2DNode.TrianglesCount(State: TX3DGraphTraverseState;
  ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): Cardinal;
begin
  { Rendering of TArcClose2DNode not implemented. }
  Result := 0;
end;

function TArcClose2DNode.SolidField: TSFBool;
begin
  Result := FdSolid;
end;

{ TCircle2DNode -------------------------------------------------------------- }

function TCircle2DNode.CalculateSlices: Cardinal;
begin
  // use default in case of 0 or invalid value
  if FdSlices.Value < MinTriangulationSlices then
    Result := DefaultTriangulationSlices
  else
    Result := FdSlices.Value;
end;

function TCircle2DNode.Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode;
var
  SlicesNow: Integer;
  C: TCoordinateNode;
  LineSet: TLineSetNode absolute Result;
  I: Integer;
  AngleSin, AngleCos, RX, RY: Single;
begin
  LineSet := TLineSetNode.Create(X3DName, BaseUrl);
  try
    SlicesNow := CalculateSlices;

    RX := Radius;
    if RadiusY <> 0 then
      RY := RadiusY
    else
      RY := RX;

    C := TCoordinateNode.Create('', BaseUrl);
    C.FdPoint.Items.Count := SlicesNow + 1;
    for I := 0 to SlicesNow - 1 do
    begin
      SinCos((I / SlicesNow) * 2 * Pi, AngleSin, AngleCos);
      C.FdPoint.Items.L[I] := Vector3(
        AngleSin * RX,
        AngleCos * RY,
        0
      );
    end;
    { close the circle }
    C.FdPoint.Items.L[SlicesNow] := C.FdPoint.Items.L[0];

    LineSet.Coord := C;
    LineSet.SetVertexCount([SlicesNow + 1]);
  except FreeAndNil(Result); raise end;
end;

{ TDisk2DNode ---------------------------------------------------------------- }

function TDisk2DNode.CalculateSlices: Cardinal;
begin
  // use default in case of 0 or invalid value
  if FdSlices.Value < MinTriangulationSlices then
    Result := DefaultTriangulationSlices
  else
    Result := FdSlices.Value;
end;

function TDisk2DNode.SolidField: TSFBool;
begin
  Result := FdSolid;
end;

function TDisk2DNode.Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode;
var
  SlicesNow: Integer;

  { Make a disc from triangles, no inner radius (no hole inside). }
  procedure ProxyTriangles;
  var
    C: TCoordinateNode;
    Triangles: TTriangleFanSetNode absolute Result;
    I: Integer;
    AngleSin, AngleCos: Float;
  begin
    Triangles := TTriangleFanSetNode.Create(X3DName, BaseUrl);
    try
      C := TCoordinateNode.Create('', BaseUrl);
      C.FdPoint.Items.Count := SlicesNow + 2;
      C.FdPoint.Items.L[0] := Vector3(0, 0, 0);
      for I := 0 to SlicesNow do
      begin
        SinCos((I / SlicesNow) * 2 * Pi, AngleSin, AngleCos);
        C.FdPoint.Items.L[I + 1] := Vector3(
          AngleCos * OuterRadius,
          AngleSin * OuterRadius,
          0
        );
      end;

      Triangles.Coord := C;
      Triangles.SetFanCount([SlicesNow + 2]);
      Triangles.Solid := Solid;
    except FreeAndNil(Result); raise end;
  end;

  { Make a disc from quads, with inner radius (hole inside). }
  procedure ProxyQuads;
  var
    C: TCoordinateNode;
    Quads: TIndexedQuadSetNode absolute Result;
    I: Integer;
    AngleSin, AngleCos: Float;
    Indexes: TInt32List;
  begin
    Quads := TIndexedQuadSetNode.Create(X3DName, BaseUrl);
    try
      C := TCoordinateNode.Create('', BaseUrl);
      C.FdPoint.Items.Count := SlicesNow * 2;
      for I := 0 to SlicesNow - 1 do
      begin
        SinCos((I / SlicesNow) * 2 * Pi, AngleSin, AngleCos);
        C.FdPoint.Items.L[I] := Vector3(
          AngleCos * OuterRadius,
          AngleSin * OuterRadius,
          0
        );
        C.FdPoint.Items.L[I + SlicesNow] := Vector3(
          AngleCos * InnerRadius,
          AngleSin * InnerRadius,
          0
        );
      end;

      Indexes := TInt32List.Create;
      try
        Indexes.Count := SlicesNow * 4;
        for I := 0 to SlicesNow - 1 do
        begin
          Indexes.L[I * 4    ] := I;
          Indexes.L[I * 4 + 1] := (I + 1) mod SlicesNow;
          Indexes.L[I * 4 + 2] := (I + 1) mod SlicesNow + SlicesNow;
          Indexes.L[I * 4 + 3] := I + SlicesNow;
        end;

        Quads.Coord := C;
        Quads.SetIndex(Indexes);
        Quads.Solid := Solid;
      finally FreeAndNil(Indexes); end;
    except FreeAndNil(Result); raise end;
  end;

begin
  SlicesNow := CalculateSlices;

  if InnerRadius <= 0 then
    ProxyTriangles
  else
    ProxyQuads;
end;

{ TPolyline2DNode ------------------------------------------------------------ }

constructor TPolyline2DNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  FdLineSegments.AddAlternativeName('point', 2);
end;

function TPolyline2DNode.Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode;
var
  C: TCoordinateNode;
  LineSet: TLineSetNode absolute Result;
  I, Count: Integer;
begin
  LineSet := TLineSetNode.Create(X3DName, BaseUrl);
  try
    Count := FdLineSegments.Items.Count;

    C := TCoordinateNode.Create('', BaseUrl);
    C.FdPoint.Items.Count := Count;
    for I := 0 to Count - 1 do
    begin
      C.FdPoint.Items.L[I] := Vector3(
        FdLineSegments.Items.L[I].X,
        FdLineSegments.Items.L[I].Y,
        0
      );
    end;

    LineSet.Coord := C;
    LineSet.SetVertexCount([Count]);
  except FreeAndNil(Result); raise end;
end;

{ TPolypoint2DNode ----------------------------------------------------------- }

function TPolypoint2DNode.Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode;
var
  C: TCoordinateNode;
  PointSet: TPointSetNode absolute Result;
  I, Count: Integer;
begin
  PointSet := TPointSetNode.Create(X3DName, BaseUrl);
  try
    Count := FdPoint.Items.Count;

    C := TCoordinateNode.Create('', BaseUrl);
    C.FdPoint.Items.Count := Count;
    for I := 0 to Count - 1 do
    begin
      C.FdPoint.Items.L[I] := Vector3(
        FdPoint.Items.L[I].X,
        FdPoint.Items.L[I].Y,
        0
      );
    end;

    PointSet.Coord := C;
  except FreeAndNil(Result); raise end;
end;

{ TRectangle2DNode ----------------------------------------------------------- }

function TRectangle2DNode.SolidField: TSFBool;
begin
  Result := FdSolid;
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

    Coord.FdPoint.Items.L[0] := Vector3(-Size[0] / 2, -Size[1] / 2, 0);
    TexCoord.FdPoint.Items.L[0] := Vector2(0, 0);

    Coord.FdPoint.Items.L[1] := Vector3( Size[0] / 2, -Size[1] / 2, 0);
    TexCoord.FdPoint.Items.L[1] := Vector2(1, 0);

    Coord.FdPoint.Items.L[2] := Vector3( Size[0] / 2,  Size[1] / 2, 0);
    TexCoord.FdPoint.Items.L[2] := Vector2(1, 1);

    Coord.FdPoint.Items.L[3] := Vector3(-Size[0] / 2,  Size[1] / 2, 0);
    TexCoord.FdPoint.Items.L[3] := Vector2(0, 1);

    QuadSet.Solid := Solid;
  except FreeAndNil(Result); raise end;
end;

function TRectangle2DNode.Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode;
begin
  Result := Rectangle2DProxy(Self, FdSize.Value, FdSolid.Value);
end;

{ TTriangleSet2DNode --------------------------------------------------------- }

function TTriangleSet2DNode.SolidField: TSFBool;
begin
  Result := FdSolid;
end;

function TTriangleSet2DNode.Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode;
var
  C: TCoordinateNode;
  TriangleSet: TTriangleSetNode absolute Result;
  I, Count: Integer;
begin
  TriangleSet := TTriangleSetNode.Create(X3DName, BaseUrl);
  try
    Count := FdVertices.Items.Count;

    C := TCoordinateNode.Create('', BaseUrl);
    C.FdPoint.Items.Count := Count;
    for I := 0 to Count - 1 do
    begin
      C.FdPoint.Items.L[I] := Vector3(
        FdVertices.Items.L[I].X,
        FdVertices.Items.L[I].Y,
        0
      );
    end;

    TriangleSet.Coord := C;
    TriangleSet.Solid := Solid;
  except FreeAndNil(Result); raise end;
end;

{ registration --------------------------------------------------------------- }

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
