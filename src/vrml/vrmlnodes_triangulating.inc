{
  Copyright 2003-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TVRMLGeometryNode.Triangulate (calls LocalTriangulate)
  -----------------------------------------------------------------------------}

type
  TTriangulateRedirect = class
    Transform: PMatrix4Single;
    NewTriangle: TNewTriangleProc;
    procedure LocalNewTriangle(const Tri: TTriangle3Single;
      Shape: TObject;
      const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
  end;

  procedure TTriangulateRedirect.LocalNewTriangle(const Tri: TTriangle3Single;
    Shape: TObject;
    const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
  begin
    NewTriangle(TriangleTransform(Tri, Transform^), Shape, MatNum,
      FaceCoordIndexBegin, FaceCoordIndexEnd);
  end;

procedure TVRMLGeometryNode.Triangulate(Shape: TObject; State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState);
var tr: TTriangulateRedirect;
begin
 { klasa TTriangulateRedirect bedzie dzialala jako posrednik :
   kazdy trojkat ktory zwroci LocalTriangulate bedzie transformowany przez
   State.Transform i dopiero wtedy przekazywany do NewTriangleProc.
   W ten sposob za pomoca LocalTriangulate implementujemy Triangulate }
 tr := TTriangulateRedirect.Create;
 try
  tr.Transform := @(State.Transform);
  tr.NewTriangle := NewTriangleProc;
  LocalTriangulate(Shape, State, OverTriangulate,
    {$ifdef FPC_OBJFPC} @ {$endif} tr.LocalNewTriangle, ProxyGeometry, ProxyState);
 finally tr.Free end;
end;

{ TVRMLGeometryNode.LocalTriangulate (using Proxy)
  ---------------------------------------------------------------------------- }

type
  TProxyRedirect = class
    NewTriangle: TNewTriangleProc;
    procedure Triangle(const Triangle: TTriangle3Single;
      Shape: TObject;
      const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
  end;

procedure TProxyRedirect.Triangle(const Triangle: TTriangle3Single;
  Shape: TObject;
  const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
begin
  NewTriangle(Triangle, Shape, 0, -1, -1);
end;

procedure TVRMLGeometryNode.LocalTriangulate(Shape: TObject; State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState);
var
  ProxyRedirect: TProxyRedirect;
begin
  if ProxyGeometry <> nil then
  begin
    ProxyRedirect := TProxyRedirect.Create;
    try
      ProxyRedirect.NewTriangle := NewTriangleProc;
      ProxyGeometry.LocalTriangulate(Shape, ProxyState, OverTriangulate, @ProxyRedirect.Triangle, nil, nil);
    finally FreeAndNil(ProxyRedirect) end;
  end else
    raise EInternalError.Create('TVRMLGeometryNode.LocalTriangulate not overridden, and node without a Proxy');
end;

{ pomocnicze procedury LocalTriangulate* dla jakichs tworow geometrycznych
  ------------------------------------------------------------------------------}

(* unused for now

procedure LocalTriangulateConvexPolygon(const Poly: array of TVector3Single;
  State: TVRMLGraphTraverseState; Node: TVRMLNode; NewTriangleProc: TNewTriangleProc);
var t: TTriangle3Single;
    i: integer;
begin
 i := 0;
 t[0] := Poly[0];
 while i+2 <= High(Poly) do
 begin
  t[1] := Poly[i+1];
  t[2] := Poly[i+2];
  NewTriangleProc(T, State, Node);
  Inc(i);
 end;
end;
*)

procedure LocalTriangulateRect(constCoord: integer;
  const constCoordValue, x1, y1, x2, y2: Single;
  Shape: TObject; NewTriangleProc: TNewTriangleProc);
var T: TTriangle3Single;
    i, c1, c2: integer;

  procedure TriAssign(TriIndex: integer; c1value, c2value: Single);
  begin
   T[TriIndex, c1] := c1value;
   T[TriIndex, c2] := c2value;
  end;

begin
  for i := 0 to 2 do T[i, constCoord] := constCoordValue;
  RestOf3dCoords(constCoord, c1, c2);
  TriAssign(0, x1, y1);
  TriAssign(1, x1, y2);
  TriAssign(2, x2, y2);
  NewTriangleProc(T, Shape, 0, -1, -1);
  TriAssign(0, x1, y1);
  TriAssign(1, x2, y2);
  TriAssign(2, x2, y1);
  NewTriangleProc(T, Shape, 0, -1, -1);
end;

{ Text nodes ----------------------------------------------------------------- }

{ VRML >= 2.0 specs say that 2D Text doesn't participate in collision
  detection. This is very sensible, as normal triangulation of Text would
  produce a lot of triangles. On the other hard, I found many VRML models
  that expect Text within Anchor and TouchSensor to be "clickable" ---
  which means that some rough triangulation of text is desired.

  So I generate 2 triangles based on Text bounding box.
  TODO: I should somehow mark them as "not participate in collision
  detection", and allow octree operations to optionally ignore
  such marked triangles. }

procedure TNodeAsciiText_1.LocalTriangulate(Shape: TObject; State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState);
var
  Box: TBox3D;
begin
  Box := LocalBoundingBox(State, ProxyGeometry, ProxyState);
  LocalTriangulateRect(2, 0, Box[0][0], Box[0][1], Box[1][0], Box[1][1],
    Shape, NewTriangleProc);
end;

procedure TNodeText.LocalTriangulate(Shape: TObject; State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState);
var
  Box: TBox3D;
begin
  Box := LocalBoundingBox(State, ProxyGeometry, ProxyState);
  LocalTriangulateRect(2, 0, Box[0][0], Box[0][1], Box[1][0], Box[1][1],
    Shape, NewTriangleProc);
end;

procedure TNodeText3D.LocalTriangulate(Shape: TObject; State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState);
begin
  { TODO: don't really know what to do for Text3D here, I certainly
    don't want to triangulate them, and using their bounding box
    could also be uncomfortable (for large depth, bbox would be large,
    and transformed bbox would contain a lot of additional space ---
    something that only oriented bounding boxes could remedy). }
end;

{ IndexedFaceSet ------------------------------------------------------------- }

type
  TIndexedFaceSetTriangulator = class
  public
    { This is passed all the way, from IndexedFaceSet_LocalTriangulate
      down to TriangleIndexes. }
    Shape: TObject;
    State: TVRMLGraphTraverseState;
    NewTriangleProc: TNewTriangleProc;
    Coord: TMFVec3f;

    { This is passed from IndexedFaceSet_LocalTriangulate to HandleCoordRange. }
    { VRML1MaterialBinding should be one of BIND_xxx constants,
      VRML1MaterialIndex may be nil if VRML1MaterialBinding is not
      anything _INDEXED. }
    VRML1MaterialBinding: Integer;
    VRML1MaterialIndex: TMFLong;
    CoordIndex: TMFLong;
    FacesConvex: boolean;

    procedure HandleCoordRange(const RangeNumber: Cardinal;
      BeginIndex, EndIndex: Integer);
  public
    { This is passed from HandleCoordRange to TriangleIndexes. }
    VRML1MaterialNumber: Integer;
    FaceIndices: PArray_Longint;
    FaceCoordIndexBegin, FaceCoordIndexEnd: Integer;

    procedure TriangleIndexes(const TriIndices: TVector3Longint);
  end;

procedure TIndexedFaceSetTriangulator.TriangleIndexes(
  const TriIndices: TVector3Longint);
var
  Tri: TTriangle3Single;
begin
  Tri[0] := Coord.ItemsSafe[FaceIndices^[TriIndices[0]]];
  Tri[1] := Coord.ItemsSafe[FaceIndices^[TriIndices[1]]];
  Tri[2] := Coord.ItemsSafe[FaceIndices^[TriIndices[2]]];
  NewTriangleProc(Tri, Shape, VRML1MaterialNumber,
    FaceCoordIndexBegin, FaceCoordIndexEnd);
end;

procedure TIndexedFaceSetTriangulator.HandleCoordRange(
  const RangeNumber: Cardinal;
  BeginIndex, EndIndex: Integer);
begin
  FaceIndices := PArray_Longint(CoordIndex.Items.Pointers[BeginIndex]);
  FaceCoordIndexBegin := BeginIndex;
  FaceCoordIndexEnd := EndIndex;

  case VRML1MaterialBinding of
    BIND_DEFAULT, BIND_OVERALL:
      VRML1MaterialNumber := 0;
    BIND_PER_PART, BIND_PER_FACE:
      VRML1MaterialNumber := RangeNumber;
    BIND_PER_PART_INDEXED, BIND_PER_FACE_INDEXED:
      VRML1MaterialNumber := VRML1MaterialIndex.Items.Items[RangeNumber];
    { TODO: we don't support here materials per vertex or per vertex indexed }
    else VRML1MaterialNumber := 0;
  end;

  if FacesConvex then
    TriangulateConvexFace(EndIndex - BeginIndex,
      @TriangleIndexes, 0) else
    TriangulateFace(FaceIndices, EndIndex - BeginIndex,
      Coord.Items.ItemsArray, @TriangleIndexes, 0);
end;

procedure TNodeIndexedFaceSet_1.LocalTriangulate(Shape: TObject;
  State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  NewTriangleProc: TNewTriangleProc;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState);
var
  Triangulator: TIndexedFaceSetTriangulator;
begin
  Triangulator := TIndexedFaceSetTriangulator.Create;
  try
    Triangulator.Shape := Shape;
    Triangulator.State := State;
    Triangulator.NewTriangleProc := NewTriangleProc;
    Triangulator.Coord := State.LastNodes.Coordinate3.FdPoint;
    Triangulator.CoordIndex := FdCoordIndex;
    Triangulator.VRML1MaterialBinding := State.LastNodes.MaterialBinding.FdValue.Value;
    Triangulator.VRML1MaterialIndex := FdMaterialIndex;
    Triangulator.FacesConvex := State.LastNodes.ShapeHints.FdFaceType.Value = FACETYPE_CONVEX;
    MakeCoordRanges(State, @Triangulator.HandleCoordRange);
  finally FreeAndNil(Triangulator) end;
end;

procedure TNodeIndexedFaceSet_2.LocalTriangulate(Shape: TObject; State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState);
var
  Triangulator: TIndexedFaceSetTriangulator;
begin
  if Coordinates(State) = nil then Exit;

  Triangulator := TIndexedFaceSetTriangulator.Create;
  try
    Triangulator.Shape := Shape;
    Triangulator.State := State;
    Triangulator.NewTriangleProc := NewTriangleProc;
    Triangulator.Coord := Coordinates(State);
    Triangulator.CoordIndex := FdCoordIndex;
    Triangulator.VRML1MaterialBinding := BIND_DEFAULT;
    Triangulator.VRML1MaterialIndex := nil;
    Triangulator.FacesConvex := FdConvex.Value;
    MakeCoordRanges(State, @Triangulator.HandleCoordRange);
  finally FreeAndNil(Triangulator) end;
end;

{ Others --------------------------------------------------------------------- }

procedure TNodeIndexedLineSet_1.LocalTriangulate(Shape: TObject; State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState);
begin
  { No triangles }
end;

procedure TNodeIndexedLineSet_2.LocalTriangulate(Shape: TObject; State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState);
begin
  { No triangles }
end;

procedure TNodeLineSet.LocalTriangulate(Shape: TObject; State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState);
begin
  { No triangles }
end;

procedure TNodePointSet_1.LocalTriangulate(Shape: TObject; State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState);
begin
  { No triangles }
end;

procedure TNodePointSet_2.LocalTriangulate(Shape: TObject; State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState);
begin
  { No triangles }
end;

{ TNodeElevationGrid --------------------------------------------------------- }

procedure TNodeElevationGrid.LocalTriangulate(Shape: TObject; State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState);
var
  I, J: Integer;
  Last1, Last2, Next1, Next2: TVector3Single;
begin
  if IsNotEmpty then
  begin
    for I := 1 to FdXDimension.Value - 1 do
    begin
      Last1[0] := FdXSpacing.Value * (I - 1);
      Last1[1] := FdHeight.Items.Items[I - 1];
      Last1[2] := 0;
      Last2[0] := FdXSpacing.Value * I;
      Last2[1] := FdHeight.Items.Items[I];
      Last2[2] := 0;
      { Next X-coordinate will be const for the life of "for J := " below. }
      Next1[0] := Last1[0];
      Next2[0] := Last2[0];
      for J := 1 to FdZDimension.Value - 1 do
      begin
        Next1[1] := FdHeight.Items.Items[I - 1 + J * FdXDimension.Value];
        Next1[2] := FdZSpacing.Value * J;
        Next2[1] := FdHeight.Items.Items[I + J * FdXDimension.Value];
        Next2[2] := Next1[2];

        { Should be revert here order based on FdCcw.Value ?
          As far as I understand, no : ccw FALSE tells me that
          normals point download (normal y is negative) and I
          should eventually cull (if solid TRUE) the other side. }
        NewTriangleProc(Triangle3Single(Last2, Last1, Next1), Shape, 0, -1, -1);
        NewTriangleProc(Triangle3Single(Last2, Next1, Next2), Shape, 0, -1, -1);

        Last1 := Next1;
        Last2 := Next2;
      end;
    end;
  end;
end;

{ TCoordinateTriangulator ---------------------------------------------------- }

type
  { Base class for triangulating VRML nodes using "coord" field.
    Somewhat analogous to TAbstractCoordinateRenderer, except
    TAbstractCoordinateRenderer does OpenGL rendering and this is for
    OpenGL-independent triangulating. }
  TCoordinateTriangulator = class
    Shape: TObject;
    State: TVRMLGraphTraverseState;

    VRML1MaterialBinding: Integer;
    VRML1MaterialIndex: TMFLong;

    { Coord is always non-nil here. }
    Coord: TDynVector3SingleArray;
    CoordIndex: TDynLongIntArray;

    NewTriangleProc: TNewTriangleProc;

    procedure Polygon(const Indexes: array of Cardinal);
  end;

procedure TCoordinateTriangulator.Polygon(const Indexes: array of Cardinal);
var
  VRML1MaterialNumber: Integer;
  RangeNumber: Cardinal;
  I: Integer;
  Triangle: TTriangle3Single;

  function Index(I: Integer): Integer;
  begin
    if CoordIndex <> nil then
      Result := CoordIndex.Items[I] else
      Result := I;
  end;

begin
  { TODO: dummy value. Matters only for Inventor IndexedTriangleMesh anyway. }
  RangeNumber := 0;

  case VRML1MaterialBinding of
    BIND_DEFAULT, BIND_OVERALL:
      VRML1MaterialNumber := 0;
    BIND_PER_PART, BIND_PER_FACE:
      VRML1MaterialNumber := RangeNumber;
    BIND_PER_PART_INDEXED, BIND_PER_FACE_INDEXED:
      VRML1MaterialNumber := VRML1MaterialIndex.Items.Items[RangeNumber];
    { TODO: we don't support here materials per vertex or per vertex indexed }
    else VRML1MaterialNumber := 0;
  end;

  { Now triangulate a simple (convex) polygon to triangles.
    I could call TriangulateConvexFace, but doing it directly is actually
    simpler. }

  if High(Indexes) >= 2 then
  begin
    Triangle[0] := Coord.Items[Index(Indexes[0])];
    Triangle[1] := Coord.Items[Index(Indexes[1])];
    Triangle[2] := Coord.Items[Index(Indexes[2])];
    NewTriangleProc(Triangle, Shape, VRML1MaterialNumber, -1, -1);
    I := 3;
    while I <= High(Indexes) do
    begin
      Triangle[1] := Triangle[2];
      Triangle[2] := Coord.Items[Index(Indexes[I])];
      NewTriangleProc(Triangle, Shape, VRML1MaterialNumber, -1, -1);
      Inc(I);
    end;
  end;
end;

{ Implements LocalTriangulate method for coordinate-based nodes,
  using node's CoordPolygons method. }
procedure LocalTriangulateCoordPolygons(
  Geometry: TVRMLGeometryNode;
  Shape: TObject;
  State: TVRMLGraphTraverseState;
  VRML1MaterialBinding: Integer;
  VRML1MaterialIndex: TMFLong;
  NewTriangleProc: TNewTriangleProc);
var
  Triangulator: TCoordinateTriangulator;
begin
  if Geometry.Coordinates(State) = nil then Exit;

  Triangulator := TCoordinateTriangulator.Create;
  try
    Triangulator.Shape := Shape;
    Triangulator.State := State;
    Triangulator.VRML1MaterialBinding := VRML1MaterialBinding;
    Triangulator.VRML1MaterialIndex := VRML1MaterialIndex;
    Triangulator.NewTriangleProc := NewTriangleProc;

    Triangulator.Coord := Geometry.Coordinates(State).Items;
    if Geometry.CoordIndex <> nil then
      Triangulator.CoordIndex := Geometry.CoordIndex.Items else
      Triangulator.CoordIndex := nil;

    Geometry.CoordPolygons(State, @Triangulator.Polygon);
  finally FreeAndNil(Triangulator) end;
end;

procedure TNodeIndexedTriangleMesh_1.LocalTriangulate(Shape: TObject;
  State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  NewTriangleProc: TNewTriangleProc;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState);
begin
  LocalTriangulateCoordPolygons(Self, Shape, State,
    State.LastNodes.MaterialBinding.FdValue.Value, FdMaterialIndex,
    NewTriangleProc);
end;

procedure TNodeIndexedTriangleSet.LocalTriangulate(Shape: TObject; State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState);
begin
  LocalTriangulateCoordPolygons(Self, Shape, State,
    BIND_DEFAULT, nil, NewTriangleProc);
end;

procedure TNodeTriangleSet.LocalTriangulate(Shape: TObject; State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState);
begin
  LocalTriangulateCoordPolygons(Self, Shape, State,
    BIND_DEFAULT, nil, NewTriangleProc);
end;

procedure TNodeIndexedTriangleFanSet.LocalTriangulate(Shape: TObject; State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState);
begin
  LocalTriangulateCoordPolygons(Self, Shape, State,
    BIND_DEFAULT, nil, NewTriangleProc);
end;

procedure TNodeTriangleFanSet.LocalTriangulate(Shape: TObject; State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState);
begin
  LocalTriangulateCoordPolygons(Self, Shape, State,
    BIND_DEFAULT, nil, NewTriangleProc);
end;

procedure TNodeIndexedTriangleStripSet.LocalTriangulate(Shape: TObject; State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState);
begin
  LocalTriangulateCoordPolygons(Self, Shape, State,
    BIND_DEFAULT, nil, NewTriangleProc);
end;

procedure TNodeTriangleStripSet.LocalTriangulate(Shape: TObject; State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState);
begin
  LocalTriangulateCoordPolygons(Self, Shape, State,
    BIND_DEFAULT, nil, NewTriangleProc);
end;

procedure TNodeIndexedQuadSet.LocalTriangulate(Shape: TObject; State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState);
begin
  LocalTriangulateCoordPolygons(Self, Shape, State,
    BIND_DEFAULT, nil, NewTriangleProc);
end;

procedure TNodeQuadSet.LocalTriangulate(Shape: TObject; State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState);
begin
  LocalTriangulateCoordPolygons(Self, Shape, State,
    BIND_DEFAULT, nil, NewTriangleProc);
end;

{ ---------------------------------------------------------------------------- }

procedure Rectangle2D_LocalTriangulate(Shape: TObject;
  State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc;
  FdSize: TSFVec2f);
var
  Triangle: TTriangle3Single;
  V: TVector3Single;
begin
  V[0] := -FdSize.Value[0] / 2;
  V[1] := -FdSize.Value[1] / 2;
  V[2] := 0;

  Triangle[0] := V;
  V[1] := -V[1];
  Triangle[1] := V;
  V[0] := -V[0];
  Triangle[2] := V;
  NewTriangleProc(Triangle, Shape, 0, -1, -1);

  Triangle[1] := Triangle[2];
  V[1] := -V[1];
  Triangle[2] := V;
  NewTriangleProc(Triangle, Shape, 0, -1, -1);
end;

procedure TNodeRectangle2D.LocalTriangulate(Shape: TObject; State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState);
begin
  Rectangle2D_LocalTriangulate(Shape, State, OverTriangulate, NewTriangleProc,
    FdSize);
end;

procedure TNodePlane.LocalTriangulate(Shape: TObject; State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState);
begin
  Rectangle2D_LocalTriangulate(Shape, State, OverTriangulate, NewTriangleProc,
    FdSize);
end;

procedure TNodeCircle2D.LocalTriangulate(Shape: TObject; State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc;
  ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState);
begin
  { No triangles }
end;