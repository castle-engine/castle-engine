{
  Copyright 2003-2008 Michalis Kamburelis.

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

{ TVRMLGeometryNode.Triangulate (calls LocalTriangulate)
  -----------------------------------------------------------------------------}

{ just a shortcuts for names often used in this file }
{$define QUADRIC_SLICES := (State.LastNodes.KambiTriangulation.QuadricSlices)}
{$define QUADRIC_STACKS := (State.LastNodes.KambiTriangulation.QuadricStacks)}
{$define RECT_DIVISIONS := (State.LastNodes.KambiTriangulation.RectDivisions)}

type
  TTriangulateRedirect = class
    Transform: PMatrix4Single;
    NewTriangle: TNewTriangleProc;
    procedure LocalNewTriangle(const Tri: TTriangle3Single;
      State: TVRMLGraphTraverseState;
      GeometryNode: TVRMLGeometryNode;
      const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
  end;

  procedure TTriangulateRedirect.LocalNewTriangle(const Tri: TTriangle3Single;
    State: TVRMLGraphTraverseState; GeometryNode: TVRMLGeometryNode;
    const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
  var i: integer;
      NewT: TTriangle3Single;
  begin
   for i := 0 to 2 do NewT[i] := MultMatrixPoint(Transform^, Tri[i]);
   NewTriangle(NewT, State, GeometryNode, MatNum,
     FaceCoordIndexBegin, FaceCoordIndexEnd);
  end;

procedure TVRMLGeometryNode.Triangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
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
  LocalTriangulate(State, OverTriangulate,
    {$ifdef FPC_OBJFPC} @ {$endif} tr.LocalNewTriangle);
 finally tr.Free end;
end;

{ TVRMLGeometryNode.LocalTriangulate (using Proxy)
  ---------------------------------------------------------------------------- }

type
  TProxyRedirect = class
    NewTriangle: TNewTriangleProc;
    SourceGeometry: TVRMLGeometryNode;
    procedure Triangle(const Triangle: TTriangle3Single;
      State: TVRMLGraphTraverseState;
      GeometryNode: TVRMLGeometryNode;
      const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
  end;

procedure TProxyRedirect.Triangle(const Triangle: TTriangle3Single;
  State: TVRMLGraphTraverseState; GeometryNode: TVRMLGeometryNode;
  const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
begin
  { We have to change GeometryNode to SourceGeometry
    (otherwise NewTriangle callback would receive a reference
    to temporary instance of Proxy, which would be very wrong
    --- for example, such triangle information could get stored in
    VRML triangle octree, and then used when picking with mouse).

    Also, since SourceGeometry has probably different idea of what
    is "face" etc., MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd
    are reset to standard values. }

  NewTriangle(Triangle, State, SourceGeometry, 0, -1, -1);
end;

procedure TVRMLGeometryNode.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
var
  P: TVRMLGeometryNode;
  ProxyRedirect: TProxyRedirect;
begin
  P := Proxy;
  if P <> nil then
  begin
    ProxyRedirect := TProxyRedirect.Create;
    try
      ProxyRedirect.SourceGeometry := Self;
      ProxyRedirect.NewTriangle := NewTriangleProc;
      P.LocalTriangulate(State, OverTriangulate, @ProxyRedirect.Triangle);
    finally
      FreeAndNil(ProxyRedirect);
      FreeAndNil(P);
    end;
  end else
    raise EInternalError.Create('TVRMLGeometryNode.TrianglesCount not overridden, and node without a Proxy');
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
  State: TVRMLGraphTraverseState; GeometryNode: TVRMLGeometryNode;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc; MatNum: integer);
var T: TTriangle3Single;
    i, c1, c2: integer;

  procedure TriAssign(TriIndex: integer; c1value, c2value: Single);
  begin
   T[TriIndex, c1] := c1value;
   T[TriIndex, c2] := c2value;
  end;

begin
 Assert(not OverTriangulate); { TODO: OverTriangulate not used here so not impl yet}

 for i := 0 to 2 do T[i, constCoord] := constCoordValue;
 RestOf3dCoords(constCoord, c1, c2);
 TriAssign(0, x1, y1);
 TriAssign(1, x1, y2);
 TriAssign(2, x2, y2);
 NewTriangleProc(T, State, GeometryNode, MatNum, -1, -1);
 TriAssign(0, x1, y1);
 TriAssign(1, x2, y2);
 TriAssign(2, x2, y1);
 NewTriangleProc(T, State, GeometryNode, MatNum, -1, -1);
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

procedure TNodeAsciiText_1.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
var
  Box: TBox3d;
begin
  Box := LocalBoundingBox(State);
  LocalTriangulateRect(2, 0, Box[0][0], Box[0][1], Box[1][0], Box[1][1],
    State, Self, false, NewTriangleProc, 0);
end;

procedure TNodeText.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
var
  Box: TBox3d;
begin
  Box := LocalBoundingBox(State);
  LocalTriangulateRect(2, 0, Box[0][0], Box[0][1], Box[1][0], Box[1][1],
    State, Self, false, NewTriangleProc, 0);
end;

procedure TNodeText3D.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
begin
  { TODO: don't really know what to do for Text3D here, I certainly
    don't want to triangulate them, and using their bounding box
    could also be uncomfortable (for large depth, bbox would be large,
    and transformed bbox would contain a lot of additional space ---
    something that only oriented bounding boxes could remedy). }
end;

{ metody LocalTriangulate konkretnych node'ow ---------------------------------}

{$define LOOP_OVER_CIRCLE_DECLARE:=
var LOC_AngleRad: Single;
    LOC_LastPt, LOC_Pt: TVector3Single;
    LOC_i: integer;}

{$define LOOP_OVER_CIRCLE:=
begin
 LOC_LastPt := Vector3Single(0, LOOP_OVER_CIRCLE_HEIGHT, LOOP_OVER_CIRCLE_RADIUS);
 for LOC_i := 1 to QUADRIC_SLICES-1 do
 begin
  LOC_AngleRad:=(LOC_i/QUADRIC_SLICES)*2*Pi;
  LOC_Pt := Vector3Single(Sin(LOC_AngleRad)*LOOP_OVER_CIRCLE_RADIUS,
                        LOOP_OVER_CIRCLE_HEIGHT,
                        Cos(LOC_AngleRad)*LOOP_OVER_CIRCLE_RADIUS);
  LOOP_OVER_CIRCLE_PROC(LOC_LastPt, LOC_Pt);
  LOC_LastPt := LOC_Pt;
 end;

 LOC_Pt := Vector3Single(0, LOOP_OVER_CIRCLE_HEIGHT, LOOP_OVER_CIRCLE_RADIUS);
 LOOP_OVER_CIRCLE_PROC(LOC_LastPt, LOC_Pt);
end;}

procedure Cone_LocalTriangulate(
  Self: TVRMLGeometryNode;
  State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc;
  const BottomRadius, Height: Single;
  const Sides, Bottom: boolean);
var BottomPt, TopPt: TVector3Single;
    MatBottom, MatSides: integer;

  procedure LOOP_OVER_CIRCLE_PROC(const LastPt, Pt: TVector3Single);
  begin
   if Bottom then
     NewTriangleProc(Triangle3Single(LastPt, Pt, BottomPt),
       State, Self, MatBottom, -1, -1);
   if Sides then
     NewTriangleProc(Triangle3Single(LastPt, Pt, TopPt),
       State, Self, MatSides, -1, -1);
  end;

LOOP_OVER_CIRCLE_DECLARE
begin
 Assert(not OverTriangulate); { TODO: OverTriangulate not used here so not impl yet }

 if State.LastNodes.MaterialBinding.FdValue.Value in
   [BIND_PER_PART, BIND_PER_PART_INDEXED] then
  begin MatSides := 0; MatBottom := 1 end else
  begin MatSides := 0; MatBottom := 0 end;

 if Sides or Bottom then
 begin
  if Bottom then
   BottomPt := Vector3Single(0, -Height/2, 0);
  if Sides then
   TopPt := Vector3Single(0, Height/2, 0);

  {$define LOOP_OVER_CIRCLE_RADIUS:=(BottomRadius)}
  {$define LOOP_OVER_CIRCLE_HEIGHT:=(-Height/2)}
  LOOP_OVER_CIRCLE
 end;
end;

procedure TNodeCone_1.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
begin
  Cone_LocalTriangulate(Self, State, OverTriangulate, NewTriangleProc,
    FdBottomRadius.Value, FdHeight.Value,
    FdParts.Flags[CONE_PARTS_SIDES], FdParts.Flags[CONE_PARTS_BOTTOM]);
end;

procedure TNodeCone_2.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
begin
  Cone_LocalTriangulate(Self, State, OverTriangulate, NewTriangleProc,
    FdBottomRadius.Value, FdHeight.Value,
    FdSide.Value, FdBottom.Value);
end;

procedure Box_LocalTriangulate(
  Self: TVRMLGeometryNode;
  State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc;
  const SizeX, SizeY, SizeZ: Single);
var MatPerPart: boolean;

  procedure LocalTriangulateBox3d(const Box: TBox3d;
    State: TVRMLGraphTraverseState; GeometryNode: TVRMLGeometryNode;
    OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
  { nadaje materialy zgodnie ze specyfikacja VRMLa dla Cube }
  const MaterialsPerPart: array[0..2, boolean]of integer = ((2, 3), (5, 4), (1, 0));
  var i, c1, c2, MatNum: integer;
  begin
   for i := 0 to 2 do
   begin
    RestOf3dCoords(i, c1, c2);
    if MatPerPart then MatNum := MaterialsPerPart[i, false] else MatNum := 0;
    LocalTriangulateRect(i, Box[0, i], Box[0, c1], Box[0, c2], Box[1, c1], Box[1, c2],
      State, GeometryNode, OverTriangulate, NewTriangleProc, MatNum);
    if MatPerPart then MatNum := MaterialsPerPart[i, true] else MatNum := 0;
    LocalTriangulateRect(i, Box[1, i], Box[0, c1], Box[0, c2], Box[1, c1], Box[1, c2],
      State, GeometryNode, OverTriangulate, NewTriangleProc, MatNum);
   end;
  end;

var Box: TBox3d;
begin
 MatPerPart := State.LastNodes.MaterialBinding.FdValue.Value in [
   BIND_PER_PART, BIND_PER_PART_INDEXED, BIND_PER_FACE, BIND_PER_FACE_INDEXED];
 Box[0] := Vector3Single(-SizeX / 2, -SizeY / 2, -SizeZ / 2);
 Box[1] := Vector3Single( SizeX / 2,  SizeY / 2,  SizeZ / 2);
 LocalTriangulateBox3d(box, State, Self, OverTriangulate, NewTriangleProc);
end;

procedure TNodeCube_1.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
begin
  Box_LocalTriangulate(Self, State, OverTriangulate, NewTriangleProc,
    FdWidth.Value, FdHeight.Value, FdDepth.Value);
end;

procedure TNodeBox.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
begin
  Box_LocalTriangulate(Self, State, OverTriangulate, NewTriangleProc,
    FdSize.Value[0], FdSize.Value[1], FdSize.Value[2]);
end;

procedure Cylinder_LocalTriangulate(
  Self: TVRMLGeometryNode;
  State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc;
  const Height, Radius: Single;
  const Bottom, Side, Top: boolean);
var BottomPt, TopPt: TVector3Single;
    MatSides, MatTop, MatBottom: integer;

  procedure LOOP_OVER_CIRCLE_PROC(const LowLastPt, LowPt: TVector3Single);
  var HighLastPt, HighPt: TVector3Single;
  begin
   HighLastPt := Vector3Single(LowLastPt[0], Height/2, LowLastPt[2]);
   HighPt     := Vector3Single(LowPt    [0], Height/2, LowPt    [2]);

   if Bottom then
     NewTriangleProc(Triangle3Single(LowLastPt, LowPt, BottomPt),
       State, Self, MatBottom, -1, -1);

   if Top then
     NewTriangleProc(Triangle3Single(HighLastPt, HighPt, TopPt),
       State, Self, MatTop, -1, -1);

   if Side then
   begin
     NewTriangleProc(Triangle3Single(LowLastPt, LowPt, HighPt),
       State, Self, MatSides, -1, -1);
     NewTriangleProc(Triangle3Single(HighLastPt, HighPt, LowLastPt),
       State, Self, MatSides, -1, -1);
   end;
  end;

LOOP_OVER_CIRCLE_DECLARE
begin
 Assert(not OverTriangulate); { TODO: OverTriangulate not used here so not impl yet }

 if State.LastNodes.MaterialBinding.FdValue.Value in
     [BIND_PER_PART, BIND_PER_PART_INDEXED] then
  begin MatSides := 0; MatTop := 1; MatBottom := 2 end else
  begin MatSides := 0; MatTop := 0; MatBottom := 0 end;

 if Bottom or Side or Top then
 begin
  if Top then
   TopPt := Vector3Single(0, Height/2, 0);
  if Bottom then
   BottomPt := Vector3Single(0, -Height/2, 0);

  {$define LOOP_OVER_CIRCLE_RADIUS:=(Radius)}
  {$define LOOP_OVER_CIRCLE_HEIGHT:=(-Height/2)}
  LOOP_OVER_CIRCLE
 end;
end;

procedure TNodeCylinder_1.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
begin
  Cylinder_LocalTriangulate(
    Self, State, OverTriangulate, NewTriangleProc,
    FdHeight.Value, FdRadius.Value,
    FdParts.Flags[CYLINDER_PARTS_BOTTOM],
    FdParts.Flags[CYLINDER_PARTS_SIDES],
    FdParts.Flags[CYLINDER_PARTS_TOP]);
end;

procedure TNodeCylinder_2.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
begin
  Cylinder_LocalTriangulate(
    Self, State, OverTriangulate, NewTriangleProc,
    FdHeight.Value, FdRadius.Value,
    FdBottom.Value, FdSide.Value, FdTop.Value);
end;

{ IndexedFaceSet ------------------------------------------------------------- }

type
  TIndexedFaceSetTriangulator = class
    { This is passed all the way, from IndexedFaceSet_LocalTriangulate
      down to TriangleIndexes. }
    Geometry: TVRMLGeometryNode;
    State: TVRMLGraphTraverseState;
    NewTriangleProc: TNewTriangleProc;
    Coord: TDynVector3SingleArray;

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
  Tri[0] := Coord.Items[FaceIndices^[TriIndices[0]]];
  Tri[1] := Coord.Items[FaceIndices^[TriIndices[1]]];
  Tri[2] := Coord.Items[FaceIndices^[TriIndices[2]]];
  NewTriangleProc(Tri, State, Geometry, VRML1MaterialNumber,
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
      Coord.ItemsArray, @TriangleIndexes, 0);
end;

procedure TNodeIndexedFaceSet_1.LocalTriangulate(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  NewTriangleProc: TNewTriangleProc);
var
  Triangulator: TIndexedFaceSetTriangulator;
begin
  Triangulator := TIndexedFaceSetTriangulator.Create;
  try
    Triangulator.Geometry := Self;
    Triangulator.State := State;
    Triangulator.NewTriangleProc := NewTriangleProc;
    Triangulator.Coord := State.LastNodes.Coordinate3.FdPoint.Items;
    Triangulator.CoordIndex := FdCoordIndex;
    Triangulator.VRML1MaterialBinding := State.LastNodes.MaterialBinding.FdValue.Value;
    Triangulator.VRML1MaterialIndex := FdMaterialIndex;
    Triangulator.FacesConvex := State.LastNodes.ShapeHints.FdFaceType.Value = FACETYPE_CONVEX;
    MakeCoordRanges(State, @Triangulator.HandleCoordRange);
  finally FreeAndNil(Triangulator) end;
end;

procedure TNodeIndexedFaceSet_2.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
var
  Triangulator: TIndexedFaceSetTriangulator;
begin
  if Coordinates(State) = nil then Exit;

  Triangulator := TIndexedFaceSetTriangulator.Create;
  try
    Triangulator.Geometry := Self;
    Triangulator.State := State;
    Triangulator.NewTriangleProc := NewTriangleProc;
    Triangulator.Coord := Coordinates(State).Items;
    Triangulator.CoordIndex := FdCoordIndex;
    Triangulator.VRML1MaterialBinding := BIND_DEFAULT;
    Triangulator.VRML1MaterialIndex := nil;
    Triangulator.FacesConvex := FdConvex.Value;
    MakeCoordRanges(State, @Triangulator.HandleCoordRange);
  finally FreeAndNil(Triangulator) end;
end;

{ Others --------------------------------------------------------------------- }

procedure TNodeIndexedLineSet_1.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
begin
  { No triangles }
end;

procedure TNodeIndexedLineSet_2.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
begin
  { No triangles }
end;

procedure TNodeLineSet.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
begin
  { No triangles }
end;

procedure TNodePointSet_1.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
begin
  { No triangles }
end;

procedure TNodePointSet_2.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
begin
  { No triangles }
end;

procedure Sphere_LocalTriangulate(
  Self: TVRMLGeometryNode;
  State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc;
  const FdRadiusValue: Single);

  procedure StackHigherLine(StackNum: integer; out Radius, Height: Single);
  { dla StackNum = 0..QUADRIC_STACKS (tak, ta funkcja wyjatkowo akceptuje
    StakNum = 0 bo wprawdzie warstwa o numerze 0 nie istnieje to jednak
    istnieje jej gorna linia, to jest przeciez dolny wierzcholek sfery.)
    oblicza promien i wysokosc (y) gornej obreczy warstwy StackNum. }
  var alfa, s, c: Float;
  begin
   { Policz Radius dla sfery o promieniu 1 i pomnoz przez FdRadiusValue.
     Podobnie radzimy sobie z Height. }
   { Dzielimy tutaj obrecz (poludniki) na QUADRIC_STACKS rownych
     kawalkow. To jest duzo lepszy pomysl niz dzielic os sfery na
     rowne kawalki; mozna to sprawdzic aktywujac na chwile ten stary
     kod :
       Radius := Sqrt( 1 - Sqr(1 - 2*StackNum/QUADRIC_STACKS) ) * FdRadiusValue;
       Height:=(-1 + StackNum * 2/QUADRIC_STACKS) * FdRadiusValue;
     On liczy Radius i Height dzielac os na rowne kawalki.
     Mozna zobaczyc ze wyniki sa duzo gorsze, przy czubku sfera jest bardzo
     nieproporcjonalnie gorzej triangulowana. Nawiasem mowiac
     quadrici OpenGLa tez dziela obrecz, a nie os (no i bardzo dobrze),
     sprawdzilem. }
   alfa := Pi * StackNum / QUADRIC_STACKS;
   SinCos(alfa, s, c);
   Radius := s * FdRadiusValue;
   Height := -c * FdRadiusValue;
  end;

var BottomPt, TopPt: TVector3Single;
    BottomAndTopStackRadius, BottomHeight, TopHeight :Single;

  procedure LOOP_OVER_CIRCLE_PROC(LastPt, Pt: TVector3Single);
  begin
   NewTriangleProc(Triangle3Single(LastPt, Pt, BottomPt), State, Self, 0, -1, -1);
   Pt[1] := TopHeight;
   LastPt[1] := TopHeight;
   NewTriangleProc(Triangle3Single(LastPt, Pt, TopPt), State, Self, 0, -1, -1);
  end;

var AngleRad, LowRadius, HighRadius, LowHeight, HighHeight: Single;
    HighLastPt, HighPt, LowLastPt, LowPt: TVector3Single;
    i, StackNum: integer;
LOOP_OVER_CIRCLE_DECLARE
begin
 { sfera jest zawsze triangulowana tak samo, bez wzgledu na OverTriangulate.
   Robimy sfere UV (poludniki + rownolezniki), nie IcoSphere
   (bo jakoscia sfery UV mozna latwiej (z wieksza dokladnoscia) sterowac).
   Chociaz moze kiedys zmienie zdanie ?  }

 { StackNum to numer robionego stack (w/g definicji stacks and slices z
   OpenGLowych quadricow). StackNum zmienia sie od 1 do QUADRIC_STACKS,
   w kazdej warstwie robimy QUADRIC_SLICES czworokatow (z wyjatkiem
   warstw pierwszej i ostatniej, gdzie robimy trojkaty).
   Zwracam uwage ze linii jest tak naprawde QUADRIC_STACKS+1,
   ale prawdziwych linii (takich ktore nie skladaja sie tylko z jednego
   zawsze tego samego pixla) jest QUADRIC_STACKS-1 }

 { najpierw narysuj warstwe pierwsza i ostatnia, czyli dla StackNum = 1 i
   QUADRIC_STACKS }
 BottomPt := Vector3Single(0, -FdRadiusValue, 0);
 TopPt := Vector3Single(0, FdRadiusValue, 0);
 StackHigherLine(1, BottomAndTopStackRadius, BottomHeight);
 TopHeight := -BottomHeight;

 {$define LOOP_OVER_CIRCLE_RADIUS := BottomAndTopStackRadius}
 {$define LOOP_OVER_CIRCLE_HEIGHT := BottomHeight}
 LOOP_OVER_CIRCLE

 for StackNum := 2 to QUADRIC_STACKS-1 do
 begin
  StackHigherLine(StackNum-1, LowRadius, LowHeight);
  StackHigherLine(StackNum, HighRadius, HighHeight);

  LowLastPt := Vector3Single(0, LowHeight, LowRadius);
  HighLastPt := Vector3Single(0, HighHeight, HighRadius);

  for i := 1 to QUADRIC_SLICES do
  begin
   AngleRad:=(i/QUADRIC_SLICES)*2*Pi;
   LowPt := Vector3Single(Sin(AngleRad)*LowRadius , LowHeight , Cos(AngleRad)*LowRadius );
   HighPt := Vector3Single(Sin(AngleRad)*HighRadius, HighHeight, Cos(AngleRad)*HighRadius);

   {rysuj czworokat}
   NewTriangleProc(Triangle3Single(LowLastPt, LowPt, HighPt), State, Self, 0, -1, -1);
   NewTriangleProc(Triangle3Single(LowLastPt, HighLastPt, HighPt), State, Self, 0, -1, -1);

   LowLastPt := LowPt;
   HighLastPt := HighPt;
  end;
 end;
end;

procedure TNodeSphere_1.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
begin
  Sphere_LocalTriangulate(Self, State, OverTriangulate, NewTriangleProc,
    FdRadius.Value);
end;

procedure TNodeSphere_2.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
begin
  Sphere_LocalTriangulate(Self, State, OverTriangulate, NewTriangleProc,
    FdRadius.Value);
end;

{$undef QUADRIC_SLICES}
{$undef QUADRIC_STACKS}
{$undef RECT_DIVISIONS}

{ TNodeElevationGrid --------------------------------------------------------- }

procedure TNodeElevationGrid.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
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
        NewTriangleProc(Triangle3Single(Last2, Last1, Next1), State, Self, 0, -1, -1);
        NewTriangleProc(Triangle3Single(Last2, Next1, Next2), State, Self, 0, -1, -1);

        Last1 := Next1;
        Last2 := Next2;
      end;
    end;
  end;
end;

{ TNodeExtrusion --------------------------------------------------------- }

type
  TExtrusionTriangleTriangulator = class
    Node: TNodeExtrusion;
    State: TVRMLGraphTraverseState;
    NewTriangleProc: TNewTriangleProc;
    TransformCap: TMatrix4Single;

    procedure ExtrusionCapTriangle(const Tri: TVector3Longint);
  end;

procedure TExtrusionTriangleTriangulator.ExtrusionCapTriangle(
  const Tri: TVector3Longint);
begin
  NewTriangleProc(Triangle3Single(
    MultMatrixPoint(TransformCap, Node.CrossSection3D(Tri[0])),
    MultMatrixPoint(TransformCap, Node.CrossSection3D(Tri[1])),
    MultMatrixPoint(TransformCap, Node.CrossSection3D(Tri[2]))),
    State, Node, 0, -1, -1);
end;

procedure TNodeExtrusion.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);

  procedure DrawCap(const TransformCap: TMatrix4Single);
  var
    Triangulator: TExtrusionTriangleTriangulator;
  begin
    Triangulator := TExtrusionTriangleTriangulator.Create;
    try
      Triangulator.Node := Self;
      Triangulator.State := State;
      Triangulator.NewTriangleProc := NewTriangleProc;
      Triangulator.TransformCap := TransformCap;

      if FdConvex.Value then
      begin
        TriangulateConvexFace(FdCrossSection.Count,
          @Triangulator.ExtrusionCapTriangle, 0);
      end else
      begin
        TriangulateFace(nil, FdCrossSection.Count,
          @CrossSection3D, @Triangulator.ExtrusionCapTriangle, 0);
      end;
    finally FreeAndNil(Triangulator) end;
  end;

type
  T01 = 0..1;
var
  { We have separate pointers and data for matrix, to speed up
    Transform[0] := Transform[1] operation. It's much faster to
    copy a pointer than 16 Single floats. }
  TransformData: array [T01] of TMatrix4Single;
  Transform: array [T01] of PMatrix4Single;

  procedure MakeCrossPoint(out Point: TVector3Single; WhichTransform: T01;
    CrossIndex: Integer);
  var
    Point2d: PVector2Single;
  begin
    Point2d := @FdCrossSection.Items.Items[CrossIndex];
    Point := MultMatrixPoint(Transform[WhichTransform]^,
      Vector3Single(Point2d^[0], 0, Point2d^[1]));
  end;

var
  I, J: Integer;
  E: TVRMLExtrusion;
  LastY, LastZ: TVector3Single;
  Cross: array [T01, T01] of TVector3Single;
  TransformCap: TMatrix4Single;
begin
  if FdCrossSection.Count = 0 then Exit;

  Transform[0] := @TransformData[0];
  Transform[1] := @TransformData[1];

  E := TVRMLExtrusion.Create;
  try
    { This is just to silence FPC 2.2.0 warnings about Cross not initialized. }
    Cross[0, 0][0] := 0.0;

    E.Node := Self;
    if E.High > 0 then
    begin
      E.SpineTransformTo1st(0, LastY, LastZ, Transform[0]^);
      for I := 1 to E.High do
      begin
        E.SpineTransformTo1st(I, LastY, LastZ, Transform[1]^);

        MakeCrossPoint(Cross[0, 0], 0, 0);
        MakeCrossPoint(Cross[1, 0], 1, 0);

        for J := 1 to FdCrossSection.Count - 1 do
        begin
          MakeCrossPoint(Cross[0, 1], 0, J);
          MakeCrossPoint(Cross[1, 1], 1, J);

          { Now make a quad using 4 Cross points }
          NewTriangleProc(Triangle3Single(Cross[0, 0], Cross[1, 0], Cross[1, 1]), State, Self, 0, -1, -1);
          NewTriangleProc(Triangle3Single(Cross[0, 0], Cross[1, 1], Cross[0, 1]), State, Self, 0, -1, -1);

          Cross[0, 0] := Cross[0, 1];
          Cross[1, 0] := Cross[1, 1];
        end;

        { I don't care about preserving current Transform[0]^.
          But current Transform[1]^ must become Transform[0]^ in the
          next loop roll. So I exchange pointers. }
        SwapValues(Transform[0], Transform[1]);
      end;
    end;

    if FdBeginCap.Value or
       FdEndCap.Value then
    begin
      if FdBeginCap.Value then
      begin
        { calculate TransformCap.
          Note that LastY, LastZ are just ignored here (they are "out only"
          for Spine = 0).  }
        E.SpineTransformTo1st(0, LastY, LastZ, TransformCap);

        DrawCap(TransformCap);
      end;

      if FdEndCap.Value then
      begin
        { calculate TriangleProcData.TransformEnd. }
        if E.High > 0 then
          { We already have it in Transform[0]^.
            This is most fortunate, otherwise calculating last transform
            is not easy (we have to iterate over whole spine for correct
            LastY, LastZ values). }
          TransformCap := Transform[0]^ else
          { Then this can be calculated just like TransformBegin
            (or copied from there, if available) }
        if not FdBeginCap.Value then
          E.SpineTransformTo1st(0, LastY, LastZ, TransformCap);
          { else calculations for FdBeginCap.Value already set TransformCap }

        DrawCap(TransformCap);
      end;
    end;

  finally FreeAndNil(E) end;
end;

{ TCoordinateTriangulator ---------------------------------------------------- }

type
  { Base class for triangulating VRML nodes using "coord" field.
    Somewhat analogous to TAbstractCoordinateRenderer, except
    TAbstractCoordinateRenderer does OpenGL rendering and this is for
    OpenGL-independent triangulating. }
  TCoordinateTriangulator = class
    Geometry: TVRMLGeometryNode;
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
    NewTriangleProc(Triangle, State, Geometry, VRML1MaterialNumber, -1, -1);
    I := 3;
    while I <= High(Indexes) do
    begin
      Triangle[1] := Triangle[2];
      Triangle[2] := Coord.Items[Indexes[Index(I)]];
      NewTriangleProc(Triangle, State, Geometry, VRML1MaterialNumber, -1, -1);
      Inc(I);
    end;
  end;
end;

{ Implements LocalTriangulate method for coordinate-based nodes,
  using node's CoordPolygons method. }
procedure LocalTriangulateCoordPolygons(
  Geometry: TVRMLGeometryNode;
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
    Triangulator.Geometry := Geometry;
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

procedure TNodeIndexedTriangleMesh_1.LocalTriangulate(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  NewTriangleProc: TNewTriangleProc);
begin
  LocalTriangulateCoordPolygons(Self, State,
    State.LastNodes.MaterialBinding.FdValue.Value, FdMaterialIndex,
    NewTriangleProc);
end;

procedure TNodeIndexedTriangleSet.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
begin
  LocalTriangulateCoordPolygons(Self, State,
    BIND_DEFAULT, nil, NewTriangleProc);
end;

procedure TNodeTriangleSet.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
begin
  LocalTriangulateCoordPolygons(Self, State,
    BIND_DEFAULT, nil, NewTriangleProc);
end;

procedure TNodeIndexedTriangleFanSet.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
begin
  LocalTriangulateCoordPolygons(Self, State,
    BIND_DEFAULT, nil, NewTriangleProc);
end;

procedure TNodeTriangleFanSet.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
begin
  LocalTriangulateCoordPolygons(Self, State,
    BIND_DEFAULT, nil, NewTriangleProc);
end;

procedure TNodeIndexedTriangleStripSet.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
begin
  LocalTriangulateCoordPolygons(Self, State,
    BIND_DEFAULT, nil, NewTriangleProc);
end;

procedure TNodeTriangleStripSet.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
begin
  LocalTriangulateCoordPolygons(Self, State,
    BIND_DEFAULT, nil, NewTriangleProc);
end;

procedure TNodeIndexedQuadSet.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
begin
  LocalTriangulateCoordPolygons(Self, State,
    BIND_DEFAULT, nil, NewTriangleProc);
end;

procedure TNodeQuadSet.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
begin
  LocalTriangulateCoordPolygons(Self, State,
    BIND_DEFAULT, nil, NewTriangleProc);
end;

{ ---------------------------------------------------------------------------- }

procedure TNodeRectangle2D.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
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
  NewTriangleProc(Triangle, State, Self, 0, -1, -1);

  Triangle[1] := Triangle[2];
  V[1] := -V[1];
  Triangle[2] := V;
  NewTriangleProc(Triangle, State, Self, 0, -1, -1);
end;

procedure TNodeCircle2D.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
begin
  { No triangles }
end;