{
  Copyright 2003-2006 Michalis Kamburelis.

  This file is part of "Kambi's 3dmodels Pascal units".

  "Kambi's 3dmodels Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's 3dmodels Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's 3dmodels Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ TNodeGeneralShape.Triangulate ktore dziala wywolujac LocalTriangulate
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
      ShapeNode: TNodeGeneralShape;
      const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
  end;

  procedure TTriangulateRedirect.LocalNewTriangle(const Tri: TTriangle3Single;
    State: TVRMLGraphTraverseState; ShapeNode: TNodeGeneralShape;
    const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
  var i: integer;
      NewT: TTriangle3Single;
  begin
   for i := 0 to 2 do NewT[i] := MultMatrixPoint(Transform^, Tri[i]);
   NewTriangle(NewT, State, ShapeNode, MatNum,
     FaceCoordIndexBegin, FaceCoordIndexEnd);
  end;

procedure TNodeGeneralShape.Triangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
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
  State: TVRMLGraphTraverseState; ShapeNode: TNodeGeneralShape;
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
 NewTriangleProc(T, State, ShapeNode, MatNum, -1, -1);
 TriAssign(0, x1, y1);
 TriAssign(1, x2, y2);
 TriAssign(2, x2, y1);
 NewTriangleProc(T, State, ShapeNode, MatNum, -1, -1);
end;

{ metody LocalTriangulate konkretnych node'ow ---------------------------------}

procedure TNodeAsciiText_1.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
begin
 { TODO: triangulate }
end;

procedure TNodeText.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
begin
 { TODO: triangulate }
end;

procedure TNodeText3D.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
begin
 { TODO: triangulate }
end;

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
  Self: TNodeGeneralShape;
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
  Self: TNodeGeneralShape;
  State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc;
  const SizeX, SizeY, SizeZ: Single);
var MatPerPart: boolean;

  procedure LocalTriangulateBox3d(const Box: TBox3d;
    State: TVRMLGraphTraverseState; ShapeNode: TNodeGeneralShape;
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
      State, ShapeNode, OverTriangulate, NewTriangleProc, MatNum);
    if MatPerPart then MatNum := MaterialsPerPart[i, true] else MatNum := 0;
    LocalTriangulateRect(i, Box[1, i], Box[0, c1], Box[0, c2], Box[1, c1], Box[1, c2],
      State, ShapeNode, OverTriangulate, NewTriangleProc, MatNum);
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
  Self: TNodeGeneralShape;
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

type
  TNewTriData = record
    State: TVRMLGraphTraverseState;
    ShapeNode: TNodeGeneralShape;
    MatNum: integer;
    Vertices: PArray_Vector3Single;
    FaceIndices: PArray_Longint;
    NewTriangleProc: TNewTriangleProc;
    FaceCoordIndexBegin, FaceCoordIndexEnd: Integer;
  end;
  PNewTriData = ^TNewTriData;

procedure NewTriangleOfFaceSet(const TriIndices: TVector3Longint; Data: Pointer);
var
  Tri: TTriangle3Single;
begin
  with PNewTriData(Data)^ do
  begin
    Tri[0] := Vertices^[FaceIndices^[TriIndices[0]]];
    Tri[1] := Vertices^[FaceIndices^[TriIndices[1]]];
    Tri[2] := Vertices^[FaceIndices^[TriIndices[2]]];
    NewTriangleProc(Tri, State, ShapeNode, MatNum,
      FaceCoordIndexBegin, FaceCoordIndexEnd);
  end;
end;

{ For IndexedTriangleMesh pass TriangleMesh = @true,
  FacesConvex doesn't matter in this case.

  MaterialBinding should be one of BIND_xxx constants,
  MaterialIndex may be nil if MaterialBinding is not
  anything _INDEXED. }
procedure IndexedPolygons_LocalTriangulate(
  Self: TNodeGeneralShape;
  State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  NewTriangleProc: TNewTriangleProc;
  Coord: TMFVec3f; CoordIndex: TMFLong;
  TriangleMesh, FacesConvex: boolean;
  MaterialBinding: Integer;
  MaterialIndex: TMFLong);

  procedure Triangulate_Face(startIndex, endIndex, faceMatNum: integer);
  var
    NewTriData: TNewTriData;
  begin
    NewTriData.State := State;
    NewTriData.ShapeNode := Self;
    NewTriData.MatNum := faceMatNum;
    NewTriData.Vertices := Coord.Items.ItemsArray;
    newTriData.FaceIndices := PArray_Longint(CoordIndex.Items.Pointers[StartIndex]);
    NewTriData.NewTriangleProc := NewTriangleProc;
    NewTriData.FaceCoordIndexBegin := startIndex;
    NewTriData.FaceCoordIndexEnd := endIndex;

    if not TriangleMesh then
    begin
      if FacesConvex then
        TriangulateConvexFace(EndIndex-StartIndex,
          @NewTriangleOfFaceSet, @NewTriData, 0) else
        TriangulateFace(NewTriData.FaceIndices, EndIndex-StartIndex,
          NewTriData.Vertices, @NewTriangleOfFaceSet, @NewTriData, 0);
    end else
    begin
      TriangulateTriangleStrip(EndIndex-StartIndex,
        @NewTriangleOfFaceSet, @NewTriData, 0);
    end;
  end;

var
  startIndex, endIndex, faceNumber, faceMatNum: integer;
begin
  startIndex := 0;
  faceNumber := 0;
  while startIndex < CoordIndex.Count do
  begin
    endIndex := startIndex;
    while (endIndex < CoordIndex.Count) and
          (CoordIndex.Items.Items[endIndex] <> -1) do
      Inc(endIndex);

    case MaterialBinding of
      BIND_DEFAULT, BIND_OVERALL: faceMatNum := 0;
      BIND_PER_PART, BIND_PER_FACE: faceMatNum := faceNumber;
      BIND_PER_PART_INDEXED, BIND_PER_FACE_INDEXED:
        faceMatNum := MaterialIndex.Items.Items[faceNumber];
      { TODO: we don't support here materials per vertex or per vertex indexed }
      else faceMatNum := 0;
    end;
    Triangulate_Face(startIndex, endIndex, faceMatNum);
    Inc(faceNumber);
    startIndex := endIndex+1;
  end;
end;

procedure TNodeIndexedFaceSet_1.LocalTriangulate(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  NewTriangleProc: TNewTriangleProc);
begin
  IndexedPolygons_LocalTriangulate(Self,
    State, OverTriangulate, NewTriangleProc,
    State.LastNodes.Coordinate3.FdPoint, FdCoordIndex,
    false,
    State.LastNodes.ShapeHints.FdFaceType.Value = FACETYPE_CONVEX,
    State.LastNodes.MaterialBinding.FdValue.Value,
    FdMaterialIndex);
end;

procedure TNodeIndexedTriangleMesh_1.LocalTriangulate(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  NewTriangleProc: TNewTriangleProc);
begin
  IndexedPolygons_LocalTriangulate(Self,
    State, OverTriangulate, NewTriangleProc,
    State.LastNodes.Coordinate3.FdPoint, FdCoordIndex,
    true,
    State.LastNodes.ShapeHints.FdFaceType.Value = FACETYPE_CONVEX,
    State.LastNodes.MaterialBinding.FdValue.Value,
    FdMaterialIndex);
end;

procedure TNodeIndexedFaceSet_2.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
begin
  if (FdCoord.Value <> nil) and (FdCoord.Value is TNodeCoordinate) then
    IndexedPolygons_LocalTriangulate(Self,
      State, OverTriangulate, NewTriangleProc,
      TNodeCoordinate(FdCoord.Value).FdPoint, FdCoordIndex,
      false,
      FdConvex.Value,
      BIND_DEFAULT { For VRML 2.0 we will ignore material number anyway },
      nil);
end;

procedure TNodeIndexedLineSet_1.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
begin
end;

procedure TNodeIndexedLineSet_2.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
begin
end;

procedure TNodePointSet_1.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
begin
end;

procedure TNodePointSet_2.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
begin
end;

procedure Sphere_LocalTriangulate(
  Self: TNodeGeneralShape;
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