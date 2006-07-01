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
    procedure LocalNewTriangle(const Tri: TTriangle3Single; State: TVRMLGraphTraverseState;
      ShapeNode: TNodeGeneralShape; MatNum: integer);
  end;
  procedure TTriangulateRedirect.LocalNewTriangle(const Tri: TTriangle3Single;
    State: TVRMLGraphTraverseState; ShapeNode: TNodeGeneralShape; MatNum: integer);
  var i: integer;
      NewT: TTriangle3Single;
  begin
   for i := 0 to 2 do NewT[i] := MultMatrixPoint(Transform^, Tri[i]);
   NewTriangle(NewT, State, ShapeNode, MatNum);
  end;

procedure TNodeGeneralShape.Triangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
var tr: TTriangulateRedirect;
begin
 { klasa TTriangulateRedirect bedzie dzialala jako posrednik :
   kazdy trojkat ktory zwroci LocalTriangulate bedzie transformowany przez
   State.CurrMatrix i dopiero wtedy przekazywany do NewTriangleProc.
   W ten sposob za pomoca LocalTriangulate implementujemy Triangulate }
 tr := TTriangulateRedirect.Create;
 try
  tr.Transform := @(State.CurrMatrix);
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
 NewTriangleProc(T, State, ShapeNode, MatNum);
 TriAssign(0, x1, y1);
 TriAssign(1, x2, y2);
 TriAssign(2, x2, y1);
 NewTriangleProc(T, State, ShapeNode, MatNum);
end;

{ metody LocalTriangulate konkretnych node'ow ---------------------------------}

procedure TNodeAsciiText.LocalTriangulate(State: TVRMLGraphTraverseState;
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

procedure TNodeCone.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
var BottomPt, TopPt: TVector3Single;
    MatBottom, MatSides: integer;

  procedure LOOP_OVER_CIRCLE_PROC(const LastPt, Pt: TVector3Single);
  begin
   if FdParts.Flags[CONE_PARTS_BOTTOM] then
    NewTriangleProc(Triangle3Single(LastPt, Pt, BottomPt), State, Self, MatBottom);
   if FdParts.Flags[CONE_PARTS_SIDES] then
    NewTriangleProc(Triangle3Single(LastPt, Pt, TopPt), State, Self, MatSides);
  end;

LOOP_OVER_CIRCLE_DECLARE
begin
 Assert(not OverTriangulate); { TODO: OverTriangulate not used here so not impl yet }

 if State.LastNodes.MaterialBinding.FdValue.Value in
   [BIND_PER_PART, BIND_PER_PART_INDEXED] then
  begin MatSides := 0; MatBottom := 1 end else
  begin MatSides := 0; MatBottom := 0 end;

 if FdParts.Flags[CONE_PARTS_SIDES] or FdParts.Flags[CONE_PARTS_BOTTOM] then
 begin
  if FdParts.Flags[CONE_PARTS_BOTTOM] then
   BottomPt := Vector3Single(0, -FdHeight.Value/2, 0);
  if FdParts.Flags[CONE_PARTS_SIDES] then
   TopPt := Vector3Single(0, FdHeight.Value/2, 0);

  {$define LOOP_OVER_CIRCLE_RADIUS:=(FdBottomRadius.Value)}
  {$define LOOP_OVER_CIRCLE_HEIGHT:=(-FdHeight.Value/2)}
  LOOP_OVER_CIRCLE
 end;
end;

procedure TNodeCube.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
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
 Box[0] := Vector3Single(-FdWidth.Value/2, -FdHeight.Value/2, -FdDepth.Value/2);
 Box[1] := Vector3Single( FdWidth.Value/2, FdHeight.Value/2, FdDepth.Value/2);
 LocalTriangulateBox3d(box, State, Self, OverTriangulate, NewTriangleProc);
end;

procedure TNodeCylinder.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
var BottomPt, TopPt: TVector3Single;
    MatSides, MatTop, MatBottom: integer;

  procedure LOOP_OVER_CIRCLE_PROC(const LowLastPt, LowPt: TVector3Single);
  var HighLastPt, HighPt: TVector3Single;
  begin
   HighLastPt := Vector3Single(LowLastPt[0], FdHeight.Value/2, LowLastPt[2]);
   HighPt    :=Vector3Single(LowPt    [0], FdHeight.Value/2, LowPt    [2]);

   if FdParts.Flags[CYLINDER_PARTS_BOTTOM] then
    NewTriangleProc(Triangle3Single(LowLastPt, LowPt, BottomPt), State, Self, MatBottom);

   if FdParts.Flags[CYLINDER_PARTS_TOP] then
    NewTriangleProc(Triangle3Single(HighLastPt, HighPt, TopPt), State, Self, MatTop);

   if FdParts.Flags[CYLINDER_PARTS_SIDES] then
   begin
    NewTriangleProc(Triangle3Single(LowLastPt, LowPt, HighPt), State, Self, MatSides);
    NewTriangleProc(Triangle3Single(HighLastPt, HighPt, LowLastPt), State, Self, MatSides);
   end;
  end;

LOOP_OVER_CIRCLE_DECLARE
begin
 Assert(not OverTriangulate); { TODO: OverTriangulate not used here so not impl yet }

 if State.LastNodes.MaterialBinding.FdValue.Value in [BIND_PER_PART, BIND_PER_PART_INDEXED] then
  begin MatSides := 0; MatTop := 1; MatBottom := 2 end else
  begin MatSides := 0; MatTop := 0; MatBottom := 0 end;

 if not FdParts.AreAllFlags(false) then
 begin
  if FdParts.Flags[CYLINDER_PARTS_TOP] then
   TopPt := Vector3Single(0, FdHeight.Value/2, 0);
  if FdParts.Flags[CYLINDER_PARTS_BOTTOM] then
   BottomPt := Vector3Single(0, -FdHeight.Value/2, 0);

  {$define LOOP_OVER_CIRCLE_RADIUS:=(FdRadius.Value)}
  {$define LOOP_OVER_CIRCLE_HEIGHT:=(-FdHeight.Value/2)}
  LOOP_OVER_CIRCLE
 end;
end;

type
  TNewTriData = record
    State: TVRMLGraphTraverseState;
    ShapeNode: TNodeGeneralShape;
    MatNum: integer;
    Vertices: PArray_Vector3Single;
    FaceIndices: PArray_Longint;
    NewTriangleProc: TNewTriangleProc;
  end;
  PNewTriData = ^TNewTriData;

procedure NewTriangleOfFaceSet(const TriIndices: TVector3Longint; Data: Pointer);
var Tri: TTriangle3Single;
begin
 with PNewTriData(Data)^ do
 begin
  Tri[0] := Vertices^[FaceIndices^[TriIndices[0]]];
  Tri[1] := Vertices^[FaceIndices^[TriIndices[1]]];
  Tri[2] := Vertices^[FaceIndices^[TriIndices[2]]];
  NewTriangleProc(Tri, State, ShapeNode, MatNum);
 end;
end;

procedure TNodeIndexed_Faces_Or_Triangles.LocalTriangulate(
  State: TVRMLGraphTraverseState; OverTriangulate: boolean;
  NewTriangleProc: TNewTriangleProc);

  procedure Triangulate_Face(startIndex, endIndex, faceMatNum: integer);
  var NewTriData: TNewTriData;
  begin
   { roznica miedzy implementacja tego w IndexedFaceSet a IndexedTriangleMesh
     jest tak subtelna ze zdecydowalem sie zapisac obie implementacje w ponizszym
     kodzie. }
   NewTriData.State := State;
   NewTriData.ShapeNode := Self;
   NewTriData.MatNum := faceMatNum;
   NewTriData.Vertices := State.LastNodes.Coordinate3.FdPoint.Items.ItemsArray;
   newTriData.FaceIndices := PArray_Longint(FdCoordIndex.Items.Pointers[StartIndex]);
   NewTriData.NewTriangleProc := NewTriangleProc;

   if Self is TNodeIndexedFaceSet then
   begin
    if State.LastNodes.ShapeHints.FdFaceType.Value = FACETYPE_UNKNOWN then
     TriangulateFace(NewTriData.FaceIndices, EndIndex-StartIndex,
       NewTriData.Vertices, @NewTriangleOfFaceSet, @NewTriData, 0) else
     TriangulateConvexFace(EndIndex-StartIndex, @NewTriangleOfFaceSet, @NewTriData, 0);
   end else
   if Self is TNodeIndexedTriangleMesh then
   begin
    TriangulateTriangleStrip(EndIndex-StartIndex, @NewTriangleOfFaceSet, @NewTriData, 0);
   end else
    EInternalError.Create('undefined in TriangulateFace');
  end;

var startIndex, endIndex, faceNumber, faceMatNum: integer;
begin
 startIndex := 0;
 faceNumber := 0;
 while startIndex < FdCoordIndex.Count do
 begin
  endIndex := startIndex;
  while (endIndex < FdCoordIndex.Count) and
        (FdCoordIndex.Items.Items[endIndex] <> -1) do
   Inc(endIndex);

  case State.LastNodes.MaterialBinding.FdValue.Value of
   BIND_DEFAULT, BIND_OVERALL: faceMatNum := 0;
   BIND_PER_PART, BIND_PER_FACE: faceMatNum := faceNumber;
   BIND_PER_PART_INDEXED, BIND_PER_FACE_INDEXED: faceMatNum := FdMaterialIndex.Items.Items[faceNumber];
   { TODO: we don't support here materials per vertex or per vertex indexed }
   else faceMatNum := 0;
  end;
  Triangulate_Face(startIndex, endIndex, faceMatNum);
  Inc(faceNumber);
  startIndex := endIndex+1;
 end;
end;

procedure TNodeIndexedLineSet.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
begin
end;

procedure TNodePointSet.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);
begin
end;

procedure TNodeSphere.LocalTriangulate(State: TVRMLGraphTraverseState;
  OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc);

  procedure StackHigherLine(StackNum: integer; out Radius, Height: Single);
  { dla StackNum = 0..QUADRIC_STACKS (tak, ta funkcja wyjatkowo akceptuje
    StakNum = 0 bo wprawdzie warstwa o numerze 0 nie istnieje to jednak
    istnieje jej gorna linia, to jest przeciez dolny wierzcholek sfery.)
    oblicza promien i wysokosc (y) gornej obreczy warstwy StackNum. }
  var alfa, s, c: Float;
  begin
   { Policz Radius dla sfery o promieniu 1 i pomnoz przez FdRadius.Value.
     Podobnie radzimy sobie z Height. }
   { Dzielimy tutaj obrecz (poludniki) na QUADRIC_STACKS rownych
     kawalkow. To jest duzo lepszy pomysl niz dzielic os sfery na
     rowne kawalki; mozna to sprawdzic aktywujac na chwile ten stary
     kod :
       Radius := Sqrt( 1 - Sqr(1 - 2*StackNum/QUADRIC_STACKS) ) * FdRadius.Value;
       Height:=(-1 + StackNum * 2/QUADRIC_STACKS) * FdRadius.Value;
     On liczy Radius i Height dzielac os na rowne kawalki.
     Mozna zobaczyc ze wyniki sa duzo gorsze, przy czubku sfera jest bardzo
     nieproporcjonalnie gorzej triangulowana. Nawiasem mowiac
     quadrici OpenGLa tez dziela obrecz, a nie os (no i bardzo dobrze),
     sprawdzilem. }
   alfa := Pi * StackNum / QUADRIC_STACKS;
   SinCos(alfa, s, c);
   Radius := s * FdRadius.Value;
   Height := -c * FdRadius.Value;
  end;

var BottomPt, TopPt: TVector3Single;
    BottomAndTopStackRadius, BottomHeight, TopHeight :Single;

  procedure LOOP_OVER_CIRCLE_PROC(LastPt, Pt: TVector3Single);
  begin
   NewTriangleProc(Triangle3Single(LastPt, Pt, BottomPt), State, Self, 0);
   Pt[1] := TopHeight;
   LastPt[1] := TopHeight;
   NewTriangleProc(Triangle3Single(LastPt, Pt, TopPt), State, Self, 0);
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
 BottomPt := Vector3Single(0, -FdRadius.Value, 0);
 TopPt := Vector3Single(0, FdRadius.Value, 0);
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
   NewTriangleProc(Triangle3Single(LowLastPt, LowPt, HighPt), State, Self, 0);
   NewTriangleProc(Triangle3Single(LowLastPt, HighLastPt, HighPt), State, Self, 0);

   LowLastPt := LowPt;
   HighLastPt := HighPt;
  end;
 end;
end;

{$undef QUADRIC_SLICES}
{$undef QUADRIC_STACKS}
{$undef RECT_DIVISIONS}

{ eof -------------------------------------------------------------------------}
