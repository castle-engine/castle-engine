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

{ @abstract(Triangulating (decomposing possibly non-convex
  polygons into triangles).) }

unit Triangulator;

interface

uses SysUtils, VectorMath, KambiUtils;

type
  TTriangulatorProc = procedure(const Tri: TVector3Longint; Data: Pointer);

{ Triangulate potentially non-convex face.

  FaceIndices[0]..FaceIndices[FaceIndicesCount-1] are indices
  to the Vertices array. They describe the outline of the polygon (face).
  You can pass FaceIndices = @nil, this is understood that indices
  are just 0..FaceIndicesCount-1 (in other words, it's equivalent
  to setting FaceIndices[0] = 0, FaceIndices[1] = 1 etc.).

  For each resulting triangle we will call TriangulatorProc
  with Tri (first param of TriangulatorProc) containing indices
  to FaceIndices[] array. We return indices to FaceIndices
  (not ready vectors from Vertices, not even indices to Vertices[] array)
  to allow the caller to obtain all information about the triangle.
  In the simple case, you can just use Vertices[FaceIndices[Tri[0..2]]]
  to obtain your triangle, in the more sophisticated cases you have
  other options to e.g. extract other vertex information from whatever
  data you have (see e.g. VRML IndexedFaceSet renderer).

  Generated triangles have the same orientation (normal from ccw etc.)
  as original polygon. This also means that if you're sure that
  your polygon is planar (and it should be --- although we handle gracefully
  small deviations from planar, this procedure doesn't actually handle
  arbitrary (dis)located 3D data) then normal vector of all your
  triangles is the same, and is equal to normal vector of original polygon
  (calculated e.g. by IndexedPolygonNormal).

  Note that you generally shouldn't use this procedure if you @italic(know)
  that your polygon is convex. Then using this is a waste of time,
  after all convex polygons can be triangulated much easier.
  You can use TriangulateConvexFace in this case, which has
  deliberately very similar interface to this procedure.

  @param(TriangulatorProcData Is just passed unmodified to every
    TriangulatorProc call (as the second parameter). This is standard
    method to pass whatever data to your callback.)

  @param(AddToIndices
    Indexes returned in Tri[0..2] are all incremented by AddToIndices
    (which may also be negative), this is useful if your FaceIndices
    is actually a pointer to the middle of some larger indexes array.
    Just pass 0 if you don't want this.)

  @seeAlso TriangulateConvexFace

  @groupBegin }
procedure TriangulateFace(
  FaceIndices: PArray_Longint; FaceIndicesCount: integer;
  Vertices: PArray_Vector3Single; TriangulatorProc: TTriangulatorProc;
  TriangulatorProcData: Pointer; AddToIndices: Longint); overload;

procedure TriangulateFace(
  FaceIndices: PArray_Longint; FaceIndicesCount: integer;
  Vertices: TGetVertexFromIndexFunc; TriangulatorProc: TTriangulatorProc;
  TriangulatorProcData: Pointer; AddToIndices: Longint); overload;
{ @groupEnd }

{ Triangulate convex polygon or triangle strip.

  These perform very easy triangulation. They have deliberately
  similar interface to TriangulateFace, so they can be used as drop-in
  replacements for TriangulateFace, when you know that your face is
  convex or you're dealing with triangle strip.

  Note that they don't even need to know FaceIndices or Vertices,
  it's enough to know FaceIndicesCount.

  They also guarantee consequent triangles orientation, like TriangulateFace.
  In case of triangle strip orientation of the first three vertices
  matters, just like for OpenGL triangle strip primitive.

  @seeAlso TriangulateFace

  @groupBegin }
procedure TriangulateConvexFace(FaceIndicesCount: integer;
  TriangulatorProc: TTriangulatorProc; TriangulatorProcData: Pointer;
  AddToIndices: Longint);

procedure TriangulateTriangleStrip(IndicesCount: integer;
  TriangulatorProc: TTriangulatorProc; TriangulatorProcData: Pointer;
  AddToIndices: Longint);
{ @groupEnd }

implementation

{$define DEFINE_NEW_TRIANGLE_PROC :=
procedure NewTriangle(const p0, p1, p2: Longint);
begin
 TriangulatorProc(Vector3Longint(
   p0+AddToIndices,
   p1+AddToIndices,
   p2+AddToIndices), TriangulatorProcData);
end;}

{ TriangulateFace ------------------------------------------------------------ }

{ TriangulateFace non-convex napisane na podstawie face2tri.C w C++ ze
  zrodel w mgflib. Przepisalem na Pascala, dostosowalem do wlasnych parametrow,
  skrocilem zapis w wielu miejscach, ale ciagle zasadniczy algorytm nie ulegl
  zadnym zmianom.

  Chwilowo nie zglebilem zupelnie do konca idei "jak i dlaczego to dziala".
  Postaram sie zmienic ten fakt jak najszybciej i wtedy znikna ponizsze
  "TODO". }

procedure TriangulateFace(
  FaceIndices: PArray_Longint; FaceIndicesCount: integer;
  Vertices: TGetVertexFromIndexFunc; TriangulatorProc: TTriangulatorProc;
  TriangulatorProcData: Pointer; AddToIndices: Longint);

  DEFINE_NEW_TRIANGLE_PROC

  {$define VertsCount := FaceIndicesCount}
  function Verts(i: Longint): TVector3Single;
  begin
    if FaceIndices <> nil then
      Result := Vertices(FaceIndices^[i]) else
      Result := Vertices(I);
  end;

var ConvexNormal, Center, nn, E1, E2, E3: TVector3Single;
    Corners, Start, MaxLenIndex, i, p0, p1, p2: Longint;
    d, MaxLen: Single;
    Outs: TDynBooleanArray;
    Empty: boolean;

  function NextNotOut(Index: Integer): Integer;
  begin
   result := Index;
   repeat result := (result+1) mod VertsCount until not Outs.Items[result];
  end;

begin
 { najpierw odrzuc przypadek gdy VertsCount < 3 i zrob trywialny przypadek
   VertsCount = 3 }
 if VertsCount = 3 then
  NewTriangle(0, 1, 2) else
 if VertsCount > 3 then
 begin
  { wyznacz Center jako prosta srednia z wszystkich punktow }
  Center := ZeroVector3Single;
  for i := 0 to VertsCount-1 do VectorAddTo1st(Center, Verts(i));
  VectorScaleTo1st(Center, 1/VertsCount);

  { wyznacz punkt sposrod Verts[] najbardziej odlegly od Center.
    MaxLen to jego odleglosc od Center, MaxLenIndex to jego index w Verts[].
    TODO - czy tu PointDistanceSqr nie wystarczy ? }
  MaxLenIndex := 0;
  MaxLen := PointsDistance(Center, Verts(0));
  for i := 1 to VertsCount-1 do
  begin
   d := PointsDistance(Center, Verts(i));
   if d > MaxLen then
   begin
    MaxLen := d;
    MaxLenIndex := i;
   end;
  end;

  { p1 to indeks najdalszego sposrod Verts, p0 to poprzedni, p2 to nastepny }
  p1 := MaxLenIndex;
  if p1 = 0 then p0 := VertsCount-1 else p0 := p1-1;
  p2 := (p1+1) mod VertsCount;

  { TODO - czy tu negate potrzebne ? }
  ConvexNormal := VectorNegate( TriangleNormal(Verts(p0), Verts(p1), Verts(p2)) );

  Corners := VertsCount;
  p0 := -1;

  Outs := TDynBooleanArray.Create(VertsCount);
  try
   Outs.SetAll(false);

   while Corners >= 3 do
   begin
    Start := p0;

    repeat
     p0 := NextNotOut(p0);
     p1 := NextNotOut(p0);
     p2 := NextNotOut(p1);

     if p0 = Start then break;

     { TODO - czy tu negate potrzebne ? }
     nn := VectorNegate( TriangleNormal(Verts(p0), Verts(p1), Verts(p2)) );
     d := PointsDistance(nn, ConvexNormal);

     E1 := VectorProduct(nn, VectorSubtract(Verts(p1), Verts(p0)));
     E2 := VectorProduct(nn, VectorSubtract(Verts(p2), Verts(p1)));
     E3 := VectorProduct(nn, VectorSubtract(Verts(p0), Verts(p2)));

     Empty := True;

     for i := 0 to VertsCount-1 do
      if (not Outs.Items[i]) and (i <> p0) and (i <> p1) and (i <> p2) then
      begin
       Empty := Empty and not (
         (VectorDotProduct(E1, VectorSubtract(Verts(i), Verts(p0))) <= - SingleEqualityEpsilon) and
         (VectorDotProduct(E2, VectorSubtract(Verts(i), Verts(p1))) <= - SingleEqualityEpsilon) and
         (VectorDotProduct(E3, VectorSubtract(Verts(i), Verts(p2))) <= - SingleEqualityEpsilon)
         );
      end;
    until (d <= 1.0) and Empty;

{ TODO - w graz.mgf.wrl jest ten blad i mimo to wszystko dziala ok
  gdy zakomentarzowalem ponizszy check ?
    if p0 = Start then raise Exception.Create('misbuilt polygonal face');}

    NewTriangle(p0, p1, p2);

    Outs.Items[p1] := True;
    Dec(Corners);
   end;
  finally Outs.Free end;
 end;
end;

type
  TVerticesGenerator = class
    Vertices: PArray_Vector3Single;
    function Generate(Index: integer): TVector3Single;
  end;

function TVerticesGenerator.Generate(Index: integer): TVector3Single;
begin
  Result := Vertices^[Index];
end;

procedure TriangulateFace(
  FaceIndices: PArray_Longint; FaceIndicesCount: integer;
  Vertices: PArray_Vector3Single; TriangulatorProc: TTriangulatorProc;
  TriangulatorProcData: Pointer; AddToIndices: Longint);
var
  G: TVerticesGenerator;
begin
  G := TVerticesGenerator.Create;
  try
    G.Vertices := Vertices;
    TriangulateFace(FaceIndices, FaceIndicesCount,
      @G.Generate, TriangulatorProc, TriangulatorProcData, AddToIndices);
  finally FreeAndNil(G); end;
end;

{ proste Triangulate ---------------------------------------------------------- }

procedure TriangulateConvexFace(FaceIndicesCount: integer;
  TriangulatorProc: TTriangulatorProc; TriangulatorProcData: Pointer;
  AddToIndices: Longint);
  DEFINE_NEW_TRIANGLE_PROC
var
  I: Integer;
begin
  for I := 0 to FaceIndicesCount - 3 do
    NewTriangle(0, I + 1, I + 2);
end;

procedure TriangulateTriangleStrip(IndicesCount: integer;
  TriangulatorProc: TTriangulatorProc; TriangulatorProcData: Pointer;
  AddToIndices: Longint);
  DEFINE_NEW_TRIANGLE_PROC
var i: integer;
begin
 for i := 0 to IndicesCount-3 do
 begin
  { musimy odwracac kolejnosc pierwszych dwoch vertexow trojkata
    zeby wszystkie trojkaty w TriangleStrip mialy konsekwentna orientacje.
    (podobnie jak OpenGL odwraca TRIANGLE_STRIP) }
  if Odd(i) then
   NewTriangle(i+1, i  , i+2) else
   NewTriangle(i  , i+1, i+2);
 end;
end;

end.
