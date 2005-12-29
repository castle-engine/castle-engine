{
  Copyright 2003-2004 Michalis Kamburelis.

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

{ @abstract(Triangulating, i.e. decomposing polygons into triangles.) }

unit Triangulator;

interface

uses SysUtils, VectorMath, KambiUtils;

type
  TTriangulatorProc = procedure(const Tri:TVector3Longint; Data:Pointer);

{ FaceIndices[0]..FaceIndices[FaceIndicesCount-1] zawieraja indeksy
  do tablicy Vertices. Te indeksy okreslaja kolejne wierzcholki sciany,
  niekoniecznie convex.
    
  TriangulateFace rozbija face na trojkaty, dla kazdego trojkata wywolujac
  TriangulatorProc z drugim parametrem = TriangulatorProcData a 
  pierwszym parametrem ustawionym na indeksy do tablicy FaceIndices[]
  ktore utworza dany trojkat (zwracamy indeksy a nie gotowe wektory z tablicy 
  Vertices (ani nawet indkesy do Vertices[]) zeby program mogl nie tylko 
  wyciagnac sobie z tablic Vertices[FaceIndices[]] wektory ale takze byc 
  moze z innych tablic wyciagnac informacje towarzyszace dla wierzcholka,
  a do tego moze byc potrzebny nie tylko indeks do Vertices ale wrecz
  indeks do FaceIndices - patrz np. w rendererze dla IndexedFaceSet.)
  Indeksy te sa zwiekszone o AddToIndices (moze byc ujemne) (to jest
  wygodne gdy FaceIndices to wskaznik do srodka jakiejs tablicy).
    
  Generowane trojkaty maja taka sama orientacje (normal z CCW) jak 
  oryginalny polygon - tzn. normale wyliczone np. IndexedPolygonNormal
  na non-convex face beda dobre dla trojkatow po triangulacji tej face
  przez TriangulateFace.
    
  Nie powinienes uzywac tej procedury gdy WIESZ ze face jest convex -
  wtedy mozna przeciez rozbic face na trojkaty bardzo latwo. Uzywaj
  tej proc gdy rzeczywiscie face moze byc non-convex - jesli wiesz ze
  jest convex to uzyj TriangulateConvexFace. }
procedure TriangulateFace(FaceIndices: PArray_Longint; FaceIndicesCount: integer;
  Vertices: PArray_Vector3Single; TriangulatorProc: TTriangulatorProc;
  TriangulatorProcData: Pointer; AddToIndices: Longint);

{ proste procedury TriangulateConvexFace i TriangulateTriangleStrip nie
  potrzebuja FaceIndices ani Vertices. Wystarczy im FaceIndicesCount.
  One tez zapewniaja konsekwentna orientacje trojkatow (w przypadku TriangleStrip
  liczy sie orientacja pierwszych trzech wierzcholkow, tzn. kolejno dostajemy
  v0-v1-v2, v2-v1-v3, v2-v3-v4, v4-v3-v5 itd., podobnie jak w OpenGLu).
  Zwracam uwage ze TriangleStrip ma po prostu jednoznacznie wyznaczona 
  triangulacje i nie ma w jego przypadku sensu rozwazanie czy jest convex czy 
  non-convex. }
procedure TriangulateConvexFace(FaceIndicesCount: integer; 
  TriangulatorProc: TTriangulatorProc; TriangulatorProcData: Pointer; 
  AddToIndices: Longint);  
procedure TriangulateTriangleStrip(IndicesCount: integer; 
  TriangulatorProc: TTriangulatorProc; TriangulatorProcData: Pointer; 
  AddToIndices: Longint);

implementation

{$define DEFINE_NEW_TRIANGLE_PROC := 
procedure NewTriangle(const p0,p1,p2: Longint);
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
  "sorry". }

procedure TriangulateFace(FaceIndices: PArray_Longint; FaceIndicesCount: integer;
  Vertices: PArray_Vector3Single; TriangulatorProc: TTriangulatorProc;
  TriangulatorProcData: Pointer; AddToIndices: Longint);
  DEFINE_NEW_TRIANGLE_PROC
    
  {$define VertsCount := FaceIndicesCount}
  function Verts(i: Longint): TVector3Single;
  begin
   result := Vertices[FaceIndices[i]];
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
 { najpierw odrzuc przypadek gdy VertsCount<3 i zrob trywialny przypadek
   VertsCount=3 }
 if VertsCount=3 then
  NewTriangle(0, 1, 2) else
 if VertsCount>3 then
 begin
  { wyznacz Center jako prosta srednia z wszystkich punktow }
  Center := ZeroVector3Single;
  for i := 0 to VertsCount-1 do VectorAddTo1st(Center, Verts(i));
  VectorScaleTo1st(Center, 1/VertsCount);
  
  { wyznacz punkt sposrod Verts[] najbardziej odlegly od Center.
    MaxLen to jego odleglosc od Center, MaxLenIndex to jego index w Verts[].
    sorry - czy tu PointDistanceSqr nie wystarczy ? }
  MaxLenIndex := 0;
  MaxLen := PointsDistance(Center, Verts(0));
  for i := 1 to VertsCount-1 do
  begin
   d := PointsDistance(Center, Verts(i));
   if d>MaxLen then
   begin
    MaxLen := d;
    MaxLenIndex := i;
   end;
  end;

  { p1 to indeks najdalszego sposrod Verts, p0 to poprzedni, p2 to nastepny }
  p1 := MaxLenIndex;
  if p1=0 then p0 := VertsCount-1 else p0 := p1-1;
  p2 := (p1+1) mod VertsCount;

  {sorry - czy tu negate potrzebne ?}
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

     if p0=Start then break;

     {sorry - czy tu negate potrzebne ?}
     nn := VectorNegate( TriangleNormal(Verts(p0), Verts(p1), Verts(p2)) );
     d := PointsDistance(nn, ConvexNormal);

     E1 := VectorProduct(nn, VectorSubtract(Verts(p1), Verts(p0)));
     E2 := VectorProduct(nn, VectorSubtract(Verts(p2), Verts(p1)));
     E3 := VectorProduct(nn, VectorSubtract(Verts(p0), Verts(p2)));

     Empty := True;

     for i := 0 to VertsCount-1 do
      if (not Outs.Items[i]) and (i<>p0) and (i<>p1) and (i<>p2) then
      begin
       Empty := Empty and not (
         (VectorDotProduct(E1, VectorSubtract(Verts(i), Verts(p0))) <= - SingleEqualityEpsilon) and
         (VectorDotProduct(E2, VectorSubtract(Verts(i), Verts(p1))) <= - SingleEqualityEpsilon) and
         (VectorDotProduct(E3, VectorSubtract(Verts(i), Verts(p2))) <= - SingleEqualityEpsilon) 
         );
      end;
    until (d <= 1.0) and Empty;

{ sorry - w graz.mgf.wrl jest ten blad i mimo to wszystko dziala ok 
  gdy zakomentarzowalem ponizszy check ?
    if p0=Start then raise Exception.Create('misbuilt polygonal face');}

    NewTriangle(p0, p1, p2);

    Outs.Items[p1] := True;
    Dec(Corners);
   end;
  finally Outs.Free end;
 end;
end;

{ proste Triangulate ---------------------------------------------------------- }

procedure TriangulateConvexFace(FaceIndicesCount: integer; 
  TriangulatorProc: TTriangulatorProc; TriangulatorProcData: Pointer; 
  AddToIndices: Longint);  
  DEFINE_NEW_TRIANGLE_PROC  
var i: integer;
begin
 for i := 0 to FaceIndicesCount-3 do NewTriangle(0, i+1, i+2);
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
