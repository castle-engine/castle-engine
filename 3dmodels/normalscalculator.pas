{
  Copyright 2003-2005,2008 Michalis Kamburelis.

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

{ @abstract(Ten modul to miejsce dla funkcji ktore obliczaja wektory normalne -
  przede wszystkim dla CreateNormals ktora liczy wektory
  normalne robiac je miejscami smooth (a miejscami zostawiajac flat)
  w zaleznosci od creaseAngle.)

  Aczkolwiek niniejszy modul zostal stworzony przede wszystkim
  na uslugi VRMLNodes to jednak nie uzywa zadnych modulow zwiazanych
  specyficznie z VRMLem i mozna go uzyc gdziekolwiek bedzie potrzebny.

  (planowalem uzyc go dla modeli 3ds'ow, GEO i OBJ'ow ale kiedy zrobilem
  konwertowanie w pamieci 3ds, obj, geo -> vrml okazalo sie to juz
  niepotrzebne : wystarczy ze w VRML'u bedziemy generowac normale
  i ladowac pliki przez LoadAsVRML i w rezultacie 3ds'y, obj i geo
  tez beda mialy normale generowane kodem z ponizszego modulu)
}

unit NormalsCalculator;

interface

uses SysUtils, KambiUtils, VectorMath;

{ CoordIndex to lista indeksow vertexow. Kazdy indeks okresla vertex
  o takim numerze z vertices, indeksy < 0 wyznaczaja poczatek kolejnej
  face. Innymi slowy, CoordIndex ma taki format jak pole CoordIndex node'a
  VRML'a IndexedFaceSet.

  Vertices i CoordIndex beda tylko czytane, nie beda poprawiane.
  Poprawnosc indeksow jest WYMAGANA (aby kazdy indeks byl < Vertices.Count),
  poprawnosc faces (zeby byly convex, nie podawaly tych samych indeksow dwa
  razy itp.) jest w tej chwili ZALECANA. To znaczy sprobujemy zrobic cos
  sensownego dla nieprawidlowych faces (bedziemy zakladac ze te podczas
  renderowania te faces beda poprawnie ztesselowane) a na ile to wyjdzie -
  - trudno powiedziec. Na pewno bedzie sie mozna czasem przyczepic.

  Zwraca tablice wektorow ktora ma tyle elementow co CoordIndex.
  Jej wartosci dla nieujemnych indeksow CoordIndex to normal dla tego
  vertexa na tej face. Jaj wartosci dla ujemnych indeksow sa niezdefiniowane.
  FromCCW kontroluje czy normale wychodza z CCW : true to wychodza z CCW,
  false to wychodza z CW.

  Liczy normale uzywajac creaseAngleRad (w radianach).
  Wszystkie normale sa znormalizowane. Grupa scian zlaczona jednym vertexem
  bedzie miala na tym vertexie jeden normal jezeli kat miedzy plaszczyznami
  miedzy kazda para scian w tej grupie bedzie < creaseAngle. To znaczy
  jezeli vertex styka sie z ilomas scianami to podzielimy te
  sciany na kilka grup i niezaleznie zrobimy smooth na tych grupach -
  np. gdy vertex styka sie z 4 scianami mozemy zrobic smooth na 2 i inny
  smooth na 2 pozostalych. Albo np. na 2, 1, 1 (czyli dwie sciany smoothed
  ze soba, dwie pozostale flat).

  Note that when creaseAngleRad >= Pi, you wil be better off
  using CreateSmoothNormals. This will work faster, and return shorter
  normals array (so it's also more memory-efficient).

  (Pamietaj zwolnic pozniej zwrocony obiekt przez Free.) }
function CreateNormals(CoordIndex: TDynLongintArray;
  vertices: TDynVector3SingleArray;
  creaseAngleRad: Single;
  FromCCW: boolean): TDynVector3SingleArray;

{ Calculate perfectly smooth per-vertex normals.

  Note that the result is not a compatible replacement for CreateNormals,
  as we generate Vertices.Count normals (since each vertex has it's own
  normal). }
function CreateSmoothNormals(CoordIndex: TDynLongintArray;
  Vertices: TDynVector3SingleArray;
  FromCCW: boolean): TDynVector3SingleArray;

{ Calculate flat per-face normals.

  Note that the result is not a compatible replacement for CreateNormals,
  as it's length is the number of @italic(faces). For each face, a single
  normal is stored, so this is most sensible compact representation.
  Using something larger would be a waste of memory and time. }
function CreateFlatNormals(coordIndex: TDynLongintArray;
  vertices: TDynVector3SingleArray;
  FromCCW: boolean): TDynVector3SingleArray;

{ Calculate always smooth normals per-vertex, for triangle set.
  Assuming CoordIndex is given like for X3D IndexedTriangleSet,
  so every three indexes on CoordIndex indicate
  a separate triangle (with excessive indexes silently ignored).

  This generates Vertices.Count normal vectors in result.
  You should access these normal vectors just like Vertices,
  i.e. they are indexed by CoordIndex. }
function CreateSmoothNormalsTriangleSet(CoordIndex: TDynLongintArray;
  Vertices: TDynVector3SingleArray;
  FromCCW: boolean): TDynVector3SingleArray;

{ Calculate always smooth normals per-vertex, for quad set.
  Assuming CoordIndex is given like for X3D IndexedQuadSet,
  so every four indexes on CoordIndex indicate
  a separate quad (with excessive indexes silently ignored).

  This generates Vertices.Count normal vectors in result.
  You should access these normal vectors just like Vertices,
  i.e. they are indexed by CoordIndex. }
function CreateSmoothNormalsQuadSet(CoordIndex: TDynLongintArray;
  Vertices: TDynVector3SingleArray;
  FromCCW: boolean): TDynVector3SingleArray;

implementation

{$define read_interface}
{$define read_implementation}

type
  TFace = record
    StartIndex: integer;
    IndicesCount: integer;
    Normal: TVector3Single
  end;
  PFace = ^TFace;

  TDynArrayItem_1 = TFace;
  PDynArrayItem_1 = PFace;
  {$define DYNARRAY_1_IS_STRUCT}
  {$I DynArray_1.inc}
  type TDynFaceArray = TDynArray_1;

function CreateNormals(CoordIndex: TDynLongintArray;
  vertices: TDynVector3SingleArray;
  CreaseAngleRad: Single;
  FromCCW: boolean): TDynVector3SingleArray;
var
  faces: TDynFaceArray; { lista faces }

  { Lista dlugosci vertices.Count ktorej kazdy element mowi do jakich
    faces nalezy ten vertex (to znaczy podaje indeksy do tablicy faces[]).

    Jezeli faces byly nieprawidlowe (w ktorym to przypadku staramy sie
    w calym tym module zachowac mozliwie sensownie) to dany vertex moze
    byc wiecej niz jeden raz na jednym faces - to nic, w tej tablicy
    bedzie odpowiednie face wymienione tylko raz. }
  verticesFaces: array of TDynIntegerArray;

  normals: TDynVector3SingleArray absolute result;

  CosCreaseAngle: Single;

  procedure CalculateFacesAndVerticesFaces;
  var thisFace: PFace;
      i, thisFaceNum: integer;
  begin
   i := 0;
   while i < CoordIndex.Count do
   begin
    thisFaceNum := faces.Length;
    faces.IncLength;
    thisFace := faces.Pointers[thisFaceNum];

    thisFace^.StartIndex := i;
    while (i < CoordIndex.Count) and (CoordIndex[i] >= 0) do
    begin
      { Two tests below secure us from invalid CoordIndex values:
        1. of course, each CoordIndex[] value must be within range.
        2. in a correct face, each vertex may occur at most once.

        We have to deal with VRML data supplied by user here,
        so we have to secure against invalid values here.

        Note that we cannot remove wrong indexes here
        (like CoordIndex.Delete(i, 1)). While tempting, removing
        bad indexes is not so easy: for example in IndexedFaceSet
        we would have to remove also appropriate textureCoord, normal
        and material indexes. Moreover, I decided that my engine doesn't
        ever change VRML data implicitly (even when this data is clearly
        incorrect...). So we cannot do such things. }

      if (CoordIndex[i] < Vertices.Count) and
         (VerticesFaces[CoordIndex[i]].IndexOf(thisFaceNum) = -1) then
        VerticesFaces[CoordIndex[i]].AppendItem(thisFaceNum);
      Inc(i);
    end;

    { licz thisFace.IndicesCount
      Skompletowalismy jedno face : to indeksy od StartIndex do i-1 }
    thisFace^.IndicesCount := i-thisFace^.StartIndex;

    { licz thisFace.Normal }
    thisFace^.Normal := IndexedPolygonNormal(
      @(CoordIndex.Items[thisFace^.StartIndex]),
      thisFace^.IndicesCount,
      Vertices.ItemsArray, Vertices.Count,
      Vector3Single(0, 0, 1));

    { przejdz do nastepnej sciany (omin ujemny indeks na ktorym stoimy;
      ew. przejdz z CoordIndex.Count do CoordIndex.Count+1, co niczemu nie szkodzi) }
    Inc(i);
   end;
  end;

  procedure SetNormal(vertexNum: integer; const face: TFace; const Normal: TVector3Single);
  { ustaw normal w tablicy normals dla sciany face i vertexu numer vertexNum
      (vertexNum to indeks do tablicy vertices, czyli to samo co elementy
      CoordIndex).
    Poniewaz staramy sie zachowywac sensownie nawet dla nieprawidlowych faces
      wiec zakladamy tu ze dany vertex moze byc w jednej scianie wiecej niz jeden
      raz i ustawiamy normal dla wszystkich wystapien tego vertexa w tej face.
    Na koncu upewnia sie Assertem ze taki vertex w ogole byl (choc raz) w tej face-
      -wiec zawsze badz pewien ze vertexNum rzeczywiscie nalezy do tej sciany ! }
  var i: integer;
      vertFound: boolean;
  begin
   vertFound := false;
   for i := face.StartIndex to face.StartIndex +face.IndicesCount -1 do
    if CoordIndex.Items[i] = vertexNum then
    begin
     vertFound := true; { vertFound := true, ale to nic, szukamy dalej }
     normals.Items[i] := Normal;
    end;
   Assert(vertFound, 'Internal error - NormalsCalculator.SetNormal failed');
  end;

  procedure CalculateVertexNormals(vertexNum: integer);
  var
    { ustalane na poczatku na verticesFaces[vertexNum] }
    thisVertexFaces: TDynIntegerArray;

    function FaceCanBeSmoothedWithFaces(faceNum: integer;
      faceNums: TDynIntegerArray): boolean;
    { czy sciana faceNum moze byc smooth razem ze wszystkimi scianami z faceNums ?
      To tutaj uwzgledniamy creaseAngleRad. faceNum  i faceNums[] to
      indeksy do tablicy thisVertexFaces. }
    var i: integer;
    begin
     for i := 0 to faceNums.Count-1 do
      { I want to check that
          AngleRadBetweenNormals(...) >= CreaseAngleRad
        so
          ArcCos(CosAngleRadBetweenNormals(...)) >= CreaseAngleRad
        so
          CosAngleBetweenNormals(...) < CosCreaseAngle }
      if CosAngleBetweenNormals(
        faces.Items[thisVertexFaces.Items[faceNum]].Normal,
        faces.Items[thisVertexFaces.Items[faceNums[i]]].Normal) <
        CosCreaseAngle then
       Exit(false);
     result := true;
    end;

  var i, j: integer;
      { aktualna grupa faces co do ktorej ustalilismy ze na tym vertexie
        maja wspolny normal. Indeksy wskazuja na indeksy w verticesFaces[vertexNum].Count }
      smoothFaces: TDynIntegerArray;
      { true jezeli dla jakiejs sciany (i dla vertexa vertexNum oczywiscie)
        juz zapisalismy normal. }
      handledFaces: TDynBooleanArray;
      Normal: TVector3Single;
  begin
   thisVertexFaces := verticesFaces[vertexNum];

   smoothFaces := nil;
   handledFaces := nil;
   try
    handledFaces := TDynBooleanArray.Create(thisVertexFaces.Count);
    handledFaces.SetAll(false);
    smoothFaces := TDynIntegerArray.Create;

    for i := 0 to thisVertexFaces.Count-1 do
     if not handledFaces[i] then
     begin

      { ustal smoothFaces }
      smoothFaces.SetLength(1);
      smoothFaces[0] := i;

      for j := i+1 to thisVertexFaces.Count-1 do
       if (not handledFaces[j]) and FaceCanBeSmoothedWithFaces(j, smoothFaces) then
        smoothFaces.AppendItem(j);

      { zaznacz handled na true scianom ze smoothFaces i wylicz ich Normal }
      FillChar(Normal, SizeOf(Normal), 0);
      for j := 0 to smoothFaces.Count-1 do
      begin
       handledFaces[smoothFaces[j]] := true;
       VectorAddTo1st(Normal, faces.Items[thisVertexFaces[smoothFaces[j]]].Normal);
      end;
      NormalizeTo1st(Normal);

      { uzyj wyliczonego normala }
      for j := 0 to smoothFaces.Count-1 do
       SetNormal(vertexNum, faces.Items[thisVertexFaces[smoothFaces[j]]], Normal);
     end;
   finally
    smoothFaces.Free;
    handledFaces.Free;
   end;
  end;

var i: integer;
begin
 CosCreaseAngle := Cos(CreaseAngleRad);

 SetLength(verticesFaces, vertices.Count);

 normals := nil;
 faces := nil;

 try
  try
   { zainicjuj verticesFaces i faces }

   for i := 0 to vertices.Count-1 do
    verticesFaces[i] := TDynIntegerArray.Create;
   faces := TDynFaceArray.Create;

   { przegladnij CoordIndex i skompletuj zawartosc tablic faces i verticesFaces }
   CalculateFacesAndVerticesFaces;

   { teraz zainicjuj normals, bo CoordIndex.Items.Length zostalo juz ustalone
     i w CoordIndex nie bedziemy wprowadzac wiecej zmian }
   normals := TDynVector3SingleArray.Create(CoordIndex.Length);

   { for each vertex, calculate all his normals (on all his faces) }
   for i := 0 to vertices.Count-1 do CalculateVertexNormals(i);

   if not FromCCW then Result.Negate;
  finally

   { free verticesFaces and faces }
   for i := 0 to vertices.Count-1 do verticesFaces[i].Free;
   faces.Free;
  end;

 except FreeAndNil(normals); raise end;
end;

function CreateSmoothNormals(CoordIndex: TDynLongintArray;
  Vertices: TDynVector3SingleArray;
  FromCCW: boolean): TDynVector3SingleArray;
var
  I, J, StartIndex: integer;
  FaceNormal: TVector3Single;
begin
  Result := TDynVector3SingleArray.Create(Vertices.Length);
  try
    Result.FillChar(0);

    I := 0;
    while I < CoordIndex.Count do
    begin
      StartIndex := I;
      while (I < CoordIndex.Count) and (CoordIndex.Items[I] >= 0) do Inc(I);
      FaceNormal := IndexedPolygonNormal(
        @(CoordIndex.Items[StartIndex]),
        I - StartIndex,
        Vertices.ItemsArray, Vertices.Count,
        Vector3Single(0, 0, 0));
      { add FaceNormal to all vertices belonging to this face }
      for J := StartIndex to I - 1 do
        VectorAddTo1st(Result.Items[CoordIndex.Items[J]], FaceNormal);

      Inc(I);
    end;

    for I := 0 to Result.Count - 1 do
      NormalizeTo1st(Result.Items[I]);

    if not FromCCW then Result.Negate;
  except FreeAndNil(Result); raise end;
end;

function CreateFlatNormals(CoordIndex: TDynLongintArray;
  vertices: TDynVector3SingleArray;
  FromCCW: boolean): TDynVector3SingleArray;
var
  i, StartIndex: integer;
  FaceNumber: Integer;
begin
  { CoordIndex.Length is just a maximum length, we will shrink it later. }
  result := TDynVector3SingleArray.Create(CoordIndex.Length);
  try
    FaceNumber := 0;

    i := 0;
    while i < CoordIndex.Count do
    begin
      StartIndex := i;
      while (i < CoordIndex.Count) and (CoordIndex.Items[i] >= 0) do Inc(i);
      Result.Items[FaceNumber] := IndexedPolygonNormal(
        @(CoordIndex.Items[StartIndex]),
        i - startIndex,
        Vertices.ItemsArray, Vertices.Count,
        Vector3Single(0, 0, 0));
      Inc(FaceNumber);

      Inc(i);
    end;

    Result.Length := FaceNumber;

    if not FromCCW then result.Negate;
  except FreeAndNil(result); raise end;
end;

function CreateSmoothNormalsTriangleSet(CoordIndex: TDynLongintArray;
  Vertices: TDynVector3SingleArray;
  FromCCW: boolean): TDynVector3SingleArray;
var
  I: integer;
  FaceNormal: TVector3Single;
begin
  Result := TDynVector3SingleArray.Create(Vertices.Length);
  try
    Result.FillChar(0);

    I := 0;
    while I + 2 < CoordIndex.Count do
    begin
      FaceNormal := TriangleNormal(
        Vertices.Items[CoordIndex.Items[I    ]],
        Vertices.Items[CoordIndex.Items[I + 1]],
        Vertices.Items[CoordIndex.Items[I + 2]]);

      VectorAddTo1st(Result.Items[CoordIndex.Items[I    ]], FaceNormal);
      VectorAddTo1st(Result.Items[CoordIndex.Items[I + 1]], FaceNormal);
      VectorAddTo1st(Result.Items[CoordIndex.Items[I + 2]], FaceNormal);

      I += 3;
    end;

    for I := 0 to Result.Count - 1 do
      NormalizeTo1st(Result.Items[I]);

    if not FromCCW then Result.Negate;
  except FreeAndNil(Result); raise end;
end;

function CreateSmoothNormalsQuadSet(CoordIndex: TDynLongintArray;
  Vertices: TDynVector3SingleArray;
  FromCCW: boolean): TDynVector3SingleArray;
var
  I: integer;
  FaceNormal: TVector3Single;
begin
  Result := TDynVector3SingleArray.Create(Vertices.Length);
  try
    Result.FillChar(0);

    I := 0;
    while I + 3 < CoordIndex.Count do
    begin
      { Normal is average of normals of two triangles. }
      FaceNormal := Normalized(VectorAdd(
        TriangleNormal(
          Vertices.Items[CoordIndex.Items[I    ]],
          Vertices.Items[CoordIndex.Items[I + 1]],
          Vertices.Items[CoordIndex.Items[I + 2]]),
        TriangleNormal(
          Vertices.Items[CoordIndex.Items[I    ]],
          Vertices.Items[CoordIndex.Items[I + 2]],
          Vertices.Items[CoordIndex.Items[I + 3]])));

      VectorAddTo1st(Result.Items[CoordIndex.Items[I    ]], FaceNormal);
      VectorAddTo1st(Result.Items[CoordIndex.Items[I + 1]], FaceNormal);
      VectorAddTo1st(Result.Items[CoordIndex.Items[I + 2]], FaceNormal);
      VectorAddTo1st(Result.Items[CoordIndex.Items[I + 3]], FaceNormal);

      I += 4;
    end;

    for I := 0 to Result.Count - 1 do
      NormalizeTo1st(Result.Items[I]);

    if not FromCCW then Result.Negate;
  except FreeAndNil(Result); raise end;
end;

end.
