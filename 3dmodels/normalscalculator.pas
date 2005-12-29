{
  Copyright 2003-2005 Michalis Kamburelis.

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

{ coordIndex to lista indeksow vertexow. Kazdy indeks okresla vertex
  o takim numerze z vertices, indeksy < 0 wyznaczaja poczatek kolejnej
  face. Innymi slowy, coordIndex ma taki format jak pole coordIndex node'a
  VRML'a IndexedFaceSet.

  Vertices i coordIndex beda tylko czytane, nie beda poprawiane.
  Poprawnosc indeksow jest WYMAGANA (aby kazdy indeks byl < Vertices.Count),
  poprawnosc faces (zeby byly convex, nie podawaly tych samych indeksow dwa
  razy itp.) jest w tej chwili ZALECANA. To znaczy sprobujemy zrobic cos
  sensownego dla nieprawidlowych faces (bedziemy zakladac ze te podczas
  renderowania te faces beda poprawnie ztesselowane) a na ile to wyjdzie -
  - trudno powiedziec. Na pewno bedzie sie mozna czasem przyczepic.

  Zwraca tablice wektorow ktora ma tyle elementow co coordIndex.
  Jej wartosci dla nieujemnych indeksow coordIndex to normal dla tego
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

  (Pamietaj zwolnic pozniej zwrocony obiekt przez Free.) }
function CreateNormals(coordIndex: TDynLongintArray;
  vertices: TDynVector3SingleArray;
  creaseAngleRad: Single;
  FromCCW: boolean): TDynVector3SingleArray;

{ j.w. ale normale beda zawsze flat }
function CreateFlatNormals(coordIndex: TDynLongintArray;
  vertices: TDynVector3SingleArray;
  FromCCW: boolean): TDynVector3SingleArray;

{ j.w. ale normale beda zawsze smoothed (jakby creaseAngle bylo nieskonczone) }
function CreateSmoothNormals(coordIndex: TDynLongintArray;
  vertices: TDynVector3SingleArray;
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

type
  TArray_TDynIntegerArray = packed array[0..MaxInt div SizeOf(Pointer) -1]of TDynIntegerArray;
  PArray_TDynIntegerArray = ^TArray_TDynIntegerArray;

procedure VectorsNegate(Vectors: TDynVector3SingleArray);
var i: integer;
begin
 for i := 0 to Vectors.Count-1 do
  Vectors.Items[i] := VectorNegate(Vectors.Items[i]);
end;

function CreateNormals(coordIndex: TDynLongintArray;
  vertices: TDynVector3SingleArray;
  creaseAngleRad: Single;
  FromCCW: boolean): TDynVector3SingleArray;
var faces: TDynFaceArray; { lista faces }
    { Lista dlugosci vertices.Count ktorej kazdy element mowi do jakich
        faces nalezy ten vertex (to znaczy podaje indeksy do tablicy faces[]).
      Jezeli faces byly nieprawidlowe (w ktorym to przypadku staramy sie
        w calym tym module zachowac mozliwie sensownie) to dany vertex moze
        byc wiecej niz jeden raz na jednym faces - to nic, w tej tablicy
        bedzie odpowiednie face wymienione tylko raz. }
    verticesFaces: PArray_TDynIntegerArray;

    normals: TDynVector3SingleArray absolute result;

  procedure CalculateFacesAndVerticesFaces;
  var thisFace: PFace;
      i, thisFaceNum: integer;
  begin
   i := 0;
   while i < coordIndex.Count do
   begin
    thisFaceNum := faces.Length;
    faces.IncLength;
    thisFace := faces.Pointers[thisFaceNum];

    thisFace.StartIndex := i;
    while (i < coordIndex.Count) and (coordIndex[i] >= 0) do
    begin
     {gdybysmy chcieli tu kasowac nieprawidlowe indeksy to moglibysmy tu robic
        if (coordIndex[i] >= vertices.Count) then coordIndex.Delete(i, 1);
      Nie robie tego bo takie kasowanie zlych indeksow np. w nodzie IndexedFaceSet
        wymagaloby tez kasowania odpowiednich indeksow textureCoord, normal
        i material. Wiec nie mozemy takiego poprawiania robic tutaj - tutaj
        musimy zakladac ze JUZ wszystko jest OK.
      Jeszcze jedno : w poprawnych faces nie wystepuje dwa razy ten sam vertex.
        Wiec test ponizej na IndexOf(thisFaceNum) nie bylby potrzebny.
        Ale my staramy sie zeby ta procedura zachowywala sie choc troche
        sensownie nawet dla nieprawidlowych faces. }
     if verticesFaces[coordIndex[i]].IndexOf(thisFaceNum) = -1 then
      verticesFaces[coordIndex[i]].AppendItem(thisFaceNum);
     Inc(i);
    end;

    { licz thisFace.IndicesCount
      Skompletowalismy jedno face : to indeksy od StartIndex do i-1 }
    thisFace.IndicesCount := i-thisFace.StartIndex;

    { licz thisFace.Normal }
    thisFace.Normal := IndexedPolygonNormal(
      @(coordIndex.Items[thisFace.StartIndex]),
      thisFace.IndicesCount,
      Vertices.Items, Vector3Single(0, 0, 1));

    { przejdz do nastepnej sciany (omin ujemny indeks na ktorym stoimy;
      ew. przejdz z coordIndex.Count do coordIndex.Count+1, co niczemu nie szkodzi) }
    Inc(i);
   end;
  end;

  procedure SetNormal(vertexNum: integer; const face: TFace; const Normal: TVector3Single);
  { ustaw normal w tablicy normals dla sciany face i vertexu numer vertexNum
      (vertexNum to indeks do tablicy vertices, czyli to samo co elementy
      coordIndex).
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
    if coordIndex.Items[i] = vertexNum then
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

    function FaceCanBeSmoothedWithFaces(faceNum: integer; faceNums: TDynIntegerArray): boolean;
    { czy sciana faceNum moze byc smooth razem ze wszystkimi scianami z faceNums ?
      To tutaj uwzgledniamy creaseAngleRad. faceNum  i faceNums[] to
      indeksy do tablicy thisVertexFaces. }
    var i: integer;
    begin
     for i := 0 to faceNums.Count-1 do
      if AngleRadBetweenVectors(
        faces.Items[thisVertexFaces[faceNum]].Normal,
        faces.Items[thisVertexFaces[faceNums[i]]].Normal) >= creaseAngleRad then
       exit(false);
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
       VectorAddTo1st(Normal, faces[thisVertexFaces[smoothFaces[j]]].Normal);
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
 normals := nil;
 verticesFaces := nil;
 faces := nil;

 try
  try
   { zainicjuj verticesFaces i faces }
   verticesFaces := GetClearMem(SizeOf(Pointer) * vertices.Count);
   for i := 0 to vertices.Count-1 do
    verticesFaces[i] := TDynIntegerArray.Create;
   faces := TDynFaceArray.Create;

   { przegladnij coordIndex i skompletuj zawartosc tablic faces i verticesFaces }
   CalculateFacesAndVerticesFaces;

   { teraz zainicjuj normals, bo coordIndex.Items.Length zostalo juz ustalone
     i w coordIndex nie bedziemy wprowadzac wiecej zmian }
   normals := TDynVector3SingleArray.Create(coordIndex.Length);

   { for each vertex, calculate all his normals (on all his faces) }
   for i := 0 to vertices.Count-1 do CalculateVertexNormals(i);

   if not FromCCW then VectorsNegate(result);
  finally

   { free verticesFaces and faces }
   if verticesFaces <> nil then
   begin
    for i := 0 to vertices.Count-1 do verticesFaces[i].Free;
    FreeMem(verticesFaces);
   end;
   faces.Free;
  end;

 except FreeAndNil(normals); raise end;
end;

function CreateSmoothNormals(coordIndex: TDynLongintArray;
  vertices: TDynVector3SingleArray;
  FromCCW: boolean): TDynVector3SingleArray;
var VertNormals: TDynVector3SingleArray;
    i, j, StartIndex: integer;
    FaceNormal: TVector3Single;
begin
 result := TDynVector3SingleArray.Create(coordIndex.Length);
 try

  VertNormals := TDynVector3SingleArray.Create(Vertices.Length);
  try
   VertNormals.FillChar(0);

   i := 0;
   while i < coordIndex.Count do
   begin
    StartIndex := i;
    while (i < coordIndex.Count) and (coordIndex.Items[i] >= 0) do Inc(i);
    FaceNormal := IndexedPolygonNormal(
       @(coordIndex.Items[StartIndex]),
       i-StartIndex,
       Vertices.Items, Vector3Single(0, 0, 0));
    {dodaj FaceNormal do normali wszystkich punktow tej face}
    for j := StartIndex to i-1 do
     VectorAddTo1st(VertNormals.Items[coordIndex.Items[j]], FaceNormal);

    Inc(i);
   end;

   for i := 0 to VertNormals.Count-1 do NormalizeTo1st(VertNormals.Items[i]);

   for i := 0 to coordIndex.Count-1 do
    if coordIndex.Items[i] >= 0 then
     result.Items[i] := VertNormals.Items[coordIndex.Items[i]];

   if not FromCCW then VectorsNegate(result);
  finally vertNormals.Free end;

 except FreeAndNil(result); raise end;
end;

function CreateFlatNormals(coordIndex: TDynLongintArray;
  vertices: TDynVector3SingleArray;
  FromCCW: boolean): TDynVector3SingleArray;
var i, j, StartIndex: integer;
    faceNormal: TVector3Single;
begin
 result := TDynVector3SingleArray.Create(coordIndex.Length);
 try

  i := 0;
  while i < coordIndex.Count do
  begin
   StartIndex := i;
   while (i < coordIndex.Count) and (coordIndex.Items[i] >= 0) do Inc(i);
   FaceNormal := IndexedPolygonNormal(
     @(coordIndex.Items[StartIndex]),
     i-startIndex,
     Vertices.Items, Vector3Single(0, 0, 0));
   for j := StartIndex to i-1 do result.Items[j] := FaceNormal;

   Inc(i);
  end;

  if not FromCCW then VectorsNegate(result);
 except FreeAndNil(result); raise end;
end;

end.
