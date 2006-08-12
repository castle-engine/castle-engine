{
  Copyright 2002-2005 Michalis Kamburelis.

  This file is part of "Kambi's 3dmodels.gl Pascal units".

  "Kambi's 3dmodels.gl Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's 3dmodels.gl Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's 3dmodels.gl Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

type
  TRenderIndexedNormals = (inOverall, inGenerate, inUseSpecifiedPerVertex,
    inUseSpecifiedPerVertexIndexed);
  TRenderIndexedMaterials = (imOverall,
    imUseSpecifiedPerVertex, imUseSpecifiedPerVertexIndexed,
    imUseSpecifiedPerFace, imUseSpecifiedPerFaceIndexed);

  TGeneralIndexedRenderer = class
    {W kazdej podklasie musisz zdefiniowac metode Render_Face w ktorej
       narysujesz face uzywajac Render_Vertex do renderowania vertexow.
       Zapewne zechcesz zdefiniowac tez Render_Init i Render_Fini
       (chociaz nie musisz; jesli jednak je zdefiniujesz, pamietaj zawsze
       uzywac inherited).
       - W podklasach nie musisz sie martwic o wspolrzedne tekstury -
         zajmiemy sie nimi calkowicie w tej klasie.
       - W podklasach musisz generowac normale (wychodzace z CCW
         o ile Render_NormalsFromCCW, wpp. wychodzace z CW) tylko jesli
	 Render_Normals = inGenerate,
	 wpp. nie mozesz wywolywac glNormal - wszystkim zajmujemy sie w tej klasie.
       - Zajmujemy sie tez w tej klasie wszystkim zwiazanym z materialami -
         nie musisz sie o nie martwic w podklasach.
     Implementujac Render_Face/Init/Fini mozesz uzywac pol State i Node
       ktore opisuja nam co dokladnie renderujemy.
     Render_Face : renders all points between startIndex to endIndex-1.
       Wszystkie indeksy po drodze sa zamknietym ciagiem otoczonym
       -1 lub koncami listy FdCoordIndex. Render_Init/Fini mozesz
       wykorzystac jak chcesz - w sposob odpowiedni dla danej podklasy. }
    State: TVRMLGraphTraverseState;
    Node: TNodeGeneralIndexed_1;
    Renderer: TVRMLOpenGLRenderer;
    Render_UseTextureCoordIndex: boolean;
    Render_Materials: TRenderIndexedMaterials;
    Render_Normals: TRenderIndexedNormals;

    procedure Render_Vertex(indexNum: integer); virtual;
    function Render_GetVertex(indexNum: integer): TVector3f;
    procedure Render_Face(startIndex, endIndex: integer); virtual; abstract;

    procedure Render_Init; virtual;
    procedure Render_Fini; virtual;

    procedure Render;
    constructor Create(ARenderer: TVRMLOpenGLRenderer);
  end;

  TIndexedFaces_Or_TrianglesRenderer = class(TGeneralIndexedRenderer)
  private
    procedure Render_Init; override;
    procedure Render_Fini; override;
    { czy normale wychodza / maja wychodzic z CCW ?
      Ta zmienna odzwierciedla stan glFronFace jaki ustawilismy OpenGL'owi. }
    Render_NormalsFromCCW: boolean;
  end;

  TIndexedFaceSetRenderer = class(TIndexedFaces_Or_TrianglesRenderer)
  private
    Render_GeneratedNormals: TDynVector3SingleArray;
    procedure Render_Init; override;
    procedure Render_Fini; override;
    procedure Render_Vertex(indexNum: integer); override;
    procedure Render_Face(startIndex, endIndex: integer); override;
  end;

  TIndexedTriangleMeshRenderer = class(TIndexedFaces_Or_TrianglesRenderer)
  private
    procedure Render_Face(startIndex, endIndex: integer); override;
  end;

  TIndexedLineSetRenderer = class(TGeneralIndexedRenderer)
  private
    procedure Render_Init; override;
    procedure Render_Fini; override;
    procedure Render_Face(startIndex, endIndex: integer); override;
  end;

{ ogolne ---------------------------------------------------------------------- }

function CreateIndexedRenderer(ARenderer: TVRMLOpenGLRenderer): TGeneralIndexedRenderer;
begin
 if ARenderer.Render_Node is TNodeIndexedTriangleMesh_1 then
  result := TIndexedTriangleMeshRenderer.Create(ARenderer) else
 if ARenderer.Render_Node is TNodeIndexedFaceSet_1 then
  result := TIndexedFaceSetRenderer.Create(ARenderer) else
 if ARenderer.Render_Node is TNodeIndexedLineSet_1 then
  result := TIndexedLineSetRenderer.Create(ARenderer) else
  raise EInternalError.Create('CreateIndexedRenderer: Node not indexed');
end;

{ TGeneralIndexedRenderer ----------------------------------------------------- }

constructor TGeneralIndexedRenderer.Create(ARenderer: TVRMLOpenGLRenderer);
begin
 inherited Create;
 Renderer := ARenderer;
 State := Renderer.Render_State;
 Node := TNodeGeneralIndexed_1(Renderer.Render_Node);
end;

procedure TGeneralIndexedRenderer.Render_Vertex(indexNum: integer);
var VertexNum: integer;
begin
 VertexNum := Node.FdCoordIndex.Items.Items[indexNum];

 if Render_UseTextureCoordIndex then
  glTexCoordv(State.LastNodes.TextureCoordinate2.FdPoint.Items.Items[
    Node.FdTextureCoordIndex.Items.Items[indexNum]]);

 case Render_Normals of
  inUseSpecifiedPerVertex: {don't do anything - glDrawElement will get the normal};
  inUseSpecifiedPerVertexIndexed: glNormalv(
    State.LastNodes.Normal.FdVector.Items.Items[
      Node.FdNormalIndex.Items.Items[indexNum]]);
 end;

 case Render_Materials of
  imUseSpecifiedPerVertex: 
    Renderer.Render_BindMaterial_1(VertexNum);
  imUseSpecifiedPerVertexIndexed: 
    Renderer.Render_BindMaterial_1(Node.FdMaterialIndex.Items.Items[indexNum]);
 end;

 Renderer.DoGLArrayElement(State.LastNodes.Coordinate3.FdPoint.Items.Items, VertexNum);
end;

function TGeneralIndexedRenderer.Render_GetVertex(indexNum: integer): TVector3f;
begin
 result := State.LastNodes.Coordinate3.FdPoint.Items.Items[
   Node.FdCoordIndex.Items.Items[indexNum]];
end;

procedure TGeneralIndexedRenderer.Render;

  procedure TextureInit;
  { setup texture-related things }
  var localBBox: TBox3d;
      localBBoxSize: TVector3f;

    procedure SetupCoordGen(out gen: TVector4f;
      const coord: integer; const genStart, genEnd: TGLfloat);
    {chcemy mapowac localBBox[0, coord]...localBBox[0, coord]+localBBoxSize[coord]
                 na genStart...genEnd.
      czyli mamy dla punktu v[0..2] :
        s1 = (v[coord] - localBBox[0, coord]) / localBBoxSize[coord]
      i mamy s1 w zakresie 0..1, teraz
        s = s1 * (genEnd-genStart) + genStart
      i mamy to co chcemy - s przedziale genStart...genEnd.
     Sprowadzajac to do postaci ktora trzeba podac OpenGL'owi :
       s = v[coord] * (genEnd-genStart) / localBBoxSize[coord]
           - localBBox[0, coord] * (genEnd-genStart) / localBBoxSize[coord]
           + genStart
     Proste sprawdzenie :
       dla genStart, End = 0, 1 mamy
       s = v[coord] / localBBoxSize[coord] -
           localBBox[0, coord] / localBBoxSize[coord] =
           (v[coord] - localBBox[0, coord]) / localBBoxSize[coord] = s1
    }
    begin
     FillChar(gen, SizeOf(gen), 0);
     gen[coord]:=(genEnd-genStart) / localBBoxSize[coord];
     gen[3] := -localBBox[0, coord] * (genEnd-genStart) / localBBoxSize[coord]
             +genStart;
    end;

  var s_gen, t_gen: TVector4f;
      SCoord, TCoord, c1, c2: integer;
  begin
   Render_UseTextureCoordIndex := false;
   if Renderer.Render_TexCoordsNeeded then
   begin
    if Node.FdTextureCoordIndex.Count >= Node.FdCoordIndex.Count then
     Render_UseTextureCoordIndex := true else
     begin
      localBBox := Node.LocalBoundingBox(State);

      {oblicz rozmiary localBBox. SCoord to wspolrzedna na ktorej rozmiar jest
       najwiekszy, TCoord to druga wspolrzedna (gdyby jakies wspolrzedne
       byly rowne, X jest wazniejsze od Y a to jest wazniejsze od Z).}
      localBBoxSize := Box3dSizes(localBBox);
      SCoord := MaxVectorCoord(localBBoxSize);
      RestOf3dCoords(SCoord, c1, c2);
      if localBboxSize[c1] >= localBBoxSize[c2] then TCoord := c1 else TCoord := c2;

      {ustaw s_gen i t_gen - parametry do generowania tekstur}
      SetupCoordGen(s_gen, SCoord, 0, 1);
      SetupCoordGen(t_gen, TCoord, 0, localBBoxSize[TCoord]/localBBoxSize[SCoord]);

      {setup generating tex coords automatically}
      glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
      glTexGenfv(GL_S, GL_OBJECT_PLANE, @s_gen);
      glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
      glTexGenfv(GL_T, GL_OBJECT_PLANE, @t_gen);
      glEnable(GL_TEXTURE_GEN_S);
      glEnable(GL_TEXTURE_GEN_T);
     end;
   end;
  end;

  procedure TextureFini;
  begin
   if Renderer.Render_TexCoordsNeeded and
     (not Render_UseTextureCoordIndex) then
   begin
    {to znaczy ze generowalismy wspolrzedne tekstury; wiec trzeba je teraz wylaczyc}
    glDisable(GL_TEXTURE_GEN_S);
    glDisable(GL_TEXTURE_GEN_T);
   end;
  end;

  function IndexListNotEmpty(MFIndexes: TMFLong): boolean;
  begin
   result:=(MFIndexes.Count > 0) and
           (not ((MFIndexes.Count = 1) and (MFIndexes.Items.Items[0] = -1)));
  end;

  procedure NormalsInit;
  { setup Render_Normals variable }
  begin
   { TODO: not all normal bindings are supported,
     we Generate Normals if normalBinding is not supported.
     Nie zrobione : PER_PART, PER_PART_INDEXED, PER_FACE, PER_FACE_INDEXED }

   Render_Normals := inGenerate;

   case State.LastNodes.NormalBinding.FdValue.Value of
    BIND_DEFAULT, BIND_PER_VERTEX_INDEXED:
      if (Node.FdNormalIndex.Count > 0) and (Node.FdNormalIndex.Items.Items[0] >=0) then
       Render_Normals := inUseSpecifiedPerVertexIndexed;
    BIND_PER_VERTEX:
      Render_Normals := inUseSpecifiedPerVertex;
    BIND_OVERALL:
      if State.LastNodes.Normal.FdVector.Count > 0 then
        Render_Normals := inOverall;
   end;

   {ponizsza instrukcja ma taki efekt : jezeli nie bylo zadnych normali w ostatnim
    node Normal (albo wrecz nie bylo zadnego takiego node'a w pliku, czyli
    LastNormal to DefaultNormalNode sceny) to bedziemy generowac normale.

    Jezeli juz bylismy ustawieni na generowanie normali albo jezeli to jest
    pusty indexed node (tzn. nie podaje zadnych faces - co jest przeciez
    dopuszczalne) to ta instrukcja nie spowoduje zadnego efektu (a wiec
    tez niczemu nie zaszkodzi). Wpp. (jezeli nie mielismy generowac
    normali i node niepusty) ta instrukcja sprawi ze taki niepoprawny VRML
    zostanie mimo wszystko dobrze odczytany i wyswietlony. }
   if State.LastNodes.Normal.FdVector.Count = 0 then
    Render_Normals := inGenerate;

   { podejmij pewna inicjalizacje juz PO ustaleniu Render_Normals }
   case Render_Normals of
    inOverall: glNormalv(State.LastNodes.Normal.FdVector.Items.Items[0]);
    inUseSpecifiedPerVertex:
      begin
       {w tym przypadku mozemy uzyc vertex array dla normali bo
        indeks vertexu = indeks normala}
       glNormalPointer(GL_FLOAT, 0, State.LastNodes.Normal.FdVector.Items.Items);
       glEnableClientState(GL_NORMAL_ARRAY);
      end;
   end;
  end;

  procedure NormalsFini;
  begin
  end;

  procedure MaterialsInit;
  { setup Render_Materials variable }
  begin
   Render_Materials := imOverall;
   case State.LastNodes.MaterialBinding.FdValue.Value of
    {BIND_OVERALL, BIND_DEFAULT: take default imOverall}
    BIND_PER_VERTEX: Render_Materials := imUseSpecifiedPerVertex;
    BIND_PER_VERTEX_INDEXED:
      if IndexListNotEmpty(Node.FdMaterialIndex) then
       Render_Materials := imUseSpecifiedPerVertexIndexed;
    BIND_PER_PART, BIND_PER_FACE: Render_Materials := imUseSpecifiedPerFace;
    BIND_PER_PART_INDEXED, BIND_PER_FACE_INDEXED:
      if IndexListNotEmpty(Node.FdMaterialIndex) then
       Render_Materials := imUseSpecifiedPerFaceIndexed;
   end;

   { TODO: we handle all material bindings, but we handle BIND_PER_PART
     and BIND_PER_PART_INDEXED wrong for IndexedLineSet. }

   if Render_Materials = imOverall then Renderer.Render_BindMaterial_1(0);
  end;

var
  startIndex, endIndex, faceNumber: integer;
  UseLockArrays: boolean;
begin
 {initialize most important thing we will use with indexed nodes :
  vertex arrays. Musimy to zainicjowac najpierw bo ponizej NormalsInit
  (a byc moze kiedys cos wiecej) moga tez zaladowac swoje vertex arrays. }
 glPushClientAttrib(GL_CLIENT_VERTEX_ARRAY_BIT);
 try
  glVertexPointer(3, GL_FLOAT, 0, State.LastNodes.Coordinate3.FdPoint.Items.Items);
  glEnableClientState(GL_VERTEX_ARRAY);

  { See
    [http://www.opengl.org/documentation/specs/version1.2/EXTspecs/compiled_vertex_array.txt]
    for description of GL_EXT_compiled_vertex_array.

    Note that I can't use it when last Coordinate3 had no nodes
    (because glLockArraysEXT(0, 0) causes OpenGL error "invalid value".) }
  UseLockArrays := GL_EXT_compiled_vertex_array and
    (State.LastNodes.Coordinate3.FdPoint.Items.Count <> 0);
  if UseLockArrays then
   glLockArraysEXT(0, State.LastNodes.Coordinate3.FdPoint.Items.Count);

  {initialize rendering}
  TextureInit;
  MaterialsInit;
  NormalsInit;
  Render_Init;
  try

   {wlasciwe rendering : wywoluj Render_Face}
   startIndex := 0;
   faceNumber := 0;
   while startIndex < Node.FdCoordIndex.Count do
   begin
    endIndex := startIndex;
    while (endIndex < Node.FdCoordIndex.Count) and
          (Node.FdCoordIndex.Items.Items[endIndex] >= 0) do
     Inc(endIndex);

    case Render_Materials of
     imUseSpecifiedPerFace:
       Renderer.Render_BindMaterial_1(faceNumber);
     imUseSpecifiedPerFaceIndexed:
       Renderer.Render_BindMaterial_1(Node.FdMaterialIndex.Items.Items[faceNumber]);
    end;

    Render_Face(startIndex, endIndex);
    Inc(faceNumber);
    startIndex := endIndex+1;
   end;
  finally

   {finalize rendering}
   Render_Fini;
   NormalsFini;
   TextureFini;
  end;

  if UseLockArrays then
   glUnlockArraysEXT;
 finally glPopClientAttrib; end;
end;

{ Render_Init / Fini w tej klasie nie sa abstract zeby umozliwic zawsze
  wywolywanie inherited; w podklasach }
procedure TGeneralIndexedRenderer.Render_Init; begin end;
procedure TGeneralIndexedRenderer.Render_Fini; begin end;

{ TIndexedFaces_Or_TrianglesRenderer -------------------------------------------- }

procedure TIndexedFaces_Or_TrianglesRenderer.Render_Init;
var sh: TNodeShapeHints;
begin
 inherited;
 sh := State.LastNodes.ShapeHints;
 glPushAttrib(GL_POLYGON_BIT);

 { To jest problem w specyfikacji VRMLa 1.0 : czy Normale podawane
   w nodzie Normal wychodza ze strony CCW scian czy tez ze strony scian
   outside, zgodnie z vertexOrdering ?
   To drugie ma problem z vertexOrdering = UNKNOWN_VERTEX_ORDERING.
   Natomiast za drugim przemawia fakt ze tak jest w VRML'u 97.

   Zdefiniuj ponizej NORMALS_ALWAYS_FROM_CCW (tylko na potrzeby implementacji
   tej procedury, ten symbol nie jest uzywany nigdzie indziej) aby miec
   wersje gdzie normale sa zawsze z ccw.

   doom.wrl/ ma normale sa zgodnie z vertexOrdering
   mgf2inv to (po wielu podejsciach) ciemna sprawa. Generuje VRML'e w ktorych
     zawsze normale sa z ccw, i w sumie zawsze vertexOrdering okazuje sie
     byc CCW wiec jest ok. Tylko nie jestem pewien czy autor mgf2inv przypadkiem
     nie pomylil sie i nie chcial napisac vertexOrdering CW.
   No coz, w kazdym razie Greg Ward nie jest najwyrazniej autorytetem w sprawie
     VRMLa wiec uznaje za prawidlowa droge zgodna z VRMLem 97 (i Doom'em)
     - normale sa zgodnie z vertexOrdering.
   Wiec ostatecznie decyduje sie aby NORMALS_ALWAYS_FROM_CCW nigdy nie bylo
     zdefiniowane (ale zostawiam sobie mozliwosc zeby kiedys ew. zmienic zdanie).
 }
 { $define NORMALS_ALWAYS_FROM_CCW}

 {$ifdef NORMALS_ALWAYS_FROM_CCW}
 Render_NormalsFromCCW := true;
 {$else}
 { zmieniamy glFrontFace na GL_CW jesli VertexOrdering jest clockwise.
   W ten sposob mowimy OpenGL'owi ktorym faces robic cull o ile
   za chwile wlaczymy culling i mowimy tez z ktorej strony polygonow
   wychodza normale (my wprawdzie moglibysmy sobie generowac normale
   z dowolnej strony z ktorej chcemy ale normale w pliku VRML'a sa
   zapisane zgodnie z aktualna orientacja vertexOrdering).
   W tym miejscu uznajemy tez VERTORDER_UNKNOWN_ORDERING za COUNTERCLOCKWISE
   (a autorzy VRMLi w ogole nie powinni podawac normali jesli
   nie podadza vertexOrdering innego niz UNKNOWN) }
 Render_NormalsFromCCW := SH.FdVertexOrdering.Value <> VERTORDER_CLOCKWISE;
 {$endif}
 if not Render_NormalsFromCCW then
  glFrontFace(GL_CW);

 {if vertex ordering is consistent and object is SOLID than we use OpenGL's
  backface culling. Warning : we should check viewer position to know
  whether to cull outside or inside polygons because if the viewer is inside
  the object we should cull outside polygons. But luckily for us (because
  it's extremely difficult to decide whether a point in space is
  inside or outside such solid object) VRML 1.0 specification states clearly
  that if an object is SOLID we can assume that the viewer is ALWAYS OUTSIDE
  OF IT. So we cull faces inside  }
 if (SH.FdVertexOrdering.Value <> VERTORDER_UNKNOWN) and
    (SH.FdShapeType.Value = SHTYPE_SOLID) then
 begin
  {$ifdef NORMALS_ALWAYS_FROM_CCW}
  { if normals always from ccw, than we must modify glCullFace to GL_FRONT
      (it has default value GL_BACK) to cull the right faces.
    If not (normals always from ccw) then we always cull GL_BACK faces,
      and glFrontFace already did the work of specifying right which
      winding constitues front face. }
  if SH.FdVertexOrdering.Value = VERTORDER_CLOCKWISE then
   glCullFace(GL_FRONT);
  {$endif}
  glEnable(GL_CULL_FACE);
 end;
end;

procedure TIndexedFaces_Or_TrianglesRenderer.Render_Fini;
begin
 glPopAttrib;
 inherited;
end;

{ TIndexedFaceSetRenderer ---------------------------------------------------- }

procedure TIndexedFaceSetRenderer.Render_Vertex(indexNum: integer);
begin
 if Render_Normals = inGenerate then
  glNormalv(Render_GeneratedNormals.Items[indexNum]);
 inherited;
end;

procedure TIndexedFaceSetRenderer.Render_Init;
begin
 inherited;
 if Render_Normals = inGenerate then
 begin
  if Renderer.Attributes.SmoothShading then
   Render_GeneratedNormals := CreateNormals(
     Node.FdCoordIndex.Items,
     State.LastNodes.Coordinate3.FdPoint.Items,
     State.LastNodes.ShapeHints.FdCreaseAngle.Value,
     Render_NormalsFromCCW) else
   Render_GeneratedNormals := CreateFlatNormals(
     Node.FdCoordIndex.Items,
     State.LastNodes.Coordinate3.FdPoint.Items,
     Render_NormalsFromCCW);
 end;
end;

procedure TIndexedFaceSetRenderer.Render_Fini;
begin
 FreeAndNil(Render_GeneratedNormals);
 inherited;
end;

procedure Render_Triangulated(const TriIndices: TVector3Longint; Data: Pointer);
begin
 TIndexedFaceSetRenderer(Data).Render_Vertex(TriIndices[0]);
 TIndexedFaceSetRenderer(Data).Render_Vertex(TriIndices[1]);
 TIndexedFaceSetRenderer(Data).Render_Vertex(TriIndices[2]);
end;

procedure TIndexedFaceSetRenderer.Render_Face(startIndex, endIndex: integer);
begin
 if State.LastNodes.ShapeHints.FdFaceType.Value = FACETYPE_UNKNOWN then
 begin
  glBegin(GL_TRIANGLES);
  TriangulateFace(Addr(Node.FdCoordIndex.Items.Items[StartIndex]),
    EndIndex-StartIndex, State.LastNodes.Coordinate3.FdPoint.Items.Items,
    Render_Triangulated, Self, StartIndex);
  glEnd;
 end else
 begin
  { zamiast GL_POLYGON uzywamy GL_TRIANGLES i robimy cos jak GL_TRIANGLE_FAN
      uzywajac glEdgeFlag. De facto, to co my chcemy zrobic to jest
      wlasnie wykonac GL_TRIANGLE_FAN tyle ze w triangle fan glEdgeFlag
      jest ignorowane (jakby zawsze bylo GL_TRUE).
    Zamiast GL_POLYGON uzywamy GL_TRIANGLE_FAN. Dlaczego ?
      Bo chcemy miec 1-znaczna triangulacje kazdego polygonu. To ma
      znaczenie kiedy np. renderujemy prostokat z cieniowaniem smooth
      i kazdy z 4 vertexow ma inny kolor. Wtedy widac wzdluz ktorej
      przekatnej biegnie triangulacja trojkata. I np. jezeli kawalek
      kwadratu wyjezdza za ekran wtedy widac jak (na skutek przycinania
      polygonu do frustum) przesuwa sie linia triangulacji. Wychodzi
      bardzo dziwny efekt, bo widac ze kolory na prostokacie sie nieco
      zmieniaja w miare jak ten wyjezdza za ekran, a to zdecydowanie nie jest
      efekt na jakim nam zalezy. Rozwiazaniem jest ustawlic 1-znaczna
      triangulacje prostokata, wtedy linia podzialu zawsze bedzie taka
      sama i problemu nie bedzie.
    Jedyna rzecz jaka (poza 1-znacznym wyznaczeniem triangulacji) robi
      GL_TRIANGLE_FAN to dodawanie w srodku dodatkowych krawedzi.
      Mozna to zobaczyc gdy przelaczymy sie na widok wireframe.
      Nie mozna uzywac glEdgeFlag z GL_TRIANGLE_FAN, jest przez nie
      ignorowane. Wiec zeby obejsc ten problem trzeba recznie realizowac
      GL_TRIANGLE_FAN przez GL_TRIANGLES i tam uzywac glEdgeFlag.
      Zrobilem to - patrz VRMLNodes_old_funcs.txt, ale niestety
      okazalo sie to bardzo zwalniac rendering (nic dziwnego -
      - musialem przeciez podawac wiele razy te same vertexy). Wiec
      chwilowo tego nie uzywam. A to ze GL_TRIANGLES_FAN powoduje ze
      w widoku wireframe widoczne jest wiecej krawedzi ? No coz,
      to zaden problem, przeciez nikt mi nie kaze dokladnie renderowac
      obiektu wireframe. }

  glBegin(GL_TRIANGLE_FAN);
  while startIndex < endIndex do
  begin
   Render_Vertex(startIndex);
   Inc(startIndex);
  end;
  glEnd;
 end;
end;

{ TIndexedTriangleMeshRenderer ------------------------------------------------ }

procedure TIndexedTriangleMeshRenderer.Render_Face(startIndex, endIndex: integer);

  procedure triangle(v1, v2, v3: integer);
  var Tri: TTriangle3Single;
      Norm: TVector3Single;
  begin
   { TODO: for IndexedTriangleMesh we always generate flat normals }
   if Render_Normals = inGenerate then
   begin
    Tri[0] := Render_GetVertex(v1);
    Tri[1] := Render_GetVertex(v2);
    Tri[2] := Render_GetVertex(v3);
    Norm := TriangleNormal(Tri);
    if not Render_NormalsFromCCW then VectorNegateTo1st(Norm);
    glNormalv(Norm);
   end;

   Render_Vertex(v1);
   Render_Vertex(v2);
   Render_Vertex(v3);
  end;

var normalOrder: boolean;
begin
 {ponizej wykonujemy "recznie" GL_TRIANGLE_STRIP. Wszystko dlatego
  ze musimy punktom, ktore dla GL_TRIANGLE_STRIP bylyby tymi samymi,
  podac rozne wektory normalne.}
 glBegin(GL_TRIANGLES);

 normalOrder := true;
 while startIndex+2 < endIndex do
 begin
  if normalOrder then
   triangle(startIndex, startIndex+1, startIndex+2) else
   triangle(startIndex+1, startIndex, startIndex+2);
  Inc(startIndex);
  {musimy za kazdym razem zmieniac normalOrder zeby zachowac konsekwentne
   ulozenie trojkatow.}
  normalOrder := not normalOrder;
 end;

 glEnd;
end;

{ TIndexedLineSetRenderer ------------------------------------------------------- }

procedure TIndexedLineSetRenderer.Render_Init;
begin
 {VRML 1.0 specification states that lines normals aren't generated - instead,
  lines are drawn unlit when there are no normals specified.}
 inherited;
 if Render_Normals = inGenerate then
 begin
  glPushAttrib(GL_LIGHTING_BIT);
  glDisable(GL_LIGHTING);
 end;
end;

procedure TIndexedLineSetRenderer.Render_Fini;
begin
 if Render_Normals = inGenerate then
  glPopAttrib;
 inherited;
end;

procedure TIndexedLineSetRenderer.Render_Face(startIndex, endIndex: integer);
var i: integer;
begin
 glBegin(GL_LINE_STRIP);
 for i := startIndex to endIndex-1 do Render_Vertex(i);
 glEnd;
end;

{ eof --------------------------------------------------------------------------- }

