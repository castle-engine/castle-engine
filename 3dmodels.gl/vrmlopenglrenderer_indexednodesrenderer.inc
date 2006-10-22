{
  Copyright 2002-2006 Michalis Kamburelis.

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

  { This is class used as a base to render various indexed nodes:
    IndexedFaceSet (both for VRML 1.0 and 2.0), IndexedTriangleMesh,
    IndexedLineSet (both for VRML 1.0 and 2.0).

    What you have to do to implement a descendant ?

    @unorderedList(
      @item(
        You have to override Render_Face where you will render
        your face by calling Render_Vertex to render vertexes.
        It's your responsibility to call appropriate glBegin / glEnd
        around Render_Vertex calls.

        Render_Face must render all points between startIndex to endIndex - 1
        (StartIndex and EndIndex are used to index CoordIndex array).

        When implementing Render_Face you can safely assume
        that all indexes between
        startIndex to endIndex - 1 are <> -1, and they are surrounded
        by -1 indexes or by the end of CoordIndex array.
        In other words, Render_Face always gets indexes specifying
        a complete face in the sense of IndexedFaceSet and similar
        nodes.)

      @item(
        You can override Render_Init and Render_Fini,
        although you don't have to (but if you do --- remember
        to call inherited, as always).)

      @item(
        If and only if Render_Normals = inGenerate
        you have to make appropriate glNormal calls.
        Normals should always be oriented accoriding to
        Render_NormalsFromCCW.
        If Render_Normals <> inGenerate then we handle everything
        related to normals in this class.)

      @item(
        Do not call Renderer.Render_BindMaterial_1
        in descendants. Everything related to materials is already
        handled in this class.

        Actually, for VRML 2.0 you can do what you want with
        material, because Render_BindMaterial_1 calls are ignored
        and material should be set anyway in Render_MaterialsBegin.
        You can use SetColor for setting unlit color (this takes
        care of using Renderer.ColorModular).)

      @item(
        Do not generate any texture coordinates (call glTexCoord etc.)
        in descendants. Everything related to textures is already
        handled in this class.)
    ) }
  TGeneralIndexedRenderer = class
  private
    Render_UseTextureCoordIndex: boolean;
    Render_Materials: TRenderIndexedMaterials;
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer);

    { This is set in our constructor. }
    Renderer: TVRMLOpenGLRenderer;

    { This will be at the beginning of our Render, and will be available
      for Render_Init, Render_Face and other methods for descendants. }
    Render_Normals: TRenderIndexedNormals;

    { Set these in descendant's constructor. }
    CoordIndex: TMFLong;
    { If Coord = nil then nothing will be rendered. }
    Coord: TMFVec3f;
    { You can leave TexCoordIndex as nil only if you are
      sure that Renderer.Render_TexCoordsNeeded will be @false.
      One way to ensure this is to modify NodeTextured function. }
    TexCoordIndex: TMFLong;
    { You can leave TexCoord as nil.
      If Renderer.Render_TexCoordsNeeded will be @true then
      the default texture coordinates will be generated in this case. }
    TexCoord: TMFVec2f;
    { You can leave NormalIndex and Normal as nil.
      Then Render_Normals will for sure be inGenerate,
      and it's up to you how to make the normals.
      Also NormalBinding is ignored in this case. }
    NormalIndex: TMFLong;
    Normal: TMFVec3f;
    { You can leave MaterialIndex as nil if you are sure that
      MaterialBinding will not be any _INDEXED value. }
    MaterialIndex: TMFLong;
    MaterialBinding: Integer;
    NormalBinding: Integer;

    procedure Render_Vertex(indexNum: integer); virtual;
    function Render_GetVertex(indexNum: integer): TVector3f;
    procedure Render_Face(startIndex, endIndex: integer); virtual; abstract;

    procedure Render_Init; virtual;
    procedure Render_Fini; virtual;

    procedure Render;

    procedure SetColor(const Color: TVector3Single);
  end;

  TIndexedFaces_Or_TrianglesRenderer = class(TGeneralIndexedRenderer)
  public
    procedure Render_Init; override;
    procedure Render_Fini; override;

    { Set these in descendant's constructor. }
    { czy normale wychodza / maja wychodzic z CCW ?
      Ta zmienna odzwierciedla stan glFronFace jaki ustawilismy OpenGL'owi. }
    Render_NormalsFromCCW: boolean;
    CullBackFaces: boolean;
  end;

  TIndexedFaceSetRenderer = class(TIndexedFaces_Or_TrianglesRenderer)
  private
    Render_GeneratedNormals: TDynVector3SingleArray;
  public
    procedure Render_Init; override;
    procedure Render_Fini; override;
    procedure Render_Vertex(indexNum: integer); override;
    procedure Render_Face(startIndex, endIndex: integer); override;

    { Set these in descendant's constructor. }
    CreaseAngle: Single;
    FaceConvex: boolean;
  end;

  TIndexedFaceSet_1Renderer = class(TIndexedFaceSetRenderer)
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer;
      ANode: TNodeIndexedFaceSet_1);
  end;

  TIndexedFaceSet_2Renderer = class(TIndexedFaceSetRenderer)
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer;
      ANode: TNodeIndexedFaceSet_2);
  end;

  TIndexedTriangleMesh_1Renderer = class(TIndexedFaces_Or_TrianglesRenderer)
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer;
      ANode: TNodeIndexedTriangleMesh_1);
    procedure Render_Face(startIndex, endIndex: integer); override;
  end;

  TIndexedLineSetRenderer = class(TGeneralIndexedRenderer)
  public
    procedure Render_Init; override;
    procedure Render_Fini; override;
    procedure Render_Face(startIndex, endIndex: integer); override;
  end;

  TIndexedLineSet_1Renderer = class(TIndexedLineSetRenderer)
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer;
      ANode: TNodeIndexedLineSet_1);
  end;

  TIndexedLineSet_2Renderer = class(TIndexedLineSetRenderer)
  private
    Color: TMFColor;
  public
    procedure Render_Init; override;
    constructor Create(ARenderer: TVRMLOpenGLRenderer;
      ANode: TNodeIndexedLineSet_2);
  end;

{ TGeneralIndexedRenderer ----------------------------------------------------- }

constructor TGeneralIndexedRenderer.Create(ARenderer: TVRMLOpenGLRenderer);
begin
  inherited Create;
  Renderer := ARenderer;
end;

procedure TGeneralIndexedRenderer.Render_Vertex(indexNum: integer);
var
  VertexNum: integer;
begin
  VertexNum := CoordIndex.Items.Items[indexNum];

  if Render_UseTextureCoordIndex then
    glTexCoordv(TexCoord.Items.Items[TexCoordIndex.Items.Items[indexNum]]);

  case Render_Normals of
    inUseSpecifiedPerVertex:
      {don't do anything - glDrawElement will get the normal};
    inUseSpecifiedPerVertexIndexed: glNormalv(
      Normal.Items.Items[NormalIndex.Items.Items[indexNum]]);
  end;

  case Render_Materials of
    imUseSpecifiedPerVertex:
      Renderer.Render_BindMaterial_1(VertexNum);
    imUseSpecifiedPerVertexIndexed:
      Renderer.Render_BindMaterial_1(MaterialIndex.Items.Items[indexNum]);
  end;

  Renderer.DoGLArrayElement(Coord.Items.ItemsArray, VertexNum);
end;

function TGeneralIndexedRenderer.Render_GetVertex(indexNum: integer): TVector3f;
begin
  result := Coord.Items.Items[CoordIndex.Items.Items[indexNum]];
end;

procedure TGeneralIndexedRenderer.Render;

var
  { Used only by TextureBegin/End }
  TextureGenerated: boolean;

  { setup texture-related things }
  procedure TextureBegin;

    procedure GenerateDefaultTextureCoordinates;
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
        gen[3] := -localBBox[0, coord] * (genEnd-genStart) /
          localBBoxSize[coord] + genStart;
      end;

    var
      s_gen, t_gen: TVector4f;
      SCoord, TCoord, c1, c2: integer;
    begin
      localBBox := Renderer.Render_Node.LocalBoundingBox(
        Renderer.Render_State);

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
      TextureGenerated := true;
    end;

  begin
    Render_UseTextureCoordIndex := false;
    if Renderer.Render_TexCoordsNeeded then
    begin
      if TexCoord <> nil then
      begin
        if TexCoordIndex.Count >= CoordIndex.Count then
          Render_UseTextureCoordIndex := true else
        begin
          { If TexCoord <> nil but TexCoordIndex is empty then
            - VRML 2.0 spec says that coordIndex is used
              to index texture coordinates.
            - VRML 1.0 spec says that in this case default texture
              coordinates should be generated (that's because for
              VRML 1.0 there is always some TexCoord <> nil,
              so it cannot be used to produce different behavior). }
          if Renderer.Render_State.ParentShape <> nil then
          begin
            glTexCoordPointer(2, GL_FLOAT, 0, TexCoord.Items.Items);
            glEnableClientState(GL_TEXTURE_COORD_ARRAY);
          end else
            GenerateDefaultTextureCoordinates;
        end;
      end else
        GenerateDefaultTextureCoordinates;
    end;
  end;

  procedure TextureEnd;
  begin
    if TextureGenerated then
    begin
      glDisable(GL_TEXTURE_GEN_S);
      glDisable(GL_TEXTURE_GEN_T);
    end;
  end;

  function IndexListNotEmpty(MFIndexes: TMFLong): boolean;
  begin
   result:=(MFIndexes.Count > 0) and
           (not ((MFIndexes.Count = 1) and (MFIndexes.Items.Items[0] = -1)));
  end;

  procedure NormalsBegin;
  { setup Render_Normals variable }
  begin
   { TODO: not all normal bindings are supported,
     we Generate Normals if normalBinding is not supported.
     Nie zrobione : PER_PART, PER_PART_INDEXED, PER_FACE, PER_FACE_INDEXED }

   Render_Normals := inGenerate;

   if (Normal = nil) or (NormalIndex = nil) then
     Exit;

   case NormalBinding of
    BIND_DEFAULT, BIND_PER_VERTEX_INDEXED:
      if (NormalIndex.Count > 0) and (NormalIndex.Items.Items[0] >=0) then
       Render_Normals := inUseSpecifiedPerVertexIndexed;
    BIND_PER_VERTEX:
      Render_Normals := inUseSpecifiedPerVertex;
    BIND_OVERALL:
      if Normal.Count > 0 then
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
   if Normal.Count = 0 then
    Render_Normals := inGenerate;

   { podejmij pewna inicjalizacje juz PO ustaleniu Render_Normals }
   case Render_Normals of
    inOverall: glNormalv(Normal.Items.Items[0]);
    inUseSpecifiedPerVertex:
      begin
       {w tym przypadku mozemy uzyc vertex array dla normali bo
        indeks vertexu = indeks normala}
       glNormalPointer(GL_FLOAT, 0, Normal.Items.Items);
       glEnableClientState(GL_NORMAL_ARRAY);
      end;
   end;
  end;

  procedure NormalsEnd;
  begin
  end;

  procedure MaterialsBegin;
  { setup Render_Materials variable }
  begin
   Render_Materials := imOverall;
   case MaterialBinding of
    {BIND_OVERALL, BIND_DEFAULT: take default imOverall}
    BIND_PER_VERTEX: Render_Materials := imUseSpecifiedPerVertex;
    BIND_PER_VERTEX_INDEXED:
      if IndexListNotEmpty(MaterialIndex) then
       Render_Materials := imUseSpecifiedPerVertexIndexed;
    BIND_PER_PART, BIND_PER_FACE: Render_Materials := imUseSpecifiedPerFace;
    BIND_PER_PART_INDEXED, BIND_PER_FACE_INDEXED:
      if IndexListNotEmpty(MaterialIndex) then
       Render_Materials := imUseSpecifiedPerFaceIndexed;
   end;

   { TODO: we handle all material bindings, but we handle BIND_PER_PART
     and BIND_PER_PART_INDEXED wrong for IndexedLineSet. }

   if Render_Materials = imOverall then Renderer.Render_BindMaterial_1(0);
  end;

var
  { This is used only by LockArraysBegin and LockArraysEnd. }
  UseLockArrays: boolean;

  procedure LockArraysBegin;
  begin
    { See
      [http://www.opengl.org/documentation/specs/version1.2/EXTspecs/compiled_vertex_array.txt]
      for description of GL_EXT_compiled_vertex_array.

      Note that I can't use it when Coord has no items
      (because glLockArraysEXT(0, 0) causes OpenGL error "invalid value".) }
    UseLockArrays := GL_EXT_compiled_vertex_array and
      (Coord.Items.Count <> 0);
    if UseLockArrays then
      glLockArraysEXT(0, Coord.Items.Count);
  end;

  procedure LockArraysEnd;
  begin
    if UseLockArrays then
      glUnlockArraysEXT;
  end;

var
  startIndex, endIndex, faceNumber: integer;
begin
  if Coord = nil then
    Exit;

  {initialize most important thing we will use with indexed nodes :
   vertex arrays. Musimy to zainicjowac najpierw bo ponizej NormalsInit
   (a byc moze kiedys cos wiecej) moga tez zaladowac swoje vertex arrays. }
  glPushClientAttrib(GL_CLIENT_VERTEX_ARRAY_BIT);
  try
    glVertexPointer(3, GL_FLOAT, 0, Coord.Items.Items);
    glEnableClientState(GL_VERTEX_ARRAY);

    {initialize rendering}
    TextureBegin;
    MaterialsBegin;
    NormalsBegin;
    { Lock arrays after setting up all arrays. }
    LockArraysBegin;
    Render_Init;
    try

      {wlasciwe rendering : wywoluj Render_Face}
      startIndex := 0;
      faceNumber := 0;
      while startIndex < CoordIndex.Count do
      begin
        endIndex := startIndex;
        while (endIndex < CoordIndex.Count) and
              (CoordIndex.Items.Items[endIndex] >= 0) do
          Inc(endIndex);

        case Render_Materials of
          imUseSpecifiedPerFace:
            Renderer.Render_BindMaterial_1(faceNumber);
          imUseSpecifiedPerFaceIndexed:
            Renderer.Render_BindMaterial_1(MaterialIndex.Items.Items[faceNumber]);
        end;

        Render_Face(startIndex, endIndex);
        Inc(faceNumber);
        startIndex := endIndex+1;
      end;
    finally

      {finalize rendering}
      Render_Fini;
      LockArraysEnd;
      NormalsEnd;
      TextureEnd;
    end;
  finally glPopClientAttrib; end;
end;

{ Render_Init / Fini w tej klasie nie sa abstract zeby umozliwic zawsze
  wywolywanie inherited; w podklasach }
procedure TGeneralIndexedRenderer.Render_Init; begin end;
procedure TGeneralIndexedRenderer.Render_Fini; begin end;

procedure TGeneralIndexedRenderer.SetColor(const Color: TVector3Single);
begin
  glColorv(Renderer.Attributes.ColorModulated(Color));
end;

{ TIndexedFaces_Or_TrianglesRenderer -------------------------------------------- }

procedure TIndexedFaces_Or_TrianglesRenderer.Render_Init;
begin
  inherited;
  glPushAttrib(GL_POLYGON_BIT);

  { zmieniamy glFrontFace na GL_CW jesli VertexOrdering jest clockwise.
    W ten sposob mowimy OpenGL'owi ktorym faces robic cull o ile
    za chwile wlaczymy culling i mowimy tez z ktorej strony polygonow
    wychodza normale (my wprawdzie moglibysmy sobie generowac normale
    z dowolnej strony z ktorej chcemy ale normale w pliku VRML'a sa
    zapisane zgodnie z aktualna orientacja vertexOrdering / ccw). }
  if not Render_NormalsFromCCW then
    glFrontFace(GL_CW);

  { if vertex ordering is consistent and object is SOLID than we use OpenGL's
    backface culling. Warning : we should check viewer position to know
    whether to cull outside or inside polygons because if the viewer is inside
    the object we should cull outside polygons. But luckily for us (because
    it's extremely difficult to decide whether a point in space is
    inside or outside such solid object) VRML 1.0 specification states clearly
    that if an object is SOLID we can assume that the viewer is ALWAYS OUTSIDE
    OF IT. So we cull faces inside. }
  if CullBackFaces then
    glEnable(GL_CULL_FACE);
end;

procedure TIndexedFaces_Or_TrianglesRenderer.Render_Fini;
begin
  glPopAttrib;
  inherited;
end;

{ TIndexedFaceSetRenderer -------------------------------------------------- }

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
     CoordIndex.Items,
     Coord.Items,
     CreaseAngle,
     Render_NormalsFromCCW) else
   Render_GeneratedNormals := CreateFlatNormals(
     CoordIndex.Items,
     Coord.Items,
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
 if not FaceConvex then
 begin
  glBegin(GL_TRIANGLES);
  TriangulateFace(Addr(CoordIndex.Items.Items[StartIndex]),
    EndIndex-StartIndex, Coord.Items.ItemsArray,
    {$ifdef FPC_OBJFPC} @ {$endif} Render_Triangulated, Self, StartIndex);
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

{ TIndexedFaceSet_1Renderer -------------------------------------------------- }

constructor TIndexedFaceSet_1Renderer.Create(
  ARenderer: TVRMLOpenGLRenderer;
  ANode: TNodeIndexedFaceSet_1);
var
  SH: TNodeShapeHints;
begin
  inherited Create(ARenderer);

  CoordIndex := ANode.FdCoordIndex;
  Coord := Renderer.Render_State.LastNodes.Coordinate3.FdPoint;
  TexCoordIndex := ANode.FdTextureCoordIndex;
  TexCoord := Renderer.Render_State.LastNodes.TextureCoordinate2.FdPoint;
  NormalIndex := ANode.FdNormalIndex;
  Normal := Renderer.Render_State.LastNodes.Normal.FdVector;
  MaterialIndex := ANode.FdMaterialIndex;

  NormalBinding := Renderer.Render_State.LastNodes.NormalBinding.FdValue.Value;
  MaterialBinding := Renderer.Render_State.LastNodes.MaterialBinding.FdValue.Value;

  SH := Renderer.Render_State.LastNodes.ShapeHints;

  { W tym miejscu uznajemy VERTORDER_UNKNOWN_ORDERING za COUNTERCLOCKWISE
    (a autorzy VRMLi w ogole nie powinni podawac normali jesli
    nie podadza vertexOrdering innego niz UNKNOWN) }
  Render_NormalsFromCCW := SH.FdVertexOrdering.Value <> VERTORDER_CLOCKWISE;

  CullBackFaces :=
    (SH.FdVertexOrdering.Value <> VERTORDER_UNKNOWN) and
    (SH.FdShapeType.Value = SHTYPE_SOLID);

  CreaseAngle := SH.FdCreaseAngle.Value;
  FaceConvex := SH.FdFaceType.Value = FACETYPE_CONVEX;
end;

{ TIndexedFaceSet_2Renderer -------------------------------------------------- }

constructor TIndexedFaceSet_2Renderer.Create(
  ARenderer: TVRMLOpenGLRenderer;
  ANode: TNodeIndexedFaceSet_2);
begin
  inherited Create(ARenderer);

  CoordIndex := ANode.FdCoordIndex;
  if (ANode.FdCoord.Value <> nil) and
     (ANode.FdCoord.Value is TNodeCoordinate) then
    Coord := TNodeCoordinate(ANode.FdCoord.Value).FdPoint;
  TexCoordIndex := ANode.FdTexCoordIndex;
  if (ANode.FdTexCoord.Value <> nil) and
     (ANode.FdTexCoord.Value is TNodeTextureCoordinate) then
    TexCoord := TNodeTextureCoordinate(ANode.FdTexCoord.Value).FdPoint;
  NormalIndex := ANode.FdNormalIndex;
  if (ANode.FdNormal.Value <> nil) and
     (ANode.FdNormal.Value is TNodeNormal) then
    Normal := TNodeNormal(ANode.FdNormal.Value).FdVector;

  if ANode.FdNormalPerVertex.Value then
  begin
    if NormalIndex.Count > 0 then
      NormalBinding := BIND_PER_VERTEX_INDEXED else
      NormalBinding := BIND_PER_VERTEX;
  end else
  begin
    if NormalIndex.Count > 0 then
      NormalBinding := BIND_PER_FACE_INDEXED else
      NormalBinding := BIND_PER_FACE;
  end;

  MaterialBinding := BIND_DEFAULT;

  Render_NormalsFromCCW := ANode.FdCcw.Value;
  CullBackFaces := ANode.FdSolid.Value;
  CreaseAngle := ANode.FdCreaseAngle.Value;
  FaceConvex := ANode.FdConvex.Value;
end;

{ TIndexedTriangleMesh_1Renderer --------------------------------------------- }

constructor TIndexedTriangleMesh_1Renderer.Create(
  ARenderer: TVRMLOpenGLRenderer;
  ANode: TNodeIndexedTriangleMesh_1);
var
  SH: TNodeShapeHints;
begin
  inherited Create(ARenderer);

  CoordIndex := ANode.FdCoordIndex;
  Coord := Renderer.Render_State.LastNodes.Coordinate3.FdPoint;
  TexCoordIndex := ANode.FdTextureCoordIndex;
  TexCoord := Renderer.Render_State.LastNodes.TextureCoordinate2.FdPoint;
  NormalIndex := ANode.FdNormalIndex;
  Normal := Renderer.Render_State.LastNodes.Normal.FdVector;
  MaterialIndex := ANode.FdMaterialIndex;

  NormalBinding := Renderer.Render_State.LastNodes.NormalBinding.FdValue.Value;
  MaterialBinding := Renderer.Render_State.LastNodes.MaterialBinding.FdValue.Value;

  SH := Renderer.Render_State.LastNodes.ShapeHints;

  { W tym miejscu uznajemy VERTORDER_UNKNOWN_ORDERING za COUNTERCLOCKWISE
    (a autorzy VRMLi w ogole nie powinni podawac normali jesli
    nie podadza vertexOrdering innego niz UNKNOWN) }
  Render_NormalsFromCCW := SH.FdVertexOrdering.Value <> VERTORDER_CLOCKWISE;

  CullBackFaces :=
    (SH.FdVertexOrdering.Value <> VERTORDER_UNKNOWN) and
    (SH.FdShapeType.Value = SHTYPE_SOLID);
end;

procedure TIndexedTriangleMesh_1Renderer.Render_Face(startIndex, endIndex: integer);

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

{ TIndexedLineSetRenderer -------------------------------------------------- }

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

{ TIndexedLineSet_1Renderer -------------------------------------------------- }

constructor TIndexedLineSet_1Renderer.Create(
  ARenderer: TVRMLOpenGLRenderer;
  ANode: TNodeIndexedLineSet_1);
begin
  inherited Create(ARenderer);

  CoordIndex := ANode.FdCoordIndex;
  Coord := Renderer.Render_State.LastNodes.Coordinate3.FdPoint;
  TexCoordIndex := ANode.FdTextureCoordIndex;
  TexCoord := Renderer.Render_State.LastNodes.TextureCoordinate2.FdPoint;
  NormalIndex := ANode.FdNormalIndex;
  Normal := Renderer.Render_State.LastNodes.Normal.FdVector;
  MaterialIndex := ANode.FdMaterialIndex;

  NormalBinding := Renderer.Render_State.LastNodes.NormalBinding.FdValue.Value;
  MaterialBinding := Renderer.Render_State.LastNodes.MaterialBinding.FdValue.Value;
end;

{ TIndexedLineSet_2Renderer -------------------------------------------------- }

procedure TIndexedLineSet_2Renderer.Render_Init;
var
  Material: TNodeMaterial_2;
begin
  inherited;

  if Color = nil then
  begin
    Material := Renderer.Render_State.ParentShape.Material;
    if Material <> nil then
      SetColor(Material.FdEmissiveColor.Value) else
      SetColor(White3Single);
  end;
end;

constructor TIndexedLineSet_2Renderer.Create(
  ARenderer: TVRMLOpenGLRenderer;
  ANode: TNodeIndexedLineSet_2);
begin
  inherited Create(ARenderer);

  CoordIndex := ANode.FdCoordIndex;
  if (ANode.FdCoord.Value <> nil) and
     (ANode.FdCoord.Value is TNodeCoordinate) then
    Coord := TNodeCoordinate(ANode.FdCoord.Value).FdPoint;

  MaterialBinding := BIND_DEFAULT;

  { IndexedLineSet color may come from various places:
    1. Color node, for each vertex or polyline, if it's not NULL
    2. Material.emissiveColor, for whole line
    3. If no material, we use default White3Single, for whole line
       (following general spec remark at Material node that
       Material = NULL makes unlit white color) }
  if (ANode.FdColor.Value <> nil) and
     (ANode.FdColor.Value is TNodeColor) then
    Color := TNodeColor(ANode.FdColor.Value).FdColor;
end;

{ eof --------------------------------------------------------------------------- }
