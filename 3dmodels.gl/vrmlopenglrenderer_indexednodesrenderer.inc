{
  Copyright 2002-2008 Michalis Kamburelis.

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

type
  TRenderIndexedNormals = (inOverall, inGenerate, inUseSpecifiedPerVertex,
    inUseSpecifiedPerVertexIndexed);
  TRenderIndexedMaterials = (imOverall,
    imUseSpecifiedPerVertex, imUseSpecifiedPerVertexIndexed,
    imUseSpecifiedPerFace, imUseSpecifiedPerFaceIndexed);

  { This is class used as a base to render various indexed nodes:
    IndexedFaceSet (both for VRML 1.0 and 2.0), IndexedTriangleMesh,
    IndexedLineSet (both for VRML 1.0 and 2.0).

    This is a descendant of TAbstractCoordinateRenderer
    that must always have CoordIndex assigned (it's always indexed).

    What you have to do to implement a descendant ?

    @unorderedList(
      @item(
        You have to override Render_Face where you will render
        your face by calling RenderVertex to render vertexes.
        It's your responsibility to call appropriate glBegin / glEnd
        around RenderVertex calls.

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
        Normals should always point from CCW (you *do not* check here FrontFaceCcw
        field, as we *do not* call glFrontFace anywhere).

        If Render_Normals <> inGenerate then we handle everything
        related to normals in this class.)

      @item(
        Do not call Renderer.Render_BindMaterial_1
        in descendants. Everything related to materials is already
        handled in this class.

        Actually, for VRML 2.0 you can do what you want with
        material, because Render_BindMaterial_1 calls are ignored
        and material should be set anyway in Render_MaterialsBegin.
        See SetColor and SetLitColor methods for example how to set
        material/color in the middle of rendering, for VRML 2.0 shapes.)

      @item(
        Do not generate any texture coordinates (call glTexCoord etc.)
        in descendants. Everything related to textures is already
        handled in this class.)
    ) }
  TGeneralIndexedRenderer = class(TAbstractCoordinateRenderer)
  private
    Render_UseTextureCoordIndex: boolean;
    Render_Materials: TRenderIndexedMaterials;

    { Return texture coordinate for given vertex, identified by IndexNum
      (IndexNum has the same meaning as for RenderVertex).

      Returns @false if no texture coords are available.

      This tries to work in all cases when we actually render some texture,
      but in practice doesn't work (yet) in all cases: when texture coords
      are generated, it returns @false. (that's because we used OpenGL to
      generate them). }
    function GetTextureCoord(IndexNum: integer; out Tex: TVector2Single): boolean;

    { Pretty much analogous to GetTextureCoord. Returns normal always from
      CCW (just like we pass to OpenGL always CCW normals, since we always
      assume front face = CCW).

      Override this in descendants only to handle
      Render_Normals = inGenerate case. }
    procedure GetNormal(IndexNum: integer; out N: TVector3Single); virtual;

    { Used only by TextureBegin/End }
    TextureGenerated: boolean;
  protected
    procedure RenderCoordinate; override;
    procedure RenderCoordinateBegin; override;
    procedure RenderCoordinateEnd; override;
  public
    { Constructor initializes various fields of this class.
      Don't do any OpenGL state changes here (initialization of OpenGL state
      should go to Render_Init). }
    constructor Create(ARenderer: TVRMLOpenGLRenderer);

    { This will be set by CalculateRender_Normals.

      CalculateRender_Normals must be called after constructor but before
      Render, so Render_Normals be already available for Render_Init,
      Render_Face etc. }
    Render_Normals: TRenderIndexedNormals;
    procedure CalculateRender_Normals;

    { Only if Render_Normals = inGenerate, you can set this in descendants
      in Render_Init (after inherited).
      If @true, then it's guaranteed that normals for the same face will
      be equal --- this may be useful for various optimization purposes. }
    NormalsFlat: boolean;

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
    StoredNormal: TMFVec3f;
    { You can leave MaterialIndex as nil if you are sure that
      MaterialBinding will not be any _INDEXED value. }
    MaterialIndex: TMFLong;
    MaterialBinding: Integer;
    NormalBinding: Integer;

    { Will be set to StoredNormal or it's inverted version, to keep
      pointing from CCW. }
    CcwNormal: TDynVector3SingleArray;

    { This will actually be modified and used
      by TIndexedFaces_Or_TrianglesRenderer, see there for comments.
      For other classes, this should't be important.
      Yes, it's a little unclean to declare it here. }
    FrontFaceCcw: boolean;

    procedure RenderVertex(indexNum: integer); override;
    procedure Render_Face(const AFaceNumber: Cardinal;
      startIndex, endIndex: integer); virtual; abstract;

    procedure Render_Init; virtual;
    procedure Render_Fini; virtual;
  end;

  TIndexedFaces_Or_TrianglesRenderer = class(TGeneralIndexedRenderer)
  public
    procedure Render_Init; override;
    procedure Render_Fini; override;

    { Set these in descendant's constructor. }
    { FrontFaceCcw declared in TGeneralIndexedRenderer, but documented here:

      FrontFaceCcw says from which face side normals in VRML file point out,
      and which faces should be culled if CullBackFaces.

      Note that we *do not* implement these by glFrontFace, we do a little
      more complicated trick, see comments at the beginning of VRMLOpenGLRenderer
      for explanation (hint: plane mirrors). }

    CullBackFaces: boolean;
  end;

  TIndexedFaceSetRenderer = class(TIndexedFaces_Or_TrianglesRenderer)
  private
    Render_GeneratedNormals: TDynVector3SingleArray;

    procedure GetNormal(IndexNum: integer; out N: TVector3Single); override;
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer);

    procedure Render_Init; override;
    procedure Render_Fini; override;
    procedure RenderVertex(indexNum: integer); override;
    procedure Render_Face(const AFaceNumber: Cardinal;
      startIndex, endIndex: integer); override;

    { Set these in descendant's constructor. }
    CreaseAngle: Single;
    FaceConvex: boolean;

    { These must be set by Renderer if BumpMappingMethod <> bmNone }
    TexOriginal, TexNormalMap, TexHeightMap, TexNormalizationCube: TGLuint;
    TexOriginalAlpha: boolean;
    TexHeightMapScale: Single;

    { Helpers for bump mapping }
    HasTangentVectors: boolean;
    STangent, TTangent: TVector3Single;
    LightPositionObjectSpace: TVector3Single;
  end;

  TIndexedFaceSet_1Renderer = class(TIndexedFaceSetRenderer)
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer;
      ANode: TNodeIndexedFaceSet_1);
  end;

  TIndexedFaceSet_2Renderer = class(TIndexedFaceSetRenderer)
  private
    Color: TMFVec3f;
    ColorPerVertex: boolean;
    ColorIndex: TMFLong;
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer;
      ANode: TNodeIndexedFaceSet_2);

    procedure RenderVertex(indexNum: integer); override;
    procedure Render_Face(const AFaceNumber: Cardinal;
      startIndex, endIndex: integer); override;

    { Sets both glColor(...) and
      glMaterial(GL_FRONT_AND_BACK, GL_DIFFUSE, ...) colors.

      This is good for VRML 2.0 IndexedFaceSet that uses colors to either
      set precalculated color (when unlit, i.e. material is NULL)
      or to change diffuseColor (when lit, see specification "lighting equaltions",
      term O_Drgb). }
    procedure SetLitColor(const AColor: TVector3Single);
  end;

  TIndexedTriangleMesh_1Renderer = class(TIndexedFaces_Or_TrianglesRenderer)
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer;
      ANode: TNodeIndexedTriangleMesh_1);
    procedure Render_Face(const AFaceNumber: Cardinal;
      startIndex, endIndex: integer); override;
  end;

  TIndexedLineSetRenderer = class(TGeneralIndexedRenderer)
  public
    procedure Render_Face(const AFaceNumber: Cardinal;
      startIndex, endIndex: integer); override;
  end;

  TIndexedLineSet_1Renderer = class(TIndexedLineSetRenderer)
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer;
      ANode: TNodeIndexedLineSet_1);
  end;

  TIndexedLineSet_2Renderer = class(TIndexedLineSetRenderer)
  private
    Color: TMFVec3f;
    ColorPerVertex: boolean;
    ColorIndex: TMFLong;
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer;
      ANode: TNodeIndexedLineSet_2);

    { Sets glColor(...). This is good for VRML 2.0 IndexedLineSet that
      is always unlit (so no point in changing material color). }
    procedure SetColor(const AColor: TVector3Single);

    procedure Render_Init; override;
    procedure RenderVertex(indexNum: integer); override;
    procedure Render_Face(const AFaceNumber: Cardinal;
      startIndex, endIndex: integer); override;
  end;

{ TGeneralIndexedRenderer ----------------------------------------------------- }

constructor TGeneralIndexedRenderer.Create(ARenderer: TVRMLOpenGLRenderer);
begin
  inherited;
end;

function TGeneralIndexedRenderer.GetTextureCoord(
  IndexNum: integer; out Tex: TVector2Single): boolean;
begin
  { The logic of this is synchronized with TextureBegin and
    RenderVertex logic. Ideally, this should calculate the
    same texture coordinate that is effectively used by OpenGL. }
  if Render_UseTextureCoordIndex then
  begin
    Result := true;
    Tex := TexCoord.Items.Items[TexCoordIndex.Items.Items[IndexNum]];
  end else
  if (Renderer.Render_TexCoordsNeeded) and
     (TexCoord <> nil) and
     (TexCoordIndex.Count < CoordIndex.Count) and
     (State.ParentShape <> nil) then
  begin
    Result := true;
    { TextureBegin sets in this case
        glTexCoordPointer(2, GL_FLOAT, 0, TexCoord.Items.Items);
      which means that CoordIndex will be used just like TexCoordIndex. }
    Tex := TexCoord.Items.Items[CoordIndex.Items.Items[IndexNum]];
  end else
    { TODO: we fail to handle cases when texture coord in automatically
      generated, see cases when GenerateDefaultTextureCoordinates is called by
      TextureBegin. For now, we return @false for them --- instead, we should
      calculate and return texture coordinates ourself. }
    Result := false;
end;

procedure TGeneralIndexedRenderer.GetNormal(
  IndexNum: integer; out N: TVector3Single);
begin
  case Render_Normals of
    inOverall: N := CcwNormal.Items[0];
    inUseSpecifiedPerVertex:
      N := CcwNormal.Items[CoordIndex.Items.Items[IndexNum]];
    inUseSpecifiedPerVertexIndexed:
      N := CcwNormal.Items[NormalIndex.Items.Items[indexNum]];
    else
      raise EInternalError.Create('GetNormal: Render_Normals ?');
  end;
end;

procedure TGeneralIndexedRenderer.RenderVertex(indexNum: integer);
var
  VertexNum: integer;
begin
  VertexNum := CoordIndex.Items.Items[indexNum];

  { Don't pass glTexCoordv here if BumpMappingMethod <> bmNone,
    then we'll handle passing texture coords appropriately
    ourselves. }
  if Render_UseTextureCoordIndex and (BumpMappingMethod = bmNone) then
  begin
    { We could use GetTextureCoord(IndexNum) here,
      but since we already checked Render_UseTextureCoordIndex,
      we do it more directly. But the result is the same as if we had used
      GetTextureCoord. }
    glTexCoordv(TexCoord.Items.Items[TexCoordIndex.Items.Items[indexNum]]);
  end;

  { If case of other Render_Normals, this will be handled elsewhere:
    - inOverall is handled in NormalsBegin
    - inGenerate is handled in RenderVertex/Face as appropriate in descendants
    - inUseSpecifiedPerVertex is also set up in NormalsBegin,
      so glDrawElement will magically do this. }
  if Render_Normals = inUseSpecifiedPerVertexIndexed then
  begin
    { We could use GetNormal(IndexNum) here, but since we already checked
      Render_Normals, we do it more directly. But the result is the same
      as if we had used GetNormal. }
    glNormalv(CcwNormal.Items[NormalIndex.Items.Items[indexNum]]);
  end;

  case Render_Materials of
    imUseSpecifiedPerVertex:
      Renderer.Render_BindMaterial_1(VertexNum);
    imUseSpecifiedPerVertexIndexed:
      Renderer.Render_BindMaterial_1(MaterialIndex.Items.Items[indexNum]);
  end;

  inherited;
end;

procedure TGeneralIndexedRenderer.CalculateRender_Normals;
begin
  { TODO: not all normal bindings are supported,
    we Generate Normals if normalBinding is not supported.
    Nie zrobione : PER_PART, PER_PART_INDEXED, PER_FACE, PER_FACE_INDEXED }

  Render_Normals := inGenerate;
  NormalsFlat := false;

  if (StoredNormal = nil) or (NormalIndex = nil) then
    Exit;

  case NormalBinding of
    BIND_DEFAULT, BIND_PER_VERTEX_INDEXED:
      if (NormalIndex.Count > 0) and (NormalIndex.Items.Items[0] >=0) then
      begin
        Render_Normals := inUseSpecifiedPerVertexIndexed;
        NormalsFlat := false;
      end;
    BIND_PER_VERTEX:
      begin
        Render_Normals := inUseSpecifiedPerVertex;
        NormalsFlat := false;
      end;
    BIND_OVERALL:
      if StoredNormal.Count > 0 then
      begin
        Render_Normals := inOverall;
        NormalsFlat := true;
      end;
  end;

  { ponizsza instrukcja ma taki efekt : jezeli nie bylo zadnych normali w ostatnim
    node Normal (albo wrecz nie bylo zadnego takiego node'a w pliku, czyli
    LastNormal to DefaultNormalNode sceny) to bedziemy generowac normale.

    Jezeli juz bylismy ustawieni na generowanie normali albo jezeli to jest
    pusty indexed node (tzn. nie podaje zadnych faces - co jest przeciez
    dopuszczalne) to ta instrukcja nie spowoduje zadnego efektu (a wiec
    tez niczemu nie zaszkodzi). Wpp. (jezeli nie mielismy generowac
    normali i node niepusty) ta instrukcja sprawi ze taki niepoprawny VRML
    zostanie mimo wszystko dobrze odczytany i wyswietlony. }
  if StoredNormal.Count = 0 then
  begin
    Render_Normals := inGenerate;
    NormalsFlat := false;
  end;
end;

procedure TGeneralIndexedRenderer.RenderCoordinateBegin;

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
      localBBox := Geometry.LocalBoundingBox(State);

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
    TextureGenerated := false;
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
          if State.ParentShape <> nil then
          begin
            { When BumpMappingMethod <> bmNone, texture coords will be
              explicitly taken at each vertex using GetTextureCoord.
              This glTexCoordPointer would only collide with this. }
            if BumpMappingMethod = bmNone then
            begin
              glTexCoordPointer(2, GL_FLOAT, 0, TexCoord.Items.Items);
              glEnableClientState(GL_TEXTURE_COORD_ARRAY);
            end;
          end else
            GenerateDefaultTextureCoordinates;
        end;
      end else
        GenerateDefaultTextureCoordinates;
    end;
  end;

  function IndexListNotEmpty(MFIndexes: TMFLong): boolean;
  begin
   result:=(MFIndexes.Count > 0) and
           (not ((MFIndexes.Count = 1) and (MFIndexes.Items.Items[0] = -1)));
  end;

  procedure NormalsBegin;
  begin
    if StoredNormal <> nil then
    begin
      if FrontFaceCcw then
        CcwNormal := StoredNormal.Items else
      begin
        CcwNormal := TDynVector3SingleArray.Create;
        CcwNormal.AssignNegated(StoredNormal.Items);
      end;
    end;

   { podejmij pewna inicjalizacje juz PO ustaleniu Render_Normals }
   case Render_Normals of
    inOverall: glNormalv(CcwNormal.Items[0]);
    inUseSpecifiedPerVertex:
      begin
       {w tym przypadku mozemy uzyc vertex array dla normali bo
        indeks vertexu = indeks normala}
       glNormalPointer(GL_FLOAT, 0, CcwNormal.Items);
       glEnableClientState(GL_NORMAL_ARRAY);
      end;
   end;
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

   if GLVersion.IsMesa and (GLVersion.MesaMajor < 6) and
     (Render_Materials in
       [imUseSpecifiedPerVertex, imUseSpecifiedPerVertexIndexed]) then
   begin
     { With older Mesa (confirmed that the bug is with 5.1 and is not with
       6.4.2) when you use BindMaterial_1 inside
       TGeneralIndexedRenderer.RenderVertex it causes later
       glPopAttrib (in RenderEnd) to crash with SIGSEGV.
       gdb backtrace starts with

       (gdb) bt
       #0  0xa7a1639c in _mesa_PopAttrib ()
          from /home/michal/installed/mesa/5.1/lib/libGL.so.1
       #1  0x081e1418 in TVRMLOPENGLRENDERER__RENDEREND (this=0xa71af3c0)
           at VRMLOpenGLRenderer.pas:1905
       #2  0x081445f3 in TVRMLFLATSCENEGL__RENDERENDSIMPLE (this=0xa70b6060)
           at VRMLFlatSceneGL.pas:1073
       #3  0x08144b32 in TVRMLFLATSCENEGL__SSSX_PREPAREEND (this=0xa70b6060)
           at VRMLFlatSceneGL.pas:1229

       Easy test for it: run `view3dscene materials.wrl' with
       materials.wrl from VRML 1.0 demos from
       [http://vrmlengine.sourceforge.net/kambi_vrml_test_suite.php].

       So I'm workaround this below by just using imOverall for old Mesa
       versions. This causes incorrect rendering result, but at least
       it works and often looks "sensible".
     }
     Render_Materials := imOverall;
   end;

   { TODO: we handle all material bindings, but we handle BIND_PER_PART
     and BIND_PER_PART_INDEXED wrong for IndexedLineSet. }

   if Render_Materials = imOverall then Renderer.Render_BindMaterial_1(0);
  end;

begin
  TextureBegin;
  MaterialsBegin;
  NormalsBegin;
end;

procedure TGeneralIndexedRenderer.RenderCoordinateEnd;

  procedure TextureEnd;
  begin
    if TextureGenerated then
    begin
      glDisable(GL_TEXTURE_GEN_S);
      glDisable(GL_TEXTURE_GEN_T);
    end;
  end;

  procedure NormalsEnd;
  begin
    if (StoredNormal <> nil) and (not FrontFaceCcw) then
      FreeAndNil(CcwNormal);
  end;

begin
  NormalsEnd;
  TextureEnd;
end;

procedure TGeneralIndexedRenderer.RenderCoordinate;
var
  startIndex, endIndex: integer;
  FaceNumber: Cardinal;
begin
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

      Render_Face(FaceNumber, startIndex, endIndex);
      Inc(faceNumber);
      startIndex := endIndex+1;
    end;
  finally
    Render_Fini;
  end;
end;

{ Render_Init / Fini w tej klasie nie sa abstract zeby umozliwic zawsze
  wywolywanie inherited; w podklasach }
procedure TGeneralIndexedRenderer.Render_Init; begin end;
procedure TGeneralIndexedRenderer.Render_Fini; begin end;

{ TIndexedFaces_Or_TrianglesRenderer -------------------------------------------- }

procedure TIndexedFaces_Or_TrianglesRenderer.Render_Init;
begin
  inherited;
  glPushAttrib(GL_POLYGON_BIT);

  { If vertex ordering is consistent and object is SOLID than we use OpenGL's
    backface culling.

    In theory, we should check viewer position to know
    whether to cull outside or inside polygons because if the viewer is inside
    the object we should cull outside polygons. But luckily for us (because
    it's extremely difficult to decide whether a point in space is
    inside or outside such solid object) VRML 1.0 specification states clearly
    that if an object is SOLID we can assume that the viewer is ALWAYS OUTSIDE
    OF IT. So we cull faces inside.

    If FrontFaceCcw then we have to cull CW faces.
    But note that we do not want to call OpenGL glFrontFace
    (see VRMLOpenGLRenderer comments for reasons), so instead of it we switch
    glCullFace. Since we assume that front = always CCW, so we know how to call
    glCullFace.
  }
  if CullBackFaces then
  begin
    glEnable(GL_CULL_FACE);
    if FrontFaceCcw then
      glCullFace(GL_BACK) else
      glCullFace(GL_FRONT);
  end;
end;

procedure TIndexedFaces_Or_TrianglesRenderer.Render_Fini;
begin
  glPopAttrib;
  inherited;
end;

{ TIndexedFaceSetRenderer -------------------------------------------------- }

constructor TIndexedFaceSetRenderer.Create(ARenderer: TVRMLOpenGLRenderer);
begin
  inherited;
  BumpMappingAllowed := true;
end;

procedure TIndexedFaceSetRenderer.GetNormal(
  IndexNum: integer; out N: TVector3Single);
begin
  if Render_Normals = inGenerate then
    N := Render_GeneratedNormals.Items[IndexNum] else
    inherited;
end;

procedure TIndexedFaceSetRenderer.RenderVertex(indexNum: integer);

  procedure DoBumpMapping;
  var
    Normal, Vertex: TVector3Single;

    function CalculateLightDirTangent: TVector3Single;

      function LightDirectionInTangentSpace(
        const Vertex, Normal, STangent, TTangent: TVector3Single): TVector3Single;
      var
        ToTangent: TMatrix3Single;
        I: Integer;
      begin
        for I := 0 to 2 do
        begin
          ToTangent[I, 0] := STangent[I];
          ToTangent[I, 1] := TTangent[I];
          ToTangent[I, 2] := Normal[I];
        end;

        Result := MultMatrixVector(ToTangent,
          VectorSubtract(LightPositionObjectSpace, Vertex));
      end;

    var
      LocalSTangent, LocalTTangent: TVector3Single;
    begin
      { calculate LightDirTangent }
      if HasTangentVectors and
        (Abs(VectorDotProduct(STangent, Normal)) < 0.95) and
        (Abs(VectorDotProduct(TTangent, Normal)) < 0.95) then
      begin
        if NormalsFlat then
        begin
          Result := LightDirectionInTangentSpace(
            Vertex, Normal, STangent, TTangent);
        end else
        begin
          { If not NormalsFlat, you want to calculate local STangent and TTangent,
            I mean STangent and TTangent adjusted to current vertex (since each
            vertex may have different normal on the face when not NormalsFlat).

            Without doing this, you would see strange artefacts, smoothed
            faces would look somewhat like flat faces. Concenptually, for smoothed
            faces, whole tangent space should vary for each vertex, so Normal,
            and both tangents may be different on each vertex. }

          LocalSTangent := STangent;
          MakeVectorsOrthoOnTheirPlane(LocalSTangent, Normal);

          LocalTTangent := TTangent;
          MakeVectorsOrthoOnTheirPlane(LocalTTangent, Normal);

          Result := LightDirectionInTangentSpace(
            Vertex, Normal, LocalSTangent, LocalTTangent);
        end;
      end else
        Result := Vector3Single(0, 0, 1);
    end;

    procedure SetBumpMappingAttribs;

      procedure SetMatrixToTangent(const Normal, STangent, TTangent: TVector3Single);
      var
        ToTangent: TMatrix3Single;
        I: Integer;
      begin
        for I := 0 to 2 do
        begin
          ToTangent[I, 0] := STangent[I];
          ToTangent[I, 1] := TTangent[I];
          ToTangent[I, 2] := Normal[I];
        end;

        Renderer.BmGLSLAttribObjectSpaceToTangent[
          BumpMappingMethod >= bmGLSLParallax].SetValue(ToTangent);
      end;

    var
      LocalSTangent, LocalTTangent: TVector3Single;
    begin
      { This somewhat duplicates CalculateLightDirTangent logic,
        but instead of calling LightDirectionInTangentSpace this must
        call SetMatrixToTangent. }
      if HasTangentVectors and
        (Abs(VectorDotProduct(STangent, Normal)) < 0.95) and
        (Abs(VectorDotProduct(TTangent, Normal)) < 0.95) then
      begin
        if NormalsFlat then
        begin
          SetMatrixToTangent(Normal, STangent, TTangent);
        end else
        begin
          { If not NormalsFlat, you want to calculate local STangent and TTangent,
            I mean STangent and TTangent adjusted to current vertex (since each
            vertex may have different normal on the face when not NormalsFlat).

            Without doing this, you would see strange artefacts, smoothed
            faces would look somewhat like flat faces. Concenptually, for smoothed
            faces, whole tangent space should vary for each vertex, so Normal,
            and both tangents may be different on each vertex. }

          LocalSTangent := STangent;
          MakeVectorsOrthoOnTheirPlane(LocalSTangent, Normal);

          LocalTTangent := TTangent;
          MakeVectorsOrthoOnTheirPlane(LocalTTangent, Normal);

          SetMatrixToTangent(Normal, LocalSTangent, LocalTTangent);
        end;
      end else
      begin
        SetMatrixToTangent(Normal,
          { would be more correct to set LocalSTangent as anything perpendicular
            to Normal, and LocalTTangent as vector product (normal, LocalSTangent) }
          Vector3Single(1, 0, 0), Vector3Single(0, 1, 0));
      end;
    end;

  var
    LightDirTangent: TVector3Single;
    Tex: TVector2Single;
  begin
    GetNormal(IndexNum, Normal);

    { This is OK, we want Vertex in object space here. }
    Vertex := Coord.Items.ItemsArray^[CoordIndex.Items.Items[indexNum]];

    Check(GetTextureCoord(IndexNum, Tex), 'TexCoord for BumpMapping');

    { set all textures coordinates }
    case BumpMappingMethod of
      bmMultiTexDotNotNormalized:
        begin
          LightDirTangent := CalculateLightDirTangent;
          NormalizeTo1st(LightDirTangent);

          { pass LightDirTangent as RGB color }
          glColor3f((LightDirTangent[0] + 1) / 2,
                    (LightDirTangent[1] + 1) / 2,
                    (LightDirTangent[2] + 1) / 2);

          glMultiTexCoord2fARB(GL_TEXTURE0_ARB, Tex[0], Tex[1]);
          glMultiTexCoord2fARB(GL_TEXTURE1_ARB, Tex[0], Tex[1]);
        end;

      bmMultiTexDotNormalized:
        begin
          LightDirTangent := CalculateLightDirTangent;
          glMultiTexCoord3fARB(GL_TEXTURE0_ARB,
            LightDirTangent[0],
            LightDirTangent[1],
            LightDirTangent[2]);

          glMultiTexCoord2fARB(GL_TEXTURE1_ARB, Tex[0], Tex[1]);
          glMultiTexCoord2fARB(GL_TEXTURE2_ARB, Tex[0], Tex[1]);
        end;

      bmGLSLNormal,
      bmGLSLParallax:
        begin
          SetBumpMappingAttribs;
          glMultiTexCoord2fARB(GL_TEXTURE0_ARB, Tex[0], Tex[1]);
          { No need to pass texture coords for 2nd texture, GL_TEXTURE1_ARB.
            Shader knows that they are the same, and will just use gl_TexCoord[0]
            to access both textures. }
        end;
      else raise EInternalError.Create('BumpMappingMethod ?');
    end;
  end;

begin
  { For Render_Normals = inGenerate and NormalsFlat, we'll handle this
    in Render_Face. }
  if (Render_Normals = inGenerate) and (not NormalsFlat) then
    glNormalv(Render_GeneratedNormals.Items[indexNum]);

  if BumpMappingMethod <> bmNone then
    DoBumpMapping;

  inherited;
end;

procedure TIndexedFaceSetRenderer.Render_Init;

  { pass alpha from primary_color (this is actual material Opacity) *
    texture alpha }
  procedure glTexEnvAlphaFromMaterialAndTexture;
  begin
    if TexOriginalAlpha then
    begin
      glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_ALPHA_EXT, GL_MODULATE);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_ALPHA_EXT, GL_PRIMARY_COLOR_EXT);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_ALPHA_EXT, GL_SRC_ALPHA);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_ALPHA_EXT, GL_TEXTURE);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND1_ALPHA_EXT, GL_SRC_ALPHA);
    end else
    begin
      glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_ALPHA_EXT, GL_REPLACE);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_ALPHA_EXT, GL_PRIMARY_COLOR_EXT);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_ALPHA_EXT, GL_SRC_ALPHA);
    end;
  end;

  procedure EnableForMultiTexDotNotNormalized;
  begin
    Renderer.ActiveTexture(0);
    glBindTexture(GL_TEXTURE_2D, TexNormalMap);
    glEnable(GL_TEXTURE_2D);
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_EXT);

    { texture unit 0: make dot between PRIMARY_COLOR and
      current texture (normal map). IOW, calculate diffuse factor. }
    glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB_EXT, GL_DOT3_RGB_ARB);
    glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_EXT, GL_PRIMARY_COLOR_EXT);
    glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB_EXT, GL_SRC_COLOR);
    glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_RGB_EXT, GL_TEXTURE);
    glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND1_RGB_EXT, GL_SRC_COLOR);

    Renderer.ActiveTexture(1);
    glBindTexture(GL_TEXTURE_2D, TexOriginal);
    glEnable(GL_TEXTURE_2D);
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_EXT);

    { texture unit 1: multiply diffuse factor by texture value. }
    glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB_EXT, GL_MODULATE);
    glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_EXT, GL_PREVIOUS_EXT);
    glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB_EXT, GL_SRC_COLOR);
    glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_RGB_EXT, GL_TEXTURE);
    glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND1_RGB_EXT, GL_SRC_COLOR);

    glTexEnvAlphaFromMaterialAndTexture;

    LightPositionObjectSpace := MultMatrixPoint(
      State.InvertedTransform,
      Renderer.BumpMappingLightPosition);
  end;

  procedure EnableForMultiTexDotNormalized;
  begin
    { texture unit 0: just lookup your coordinates (which are
      3D lighting dir, not normalized) in cube map.
      This calculates normalized lighting dir. }
    Renderer.ActiveTexture(0);
    glBindTexture(GL_TEXTURE_CUBE_MAP_ARB, TexNormalizationCube);
    glEnable(GL_TEXTURE_CUBE_MAP_ARB);
    { make sure GL_TEXTURE_2D is disabled here (other bump mapping
      methods could enable this) }
    glDisable(GL_TEXTURE_2D);
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_EXT);

    glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB_EXT, GL_REPLACE);
    glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_EXT, GL_TEXTURE);
    glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB_EXT, GL_SRC_COLOR);

    Renderer.ActiveTexture(1);
    glBindTexture(GL_TEXTURE_2D, TexNormalMap);
    glEnable(GL_TEXTURE_2D);
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_EXT);

    { texture unit 1: make dot between previous (light dir normalized) and
      current texture (normal map). IOW, calculate diffuse factor. }
    glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB_EXT, GL_DOT3_RGB_ARB);
    glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_EXT, GL_PREVIOUS_EXT);
    glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB_EXT, GL_SRC_COLOR);
    glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_RGB_EXT, GL_TEXTURE);
    glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND1_RGB_EXT, GL_SRC_COLOR);

    Renderer.ActiveTexture(2);
    { We bind NormalAndBumpTex, although actually we don't use
      bump stored there as alpha channel (they are only for emboss
      method). }
    glBindTexture(GL_TEXTURE_2D, TexOriginal);
    glEnable(GL_TEXTURE_2D);
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_EXT);

    { texture unit 2: multiply diffuse factor by texture value. }
    glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB_EXT, GL_MODULATE);
    glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_EXT, GL_PREVIOUS_EXT);
    glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB_EXT, GL_SRC_COLOR);
    glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_RGB_EXT, GL_TEXTURE);
    glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND1_RGB_EXT, GL_SRC_COLOR);

    glTexEnvAlphaFromMaterialAndTexture;

    LightPositionObjectSpace := MultMatrixPoint(
      State.InvertedTransform,
      Renderer.BumpMappingLightPosition);
  end;

  procedure EnableForGLSL;
  var
    BmParallax: boolean;
  begin
    { This binds textures in the same way as EnableForMultiTexDotNotNormalized,
      but this doesn't call glTexEnvi or glEnable(GL_TEXTURE_2D)
      (it's useless, shader will control this). }
    Renderer.ActiveTexture(0);
    glBindTexture(GL_TEXTURE_2D, TexNormalMap);

    Renderer.ActiveTexture(1);
    glBindTexture(GL_TEXTURE_2D, TexOriginal);

    BmParallax := BumpMappingMethod >= bmGLSLParallax;

    if BmParallax then
    begin
      Renderer.ActiveTexture(2);
      glBindTexture(GL_TEXTURE_2D, TexHeightMap);
    end;

    Renderer.BmGLSLProgram[BmParallax].Enable;
    Renderer.BmGLSLProgram[BmParallax].SetUniform('world_space_to_object',
      State.InvertedTransform);

    if BmParallax then
    begin
      Renderer.BmGLSLProgram[BmParallax].SetUniform('scale', TexHeightMapScale);
      if not Renderer.BmSteepParallaxMapping then
        Renderer.BmGLSLProgram[BmParallax].SetUniform('bias', -TexHeightMapScale/2);
    end;

    glMaterialv(GL_FRONT_AND_BACK, GL_AMBIENT, Vector4Single(1, 1, 1, 1));
  end;

begin
  if BumpMappingMethod <> bmNone then
  begin
    case BumpMappingMethod of
      bmMultiTexDotNotNormalized: EnableForMultiTexDotNotNormalized;
      bmMultiTexDotNormalized   : EnableForMultiTexDotNormalized;
      bmGLSLNormal,
      bmGLSLParallax            : EnableForGLSL;
      else raise EInternalError.Create('BumpMappingMethod ?');
    end;
  end;

  inherited;

  if Render_Normals = inGenerate then
  begin
    { I could just do normal CreateNormals for all CreaseAngle values,
      so the test for "CreaseAngle <> 0" is not really required below.
      But I want to set NormalsFlat to @true whenever possible
      (as NormalsFlat allow some optimizations), so the test for
      "CreaseAngle <> 0" is useful there (as many VRML models will have
      default creaseAngle = 0, so NormalsFlat = true optimizations may be
      often applicable). }

    if Renderer.Attributes.SmoothShading and (CreaseAngle <> 0) then
    begin
      Render_GeneratedNormals := CreateNormals(
        CoordIndex.Items, Coord.Items, CreaseAngle, true);
      NormalsFlat := false;
    end else
    begin
      Render_GeneratedNormals := CreateFlatNormals(
        CoordIndex.Items, Coord.Items, true);
      NormalsFlat := true;
    end;
  end;
end;

procedure TIndexedFaceSetRenderer.Render_Fini;

  { DisableXxx procedures disable textures (2d and cube map)
    that were enabled by their SetXxx counterparts in Render_Init.
    In case of shader implementation, this also disables the shader.

    Note that they still leave some state behind:
    - currently bound textures
    - texture environments
    - the currently bound texture is left unspecified
    These must be set always when anything will enable some texture. }

  procedure DisableForMultiTexDotNotNormalized;
  begin
    Renderer.ActiveTexture(0);
    glDisable(GL_TEXTURE_2D);

    Renderer.ActiveTexture(1);
    glDisable(GL_TEXTURE_2D);
  end;

  procedure DisableForMultiTexDotNormalized;
  begin
    { texture unit 0: just lookup your coordinates (which are
      3D lighting dir, not normalized) in cube map.
      This calculates normalized lighting dir. }
    Renderer.ActiveTexture(0);
    glDisable(GL_TEXTURE_CUBE_MAP_ARB);

    Renderer.ActiveTexture(1);
    glDisable(GL_TEXTURE_2D);

    Renderer.ActiveTexture(2);
    glDisable(GL_TEXTURE_2D);;
  end;

  procedure DisableForGLSL;
  begin
    { No need to disable textures, as we didn't enable them in
      EnableForGLSL (shader doesn't need them enabled). }

    Renderer.BmGLSLProgram[BumpMappingMethod >= bmGLSLParallax].Disable;
  end;

begin
  FreeAndNil(Render_GeneratedNormals);
  inherited;

  if BumpMappingMethod <> bmNone then
  begin
    case BumpMappingMethod of
      bmMultiTexDotNotNormalized: DisableForMultiTexDotNotNormalized;
      bmMultiTexDotNormalized   : DisableForMultiTexDotNormalized;
      bmGLSLNormal,
      bmGLSLParallax            : DisableForGLSL;
      else raise EInternalError.Create('BumpMappingMethod ?');
    end;
  end;
end;

procedure Render_Triangulated(const TriIndices: TVector3Longint; Data: Pointer);
begin
 TIndexedFaceSetRenderer(Data).RenderVertex(TriIndices[0]);
 TIndexedFaceSetRenderer(Data).RenderVertex(TriIndices[1]);
 TIndexedFaceSetRenderer(Data).RenderVertex(TriIndices[2]);
end;

procedure TIndexedFaceSetRenderer.Render_Face(const AFaceNumber: Cardinal;
  startIndex, endIndex: integer);

  { Calculates HasTangentVectors, STangent, TTangent }
  procedure CalculateTangentVectors;

    { This procedure can change Triangle*, but only by swapping some vertexes,
      so we pass Triangle* by reference instead of by value, to avoid
      needless mem copying.

      Returns @false if cannot be calculated. }
    function CalculateTangent(IsSTangent: boolean; var Tangent: TVector3Single;
      var Triangle3D: TTriangle3Single;
      var TriangleTexCoord: TTriangle2Single): boolean;
    var
      D: TVector2Single;
      LineA, LineBC, DIn3D: TVector3Single;
      MiddleIndex: Integer;
      FarthestDistance, NewDistance, Alpha: Single;
      SearchCoord, OtherCoord: Cardinal;
    begin
      if ISSTangent then
        SearchCoord := 0 else
        SearchCoord := 1;
      OtherCoord := 1 - SearchCoord;

      { choose such that 1st and 2nd points have longest distance along
        OtherCoord, so 0 point is in the middle. }

      { MiddleIndex means that
        MiddleIndex, (MiddleIndex + 1) mod 3 are farthest. }

      MiddleIndex := 2;
      FarthestDistance := Abs(TriangleTexCoord[0][OtherCoord] - TriangleTexCoord[1][OtherCoord]);

      NewDistance := Abs(TriangleTexCoord[1][OtherCoord] - TriangleTexCoord[2][OtherCoord]);
      if NewDistance > FarthestDistance then
      begin
        MiddleIndex := 0;
        FarthestDistance := NewDistance;
      end;

      NewDistance := Abs(TriangleTexCoord[2][OtherCoord] - TriangleTexCoord[0][OtherCoord]);
      if NewDistance > FarthestDistance then
      begin
        MiddleIndex := 1;
        FarthestDistance := NewDistance;
      end;

      if IsZero(FarthestDistance) then
        Exit(false);

      if MiddleIndex <> 0 then
      begin
        SwapValues(TriangleTexCoord[0], TriangleTexCoord[MiddleIndex]);
        SwapValues(Triangle3D      [0], Triangle3D      [MiddleIndex]);
      end;

      if IsSTangent then
      begin
        { we want line Y = TriangleTexCoord[0][1]. }
        LineA[0] := 0;
        LineA[1] := 1;
        LineA[2] := -TriangleTexCoord[0][1];
      end else
      begin
        { we want line X = TriangleTexCoord[0][0]. }
        LineA[0] := 1;
        LineA[1] := 0;
        LineA[2] := -TriangleTexCoord[0][0];
      end;
      LineBC := LineOfTwoDifferentPoints2d(
        TriangleTexCoord[1], TriangleTexCoord[2]);

      try
        D := Lines2DIntersection(LineA, LineBC);
      except
        on ELinesParallel do begin Result := false; Exit; end;
      end;

      { LineBC[0, 1] is vector 2D orthogonal to LineBC.
        If Abs(LineBC[0]) is *smaller* then it means that B and C points
        are most different on 0 coord. }
      if Abs(LineBC[0]) < Abs(LineBC[1]) then
        Alpha := (                  D[0] - TriangleTexCoord[1][0]) /
                 (TriangleTexCoord[2][0] - TriangleTexCoord[1][0]) else
        Alpha := (                  D[1] - TriangleTexCoord[1][1]) /
                 (TriangleTexCoord[2][1] - TriangleTexCoord[1][1]);

      DIn3D := VectorAdd(
        VectorScale(Triangle3D[1], 1 - Alpha),
        VectorScale(Triangle3D[2], Alpha));

      if D[SearchCoord] > TriangleTexCoord[0][SearchCoord] then
        Tangent := VectorSubtract(DIn3D, Triangle3D[0]) else
        Tangent := VectorSubtract(Triangle3D[0], DIn3D);

      NormalizeTo1st(Tangent);

      Result := true;
    end;

  var
    Triangle3D: TTriangle3Single;
    TriangleTexCoord: TTriangle2Single;
  begin
    HasTangentVectors := false;
    if (BumpMappingMethod <> bmNone) and (StartIndex + 2 < EndIndex) then
    begin
      { calculate Triangle3D }
      Triangle3D[0] := Coord.Items.ItemsArray^[CoordIndex.Items.Items[StartIndex]];
      Triangle3D[1] := Coord.Items.ItemsArray^[CoordIndex.Items.Items[StartIndex + 1]];
      Triangle3D[2] := Coord.Items.ItemsArray^[CoordIndex.Items.Items[StartIndex + 2]];

      { This is just to shut up FPC 2.2.0 warnings about
        TriangleTexCoord not initialized. }
      TriangleTexCoord[0][0] := 0.0;

      HasTangentVectors :=
        { calculate TriangleTexCoord }
        GetTextureCoord(StartIndex    , TriangleTexCoord[0]) and
        GetTextureCoord(StartIndex + 1, TriangleTexCoord[1]) and
        GetTextureCoord(StartIndex + 2, TriangleTexCoord[2]) and
        { calculate STangent, TTangent }
        CalculateTangent(true , STangent, Triangle3D, TriangleTexCoord) and
        CalculateTangent(false, TTangent, Triangle3D, TriangleTexCoord) and
        (Abs(VectorDotProduct(STangent, TTangent)) < 0.95);
    end;
  end;

begin
  if (Render_Normals = inGenerate) and NormalsFlat then
    glNormalv(Render_GeneratedNormals.Items[StartIndex]);

  CalculateTangentVectors;

 if not FaceConvex then
 begin
  glBegin(GL_TRIANGLES);
  TriangulateFace(Addr(CoordIndex.Items.Items[StartIndex]),
    EndIndex-StartIndex, Coord.Items.ItemsArray,
    {$ifdef FPC_OBJFPC} @ {$endif} Render_Triangulated, Self, StartIndex);
  glEnd;
 end else
 begin
   {$ifdef DARWIN}

   { On Mac OS X 10.4.9, with Radeon ATI graphic card
     (it's MacBook Pro computer), there's definitely a bug
     in OpenGL implementation: it seems not able to handle
     GL_TRIANGLE_FAN properly, which is visible on "The Gate" and
     "Cages" levels of "The Castle": some triangles on these
     levels (on "The Gate", it concerns that back wall and gate
     geometry) seem to randomly disappear.

     I didn't make enough
     tests to actually make sure what's the precise condition
     to trigger this bug (as other "The Castle" levels,
     and castle.wrl sample VRML work OK). I also didn't test
     on other Mac OS X systems. (However, I did test this on the
     same computer with Linux Radeon OpenGL, and on Linux
     it works OK --- so the problem is within software, OpenGL
     probably, on Mac OS X ?). For safety I disable using
     GL_TRIANGLE_FAN entirely for now on Darwin. }

  glBegin(GL_TRIANGLES);
  TriangulateConvexFace(EndIndex-StartIndex,
    {$ifdef FPC_OBJFPC} @ {$endif} Render_Triangulated, Self, StartIndex);
  glEnd;

  {$else DARWIN}

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
   RenderVertex(startIndex);
   Inc(startIndex);
  end;
  glEnd;

  {$endif DARWIN}
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
  Coord := State.LastNodes.Coordinate3.FdPoint;
  TexCoordIndex := ANode.FdTextureCoordIndex;
  TexCoord := State.LastNodes.TextureCoordinate2.FdPoint;
  NormalIndex := ANode.FdNormalIndex;
  StoredNormal := State.LastNodes.Normal.FdVector;
  MaterialIndex := ANode.FdMaterialIndex;

  NormalBinding := State.LastNodes.NormalBinding.FdValue.Value;
  MaterialBinding := State.LastNodes.MaterialBinding.FdValue.Value;

  SH := State.LastNodes.ShapeHints;

  { W tym miejscu uznajemy VERTORDER_UNKNOWN_ORDERING za COUNTERCLOCKWISE
    (a autorzy VRMLi w ogole nie powinni podawac normali jesli
    nie podadza vertexOrdering innego niz UNKNOWN) }
  FrontFaceCcw := SH.FdVertexOrdering.Value <> VERTORDER_CLOCKWISE;

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
  Coord := ANode.Coord;
  TexCoordIndex := ANode.FdTexCoordIndex;
  if (ANode.FdTexCoord.Value <> nil) and
     (ANode.FdTexCoord.Value is TNodeTextureCoordinate) then
    TexCoord := TNodeTextureCoordinate(ANode.FdTexCoord.Value).FdPoint;
  NormalIndex := ANode.FdNormalIndex;
  if (ANode.FdNormal.Value <> nil) and
     (ANode.FdNormal.Value is TNodeNormal) then
    StoredNormal := TNodeNormal(ANode.FdNormal.Value).FdVector;

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

  FrontFaceCcw := ANode.FdCcw.Value;
  CullBackFaces := ANode.FdSolid.Value;
  CreaseAngle := ANode.FdCreaseAngle.Value;
  FaceConvex := ANode.FdConvex.Value;

  { calculate Color, ColorPerVertex, ColorIndex fields }
  if (ANode.FdColor.Value <> nil) and
     (ANode.FdColor.Value is TNodeColor) then
    Color := TNodeColor(ANode.FdColor.Value).FdColor else
    Color := nil;
  ColorPerVertex := ANode.FdColorPerVertex.Value;
  ColorIndex := ANode.FdColorIndex;
end;

procedure TIndexedFaceSet_2Renderer.SetLitColor(const AColor: TVector3Single);
begin
  { We use Material_BoundOpacity since we do not want to change
    material opacity here, only it's RGB part. }

  if Renderer.Attributes.PureGeometry or
     (not Renderer.Attributes.ControlMaterials) then
    Exit;

  glMaterialv(GL_FRONT_AND_BACK, GL_DIFFUSE,
    Vector4f(Renderer.Attributes.ColorModulated(AColor),
    Renderer.Material_BoundOpacity));

  glColorv(Vector4f(Renderer.Attributes.ColorModulated(AColor),
    Renderer.Material_BoundOpacity));
end;

procedure TIndexedFaceSet_2Renderer.RenderVertex(IndexNum: integer);
begin
  { Implement different color per vertex here. }
  if (Color <> nil) and ColorPerVertex then
  begin
    if ColorIndex.Count <> 0 then
      SetLitColor(Color.ItemsSafe[ColorIndex.ItemsSafe[IndexNum]]) else
      SetLitColor(Color.ItemsSafe[CoordIndex.ItemsSafe[IndexNum]]);
  end;

  inherited;
end;

procedure TIndexedFaceSet_2Renderer.Render_Face(const AFaceNumber: Cardinal;
  startIndex, endIndex: integer);
begin
  { Implement different color per face here. }
  if (Color <> nil) and (not ColorPerVertex) then
  begin
    if ColorIndex.Count <> 0 then
      SetLitColor(Color.ItemsSafe[ColorIndex.ItemsSafe[AFaceNumber]]) else
      SetLitColor(Color.ItemsSafe[AFaceNumber]);
  end;

  inherited;
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
  Coord := State.LastNodes.Coordinate3.FdPoint;
  TexCoordIndex := ANode.FdTextureCoordIndex;
  TexCoord := State.LastNodes.TextureCoordinate2.FdPoint;
  NormalIndex := ANode.FdNormalIndex;
  StoredNormal := State.LastNodes.Normal.FdVector;
  MaterialIndex := ANode.FdMaterialIndex;

  NormalBinding := State.LastNodes.NormalBinding.FdValue.Value;
  MaterialBinding := State.LastNodes.MaterialBinding.FdValue.Value;

  SH := State.LastNodes.ShapeHints;

  { W tym miejscu uznajemy VERTORDER_UNKNOWN_ORDERING za COUNTERCLOCKWISE
    (a autorzy VRMLi w ogole nie powinni podawac normali jesli
    nie podadza vertexOrdering innego niz UNKNOWN) }
  FrontFaceCcw := SH.FdVertexOrdering.Value <> VERTORDER_CLOCKWISE;

  CullBackFaces :=
    (SH.FdVertexOrdering.Value <> VERTORDER_UNKNOWN) and
    (SH.FdShapeType.Value = SHTYPE_SOLID);
end;

procedure TIndexedTriangleMesh_1Renderer.Render_Face(const AFaceNumber: Cardinal;
  startIndex, endIndex: integer);

  procedure DoNormal(const v1, v2, v3: integer);
  begin
    { TODO: for IndexedTriangleMesh we always generate flat normals }
    if Render_Normals = inGenerate then
    begin
      { We always pass to OpenGL normals from CCW, so TriangleNormal is Ok. }
      glNormalv(TriangleNormal(
        GetVertex(V1),
        GetVertex(V2),
        GetVertex(V3)));
    end;
  end;

var
  NormalOrder: boolean;
begin
  if StartIndex + 2 < EndIndex then
  begin
    glPushAttrib(GL_LIGHTING_BIT { for shade model });
    glShadeModel(GL_FLAT);

      glBegin(GL_TRIANGLE_STRIP);

      RenderVertex(StartIndex);
      RenderVertex(StartIndex + 1);

      NormalOrder := true;

      while StartIndex + 2 < EndIndex do
      begin
        if NormalOrder then
          DoNormal(StartIndex    , StartIndex + 1, StartIndex + 2) else
          DoNormal(StartIndex + 1, StartIndex    , StartIndex + 2);
        RenderVertex(StartIndex + 2);
        Inc(StartIndex);
        NormalOrder := not NormalOrder;
      end;

      glEnd;

    glPopAttrib();
  end;
end;

{ TIndexedLineSetRenderer -------------------------------------------------- }

procedure TIndexedLineSetRenderer.Render_Face(const AFaceNumber: Cardinal;
  startIndex, endIndex: integer);
var i: integer;
begin
  glBegin(GL_LINE_STRIP);
  for i := startIndex to endIndex - 1 do RenderVertex(i);
  glEnd;
end;

{ TIndexedLineSet_1Renderer -------------------------------------------------- }

constructor TIndexedLineSet_1Renderer.Create(
  ARenderer: TVRMLOpenGLRenderer;
  ANode: TNodeIndexedLineSet_1);
begin
  inherited Create(ARenderer);

  CoordIndex := ANode.FdCoordIndex;
  Coord := State.LastNodes.Coordinate3.FdPoint;
  TexCoordIndex := ANode.FdTextureCoordIndex;
  TexCoord := State.LastNodes.TextureCoordinate2.FdPoint;
  NormalIndex := ANode.FdNormalIndex;
  StoredNormal := State.LastNodes.Normal.FdVector;
  MaterialIndex := ANode.FdMaterialIndex;

  NormalBinding := State.LastNodes.NormalBinding.FdValue.Value;
  MaterialBinding := State.LastNodes.MaterialBinding.FdValue.Value;
end;

{ TIndexedLineSet_2Renderer -------------------------------------------------- }

constructor TIndexedLineSet_2Renderer.Create(
  ARenderer: TVRMLOpenGLRenderer;
  ANode: TNodeIndexedLineSet_2);
begin
  inherited Create(ARenderer);

  CoordIndex := ANode.FdCoordIndex;
  Coord := ANode.Coord;

  MaterialBinding := BIND_DEFAULT;

  if (ANode.FdColor.Value <> nil) and
     (ANode.FdColor.Value is TNodeColor) then
    Color := TNodeColor(ANode.FdColor.Value).FdColor else
    Color := nil;
  ColorPerVertex := ANode.FdColorPerVertex.Value;
  ColorIndex := ANode.FdColorIndex;
end;

procedure TIndexedLineSet_2Renderer.SetColor(const AColor: TVector3Single);
begin
  if Renderer.Attributes.PureGeometry or
     (not Renderer.Attributes.ControlMaterials) then
    Exit;

  glColorv(Renderer.Attributes.ColorModulated(AColor));
end;

procedure TIndexedLineSet_2Renderer.Render_Init;
var
  Material: TNodeMaterial_2;
begin
  inherited;

  { Implement "one color for the whole lineset" case here.

    IndexedLineSet color may come from various places:
    1. Color node, for each vertex or polyline, if it's not NULL
    2. Material.emissiveColor, for whole line
    3. If no material, we use default White3Single, for whole line
       (following general spec remark at Material node that
       Material = NULL makes unlit white color) }
  if Color = nil then
  begin
    Material := State.ParentShape.Material;
    if Material <> nil then
      SetColor(Material.FdEmissiveColor.Value) else
      SetColor(White3Single);
  end;
end;

procedure TIndexedLineSet_2Renderer.RenderVertex(IndexNum: integer);
begin
  { Implement different color per vertex here. }
  if (Color <> nil) and ColorPerVertex then
  begin
    if ColorIndex.Count <> 0 then
      SetColor(Color.ItemsSafe[ColorIndex.ItemsSafe[IndexNum]]) else
      SetColor(Color.ItemsSafe[CoordIndex.ItemsSafe[IndexNum]]);
  end;

  inherited;
end;

procedure TIndexedLineSet_2Renderer.Render_Face(const AFaceNumber: Cardinal;
  startIndex, endIndex: integer);
begin
  { Implement different color per face here. }
  if (Color <> nil) and (not ColorPerVertex) then
  begin
    if ColorIndex.Count <> 0 then
      SetColor(Color.ItemsSafe[ColorIndex.ItemsSafe[AFaceNumber]]) else
      SetColor(Color.ItemsSafe[AFaceNumber]);
  end;

  inherited;
end;

{ eof --------------------------------------------------------------------------- }
