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

{ Utilities for VRML / X3D mesh rendering.
  This is internal for VRMLOpenGLRenderer unit.

  Some ideas for implementation:

  - VRMLOpenGLRenderer unit, and TVRMLOpenGLRenderer class are already
    large and complicated. The idea of this file is to take some tasks
    from their hands. TVRMLMeshRenderer and descendants are created
    only for the time of one RenderShapeState call, and are used
    to render specific non-trivial VRML mesh nodes.

  - This tries to reuse and reimplement some ideas that I got
    from TGeneralIndexedRenderer. TGeneralIndexedRenderer hierarchy
    was a good idea, but it was never cleanly enough implemented.
    Time showed that cleaner implementation of such hierarchy is needed,
    to be usable for rendering *all* VRML nodes.

  - Since all classes within this file and VRMLOpenGLRenderer live
    in one Pascal unit, we could break it and access each other's
    private fields etc. We try to not do it. We try to have a clean
    documented interface. Experience with TGeneralIndexedRenderer
    taught me that this has to be really flexible and designed for
    long-term work, to encompass many nodes and features. So, while
    this is internal for VRMLOpenGLRenderer, it still must be implemented
    and documented cleanly.

  - In the future, this may be abstracted from OpenGL and used as a basis
    also for TVRMLGeometryNode.Triangulate methods implementation.
}

type
  TVRMLMeshRenderer = class
  private
    { Common code before both DoGLVertex and DoGLArrayElement.

      In this class:
      - calls Renderer.OnBeforeGLVertex if assigned.
      - sets fog coordinate.

      Passed Vert is local vertex coordinate (in State.Transform space),
      just like for glVertex. }
    procedure DoBeforeGLVertex(const Vert: TVector3Single);

    FRenderer: TVRMLOpenGLRenderer;
    FState: TVRMLGraphTraverseState;
    FGeometry: TVRMLGeometryNode;
    FAttributes: TVRMLRenderingAttributes;
  protected
    { DoGLVertex and DoGLArrayElement are the only two methods
      that can be used to render vertexes. One calls glVertex, second one
      glArrayElemene, this should be self-explanatory.

      Both call DoBeforeGLVertex at the beginning.

      For DoGLArrayElement, we also need Verts: an array of vertexes
      in local coordinates. The same one that you used to initialize
      OpenGL vertex coord array (we may need to access vertex coord ourselves). }
    procedure DoGLVertex(const Vert: TVector3Single);
    procedure DoGLArrayElement(const Verts: PArray_Vector3Single; ith: TGLint);

    { Current rendering properties, constant for the whole
      lifetime of the renderer, set in constructor.
      Setting Renderer automatically sets State, Geometry, Attributes. }
    property Renderer: TVRMLOpenGLRenderer read FRenderer;
    property State: TVRMLGraphTraverseState read FState;
    property Geometry: TVRMLGeometryNode read FGeometry;
    property Attributes: TVRMLRenderingAttributes read FAttributes;

    { Initialized in our constructor to false.
      Set this @italic(in constructor) (not later) to true when you're in
      subclass able to do bump mapping. }
    BumpMappingAllowed: boolean;

    { CullBackFaces says if we should enable back-face culling.
      If @true, then in our Render, we will do glEnable(GL_CULL_FACE),
      and set glCullFace such that front face will be visible.
      FrontFaceCcw says what is "front face".

      You can set these only in your constructor.
      By default, both are false, unless the Geometry is of
      X3DComposedGeometryNode type, then we use X3DComposedGeometryNode
      solid/ccw fields. So for X3DComposedGeometryNode it's already handled
      here -- it's set, and handled during Render.

      (In this class, FrontFaceCcw is ignored if CullBackFaces = @false.
      However, note that in subclasses (TAbstractNorColMatTexCoordinateRenderer),
      FrontFaceCcw is also used to denote
      "from which side of the face user-supplied normals point".
      So FrontFaceCcw says from which face side normals in VRML file point out,
      and which faces should be culled if CullBackFaces.)

      Note that we *do not* implement these by glFrontFace, we do a little
      more complicated trick, see comments at the beginning of VRMLOpenGLRenderer
      for explanation (hint: plane mirrors).

      If CullBackFaces = @true, then you can call glCullFace
      in Render if you want to change which
      faces are culled (ccw or cw) in the middle of rendering. We are inside
      glPushAttrib(GL_POLYGON_BIT) anyway (if CullBackFaces),
      so glCullFace changes are protected.

      @groupBegin }
    CullBackFaces: boolean;
    FrontFaceCcw: boolean;
    { @groupEnd }

    { Render the whole geometry.

      For some details what is, and what is not yet set in OpenGL
      before this, you have to see TVRMLOpenGLRender.RenderShapeState
      implementation.

      Material for VRML 1.0: currently bound material is undefined
      So for VRML 1.0, before you actually render anything for OpenGL,
      you must call Render_BindMaterial_1 at least once.
      You cannot call glMaterial or glColor here --- this is the task of
      Render_BindMaterial_1.

      Texture: you only have to generate texture coordinates in descendants.
      If not Render_TexCoordsNeeded, you don't have to (but you can,
      although they will be unused) generate texture coords.
      Do not change here the bound texture or operate on texture matrix
      or texture enabled state.

      For now, see comments at the beginning of
      vrmlopenglrenderer_render_specificnodes.inc
      for general direction what can and what can not be used here. }
    procedure DoRender; virtual; abstract;
  public
    { Constructor.

      For descendants implementors:
      Don't do any OpenGL state changes here (initialization of OpenGL state
      should go to Render, for example RenderCoordinateBegin). }
    constructor Create(ARenderer: TVRMLOpenGLRenderer);

    { Do the actual render of the whole geometry.
      Don't override this, this only sets some common things
      (like CullBackFaces) and calls DoRender. }
    procedure Render;

    { Initialized in our constructor to bmNone.
      Renderer will set this to Renderer.BumpMappingMethod
      only if bump mapping should be used and BumpMappingAllowed. }
    BumpMappingMethod: TBumpMappingMethod;
  end;

  { Base abstract class for rendering nodes with explicit "coord" field.
    (Both IndexedXxx and not indexed).

    Usage:
    - You must set Coord field in constructor, or (if Coord = @nil)
      nothing will be rendered.

    - Don't override Render --- this is already overridden here
      to do everything necessary. Instead, override
        RenderCoordinate
        RenderCoordinateBegin
        RenderCoordinateEnd

      These are called only when Coord is assigned.

      Render pushes client attribs and sets up vertex array for Coord.
      Then calls RenderCoordinateBegin and then locks arrays.
      So RenderCoordinateBegin is a good place to initialize other
      OpenGL arrays (for example, for texture coordinates or normal
      coordinates) if you want.

      Inside, RenderCoordinate is called.

      Then arrays are unlocked, RenderCoordinateEnd is called (so it
      should finalize whatever needs to be finalized for previous
      RenderCoordinateBegin call) and client attribs are popped.
  }
  TAbstractCoordinateRenderer = class(TVRMLMeshRenderer)
  protected
    { Coordinates, usually taken from (coord as Coordinate).points field.
      If @nil then nothing will be rendered. }
    Coord: TMFVec3f;

    { Coordinate index. Set this in descendant's constructor if it's
      indexed geometry.

      If @nil, then RenderVertex (and all other
      routines taking some index) will just directly index Coord
      (this is useful for non-indexed geometry, like TriangleSet
      instead of IndexedTriangleSet). }
    CoordIndex: TMFLong;

    { Use this in descendants as analogy of glVertex / glDrawElement.
      This sends vertex to OpenGL (using TVRMLMeshRenderer.DoGLDrawElement,
      but this is internal information :) ). Given IndexNum
      indexes Coord, or if (CoordIndex is assigned) indexes CoordIndex
      (and CoordIndex then indexes actual Coord). }
    procedure RenderVertex(IndexNum: integer); virtual;

    { This gets vertex coordinate, exactly the same as would be
      rendered by RenderVertex. Returned vertex is in local coordinate space
      (use State.Transform if you want to get global coordinates). }
    function GetVertex(IndexNum: integer): TVector3f;

    { Count of indexes. You can pass index between 0 and CoordCount - 1
      to various methods taking an index, like RenderVertex. }
    function CoordCount: Integer;

    procedure DoRender; override;

    { Override these to plug your rendering algorithm.
      See the description of this class @className for usage overview
      of these methods.

      When overriding RenderCoordinateBegin, always call inherited
      at the begin. When overriding RenderCoordinateEnd, always call inherited
      at the end.

      @groupBegin }
    procedure RenderCoordinate; virtual; abstract;
    procedure RenderCoordinateBegin; virtual;
    procedure RenderCoordinateEnd; virtual;
    { @groupEnd }

    { RenderCoordsRanges iterates over CoordIndex, passing each
      range of non-negative indexes to RenderCoordsRange.

      You can use this only when CoordIndex <> nil, and it's
      sensible only when VRML specification says that coordIndex
      may be negative to separate some various-length parts of the
      coordinates. The most typical example of this is IndexedFaceSet,
      where each "range of non-negative indexes" is simply a polygon.

      RenderCoordsRange is supposed to render the parts of the mesh
      between BeginIndex and EndIndex - 1 vertices.
      BeginIndex and EndIndex are indexes to CoordIndex array.
      It's guaranteed that all indexes between
      BeginIndex and EndIndex - 1 are non-negative, and they are surrounded
      by negative or by the ends of CoordIndex array.
      In other words, RenderCoordsRange always gets indexes specifying
      a complete face in the sense of IndexedFaceSet and similar
      nodes.

      As always, it's your responsibility to call appropriate glBegin / glEnd
      around RenderVertex calls --- maybe within each RenderCoordsRange,
      maybe the whole RenderCoordsRanges call is within such glbegin/end.

      @groupBegin }
    procedure RenderCoordsRange(
      const RangeNumber: Cardinal;
      BeginIndex, EndIndex: Integer); virtual;
    procedure RenderCoordsRanges;
    { @groupEnd }
  end;

  TTextureCoordsImplementation = (
    { Texture coords not generated, because not needed by Renderer. }
    tcNotGenerated,
    { Auto-generated from local vertex position, using STextureGen, TTextureGen. }
    tcAutoGenerated,
    { IndexNum is an index to TexCoordIndex, as this indexes TexCoord. }
    tcTexIndexed,
    { IndexNum is an index to CoordIndex, as this indexes TexCoord. }
    tcCoordIndexed,
    { IndexNum is a direct index to TexCoord. }
    tcNonIndexed);

  { Enhances TAbstractCoordinateRenderer with the ability to handle
    texture coordinate generation.

    Usage:

    - Set TexCoord, if available. When TexCoord = @nil,
      then we'll generate default texture coordinates,
      following VRML 2.0 / X3D IndexedFaceSet default texture coord algorithm.

      (X3D spec doesn't say what happens for nodes like
      IndexedTriangleSet when texture is specified (in Appearance.texture)
      but texture coords are not present.
      For now, we just generate texture coords for them just like for
      IndexedFaceSet.)

    - Set TexCoordIndex, if available. This works just like CoordIndex:

      If @nil, then RenderVertex (and all other
      routines taking some index) will just directly index TexCoord
      (this is useful for non-indexed geometry, like TriangleSet
      instead of IndexedTriangleSet). Otherwise, they will index TexCoordIndex,
      and then TexCoordIndex provides index to TexCoord.

      As a special case, if TexCoordIndex is assigned but empty
      (actually, just checked as "shorter than CoordIndex")
      then IndexNum indexed CoordIndex. So CoordIndex acts as
      TexCoordIndex in this case. This case is specially for
      for IndexedFaceSet in VRML >= 2.0.

      Always when TexCoordIndex is non-nil, also make sure that CoordIndex
      is non-nil. When TexCoordIndex is nil, make sure that CoordIndex is nil.
      (This restriction may be removed in the future,
      but for now nothing needs it.)

    This class takes care to generate tex coords, or use supplied
    TexCoord and TexCoordIndex. Assuming texture coordinates
    will be wanted at all (Renderer.Render_TexCoordsNeeded).
    So you do not have to generate any texture coordinates (call glTexCoord etc.)
    in descendants. Everything related to textures is already
    handled in this class.

    (The only exception to above, when you need to generate some
    texture coords, is when you allow bump mapping, that is
    you set BumpMappingAllowed := true for your TAbstractTexCoordinateRenderer
    descendant. See IndexedFaceSet implementation for now what must
    be done then, and see implementation of this class to see what
    is different when BumpMappingMethod <> bmNone.

    GetTextureCoord may be useful in descendants in this case.) }
  TAbstractTexCoordinateRenderer = class(TAbstractCoordinateRenderer)
  private
    TexImplementation: TTextureCoordsImplementation;

    { Set only if TexImplementation = tcAutoGenerated,
      these are vectors used to generate
      texture coords from local (object space) vertex positions. }
    STextureGen, TTextureGen: TVector4f;
  protected
    TexCoord: TMFVec2f;
    TexCoordIndex: TMFLong;

    { Return texture coordinate for given vertex, identified by IndexNum.
      IndexNum indexes TexCoord, or TexCoordIndex (if TexCoordIndex assigned),
      or CoordIndex (if TexCoordIndex assigned but empty, for IndexedFaceSet).

      Returns @false if no texture coords are available.

      Works in all cases when we actually render some texture. }
    function GetTextureCoord(IndexNum: integer; out Tex: TVector2Single): boolean;

    procedure RenderVertex(IndexNum: Integer); override;

    procedure RenderCoordinateBegin; override;
    procedure RenderCoordinateEnd; override;
  end;

  TMaterials1Implementation = (miOverall,
    miPerVertexCoordIndexed,
    miPerVertexMatIndexed,
    miPerFace,
    miPerFaceMatIndexed);

  { Enhances TAbstractTexCoordinateRenderer with the ability to handle
    per-face or per-vertex VRML 1.0 materials.

    Usage:
    - Just set MaterialIndex and MaterialBinding using your node's fields.
    - For VRML >= 2.0 nodes, you don't have to do anything.
      You can just leave MaterialBinding as default BIND_DEFAULT,
      but it really doesn't matter: the only effect of this class
      are calls to Render_BindMaterial_1. And Render_BindMaterial_1
      simply does nothing for VRML >= 2.0 geometry nodes.

    Since this must have a notion of what "face" is, it assumes that
    your RenderCoordsRange constitutes rendering of a single face.
    If this isn't true, then "per face" materials will not work
    correctly.

    Note that "per vertex" materials require smooth shading,
    so you should set this in your Render. There's no way to implement
    them with flat shading.

    Do not call Renderer.Render_BindMaterial_1
    in descendants. Everything related to VRML 1.0 materials is already
    handled in this class. }
  TAbstractMatTexCoordinateRenderer = class(TAbstractTexCoordinateRenderer)
  private
    Mat1Implementation: TMaterials1Implementation;
  protected
    { You can leave MaterialIndex as nil if you are sure that
      MaterialBinding will not be any _INDEXED value. }
    MaterialIndex: TMFLong;
    MaterialBinding: Integer;

    procedure RenderVertex(IndexNum: Integer); override;
    procedure RenderCoordsRange(const RangeNumber: Cardinal;
      BeginIndex, EndIndex: integer); override;
    procedure RenderCoordinateBegin; override;
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer);
  end;

  { Enhances TAbstractMatTexCoordinateRenderer with the ability to handle
    per-face or per-vertex VRML >= 2.0 colors.

    - Usage: set Color, ColorPerVertex and ColorIndex.

      If Color = @nil, this class will not do anything.
      Otherwise, colors will be used.

      ColorPerVertex specifies per-vertex or per-face.
      Just like for VRML 1.0, the same restrictions apply:
      - if you want per-face to work, then RenderCoordsRange must
        correspond to a single face.
      - if you want per-vertex to work, you must use smooth shading.

      ColorIndex: if set, then vertex IndexNum or face number will
      index ColorIndex, and then ColorIndex indexes Color items.
      Otherwise, for vertex we use CoordIndex (instead of ColorIndex)
      and for face we'll use just face number.

    Everything related to setting VRML 2.0
    material should be set in Render_MaterialsBegin, and everything
    related to VRML 2.0 colors is handled in this class.
    So in summary, this class takes care of everything related to
    materials / colors. }
  TAbstractColMatTexCoordinateRenderer = class(TAbstractMatTexCoordinateRenderer)
  protected
    Color: TMFVec3f;
    ColorPerVertex: boolean;
    ColorIndex: TMFLong;

    procedure RenderVertex(IndexNum: integer); override;
    procedure RenderCoordsRange(
      const RangeNumber: Cardinal;
      BeginIndex, EndIndex: Integer); override;
  end;

  TNormalsImplementation = (
    { Do nothing about normals (in TAbstractNorColMatTexCoordinateRenderer)
      class. Passing normals to OpenGL is left for descendants. }
    niNone,
    { The first item of StoredNormal specifies the one and only normal
      for the whole geometry. }
    niOverall,
    { Each vertex has it's normal vector, IndexNum specifies direct index
      to StoredNormal.

      TODO: for Triangle/Quad sets, implemented but untested yet! }
    niPerVertexNonIndexed,
    { Each vertex has it's normal vector, IndexNum specifies index to
      CoordIndex and this is an index to StoredNormal. }
    niPerVertexCoordIndexed,
    { Each vertex has it's normal vector, IndexNum specifies index to
      NormalIndex and this is an index to StoredNormal. }
    niPerVertexNormalIndexed);

  { Enhances TAbstractColMatTexCoordinateRenderer with the ability
    to use normals, both taken from user data (that is, stored in VRML file)
    and generated.

    Usage:

    - You have to set NorImplementation in descendant.
      Default value, NorImplementation = niNone, simply means that
      this class does nothing and it's your responsibility to generate
      and use normal vectors. See TNormalsImplementation for other meanings,
      and which properties from
        StoredNormal
        NormalIndex
      you also have to assign to make them work.

      For VRML 1.0, you most definitely want to set both StoredNormal
      and NormalIndex and then call NorImplementation :=
      NorImplementationFromVRML1Binding. This should take care of
      VRML 1.0 needs completely.

    - If and only if NorImplementation = niNone (either you left it as
      default, or NorImplementationFromVRML1Binding returned this,
      or you set this...)
      you have to make appropriate glNormal calls yourself.

      Normals should always point from CCW (you *do not* check here FrontFaceCcw
      field, as we *do not* call glFrontFace anywhere).

      If NorImplementation <> niNone then we handle everything
      related to normals in this class.

    Note that PerVertexXxx normals require smooth shading to work Ok.

    Note that we use FrontFaceCcw value for additional information:
    it specifies whether StoredNormal has normals pointing from CCW
    (true) or CW (false).
  }
  TAbstractNorColMatTexCoordinateRenderer = class(TAbstractColMatTexCoordinateRenderer)
  private
    { Will be set to StoredNormal or it's inverted version, to keep
      pointing from CCW. }
    CcwNormal: TDynVector3SingleArray;
  protected
    { This is calculated in constructor. Unlike similar TexImplementation,
      MatImplementation (which are calculated only in RenderCoordsBegin).
      Reasons:
      - Descendants may want to change NorImplementation. In other words,
        full automatic detection only in TAbstractNorColMatTexCoordinateRenderer
        is not done, it's possible in descendants to explicitly change this.
      - NodeLit uses this, so it must be available after creation and
        before rendering. }
    NorImplementation: TNormalsImplementation;

    function NorImplementationFromVRML1Binding(
      NormalBinding: Integer): TNormalsImplementation;

    { Returns normal vector for given vertex, identified by IndexNum
      (IndexNum has the same meaning as for RenderVertex).

      Returns normal always from
      CCW (just like we pass to OpenGL always CCW normals, since we always
      assume front face = CCW).

      Override this in descendants only to handle
      NorImplementation = niNone case. }
    procedure GetNormal(IndexNum: integer; out N: TVector3Single); virtual;

    procedure RenderCoordinateBegin; override;
    procedure RenderCoordinateEnd; override;

    { Only if NorImplementation = niNone, you can set this in descendants
      (at any time, in constructor or RenderCoordinateBegin, but not later).
      If @true, then it's guaranteed that normals for the same face will
      be equal --- this may be useful for various optimization purposes. }
    NormalsFlat: boolean;

    NormalIndex: TMFLong;
    StoredNormal: TMFVec3f;

    procedure RenderVertex(IndexNum: Integer); override;
  end;

{ TVRMLMeshRenderer ---------------------------------------------------------- }

constructor TVRMLMeshRenderer.Create(ARenderer: TVRMLOpenGLRenderer);
begin
  inherited Create;

  FRenderer := ARenderer;
  FState := Renderer.Render_State;
  FGeometry := Renderer.Render_Node;
  FAttributes := Renderer.Attributes;

  if Geometry is TNodeX3DComposedGeometryNode then
  begin
    CullBackFaces := (Geometry as TNodeX3DComposedGeometryNode).FdSolid.Value;
    FrontFaceCcw := (Geometry as TNodeX3DComposedGeometryNode).FdCcw.Value;
  end;
end;

procedure TVRMLMeshRenderer.DoBeforeGLVertex(const Vert: TVector3Single);
var
  VertGlobal, VertProjected: TVector3Single;
  VertFogDistance: Single;
begin
  { We check first, to avoid calculating VertGlobal when it's not needed. }
  if Assigned(Attributes.OnBeforeGLVertex) or
     Renderer.FogVolumetric then
  begin
    VertGlobal := MultMatrixPoint(State.Transform, Vert);

    if Renderer.FogVolumetric then
    begin
      VertProjected := PointOnLineClosestToPoint(
        ZeroVector3Single, Renderer.FogVolumetricDirection, VertGlobal);
      VertFogDistance := VectorLen(VertProjected);
      if not AreParallelVectorsSameDirection(
        VertProjected, Renderer.FogVolumetricDirection) then
        VertFogDistance := -VertFogDistance;
      { Now I want
        - VertFogDistance = FogVolumetricVisibilityStart -> 0
        - VertFogDistance = FogVolumetricVisibilityStart + X -> X
          (so that VertFogDistance = FogVolumetricVisibilityStart +
          FogVisibilityRangeScaled -> FogVisibilityRangeScaled) }
      VertFogDistance -= Renderer.FogVolumetricVisibilityStart;

      { When VertFogDistance < 0 our intention is to have no fog.
        So VertFogDistance < 0 should be equivalent to VertFogDistance = 0.
        However, OpenGL doesn't necessarily interpret it like this.

        Since factor given by glFogCoordfEXT is interpreted just like
        eye distance (i.e. it's processed by appopriate linear or exp or exp2
        equations), negative values may produce quite unexpected results
        (unless you really look at the equations).

        This is mentioned in the extension specification
        [http://oss.sgi.com/projects/ogl-sample/registry/EXT/fog_coord.txt].
        First is says:

          * Should the specified value be used directly as the fog weighting
            factor, or in place of the z input to the fog equations?

    	    As the z input; more flexible and meets ISV requests.

        ... which means that what glFogCoordfEXT gives is interpreted
        just like eye distance for normal fog (so it's e.g. affected
        by fog linear / exp / exp2 modes, affected by fog start and end values,
        etc.). Later it says:

          * Should the fog coordinate be restricted to non-negative values?

            Perhaps. Eye-coordinate distance of fragments will be
            non-negative due to clipping. Specifying explicit negative
            coordinates may result in very large computed f values, although
            they are defined to be clipped after computation.

        ... and this is precisely why specifying negative glFogCoordfEXT
        parameters is a bad idea: you don't really know what OpenGL
        implementation will do. NVidia OpenGL seems to actually assume
        that factor < 0 means the same as factor = 0, so my code
        worked OK without this "MaxTo1st(VertFogDistance, 0);" below
        (because NVidia OpenGL was actually doing it anyway).
        Mesa 3D (and Radeon, as I suspect, because similar problems
        were reported for "The Castle" on Radeon) seem to just use the negative
        value directly, which causes strange artifacts
        (see e.g. "The Castle" gate_final.wrl VRML file).

        The line "MaxTo1st(VertFogDistance, 0);" makes volumetric fog work
        OK as expected for all OpenGL implementations. }
      MaxTo1st(VertFogDistance, 0);
      glFogCoordfEXT(VertFogDistance);
    end;

    if Assigned(Attributes.OnBeforeGLVertex) then
      Attributes.OnBeforeGLVertex(Geometry, VertGlobal);
  end;
end;

procedure TVRMLMeshRenderer.DoGLVertex(const Vert: TVector3Single);
begin
  DoBeforeGLVertex(Vert);
  glVertexv(Vert);
end;

procedure TVRMLMeshRenderer.DoGLArrayElement(
  const Verts: PArray_Vector3Single; ith: TGLint);
begin
  DoBeforeGLVertex(Verts^[ith]);
  glArrayElement(ith);
end;

procedure TVRMLMeshRenderer.Render;
begin
  if CullBackFaces then
  begin
    glPushAttrib(GL_POLYGON_BIT);

    { If vertex ordering is consistent and object is SOLID than we use OpenGL's
      backface culling.

      If FrontFaceCcw then we have to cull CW faces.
      But note that we do not want to call OpenGL glFrontFace
      (see VRMLOpenGLRenderer comments for reasons), so instead of it we switch
      glCullFace. Since we assume that front = always CCW, so we know how to call
      glCullFace.
    }
    glEnable(GL_CULL_FACE);
    if FrontFaceCcw then
      glCullFace(GL_BACK) else
      glCullFace(GL_FRONT);
  end;

  try
    DoRender;
  finally

    if CullBackFaces then
      glPopAttrib;

  end;
end;

{ TAbstractCoordinateRenderer ------------------------------------------------ }

procedure TAbstractCoordinateRenderer.DoRender;
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

begin
  if Coord = nil then
    Exit;

  { Initialize vertex arrays that we we will use with indexed nodes.
    GL_CLIENT_VERTEX_ARRAY_BIT pushes state of every vertex array, including
    eventual normal and texture coord arrays (possibly set by
    RenderCoordinateBegin). }
  glPushClientAttrib(GL_CLIENT_VERTEX_ARRAY_BIT);
  try
    glVertexPointer(3, GL_FLOAT, 0, Coord.Items.Items);
    glEnableClientState(GL_VERTEX_ARRAY);

    RenderCoordinateBegin;
    try
      { Lock arrays after setting up all arrays. }
      LockArraysBegin;
      try
        RenderCoordinate;
      finally LockArraysEnd; end;
    finally RenderCoordinateEnd; end;
  finally glPopClientAttrib; end;
end;

procedure TAbstractCoordinateRenderer.RenderCoordinateBegin;
begin
  { nothing to do in this class }
end;

procedure TAbstractCoordinateRenderer.RenderCoordinateEnd;
begin
  { nothing to do in this class }
end;

procedure TAbstractCoordinateRenderer.RenderVertex(IndexNum: integer);
var
  VertexNum: integer;
begin
  { This assertion should never fail, it's the responsibility
    of the programmer. }
  Assert(IndexNum < CoordCount);

  if CoordIndex <> nil then
  begin
    VertexNum := CoordIndex.Items.Items[IndexNum];
    if VertexNum >= Coord.Count then
    begin
      VRMLNonFatalError(Format('Wrong vertex index in indexed node %s ' +
        '(not enouch points in Coordinate node defined: ' +
        'index is %d, we have only %d vertices)',
        [Geometry.NodeTypeName, VertexNum, Coord.Count]));
      Exit;
    end;
  end else
    VertexNum := IndexNum;

  DoGLArrayElement(Coord.Items.ItemsArray, VertexNum);
end;

function TAbstractCoordinateRenderer.GetVertex(IndexNum: integer): TVector3f;
var
  VertexNum: integer;
begin
  { This assertion should never fail, it's the responsibility
    of the programmer. }
  Assert(IndexNum < CoordCount);

  if CoordIndex <> nil then
  begin
    VertexNum := CoordIndex.Items.Items[IndexNum];
    if VertexNum >= Coord.Count then
    begin
      VRMLNonFatalError(Format('Wrong vertex index in indexed node %s ' +
        '(not enouch points in Coordinate node defined: ' +
        'index is %d, we have only %d vertices)',
        [Geometry.NodeTypeName, VertexNum, Coord.Count]));
      Exit(ZeroVector3Single);
    end;
  end else
    VertexNum := IndexNum;

  Result := Coord.Items.Items[VertexNum];
end;

function TAbstractCoordinateRenderer.CoordCount: Integer;
begin
  if CoordIndex <> nil then
    Result := CoordIndex.Items.Count else
    Result := Coord.Items.Count;
end;

procedure TAbstractCoordinateRenderer.RenderCoordsRange(
  const RangeNumber: Cardinal;
  BeginIndex, EndIndex: Integer);
begin
  { nothing to do in this class }
end;

procedure TAbstractCoordinateRenderer.RenderCoordsRanges;
var
  BeginIndex, EndIndex: integer;
  RangeNumber: Cardinal;
begin
  Assert(CoordIndex <> nil);
  BeginIndex := 0;
  RangeNumber := 0;
  while BeginIndex < CoordIndex.Count do
  begin
    EndIndex := BeginIndex;
    while (EndIndex < CoordIndex.Count) and
          (CoordIndex.Items.Items[EndIndex] >= 0) do
      Inc(EndIndex);
    RenderCoordsRange(RangeNumber, BeginIndex, EndIndex);
    Inc(RangeNumber);
    BeginIndex := EndIndex + 1;
  end;
end;

{ TAbstractTexCoordinateRenderer ----------------------------------------- }

{ TODO:
  - maybe move NodeTextured here now? We can query created mesh renderer
    before calling it's render!
}

procedure TAbstractTexCoordinateRenderer.RenderCoordinateBegin;

  procedure AutoTextureCoordinates;
  var
    LocalBBox: TBox3d;
    LocalBBoxSize: TVector3Single;

    procedure SetupCoordGen(out Gen: TVector4f;
      const Coord: integer; const GenStart, GenEnd: TGLfloat);

    { We want to map float from range
        LocalBBox[0, Coord]...LocalBBox[0, Coord] + LocalBBoxSize[Coord]
      to
        GenStart...GenEnd.

      For a 3D point V let's define S1 as
	S1 = (V[Coord] - LocalBBox[0, Coord]) / LocalBBoxSize[Coord]
      and so S1 is in 0..1 range, now
	S = S1 * (GenEnd - GenStart) + GenStart
      and so S is in GenStart...GenEnd range, like we wanted.

      It remains to rewrite this to a form that we can pass to OpenGL
      glTexGenfv(..., GL_OBJECT_PLANE, ...).

        S = V[Coord] * (GenEnd - GenStart) / LocalBBoxSize[Coord]
	   - LocalBBox[0, Coord] * (GenEnd - GenStart) / LocalBBoxSize[Coord]
	   + GenStart

      Simple check: for GenStart = 0, GenEnd = 1 this simplifies to
        S = V[Coord] / LocalBBoxSize[Coord] -
	    LocalBBox[0, Coord] / LocalBBoxSize[Coord]
          = (V[Coord] - LocalBBox[0, Coord]) / LocalBBoxSize[Coord]
          = S1
    }

    begin
      FillChar(Gen, SizeOf(Gen), 0);
      Gen[Coord] := (GenEnd - GenStart) / LocalBBoxSize[Coord];
      Gen[3] :=
        - LocalBBox[0, Coord] * (GenEnd - GenStart) / LocalBBoxSize[Coord]
        + GenStart;
    end;

  var
    SCoord, TCoord, C1, C2: integer;
  begin
    LocalBBox := Geometry.LocalBoundingBox(State);

    { Calculate SCoord and TCoord. Following VRML spec:
      SCoord is the coord where LocalBBoxSize is largest,
      TCoord is the second-to-largest (and if some sizes are equal,
      then X is more important than Y than Z). }
    LocalBBoxSize := Box3dSizes(LocalBBox);
    SCoord := MaxVectorCoord(LocalBBoxSize);
    RestOf3dCoords(SCoord, C1, C2);
    if LocalBboxSize[C1] >= LocalBBoxSize[C2] then
      TCoord := C1 else
      TCoord := C2;

    { Calculate STextureGen, TTextureGen. }
    SetupCoordGen(STextureGen, SCoord, 0, 1);
    SetupCoordGen(TTextureGen, TCoord, 0, LocalBBoxSize[TCoord] / LocalBBoxSize[SCoord]);

    { Setup OpenGL to generate tex coords automatically }
    glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
    glTexGenv(GL_S, GL_OBJECT_PLANE, STextureGen);
    glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
    glTexGenv(GL_T, GL_OBJECT_PLANE, TTextureGen);
    glEnable(GL_TEXTURE_GEN_S);
    glEnable(GL_TEXTURE_GEN_T);
    TexImplementation := tcAutoGenerated;
  end;

begin
  inherited;

  TexImplementation := tcNotGenerated;

  if Renderer.Render_TexCoordsNeeded then
  begin
    if TexCoord <> nil then
    begin
      if TexCoordIndex = nil then
      begin
        { This happens only for X3D non-indexed primitives:
          Triangle[Fan/Strip]Set, QuadSet. Spec says that TexCoord should be
          used just like Coord, so IndexNum indexes it directly. }
        TexImplementation := tcNonIndexed;

        Assert(CoordIndex = nil);

        { so IndexNum indexes directly Coord, and so also TexCoord. }
        glTexCoordPointer(2, GL_FLOAT, 0, TexCoord.Items.Items);
        glEnableClientState(GL_TEXTURE_COORD_ARRAY);
      end else
      if TexCoordIndex.Count >= CoordIndex.Count then
      begin
	TexImplementation := tcTexIndexed;
      end else
      begin
	{ If TexCoord <> nil but TexCoordIndex is empty then
	  - VRML 2.0 spec says that coordIndex is used
	    to index texture coordinates for IndexedFaceSet.
	  - VRML 1.0 spec says that in this case default texture
	    coordinates should be generated (that's because for
	    VRML 1.0 there is always some TexCoord <> nil,
	    so it cannot be used to produce different behavior).
          - Note that this cannot happen at all for X3D primitives
            like IndexedTriangle[Fan/Strip]Set, QuadSet, since they
            have TexCoordIndex = CoordIndex (just taken from "index" field).
        }
	if State.ParentShape <> nil then
	begin
	  { When BumpMappingMethod <> bmNone, texture coords will be
	    explicitly taken at each vertex using GetTextureCoord.
	    This glTexCoordPointer would only collide with this. }
	  if BumpMappingMethod = bmNone then
	  begin
	    glTexCoordPointer(2, GL_FLOAT, 0, TexCoord.Items.Items);
	    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
            TexImplementation := tcCoordIndexed;
	  end;
	end else
	  AutoTextureCoordinates;
      end;
    end else
      AutoTextureCoordinates;
  end;
end;

procedure TAbstractTexCoordinateRenderer.RenderCoordinateEnd;
begin
  if TexImplementation = tcAutoGenerated then
  begin
    glDisable(GL_TEXTURE_GEN_S);
    glDisable(GL_TEXTURE_GEN_T);
  end;

  inherited;
end;

function TAbstractTexCoordinateRenderer.GetTextureCoord(
  IndexNum: integer; out Tex: TVector2Single): boolean;
var
  Vertex: TVector3Single;
begin
  Result := TexImplementation <> tcNotGenerated;

  if Result then
  begin
    { This assertion should never fail, it's the responsibility
      of the programmer. Note that we don't need any TexCoordCount
      here, since IndexNum allowed for GetTextureCoord are the same
      and come from the same range as coords. }
    Assert(IndexNum < CoordCount);

    case TexImplementation of
      tcAutoGenerated:
        begin
          { Calculate texture coordinates just like OpenGL }
          Vertex := GetVertex(IndexNum);
          Tex[0] := VectorDotProduct(Vertex, STextureGen);
          Tex[1] := VectorDotProduct(Vertex, TTextureGen);
        end;
      tcTexIndexed:
        begin
          { tcTexIndexed is set only if
            TexCoordIndex.Count >= CoordIndex.Count, so the first index
            is Ok for sure. }
          Tex := TexCoord.ItemsSafe[TexCoordIndex.Items.Items[IndexNum]];
        end;
      tcCoordIndexed:
        begin
          { We already checked that IndexNum < CoordCount, so the first index
            is Ok for sure. }
          Tex := TexCoord.ItemsSafe[CoordIndex.Items.Items[IndexNum]];
        end;
      tcNonIndexed:
        begin
          Tex := TexCoord.ItemsSafe[IndexNum];
        end;
      else raise EInternalError.Create('TAbstractTexCoordinateRenderer.GetTextureCoord?');
    end;
  end;
end;

procedure TAbstractTexCoordinateRenderer.RenderVertex(indexNum: integer);
begin
  { Don't pass glTexCoordv here if BumpMappingMethod <> bmNone,
    then we'll handle passing texture coords appropriately
    ourselves. }
  if BumpMappingMethod = bmNone then
  begin
    { We could use GetTextureCoord(IndexNum) here,
      but since we have to check TexImplementation anyway,
      we do it more directly. But the result is the same as if we had used
      GetTextureCoord. }

    case TexImplementation of
      tcTexIndexed:
        glTexCoordv(TexCoord.Items.Items[TexCoordIndex.Items.Items[IndexNum]]);
      { tcNonGenerated: don't do anything, no texture coords. }
      { tcNonIndexed,
        tcCoordIndexed: glTexCoordPointer takes care of it, will be done when
          inherited will call DoGLArrayElement.
        tcAutoGenerated: glTexGenv takes care of it. }
    end;
  end;

  inherited;
end;

{ TAbstractMatTexCoordinateRenderer ------------------------------------------ }

constructor TAbstractMatTexCoordinateRenderer.Create(ARenderer: TVRMLOpenGLRenderer);
begin
  inherited;
  MaterialBinding := BIND_DEFAULT;
end;

procedure TAbstractMatTexCoordinateRenderer.RenderCoordinateBegin;

  function IndexListNotEmpty(MFIndexes: TMFLong): boolean;
  begin
    Result :=
      (MFIndexes.Count > 0) and
      { For VRML 1.0, [-1] value is default for materialIndex
        and should be treated as "empty", as far as I understand
        the spec. }
      (not ((MFIndexes.Count = 1) and (MFIndexes.Items.Items[0] = -1)));
  end;

begin
  inherited;

  { Calculate Mat1Implementation }

  Mat1Implementation := miOverall;

  case MaterialBinding of
    { BIND_OVERALL, BIND_DEFAULT: take default miOverall }
    BIND_PER_VERTEX:
      Mat1Implementation := miPerVertexCoordIndexed;
    BIND_PER_VERTEX_INDEXED:
      if IndexListNotEmpty(MaterialIndex) then
        Mat1Implementation := miPerVertexMatIndexed;
    BIND_PER_PART, BIND_PER_FACE:
      Mat1Implementation := miPerFace;
    BIND_PER_PART_INDEXED, BIND_PER_FACE_INDEXED:
      if IndexListNotEmpty(MaterialIndex) then
        Mat1Implementation := miPerFaceMatIndexed;
  end;

  if GLVersion.IsMesa and (GLVersion.MesaMajor < 6) and
     (Mat1Implementation in
     [ miPerVertexCoordIndexed,
       miPerVertexMatIndexed ]) then
  begin
    { With older Mesa (confirmed that the bug is with 5.1 and is not with
      6.4.2) when you use BindMaterial_1 inside
      RenderVertex it causes later
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

      So I'm workaround this below by just using miOverall for old Mesa
      versions. This causes incorrect rendering result, but at least
      it works and often looks "sensible".
    }
    Mat1Implementation := miOverall;
  end;

  { TODO: we handle all material bindings, but we handle BIND_PER_PART
    and BIND_PER_PART_INDEXED wrong for IndexedLineSet. }

  if Mat1Implementation = miOverall then
    Renderer.Render_BindMaterial_1(0);
end;

procedure TAbstractMatTexCoordinateRenderer.RenderVertex(IndexNum: Integer);
begin
  case Mat1Implementation of
    miPerVertexCoordIndexed:
      Renderer.Render_BindMaterial_1(CoordIndex.ItemsSafe[IndexNum]);
    miPerVertexMatIndexed:
      Renderer.Render_BindMaterial_1(MaterialIndex.ItemsSafe[IndexNum]);
  end;

  inherited;
end;

procedure TAbstractMatTexCoordinateRenderer.RenderCoordsRange(
  const RangeNumber: Cardinal;
  BeginIndex, EndIndex: Integer);
begin
  inherited;

  case Mat1Implementation of
    miPerFace:
      Renderer.Render_BindMaterial_1(RangeNumber);
    miPerFaceMatIndexed:
      Renderer.Render_BindMaterial_1(MaterialIndex.Items.Items[RangeNumber]);
  end;
end;

{ TAbstractColMatTexCoordinateRenderer --------------------------------------- }

procedure TAbstractColMatTexCoordinateRenderer.RenderVertex(IndexNum: integer);
begin
  { Implement different color per vertex here. }
  if (Color <> nil) and ColorPerVertex then
  begin
    if ColorIndex.Count <> 0 then
      Renderer.SetColor_2(Color.ItemsSafe[ColorIndex.ItemsSafe[IndexNum]]) else
      Renderer.SetColor_2(Color.ItemsSafe[CoordIndex.ItemsSafe[IndexNum]]);
  end;

  inherited;
end;

procedure TAbstractColMatTexCoordinateRenderer.RenderCoordsRange(
  const RangeNumber: Cardinal;
  BeginIndex, EndIndex: Integer);
begin
  inherited;

  { Implement different color per face here. }
  if (Color <> nil) and (not ColorPerVertex) then
  begin
    if ColorIndex.Count <> 0 then
      Renderer.SetColor_2(Color.ItemsSafe[ColorIndex.ItemsSafe[RangeNumber]]) else
      Renderer.SetColor_2(Color.ItemsSafe[RangeNumber]);
  end;
end;

{ TAbstractNorColMatTexCoordinateRenderer ----------------------------------------------------- }

function TAbstractNorColMatTexCoordinateRenderer.
  NorImplementationFromVRML1Binding(NormalBinding: Integer): TNormalsImplementation;
begin
  { TODO: not all normal bindings are supported,
    we Generate Normals if normalBinding is not supported.
    Nie zrobione : PER_PART, PER_PART_INDEXED, PER_FACE, PER_FACE_INDEXED }

  Result := niNone;
  NormalsFlat := false;

  if (StoredNormal = nil) or (NormalIndex = nil) then
    Exit;

  case NormalBinding of
    BIND_DEFAULT, BIND_PER_VERTEX_INDEXED:
      if (NormalIndex.Count > 0) and (NormalIndex.Items.Items[0] >=0) then
      begin
        Result := niPerVertexNormalIndexed;
        NormalsFlat := false;
      end;
    BIND_PER_VERTEX:
      if CoordIndex <> nil then
      begin
        Result := niPerVertexCoordIndexed;
        NormalsFlat := false;
      end;
    BIND_OVERALL:
      if StoredNormal.Count > 0 then
      begin
        Result := niOverall;
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
    Result := niNone;
    NormalsFlat := false;
  end;
end;

procedure TAbstractNorColMatTexCoordinateRenderer.GetNormal(
  IndexNum: integer; out N: TVector3Single);
begin
  case NorImplementation of
    niOverall:
      N := CcwNormal.Items[0];
    niPerVertexNonIndexed:
      N := CcwNormal.Items[IndexNum];
    niPerVertexCoordIndexed:
      N := CcwNormal.Items[CoordIndex.Items.Items[IndexNum]];
    niPerVertexNormalIndexed:
      N := CcwNormal.Items[NormalIndex.Items.Items[indexNum]];
    else
      raise EInternalError.Create('GetNormal: NorImplementation ?');
  end;
end;

procedure TAbstractNorColMatTexCoordinateRenderer.RenderVertex(IndexNum: Integer);
begin
  { If case of other NorImplementation, this will be handled elsewhere:
    - niOverall is handled in RenderCoordinateBegin
    - niNone is handled in descendants
    - niPerVertexNonIndexed with CoordIndex = nil, and
      inPerVertexCoordIndexed are also set up in RenderCoordinateBegin,
      so glDrawElement will magically do this. }

  if NorImplementation = niPerVertexNormalIndexed then
  begin
    { We could use GetNormal(IndexNum) here, but since we already checked
      NorImplementation, we do it more directly. But the result is the same
      as if we had used GetNormal. }
    glNormalv(CcwNormal.Items[NormalIndex.Items.Items[IndexNum]]);
  end;

  if (NorImplementation = niPerVertexNonIndexed) and (CoordIndex <> nil) then
  begin
    glNormalv(CcwNormal.Items[IndexNum]);
  end;

  inherited;
end;

procedure TAbstractNorColMatTexCoordinateRenderer.RenderCoordinateBegin;
begin
  inherited;

  if StoredNormal <> nil then
  begin
    if FrontFaceCcw then
      CcwNormal := StoredNormal.Items else
    begin
      CcwNormal := TDynVector3SingleArray.Create;
      CcwNormal.AssignNegated(StoredNormal.Items);
    end;
  end;

  if (NorImplementation = niPerVertexCoordIndexed) or
     ( (NorImplementation = niPerVertexNonIndexed) and
       (CoordIndex = nil) ) then
  begin
    { When IndexNum for normal works exactly like for vertex,
      we can use glNormalPointer. This is true in two cases:
      - there is no coordIndex, and normal vectors are not indexed
      - there is coordIndex, and normal vectors are indexed by coordIndex }
    glNormalPointer(GL_FLOAT, 0, CcwNormal.Items);
    glEnableClientState(GL_NORMAL_ARRAY);
  end else
  if NorImplementation = niOverall then
  begin
    glNormalv(CcwNormal.Items[0]);
  end;
end;

procedure TAbstractNorColMatTexCoordinateRenderer.RenderCoordinateEnd;
begin
  if (StoredNormal <> nil) and (not FrontFaceCcw) then
    FreeAndNil(CcwNormal);

  inherited;
end;
