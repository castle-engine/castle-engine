{
  Copyright 2002-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Utilities for VRML / X3D mesh rendering.
  This is internal for VRMLGLRenderer unit.

  Some ideas for implementation:

  - VRMLGLRenderer unit, and TVRMLGLRenderer class are already
    large and complicated. The idea of this file is to take some tasks
    from their hands. TVRMLMeshRenderer and descendants are created
    only for the time of one RenderShape call, and are used
    to render specific non-trivial VRML mesh nodes.

  - This tries to reuse and reimplement some ideas that I got
    from TGeneralIndexedRenderer. TGeneralIndexedRenderer hierarchy
    was a good idea, but it was never cleanly enough implemented.
    Time showed that cleaner implementation of such hierarchy is needed,
    to be usable for rendering *all* VRML nodes.

  - Since all classes within this file and VRMLGLRenderer live
    in one Pascal unit, we could break it and access each other's
    private fields etc. We try to not do it. We try to have a clean
    documented interface. Experience with TGeneralIndexedRenderer
    taught me that this has to be really flexible and designed for
    long-term work, to encompass many nodes and features. So, while
    this is internal for VRMLGLRenderer, it still must be implemented
    and documented cleanly.

  - In the future, this may be abstracted from OpenGL and used as a basis
    also for TVRMLGeometryNode.Triangulate methods implementation.
}

type
  TVRMLMeshRenderer = class
  private
    FRenderer: TVRMLGLRenderer;
    FShape: TVRMLShape;
    FState: TVRMLGraphTraverseState;
    FGeometry: TVRMLGeometryNode;
    FAttributes: TVRMLRenderingAttributes;
  protected
    { Current rendering properties, constant for the whole
      lifetime of the renderer, set in constructor.
      Setting Renderer automatically sets Shape, State, Geometry, Attributes. }
    property Renderer: TVRMLGLRenderer read FRenderer;
    property Shape: TVRMLShape read FShape;
    property State: TVRMLGraphTraverseState read FState;
    property Geometry: TVRMLGeometryNode read FGeometry;
    property Attributes: TVRMLRenderingAttributes read FAttributes;
  protected
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

      In this class, FrontFaceCcw is ignored if CullBackFaces = @false.

      Note that we *do not* implement these by glFrontFace, we do a little
      more complicated trick, see comments at the beginning of VRMLGLRenderer
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

    { This causes the whole Render to be done with flat shading set. }
    ForceFlatShading: boolean;

    { Render the whole geometry.

      For some details what is, and what is not yet set in OpenGL
      before this, you have to see TVRMLOpenGLRender.RenderShape
      implementation.

      Material for VRML 1.0: currently bound material is zero.
      You can call Render_BindMaterial_1 inside.
      You cannot call glMaterial or glColor here --- this is the task of
      Render_BindMaterial_1.

      Texture: you only have to generate texture coordinates in descendants.
      For all texture units < TexCoordsNeeded. (For other tex units,
      you don't have to (but you can,
      although they will be unused) generate texture coords.)
      Do not change here the bound texture or operate on texture matrix
      or texture enabled state.

      For now, see comments at the beginning of
      vrmlglrenderer_render_specificnodes.inc
      for general direction what can and what can not be used here. }
    procedure DoRender; virtual; abstract;

    procedure WarningShadingProblems(
      const ColorPerVertex, NormalPerVertex: boolean);
  public
    { Constructor.

      For descendants implementors:
      Don't do any OpenGL state changes here (initialization of OpenGL state
      should go to Render). }
    constructor Create(ARenderer: TVRMLGLRenderer);

    { Do the actual render of the whole geometry.
      Don't override this, this only sets some common things
      (like CullBackFaces) and calls DoRender. }
    procedure Render;
  public
    { Initialized in our constructor to bmNone.
      Renderer will set this to Renderer.BumpMappingMethod
      only if bump mapping should be used and BumpMappingAllowed. }
    BumpMappingMethod: TBumpMappingMethod;

    { @abstract(Will rendering of this node possibly call Renderer.SetColor,
      or use Arrays.Color?)

      In this class, returns @false, set to @true in descendants to allow
      Renderer.MaterialsBegin do some preparations (like material from color
      for lit stuff). }
    function NeedsSetColor: boolean; virtual;

    { @abstract(Will rendering of this node possibly call
      Renderer.Render_BindMaterial_1 with InsideBeginEnd = @true?)

      In this class, returns @false, set to @true in descendants to allow
      Renderer.MaterialsBegin do some preparations. }
    function NeedsBindMaterial1InsideBeginEnd: boolean; virtual;
  end;

  { Base abstract class for rendering nodes with explicit "coord" field.
    (Both IndexedXxx and not indexed).

    Geometry must be based on coordinates when using this,
    that is TVRMLGeometryNode.Coord returns @true.
    In our constructor we initialize Coord and CoordIndex
    from Geometry, using TVRMLGeometryNode.Coord and TVRMLGeometryNode.CoordIndex
    values. If Coord will be @nil, nothing will be rendered.

    Usage:
    - Don't override Render --- this is already overridden here
      to do everything necessary. Instead, override

        GenerateCoordinateBegin
        GenerateCoordinate
        GenerateCoordinateEnd

        RenderCoordinateEnd

      These are called only when Coord is assigned.
  }
  TAbstractCoordinateRenderer = class(TVRMLMeshRenderer)
  private
    FCurrentRangeNumber: Cardinal;
    FCoord: TMFVec3f;
    FCoordIndex: TMFLong;

    { Available inside GenerateCoordinate* and RenderCoordinate* }
    Arrays: TGeometryArrays;
    { Only when Arrays.Indexes = nil but original node was indexed. }
    IndexesFromCoordIndex: TDynLongIntArray;
    { Index to Arrays index.
      If CoordIndex is assigned (that is either Arrays.Indexes or
      IndexesFromCoordIndex are assigned) then points to current vertex
      in Arrays.Indexes or IndexesFromCoordIndex. If CoordIndex not assigned,
      this simply points to Arrays attributes.
      You have to call GenerateVertex in exactly the same order as you
      generated IndexesFromCoordIndex to make this valid. }
    ArrayIndexNum: Integer;
  protected
    { Coordinates, taken from Geometry.Coord.
      Usually coming from (coord as Coordinate).points field.
      If @nil then nothing will be rendered. }
    property Coord: TMFVec3f read FCoord;

    { Coordinate index, taken from Geometry.CoordIndex.

      If @nil, then GenerateVertex (and all other
      routines taking some index) will just directly index Coord
      (this is useful for non-indexed geometry, like TriangleSet
      instead of IndexedTriangleSet). }
    property CoordIndex: TMFLong read FCoordIndex;

    { Generate arrays content for given vertex.
      Given IndexNum indexes Coord, or (if CoordIndex is assigned)
      indexes CoordIndex (and CoordIndex then indexes actual Coord). }
    procedure GenerateVertex(IndexNum: Integer); virtual;

    { This gets vertex coordinate, exactly the same as would be
      rendered by GenerateVertex. Returned vertex is in local coordinate space
      (use State.Transform if you want to get global coordinates). }
    function GetVertex(IndexNum: integer): TVector3f;

    { Count of indexes. You can pass index between 0 and CoordCount - 1
      to various methods taking an index, like GenerateVertex. }
    function CoordCount: Integer;

    procedure DoRender; override;

    { Override these to plug your rendering algorithm.
      See the description of this class @className for usage overview
      of these methods.

      When overriding GenerateCoordinateBegin, always call inherited
      at the begin. When overriding GenerateCoordinateEnd, always call inherited
      at the end.

      @groupBegin }
    procedure GenerateCoordinate; virtual; abstract;
    procedure GenerateCoordinateBegin; virtual;
    procedure GenerateCoordinateEnd; virtual;
    procedure RenderCoordinateEnd; virtual;
    { @groupEnd }

    { GenerateCoordsRange is supposed to generate arrays content for
      one coordinate range.
      This is not called, not used, anywhere in this base
      TAbstractCoordinateRenderer class.
      In descendants, it may be useful to use this, like
      Geometry.MakeCoordRanges(State, @GenerateCoordsRange).

      GenerateCoordsRange is supposed to generate the parts of the mesh
      between BeginIndex and EndIndex - 1 vertices.
      BeginIndex and EndIndex are indexes to CoordIndex array,
      if CoordIndex is assigned, or just indexes to Coord. }
    procedure GenerateCoordsRange(
      const RangeNumber: Cardinal;
      BeginIndex, EndIndex: Integer); virtual;

    { The number of current range, equal to RangeNumber passed to
      GenerateCoordsRange. Read this only while in GenerateCoordsRange.
      In fact, this is just set by GenerateCoordsRange in this class
      (so call @code(inherited) first when overriding it).

      It's comfortable e.g. when you need RangeNumber inside GenerateVertex,
      and you know that GenerateVertex will be called only from
      GenerateCoordsRange. }
    property CurrentRangeNumber: Cardinal read FCurrentRangeNumber;

    { If CoordIndex assigned (this VRML/X3D node is IndexedXxx)
      then calculate and set IndexesFromCoordIndex here.
      This is also the place to set Arrays.Primitive and Arrays.Counts. }
    procedure PrepareIndexesPrimitives; virtual; abstract;

    { Called when constructing Arrays, before the Arrays.Count is set.
      Descendants can override this to do stuff like Arrays.AddColor or
      Arrays.AddAttribute('foo'). Descendants can also set AllowIndexed
      to @false, if we can't use indexed rendering (because e.g. we have
      colors per-face, which means that the same vertex position may have
      different colors,  which means it has to be duplicated in arrays anyway,
      so there's no point in indexing). }
    procedure PrepareAttributes(var AllowIndexed: boolean); virtual;
  public
    constructor Create(ARenderer: TVRMLGLRenderer);
  end;

  { Various supported texture coordinate generation methods.

    Various methods are possible, for example supported X3D
    TextureCoordinateGenerator.mode values
    (and the ProjectedTextureCoordinate).
    Also standard automatic texture coord methods (required by specification
    to be used when no texCoord value provided) may be expressed here,
    as tgBounds2d or tgBounds3d.

    Special value tgExplicit means that texture coordinates are not generated. }
  TTextureCoordinateGeneration = (
    tgExplicit2d,
    tgExplicit3d,
    tgExplicit4d,
    { Automatic 2D texture coordinate generation. Described
      in X3D specification for IndexedFaceSet without any texCoord provided. }
    tgBounds2d,
    { Automatic 3D texture coordinate generation for primitives.
      Described in X3D specification (component "Texturing3D"),
      designed to map 3D textures nicely. }
    tgBounds3d,
    tgSphere,
    tgCoord,
    tgCoordEye,
    tgCameraSpaceNormal,
    tgWorldSpaceNormal,
    tgCameraSpaceReflectionVector,
    tgWorldSpaceReflectionVector,
    tgProjection);

const
  ExplicitTexCoords = [tgExplicit2d, tgExplicit3d, tgExplicit4d];

type
  TTextureCoordsImplementation = (
    { Texture coords not generated, because not needed by Renderer. }
    tcNotGenerated,
    { All texture coords (on all texture units, for multi-texturing)
      are automatically generated.

      Actually we could use some other value
      like tcTexIndexed (anything else than tcNotGenerated) to indicate
      this, since for everything <> tcNotGenerated we actually check
      TexCoordGen to know if coords should be explicit or automatic.
      But using special tcAllGenerated feels cleaner. }
    tcAllGenerated,
    { IndexNum is an index to TexCoordIndex, as this indexes TexCoordGen/Array[TextureUnit]. }
    tcTexIndexed,
    { IndexNum is an index to CoordIndex, as this indexes TexCoordGen/Array[TextureUnit]. }
    tcCoordIndexed,
    { IndexNum is a direct index to TexCoordGen/Array[TextureUnit]. }
    tcNonIndexed);

  T3DTextureGen = array [0..2] of TVector4f;

  { Enhances TAbstractCoordinateRenderer with the ability to handle
    texture coordinate generation.

    Usage:

    - TexCoord is already set from TVRMLGeometryNode.TexCoord,
      so just be sure it's overriden for your Geometry node class.

      Don't worry, we will automatically check TexCoord class,
      and do something useful with it only if we can.
      When TexCoord = @nil,
      then we'll generate default texture coordinates,
      following VRML 2.0 / X3D IndexedFaceSet default texture coord algorithm.

      (X3D spec doesn't say what happens for nodes like
      IndexedTriangleSet when texture is specified (in Appearance.texture)
      but texture coords are not present.
      For now, we just generate texture coords for them just like for
      IndexedFaceSet.)

    - Set TexCoordIndex, if available. This works just like CoordIndex:

      If @nil, then GenerateVertex (and all other
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
    will be wanted at all (Renderer.TexCoordsNeeded).
    So you do not have to generate any texture coordinates (call
    gl[Multi]TexCoord etc.)
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

    { If one of the TexCoordGen is tgBounds2d or tgBounds3d, then
      these are vectors used to generate
      texture coords from local (object space) vertex positions.
      TextureGen[0] says how to generates S texture coord,
      TextureGen[1] says how to generates T texture coord,
      and TextureGen[2] (only for tgBounds3d) is fo R tex coord.}
    TextureGen: array of T3DTextureGen;

    { Source of texture coordinates, for each texture unit.
      These arrays have always exactly the same length.
      For some TexImplementation values, these are simply not used.

      They are used currently for
      - explicit texture coordinates
        (Then appropriate unit has Gen = tgExplicit2d and Array2d = array
        of explicit coords. Or analogous for 3d and 4d.)
      - texture coords generated by
        X3D TextureCoordinateGenerator and ProjectedTextureCoordinate
        (then Gen <> tgExplicit* (may be tgBounds* or not).
        Array* value is undefined and should not be used).
      - automatic texture coords by tgBounds2d, tgBounds3d
        (then Gen is one of tgBounds*.
        TexCoordGenNode and TexCoordArray* are undefined.)

      This has the length of TexCoordsNeeded.
      TexCoordGen/Array[0] are coords for 1st texture unit
      (that is, Renderer.FirstGLFreeTexture), and so on. }
    TexCoordGen: array of TTextureCoordinateGeneration;
    TexCoordGenNode: array of TNodeX3DTextureCoordinateNode;
    TexCoordArray2d: array of TMFVec2f;
    TexCoordArray3d: array of TMFVec3f;
    TexCoordArray4d: array of TMFVec4f;

    TexCoord: TVRMLNode;
  protected
    TexCoordIndex: TMFLong;

    { Return texture coordinate for given vertex, identified by IndexNum.
      IndexNum indexes TexCoordGen/Array[TextureUnit], or TexCoordIndex
      (if TexCoordIndex assigned),
      or CoordIndex (if TexCoordIndex assigned but empty, for IndexedFaceSet).

      Returns @false if no texture coords are available, for given
      TextureUnit.

      Works in all cases when we actually render some texture.

      Overloaded version with only TVector2Single just ignores the 3rd and
      4th texture coordinate, working only when texture coord is
      normal 2D coord. }
    function GetTextureCoord(IndexNum: integer;
      const TextureUnit: Cardinal; out Tex: TVector4Single): boolean;
    function GetTextureCoord(IndexNum: integer;
      const TextureUnit: Cardinal; out Tex: TVector2Single): boolean;

    procedure PrepareAttributes(var AllowIndexed: boolean); override;

    procedure GenerateVertex(IndexNum: Integer); override;

    procedure GenerateCoordinateBegin; override;
    procedure RenderCoordinateEnd; override;
  public
    constructor Create(ARenderer: TVRMLGLRenderer);
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
      Call UpdateMat1Implementation afterwards.
    - For VRML >= 2.0 nodes, you don't have to do anything.
      You can just leave MaterialBinding as default BIND_DEFAULT,
      but it really doesn't matter: the only effect of this class
      are calls to Render_BindMaterial_1. And Render_BindMaterial_1
      simply does nothing for VRML >= 2.0 geometry nodes.

    Since this must have a notion of what "face" is, it assumes that
    your GenerateCoordsRange constitutes rendering of a single face.
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
    { Must be set in constructor. MaterialsBegin (may someday) depend on this.

      For this reason, call UpdateMat1Implementation inside descendant
      constructor after changing this. }
    Mat1Implementation: TMaterials1Implementation;
  protected
    { You can leave MaterialIndex as nil if you are sure that
      MaterialBinding will not be any _INDEXED value.

      Be sure to change these only inside constructor,
      and call UpdateMat1Implementation afterwards. We want Mat1Implementation
      ready after constructor. }
    MaterialIndex: TMFLong;
    MaterialBinding: Integer;
    procedure UpdateMat1Implementation;

    procedure GenerateVertex(IndexNum: Integer); override;
    procedure GenerateCoordsRange(const RangeNumber: Cardinal;
      BeginIndex, EndIndex: integer); override;
    procedure GenerateCoordinateBegin; override;
  public
    constructor Create(ARenderer: TVRMLGLRenderer);
    function NeedsBindMaterial1InsideBeginEnd: boolean; override;
  end;

  { Enhances TAbstractMatTexCoordinateRenderer with the ability to handle
    per-face or per-vertex VRML >= 2.0 colors.

    - Usage: set Color or ColorRGBA (at most one of them), ColorPerVertex,
      ColorIndex.

      If Color and ColorRGBA = @nil, this class will not do anything.
      Otherwise, colors will be used.

      ColorPerVertex specifies per-vertex or per-face.
      Just like for VRML 1.0, the same restrictions apply:
      - if you want per-face to work, then GenerateCoordsRange must
        correspond to a single face.
      - if you want per-vertex to work, you must use smooth shading.

      ColorIndex: if set and non-empty, then vertex IndexNum or face number will
      index ColorIndex, and then ColorIndex indexes Color items.
      Otherwise, for vertex we use CoordIndex (if assigned, otherwise it directly
      accesses colors)
      and for face we'll use just face number.

    - You can also set RadianceTransfer
      (although we actually handle it here for all X3DComposedGeometryNode
      descendants, which is pretty enough). If set, and non-empty,
      and Attributes.OnRadianceTransfer are defined, we will use it.

      Note that assigning RadianceTransfer some non-nil value may actually
      result is setting it to nil (when we detect that RadianceTransfer should
      not be used actually).

      We will then ignore Color, ColorRGBA, ColorPerVertex, ColorIndex
      settings --- only the colors returned by OnRadianceTransfer
      will be used.

    - Renderer.Attributes.OnVertexColor, if assigned,
      will be automatically used here to calculate color for each vertex.
      If this will be assigned, then the above things
      (Color, ColorRGBA, ColorPerVertex, ColorIndex, RadianceTransfer)
      will be ignored -- only the colors returned by OnVertexColor will
      be used.

    Everything related to setting VRML 2.0
    material should be set in Render_MaterialsBegin, and everything
    related to VRML 2.0 colors is handled in this class.
    So in summary, this class takes care of everything related to
    materials / colors. }
  TAbstractColMatTexCoordinateRenderer = class(TAbstractMatTexCoordinateRenderer)
  private
    RadianceTransferVertexSize: Cardinal;
    FRadianceTransfer: TDynVector3SingleArray;
    FaceColor: TVector4Single;
    procedure SetRadianceTransfer(const Value: TDynVector3SingleArray);
  protected
    Color: TMFVec3f;
    ColorRGBA: TMFColorRGBA;
    ColorPerVertex: boolean;
    ColorIndex: TMFLong;

    property RadianceTransfer: TDynVector3SingleArray
      read FRadianceTransfer write SetRadianceTransfer;

    procedure PrepareAttributes(var AllowIndexed: boolean); override;
    procedure GenerateVertex(IndexNum: integer); override;
    procedure GenerateCoordsRange(const RangeNumber: Cardinal;
      BeginIndex, EndIndex: Integer); override;
  public
    constructor Create(ARenderer: TVRMLGLRenderer);

    function NeedsSetColor: boolean; override;
  end;

  TNormalsImplementation = (
    { Do nothing about normals (in TAbstractNorColMatTexCoordinateRenderer)
      class. Passing normals to OpenGL is left for descendants. }
    niNone,
    { The first item of Normals specifies the one and only normal
      for the whole geometry. }
    niOverall,
    { Each vertex has it's normal vector, IndexNum specifies direct index
      to Normals. }
    niPerVertexNonIndexed,
    { Each vertex has it's normal vector, IndexNum specifies index to
      CoordIndex and this is an index to Normals. }
    niPerVertexCoordIndexed,
    { Each vertex has it's normal vector, IndexNum specifies index to
      NormalIndex and this is an index to Normals. }
    niPerVertexNormalIndexed,
    { Face number is the index to Normals. }
    niPerFace,
    { Face number is the index to NormalIndex, and this indexes Normals. }
    niPerFaceNormalIndexed);

  { Enhances TAbstractColMatTexCoordinateRenderer with the ability
    to use normals, both taken from user data (that is, stored in VRML file)
    and generated.

    Usage:

    - You have to set NorImplementation in descendant.
      Default value, NorImplementation = niNone, simply means that
      this class does nothing and it's your responsibility to generate
      and use normal vectors. See TNormalsImplementation for other meanings,
      and which properties from
        Normals
        NormalsCcw (should always be set when setting Normals)
        NormalIndex
      you also have to assign to make them work.

      For VRML 1.0, you most definitely want to set both Normals
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

    Note that PerVertexXxx normals require smooth shading to work Ok. }
  TAbstractNorColMatTexCoordinateRenderer = class(TAbstractColMatTexCoordinateRenderer)
  private
    { Will be set to Normals or it's inverted version, to keep
      pointing from CCW. }
    CcwNormals: TDynVector3SingleArray;
    FaceNormal: TVector3Single;
    function CcwNormalsSafe(const Index: Integer): TVector3Single;
  protected
    NormalIndex: TMFLong;
    Normals: TDynVector3SingleArray;
    NormalsCcw: boolean;

    { This is calculated in constructor. Unlike similar TexImplementation
      (which is calculated only in RenderCoordsBegin).
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
      (IndexNum has the same meaning as for GenerateVertex) and FaceNumber
      (since normals may be available per-face, we need to know face number
      as well as vertex number).

      Returns normal always from
      CCW (just like we pass to OpenGL always CCW normals, since we always
      assume front face = CCW).

      Override this in descendants only to handle
      NorImplementation = niNone case. }
    procedure GetNormal(IndexNum: Integer; RangeNumber: Integer;
      out N: TVector3Single); virtual;

    { If @true, then it's guaranteed that normals for the same face will
      be equal. This may be useful for various optimization purposes.

      Override this in descendants only to handle
      NorImplementation = niNone case. The implementation in this class
      just derives it from NorImplementation, and for niNone answers @false
      (safer answer). }
    function NormalsFlat: boolean; virtual;

    procedure GenerateCoordinateBegin; override;
    procedure GenerateCoordinateEnd; override;
    procedure GenerateCoordsRange(const RangeNumber: Cardinal;
      BeginIndex, EndIndex: Integer); override;

    procedure PrepareAttributes(var AllowIndexed: boolean); override;

    procedure GenerateVertex(IndexNum: Integer); override;
  end;

{ TVRMLMeshRenderer ---------------------------------------------------------- }

constructor TVRMLMeshRenderer.Create(ARenderer: TVRMLGLRenderer);
begin
  inherited Create;

  FRenderer := ARenderer;
  FShape := Renderer.CurrentShape;
  FState := Renderer.CurrentState;
  FGeometry := Renderer.CurrentGeometry;
  FAttributes := Renderer.Attributes;

  if Geometry is TNodeX3DComposedGeometryNode then
  begin
    CullBackFaces := (Geometry as TNodeX3DComposedGeometryNode).FdSolid.Value;
    FrontFaceCcw := (Geometry as TNodeX3DComposedGeometryNode).FdCcw.Value;
  end;
end;

procedure TVRMLMeshRenderer.Render;
var
  PushedAttribs: TGLbitfield;
begin
  PushedAttribs := 0;

  if CullBackFaces then
    PushedAttribs := PushedAttribs or GL_POLYGON_BIT;
  if ForceFlatShading then
    PushedAttribs := PushedAttribs or GL_LIGHTING_BIT { for shade model };

  if PushedAttribs <> 0 then
    glPushAttrib(PushedAttribs);

  try
    if CullBackFaces then
    begin
      { If vertex ordering is consistent and object is SOLID than we use OpenGL's
        backface culling.

        If FrontFaceCcw then we have to cull CW faces.
        But note that we do not want to call OpenGL glFrontFace
        (see VRMLGLRenderer comments for reasons), so instead of it we switch
        glCullFace. Since we assume that front = always CCW, so we know how to call
        glCullFace.
      }
      glEnable(GL_CULL_FACE);
      if FrontFaceCcw then
        glCullFace(GL_BACK) else
        glCullFace(GL_FRONT);
    end;

    if ForceFlatShading then
    begin
      glShadeModel(GL_FLAT);
    end;

    DoRender;

  finally
    if PushedAttribs <> 0 then
      glPopAttrib;
  end;
end;

procedure TVRMLMeshRenderer.WarningShadingProblems(
  const ColorPerVertex, NormalPerVertex: boolean);
const
  SPerVertex: array [boolean] of string = ('per-face', 'per-vertex');
begin
  VRMLWarning(vwSerious, Format(
    'Colors %s and normals %s used in the same node %s. Shading results may be incorrect',
    [ SPerVertex[ColorPerVertex], SPerVertex[NormalPerVertex],
      Geometry.NodeTypeName]));
end;

function TVRMLMeshRenderer.NeedsSetColor: boolean;
begin
  Result := false;
end;

function TVRMLMeshRenderer.NeedsBindMaterial1InsideBeginEnd: boolean;
begin
  Result := false;
end;

{ TAbstractCoordinateRenderer ------------------------------------------------ }

constructor TAbstractCoordinateRenderer.Create(ARenderer: TVRMLGLRenderer);
begin
  inherited;
  Check(Geometry.Coord(State, FCoord),
    'TAbstractCoordinateRenderer is only for coordinate-based nodes');
  FCoordIndex := Geometry.CoordIndex;
end;

procedure TAbstractCoordinateRenderer.DoRender;

  { Calculate Arrays.Indexes, Arrays.Count, Arrays.Position,
    call PrepareAttributes in the middle. }
  procedure GenerateArrays;
  var
    AllowIndexed: boolean;
    MaxIndex: Integer;
  begin
    PrepareIndexesPrimitives;

    if IndexesFromCoordIndex <> nil then
    begin
      MaxIndex := IndexesFromCoordIndex.Max;

      { check do we have enough coordinates. TGeometryArrays data may be passed
        quite raw to OpenGL, so this may be our last chance to check correctess
        and avoid passing data that would cause OpenGL errors. }
      if MaxIndex >= Coord.Count then
      begin
        VRMLWarning(vwSerious, Format('Not enough coordinates: coordIndex references %d coordinates, but we only have %d coordinates',
          [MaxIndex + 1, Coord.Count]));
        Exit; { leave Arrays created but empty }
      end;
    end;

    AllowIndexed := true;
    PrepareAttributes(AllowIndexed);

    if Log then
      WritelnLog('Renderer', Format('Shape %s is rendered with indexes: %s',
        [Shape.Geometry.NodeTypeName, BoolToStr[AllowIndexed]]));

    if AllowIndexed or (IndexesFromCoordIndex = nil) then
    begin
      Arrays.Indexes := IndexesFromCoordIndex;
      IndexesFromCoordIndex := nil;

      Arrays.Count := Coord.Count;

      Coord.Items.AssignToInterleaved(Arrays.Position, Arrays.CoordinateSize);
    end else
    begin
      Arrays.Count := IndexesFromCoordIndex.Count;

      { Expand IndexesFromCoordIndex, to specify vertexes multiple times }
      Coord.Items.AssignToInterleavedIndexed(
        Arrays.Position, Arrays.CoordinateSize, IndexesFromCoordIndex);
    end;

    GenerateCoordinateBegin;
    try
      GenerateCoordinate;
    finally GenerateCoordinateEnd; end;
  end;

  procedure RenderArrays;

    var
      { This is used only by LockArraysBegin and LockArraysEnd. }
      UseLockArrays: boolean;

      procedure LockArraysBegin;
      begin
        { See
          [http://www.opengl.org/documentation/specs/version1.2/EXTspecs/compiled_vertex_array.txt]
          for description of GL_EXT_compiled_vertex_array. }
        UseLockArrays := GL_EXT_compiled_vertex_array and
          { Under Radeon X1600 (chantal), fglrx, with roNone optimization,
            rendering some shapes without indexes (like 16000heights from
            http://www.web3d.org/x3d/content/examples/Conformance/Geometry/ElevationGrid/index.html,
            or shadow_fields demo) causes invalid results
            (look like random vertexes). So disable using locking for them. }
          (not (GLVersion.IsFglrx and
                (Arrays.Indexes = nil) {TODO:and
                (not Renderer.InsideDisplayList)})) and
          { Don't use when Arrays.Count has no items,
            glLockArraysEXT(0, 0) causes OpenGL error "invalid value". }
         (Arrays.Count <> 0);
        if UseLockArrays then
          glLockArraysEXT(0, Arrays.Count);
      end;

      procedure LockArraysEnd;
      begin
        if UseLockArrays then
          glUnlockArraysEXT;
      end;

  const
    ToMode: array [TGeometryPrimitive] of TGLenum =
    ( GL_TRIANGLES, GL_QUADS, GL_TRIANGLE_FAN, GL_TRIANGLE_STRIP,
      GL_LINE_STRIP, GL_POINTS );
  var
    I: Integer;
    First: Cardinal;
    Mode: TGLenum;
  begin
    { Initialize vertex arrays that we we will use with indexed nodes.
      GL_CLIENT_VERTEX_ARRAY_BIT pushes state of every vertex array, including
      normal and texture coord arrays.

      Mesa 7.2 (at least this one) crashes awfully when using
      glPushClientAttrib(GL_CLIENT_VERTEX_ARRAY_BIT).
      The crash is a segfault, sometimes from my code, sometimes from
      within the mesa lib, sometimes from GTK... evidently, Mesa's
      glPushClientAttrib(GL_CLIENT_VERTEX_ARRAY_BIT) cripples some memory
      leading to crash at some random place later.

      To reproduce: open with view3dscene almost any non-empty model,
      like kambi_vrml_test_suite/x3d/kambi_extensions/teapot_simple.x3dv.
      Sometimes you need to open it a couple of times... and you get this crash.
      Reproducible easily on "chantal" (mac book pro) and "domek"
      (netbook MSI Wind).

      TODO: I don't know is it known bug, in what Mesa version it's produced
      and when (if) it's fixed... for now I'll just avoid doing
      glPushClientAttrib(GL_CLIENT_VERTEX_ARRAY_BIT) with Mesa.
      Yes, this makes rendering code buggy (GL_CLIENT_VERTEX_ARRAY_BIT
      "leaks out" from our VRML renderer), but it's not a problem
      in practice for my programs --- I didn't depend anywhere that
      VRML renderer preserves that glVertexPointer state.
    }
    if not GLVersion.IsMesa then
      glPushClientAttrib(GL_CLIENT_VERTEX_ARRAY_BIT);
    try
      glEnableClientState(GL_VERTEX_ARRAY);
      glVertexPointer(3, GL_FLOAT, Arrays.CoordinateSize, Arrays.Position);

      glEnableClientState(GL_NORMAL_ARRAY);
      glNormalPointer(GL_FLOAT, Arrays.CoordinateSize, Arrays.Normal);

      if (Arrays.Color <> nil) and
         Attributes.ControlMaterials and
         (not Attributes.PureGeometry) then
      begin
        glEnableClientState(GL_COLOR_ARRAY);
        glColorPointer(4, GL_FLOAT, Arrays.AttributeSize, Arrays.Color);
      end;

      if GLUseMultiTexturing then
      begin
        for I := 0 to Arrays.TexCoords.Count - 1 do
          if Arrays.TexCoords[I] <> nil then
          begin
            glClientActiveTextureARB(GL_TEXTURE0_ARB +
              Integer(Renderer.Attributes.FirstGLFreeTexture) + I);
            glEnableClientState(GL_TEXTURE_COORD_ARRAY);
            glTexCoordPointer(Arrays.TexCoords[I].Dimensions, GL_FLOAT,
              Arrays.AttributeSize,
              Arrays.TexCoord(Arrays.TexCoords[I].Dimensions, I, 0));
          end;
      end else
      if (Arrays.TexCoords.Count <> 0) and
         (Arrays.TexCoords[0] <> nil) then
      begin
        glEnableClientState(GL_TEXTURE_COORD_ARRAY);
        glTexCoordPointer(Arrays.TexCoords[0].Dimensions, GL_FLOAT,
          Arrays.AttributeSize,
          Arrays.TexCoord(Arrays.TexCoords[0].Dimensions, 0, 0));
      end;

      { Lock arrays after setting up all arrays. }
      LockArraysBegin;
      try
        Mode := ToMode[Arrays.Primitive];
        First := 0;
        if Arrays.Indexes <> nil then
        begin
          if Arrays.Counts = nil then
            glDrawElements(Mode, Arrays.Indexes.Count, GL_UNSIGNED_INT, Arrays.Indexes.Pointers[First]) else
            for I := 0 to Arrays.Counts.Count - 1 do
            begin
              glDrawElements(Mode, Arrays.Counts[I], GL_UNSIGNED_INT, Arrays.Indexes.Pointers[First]);
              First += Arrays.Counts[I];
            end;
        end else
        begin
          if Arrays.Counts = nil then
            glDrawArrays(Mode, First, Arrays.Count) else
            for I := 0 to Arrays.Counts.Count - 1 do
            begin
              glDrawArrays(Mode, First, Arrays.Counts[I]);
              First += Arrays.Counts[I];
            end;
        end;
      finally LockArraysEnd; end;
    finally
      if not GLVersion.IsMesa then
        glPopClientAttrib;
    end;

    RenderCoordinateEnd;
  end;

begin
  if Coord = nil then
    Exit;

  IndexesFromCoordIndex := nil;
  ArrayIndexNum := 0;
  Arrays := TGeometryArrays.Create;
  try
    GenerateArrays;
    RenderArrays;
  finally
    FreeAndNil(Arrays);
    FreeAndNil(IndexesFromCoordIndex);
  end;
end;

procedure TAbstractCoordinateRenderer.PrepareAttributes(var AllowIndexed: boolean);
begin
  { nothing to do in this class }
end;

procedure TAbstractCoordinateRenderer.GenerateCoordinateBegin;
begin
  { nothing to do in this class }
end;

procedure TAbstractCoordinateRenderer.GenerateCoordinateEnd;
begin
  { nothing to do in this class }
end;

procedure TAbstractCoordinateRenderer.RenderCoordinateEnd;
begin
  { nothing to do in this class }
end;

procedure TAbstractCoordinateRenderer.GenerateVertex(IndexNum: integer);

(*
  procedure SetFogColor(const Vert: TVector3Single);
  var
    VertGlobal, VertProjected: TVector3Single;
    VertFogDistance: Single;
  begin
    { We check first, to avoid calculating VertGlobal when it's not needed. }
    if Renderer.FogVolumetric then
    begin
      VertGlobal := MatrixMultPoint(State.Transform, Vert);

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
    end;
  end;
*)

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
      CoordIndex.VRMLWarning_WrongVertexIndex(
        Geometry.NodeTypeName, VertexNum, Coord.Count);
      Exit;
    end;
  end else
    VertexNum := IndexNum;

  // TODO: port here SetFogColor code

  Inc(ArrayIndexNum);
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
      CoordIndex.VRMLWarning_WrongVertexIndex(
        Geometry.NodeTypeName, VertexNum, Coord.Count);
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

procedure TAbstractCoordinateRenderer.GenerateCoordsRange(
  const RangeNumber: Cardinal; BeginIndex, EndIndex: Integer);
begin
  FCurrentRangeNumber := RangeNumber;
end;

{ TAbstractTexCoordinateRenderer ----------------------------------------- }

{ TODO:
  - maybe move NodeTextured here now? We can query created mesh renderer
    before calling it's render!
}

constructor TAbstractTexCoordinateRenderer.Create(ARenderer: TVRMLGLRenderer);
begin
  inherited;
  if not Geometry.TexCoord(State, TexCoord) then
    TexCoord := nil;
end;

procedure TAbstractTexCoordinateRenderer.PrepareAttributes(
  var AllowIndexed: boolean);

  { Is a texture used on given unit, and it's 3D texture. }
  function IsTexture3D(const TexUnit: Cardinal): boolean;
  var
    Tex: TNodeX3DTextureNode;
  begin
    Tex := State.Texture;
    Result := (
      (Tex <> nil) and
      ( ( (TexUnit = 0) and
          (Tex is TNodeX3DTexture3DNode) )
        or
        ( (Tex is TNodeMultiTexture) and
          (TNodeMultiTexture(Tex).FdTexture.Count > TexUnit) and
          (TNodeMultiTexture(Tex).FdTexture.Items[TexUnit] is TNodeX3DTexture3DNode)
        )));
  end;

var
  { Just copy of Renderer.TexCoordsNeeded, for comfort }
  TexCoordsNeeded: Cardinal;

  { Set length of TexCoordGen*, TexCoordArray* arrays. }
  procedure SetTexLengths(const Count: Integer);
  begin
    SetLength(TexCoordGen    , Count);
    SetLength(TexCoordGenNode, Count);
    SetLength(TexCoordArray2d, Count);
    SetLength(TexCoordArray3d, Count);
    SetLength(TexCoordArray4d, Count);
    SetLength(TextureGen     , Count);
  end;

  { Initialize TexCoordGen/Array.

    1. Uses TexCoord, to get usable explicit texture coordinates.
    2. If this generated any usable coords (that is,
       Length(TexCoordGen/Array) <> 0 after 1st step) then this array
       is grown/shrink using GLUseMultiTexturing/TexCoordsNeeded information.

    After this, TexCoordGen/Array is either empty (if no (valid) texture coords
    found, or not required because TexCoordsNeeded = 0) or has required
    number of items (exactly 1 for not GLUseMultiTexturing,
    exactly TexCoordsNeeded for GLUseMultiTexturing). }
  procedure InitializeTexCoordGenArray;

    function TexCoordGenFromString(const S: string; const IsTexture3D: boolean): TTextureCoordinateGeneration;
    begin
      if S = 'SPHERE' then
        Result := tgSphere else
      if S = 'COORD' then
        Result := tgCoord else
      if (S = 'COORD-EYE') or (S = 'CAMERASPACEPOSITION') then
        Result := tgCoordEye else
      if S = 'CAMERASPACENORMAL' then
        Result := tgCameraSpaceNormal else
      if S = 'WORLDSPACENORMAL' then
        Result := tgWorldSpaceNormal else
      if S = 'CAMERASPACEREFLECTIONVECTOR' then
        Result := tgCameraSpaceReflectionVector else
      if S = 'WORLDSPACEREFLECTIONVECTOR' then
        Result := tgWorldSpaceReflectionVector else
      if S = 'PROJECTION' then
        Result := tgProjection else
      if S = 'BOUNDS' then
      begin
        if IsTexture3D then
          Result := tgBounds3d else
          Result := tgBounds2d;
      end else
      if S = 'BOUNDS2D' then
        Result := tgBounds2d else
      if S = 'BOUNDS3D' then
        Result := tgBounds3d else
      begin
        Result := tgCoord;
        VRMLWarning(vwSerious, Format('Unsupported TextureCoordinateGenerator.mode: "%s", will use "COORD" instead',
          [S]));
      end;
    end;

    { Set length of TexCoordGen*, TexCoordArray* arrays.
      Also, sets to nil all TexCoordGenNode and TexCoordArray* arrays items. }
    procedure InitializeTexLengths(const Count: Integer);
    begin
      SetTexLengths(Count);
      if Count <> 0 then
      begin
        FillChar(TexCoordGenNode[0], SizeOf(Pointer) * Count, 0);
        FillChar(TexCoordArray2d[0], SizeOf(Pointer) * Count, 0);
        FillChar(TexCoordArray3d[0], SizeOf(Pointer) * Count, 0);
        FillChar(TexCoordArray4d[0], SizeOf(Pointer) * Count, 0);
      end;
    end;

  var
    MultiTexCoord: TVRMLNodesList;
    I, LastCoord: Integer;
  begin
    if TexCoord = nil then
    begin
      InitializeTexLengths(0);
      Exit;
    end else
    if TexCoord is TNodeTextureCoordinate then
    begin
      InitializeTexLengths(1);
      TexCoordGen    [0] := tgExplicit2d;
      TexCoordArray2d[0] := TNodeTextureCoordinate(TexCoord).FdPoint;
    end else
    if TexCoord is TNodeTextureCoordinate2 then
    begin
      InitializeTexLengths(1);
      TexCoordGen    [0] := tgExplicit2d;
      TexCoordArray2d[0] := TNodeTextureCoordinate2(TexCoord).FdPoint;
    end else
    if TexCoord is TNodeTextureCoordinate3D then
    begin
      InitializeTexLengths(1);
      TexCoordGen    [0] := tgExplicit3d;
      TexCoordArray3d[0] := TNodeTextureCoordinate3D(TexCoord).FdPoint;
    end else
    if TexCoord is TNodeTextureCoordinate4D then
    begin
      InitializeTexLengths(1);
      TexCoordGen    [0] := tgExplicit4d;
      TexCoordArray4d[0] := TNodeTextureCoordinate4D(TexCoord).FdPoint;
    end else
    if TexCoord is TNodeTextureCoordinateGenerator then
    begin
      InitializeTexLengths(1);
      TexCoordGenNode[0] := TNodeTextureCoordinateGenerator(TexCoord);
      TexCoordGen    [0] := TexCoordGenFromString(TNodeTextureCoordinateGenerator(TexCoord).FdMode.Value,
        IsTexture3D(0));
    end else
    if TexCoord is TNodeProjectedTextureCoordinate then
    begin
      InitializeTexLengths(1);
      TexCoordGenNode[0] := TNodeProjectedTextureCoordinate(TexCoord);
      TexCoordGen    [0] := tgProjection;
    end else
    if TexCoord is TNodeMultiTextureCoordinate then
    begin
      MultiTexCoord := TNodeMultiTextureCoordinate(TexCoord).FdTexCoord.Items;
      InitializeTexLengths(MultiTexCoord.Count);
      for I := 0 to MultiTexCoord.Count - 1 do
      begin
        if (MultiTexCoord.Items[I] <> nil) and
           (MultiTexCoord.Items[I] is TNodeTextureCoordinate) then
        begin
          TexCoordGen    [I] := tgExplicit2d;
          TexCoordArray2d[I] := TNodeTextureCoordinate(MultiTexCoord.Items[I]).FdPoint;
        end else
        if (MultiTexCoord.Items[I] <> nil) and
           (MultiTexCoord.Items[I] is TNodeTextureCoordinate3D) then
        begin
          TexCoordGen    [I] := tgExplicit3d;
          TexCoordArray3d[I] := TNodeTextureCoordinate3D(MultiTexCoord.Items[I]).FdPoint;
        end else
        if (MultiTexCoord.Items[I] <> nil) and
           (MultiTexCoord.Items[I] is TNodeTextureCoordinate4D) then
        begin
          TexCoordGen    [I] := tgExplicit4d;
          TexCoordArray4d[I] := TNodeTextureCoordinate4D(MultiTexCoord.Items[I]).FdPoint;
        end else
        if (MultiTexCoord.Items[I] <> nil) and
           (MultiTexCoord.Items[I] is TNodeTextureCoordinateGenerator) then
        begin
          TexCoordGenNode[I] := TNodeTextureCoordinateGenerator(MultiTexCoord.Items[I]);
          TexCoordGen    [I] := TexCoordGenFromString(
            TNodeTextureCoordinateGenerator(MultiTexCoord.Items[I]).FdMode.Value,
            IsTexture3D(I));
        end else
        if (MultiTexCoord.Items[I] <> nil) and
           (MultiTexCoord.Items[I] is TNodeProjectedTextureCoordinate) then
        begin
          TexCoordGenNode[I] := TNodeProjectedTextureCoordinate(MultiTexCoord.Items[I]);
          TexCoordGen    [I] := tgProjection;
        end else
        begin
          SetTexLengths(0);
          if MultiTexCoord.Items[I] <> nil then
            VRMLWarning(vwSerious, Format('Unsupported texture coordinate node: %s, inside multiple texture coordinate', [MultiTexCoord.Items[I].NodeTypeName])) else
            VRMLWarning(vwSerious, 'NULL texture coordinate node');
          Exit;
        end;
      end;
    end else
    begin
      SetTexLengths(0);
      VRMLWarning(vwSerious, Format('Unsupported texture coordinate node: %s', [TexCoord.NodeTypeName]));
      Exit;
    end;

    Assert(Length(TexCoordGen) = Length(TexCoordArray2d));
    Assert(Length(TexCoordGen) = Length(TexCoordArray3d));
    Assert(Length(TexCoordGen) = Length(TexCoordArray4d));

    { Now fix using GLUseMultiTexturing, TexCoordsNeeded. }
    if Length(TexCoordGen) <> 0 then
    begin
      if GLUseMultiTexturing then
      begin
        if Cardinal(Length(TexCoordGen)) > TexCoordsNeeded then
        begin
          SetTexLengths(TexCoordsNeeded);
        end else
        if Cardinal(Length(TexCoordGen)) < TexCoordsNeeded then
        begin
          LastCoord := High(TexCoordGen);
          SetTexLengths(TexCoordsNeeded);

          { We copy TexCoordArray*[LastCoord] to all following items.
            This way we do what X3D spec says:
            - if non-MultiTextureCoordinate is used for multitexturing,
              channel 0 is replicated
            - if MultiTextureCoordinate is used, but with too few items,
              last channel is replicated. }
          for I := LastCoord + 1 to TexCoordsNeeded - 1 do
          begin
            TexCoordGen    [I] := TexCoordGen    [LastCoord];
            TexCoordArray2d[I] := TexCoordArray2d[LastCoord];
            TexCoordArray3d[I] := TexCoordArray3d[LastCoord];
            TexCoordArray4d[I] := TexCoordArray4d[LastCoord];
          end;
        end;

        Assert(Cardinal(Length(TexCoordArray2d)) = TexCoordsNeeded);
        Assert(Cardinal(Length(TexCoordArray3d)) = TexCoordsNeeded);
        Assert(Cardinal(Length(TexCoordArray4d)) = TexCoordsNeeded);
      end else
      begin
        { I know at this point that TexCoordArray has at least one item.
          So leave *only* this item, if not GLUseMultiTexturing. }
        SetTexLengths(1);
      end;
    end;

    Assert(Length(TexCoordGen) = Length(TexCoordArray2d));
    Assert(Length(TexCoordGen) = Length(TexCoordArray3d));
    Assert(Length(TexCoordGen) = Length(TexCoordArray4d));
  end;

  { Setup 2D texture coordinates suitable for IndexedFaceSet without
    explicit texture coords, following X3D 3D Texturing spec.

    When TexUnitAll then we set up and enable OpenGL
    glTexGen correctly on all texture units within TexCoordsNeeded.
    We also set TexImplementation := tcAllGenerated,
    TexCoordGen (to tgBounds2d), TextureGen.

    Whem not TexUnitAll, we only set up and enable OpenGL
    glTexGen on current unit. }
  procedure Bounds2DTextureGen(const TexUnitAll: boolean);

    procedure DoTexUnit(const TexGenVectors: T3DTextureGen);
    begin
      { Setup OpenGL to generate tex coords automatically }
      glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
      glTexGenv(GL_S, GL_OBJECT_PLANE, TexGenVectors[0]);
      glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
      glTexGenv(GL_T, GL_OBJECT_PLANE, TexGenVectors[1]);
      glEnable(GL_TEXTURE_GEN_S);
      glEnable(GL_TEXTURE_GEN_T);
    end;

  var
    LocalBBox: TBox3D;
    LocalBBoxSize: TVector3Single;

    { Setup and enable glTexGen to make automatic 2D texture coords
      based on shape bounding box. On texture unit 0. }
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
    SCoord, TCoord, I: integer;
    TexGenVectors: T3DTextureGen;
  begin
    { calculate TexGenVectors first }

    LocalBBox := Shape.LocalBoundingBox;

    if not IsEmptyBox3d(LocalBBox) then
    begin
      LocalBBoxSize := Box3DSizes(LocalBBox);

      Geometry.GetTextureBounds2DST(LocalBBoxSize, SCoord, TCoord);

      { Calculate TextureGen[0..1]. }
      SetupCoordGen(TexGenVectors[0], SCoord, 0, 1);
      SetupCoordGen(TexGenVectors[1], TCoord, 0, LocalBBoxSize[TCoord] / LocalBBoxSize[SCoord]);
    end else
    begin
      { When local bounding box is empty, set these to any sensible value }
      TexGenVectors[0] := Vector4Single(1, 0, 0, 0);
      TexGenVectors[1] := Vector4Single(0, 1, 0, 0);
    end;

    TexGenVectors[2] := Vector4Single(0, 0, 1, 0); //< whatever, just to be defined.

    { use TexGenVectors on relevant tex units }

    if TexUnitAll then
    begin
      TexImplementation := tcAllGenerated;
      SetTexLengths(TexCoordsNeeded);
      for I := 0 to TexCoordsNeeded - 1 do
      begin
        TexCoordGen[I] := tgBounds2d;
        TextureGen[I] := TexGenVectors;
        Renderer.ActiveTexture(I);
        DoTexUnit(TexGenVectors);
      end;
    end else
      DoTexUnit(TexGenVectors);
  end;

  { Setup 3D texture coordinates suitable for "primitives without
    explicit texture coordinates", following X3D 3D Texturing spec.

    When TexUnitAll then we set up and enable OpenGL
    glTexGen correctly on all texture units within TexCoordsNeeded.
    We also set TexImplementation := tcAllGenerated,
    TexCoordGen (to tgBounds3d), TextureGen.

    Whem not TexUnitAll, we only set up and enable OpenGL
    glTexGen on current unit. }
  procedure Bounds3DTextureGen(const TexUnitAll: boolean);

    procedure DoTexUnit(const TexGenVectors: T3DTextureGen);
    begin
      glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
      glTexGenv(GL_S, GL_OBJECT_PLANE, TexGenVectors[0]);
      glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
      glTexGenv(GL_T, GL_OBJECT_PLANE, TexGenVectors[1]);
      glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
      glTexGenv(GL_R, GL_OBJECT_PLANE, TexGenVectors[2]);
      glEnable(GL_TEXTURE_GEN_S);
      glEnable(GL_TEXTURE_GEN_T);
      glEnable(GL_TEXTURE_GEN_R);
    end;

  var
    Box: TBox3D;
    XStart, YStart, ZStart, XSize, YSize, ZSize: Single;
    I: Integer;
    TexGenVectors: T3DTextureGen;
  begin
    { calculate TexGenVectors first }

    Box := Shape.LocalBoundingBox;

    if not IsEmptyBox3d(Box) then
    begin

      { It's easiest to generate them with glTexGen.

        Texture S should range from 0..1 when X changes from X1 .. X2.
        So S = X / (X2 - X1) - X1 / (X2 - X1).
        Same for T.
        For R, X3D spec says that coords go backwards, so just SwapValues.
      }

      SwapValues(Box[0][2], Box[1][2]);

      XStart := Box[0][0];
      YStart := Box[0][1];
      ZStart := Box[0][2];

      XSize := Box[1][0] - Box[0][0];
      YSize := Box[1][1] - Box[0][1];
      ZSize := Box[1][2] - Box[0][2];

      TexGenVectors[0] := Vector4Single(1 / XSize, 0, 0, - XStart / XSize);
      TexGenVectors[1] := Vector4Single(0, 1 / YSize, 0, - YStart / YSize);
      TexGenVectors[2] := Vector4Single(0, 0, 1 / ZSize, - ZStart / ZSize);
    end else
    begin
      { When local bounding box is empty, set these to any sensible value }
      TexGenVectors[0] := Vector4Single(1, 0, 0, 0);
      TexGenVectors[1] := Vector4Single(0, 1, 0, 0);
      TexGenVectors[2] := Vector4Single(0, 0, 1, 0);
    end;

    { use TexGenVectors on relevant tex units }

    if TexUnitAll then
    begin
      TexImplementation := tcAllGenerated;
      SetTexLengths(TexCoordsNeeded);
      for I := 0 to TexCoordsNeeded - 1 do
      begin
        TexCoordGen[I] := tgBounds3d;
        TextureGen[I] := TexGenVectors;
        Renderer.ActiveTexture(I);
        DoTexUnit(TexGenVectors);
      end;
    end else
      DoTexUnit(TexGenVectors);
  end;

  { Set texture generation, for all texture units
    (required from TexCoordsNeeded GLUseMultiTexturing) where
    TexCoordGen[] <> tgExplicit.

    Call this only if InitializeTexCoordGenArray generated some non-empty
    TexCoordGen/Array. }
  procedure EnableTextureCoordGen;

    { GenNode may be TextureCoordinateGenerator or ProjectedTextureCoordinate }
    procedure SetupTextureGen(const TexUnit: Cardinal;
      const Gen: TTextureCoordinateGeneration;
      const GenNode: TNodeX3DTextureCoordinateNode);

      { Do the necessary things for WorldSpaceNormal and
        WorldSpaceReflectionVector. }
      procedure TextureTransformToWorldSpace(const SuccessName, FailName: string);
      begin
        { Set the 4th texture coord component (Q) to 0.0.
          Reason: We use matrix 4x4, so it will transform whole texture
          coordinate (4-valued vector, since we're in homogeneous
          coordinates). We want the generated STR to be transformed
          like a direction, so we want to set Q = always 0.0
          (in homogeneous coords, this means that vector represents
          a direction and will be accordingly transformed). }
        Renderer.MultiTexCoord(TexUnit, ZeroVector4Single);

        glMatrixMode(GL_TEXTURE);
        Renderer.PushTextureUnit(TexUnit);
        { CameraMatrix transforms from world space to camera space,
          we want to transform directions from camera space to world
          space below. So inverted CameraMatrix is exactly what we need. }
        RenderState.CameraInverseMatrixNeeded;
        glMultMatrix(RenderState.CameraInverseMatrix);
        glMatrixMode(GL_MODELVIEW);

        if GLVersion.IsMesa and Log then
          { Mesa 7.0.4 makes textures strange (like each cube face was
            1-pixel wide?, although I checked --- they are loaded with
            correct sizes). Output this on log, since it's done by renderer,
            potentially many times for a frame.
            Nothing more intelligent now than warning about this is done. }
          WritelnLog('TexCoord', Format('Warning: Transforming tex coords to WORLDSPACE* under Mesa3D may mess up static cube maps (ImageCubeMapTexture and ComposedCubeMapTexture, but not GeneratedCubeMapTexure)',
            [SuccessName, FailName]));
      end;

      { Get projector matrices from a TextureCoordinateGenerator or
        ProjectedTextureCoordinate node.
        If this projector is a correct light source or viewpoint,
        then get it's projection and modelview matrix and return @true.
        Returns @false (and does correct VRMLWarning)
        if it's empty or incorrect. }
      function GetProjectorMatrices(Generator: TVRMLNode;
        out Projection, Modelview: TMatrix4Single): boolean;
      var
        ProjectorValue: TVRMLNode; { possible ProjectorLight or ProjectorViewpoint }
        ProjectorLight: TNodeX3DLightNode;
        ProjectorViewpoint: TNodeX3DViewpointNode;
      begin
        Result := false;

        if GenNode is TNodeTextureCoordinateGenerator then
        begin
          ProjectorValue := TNodeTextureCoordinateGenerator(GenNode).FdProjectedLight.Value;
          if (ProjectorValue <> nil) and
             (ProjectorValue is TNodeX3DLightNode) then
	  begin
            ProjectorLight := TNodeX3DLightNode(ProjectorValue);
	    Projection := ProjectorLight.ProjectionMatrix;
	    Modelview := ProjectorLight.ModelviewMatrix;
	    Result := true;
	  end else
            VRMLWarning(vwSerious, 'Using TextureCoordinateGenerator.mode = "PROJECTION", but TextureCoordinateGenerator.projectedLight is NULL or incorrect');
        end else
        if GenNode is TNodeProjectedTextureCoordinate then
        begin
          ProjectorValue := TNodeProjectedTextureCoordinate(GenNode).FdProjector.Value;
          if (ProjectorValue <> nil) and
             (ProjectorValue is TNodeX3DLightNode) then
	  begin
            ProjectorLight := TNodeX3DLightNode(ProjectorValue);
	    Projection := ProjectorLight.ProjectionMatrix;
	    Modelview := ProjectorLight.ModelviewMatrix;
	    Result := true;
	  end else
          if (ProjectorValue <> nil) and
             (ProjectorValue is TNodeX3DViewpointNode) then
	  begin
            ProjectorViewpoint := TNodeX3DViewpointNode(ProjectorValue);
	    Projection := ProjectorViewpoint.ProjectionMatrix;
	    Modelview := ProjectorViewpoint.ModelviewMatrix;
	    Result := true;
	  end else
            VRMLWarning(vwSerious, 'ProjectedTextureCoordinate.projector is NULL or incorrect');
        end else
          { This should not actually happen (GenNode passed here should be like this) }
          VRMLWarning(vwSerious, 'Invalid texture generator node');
      end;

    const
      ProjectionScalingMatrix: TMatrix4Single =
      ( (0.5,   0,   0, 0),
        (  0, 0.5,   0, 0),
        (  0,   0, 0.5, 0),
        (0.5, 0.5, 0.5, 1) );
    var
      ProjectionM: TMatrix4Single;
      ProjectorProjectionMatrix, ProjectorModelviewMatrix: TMatrix4Single;
    begin
      case Gen of
        tgSphere:
          begin
            glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
            glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
            glEnable(GL_TEXTURE_GEN_S);
            glEnable(GL_TEXTURE_GEN_T);
          end;
        tgCoord:
          begin
            glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
            glTexGenv(GL_S, GL_OBJECT_PLANE, Vector4Single(1, 0, 0, 0));
            glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
            glTexGenv(GL_T, GL_OBJECT_PLANE, Vector4Single(0, 1, 0, 0));
            glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
            glTexGenv(GL_R, GL_OBJECT_PLANE, Vector4Single(0, 0, 1, 0));
            glEnable(GL_TEXTURE_GEN_S);
            glEnable(GL_TEXTURE_GEN_T);
            glEnable(GL_TEXTURE_GEN_R);
          end;
        tgCoordEye:
          begin
            { Vectors given to EYE_PLANE are transformed by inverted
              modelview matrix (at the moment of glTexGen call).
              We don't want to transform them, so just temporarily setting
              modelview matrix to identity makes things work Ok. }
            glPushMatrix;
              glLoadIdentity;
              glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
              glTexGenv(GL_S, GL_EYE_PLANE, Vector4Single(1, 0, 0, 0));
              glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
              glTexGenv(GL_T, GL_EYE_PLANE, Vector4Single(0, 1, 0, 0));
              glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
              glTexGenv(GL_R, GL_EYE_PLANE, Vector4Single(0, 0, 1, 0));
            glPopMatrix;
            glEnable(GL_TEXTURE_GEN_S);
            glEnable(GL_TEXTURE_GEN_T);
            glEnable(GL_TEXTURE_GEN_R);
          end;
        tgCameraSpaceNormal,
        tgWorldSpaceNormal:
          begin
            if not GL_ARB_texture_cube_map then
            begin
              VRMLWarning(vwSerious, 'ARB_texture_cube_map not supported by your OpenGL implementation, cannot generate texture coordinates with mode = "CAMERASPACENORMAL", will do "COORD" instead');
              glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
              glTexGenv(GL_S, GL_OBJECT_PLANE, Vector4Single(1, 0, 0, 0));
              glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
              glTexGenv(GL_T, GL_OBJECT_PLANE, Vector4Single(0, 1, 0, 0));
              glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
              glTexGenv(GL_R, GL_OBJECT_PLANE, Vector4Single(0, 0, 1, 0));
            end else
            begin
              glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP_ARB);
              glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP_ARB);
              glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP_ARB);

              if Gen = tgWorldSpaceNormal then
                TextureTransformToWorldSpace('WORLDSPACENORMAL', 'CAMERASPACENORMAL');
            end;

            glEnable(GL_TEXTURE_GEN_S);
            glEnable(GL_TEXTURE_GEN_T);
            glEnable(GL_TEXTURE_GEN_R);
          end;
        tgCameraSpaceReflectionVector,
        tgWorldSpaceReflectionVector:
          begin
            if not GL_ARB_texture_cube_map then
            begin
              VRMLWarning(vwSerious, 'ARB_texture_cube_map not supported by your OpenGL implementation, cannot generate texture coordinates with mode = "CAMERASPACEREFLECTIONVECTOR", will do "COORD" instead');
              glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
              glTexGenv(GL_S, GL_OBJECT_PLANE, Vector4Single(1, 0, 0, 0));
              glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
              glTexGenv(GL_T, GL_OBJECT_PLANE, Vector4Single(0, 1, 0, 0));
              glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
              glTexGenv(GL_R, GL_OBJECT_PLANE, Vector4Single(0, 0, 1, 0));
            end else
            begin
              glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP_ARB);
              glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP_ARB);
              glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP_ARB);

              if Gen = tgWorldSpaceReflectionVector then
                TextureTransformToWorldSpace('WORLDSPACEREFLECTIONVECTOR', 'CAMERASPACEREFLECTIONVECTOR');
            end;

            glEnable(GL_TEXTURE_GEN_S);
            glEnable(GL_TEXTURE_GEN_T);
            glEnable(GL_TEXTURE_GEN_R);
          end;
        tgProjection:
          begin
            if GetProjectorMatrices(GenNode,
              ProjectorProjectionMatrix, ProjectorModelviewMatrix) then
            begin
              glPushMatrix;
                glLoadMatrix(RenderState.CameraMatrix);
                ProjectionM := MatrixMult(
                  ProjectionScalingMatrix,
                  MatrixMult(
                    ProjectorProjectionMatrix,
                    ProjectorModelviewMatrix));

                glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
                glTexGenv(GL_S, GL_EYE_PLANE, MatrixRow(ProjectionM, 0));

                glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
                glTexGenv(GL_T, GL_EYE_PLANE, MatrixRow(ProjectionM, 1));

                glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
                glTexGenv(GL_R, GL_EYE_PLANE, MatrixRow(ProjectionM, 2));

                glTexGeni(GL_Q, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
                glTexGenv(GL_Q, GL_EYE_PLANE, MatrixRow(ProjectionM, 3));
              glPopMatrix;
              glEnable(GL_TEXTURE_GEN_S);
              glEnable(GL_TEXTURE_GEN_T);
              glEnable(GL_TEXTURE_GEN_R);
              glEnable(GL_TEXTURE_GEN_Q);
            end;
          end;
        tgBounds2d: Bounds2DTextureGen(false);
        tgBounds3d: Bounds3DTextureGen(false);
        else raise EInternalError.Create('SetupTextureGen - Gen?');
      end;
    end;

  var
    I: Integer;
  begin
    if GLUseMultiTexturing then
    begin
      for I := 0 to TexCoordsNeeded - 1 do
        if not (TexCoordGen[I] in ExplicitTexCoords) then
        begin
          Renderer.ActiveTexture(I);
          SetupTextureGen(I, TexCoordGen[I], TexCoordGenNode[I]);
        end;
    end else
    if not (TexCoordGen[0] in ExplicitTexCoords) then
    begin
      SetupTextureGen(0, TexCoordGen[0], TexCoordGenNode[0]);
    end;
  end;

  procedure AddExplicitTextureCoordAttributes;
  var
    I: Integer;
  begin
    for I := 0 to High(TexCoordGen) do
      case TexCoordGen[I] of
        tgExplicit2d: Arrays.AddTexCoord2D(I);
        tgExplicit3d: Arrays.AddTexCoord3D(I);
        tgExplicit4d: Arrays.AddTexCoord4D(I);
      end;
  end;

begin
  inherited;

  TexImplementation := tcNotGenerated;

  TexCoordsNeeded := Renderer.TexCoordsNeeded;

  { Make sure they are initially empty, since we check this afterwards. }
  SetLength(TexCoordGen    , 0);
  SetLength(TexCoordArray2d, 0);
  SetLength(TexCoordArray3d, 0);
  SetLength(TexCoordArray4d, 0);

  if TexCoordsNeeded > 0 then
  begin
    if { Original shape node is a primitive, without explicit texCoord }
       Shape.OriginalGeometry.AutoGenerate3DTexCoords and
       { First texture is 3D texture }
       IsTexture3D(0) then
    begin
      Bounds3DTextureGen(true);
    end else
    begin
      InitializeTexCoordGenArray;
      if Length(TexCoordGen) > 0 then
      begin
        if TexCoordIndex = nil then
        begin
          { This happens only for X3D non-indexed primitives:
            Triangle[Fan/Strip]Set, QuadSet. Spec says that TexCoord should be
            used just like Coord, so IndexNum indexes it directly. }
          TexImplementation := tcNonIndexed;

          Assert(CoordIndex = nil);

          AddExplicitTextureCoordAttributes;
          EnableTextureCoordGen;
        end else
        if TexCoordIndex.Count >= CoordIndex.Count then
        begin
          TexImplementation := tcTexIndexed;

          AddExplicitTextureCoordAttributes;
          EnableTextureCoordGen;
        end else
        begin
          { If TexCoord <> nil (non-empty TexCoordArray guarantees this)
            but TexCoordIndex is empty then
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
          if State.ShapeNode <> nil then
          begin
            TexImplementation := tcCoordIndexed;

            { When BumpMappingMethod <> bmNone, texture coords will be
              explicitly taken at each vertex using GetTextureCoord.
              glTexCoordPointer (inside EnableTextureCoordArray) would only collide with this. }
            if BumpMappingMethod = bmNone then
            begin
              AddExplicitTextureCoordAttributes;
              EnableTextureCoordGen;
            end;
          end else
            Bounds2DTextureGen(true);
        end;
      end else
        Bounds2DTextureGen(true);
    end;
  end;

  if TexImplementation in [tcTexIndexed, tcNonIndexed] then
    AllowIndexed := false;
end;

procedure TAbstractTexCoordinateRenderer.GenerateCoordinateBegin;

  { Set explicit coordinates as gl[Multi]TexCoordPointer, for all texture units
    (required from TexCoordsNeeded GLUseMultiTexturing) where
    TexCoordGen[] = tgExplicit*.

    Call this only if InitializeTexCoordGenArray generated some non-empty
    TexCoordGen/Array. }
  procedure EnableTextureCoordArray;
  var
    I: Integer;

    procedure Handle(const Dimensions: TTexCoordDimensions;
      TexCoordArray: TDynArrayBase);
    var
      A: Pointer;
    begin
      A := Arrays.TexCoord(Dimensions, I, 0);

      if TexImplementation = tcCoordIndexed then
      begin
        if Arrays.Indexes <> nil then
          TexCoordArray.AssignToInterleaved(A, Arrays.AttributeSize) else
          TexCoordArray.AssignToInterleavedIndexed(A, Arrays.AttributeSize, IndexesFromCoordIndex);
      end else
      begin
        Assert(TexImplementation = tcNonIndexed);
        Assert(CoordIndex = nil); { tcNonIndexed happens only for non-indexed triangle/quad primitives }
        Assert(Arrays.Indexes = nil);

        if TexCoordArray.Count = Arrays.Count then
          TexCoordArray.AssignToInterleaved(A, Arrays.AttributeSize) else
          VRMLWarning(vwSerious, Format('Texture coordinates count invalid for %s. Expected %d, got %d',
            [Geometry.NodeTypeName, Arrays.Count, TexCoordArray.Count]));
      end;
    end;

  begin
    for I := 0 to Renderer.TexCoordsNeeded - 1 do
      case TexCoordGen[I] of
        tgExplicit2d: Handle(2, TexCoordArray2d[I].Items);
        tgExplicit3d: Handle(3, TexCoordArray3d[I].Items);
        tgExplicit4d: Handle(4, TexCoordArray4d[I].Items);
      end;
  end;

begin
  inherited;

  if (BumpMappingMethod = bmNone) and
     (TexImplementation in [tcNonIndexed, tcCoordIndexed]) then
    EnableTextureCoordArray;
end;

procedure TAbstractTexCoordinateRenderer.RenderCoordinateEnd;

  procedure DisableTextureGen(const TextureUnit: Cardinal);
  begin
    Renderer.ActiveTexture(TextureUnit);
    glDisable(GL_TEXTURE_GEN_S);
    glDisable(GL_TEXTURE_GEN_T);
    glDisable(GL_TEXTURE_GEN_R);
    glDisable(GL_TEXTURE_GEN_Q);
  end;

var
  I: Integer;
begin
  { Disable OpenGL GL_TEXTURE_GEN_x, if was (possibly)
    enabled by PrepareAttributes. }
  if Renderer.TexCoordsNeeded > 0 then
  begin
    if GLUseMultiTexturing then
    begin
      for I := 0 to Renderer.TexCoordsNeeded - 1 do
        if not (TexCoordGen[I] in ExplicitTexCoords) then
          DisableTextureGen(I);
    end else
    if not (TexCoordGen[0] in ExplicitTexCoords) then
      DisableTextureGen(0);
  end;

  inherited;
end;

function TAbstractTexCoordinateRenderer.GetTextureCoord(
  IndexNum: integer; const TextureUnit: Cardinal;
  out Tex: TVector4Single): boolean;

  function GenerateTexCoord(const Gen: TTextureCoordinateGeneration): TVector4Single;
  var
    Vertex: TVector3Single;
  begin
    case Gen of
      tgBounds2d:
        begin
          Vertex := GetVertex(IndexNum);
          Result[0] := VectorDotProduct(Vertex, TextureGen[TextureUnit][0]);
          Result[1] := VectorDotProduct(Vertex, TextureGen[TextureUnit][1]);
          Result[2] := 0;
          Result[3] := 1;
        end;
      tgBounds3d:
        begin
          Vertex := GetVertex(IndexNum);
          Result[0] := VectorDotProduct(Vertex, TextureGen[TextureUnit][0]);
          Result[1] := VectorDotProduct(Vertex, TextureGen[TextureUnit][1]);
          Result[2] := VectorDotProduct(Vertex, TextureGen[TextureUnit][2]);
          Result[3] := 1;
        end;
      tgCoord:
        begin
          Vertex := GetVertex(IndexNum);
          Result[0] := Vertex[0];
          Result[1] := Vertex[1];
          Result[2] := Vertex[2];
          Result[3] := 1;
        end;
      else VRMLWarning(vwSerious, Format('Generating on CPU texture coordinates with %d not implemented yet',
        [Gen]));
    end;
  end;

  { TTextureCoordinateGeneration must be one of tgExplicit* constants.
    This sets Tex to TexCoordArray*[TextureUnit][Index] value. }
  procedure SetTexFromTexCoordArray(const Gen: TTextureCoordinateGeneration;
    const Index: Integer);
  var
    Tex2d: TVector2Single absolute Tex;
    Tex3d: TVector3Single absolute Tex;
  begin
    case Gen of
      tgExplicit2d: Tex2d := TexCoordArray2d[TextureUnit].ItemsSafe[Index];
      tgExplicit3d: Tex3d := TexCoordArray3d[TextureUnit].ItemsSafe[Index];
      tgExplicit4d: Tex   := TexCoordArray4d[TextureUnit].ItemsSafe[Index];
    end;
  end;

begin
  Result := TexImplementation <> tcNotGenerated;

  if Result then
  begin
    { This assertion should never fail, it's the responsibility
      of the programmer. Note that we don't need any TexCoordCount
      here, since IndexNum allowed for GetTextureCoord are the same
      and come from the same range as coords. }
    Assert(IndexNum < CoordCount);

    { Initialize to common values }
    Tex := Vector4Single(0, 0, 0, 1);

    Result := TextureUnit < Cardinal(Length(TexCoordGen));
    if not Result then Exit;

    if TexCoordGen[TextureUnit] in ExplicitTexCoords then
    begin
      case TexImplementation of
        tcTexIndexed:
          { tcTexIndexed is set only if
            TexCoordIndex.Count >= CoordIndex.Count, so the IndexNum index
            is Ok for sure. That's why we don't do "ItemsSafe"
            for TexCoordIndex. }
          SetTexFromTexCoordArray(TexCoordGen[TextureUnit], TexCoordIndex.Items.Items[IndexNum]);
        tcCoordIndexed:
          { We already checked that IndexNum < CoordCount, so the first index
            is Ok for sure. }
          SetTexFromTexCoordArray(TexCoordGen[TextureUnit], CoordIndex.Items.Items[IndexNum]);
        tcNonIndexed:
          SetTexFromTexCoordArray(TexCoordGen[TextureUnit], IndexNum);
        else raise EInternalError.Create('TAbstractTexCoordinateRenderer.GetTextureCoord?');
      end;
    end else
      Tex := GenerateTexCoord(TexCoordGen[TextureUnit]);
  end;
end;

function TAbstractTexCoordinateRenderer.GetTextureCoord(
  IndexNum: integer; const TextureUnit: Cardinal;
  out Tex: TVector2Single): boolean;
var
  Tex4f: TVector4Single;
begin
  Result := GetTextureCoord(IndexNum, TextureUnit, Tex4f);
  Tex[0] := Tex4f[0];
  Tex[1] := Tex4f[1];
end;

procedure TAbstractTexCoordinateRenderer.GenerateVertex(indexNum: integer);

  procedure DoTexCoord(Index: Integer);
  var
    TextureUnit: Integer;
  begin
    for TextureUnit := 0 to High(TexCoordGen) do
    begin
      case TexCoordGen[TextureUnit] of
        tgExplicit2d: Arrays.TexCoord2D(TextureUnit, ArrayIndexNum)^ := TexCoordArray2d[TextureUnit].ItemsSafe[Index];
        tgExplicit3d: Arrays.TexCoord3D(TextureUnit, ArrayIndexNum)^ := TexCoordArray3d[TextureUnit].ItemsSafe[Index];
        tgExplicit4d: Arrays.TexCoord4D(TextureUnit, ArrayIndexNum)^ := TexCoordArray4d[TextureUnit].ItemsSafe[Index];
      end;
    end;
  end;

begin
  { Don't pass glTexCoordv here if BumpMappingMethod <> bmNone,
    then we'll handle passing texture coords appropriately
    ourselves. }
  if (BumpMappingMethod = bmNone) and
     (TexImplementation = tcTexIndexed) then
    DoTexCoord(TexCoordIndex.Items.Items[IndexNum]);

  inherited;
end;

{ TAbstractMatTexCoordinateRenderer ------------------------------------------ }

constructor TAbstractMatTexCoordinateRenderer.Create(ARenderer: TVRMLGLRenderer);
begin
  inherited;
  MaterialBinding := BIND_DEFAULT;
  UpdateMat1Implementation;
end;

procedure TAbstractMatTexCoordinateRenderer.UpdateMat1Implementation;

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

  { TODO: we handle all material bindings, but we handle BIND_PER_PART
    and BIND_PER_PART_INDEXED wrong for IndexedLineSet. }
end;

procedure TAbstractMatTexCoordinateRenderer.GenerateCoordinateBegin;
begin
  inherited;

  if (Geometry is TVRMLGeometryNode_1) and
     (Mat1Implementation = miOverall) then
    { For miOverall, job is already done by MaterialsBegin that bound index 0. }
    Assert(Renderer.Material1BoundNumber = 0);
end;

procedure TAbstractMatTexCoordinateRenderer.GenerateVertex(IndexNum: Integer);
begin
  case Mat1Implementation of
    miPerVertexCoordIndexed:
      Renderer.Render_BindMaterial_1(CoordIndex.ItemsSafe[IndexNum], true);
    miPerVertexMatIndexed:
      Renderer.Render_BindMaterial_1(MaterialIndex.ItemsSafe[IndexNum], true);
  end;

  inherited;
end;

procedure TAbstractMatTexCoordinateRenderer.GenerateCoordsRange(
  const RangeNumber: Cardinal; BeginIndex, EndIndex: Integer);
begin
  inherited;

  case Mat1Implementation of
    miPerFace:
      Renderer.Render_BindMaterial_1(RangeNumber, true);
    miPerFaceMatIndexed:
      Renderer.Render_BindMaterial_1(MaterialIndex.Items.Items[RangeNumber], true);
  end;
end;

function TAbstractMatTexCoordinateRenderer.NeedsBindMaterial1InsideBeginEnd: boolean;
begin
  Result := Mat1Implementation in
    [ miPerFace, miPerFaceMatIndexed,
      miPerVertexCoordIndexed, miPerVertexMatIndexed ];
end;

{ TAbstractColMatTexCoordinateRenderer --------------------------------------- }

constructor TAbstractColMatTexCoordinateRenderer.Create(ARenderer: TVRMLGLRenderer);
begin
  inherited;

  if Geometry is TNodeX3DComposedGeometryNode then
    RadianceTransfer := (Geometry as TNodeX3DComposedGeometryNode).FdRadianceTransfer.Items;
end;

procedure TAbstractColMatTexCoordinateRenderer.SetRadianceTransfer(
  const Value: TDynVector3SingleArray);
begin
  FRadianceTransfer := Value;

  if (RadianceTransfer.Count <> 0) and
     Assigned(Renderer.Attributes.OnRadianceTransfer) then
  begin
    if RadianceTransfer.Count mod Coord.Count <> 0 then
    begin
      VRMLWarning(vwSerious, 'radianceTransfer field must be emppty, or have a number of items being multiple of coods');
      FRadianceTransfer := nil;
    end else
    if RadianceTransfer.Count < Coord.Count then
    begin
      VRMLWarning(vwSerious, 'radianceTransfer field must be emppty, or have a number of items >= number of coods');
      FRadianceTransfer := nil;
    end else
      RadianceTransferVertexSize := RadianceTransfer.Count div Coord.Count;
  end else
    FRadianceTransfer := nil;
end;

procedure TAbstractColMatTexCoordinateRenderer.PrepareAttributes(var AllowIndexed: boolean);
begin
  inherited;

  if Assigned(Renderer.Attributes.OnVertexColor) or
     (RadianceTransfer <> nil) then
  begin
    Arrays.AddColor;
    AllowIndexed := false;
  end else
  if (Color <> nil) or (ColorRGBA <> nil) then
  begin
    Arrays.AddColor;
    if (ColorIndex <> nil) or (not ColorPerVertex) then
      AllowIndexed := false;
  end;
end;

procedure TAbstractColMatTexCoordinateRenderer.GenerateVertex(IndexNum: integer);
var
  VertexColor: TVector3Single;
  VertexIndex: Cardinal;
begin
  { Implement different color per vertex here. }
  if Assigned(Renderer.Attributes.OnVertexColor) then
  begin
    if CoordIndex <> nil then
      VertexIndex := CoordIndex.ItemsSafe[IndexNum] else
      VertexIndex := IndexNum;

    { Get vertex color, taking various possible configurations.
      OnVertexColor will be able to change it. }
    if (Color <> nil) and ColorPerVertex then
    begin
      if (ColorIndex <> nil) and (ColorIndex.Count <> 0) then
        VertexColor := Color.ItemsSafe[ColorIndex.ItemsSafe[IndexNum]] else
      if CoordIndex <> nil then
        VertexColor := Color.ItemsSafe[CoordIndex.ItemsSafe[IndexNum]] else
        VertexColor := Color.ItemsSafe[IndexNum];
    end else
    if (State.ShapeNode <> nil) and
       (State.ShapeNode.Material <> nil) then
    begin
      VertexColor := State.ShapeNode.Material.FdDiffuseColor.Value;
    end else
      VertexColor := White3Single; { default fallback }

    Renderer.Attributes.OnVertexColor(VertexColor, Shape,
      GetVertex(IndexNum), VertexIndex);
    Arrays.Color(ArrayIndexNum)^ := Vector4Single(VertexColor, Renderer.Material_BoundOpacity);
  end else
  if RadianceTransfer <> nil then
  begin
    if CoordIndex <> nil then
      VertexIndex := CoordIndex.ItemsSafe[IndexNum] else
      VertexIndex := IndexNum;

    VertexColor := Renderer.Attributes.OnRadianceTransfer(Geometry,
      @(RadianceTransfer.Items[VertexIndex * RadianceTransferVertexSize]),
      RadianceTransferVertexSize);

    Arrays.Color(ArrayIndexNum)^ := Vector4Single(VertexColor, Renderer.Material_BoundOpacity);
  end else
  if Color <> nil then
  begin
    if ColorPerVertex then
    begin
      if (ColorIndex <> nil) and (ColorIndex.Count <> 0) then
        Arrays.Color(ArrayIndexNum)^ := Vector4Single(Color.ItemsSafe[ColorIndex.ItemsSafe[IndexNum]], Renderer.Material_BoundOpacity) else
      if CoordIndex <> nil then
        Arrays.Color(ArrayIndexNum)^ := Vector4Single(Color.ItemsSafe[CoordIndex.ItemsSafe[IndexNum]], Renderer.Material_BoundOpacity) else
        Arrays.Color(ArrayIndexNum)^ := Vector4Single(Color.ItemsSafe[IndexNum], Renderer.Material_BoundOpacity);
    end else
      Arrays.Color(ArrayIndexNum)^ := FaceColor;
  end else
  if ColorRGBA <> nil then
  begin
    if ColorPerVertex then
    begin
      if (ColorIndex <> nil) and (ColorIndex.Count <> 0) then
        Arrays.Color(ArrayIndexNum)^ := ColorRGBA.ItemsSafe[ColorIndex.ItemsSafe[IndexNum]] else
      if CoordIndex <> nil then
        Arrays.Color(ArrayIndexNum)^ := ColorRGBA.ItemsSafe[CoordIndex.ItemsSafe[IndexNum]] else
        Arrays.Color(ArrayIndexNum)^ := ColorRGBA.ItemsSafe[IndexNum];
    end else
      Arrays.Color(ArrayIndexNum)^ := FaceColor;
  end;

  inherited;
end;

procedure TAbstractColMatTexCoordinateRenderer.GenerateCoordsRange(
  const RangeNumber: Cardinal; BeginIndex, EndIndex: Integer);
begin
  inherited;

  { Implement different color per face here. }
  if (not Assigned(Renderer.Attributes.OnVertexColor)) and
     (RadianceTransfer = nil) then
  begin
    if (Color <> nil) and (not ColorPerVertex) then
    begin
      if (ColorIndex <> nil) and (ColorIndex.Count <> 0) then
        FaceColor := Vector4Single(Color.ItemsSafe[ColorIndex.ItemsSafe[RangeNumber]], Renderer.Material_BoundOpacity) else
        FaceColor := Vector4Single(Color.ItemsSafe[RangeNumber], Renderer.Material_BoundOpacity);
    end else
    if (ColorRGBA <> nil) and (not ColorPerVertex) then
    begin
      if (ColorIndex <> nil) and (ColorIndex.Count <> 0) then
        FaceColor := ColorRGBA.ItemsSafe[ColorIndex.ItemsSafe[RangeNumber]] else
        FaceColor := ColorRGBA.ItemsSafe[RangeNumber];
    end;
  end;
end;

function TAbstractColMatTexCoordinateRenderer.NeedsSetColor: boolean;
begin
  Result :=
    Assigned(Renderer.Attributes.OnVertexColor) or
    (RadianceTransfer <> nil) or
    (Color <> nil) or (ColorRGBA <> nil);
end;

{ TAbstractNorColMatTexCoordinateRenderer ----------------------------------------------------- }

procedure TAbstractNorColMatTexCoordinateRenderer.PrepareAttributes(
  var AllowIndexed: boolean);
begin
  inherited;

  if not (
      { When IndexNum for normal works exactly like for position,
        then normals can be indexed. This is true in two cases:
        - there is no coordIndex, and normal vectors are not indexed
        - there is coordIndex, and normal vectors are indexed by coordIndex }
      (NorImplementation = niPerVertexCoordIndexed) or
     ((NorImplementation = niPerVertexNonIndexed) and (CoordIndex = nil)) ) then
    AllowIndexed := false;
end;

function TAbstractNorColMatTexCoordinateRenderer.
  NorImplementationFromVRML1Binding(NormalBinding: Integer): TNormalsImplementation;
begin
  Result := niNone;

  if (Normals = nil) or (NormalIndex = nil) then
    Exit;

  case NormalBinding of
    BIND_DEFAULT, BIND_PER_VERTEX_INDEXED:
      if (NormalIndex.Count > 0) and (NormalIndex.Items.Items[0] >= 0) then
        Result := niPerVertexNormalIndexed;
    BIND_PER_VERTEX:
      if CoordIndex <> nil then
        Result := niPerVertexCoordIndexed;
    BIND_OVERALL:
      if Normals.Count > 0 then
        Result := niOverall;
    BIND_PER_PART, BIND_PER_FACE:
      Result := niPerFace;
    BIND_PER_PART_INDEXED, BIND_PER_FACE_INDEXED:
      if (NormalIndex.Count > 0) and (NormalIndex.Items.Items[0] >= 0) then
        Result := niPerFaceNormalIndexed;
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
  if Normals.Count = 0 then
    Result := niNone;
end;

function TAbstractNorColMatTexCoordinateRenderer.CcwNormalsSafe(
  const Index: Integer): TVector3Single;
begin
  if Index < CcwNormals.Count then
    Result := CcwNormals.Items[Index] else
    Result := ZeroVector3Single;
end;

procedure TAbstractNorColMatTexCoordinateRenderer.GetNormal(
  IndexNum: Integer; RangeNumber: Integer; out N: TVector3Single);
begin
  case NorImplementation of
    niOverall:
      N := CcwNormals.Items[0];
    niPerVertexNonIndexed:
      N := CcwNormals.Items[IndexNum];
    niPerVertexCoordIndexed:
      N := CcwNormals.Items[CoordIndex.Items.Items[IndexNum]];
    niPerVertexNormalIndexed:
      N := CcwNormals.Items[NormalIndex.ItemsSafe[IndexNum]];
    niPerFace:
      N := CcwNormals.Items[RangeNumber];
    niPerFaceNormalIndexed:
      N := CcwNormals.Items[NormalIndex.ItemsSafe[RangeNumber]];
    else
      raise EInternalError.Create('GetNormal: NorImplementation ?');
  end;
end;

function TAbstractNorColMatTexCoordinateRenderer.NormalsFlat: boolean;
begin
  Result := NorImplementation in [niOverall, niPerFace, niPerFaceNormalIndexed];
end;

procedure TAbstractNorColMatTexCoordinateRenderer.GenerateVertex(IndexNum: Integer);
begin
  case NorImplementation of
    niPerVertexNormalIndexed:
      begin
        Assert(Arrays.Indexes = nil);
        Arrays.Normal(ArrayIndexNum)^ := CcwNormalsSafe(NormalIndex.ItemsSafe[IndexNum]);
      end;
    niPerVertexNonIndexed:
      if CoordIndex <> nil then
      begin
        Assert(Arrays.Indexes = nil);
        Arrays.Normal(ArrayIndexNum)^ := CcwNormalsSafe(IndexNum);
      end;
    niPerFace, niPerFaceNormalIndexed:
      begin
        Assert(Arrays.Indexes = nil);
        Arrays.Normal(ArrayIndexNum)^ := FaceNormal;
      end;
  end;

  inherited;
end;

procedure TAbstractNorColMatTexCoordinateRenderer.GenerateCoordinateBegin;

  procedure SetAllNormals(const Value: TVector3Single);
  var
    N: PVector3Single;
    I: Integer;
  begin
    N := Arrays.Normal;
    for I := 0 to Arrays.Count - 1 do
    begin
      N^ := Value;
      Arrays.IncNormal(N);
    end;
  end;

begin
  inherited;

  if Normals <> nil then
  begin
    if NormalsCcw then
      CcwNormals := Normals else
    begin
      CcwNormals := TDynVector3SingleArray.Create;
      CcwNormals.AssignNegated(Normals);
    end;
  end;

  if NorImplementation = niPerVertexCoordIndexed then
  begin
    if Arrays.Indexes <> nil then
      CcwNormals.AssignToInterleaved(Arrays.Normal, Arrays.CoordinateSize) else
      CcwNormals.AssignToInterleavedIndexed(Arrays.Normal, Arrays.CoordinateSize, IndexesFromCoordIndex);
  end else
  if (NorImplementation = niPerVertexNonIndexed) and (CoordIndex = nil) then
  begin
    Assert(Arrays.Indexes = nil);
    if CcwNormals.Count = Arrays.Count then
      CcwNormals.AssignToInterleaved(Arrays.Normal, Arrays.CoordinateSize) else
      VRMLWarning(vwSerious, Format('Normal coordinates count invalid for %s. Expected %d, got %d',
        [Geometry.NodeTypeName, Arrays.Count, CcwNormals.Count]));
  end else
  if NorImplementation = niOverall then
  begin
    SetAllNormals(CcwNormalsSafe(0));
  end;
end;

procedure TAbstractNorColMatTexCoordinateRenderer.GenerateCoordinateEnd;
begin
  if (Normals <> nil) and (not NormalsCcw) then
    FreeAndNil(CcwNormals);

  inherited;
end;

procedure TAbstractNorColMatTexCoordinateRenderer.GenerateCoordsRange(
  const RangeNumber: Cardinal; BeginIndex, EndIndex: Integer);
begin
  inherited;

  { FaceNormal will be actually copied to Arrays.Normal in GenerateVertex
    for this face. }
  case NorImplementation of
    niPerFace:
      FaceNormal := CcwNormalsSafe(RangeNumber);
    niPerFaceNormalIndexed:
      FaceNormal := CcwNormalsSafe(NormalIndex.ItemsSafe[RangeNumber]);
  end;
end;
